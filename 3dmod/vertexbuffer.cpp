/*
 *  vertexbuffer.cpp - Functions for working with vertex buffer objects
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#define GL_GLEXT_PROTOTYPES
#include <qgl.h>
#include "imod.h"
#include "vertexbuffer.h"
#include <map>
using namespace std;

VertBufData *vbDataNew()
{
  VertBufData *vbd;
  vbd = B3DMALLOC(VertBufData, 1);
  if (!vbd)
    return NULL;
  vbDataInit(vbd);
  return vbd;
}

void vbDataInit(VertBufData *vbd)
{
  vbd->vbObj = 0;
  vbd->ebObj = 0;
  vbd->vboSize = 0;
  vbd->eboSize = 0;
  vbd->numSpecialSets = 0;
  vbd->numIndSpecial = NULL;
  vbd->rgbtSpecial = NULL;
  vbd->specialSize = 0;
  vbd->remnantIndList = NULL;
  vbd->remListSize = 0;
  vbd->remnantStore = NULL;
}

void vbDataClear(VertBufData *vbd)
{
  if (vbd->vbObj)
    glDeleteBuffers(1, &vbd->vbObj);
  if (vbd->ebObj)
    glDeleteBuffers(1, &vbd->ebObj);
  B3DFREE(vbd->numIndSpecial);
  B3DFREE(vbd->rgbtSpecial);
  B3DFREE(vbd->remnantIndList);
  ilistDelete(vbd->remnantStore);
  vbDataInit(vbd);
}

void vbDataDelete(VertBufData *vbd)
{
  vbDataClear(vbd);
  free(vbd);
}

void vbCleanupVBD(Imesh *mesh)
{
  if (!mesh || !mesh->vertBuf)
    return;
  vbDataDelete(mesh->vertBuf);
  mesh->vertBuf = NULL;
}

void vbCleanupVBD(Iobj *obj)
{
  if (!obj->mesh)
    return;
  for (int m = 0; m < obj->meshsize; m++)
    vbCleanupVBD(&obj->mesh[m]);
}

void vbCleanupVBD(Imod *imod)
{
  if (!imod)
    return;
  for (int ob = 0; ob < imod->objsize; ob++)
    vbCleanupVBD(&imod->obj[ob]);
}

int vbAnalyzeMesh(Imesh *mesh, float zscale, DrawProps *defProps, int handleFlags,
                  int nonVboFlags)
{
  typedef map<b3dUInt32,int> RGBTmap;
  bool valid = true;
  RGBTmap colors;
  pair<RGBTmap::iterator,bool> mapret;
  RGBTmap::iterator mapit;
  GLenum error;
  VertBufData *vbd = mesh->vertBuf;
  Istore *stp;
  Istore store;
  GLuint *inds;
  int *mlist = mesh->list;
  DrawProps curProps;
  int i, j, cumInd, defInd, nextItemIndex, stateFlags, vertDflt, changeFlags, firstDflt;
  b3dUInt32 vertRGBT, firstRGBT;
  int remInd, curSave, curNext;
  int numDefaultTri = 0, numMixedTri = 0;
  int useFill = handleFlags & CHANGED_FCOLOR;

  // Check if there is a current VBO and it is all still valid 
  vbPackRGBT(defProps, useFill, firstRGBT);
  if (vbd && vbd->vbObj && 
      (!ilistSize(mesh->store) || 
       (vbd->handleFlags == handleFlags && vbd->defaultRGBT == firstRGBT)) &&
      vbd->checksum == istoreChecksum(mesh->store)) {

    // If Z-scale still valid, return a -1; if have to fix the Z-scale, do it, return -2
    if (fabs((double)(zscale - vbd->zscale)) < 1.e-4)
      return -1;
    glBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    if (vbLoadVertexNormalArray(mesh, zscale))
      return 1;
    vbd->zscale = zscale;
    return -2;
  }

  // Now proceed to full analysis
  nextItemIndex = istoreFirstChangeIndex(mesh->store);
  
  for (i = 0; i < mesh->lsize && valid; i++) {
    switch (mlist[i]) {

    case IMOD_MESH_BGNTRI:
    case IMOD_MESH_ENDTRI:
    case IMOD_MESH_BGNPOLY:
    case IMOD_MESH_NORMAL:
    case IMOD_MESH_BGNBIGPOLY:
    case IMOD_MESH_SWAP:
      valid = false;
      break;

    case IMOD_MESH_BGNPOLYNORM:
      i++;
      while (mlist[i] != IMOD_MESH_ENDPOLY && valid) {
        valid = (mlist[i] == mlist[i+1] + 1) && (mlist[i+2] == mlist[i+3] + 1) && 
          (mlist[i+4] == mlist[i+5] + 1);
        i += 6;
        numDefaultTri++;
      }
      break;

    case IMOD_MESH_BGNPOLYNORM2:
      i++;
      while (mlist[i] != IMOD_MESH_ENDPOLY) {
        if (nextItemIndex < i || nextItemIndex > i + 2) {
          
          // Count a default triangle if no changes in this range
          numDefaultTri++;
          i += 3;
        } else {

          // Otherwise look at each vertex and get its properties if it is changed
          for (j = 0; j < 3; j++) {
            vertDflt = 1;
            if (i == nextItemIndex) {
              curProps = *defProps;
              stateFlags = 0;
              nextItemIndex = istoreNextChange(mesh->store, defProps, &curProps,
                                               &stateFlags, &changeFlags);
              if (stateFlags & handleFlags) {
                vertDflt = 0;
                vbPackRGBT(&curProps, useFill, vertRGBT);
              }

              // Take triangle as mixed if it has unhandleable flags
              if (stateFlags & nonVboFlags) {
                firstDflt = -1;
                i++;
                continue;
              }
            }

            // For first vertex record the triangle properties, for later one record if 
            // there is a mismatch from the first
            if (!j) {
              firstDflt = vertDflt;
              firstRGBT = vertRGBT;
            } else if (vertDflt != firstDflt || (!vertDflt && vertRGBT != firstRGBT)) {
              firstDflt = -1;
            }
            i++;
          }
          
          // Count whether it is a default or mixed triangle
          if (firstDflt < 0) {
            numMixedTri++;
          } else if (firstDflt > 0) {
            numDefaultTri++;
          } else {
            
            // For a special triangle, add to list of RGBT values with a count of 1 if it
            // is not on the list; if it is already on the list increment its count;
            mapret = colors.insert(pair<b3dUInt32,int>(firstRGBT, 1));
            if (mapret.second == false)
              mapret.first->second++;
          }
        }
      }
      break;

    case IMOD_MESH_END:
      break;
    }
  }

  if (!valid)
    return 3;
  if (!colors.size() && !numDefaultTri)
    return 2;

  // Allocate vertBuf if needed
  if (!mesh->vertBuf)
    mesh->vertBuf = vbDataNew();
  vbd = mesh->vertBuf; 
  if (!vbd)
    return 1;

  // Now allocate whatever pieces are needed in there
  vbd->numSpecialSets = colors.size();
  cumInd = numDefaultTri * 3;
  vbd->numIndDefault = cumInd;
  if (vbd->numSpecialSets) {
    if (vbd->numSpecialSets > vbd->specialSize) {
      B3DFREE(vbd->numIndSpecial);
      B3DFREE(vbd->rgbtSpecial);
      vbd->numIndSpecial = B3DMALLOC(int, vbd->numSpecialSets);
      vbd->rgbtSpecial = B3DMALLOC(b3dUInt32, vbd->numSpecialSets);
      vbd->specialSize = vbd->numSpecialSets;
      if (!vbd->numIndSpecial || !vbd->rgbtSpecial) {
        vbCleanupVBD(mesh);
        return 1;
      }
    }
      
    // Add up the special set sizes and re-initialize the counts to be starting indexes
    mapit = colors.begin();
    for (i = 0; i < vbd->numSpecialSets; i++) {
      vbd->rgbtSpecial[i] = mapit->first;
      vbd->numIndSpecial[i] = mapit->second * 3;
      mapit->second = cumInd;
      cumInd += vbd->numIndSpecial[i];
      mapit++;
    }
  }
  imodTrace('b',"dfltInd %d  spec sets %d cumind %d  remnant %d", vbd->numIndDefault,
            vbd->numSpecialSets, cumInd, numMixedTri);

  // Allocate index array for remnant indices if needed and create the store
  vbd->numRemnant = 0;
  if (numMixedTri) {
    vbd->numRemnant = numMixedTri * 3 + 3;
    if (vbd->numRemnant > vbd->remListSize) {
      B3DFREE(vbd->remnantIndList);
      vbd->remListSize = vbd->numRemnant;
      vbd->remnantIndList = B3DMALLOC(int, vbd->remListSize);
      if (!vbd->remnantIndList) {
        vbCleanupVBD(mesh);
        return 1;
      }
    }
    ilistDelete(vbd->remnantStore);
    vbd->remnantStore = ilistNew(sizeof(Istore), vbd->numRemnant / 8);
    if (!vbd->remnantStore) {
      vbCleanupVBD(mesh);
      return 1;
    }
    vbd->remnantStore->quantum = B3DMAX(vbd->remnantStore->quantum, vbd->numRemnant / 8);
    vbd->remnantIndList[0] = IMOD_MESH_BGNPOLYNORM2;
  }

  // Now get the vertex buffers themselves
  if (!vbd->vbObj || 3 * mesh->vsize > vbd->vboSize) {
    vbd->vboSize = 3 * mesh->vsize;
    if (vbd->vbObj)
      glDeleteBuffers(1, &vbd->vbObj);
    glGenBuffers(1, &vbd->vbObj);
    glBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    glBufferData(GL_ARRAY_BUFFER, 3 * mesh->vsize * sizeof(GLfloat), NULL,GL_STATIC_DRAW);
    error = glGetError();
    if (error) {
      vbCleanupVBD(mesh);
      return 1;
    }
  } else 
    glBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);

  if (!vbd->ebObj || cumInd > vbd->eboSize) {
    vbd->eboSize = cumInd; 
    if (vbd->ebObj)
      glDeleteBuffers(1, &vbd->ebObj);
    glGenBuffers(1, &vbd->ebObj);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,  cumInd * sizeof(GLuint), NULL,GL_STATIC_DRAW);
    error = glGetError();
    if (error) {
      vbCleanupVBD(mesh);
      return 1;
    }
  } else 
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
        
  imodTrace('b',"vbObj %d  ebObj %d", vbd->vbObj, vbd->ebObj);
  // Load the vertices and finish with buffer for now
  if (vbLoadVertexNormalArray(mesh, zscale)) {
    vbCleanupVBD(mesh);
    return 1;
  }
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  vbd->zscale = zscale;
  vbd->handleFlags = handleFlags;
  vbPackRGBT(defProps, useFill, vbd->defaultRGBT);
  vbd->checksum = istoreChecksum(mesh->store);

  // Get temporary array for indexes
  inds = B3DMALLOC(GLuint,  cumInd);
  if (!inds) {
    vbCleanupVBD(mesh);
    return 1;
  }

  // No fine grain: copy all the indices into index array
  defInd = 0;
  if (!mesh->store) {
    i = 0;
    while (mlist[i] != IMOD_MESH_END) {
      if (mlist[i] ==  IMOD_MESH_BGNPOLYNORM) {
        i++;
        while (mlist[i] != IMOD_MESH_ENDPOLY) {
          i++;
          inds[defInd++] = mlist[i++] / 2;
        }
      }
      else if (mlist[i] ==  IMOD_MESH_BGNPOLYNORM2) {
        i++;
        while (mlist[i] != IMOD_MESH_ENDPOLY) {
          inds[defInd++] = mlist[i++] / 2;
        }
      }
      i++;
    }
  } else {

    // Otherwise process all triangles into index array or remnant arrays
    nextItemIndex = istoreFirstChangeIndex(mesh->store);
    remInd = 1;
    for (i = 0; i < mesh->lsize; i++) {
      switch (mlist[i]) {
      case IMOD_MESH_BGNPOLYNORM2:
        i++;
        while (mlist[i] != IMOD_MESH_ENDPOLY) {
          
          // Repeat the analysis to determine default, special, or mixed triangle
          curSave = mesh->store->current;
          if (nextItemIndex < i || nextItemIndex > i + 2) {
            firstDflt = 1;
          } else {

            // Otherwise look at each vertex and get its properties if it is changed
            for (j = 0; j < 3; j++) {
              vertDflt = 1;
              if (i + j == nextItemIndex) {
                curProps = *defProps;
                stateFlags = 0;
                nextItemIndex = istoreNextChange(mesh->store, defProps, &curProps,
                                                 &stateFlags, &changeFlags);
                if (stateFlags & handleFlags) {
                  vertDflt = 0;
                  vbPackRGBT(&curProps, useFill, vertRGBT);
                }
                if (stateFlags & nonVboFlags) {
                  firstDflt = -1;
                  continue;
                }
              }
              
              // For first vertex record the triangle properties, for later one stop if 
              // there is a mismatch from the first
              if (!j) {
                firstDflt = vertDflt;
                firstRGBT = vertRGBT;
              } else if (vertDflt != firstDflt || (!vertDflt && vertRGBT != firstRGBT)) {
                firstDflt = -1;
              }
            }
          }
          
          // Save indexes for default or special triangles
          if (firstDflt > 0) {
            inds[defInd++] = mlist[i++] / 2;
            inds[defInd++] = mlist[i++] / 2;
            inds[defInd++] = mlist[i++] / 2;
          } else if (firstDflt == 0) {
            mapit = colors.find(firstRGBT);
            inds[mapit->second++] = mlist[i++] / 2;
            inds[mapit->second++] = mlist[i++] / 2;
            inds[mapit->second++] = mlist[i++] / 2;
          } else {

            // For mixed triangle, save the current pointer, copy the index for each 
            // vertex to the remnant index array, and copy all stores for that vertex
            // to the remnant store, changing the index to the new value
            curNext = mesh->store->current;
            for (j = 0; j < 3; j++) {
              vbd->remnantIndList[remInd] = mlist[i];
              while (curSave < curNext) {
                stp = istoreItem(mesh->store, curSave);
                if (stp->index.i == i) {
                  store = *stp;
                  store.index.i = remInd;
                  if (istoreInsert(&vbd->remnantStore, &store)) {
                    free(inds);
                    vbCleanupVBD(mesh);
                    return 1;
                  }
                  curSave++;
                } else
                  break;
              }
              i++;
              remInd++;
            }
            mesh->store->current = curNext;
          }
        }
        break;

      case IMOD_MESH_END:
        break;
      }
    }
  }
  if (vbd->numRemnant) {
    vbd->remnantIndList[remInd++] = IMOD_MESH_ENDPOLY;
    vbd->remnantIndList[remInd++] = IMOD_MESH_END;
  }
  glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, cumInd * sizeof(GLuint), inds);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  free(inds);
  return 0;
}

/* 
 * Packs the vertex/normal array into a temporary array, scaling by the given Z scale,
 * and loads this into GL_ARRAY_BUFFER, which must already be bound
 */
int vbLoadVertexNormalArray(Imesh *mesh, float zscale)
{
  // Temporary array for vertices
  int i, numVert = 0;
  Ipoint *vert = mesh->vert;
  GLfloat *verts = B3DMALLOC(GLfloat, 3 * mesh->vsize);
  if (!verts)
    return 1;
  
  // Load the vertices and finish with buffer for now
  for (i = 0; i < mesh->vsize; i += 2) {
    verts[numVert++] = vert[i + 1].x;
    verts[numVert++] = vert[i + 1].y;
    verts[numVert++] = vert[i + 1].z;
    verts[numVert++] = vert[i].x;
    verts[numVert++] = vert[i].y;
    verts[numVert++] = vert[i].z * zscale;
  }
  glBufferSubData(GL_ARRAY_BUFFER, 0, numVert * sizeof(GLfloat), verts);
  free(verts);
  return 0;
}

/*
 * Convert color and trans values directly or from a DrawProps to one 32-bit number
 */
void vbPackRGBT(float red, float green, float blue, int trans, b3dUInt32 &rgbtVal)
{
  int r = (int)(255. * red);
  int g = (int)(255. * green);
  int b = (int)(255. * blue);
  B3DCLAMP(r, 0, 255);
  B3DCLAMP(g, 0, 255);
  B3DCLAMP(b, 0, 255);
  B3DCLAMP(trans, 0, 255);
  rgbtVal = (r << 24) | (g << 16) | (b << 8) | trans;
}

void vbPackRGBT(DrawProps *props, int useFill, b3dUInt32 &rgbtVal)
{
  if (useFill)
    vbPackRGBT(props->fillRed, props->fillGreen, props->fillBlue, props->trans, rgbtVal);
  else
    vbPackRGBT(props->red, props->green, props->blue, props->trans, rgbtVal);
}

/* 
 * Unpack the color and trans components from the rgbt value
 */
void vbUnpackRGBT(b3dUInt32 rgbtVal, float &red, float &green, float &blue, int &trans)
{
  red = (float)(((rgbtVal >> 24) & 255) / 255.);
  green = (float)(((rgbtVal >> 16) & 255) / 255.);
  blue = (float)(((rgbtVal >> 8) & 255) / 255.);
  trans = rgbtVal & 255;
}

void vbUnpackRGBT(b3dUInt32 rgbtVal, int useFill, DrawProps *props)
{
  if (useFill)
    vbUnpackRGBT(rgbtVal, props->fillRed, props->fillGreen, props->fillBlue, 
                 props->trans);
  else
    vbUnpackRGBT(rgbtVal, props->red, props->green, props->blue, props->trans);
}

