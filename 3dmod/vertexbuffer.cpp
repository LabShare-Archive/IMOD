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

#include "imod.h"
#include "imodv.h"
#include "vertexbuffer.h"
#include "mv_ogl.h"

typedef map<int,b3dUInt32> ReverseMap;

// Declarations of local functions for dealing with VBD's
static VertBufData *vbDataNew();
static void vbDataInit(VertBufData *vbd);
static void vbDataClear(VertBufData *vbd);
static void vbDataDelete(VertBufData *vbd);

// Local and global functions for dealing with VBD's
static VertBufData *vbDataNew()
{
  VertBufData *vbd;
  vbd = B3DMALLOC(VertBufData, 1);
  if (!vbd)
    return NULL;
  vbDataInit(vbd);
  return vbd;
}

static void vbDataInit(VertBufData *vbd)
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
  vbd->numFanIndSpecial = NULL;
}

static void vbDataClear(VertBufData *vbd)
{
  if (!vbd)
    return;
  if (vbd->vbObj)
    b3dDeleteBuffers(1, (GLuint *)&vbd->vbObj);
  if (vbd->ebObj)
    b3dDeleteBuffers(1, (GLuint *)&vbd->ebObj);
  B3DFREE(vbd->numIndSpecial);
  B3DFREE(vbd->numFanIndSpecial);
  B3DFREE(vbd->rgbtSpecial);
  B3DFREE(vbd->remnantIndList);
  ilistDelete(vbd->remnantStore);
  vbDataInit(vbd);
}

static void vbDataDelete(VertBufData *vbd)
{
  vbDataClear(vbd);
  B3DFREE(vbd);
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
  vbCleanupContVBD(obj);
  vbCleanupSphereVBD(obj);
  vbCleanupMeshVBD(obj);
}

void vbCleanupVBD(Imod *imod)
{
  if (!imod)
    return;
  for (int ob = 0; ob < imod->objsize; ob++)
    vbCleanupVBD(&imod->obj[ob]);
}

void vbCleanupMeshVBD(Iobj *obj)
{
  if (!obj->mesh)
    return;
  for (int m = 0; m < obj->meshsize; m++)
    vbCleanupVBD(&obj->mesh[m]);
}

void vbCleanupContVBD(Iobj *obj)
{
  vbDataDelete(obj->vertBufCont);
  obj->vertBufCont = NULL;
}

void vbCleanupSphereVBD(Iobj *obj)
{
  vbDataDelete(obj->vertBufSphere);
  obj->vertBufSphere = NULL;
}

// THE VB MANAGER CLASS
VertBufManager::VertBufManager()
{
  mInds = NULL;
  mIndSize = 0;
  mVerts = NULL;
  mVertSize = 0;
  mDefSphInds = NULL;
  mDefSphIndSize = 0;
  mDefSphVerts = NULL;
  mDefSphVertSize = 0;

}

/*
 * Analyze a mesh and pack it into VBOs if it qualifies
 */
int VertBufManager::analyzeMesh(Imesh *mesh, float zscale, int fillType, int useFillColor,
                  DrawProps *defProps)
{
  bool valid = true;
  RGBTmap colors;
  pair<RGBTmap::iterator,bool> mapret;
  RGBTmap::iterator mapit;
  VertBufData *vbd = mesh->vertBuf;
  RGBTindices rInd;
  Istore *stp;
  Istore store;
  int *mlist = mesh->list;
  DrawProps curProps;
  int i, j, cumInd, defInd, nextItemIndex, stateFlags, vertDflt, changeFlags, firstDflt;
  b3dUInt32 vertRGBT, firstRGBT;
  int remInd, curSave, curNext;
  int numDefaultTri = 0, numMixedTri = 0;
  int handleFlags, nonVboFlags = 0;
  
  if (fillType)
    handleFlags = (useFillColor ? CHANGED_FCOLOR : CHANGED_COLOR) | CHANGED_TRANS;
  else {
    handleFlags = CHANGED_COLOR | CHANGED_TRANS;
    nonVboFlags = CHANGED_3DWIDTH;
  }
  rInd.numFanInds = 0;

  // Check if there is a current VBO and it is all still valid 
  packRGBT(defProps, useFillColor, firstRGBT);
  if (vbd && vbd->vbObj && fillType == vbd->fillType && 
      (!ilistSize(mesh->store) || 
       (vbd->useFillColor == useFillColor && vbd->defaultRGBT == firstRGBT)) &&
      vbd->checksum == istoreChecksum(mesh->store)) {

    // If Z-scale still valid, return a -1; if have to fix the Z-scale, do it, return -2
    if (fabs((double)(zscale - vbd->zscale)) < 1.e-4)
      return -1;
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    if (loadVertexNormalArray(mesh, zscale, fillType)) {
      vbCleanupVBD(mesh);
      return 1;
    }
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
                packRGBT(&curProps, useFillColor, vertRGBT);
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
            rInd.firstElement = i;
            rInd.numInds = 1;
            mapret = colors.insert(pair<b3dUInt32,RGBTindices>(firstRGBT, rInd));
            if (mapret.second == false)
              mapret.first->second.numInds++;
          }
        }
      }
      break;

    case IMOD_MESH_END:
      break;
    }
  }

  if (!valid) {
    vbCleanupVBD(mesh);
    return 3;
  }
  if (!colors.size() && !numDefaultTri) {
    vbCleanupVBD(mesh);
    return 2;
  }

  vbd = allocateVBDIfNeeded(&mesh->vertBuf); 
  if (!vbd)
    return 1;

  // Now allocate whatever pieces are needed in there.  Set remnant value in vbd first
  // Add up the special set sizes and re-initialize the counts to be starting indexes
  cumInd = numDefaultTri * 3;
  vbd->numRemnant = 0;
  if (numMixedTri)
    vbd->numRemnant = numMixedTri * 3 + 3;
  i = -1;
  if (allocateSpecialSets(vbd, colors.size(), cumInd, 0) ||
      processMap(vbd, &colors, cumInd, 3, i)) {
    vbCleanupVBD(mesh);
    return 1;
  }
      
  imodTrace('b',"dfltInd %d  spec sets %d cumind %d  remnant %d", vbd->numIndDefault,
            vbd->numSpecialSets, cumInd, numMixedTri);

  // Create the store for remnants
  if (numMixedTri) {
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
  if (genAndBindBuffers(vbd,  mesh->vsize, cumInd)) {
    vbCleanupVBD(mesh);
    return 1;
  }
        
  imodTrace('b',"vbObj %d  ebObj %d", vbd->vbObj, vbd->ebObj);

  // Load the vertices and finish with buffer for now
  if (loadVertexNormalArray(mesh, zscale, fillType)) {
    vbCleanupVBD(mesh);
    return 1;
  }
  b3dBindBuffer(GL_ARRAY_BUFFER, 0);

  // Set the identifiers of this vb data
  vbd->zscale = zscale;
  vbd->fillType = fillType;
  vbd->useFillColor = useFillColor;
  packRGBT(defProps, useFillColor, vbd->defaultRGBT);
  vbd->checksum = istoreChecksum(mesh->store);

  // Get or use temporary array for indexes
  if (allocateTempInds(cumInd)) {
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
          mInds[defInd++] = mlist[i++] / 2;
        }
      }
      else if (mlist[i] ==  IMOD_MESH_BGNPOLYNORM2) {
        i++;
        while (mlist[i] != IMOD_MESH_ENDPOLY) {
          mInds[defInd++] = mlist[i++] / 2;
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
                  packRGBT(&curProps, useFillColor, vertRGBT);
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
            mInds[defInd++] = mlist[i++] / 2;
            mInds[defInd++] = mlist[i++] / 2;
            mInds[defInd++] = mlist[i++] / 2;
          } else if (firstDflt == 0) {
            mapit = colors.find(firstRGBT);
            mInds[mapit->second.numInds++] = mlist[i++] / 2;
            mInds[mapit->second.numInds++] = mlist[i++] / 2;
            mInds[mapit->second.numInds++] = mlist[i++] / 2;
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
  b3dBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, cumInd * sizeof(GLuint), mInds);
  b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  return 0;
}

/*
 * Analyze spherical points in the given object and convert them to vectors and
 * normals for faster drawing
 */
int VertBufManager::analyzeSpheres(Iobj *obj, int obNum, float zscale, int xybin,
                                   float scrnScale, int quality, int fillType,
                                   int useFillColor, int thickenCont, int checkTime)
{
  RGBTmap colors;
  pair<RGBTmap::iterator,bool> mapret;
  RGBTmap::iterator mapit;
  RGBTindices rInd;
  DrawProps defProps, contProps;
  b3dUInt32 contRGBT;
  VertBufData *vbd = obj->vertBufSphere;
  Icont *cont;
  int handleFlags, nonVboFlags = 0, numRemnant, fullState, skip, numVerts, co, pt;
  int colorType, cumFanInd, cumQuadInd, surfState, contState, stepRes;
  float drawsize;
  int numDefSphVert, numDefSphQuad, numDefSphFan, numTriples,indVert, indQuad, indFan;
  int indQuadDef, indFanDef, irem, match;

  if (fillType > 0)
    handleFlags = (useFillColor ? CHANGED_FCOLOR : CHANGED_COLOR) | CHANGED_TRANS;
  else {
    handleFlags = CHANGED_COLOR | CHANGED_TRANS;
    nonVboFlags = CHANGED_3DWIDTH;
  }
  colorType = useFillColor ? GEN_STORE_FCOLOR : GEN_STORE_COLOR;

  istoreDefaultDrawProps(obj, &defProps);

  // Check if there is a current VBO and it is all still valid 
  // TODO: handle finegrain AND size changes
  //packRGBT(defProps, useFillColor, contRGBT);
  if (vbd && vbd->vbObj && fillType == vbd->fillType && vbd->useFillColor == useFillColor
      && quality == vbd->quality && obj->pdrawsize == vbd->pdrawsize &&
      checkTime == vbd->checkTime && fabs((double)(scrnScale - vbd->scrnScale)) < 1.e-4 &&
      fabs((double)(zscale - vbd->zscale)) < 1.e-4 && 
      (fillType != 0 || thickenCont == vbd->thickenCont)) {
    match = 1;
    if (fillType == 0 && thickenCont)
      match = checkSelectedAreRemnants(vbd, obNum);
    if (match && !Imodv->standalone && obNum == Imodv->imod->cindex.object && 
        vbd->checksum != imodObjectChecksum(obj, obNum))
      match = 0;
    if (match)
      return -1;
  }

  numRemnant = 0;
  cumQuadInd = 0;
  cumFanInd = 0;
  numVerts = 0;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;
    fullState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co, 
                                          cont->surf, &contState, &surfState);

    // Skip a gap without counting it; count one to be excluded or that can't be handled
    if (contProps.gap)
      continue;
    skip = 0;
    if ((fullState & nonVboFlags) || (thickenCont && imodvCheckThickerContour(co)))
      skip = 1;

    // Check for point changes that would need to be handled
    if (!skip && cont->store) {
      if (istoreCountItems(cont->store, colorType, 1) || 
          istoreCountItems(cont->store, GEN_STORE_TRANS, 1))
        skip = 1;
      if (!skip && fillType <= 0 && istoreCountItems(cont->store, GEN_STORE_3DWIDTH, 1))
        skip = 1;
    }
    if (skip) {
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 1);
      numRemnant++;
      continue;
    }

    // Loop on the points and determine number of vertices and indices needed for each
    // Since we don't skip point drawing, this doesn't need store changes checked
    rInd.numInds = 0;
    rInd.numFanInds = 0;
    for (pt = 0; pt < cont->psize; pt++) {

      // Only draw zero-size points with scattered point objects
      drawsize = imodPointGetSize(obj, cont, pt) / xybin;
      if (!iobjScat(obj->flags) && !drawsize)
        continue;
      stepRes = sphereResForSize(drawsize);
      numVerts += sphereCounts(2 * stepRes, stepRes, fillType, rInd.numInds,
                               rInd.numFanInds);

      // For a special contour, add to list of RGBT values with the counts if it
      // is not on the list; if it is already on the list increment its count;
      if (fullState & handleFlags) {
        rInd.firstElement = co;
        packRGBT(&contProps, useFillColor, contRGBT);
        mapret = colors.insert(pair<b3dUInt32,RGBTindices>(contRGBT, rInd));
        if (mapret.second == false) {
          mapret.first->second.numInds += rInd.numInds;
          mapret.first->second.numFanInds += rInd.numFanInds;
        }
      } else {
        cumQuadInd += rInd.numInds;
        cumFanInd += rInd.numFanInds;
      }
    }
  }      

  if (!numVerts) {
    vbCleanupSphereVBD(obj);
    return 2;
  }

  // Get parameters for default sphere
  drawsize = obj->pdrawsize / xybin;
  stepRes = sphereResForSize(drawsize);
  numDefSphVert = sphereCounts(2 * stepRes, stepRes, fillType, numDefSphQuad, 
                               numDefSphFan);
  imodTrace('b', "numverts %d cumfan %d cumquad %d def vert %d quad %d fan %d rem %d",
            numVerts, cumFanInd, cumQuadInd, numDefSphVert,numDefSphQuad, numDefSphFan,
            numRemnant);

  vbd = allocateVBDIfNeeded(&obj->vertBufSphere); 
  if (!vbd || drawsize < 0) {
    vbCleanupSphereVBD(obj);
    return 1;
  }
  vbd->numFanIndDefault = cumFanInd;
  vbd->numRemnant = numRemnant;

  // Now allocate whatever pieces are needed in VBD
  // Add up the special set sizes and re-initialize the counts to be starting indexes
  if (allocateSpecialSets(vbd, colors.size(), cumQuadInd, 1) ||
      processMap(vbd, &colors, cumQuadInd, 1, cumFanInd)) {
    vbCleanupSphereVBD(obj);
    return 1;
  }
  vbd->fanIndStart = cumQuadInd;

  // Now allocate temps plus default sphere
  numTriples = (1 + (fillType > 0 ? 1 : 0)) * numVerts;
  if (allocateTempVerts(numTriples) || allocateTempInds(cumFanInd) ||
      allocateDefaultSphere(numDefSphVert, numDefSphQuad + numDefSphFan, fillType)) {
    vbCleanupSphereVBD(obj);
    return 1;
  }
    
  if (genAndBindBuffers(vbd, numTriples, cumFanInd)) {
    vbCleanupSphereVBD(obj);
    return 1;
  }

  // Build the default sphere
  indVert = 0;
  indQuad = 0;
  indFan = numDefSphQuad;
  makeSphere(drawsize, 2 * stepRes, stepRes, mDefSphVerts, mDefSphInds, indVert, indQuad, 
           indFan, fillType, 0., 0., 0.);
  imodTrace('b', "numtriples %d cumquad %d cumfan %d after def vert %d quad %d fan %d", 
            numTriples, cumQuadInd, cumFanInd, indVert, indQuad,
            indFan);
  /*for (pt = 0; pt < indFan; pt++) {
    imodPrintStderr(" %d", mDefSphInds[pt]);
    if ((pt + 1) % 16 == 0 || pt == indFan - 1) imodPrintStderr("\n");
    } */

  // Set the identifiers of this vb data
  vbd->zscale = zscale;
  vbd->fillType = fillType;
  vbd->useFillColor = useFillColor;
  packRGBT(&defProps, useFillColor, vbd->defaultRGBT);
  vbd->checkTime = checkTime;
  vbd->scrnScale = scrnScale;
  vbd->quality = quality;
  vbd->pdrawsize = obj->pdrawsize;
  vbd->thickenCont = thickenCont;
  vbd->checksum = imodObjectChecksum(obj, obNum);

  // Process contours and points in them
  indVert = 0;
  indQuadDef = 0;
  indFanDef = cumQuadInd;
  irem = 0;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;
    fullState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co, 
                                        cont->surf, &contState, &surfState);
    if (contProps.gap)
      continue;

    // Add marked contours to the remnant list
    if (cont->flags & ICONT_TEMPUSE) {
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
      vbd->remnantIndList[irem++] = co;
      continue;
    }

    // Set the starting indices for the contour's quads and fans
    if (fullState & handleFlags) {
      packRGBT(&contProps, useFillColor, contRGBT);
      mapit = colors.find(contRGBT);
      indQuad = mapit->second.numInds;
      indFan = mapit->second.numFanInds;
    } else {
      indQuad = indQuadDef;
      indFan = indFanDef;
    }

    // Loop on the points and make sohere or copy the default
    for (pt = 0; pt < cont->psize; pt++) {

      drawsize = imodPointGetSize(obj, cont, pt);
      if (!iobjScat(obj->flags) && !drawsize)
        continue;
      if (drawsize == obj->pdrawsize) {
        copyDefaultSphere(mDefSphVerts, mDefSphInds, numDefSphVert, numDefSphQuad,
                          numDefSphFan, fillType, mVerts, mInds, indVert, indQuad,
                          indFan,  cont->pts[pt].x, cont->pts[pt].y, 
                          cont->pts[pt].z * zscale);
      } else {
        drawsize /= xybin;
        stepRes = sphereResForSize(drawsize);
        makeSphere(drawsize, 2 * stepRes, stepRes, mVerts, mInds, indVert, indQuad,
                 indFan, fillType, cont->pts[pt].x, cont->pts[pt].y, 
                 cont->pts[pt].z * zscale);
      }
    }

    // Save the new indices back where they came from
    if (fullState & handleFlags) {
      mapit->second.numInds = indQuad;
      mapit->second.numFanInds = indFan;
    } else {
      indQuadDef = indQuad;
      indFanDef = indFan;
    }
  }

  imodTrace('b', "cumfan %d after load vert %d quad %d fan %d  irem %d", 
            cumFanInd, indVert, indQuad, indFan, irem);
  /* for (pt = 0; pt < cumFanInd; pt++) {
    imodPrintStderr(" %d", mInds[pt]);
    if ((pt + 1) % 16 == 0 || pt == cumFanInd - 1) imodPrintStderr("\n");
    } */

  // Transfer to GL
  b3dBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, cumFanInd * sizeof(GLuint), mInds);
  b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  b3dBufferSubData(GL_ARRAY_BUFFER, 0, numTriples * 3 * sizeof(GLfloat), mVerts);
  b3dBindBuffer(GL_ARRAY_BUFFER, 0);

  return 0;
}

/*
 * Analyze the given object and set up for contour drawing with VBOs
 */
int VertBufManager::analyzeConts(Iobj *obj, int obNum, int thickenCont, int checkStipple,
                                 int checkTime)
{
  RGBTmap colors;
  pair<RGBTmap::iterator,bool> mapret;
  RGBTmap::iterator mapit;
  DrawProps defProps, contProps;
  VertBufData *vbd = obj->vertBufCont;
  RGBTindices rInd;
  b3dUInt32 contRGBT;
  Icont *cont;
  int handleFlags, nonVboFlags, numRemnant, fullState, skip, numVerts, cumInd, co;
  int numInds, ivert, irem, iDefInd, contState, surfState, ind, pt, psize, match;

  rInd.numFanInds = 0;
  handleFlags = CHANGED_COLOR | CHANGED_TRANS;
  nonVboFlags = CHANGED_3DWIDTH;

  // Check if there is a current VBO and it is all still valid 
  // TODO: handle finegrain
  if (vbd && vbd->vbObj && checkStipple == vbd->checkStipple && 
      thickenCont == vbd->thickenCont && checkTime == vbd->checkTime) {
    match = 1;
    if (thickenCont)
      match = checkSelectedAreRemnants(vbd, obNum);
    if (match && checkStipple) {
      for (co = 0; co < obj->contsize; co++) {
        if ((obj->cont[co].flags & ICONT_STIPPLED) && 
            !numberInList(co, vbd->remnantIndList, vbd->numRemnant, 0)) {
          match = 0;
          break;
        }
      }
    }
    if (match && !Imodv->standalone && obNum == Imodv->imod->cindex.object && 
        vbd->checksum != imodObjectChecksum(obj, obNum))
      match = 0;

    if (match)
      return -1;
  }

  istoreDefaultDrawProps(obj, &defProps);
  numRemnant = 0;
  cumInd = 0;
  numVerts = 0;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;
    setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
    fullState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co, 
                                        cont->surf, &contState, &surfState);

    // Skip a gap without counting it; count ones to be excluded or that can't be handled
    skip = 0;
    if (contProps.gap)
      continue;
    if ((thickenCont && imodvCheckThickerContour(co)) || (fullState & nonVboFlags) || 
        (checkStipple && (cont->flags & ICONT_STIPPLED)))
      skip = 1;

    // Check for point changes that would need to be handled
    if (!skip && cont->store) {
      if (istoreCountItems(cont->store, GEN_STORE_COLOR, 1) || 
          istoreCountItems(cont->store, GEN_STORE_TRANS, 1))
        skip = 1;
      if (!skip && istoreCountItems(cont->store, GEN_STORE_3DWIDTH, 1))
        skip = 1;
      if (!skip && istoreCountItems(cont->store, GEN_STORE_GAP, 1))
        skip = 1;
    }
    if (skip) {
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 1);
      numRemnant++;
      continue;
    }

    // Determine the number of indices needed including restart index
    numVerts += cont->psize;
    numInds = cont->psize + 1;
    if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN))
      numInds++;

    // For a special contour, add to list of RGBT values with the index count if it
    // is not on the list; if it is already on the list add to its count;
    if (fullState & handleFlags) {
      rInd.firstElement = co;
      rInd.numInds = numInds;
      packRGBT(&contProps, 0, contRGBT);
      mapret = colors.insert(pair<b3dUInt32,RGBTindices>(contRGBT, rInd));
      if (mapret.second == false)
        mapret.first->second.numInds += numInds;
    } else {
      cumInd += numInds;
    }
  }      

  if (!numVerts) {
    vbCleanupContVBD(obj);
    return 2;
  }

  vbd = allocateVBDIfNeeded(&obj->vertBufCont);
  if (!vbd)
    return 1;

  // Add up the special set sizes and re-initialize the counts to be starting indexes
  vbd->numRemnant = numRemnant;
  co = -1;
  if (allocateSpecialSets(vbd, colors.size(), cumInd, 0) || 
      processMap(vbd, &colors, cumInd, 1, co)) {
    vbCleanupContVBD(obj);
    return 1;
  }
      
  imodTrace('b',"dfltInd %d  spec sets %d cumind %d  remnant %d", vbd->numIndDefault,
            vbd->numSpecialSets, cumInd, numRemnant);
  
  if (genAndBindBuffers(vbd, numVerts, cumInd) ||
      allocateTempVerts(numVerts) || allocateTempInds(cumInd)) {
    vbCleanupContVBD(obj);
    return 1;
  }

  // Set properties of this VB
  packRGBT(&defProps, 0, vbd->defaultRGBT);
  vbd->checkTime = checkTime;
  vbd->checkStipple = checkStipple;
  vbd->thickenCont = thickenCont;
  vbd->checksum = imodObjectChecksum(obj, obNum);

  // Load the vertex and index temp arrays
  ivert = 0;
  iDefInd = 0;
  irem = 0;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (!imodvCheckContourDraw(cont, co, checkTime))
      continue;
    fullState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co, 
                                        cont->surf, &contState, &surfState);
    if (contProps.gap)
      continue;

    // Add marked contours to the remnant list
    if (cont->flags & ICONT_TEMPUSE) {
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
      vbd->remnantIndList[irem++] = co;
      continue;
    }

    // Copy points onto vertex list
    psize = cont->psize;
    memcpy(&mVerts[3 * ivert], cont->pts, psize * 3 * sizeof(float));
    numInds = psize + 1;
    if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN))
      numInds++;

    // Find index where indices start and increment it
    if (fullState & handleFlags) {
      packRGBT(&contProps, 0, contRGBT);
      mapit = colors.find(contRGBT);
      ind = mapit->second.numInds;
      mapit->second.numInds += numInds;
    } else {
      ind = iDefInd;
      iDefInd += numInds;
    }

    // Set up indices to all points, add one if closed, add restart index;
    for (pt = 0; pt < psize; pt++)
      mInds[ind + pt] = ivert + pt;
    if (numInds > psize + 1) {
      mInds[ind + psize] = ivert;
      mInds[ind + psize + 1] = RESTART_INDEX;
    } else
      mInds[ind + psize] = RESTART_INDEX;
    ivert += psize;
  }
  imodTrace('b',"ivert %d  numVerts*3 %d idefind %d cumind %d", ivert, numVerts * 3,
            iDefInd, cumInd);
  /*for (pt = 0; pt < cumInd; pt++) {
    imodPrintStderr(" %d", mInds[pt]);
    if ((pt + 1) % 16 == 0) imodPrintStderr("\n");
  }
  imodPrintStderr("\n"); */

  // Transfer to GL
  b3dBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, cumInd * sizeof(GLuint), mInds);
  b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  b3dBufferSubData(GL_ARRAY_BUFFER, 0, numVerts * 3 * sizeof(GLfloat), mVerts);
  b3dBindBuffer(GL_ARRAY_BUFFER, 0);
  return 0;
}

/*
 * Returns 1 if all contours in the remnant list are on selection list
 */
int VertBufManager::checkSelectedAreRemnants(VertBufData *vbd, int obNum)
{
  Iindex *index;
  if (Imodv->imod->cindex.object == obNum && Imodv->imod->cindex.contour >= 0 &&
      !numberInList(Imodv->imod->cindex.contour, vbd->remnantIndList, vbd->numRemnant, 0))
    return 0;
  for (int i = 0; i < ilistSize(Imodv->vi->selectionList); i++) {
    index = (Iindex *)ilistItem(Imodv->vi->selectionList, i);
    if (index->object == obNum && index->contour >= 0 &&
        !numberInList(index->contour, vbd->remnantIndList, vbd->numRemnant, 0))
      return 0;
  }
  return 1;
}

/*
 * When drawing nontrans and default is trans, call this to check the remnants;
 * if it is all trans, then check all the special sets for any non-trans;
 * If everything is trans, set flag and give nonzero return
 */
int VertBufManager::checkAllTrans(Iobj *obj, VertBufData *vbd, int &remnantMatchesTrans)
{
  float red, green, blue;
  int trans, j, specialNonTrans = 0;

  remnantMatchesTrans = istoreTransStateMatches(vbd->remnantStore, 0);
  if (!remnantMatchesTrans) {
    for (j = 0; j < vbd->numSpecialSets; j++) {
      unpackRGBT(vbd->rgbtSpecial[j], red, green, blue, trans);
      if (!trans) {
        specialNonTrans = 1;
        break;
      }
    }
    if (!specialNonTrans) {
      obj->flags |= IMOD_OBJFLAG_TEMPUSE;
      return 1;
    }
  }
  return 0;
}

/* 
 * Packs the vertex/normal array into a temporary array, scaling by the given Z scale,
 * and loads this into GL_ARRAY_BUFFER, which must already be bound
 */
int VertBufManager::loadVertexNormalArray(Imesh *mesh, float zscale, int fillType)
{
  // Temporary array for vertices
  int i, numVert = 0;
  Ipoint *vert = mesh->vert;
  if (allocateTempVerts(mesh->vsize))
    return 1;
  
  // Load the vertices and finish with buffer for now
  if (fillType) {
    for (i = 0; i < mesh->vsize; i += 2) {
      mVerts[numVert++] = vert[i + 1].x;
      mVerts[numVert++] = vert[i + 1].y;
      mVerts[numVert++] = vert[i + 1].z;
      mVerts[numVert++] = vert[i].x;
      mVerts[numVert++] = vert[i].y;
      mVerts[numVert++] = vert[i].z * zscale;
    }
  } else {
    for (i = 0; i < mesh->vsize; i += 2) {
      mVerts[numVert++] = vert[i].x;
      mVerts[numVert++] = vert[i].y;
      mVerts[numVert++] = vert[i].z * zscale;
    }
  }
  b3dBufferSubData(GL_ARRAY_BUFFER, 0, numVert * sizeof(GLfloat), mVerts);
  return 0;
}

/*
 * Convert color and trans values directly or from a DrawProps to one 32-bit number
 */
void VertBufManager::packRGBT(float red, float green, float blue, int trans, b3dUInt32 &rgbtVal)
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

void VertBufManager::packRGBT(DrawProps *props, int useFill, b3dUInt32 &rgbtVal)
{
  if (useFill)
    packRGBT(props->fillRed, props->fillGreen, props->fillBlue, props->trans, rgbtVal);
  else
    packRGBT(props->red, props->green, props->blue, props->trans, rgbtVal);
}

/* 
 * Unpack the color and trans components from the rgbt value
 */
void VertBufManager::unpackRGBT(b3dUInt32 rgbtVal, float &red, float &green, float &blue, int &trans)
{
  red = (float)(((rgbtVal >> 24) & 255) / 255.);
  green = (float)(((rgbtVal >> 16) & 255) / 255.);
  blue = (float)(((rgbtVal >> 8) & 255) / 255.);
  trans = rgbtVal & 255;
}

void VertBufManager::unpackRGBT(b3dUInt32 rgbtVal, int useFill, DrawProps *props)
{
  if (useFill)
    unpackRGBT(rgbtVal, props->fillRed, props->fillGreen, props->fillBlue, 
                 props->trans);
  else
    unpackRGBT(rgbtVal, props->red, props->green, props->blue, props->trans);
}

/*
 * Free all the temporary arrays after drawing if any were used
 */
void VertBufManager::clearTempArrays()
{
  B3DFREE(mInds);
  B3DFREE(mVerts);
  mInds = NULL;
  mIndSize = 0;
  mVerts = NULL;
  mVertSize = 0;
  B3DFREE(mDefSphInds);
  B3DFREE(mDefSphVerts);
  mDefSphInds = NULL;
  mDefSphIndSize = 0;
  mDefSphVerts = NULL;
  mDefSphVertSize = 0;
}

/*
 * Allocate arrays in the VBD for the special sets and the remnants and set some values
 * in the VBD
 */
int VertBufManager::allocateSpecialSets(VertBufData *vbd, int numSets, int cumInd,
                                        int sphere)
{
  vbd->numSpecialSets = numSets;
  vbd->numIndDefault = cumInd;
  if (vbd->numSpecialSets > vbd->specialSize) {
    B3DFREE(vbd->numIndSpecial);
    B3DFREE(vbd->rgbtSpecial);
    B3DFREE(vbd->numFanIndSpecial);
    vbd->numIndSpecial = B3DMALLOC(int, vbd->numSpecialSets);
    vbd->rgbtSpecial = B3DMALLOC(b3dUInt32, vbd->numSpecialSets);
    if (sphere)
      vbd->numFanIndSpecial = B3DMALLOC(int, vbd->numSpecialSets);
    vbd->specialSize = vbd->numSpecialSets;
    if (!vbd->numIndSpecial || !vbd->rgbtSpecial || (sphere && !vbd->numFanIndSpecial))
      return 1;
  }
  if (vbd->numRemnant > vbd->remListSize) {
    B3DFREE(vbd->remnantIndList);
    vbd->remListSize = vbd->numRemnant;
    vbd->remnantIndList = B3DMALLOC(int, vbd->remListSize);
    if (!vbd->remnantIndList)
      return 1;
  }
  return 0;
}

/*
 * Take the map from RGB to index counts produced during initial scan, save the index
 * counts in the VBD, and convert the index counts to starting indices (for quads and fans
 * if doing spheres).
 */
int VertBufManager::processMap(VertBufData *vbd, RGBTmap *colors, int &cumInd, 
                             int indPerItem, int &cumFanInd)
{
  ReverseMap elements;
  ReverseMap::iterator revit;
  RGBTmap::iterator mapit;
  pair<ReverseMap::iterator,bool> mret;
  int i;

  // Build a reverse map: this will put them in order by the first index where the RGBT
  // was encountered
  for (mapit = colors->begin(); mapit != colors->end(); mapit++) {
    mret = elements.insert(pair<int,b3dUInt32>(mapit->second.firstElement, mapit->first));
    if (!mret.second)
      return 1;
  }

  // Now going in order by first index, assign the starting points for each portion
  // of index array
  revit = elements.begin();
  for (i = 0; i < vbd->numSpecialSets; i++) {
    mapit = colors->find(revit->second);
    vbd->rgbtSpecial[i] = mapit->first;
    vbd->numIndSpecial[i] = mapit->second.numInds * indPerItem;
    mapit->second.numInds = cumInd;
    cumInd += vbd->numIndSpecial[i];
    revit++;
  }

  if (cumFanInd < 0)
    return 0;
  
  // Do same for the fan indices if any
  cumFanInd += cumInd;
  revit = elements.begin();
  for (i = 0; i < vbd->numSpecialSets; i++) {
    mapit = colors->find(revit->second);
    vbd->numFanIndSpecial[i] = mapit->second.numFanInds;
    mapit->second.numFanInds = cumFanInd;
    cumFanInd += vbd->numFanIndSpecial[i];
    revit++;
  }
  return 0;
}

// Does just that
VertBufData *VertBufManager::allocateVBDIfNeeded(VertBufData **vbdp)
{
  if (!*vbdp)
    *vbdp = vbDataNew();
  return *vbdp;
}

/*
 * If there are no existing buffers or they are not big enough, generate vertex buffers 
 * and allocate to needed size.  In any case, bind them.
 */
int VertBufManager::genAndBindBuffers(VertBufData *vbd, int numVerts, int cumInd) 
{
  if (!vbd->vbObj || 3 * numVerts > vbd->vboSize) {
    vbd->vboSize = 3 * numVerts;
    if (vbd->vbObj)
      b3dDeleteBuffers(1, (GLuint *)&vbd->vbObj);
    b3dGenBuffers(1, (GLuint *)&vbd->vbObj);
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);
    b3dBufferData(GL_ARRAY_BUFFER, 3 * numVerts * sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    if (glGetError())
      return 1;
  } else 
    b3dBindBuffer(GL_ARRAY_BUFFER, vbd->vbObj);

  if (!vbd->ebObj || cumInd > vbd->eboSize) {
    vbd->eboSize = cumInd; 
    if (vbd->ebObj)
      b3dDeleteBuffers(1, (GLuint *)&vbd->ebObj);
    b3dGenBuffers(1, (GLuint *)&vbd->ebObj);
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);
    b3dBufferData(GL_ELEMENT_ARRAY_BUFFER,  cumInd * sizeof(GLuint), NULL,GL_STATIC_DRAW);
    if (glGetError())
      return 1;
  } else 
    b3dBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbd->ebObj);

  return 0;
}

/*
 * Routines for allocating temporrary arrays, for vertices and indexes
 */
int VertBufManager::allocateTempVerts(int numVerts) 
{
  if (3 * numVerts > mVertSize) {
    B3DFREE(mVerts);
    mVertSize = 3 * numVerts;
    mVerts = B3DMALLOC(GLfloat, mVertSize);
    if (!mVerts) {
      mVertSize = 0;
      return 1;
    }
  }
  return 0;
}

int VertBufManager::allocateTempInds(int cumInd) 
{
  if (cumInd > mIndSize) {
    B3DFREE(mInds);
    mIndSize = cumInd;
    mInds = B3DMALLOC(GLuint, mIndSize);
    if (!mInds) {
      mIndSize = 0;
      return 1;
    }
  }
  return 0;
}

// Allocate arrays for the default sized sphere
int VertBufManager::allocateDefaultSphere(int numVerts, int numInds, int needNorm)
{
  int needed = numVerts * (1 + (needNorm > 0 ? 1 : 0));
  if (needed > mDefSphVertSize) {
    B3DFREE(mDefSphVerts);
    mDefSphVertSize = needed;
    mDefSphVerts = B3DMALLOC(GLfloat, 3 * needed);
    if (!mDefSphVerts) {
      mDefSphVertSize = 0;
      return 1;
    }
  }
  if (numInds > mDefSphIndSize) {
    B3DFREE(mDefSphInds);
    mDefSphIndSize = numInds;
    mDefSphInds = B3DMALLOC(GLuint, numInds);
    if (!mDefSphInds) {
      mDefSphIndSize = 0;
      return 1;
    }
  }
  return  0;
}

// Convenience functions for putting normals and vertices in the arrays
inline void VertBufManager::loadNormal(float x, float y, float z)
{
  mVertex[6*mIndVert] = x;
  mVertex[6*mIndVert+1] = y;
  mVertex[6*mIndVert+2] = z;
}
inline void VertBufManager::loadVertex(float x, float y, float z)
{
  int indBase = (3 + mNormOffset) * mIndVert + mNormOffset;
  mVertex[indBase] = x + mXadd;
  mVertex[indBase+1] = y + mYadd;
  mVertex[indBase+2] = z + mZadd;
  mIndVert++;
}

#define PI 3.1415927
#define CACHE_SIZE 50
#define sinCache2a sinCache1a
#define cosCache2a cosCache1a

/*
 * Makes vertices and indices for a sphere.
 * This was abstracted and heavily adapted from gluSphere in quad.c of the mesa library.
 * That file was marked:
 *
 * SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
 * Copyright (C) 1991-2000 Silicon Graphics, Inc. All Rights Reserved.
 *
 * but I don't consider what is left from there to be a "substantial portion of the 
 * Software" so I don't see the need to propagate this information with the binaries.
 *
 * If needNorm is 1 it creates vertices and normals just as in the original gluSphere
 * If needNorm is 0 it creates a set of line strips to avoid double-drawing lines
 * and creating intensity differences with transparency
 * if needNorm is -1 is just makes one line strip for longitudinal lines, for point 
 * drawing.
 * It packs the data sequentially in the given arrays, maintaining the indices for
 * the next elements, and adds the given coordinates.
 */
int VertBufManager::makeSphere(float radius, int slices, int stacks, GLfloat *vertex, 
                               GLuint *index, int &indVert, int &indQuad, int &indFan,
                               int needNorm, float xadd, float yadd, float zadd)
{
  int i,j;
  float sinCache1a[CACHE_SIZE];
  float cosCache1a[CACHE_SIZE];
  float sinCache1b[CACHE_SIZE];
  float cosCache1b[CACHE_SIZE];
  float sinCache2b[CACHE_SIZE];
  float cosCache2b[CACHE_SIZE];
  float angle;
  float zHigh;
  float sintemp2 = 0.0, sintemp3 = 0.0;
  float costemp3 = 0.0;
  int start, finish;

  if (slices >= CACHE_SIZE) slices = CACHE_SIZE-1;
  if (stacks >= CACHE_SIZE) stacks = CACHE_SIZE-1;
  if (slices < 2 || stacks < 1 || radius < 0.0) {
    return 1;
  }

  /* Cache is the vertex locations cache */
  /* Cache2 is the various normals at the vertices themselves */

  for (i = 0; i < slices; i++) {
    angle = 2 * PI * i / slices;
    sinCache1a[i] = sin((double)angle);
    cosCache1a[i] = cos((double)angle);
  }

  for (j = 0; j <= stacks; j++) {
    angle = PI * j / stacks;
    sinCache2b[j] = sin((double)angle);
    cosCache2b[j] = cos((double)angle);
    sinCache1b[j] = radius * sin((double)angle);
    cosCache1b[j] = radius * cos((double)angle);
  }
  /* Make sure it comes to a point */
  sinCache1b[0] = 0;
  sinCache1b[stacks] = 0;

  sinCache1a[slices] = sinCache1a[0];
  cosCache1a[slices] = cosCache1a[0];

  /* Do ends of sphere as TRIANGLE_FAN's
   */
  start = 1;
  finish = stacks - 1;

  mVertex = vertex;
  mIndVert = indVert;
  mXadd = xadd;
  mYadd = yadd;
  mZadd = zadd;
  mNormOffset = needNorm > 0 ? 3 : 0;

  /* Low end first (j == 0 iteration) */
  sintemp2 = sinCache1b[1];
  zHigh = cosCache1b[1];
  sintemp3 = sinCache2b[1];
  costemp3 = cosCache2b[1];
  if (needNorm > 0) {
    index[indFan++] = mIndVert;
    loadNormal(sinCache2a[0] * sinCache2b[0],
               cosCache2a[0] * sinCache2b[0],
               cosCache2b[0]);
  }
  loadVertex(0., 0., radius);
  if (needNorm > 0) {
    for (i = slices; i >= 0; i--) {
      index[indFan++] = mIndVert;
      loadNormal(sinCache2a[i] * sintemp3,
                 cosCache2a[i] * sintemp3,
                 costemp3);
      loadVertex(sintemp2 * sinCache1a[i],
                 sintemp2 * cosCache1a[i], zHigh);
    }
    index[indFan++] = RESTART_INDEX;
  } else {
    for (i = 0; i <= slices; i++) 
      loadVertex(sintemp2 * sinCache1a[i],
                 sintemp2 * cosCache1a[i], zHigh);
  }

  for (j = start; j < finish; j++) {
    zHigh = cosCache1b[j+1];
    sintemp2 = sinCache1b[j+1];
    sintemp3 = sinCache2b[j+1];
    costemp3 = cosCache2b[j+1];

    // QUAD STRIP
    if (needNorm > 0) {
      for (i = 0; i <= slices; i++) {
        index[indQuad++] = mIndVert;
        loadNormal(sinCache2a[i] * sintemp3,
                   cosCache2a[i] * sintemp3,
                   costemp3);
        loadVertex(sintemp2 * sinCache1a[i],
                   sintemp2 * cosCache1a[i], zHigh);
        
        // Reverse the order of the previous disk of points the first time because it
        // was added in inverse order
        if (j == start)
          index[indQuad++] = mIndVert - 2 * i - 2;
        else
          index[indQuad++] = mIndVert - slices - 2;
      }
      index[indQuad++] = RESTART_INDEX;
    } else {
      for (i = 0; i <= slices; i++)
        loadVertex(sintemp2 * sinCache1a[i],
                   sintemp2 * cosCache1a[i], zHigh);
    }
  }

  /* High end last (j == stacks-1 iteration) */
  if (needNorm > 0) {
    index[indFan++] = mIndVert;
    loadNormal(sinCache2a[stacks] * sinCache2b[stacks],
               cosCache2a[stacks] * sinCache2b[stacks],
               cosCache2b[stacks]);
    loadVertex(0.0, 0.0, -radius);
    for (i = 0; i <= slices; i++) {
      
      // Reverse the order of the points if they are the first disk
      if (start < finish)
        index[indFan++] = mIndVert + i - slices - 2;
      else
        index[indFan++] = mIndVert - i - 2;
    }
    index[indFan++] = RESTART_INDEX;
  } else {

    // Now make the indices for a line strip along all longitude lines
    loadVertex(0.0, 0.0, -radius);
    index[indQuad++] = indVert;
    for (i = 0; i < slices; i++) {
      if (i % 2) {
        for (j = stacks - 2; j >= 0; j--)
          index[indQuad++] = indVert + 1 + i + j * (slices + 1);
        index[indQuad++] = indVert;
      } else {
        for (j = 0; j <= stacks - 2; j++)
          index[indQuad++] = indVert + 1 + i + j * (slices + 1);
        index[indQuad++] = mIndVert - 1;
      }     
    }
    index[indQuad++] = RESTART_INDEX;
    
    // And indexes for line strips along each disk unless doing points
    if (needNorm == 0) {
      for (j = 0; j <= stacks - 2; j++) {
        for (i = 0; i <= slices; i++)
          index[indQuad++] = indVert + 1 + i + j * (slices + 1);
        index[indQuad++] = RESTART_INDEX;
      }
    }
    
  }
  indVert = mIndVert;
  return 0;
}

/*
 * Returns the number of vertices in a sphere with the given number of slices and
 * stacks, and the number of indices for quadrilaterals and triangle fans, including
 * restart indices.  Quads and fans are assumed if fillType > 0, otherwise line strips
 * for lines or points
 */
int VertBufManager::sphereCounts(int slices, int stacks, int fillType, int &numQuad,
                                 int &numFan)
{
  int numVert;
  if (fillType > 0 ) {
    numQuad = (2 * (slices + 1) + 1) * (stacks - 2);
    numFan = 2 * (slices + 3);
  } else {
    numQuad = stacks * slices + 2;
    if (fillType == 0)
      numQuad += (stacks - 1) * (slices + 2);
    numFan = 0;
  }
  numVert = (slices + 1)  * (stacks - 1) + 2;
  return numVert;
}

/*
 * Copies the default sized sphere into the running vertex and index arrays, adjusting
 * coordinates and indices as appropriate
 */
void VertBufManager::copyDefaultSphere(GLfloat *defVert, GLuint *defInd, int numVert,
                                       int numQuad, int numFan,  int needNorm,
                                       GLfloat *vertex, GLuint *index, int &indVert,
                                       int &indQuad, int &indFan, float xadd, float yadd,
                                       float zadd)
{
  int i, idef;
  int trueIndex;
  mVertex = vertex;
  mIndVert = indVert;
  mXadd = xadd;
  mYadd = yadd;
  mZadd = zadd;
  mNormOffset = needNorm > 0 ? 3 : 0;
  idef = 0;

  // Assume the quad indexes are right before the fan indexes
  for (i = 0; i < numQuad; i++) {
    trueIndex = defInd[idef] != RESTART_INDEX ? 1 : 0;
    index[indQuad++] = defInd[idef++] + indVert * trueIndex;
  }
  for (i = 0; i < numFan; i++) {
    trueIndex = defInd[idef] != RESTART_INDEX ? 1 : 0;
    index[indFan++] = defInd[idef++] + indVert * trueIndex;
  }
  idef = 0;
  for (i = 0; i < numVert; i++) {
    if (needNorm > 0) {
      loadNormal(defVert[idef], defVert[idef+1], defVert[idef+2]);
      idef += 3;
    }
    loadVertex(defVert[idef], defVert[idef+1], defVert[idef+2]);
    idef += 3;
  }
  indVert = mIndVert;
}

