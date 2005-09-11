/*
 *  imodmesh.c -- Add mesh data to a model.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end
*/

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>

#include "imodel.h"
#include "mkmesh.h"

void switchskin(Imod *imod, Imod *simod, int append, int list[], int nlist,
                Ipoint *scale);
void reresobj(Iobj *obj, double dist);
void ReduceObj(Iobj *obj, double dist);
void resecobj(Iobj *obj, int minz, int maxz, int incz);
static void imodMeshesRescaleNormal
(Imesh *inMesh, int *meshsize, Ipoint *spnt);
static void imodMeshesDeleteZRange
(Imesh *mesh, int *meshsize, int minz, int maxz, int resol);
static int imodMeshesDeleteRes(Imesh *mesh, int *size, int resol);
static void imodMeshDeleteZRange(Imesh *mesh, int minz, int maxz);
int cleanzero(Imod *imod);
Imesh *imeshReduce(Imesh *mesh, Ipoint *scale, int filterPct, int itr);
int ObjOnList(int ob, int list[], int nlist);

#define DEFAULT_TOL  0.25

static int imodmesh_usage(char *prog, int retcode)
{
  fprintf(stderr, "%s usage: %s [options] [input files...]\n",prog,prog);
  fprintf(stderr, "options:\n");
  fprintf(stderr, "\t-c  \tCap ends of object.\n");
  fprintf(stderr, "\t-C  \tCap all unconnected ends of object.\n");
  fprintf(stderr, "\t-D list\tDo not cap ends unconnected to Z values in the list.\n");
  fprintf(stderr, "\t-p #\tOnly connect contours that overlap by the "
          "given percentage.\n");
  fprintf(stderr, "\t-s  \tConnect contours across sections with no contours.\n");
  fprintf(stderr, "\t-P #\tDo multiple passes to connect contours farther apart in Z.\n");
  fprintf(stderr, "\t-S  \tUse surface numbers for connections.\n");
  fprintf(stderr, "\t-I  \tIgnore time values and connect across times.\n");
  fprintf(stderr, "\t-f  \tForce more connections to non-overlapping contours.\n");
  fprintf(stderr, "\t-t list\tRender open contour objects in list as tubes.\n");
  fprintf(stderr, "\t-d #\tSet diameter for tubes (default is 3D line width).\n");
  fprintf(stderr, "\t-E  \tCap ends of tubes.\n");
  fprintf(stderr, "\t-T  \tDo time consuming calculations, "
          "may help reduce artifacts.\n");
    
  fprintf(stderr, "\t-o list\tDo operations only on objects in list"
          " (ranges allowed).\n");
  fprintf(stderr, "\t-r #\tResolution factor in pixels (use -R instead).\n");
  fprintf(stderr, "\t-R #\tTolerance (maximum error) for point reduction"
          " (default %.2f).\n", DEFAULT_TOL);
    
  fprintf(stderr, "\t-i #\tOnly mesh sections at the given z increment.\n");
  fprintf(stderr, "\t-z #,#,#  Only mesh sections within the given z range "
          "at a z increment.\n");
  fprintf(stderr, "\t-l \tMark new meshes as low-resolution and keep "
          "high res meshes.\n");
  fprintf(stderr, "\t-x #,#\tClip mesh outside given lower and upper limits in X.\n");
  fprintf(stderr, "\t-y #,#\tClip mesh outside given lower and upper limits in Y.\n");

  fprintf(stderr, "\t-a \tAppend mesh data to object, replacing mesh"
          " in same z range.\n");
  fprintf(stderr, "\t-e  \tErase meshes, overrides other options.\n");
  fprintf(stderr, "\t-n \tRescale normals using the -Z value "
          "for existing meshes.\n");
  fprintf(stderr, "\t-Z #\tNormal scaling z multiplier.\n");
  fprintf(stderr, "\t-B  \tMake mesh backward-compatible to IMOD before 3.6.14.\n");
  if (retcode)
    exit(retcode);
  return(retcode);
}

#define DEFAULT_VALUE 0x7fffffff
#define DELETE_INDEX  -123456
#define LOWRES_INCZ  4
#define LOWRES_TOL   2.

int main(int argc, char **argv)
{
  Imod *imod, *simod;
  Iobj *obj, *tobj;
  Ipoint spnt;
  Ipoint max, maxsave;
  FILE *fout;
  char *rst;
  int ob, i, ch;
  int sob = -1; /* only use single object */
  int erase_mesh = FALSE;
  int renorm     = FALSE;
  int cap = IMESH_CAP_OFF;
  int skip = FALSE;
  double res = -1.0f;
  int surf = FALSE;
  int stray = FALSE;
  int norm  = TRUE;
  int times = TRUE;
  int capTubes = FALSE;
  unsigned int flags = 0;
  double overlap = 0.0;
  int reduce = -1;
  double tol = -1.;
  int resol = 0;
  int passes = 1;

  /* Added 2.00 mesh only given sections. */
  int minz = DEFAULT_VALUE;
  int maxz = DEFAULT_VALUE;
  int incz = -1;
  float zscale = 1.0;
  int append = 0;
  Imesh *tmsh;
  int    tmshsize, m;
  Ipoint triMin = {-1.e30, -1.e30, -1.e30};
  Ipoint triMax = {1.e30, 1.e30, 1.e30};
  
  extern double meshDiameterSize;
  extern int newPolyNorm;

  int nlist = 0;
  int *list;
  int cap_skip_nz = 0;
  int *cap_skip_zlist;
  int ilist;
  int ntube_list = 0;
  int *tube_list;
  char *progname = imodProgName(argv[0]);
  newPolyNorm = 1;

  if (argc < 1){
    imodVersion(progname);
    imodCopyright();
    imodmesh_usage(progname, -1);
  }
    
  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
            
      case 'd':
        meshDiameterSize = atof(argv[++i]);
      break;
      case 'o': /* select object list */
        list = parselist(argv[++i], &nlist);
        if (!list) {
          fprintf(stderr, "%s: Error parsing object list\n", progname);
          exit(3);
        }
        break;
      case 'r':
        res = atof(argv[++i]);
        fprintf(stderr, "The -r option is obsolete; -R gives a more "
                "faithful rendering\n of surfaces around corners and "
                "should be used instead\n");
        tol = 0.0;
        break;
      case 'R':
        /*            reduce = atoi(argv[++i]); */
        tol = atof(argv[++i]);
        if (tol < 0.)
          tol = 0.;
        break;
      case 'c':
        cap = IMESH_CAP_END;
        break;
      case 'C':
        cap = IMESH_CAP_ALL;
        break;
      case 'D': /* Do not cap at Z in list. */
        cap_skip_zlist = parselist(argv[++i], &cap_skip_nz);
        if (!cap_skip_zlist) {
          fprintf(stderr, "%s: Error parsing Z list\n", progname);
          exit(3);
        }
        break;
      case 'e': /* erase meshes */
        erase_mesh = TRUE;
        break;
      case 'f':
        stray = TRUE;
        break;
      case 'F':
        cap += 2;
      break;
      case 'E':
        capTubes = TRUE;
        break;
        
      case 'n':
        renorm = TRUE;
        break;
        
      case 'N':
        norm = FALSE;
        break;
        
      case 't':
        tube_list = parselist(argv[++i], &ntube_list);
        if (!tube_list) {
          fprintf(stderr, "%s: Error parsing list of objects to "
                  "mesh as tubes\n", progname);
          exit(3);
        }
        break;
            
      case 'T':
        flags |= IMESH_MK_FAST;
        break;
        
      case 's':
        skip = TRUE;
        break;
      case 'S':
        surf = TRUE;
      break;
      case 'I':
        times = FALSE;
        break;
        
      case 'z':
        sscanf(argv[++i], "%d%*c%d%*c%d", &minz, &maxz, &incz);
        break;
      case 'i':
        incz = atoi(argv[++i]);
        if (incz < 1)
          incz = 1;
        break;
      case 'x':
        sscanf(argv[++i], "%f%*c%f", &triMin.x, &triMax.x);
        break;
      case 'y':
        sscanf(argv[++i], "%f%*c%f", &triMin.y, &triMax.y);
        break;
      case 'Z':
        zscale = atof(argv[++i]);
        break;
      case 'a':
        append = TRUE;
        break;
      case 'p':
        overlap = atof(argv[++i]) / 100.;
        break;
      case 'l':
        resol = 1;
        break;
      case 'P':
        passes = atoi(argv[++i]);
        if (passes < 1)
          passes =1;
        break;
      case 'B':
        newPolyNorm = 0;
        break;
        
      case '?':
        imodmesh_usage(progname, -1);
        break;
      default:
        fprintf(stderr, "%s: unknown option %s\n", progname, argv[i]);
        imodmesh_usage(progname, -1);
        break;
               
      }
    } else
      break;
  }

  if (i >= argc)
    imodmesh_usage(progname, -1);
    
  if (skip)  flags |= IMESH_MK_SKIP;
  if (stray) flags |= IMESH_MK_STRAY;
  if (surf)  flags |= IMESH_MK_SURF;
  if (times)  flags |= IMESH_MK_TIME;
  if (norm)  flags |= IMESH_MK_NORM;
  if (capTubes)  flags |= IMESH_CAP_TUBE;

  if (resol) {
    if (incz < 0) {
      incz = LOWRES_INCZ;
      printf("Setting Z increment to %d for low resolution mesh\n", 
             incz);
    }
    if (tol < 0.) {
      tol = LOWRES_TOL;
      printf("Setting tolerance to %.2f for low resolution mesh\n", 
             tol);
    }
  } else if (tol < 0.) {
    tol = DEFAULT_TOL;
    printf("Setting tolerance to %.2f for point reduction\n", tol);
  }
  if (incz < 1)
    incz = 1;

  /* Skin multiple models in serial. */
  while(i < argc){

    printf("Model: %s\n", argv[i]);
        
    simod = imodRead(argv[i]); /* The final output model. */
    if (!simod){
      fprintf(stderr, "%s: Error reading model %s\n",
              progname, argv[i]);
      exit(3);
    }
    /* delete the meshes completely */
    for (ob = 0; ob < simod->objsize; ob++){
      obj = &(simod->obj[ob]);
      if (obj->meshsize){
        imodMeshesDelete(obj->mesh, obj->meshsize);
        obj->meshsize = 0;
      }
    }

    imod  = imodRead(argv[i]); /* The model we will edit for skinning. */
    if (!imod){
      fprintf(stderr, "%s: Error reading model %s\n",
              progname, argv[i]);
      exit(3);
    }
        
    /* Skinning doesn't like dirty models.  But keep empty objs (DNM) */
    /* DNM: also, don't clip to maximum area! */
    maxsave.x = imod->xmax;
    maxsave.y = imod->ymax;
    maxsave.z = imod->zmax;
    imodel_maxpt(imod, &max);
    imod->xmax = max.x + 1.;
    imod->ymax = max.y + 1.;
    imod->zmax = max.z + 1.;
    imodel_model_clean(imod, 1);
    cleanzero(imod);
    imod->xmax = maxsave.x;
    imod->ymax = maxsave.y;
    imod->zmax = maxsave.z;
        
    spnt.x = imod->xscale;
    spnt.y = imod->yscale;
    spnt.z = imod->zscale * zscale;

    /* If there is a list of capping exclusion Z values, check if 0 and
       model max are on list, and extend range by 1 */
    if (cap_skip_nz) {
      cap_skip_zlist = (int *)realloc(cap_skip_zlist, (cap_skip_nz + 3) * 
                                      sizeof(int));
      if (!cap_skip_zlist) {
        fprintf(stderr, "%s: Error getting memory for bigger cap exclusion"
                " list\n", progname);
        exit(3);
      }
    }
    for (ilist = 0; ilist < cap_skip_nz; ilist++) {
      if (!cap_skip_zlist[ilist]) {
        cap_skip_zlist[cap_skip_nz++] = -1;
        break;
      }
    }
    for (ilist = 0; ilist < cap_skip_nz; ilist++) {
      if (cap_skip_zlist[ilist] == floor (max.z + 0.5) ) {
        cap_skip_zlist[cap_skip_nz++] = floor(max.z + 1.5);
        break;
      }
    }

    if (erase_mesh)
      printf("Erasing mesh from objects.\n");

    for (ob = 0; ob < imod->objsize; ob++){
      obj = &(imod->obj[ob]);
      if (!obj->contsize)
        continue;
      if (ObjOnList(ob, list, nlist)) {
        if (ntube_list > 0 && ObjOnList(ob, tube_list, ntube_list))
          flags |= IMESH_MK_TUBE;
        else
          flags &= ~IMESH_MK_TUBE;

        if (erase_mesh){
          if (obj->meshsize){
            imodMeshesDeleteZRange(obj->mesh, &obj->meshsize,
                                   minz, maxz, resol);
          }
          /*  }else if (reduce >= 0){
              imeshReduce(obj->mesh, &spnt, 50, reduce); */
        }else if (renorm){
          spnt.x = spnt.y = 1.0f;
          spnt.z = zscale;
          imodMeshesRescaleNormal(obj->mesh, &obj->meshsize,
                                  &spnt);
        }else{
          if (!iobjScat(obj->flags)){
            tmshsize = 0;
            if (obj->meshsize){
              /* DNM: if appending, delete defined range 
                 and save mesh to add to later */
              if (append) {
                imodMeshesDeleteZRange(obj->mesh, 
                                       &obj->meshsize,
                                       minz, maxz, resol);
              } else 
                imodMeshesDeleteRes(obj->mesh,
                                    &obj->meshsize, resol);
              tmsh = obj->mesh;
              tmshsize = obj->meshsize;
              obj->meshsize = 0;
              obj->mesh = NULL;
            }
            printf("Meshing object  %d\n",ob+1);
            fflush(stdout);
            resecobj(obj, minz, maxz, incz);
            ReduceObj(obj, tol);
            reresobj(obj, res);
            SkinObject(obj, &spnt, overlap, cap,
                       cap_skip_zlist, cap_skip_nz, incz,
                       flags, passes, triMin, triMax, NULL);
            /* set resolution flag now */
            for(m = 0; m < obj->meshsize; m++) {
              obj->mesh[m].flag |= (resol << 
                                    IMESH_FLAG_RES_SHIFT);
            }
            if (tmshsize){
              /* If appending, add new mesh to saved mesh 
                 then put it back into this object's mesh */
              for(m = 0; m < obj->meshsize; m++)
                tmsh = imodel_mesh_add(&obj->mesh[m],
                                       tmsh, &tmshsize);
              if (obj->meshsize)
                free(obj->mesh);
              obj->mesh = imeshReMeshNormal(tmsh, &tmshsize,
                                            &spnt, resol); 
              obj->meshsize = tmshsize;
            }

          }else
            printf("Skipping object %d\n", ob+1);
        }
      }

      /* regardless of what happened to the mesh, now take the mesh
         from the working model and assign it to the output model */
      simod->obj[ob].mesh      = obj->mesh;
      simod->obj[ob].meshsize  = obj->meshsize;
      obj->mesh     = NULL;
      obj->meshsize = 0;
             
    }
        
    /* Save backup of Model to Model~ */
    if (imodBackupFile(argv[i])) {
      fprintf(stderr, "%s: Error, couldn't create backup", progname);
      exit(3);
    }

    fout = fopen(argv[i], "wb");
    if (!fout){
      fprintf(stderr, "%s: Error, couldn't open output.", progname);
      exit(3);
    }

    /*  switchskin(imod, simod, append, list, nlist, &spnt); */
        
    /*    if (simod->flags & IMODF_FLIPYZ)
     *         imodFlipYZ(simod);
     */
        
    imodWrite(simod, fout);
    fclose(fout);
    imodDelete(imod);
    imodDelete(simod);
    i++;
  }
  exit(0);
}


void resecobj(Iobj *obj, int minz, int maxz, int incz)
{
  Icont *cont;
  int co,p,pt,nco;
  int z,tz;
  float zval;
  int rmcont;


  if (incz <= 0) incz = 1;
  if ((minz == DEFAULT_VALUE) && 
      (maxz == DEFAULT_VALUE) && 
      (incz == 1))
    return;

  /* printf("Z section filter from %d to %d, step by %d\n", minz, maxz, incz);
     printf("%d contours\n", obj->contsize); */

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    rmcont = FALSE;
        
    if (!cont->psize) rmcont = TRUE;

    /*
     * find the zvalue of the contour.
     */
    if (!rmcont){
      for(zval = 0.0f, p = 0; p < cont->psize; p++)
        zval += cont->pts[p].z;
      zval /= (float)cont->psize;
    }
    if (zval > 0.0f) 
      zval += 0.5f;
    else
      zval -= 0.5f;
    z = zval;
        
    /*
     * Check the Z value.
     */
    if ((minz != DEFAULT_VALUE) && (z < minz))
      rmcont = TRUE;

    if ((maxz != DEFAULT_VALUE) && (z > maxz))
      rmcont = TRUE;
        
    /*
      if ((rmcont == FALSE) && (incz > 1)){
      rmcont = TRUE;
      for(tz = minz; tz <= maxz; tz+= incz){
      if (z == tz) rmcont = FALSE;
      }
      }
    */
    if (z%incz)
      rmcont = TRUE;

    if (rmcont){

      /* printf("removed cont %d : %d\n", co, z); */
      imodObjectRemoveContour(obj, co);
      co--;

    }

  }

  /*
   * ToDO multiply z values by 1/zstep.
   *      then multiply mesh verts by zstep.
   */
  /*
    zval = 1.0f/(float)incz;
    for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
        
    for(pt = 0; pt < cont->psize; pt++){
    cont->pts[pt].z *= zval;
    }
    }
  */
}
     
void reresobj(Iobj *obj, double dist)
{
  int co;
  Icont *cont;
  Icont *tc;
  double d;

  if (dist <= 0.0)
    return;
  if (!obj)
    return;

  for (co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    d = dist;
    if (cont->psize > 5)
      for(;;){
        tc = imodContourDup(cont);
        imodContourShave(tc, d);
        if (tc->psize < 5){
          d *= 0.5;
          imodContourDelete(tc);
        }else{
          if (cont->pts)
            free(cont->pts);
          imodContourCopy(tc, cont);
          break;
        }
      }
  }
  return;
}

void ReduceObj(Iobj *obj, double dist)
{
  int co;
  Icont *cont;
  Icont *tc;
  float tol;

  if (dist <= 0.0)
    return;
  if (!obj)
    return;

  for (co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    tol = dist;
    if (cont->psize > 4)
      while(tol > 0.01 * dist){
        tc = imodContourDup(cont);
        imodContourReduce(tc, tol);
        if (tc->psize < 4){
          tol *= 0.5;
          imodContourDelete(tc);
        }else{
          if (cont->pts)
            free(cont->pts);
          imodContourCopy(tc, cont);
          break;
        }
      }
  }
  return;
}

static int imodMeshesDeleteRes(Imesh *mesh, int *size, int resol)
{
  int ms;
  int newsize = 0;
  if (!mesh)
    return(-1);
  for(ms = 0; ms < *size; ms++){
    if (imeshResol(mesh[ms].flag) == resol) {
      if (mesh[ms].vert)
        free(mesh[ms].vert);
      if (mesh[ms].list)
        free(mesh[ms].list);
    } else
      mesh[newsize++] = mesh[ms];
  }
  if (!newsize) {
    free(mesh);
    mesh = NULL;
  }
  *size = newsize;
  return(0);
}

static void imodMeshesDeleteZRange(Imesh *mesh, 
                                   int *meshsize, 
                                   int minz, int maxz, int resol)
{
  int m;
  if ((minz == DEFAULT_VALUE) && (maxz == DEFAULT_VALUE)){
    imodMeshesDeleteRes(mesh, meshsize, resol);
    return;
  }

  for(m = 0; m < *meshsize; m++){
    if (imeshResol(mesh[m].flag) == resol)
      imodMeshDeleteZRange(&mesh[m], minz, maxz);
  }
  return;
}

int deletez(Ipoint point, int minz, int maxz)
{
  int rmpoint = FALSE;
  int rmlow   = FALSE;
  int rmup    = FALSE;
  int z = point.z + 0.5f;

  if ((minz != DEFAULT_VALUE) && (z >= minz))
    rmlow = TRUE;
    
  if ((maxz != DEFAULT_VALUE) && (z <= maxz))
    rmup = TRUE;
  if ( (rmlow) && (rmup))
    rmpoint = TRUE;

  return(rmpoint);
}


/* Delete geometries in the range of minz and maxz.
 * The vertex data is not changed, only the index array
 * is changed.
 */
static void imodMeshDeleteZRange(Imesh *mesh, int minz, int maxz)
{
  int i;
  Ipoint pt[6];
  int newsize = 0;

  for(i = 0; i < mesh->lsize; i++){
        
    switch(mesh->list[i]){

    case IMOD_MESH_NORMAL:
      i++;
      break;


    case IMOD_MESH_BGNPOLYNORM:
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        if ((i + 7) > mesh->lsize) 
          break;
        pt[0] = mesh->vert[mesh->list[i++]];
        pt[1] = mesh->vert[mesh->list[i++]];
        pt[2] = mesh->vert[mesh->list[i++]];
        pt[3] = mesh->vert[mesh->list[i++]];
        pt[4] = mesh->vert[mesh->list[i++]];
        pt[5] = mesh->vert[mesh->list[i]];

        if ((deletez(pt[1], minz, maxz)) &&
            (deletez(pt[3], minz, maxz)) &&
            (deletez(pt[5], minz, maxz))
            ){
          i -= 5;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i] = DELETE_INDEX;
        }
      }
      break;

    case IMOD_MESH_BGNPOLYNORM2:
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        if ((i + 4) > mesh->lsize) 
          break;
        pt[1] = mesh->vert[mesh->list[i++]];
        pt[3] = mesh->vert[mesh->list[i++]];
        pt[5] = mesh->vert[mesh->list[i]];

        if ((deletez(pt[1], minz, maxz)) &&
            (deletez(pt[3], minz, maxz)) &&
            (deletez(pt[5], minz, maxz))
            ){
          i -= 2;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i++] = DELETE_INDEX;
          mesh->list[i] = DELETE_INDEX;
        }
      }
      break;


    case IMOD_MESH_END:
    case IMOD_MESH_SWAP:
    case IMOD_MESH_ENDTRI:
    case IMOD_MESH_BGNTRI:
      break;
            
    default:
      /*            if (deletez(mesh->vert[mesh->list[i]], minz, maxz))
                    mesh->list[i] = DELETE_INDEX;  seems like a bad idea */
      /*                imodMeshDeleteIndex(mesh, i); */

      break;

    }

  }

  for(i = 0; i < mesh->lsize; i++){
    if (mesh->list[i] != DELETE_INDEX)
      mesh->list[newsize++] = mesh->list[i];
  }
  /*    printf("original and new indices %d %d\n", mesh->lsize, newsize); */
  mesh->lsize = newsize;
}



/* avoid underflow exceptions. */

int cleanzero(Imod *imod)
{
  Iobj   *obj;
  Icont  *cont;
  Ipoint pnt;
  int ob,co,pt;

  for(ob = 0 ; ob < imod->objsize; ob++){
    obj = &imod->obj[ob];
    for(co = 0; co < obj->contsize; co++){
      cont = &obj->cont[co];
      for(pt = 0; pt < cont->psize; pt++){
        pnt = cont->pts[pt];

        if ((pnt.x < 0.001f) && (pnt.x > -0.001f))
          pnt.x = 0.0f;
        if ((pnt.y < 0.001f) && (pnt.y > -0.001f))
          pnt.y = 0.0f;
        if ((pnt.z < 0.001f) && (pnt.z > -0.001f))
          pnt.z = 0.0f;
                    
      }
    }
  }
  return(0);
}


static void imodMeshesRescaleNormal
(Imesh *inMesh, int *meshsize, Ipoint *spnt)
{
  int m;
  int i, j, ind, lsize;
  int listInc, vertBase, normAdd;
  Imesh *mesh;
    
  if ((!inMesh) || (*meshsize < 1)) return;


  for(m = 0; m < *meshsize; m++){
    mesh = &inMesh[m];
    lsize = mesh->lsize;
        
    for(i  = 0; i < lsize; i++){
      switch(mesh->list[i]){
      case IMOD_MESH_BGNPOLY:
      case IMOD_MESH_BGNBIGPOLY:
        while(mesh->list[++i] != IMOD_MESH_ENDPOLY);
        break;
                
      case IMOD_MESH_NORMAL:
        i++;
        if ((mesh->list[i] < mesh->vsize) && (mesh->list[i] > -1)){
          mesh->vert[mesh->list[i]].x *= spnt->x;
          mesh->vert[mesh->list[i]].y *= spnt->y;
          mesh->vert[mesh->list[i]].z *= spnt->z;
          imodPointNormalize(&mesh->vert[mesh->list[i]]);
        }
        break;
                
      case IMOD_MESH_BGNPOLYNORM:
      case IMOD_MESH_BGNPOLYNORM2:
        imodMeshPolyNormFactors(mesh->list[i++], &listInc, &vertBase,
                                &normAdd);
        while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
          for (j = 0; j < 3; j++) {
            ind = mesh->list[i] + normAdd;
            mesh->vert[ind].x *= spnt->x;
            mesh->vert[ind].y *= spnt->y;
            mesh->vert[ind].z *= spnt->z;
            imodPointNormalize(&mesh->vert[ind]);
            i += listInc;
          }
        }
        break;
                
      case IMOD_MESH_BGNTRI:
      case IMOD_MESH_ENDTRI:
      case IMOD_MESH_SWAP:
        break;
                
      default:
        break;
      }
    }
  }
  return;
}


int ObjOnList(int ob, int list[], int nlist)
{
  int i;
  if (!nlist)
    return 1;
  else
    for (i = 0; i < nlist; i++) {
      if (ob + 1 == list[i])
        return 1;
    }
  return 0;
}

/*
$Log$
Revision 3.11  2005/05/31 00:55:18  mast
Flush after skinning message for Windows

Revision 3.10  2005/04/04 22:37:06  mast
Fixed clipping of model in Z due to float to integer truncation

Revision 3.9  2005/03/20 19:56:05  mast
Eliminating duplicate functions

Revision 3.8  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.7  2005/01/25 01:42:04  mast
Fixed problem with extending cap exclusion list

Revision 3.6  2004/09/10 21:34:01  mast
Eliminated long variables

Revision 3.4.4.1  2004/07/07 19:26:21  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.4  2003/10/26 14:46:41  mast
fixed problem in eliminating getopt

Revision 3.3  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.2  2003/08/26 03:51:50  mast
Fix usage statement for -E option

Revision 3.1  2003/08/26 03:49:52  mast
Added option to cap tubes

*/
