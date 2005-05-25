/*
 *   FILE: imodel_fwrap.c
 *
 *   PURPOSE: Load an IMOD model file from Fortran code.
 *            Define F77FUNCAP, F77STRING to make for VMS.
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

Log at end of file
*/

#include <stdio.h>
#include <math.h>
#include "imodel.h"
/* These values should match max_obj_num and max_pt in model.inc */
#define FWRAP_MAX_OBJECT 200000
#define FWRAP_MAX_POINTS 10000000
#define FWRAP_MAX_CLIP_PLANES 100

#define FWRAP_NOERROR              0
#define FWRAP_ERROR_BAD_FILENAME  -1
#define FWRAP_ERROR_FILE_NOT_IMOD -2
#define FWRAP_ERROR_FILE_TO_BIG   -3
#define FWRAP_ERROR_NO_OBJECTS    -4
#define FWRAP_ERROR_NO_MODEL      -5
#define FWRAP_ERROR_BAD_OBJNUM    -6
#define FWRAP_ERROR_MEMORY        -7

#define NO_VALUE_PUT           -9999.

/*#define FWRAP_DEBUG */

#ifdef F77FUNCAP
#define newimod      NEWIMOD
#define deleteimod   DELETEIMOD
#define deleteiobj   DELETEIOBJ
#define getimod      GETIMOD
#define putimod      PUTIMOD
#define openimoddata OPENIMODDATA
#define getimodhead  GETIMODHEAD
#define getimodscales GETIMODSCALES
#define getimodmaxes GETIMODMAXES
#define putimodmaxes PUTIMODMAXES
#define getimodscat  GETIMODSCAT
#define getimodclip  GETIMODCLIP
#define putimodscat  PUTIMODSCAT
#define getimodmesh  GETIMODMESH
#define putimodmesh  PUTIMODMESH
#define writeimod    WRITEIMOD
#define getimodflags GETIMODFLAGS
#define getimodsizes GETIMODSIZES
#define getimodverts GETIMODVERTS
#define putimodflag  PUTIMODFLAG
#define fromvmsfloats FROMVMSFLOATS
#define putscatsize  PUTSCATSIZE
#define putsymsize   PUTSYMSIZE
#define putsymtype   PUTSYMTYPE
#define putobjcolor PUTOBJCOLOR
#define putimodzscale PUTIMODZSCALE
#define putimodrotation PUTIMODROTATION
#define getimodtimes  GETIMODTIMES
#define getimodsurfaces  GETIMODSURFACES
#define getimodobjname  GETIMODOBJNAME
#define getimodobjsize  GETIMODOBJSIZE
#define getimodnesting  GETIMODNESTING
#define imodpartialmode IMODPARTIALMODE
#define getimodobjlist  GETIMODOBJLIST
#define getimodobjrange GETIMODOBJRANGE
#else
#define newimod      newimod_
#define deleteimod   deleteimod_
#define deleteiobj   deleteiobj_
#define getimod      getimod_
#define putimod      putimod_
#define openimoddata openimoddata_
#define getimodhead  getimodhead_
#define getimodscales getimodscales_
#define getimodmaxes getimodmaxes_
#define putimodmaxes putimodmaxes_
#define getimodclip  getimodclip_
#define getimodscat  getimodscat_
#define putimodscat  putimodscat_
#define getimodmesh  getimodmesh_
#define putimodmesh  putimodmesh_
#define writeimod    writeimod_
#define getimodflags getimodflags_
#define getimodsizes getimodsizes_
#define getimodverts getimodverts_
#define putimodflag  putimodflag_
#define fromvmsfloats fromvmsfloats_
#define putscatsize  putscatsize_
#define putsymsize   putsymsize_
#define putsymtype   putsymtype_
#define putobjcolor putobjcolor_
#define putimodzscale putimodzscale_
#define putimodrotation putimodrotation_
#define getimodtimes  getimodtimes_
#define getimodsurfaces  getimodsurfaces_
#define getimodobjname  getimodobjname_
#define getimodobjsize  getimodobjsize_
#define getimodnesting  getimodnesting_
#define imodpartialmode imodpartialmode_
#define getimodobjlist  getimodobjlist_
#define getimodobjrange getimodobjrange_
#endif

static Imod *Fimod = NULL;

static int partialMode = 0;
static int nflags_put = 0;
static int *flags_put = NULL;
static int maxes_put = 0;
static int xmax_put, ymax_put, zmax_put;
static float zscale_put = NO_VALUE_PUT;
static Ipoint rotation_put = {NO_VALUE_PUT, NO_VALUE_PUT, NO_VALUE_PUT};

#define SCAT_SIZE_FLAG  (1 << 2)
#define SYMBOL_SIZE_FLAG (1 << 3)
#define SYMBOL_TYPE_FLAG (1 << 4)
/* Color won't work unless the value shift is <=8 */
#define OBJECT_COLOR_FLAG (1 << 5)
#define FLAG_VALUE_SHIFT 8

/* DNM: a common function to delete model and object flags */
static void deleteFimod()
{
  if (Fimod)
    imodDelete(Fimod);
  Fimod = NULL;
  if (flags_put)
    free(flags_put);
  flags_put = NULL;
  nflags_put = 0;
  maxes_put = 0;
  zscale_put = NO_VALUE_PUT;
  rotation_put.x = rotation_put.y = rotation_put.z = NO_VALUE_PUT;
}

/*!
 * Turns on mode for returning only part of model if [mode] is non-zero.
 */
void imodpartialmode(int *mode)
{
  partialMode = *mode;
}

/*!
 * Reads IMOD model from the file given by [fname] and transforms coordinates,
 * leaves it in a static model structure for further actions.
 * Returns all contour data into the given arrays unless in partial mode.
 * ^  [ibase]   = starting index of contours in coord array, numbered from 0
 * ^  [npt]     = number of points in contour
 * ^  [coord]   = X, Y, Z coordinates of points
 * ^  [color]   = On/Off flag and "color" = 256 - IMOD object number
 * ^  [npoint]  = Total number of points returned, or 0 in partial mode
 * ^  [nobject] = Total number of contours returned, or 0 in partial mode
 * ^  [fsize] is omitted from fortran call.
 */
int getimod(int ibase[], int npt[], float coord[][3], int color[][2],
             int *npoint, int *nobject, char *fname, int fsize)
{
  int err = openimoddata(fname, fsize);

  if (err)
    return err;

  *npoint = *nobject = 0;
  if (partialMode)
    return FWRAP_NOERROR;
  return (getimodobjrange(1, Fimod->objsize, ibase, npt, coord, color, npoint, 
                         nobject));
}

/*!
 * Reads in an IMOD model from a file with name [fname], places it into a 
 * static model structure and transforms coordinates.
 * Called from fortran as [openimoddata(fname)]
 */
int openimoddata(char *fname, int fsize)
{
  Imod   *model;
  IrefImage *iref;
  Imat  *mat;
  Ipoint pnt;
  FILE *fin;
  char *cfilename;

  model = imodNew();
  if (!model)
    return(-1);

  cfilename = f2cString(fname, fsize);
  if (!cfilename)
    return FWRAP_ERROR_MEMORY;

  fin = fopen(cfilename, "rb");

  free(cfilename);

  if (!fin)
    return(FWRAP_ERROR_BAD_FILENAME);

  model->file = fin;
  if (imodReadFile( model)) {
    fclose(fin);
    return(FWRAP_ERROR_FILE_NOT_IMOD);
  }
  fclose(fin);

  if (!model->objsize)
    return(FWRAP_ERROR_NO_OBJECTS);

  /* DNM: need to delete model to avoid memory leak */
  deleteFimod();

  Fimod = model;
  /*
   *  Translate reference image coordinates to 
   *  the identity matrix.
   *  DNM 11/5/98: rearranged to correspond to proper conventions 
   */
  iref = Fimod->refImage;

  if (Fimod->flags & IMODF_FLIPYZ)
    imodFlipYZ(Fimod);

  if (iref) {

    mat = imodMatNew(3);

    imodMatScale(mat, &iref->cscale);

    pnt.x =  -iref->ctrans.x;
    pnt.y =  -iref->ctrans.y;
    pnt.z =  -iref->ctrans.z;
    imodMatTrans(mat, &pnt);

    /* DNM 11/5/98: no fortran code expects or wants tilt angles to be
       applied, so leave this out */
    /*
      imodMatRot(mat, -iref->crot.x, X);
      imodMatRot(mat, -iref->crot.y, Y);
      imodMatRot(mat, -iref->crot.z, Z);
    */
         
    imodTransform(Fimod, mat);
    imodMatDelete(mat);
  }
  
  return FWRAP_NOERROR;
}

/*!
 * Returns contours from the given list of objects into the model arrays
 * ^  [objList] = List of IMOD objects to retrieve, numbered from 1
 * ^  [ninList] = Number of objects in list
 * ^  [ibase]   = starting index of contours in coord array, numbered from 0
 * ^  [npt]     = number of points in contour
 * ^  [coord]   = X, Y, Z coordinates of points
 * ^  [color]   = On/Off flag and "color" = 256 - IMOD object number
 * ^  [npoint]  = Total number of points returned
 * ^  [nobject] = Total number of contours returned
 */
int getimodobjlist(int objList[], int *ninList, int ibase[], int npt[],
                   float coord[][3], int color[][2], int *npoint, int *nobject)
{
  Iobj  *obj;
  Icont *cont;
  int ind;
  int ob, co, pt;
  int ncontour = 0;
  int npoints = 0;
  int coord_index = 0;
  int ibase_val = 0;
  int coi = 0;

  if (!Fimod)
    return FWRAP_ERROR_NO_MODEL;
  if (*ninList <= 0)
    return FWRAP_ERROR_NO_OBJECTS;

  /* Check object numbers and count contours and points */
  for (ind = 0; ind < *ninList; ind++) {
    ob = objList[ind] - 1;
    if (ob < 0 || ob >= Fimod->objsize)
      return(FWRAP_ERROR_BAD_OBJNUM);
    obj = &(Fimod->obj[ob]);
    ncontour += obj->contsize;
    for (co = 0; co < obj->contsize; co++)
      npoints += obj->cont[co].psize;
  }
  
  if (ncontour > FWRAP_MAX_OBJECT) {
    fprintf(stderr, "getimod: Too many contours in model for Fortran "
            "program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (npoints > FWRAP_MAX_POINTS) {
    fprintf(stderr, "getimod: Too many points in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  *npoint  = npoints;
  *nobject = ncontour; 

  for (ind = 0; ind < *ninList; ind++) {
    ob = objList[ind] - 1;
    obj = &(Fimod->obj[ob]);
    for (co = 0; co < obj->contsize; co++, coi++) {
      cont = &(obj->cont[co]);
      ibase[coi] = ibase_val;
      ibase_val += cont->psize;
      npt[coi]   = cont->psize;
      color[coi][0] = 1;
      color[coi][1] = 255 - ob;

      for (pt = 0; pt < cont->psize; pt++, coord_index++) {
        coord[coord_index][0] = cont->pts[pt].x;
        coord[coord_index][1] = cont->pts[pt].y;
        coord[coord_index][2] = cont->pts[pt].z;
      }
    }
  }
  return(FWRAP_NOERROR);

}

/*!
 * Returns contours from the given range of objects into the model arrays
 * ^  [objStart], [objEnd] = Starting and ending IMOD objects to retrieve,
 * numbered from 1
 * ^  [ninList] = Number of objects in list
 * ^  [ibase]   = starting index of contours in coord array, numbered from 0
 * ^  [npt]     = number of points in contour
 * ^  [coord]   = X, Y, Z coordinates of points
 * ^  [color]   = On/Off flag and "color" = 256 - IMOD object number
 * ^  [npoint]  = Total number of points returned
 * ^  [nobject] = Total number of contours returned
 */
int getimodobjrange(int *objStart, int *objEnd, int ibase[], int npt[],       
                   float coord[][3], int color[][2], int *npoint, int *nobject)
{
  int *objList;
  int i;
  int ninList = *objEnd + 1 - *objStart;

  if (!Fimod)
    return FWRAP_ERROR_NO_MODEL;
  if (ninList <= 0)
    return FWRAP_ERROR_NO_OBJECTS;
  objList = (int *)malloc(ninList * sizeof(int));
  if (!objList)
    return FWRAP_ERROR_MEMORY;
  for (i = 0; i < ninList; i++)
    objList[i] = *objStart + i;
  return (getimodobjlist(objList, &ninList, ibase, npt, coord, color, npoint, 
                         nobject));
}

/*
 * Returns data from all scattered point objects into the model arrays.
 * Combines all contours of each scattered point object into one contour. 
 * Other objects are returned as a single contour of 0 length.  
 * ^  [ibase]   = starting index of contours in coord array, numbered from 0
 * ^  [npt]     = number of points in contour
 * ^  [coord]   = X, Y, Z coordinates of points
 * ^  [color]   = On/Off flag and "color" = 256 - IMOD object number
 * ^  [npoint]  = Total number of points returned
 * ^  [maxobject] = Object number of last scattered point object
 */
int getimodscat (int ibase[],     /* index into coord array */
                 int npt[],       /* contour sizes */
                 float coord[][3],   /* coords */
                 int   color[][2],   /* colors 255..., 1 */
                 int   *npoint,   /* total number of points. */
                 int   *maxobject)  /* maximum object number. */
{
  int ob, co, pt;
  int npoints=0, maxobj=0;
  int coi=0, coord_index=0;
  Imod *model;
  Iobj *obj;
  Icont *scont, *cont;
  if (!Fimod) return(FWRAP_ERROR_NO_MODEL);
  if (!Fimod->objsize) return(FWRAP_ERROR_NO_OBJECTS);
  model = Fimod;

  for (ob = 0; ob < model->objsize; ob++) {
    obj = &(model->obj[ob]);
    if (!iobjScat(obj->flags))
      continue;
    maxobj = ob+1;
    for (co = 0; co < obj->contsize; co++)
      npoints += obj->cont[co].psize;
  }

  if (maxobj > FWRAP_MAX_OBJECT) {
    fprintf(stderr, "getimodscat: Too many contours in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (npoints > FWRAP_MAX_POINTS) {
    fprintf(stderr, "getimodscat: Too many points in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }
  *npoint  = npoints;
  *maxobject = maxobj;

  for (ob = 0; ob < model->objsize; ob++, coi++) {
    obj = &(model->obj[ob]);

    scont = imodContourNew(); /* Create empty contour. */

    if (iobjScat(obj->flags)) {
      for (co = 0; co < obj->contsize; co++) {
        cont = &(obj->cont[co]);
        for (pt = 0; pt < cont->psize; pt++)
          imodPointAppend(scont, &cont->pts[pt]);
      }
    }

    ibase[coi] = coord_index;
    npt[coi]   = scont->psize;

    color[coi][0] = 1;
    color[coi][1] = 255 - ob;
      
    for (pt = 0; pt < scont->psize; pt++, coord_index++) {
      coord[coord_index][0] = scont->pts[pt].x;
      coord[coord_index][1] = scont->pts[pt].y;
      coord[coord_index][2] = scont->pts[pt].z;
    }
    imodContourDelete(scont);
  }
  return FWRAP_NOERROR;
}

/*!
 * Returns mesh data for the given object [objnum].  The vertex/normal data
 * are returned in the array [verts], whose maximum size is specified by 
 * [limverts], and the index data are returned in the array [index], whose
 * maximum size is specified by [limindex].
 */
int  getimodmesh(int *objnum, float *verts, int *index, int *limverts, 
                  int *limindex)
{
  int i, m, vsum, lsum, resol;
  Iobj *obj;
  Imesh *mesh;

  if (!Fimod) return(FWRAP_ERROR_NO_MODEL);
  Fimod->cindex.object = *objnum - 1;
  obj = &(Fimod->obj[Fimod->cindex.object]);
  if (!obj->meshsize) return(-1);

  mesh = obj->mesh;
  imodMeshNearestRes(mesh, obj->meshsize, 0, &resol);

  vsum = 0;
  lsum = 0;
  for (m = 0; m < obj->meshsize; m++) {
    if (imeshResol(mesh[m].flag) == resol) {
      vsum += mesh[m].vsize;
      lsum += mesh[m].lsize;
    }
  }

  if (vsum > *limverts) {
    fprintf(stderr, "getimodmesh: Too many vertices in mesh for "
            "Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (lsum > *limindex) {
    fprintf(stderr, "getimodmesh: Too many indices in mesh for "
            "Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  for (m = 0; m < obj->meshsize; m++) {
    if (imeshResol(mesh[m].flag) == resol) {
      for (i = 0; i < mesh[m].vsize; i++) {
        *verts++ = mesh[m].vert[i].x;
        *verts++ = mesh[m].vert[i].y; 
        *verts++ = mesh[m].vert[i].z;
      }
      for (i = 0; i < mesh[m].lsize; i++)
        *index++ = mesh[m].list[i];
    }
  }

  return FWRAP_NOERROR;
}

/*!
 * Returns mesh data with normals omitted for the given object [objnum].
 * The vertex data are returned in the array [verts], whose maximum size is 
 * specified by [limverts], and the index data are returned in the array 
 * [index], whose maximum size is specified by [limindex].  [nverts] and 
 * [nindex] are returned with the number of vertices and indexes.
 */
int  getimodverts(int *objnum, float *verts, int *index, int *limverts, 
                   int *limindex, int *nverts, int *nindex)
{
  int i, j, mi, resol, vsum, m;
  Iobj *obj;
  Imesh *mesh;

  if (!Fimod) return(FWRAP_ERROR_NO_MODEL);
  Fimod->cindex.object = *objnum - 1;
  obj = &(Fimod->obj[Fimod->cindex.object]);
  if (!obj->meshsize) return(-1);

  mesh = obj->mesh;
  imodMeshNearestRes(mesh, obj->meshsize, 0, &resol);

  vsum = 0;
  for (m = 0; m < obj->meshsize; m++)
    if (imeshResol(mesh[m].flag) == resol)
      vsum += mesh[m].vsize;


  if (vsum / 2 > *limverts) {
    /* we don't want this message when running mtk */
    /*  fprintf(stderr, "getimodverts: Too many vertices in mesh for "
        "Fortran program\n"); */
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  *nverts = 0;
  j = 0;
  for (m = 0; m < obj->meshsize; m++) {
    mesh = &obj->mesh[m];
    if (imeshResol(mesh->flag) != resol)
      continue;
    mi = mesh->vsize;
    for (i = 0; i < mi; i += 2) {
      *verts = mesh->vert[i].x;
      verts++;
      *verts = mesh->vert[i].y; 
      verts++;
      *verts = mesh->vert[i].z;
      verts++;
    }
    *nverts += mi / 2;

    mi = mesh->lsize;
    i = 0;
    while (mesh->list[i] != IMOD_MESH_END && i < mi) {
      while (mesh->list[i] != IMOD_MESH_END && 
             mesh->list[i] != IMOD_MESH_BGNPOLYNORM && i < mi)
        i++;
      if (mesh->list[i] == IMOD_MESH_BGNPOLYNORM) {
        index[j++] = IMOD_MESH_BGNPOLYNORM;
        i++;
        while (mesh->list[i] != IMOD_MESH_ENDPOLY && i < mi) {
          if (j + 6 > *limindex) {
            printf ("%d %d %d %d\n",i,mi,j, *limindex);
            fprintf(stderr, "getimodverts: Too many indices"
                    " in mesh for Fortran program\n");
            return(FWRAP_ERROR_FILE_TO_BIG);
          }
          index[j++] = mesh->list[i+1] / 2;
          index[j++] = mesh->list[i+3] / 2;
          index[j++] = mesh->list[i+5] / 2;
          i += 6;
        }

        index[j++] = IMOD_MESH_ENDPOLY;
        i++;
      }
    }
  }
  index[j++] = IMOD_MESH_END;
  *nindex=j;
  return FWRAP_NOERROR;
}

/*!
 * Returns point sizes for the given object [ob] in array [sizes]; [limsizes] 
 * specifies the size of the array and the number or points is returned 
 * in [nsizes].
 */
int getimodsizes(int *ob, float *sizes, int *limsizes, int *nsizes)
{
  Iobj *obj;
  Icont *cont;
  int co, pt;
  *nsizes = 0;
  if (!Fimod) return(FWRAP_ERROR_NO_MODEL);
  if (*ob < 1 && *ob > Fimod->objsize)
    return(1);
  obj = &Fimod->obj[*ob - 1];
  if (!obj) return(1);
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (*nsizes + cont->psize > *limsizes) {
      fprintf(stderr, "getimodsizes: Model too large for Fortran program\n");
      return(FWRAP_ERROR_FILE_TO_BIG);
    }
    for (pt = 0; pt < cont->psize; pt++)
      *sizes++ = imodPointGetSize(obj, cont, pt);
    *nsizes += cont->psize;
  }
     
  return FWRAP_NOERROR;
}          

/*!
 * Returns time values for all contours in all objects into array [times]
 */
int getimodtimes(int *times)
{
  int ob, co;
  Iobj *obj;
  int coi = 0;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  for (ob = 0; ob < Fimod->objsize; ob++) {
    obj = &(Fimod->obj[ob]);
    for (co = 0; co < obj->contsize; co++, coi++)
      times[coi] = obj->cont[co].type;
  }

  return FWRAP_NOERROR;
}

/*!
 * Returns surface numbers for all contours in all objects into array [surfs]
 */
int getimodsurfaces(int *surfs)
{
  int ob, co;
  Iobj *obj;
  int coi = 0;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  for (ob = 0; ob < Fimod->objsize; ob++) {
    obj = &(Fimod->obj[ob]);
    for (co = 0; co < obj->contsize; co++, coi++)
      surfs[coi] = obj->cont[co].surf;
  }

  return FWRAP_NOERROR;
}


#define OBJ_EMPTY    -2
#define OBJ_HAS_DATA -1

static float Wmod_Colors[9][3]  =   { {0.90, 0.82, 0.37},  /* Dim Yellow  */
                                      {0.54, 0.51, 0.01},  /* Olive Brown */
                                      {0.94, 0.49, 0.0},   /* Orange      */
                                      {1.00, 0.0,  0.0},   /* Red         */
                                      {0.0,  1.0,  0.0},   /* Green       */
                                      {0.0,  0.0,  1.0},   /* Blue        */
                                      {1.0,  1.0,  0.0},   /* Yellow      */
                                      {1.0,  0.0,  1.0},   /* Magenta     */
                                      {0.0,  1.0,  1.0}    /* Cyan        */
};

/*!
 * Puts model contours from arrays back into IMOD model structure.  If partial
 * mode is off, all existing contours in all objects are deleted.  If partial
 * mode is on, existing contours will be deleted only from objects included 
 * in these data (as specified by the [color] values); other objects will be 
 * retained.
 * ^  [ibase]   = starting index of contours in [cindex] array, numbered from 0
 * ^  [npt]     = number of points in contour
 * ^  [coord]   = X, Y, Z coordinates of points
 * ^  [cindex]  = index to points in [coord] array, numbered from 1
 * ^  [color]   = On/Off flag and "color" = 256 - IMOD object number
 * ^  [npoint]  = Total number of points
 * ^  [nobject] = Total number of contours
 */
int putimod(int ibase[], int npt[], float coord[][3], int cindex[], 
            int color[][2], int *npoint, int *nobject)
{
  Iobj  *obj;
  Icont *cont;
  int ob, pt, ci;
  int object;
  int nobj = 0;
  int *objlookup;
  int *nsaved;
  int mincolor;
  int maxobj;
  int wimpno;

  if (!Fimod) {
    Fimod = imodNew();
    Fimod->objsize = 0;
    Fimod->obj = NULL;
  }

  mincolor = 256 - Fimod->objsize;

  /* Find minimum color, and maximum object #; get arrays */
  /* Skip empty contours unless we are in partial mode, where they are needed
     to signal that an existing object is now empty */
  for (object = 0; object < *nobject; object++) {
    if (npt[object] == 0 && !partialMode) 
      continue;
    if (mincolor > color[object][1])
      mincolor = color[object][1];
  }
  maxobj = 256 - mincolor;

  objlookup = (int *)malloc(maxobj * sizeof(int));
  nsaved = (int *)malloc(maxobj * sizeof(int));

  /*
   * Mark obj lookup array to empty=-2 or used=-1 for new data passed in
   * or index >= 0 for an existing object.  Set nsaved to 0 to keep track of
   * # of contours passed back in to an object.  objlookup is index from
   * (winp color - mincolor) to real object, nsaved is index on real object.
   */
  for (ob = 0; ob < maxobj; ob++) {
    objlookup[ob] = OBJ_EMPTY;
    nsaved[ob] = 0;
  }

  /* Fill lookup table for existing objects; if in partial mode set nsaved
     to full count to keep contours from being deleted later */
  for (ob = 0; ob < Fimod->objsize; ob++) {
    objlookup[(255 - mincolor) - ob] = ob;
    nobj++;
    if (partialMode)
      nsaved[ob] = Fimod->obj[ob].contsize;
  }

  /* For all non-empty contours passed back, mark an empty object as having
     data.  In partial mode, for ALL contours passed back for an existing 
     object, set nsaved back to
     0 so that if all contours got deleted they will be deleted from object */
  for (object = 0; object < *nobject; object++) {
    ob = color[object][1] - mincolor;
    if (objlookup[ob] >= 0)
      nsaved[objlookup[ob]] = 0;
    if (npt[object] == 0)
      continue;
    if (objlookup[ob] == OBJ_EMPTY)
      objlookup[ob] = OBJ_HAS_DATA;
  }

   /*  Do not remove old object data yet */

  /*
   * Create additional imod objects if needed.
   */
  for (ob = maxobj - 1; ob >= 0; ob--) {
    if (objlookup[ob] != OBJ_HAS_DATA)
      continue;
    objlookup[ob] = Fimod->objsize;
    imodNewObject(Fimod);
    wimpno = ob + mincolor;

    /* Just use top 6 colors and let new color scheme hold after that */
    if (wimpno >= 250) {
      Fimod->obj[nobj].red   = Wmod_Colors[wimpno - 247][0];
      Fimod->obj[nobj].green = Wmod_Colors[wimpno - 247][1];
      Fimod->obj[nobj].blue  = Wmod_Colors[wimpno - 247][2];
        
    } /*else{
        Fimod->obj[nobj].red   =  wimpno / 255.0f;
        Fimod->obj[nobj].green = wimpno / 255.0f;
        Fimod->obj[nobj].blue  = wimpno / 255.0f;
        } */
    Fimod->obj[nobj].flags |= IMOD_OBJFLAG_OPEN;
    sprintf(Fimod->obj[nobj].name, "Wimp no. %d", wimpno);
    nobj++;
  }


  /*
   * Copy all wimp objects to imod contours.
   */
  for (object = 0; object < *nobject; object++) {
     
    /* Don't even look up if empty and out of range */
    if (npt[object] == 0 && (color[object][1] < mincolor ||
                             color[object][1] > 255))
      continue;
    ob  = objlookup[color[object][1] - mincolor];
    if (ob < 0) {
      if (npt[object] != 0)
        fprintf(stderr, "putimod warning: bad object bounds %d\n", ob);
      continue;
    }
    if (ob > Fimod->objsize) {
      fprintf(stderr, "putimod warning: bad object bounds %d\n", ob);
      continue;
    }
    obj = &Fimod->obj[ob];

    if (nsaved[ob] < obj->contsize) {

      /* Existing contour */
      cont = &(obj->cont[nsaved[ob]]);

      /* Possible criterion was to require that number of points match,
         but this lost surface numbers with point reduction */
      /* For empty contour, clear out existing data */
      if (!npt[object] && cont->psize) {
        imodel_contour_clear(cont);
      } else if (npt[object] != cont->psize) {

        /* Otherwise for contour not the right size, adjust memory */
        if (!cont->psize)
          cont->pts = (Ipoint *)malloc(sizeof(Ipoint) * npt[object]);
        else {
          cont->pts = (Ipoint *)realloc(cont->pts, 
                                        sizeof(Ipoint) * npt[object]);
          if (cont->sizes)
            cont->sizes = (float *)realloc(cont->sizes, 
                                           sizeof(float) * npt[object]);
        }
        if (!cont->pts)
          return FWRAP_ERROR_MEMORY;
      }
      cont->psize = npt[object];
      for (pt = 0; pt < cont->psize; pt++) {
        ci = cindex[ibase[object] + pt] - 1;
        cont->pts[pt].x = coord[ci][0]; 
        cont->pts[pt].y = coord[ci][1]; 
        cont->pts[pt].z = coord[ci][2]; 
      }
    } else {

      /* otherwise, if # saved = # of contours, add a new contour */
      cont = imodContourNew();
      if (!cont)
        return FWRAP_ERROR_MEMORY;
      cont->psize = npt[object];

      if (cont->psize) {
        cont->pts = (Ipoint *)malloc(sizeof(Ipoint) * cont->psize);
        if (!cont->pts) 
          return FWRAP_ERROR_MEMORY;
      }

      for (pt = 0; pt < cont->psize; pt++) {
        ci = cindex[ibase[object] + pt] - 1;
        cont->pts[pt].x = coord[ci][0]; 
        cont->pts[pt].y = coord[ci][1]; 
        cont->pts[pt].z = coord[ci][2]; 
      }
         
      imodObjectAddContour(obj, cont);
    }
    nsaved[ob]++;
  }

  /*
   *  Remove old contours that were not replaced
   */     
  for (ob = 0; ob < Fimod->objsize; ob++) {
    if (imodContoursDeleteToEnd(&(Fimod->obj[ob]), nsaved[ob]))
      return FWRAP_ERROR_MEMORY;
  }   

  return FWRAP_NOERROR;
}

/*!
 * Writes the current model structure to the file given by [fname] after 
 * transforming back to index coordinates.  Called from fortran as 
 * [writeimod(fname)].
 */
int writeimod(char *fname, int fsize)
{
  int i, ob, flag, value;
  int retcode = 0;
  Iobjview *objview;
  char *filename = f2cString(fname, fsize);
  if (!filename)
    return(FWRAP_ERROR_BAD_FILENAME);
  if (!Fimod) {
    free(filename); 
    return(FWRAP_ERROR_NO_MODEL);
  }

  /*
   *  Translate coordinates to match reference image.
   *  DNM 11/5/98: rearranged to correspond to proper conventions 
   * 5/21/05: moved this up from putimod
   */
  if (Fimod->refImage) {
    IrefImage *iref = Fimod->refImage;
    Imat  *mat = imodMatNew(3); 
    Ipoint pnt;

    /* DNM 11/5/98: no fortran code expects or wants tilt angles to be
       applied, so leave this out */
    /*
      imodMatRot(mat, iref->crot.z, Z);
      imodMatRot(mat, iref->crot.y, Y);
      imodMatRot(mat, iref->crot.x, X);
    */

    imodMatTrans(mat, &iref->ctrans);

    pnt.x = 1.0/iref->cscale.x;
    pnt.y = 1.0/iref->cscale.y;
    pnt.z = 1.0/iref->cscale.z;
    imodMatScale(mat, &pnt);
    
    imodTransform(Fimod, mat);
    imodMatDelete(mat);


  }

  /* Reflip data back if necessary */

  if (Fimod->flags & IMODF_FLIPYZ)
    imodFlipYZ(Fimod);

  for (i = 0; i < nflags_put; i++) {
    ob = flags_put[2 * i];
    flag = flags_put[2 * i + 1];
    value = flag >> FLAG_VALUE_SHIFT;
    /* printf("object %d  flag %d  value %d\n", ob, flag & 0xff, value); */
    if (ob >= 0 && ob < Fimod->objsize) {

      /* Save some data in the current view if it has an object view for
         this object */
      objview = NULL;
      if (Fimod->cview > 0 && Fimod->view[Fimod->cview].objvsize > ob)
        objview = &(Fimod->view[Fimod->cview].objview[ob]);

      switch (flag) {
      case 0:
        Fimod->obj[ob].flags &= ~IMOD_OBJFLAG_OPEN;
        Fimod->obj[ob].flags &= ~IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = Fimod->obj[ob].flags;
        break;
      case 1:
        Fimod->obj[ob].flags |= IMOD_OBJFLAG_OPEN;
        Fimod->obj[ob].flags &= ~IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = Fimod->obj[ob].flags;
        break;
      case 2:
        Fimod->obj[ob].flags &= ~IMOD_OBJFLAG_OPEN;
        Fimod->obj[ob].flags |= IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = Fimod->obj[ob].flags;
        break;
      default:
        if (flag & SCAT_SIZE_FLAG) {
          Fimod->obj[ob].pdrawsize = value;
          if (objview)
            objview->pdrawsize = Fimod->obj[ob].pdrawsize;
        }
        if (flag & SYMBOL_SIZE_FLAG)
          Fimod->obj[ob].symsize = value;
        if (flag & SYMBOL_TYPE_FLAG)
          Fimod->obj[ob].symbol = value;
        if (flag & OBJECT_COLOR_FLAG) {
          Fimod->obj[ob].red = (value & 255) / 255.;
          Fimod->obj[ob].green = ((value >> 8) & 255) / 255.;
          Fimod->obj[ob].blue = ((value >> 16) & 255) / 255.;
          if (objview) {
            objview->red = Fimod->obj[ob].red;
            objview->green = Fimod->obj[ob].green;
            objview->blue = Fimod->obj[ob].blue;
          }
        }
        break;
      }
    }
  }
       
  if (maxes_put) {
    Fimod->xmax = xmax_put;
    Fimod->ymax = ymax_put;
    Fimod->zmax = zmax_put;
  }

  /* Save Zscale, save rotation in current view if there is one */
  if (zscale_put != NO_VALUE_PUT)
    Fimod->zscale = zscale_put;
  if (Fimod->cview > 0 && rotation_put.x != NO_VALUE_PUT)
    Fimod->view[Fimod->cview].rot = rotation_put;
     
  /* Complete the set of object views just before writing */
  imodObjviewComplete(Fimod);
  Fimod->file = fopen(filename, "wb");
  retcode = imodWriteFile(Fimod);
  free(filename);
  return(retcode);
}

/*!
 * Puts scattered points back into object [ob] from array [verts].  The
 * number of points must be the same as the number in the original object.
 */
int putimodscat(int *ob, float *verts)
{
  Iobj *obj;
  Icont *cont;
  int co, pt, v;
     
  if (!Fimod)
    return(1);
  if (*ob < 1 && *ob > Fimod->objsize)
    return(1);
  obj = &Fimod->obj[*ob - 1];
  if (!obj) return(1);
  if (!iobjScat(obj->flags)) return(2);

  for (co = 0, v = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    for (pt = 0; pt < cont->psize; pt++) {
      cont->pts[pt].x = verts[v++];
      cont->pts[pt].y = verts[v++];
      cont->pts[pt].z = verts[v++];
    }
  }
  return(0);
}

/*!
 * Puts vertex/normal data back into the current model object from array 
 * [verts] and sets the flag that normals are magnitudes.
 */
int putimodmesh(float *verts)
{
  int i,mi;
  Iobj *obj = &(Fimod->obj[Fimod->cindex.object]);
  float *fvert;

  obj->mesh->flag |= IMESH_FLAG_NMAG;
  mi = obj->mesh->vsize * 3;
  fvert = (float *) obj->mesh->vert;

  for (i = 0; i < mi; i++) {
    fvert[i] = verts[i];
  }
  return FWRAP_NOERROR;
}

/*!
 * Deletes current model and allocates a new, default model structure
 */
int newimod(void)
{
    deleteFimod();
    Fimod = imodNew();
    if (!Fimod)
      return FWRAP_ERROR_NO_MODEL;
    Fimod->objsize = 0;
    Fimod->obj = NULL;
    return FWRAP_NOERROR;
}

/*!
 * Deletes current model and frees all memory
 */
int deleteimod(void)
{
    if (!Fimod)
      return FWRAP_ERROR_NO_MODEL;
    deleteFimod();
    return FWRAP_NOERROR;
}

/*!
 * Removes all objects from model.
 */
int deleteiobj(void)
{
    if (!Fimod)
        return FWRAP_ERROR_NO_MODEL;

    imodObjviewsFree(Fimod);
    imodObjectsDelete(Fimod->obj, Fimod->objsize);
    Fimod->objsize = 0;
    Fimod->obj = NULL;

    return FWRAP_NOERROR;
}


/*!
 * Returns number of IMOD objects in the model, or -1 for error.
 */
int getimodobjsize()
{
  if (!Fimod)
    return -1;
  return Fimod->objsize;
}

/*!
 * Returns pixel size in microns in [um] and Z-scale in [zscale]
 */
int getimodheado(float *um, float *zscale)
{
  int exval;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  exval = Fimod->units + 6;
  *um = Fimod->pixsize * (pow(10.0, (double)exval));
  *zscale = Fimod->zscale;
  return FWRAP_NOERROR;
}

/*!
 * Returns maximum X, Y, Z coordinates of image that model was loaded on
 */
int getimodmaxes(int *xmax, int *ymax, int *zmax)
{
  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  *xmax = Fimod->xmax;
  *ymax = Fimod->ymax;
  *zmax = Fimod->zmax;
  return FWRAP_NOERROR;
}

/*!
 * Returns header values: pixel size in microns in [um], Z-scale in [zscale],
 * offsets to get from model coordinates to full volume index coordinates in
 * [xoffset], [yoffset], [zoffset], and [ifflip] nonzero if Y/Z are flipped.
 */
int getimodhead(float *um, float *zscale,
                float *xoffset, float *yoffset, float *zoffset,
                int *ifflip)
{
  int exval;
  IrefImage *iref;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  exval = Fimod->units + 6;
  *um = Fimod->pixsize * (pow(10.0, (double)exval));
  *zscale = Fimod->zscale;

     

  iref = Fimod->refImage;
     
  /* DNM 7/20/02: If image origin has been stored in otrans, return that
     because that is what is needed to get to full volume index coords */
  if (!iref) {
    *xoffset = 0;
    *yoffset = 0;
    *zoffset = 0;
  } else if (Fimod->flags & IMODF_OTRANS_ORIGIN) {
    *xoffset = -iref->otrans.x;
    *yoffset = -iref->otrans.y;
    *zoffset = -iref->otrans.z;
  }else{
    *xoffset = -iref->ctrans.x;
    *yoffset = -iref->ctrans.y;
    *zoffset = -iref->ctrans.z;
  }


  if (Fimod->flags & IMODF_FLIPYZ) {
    *ifflip = 1;
  }else{
    *ifflip = 0;
  }
  return FWRAP_NOERROR;
}

/*!
 * Returns the image file scale values used to get from model to index coords 
 */
int getimodscales(float *ximscale, float *yimscale, float *zimscale)
{
  IrefImage *iref;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  iref = Fimod->refImage;
     
  if (iref) {
    *ximscale = iref->cscale.x;
    *yimscale = iref->cscale.y;
    *zimscale = iref->cscale.z;
  }else{
    *ximscale = 1.;
    *yimscale = 1.;
    *zimscale = 1.;
  }
  return FWRAP_NOERROR;
}

/*!
 * Return a flag for each object in array [flags]; [limflags] specifies the 
 * maximum size of the array.  The flag is the sum of 0, 1 or 2 for closed,
 * open or scattered point object, plus 4 if the object has a mesh.
 */
int getimodflags(int *flags, int *limflags)
{
  int i;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);
  if (Fimod->objsize > *limflags) {
    fprintf(stderr, "getimodflags: Too many objects for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }
  for (i = 0; i < *limflags; i++)
    flags[i] = -1;
  for (i = 0; i < Fimod->objsize; i++) {
    if (!Fimod->obj[i].contsize)
      continue;
    flags[i]=0;
    if (Fimod->obj[i].flags & IMOD_OBJFLAG_OPEN)
      flags[i] = 1;
    if (Fimod->obj[i].flags & IMOD_OBJFLAG_SCAT)
      flags[i] = 2;
    if (Fimod->obj[i].meshsize)
      flags[i] += 4;
  }
  return FWRAP_NOERROR;
}

/*!
 * Returns name of object [ob] in [fname]; [fsize] is omitted in the call from
 * fortran.
 */
int getimodobjname(int *ob, char *fname, int fsize)
{
  Iobj *obj;
  int i = 0;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);
  if (*ob < 1 && *ob > Fimod->objsize)
    return(1);
  obj = &Fimod->obj[*ob - 1];
  if (!obj) 
    return(1);

  for (i = 0; i < fsize && i < IOBJ_STRSIZE && obj->name[i] ; i++)
    fname[i] = obj->name[i];
  for (; i < fsize; i++)
    fname[i] = 0x20;
  return FWRAP_NOERROR;
}

/*!
 * Returns the clipping planes for the object [objnum] in the array [clip].
 * Each clipping plane is specified by 4 numbers: the normal and the dot
 * product of normal and point, scaled by the pixel size.
 * The return value is the number of clipping planes or negative for an error.
 */
int getimodclip(int *objnum, float *clip)
{
  Iobj *obj;
  int nout, i, iout = 0;
  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  if ((*objnum > Fimod->objsize) || (*objnum < 1))
    return(FWRAP_ERROR_BAD_OBJNUM);
     
  Fimod->cindex.object = *objnum - 1;
  obj = &(Fimod->obj[Fimod->cindex.object]);

  /* DNM 9/19/04: modified for multiple clip planes */
  nout = obj->clips.count;
  if (nout > FWRAP_MAX_CLIP_PLANES)
    nout = FWRAP_MAX_CLIP_PLANES;

  for (i = 0; i < nout; i++) {
    clip[iout++] = obj->clips.normal[i].x;
    clip[iout++] = obj->clips.normal[i].y;
    clip[iout++] = obj->clips.normal[i].z / Fimod->zscale;
    clip[iout] = (obj->clips.normal[i].x * obj->clips.point[i].x) +
      (obj->clips.normal[i].y * obj->clips.point[i].y) +
      (obj->clips.normal[i].z * obj->clips.point[i].z);
    clip[iout++] *= Fimod->pixsize;
  }
  return(nout);
}

/*!
 * Sets the maximum X, Y, Z coordinates for the model
 */
/* DNM 6/8/01: change this to save the values until a model is written */
int putimodmaxes(int *xmax, int *ymax, int *zmax)
{
  xmax_put = *xmax;
  ymax_put = *ymax;
  zmax_put = *zmax;
  maxes_put = 1;
  return FWRAP_NOERROR;
}

/*!
 * Specifies a flag or value for object [objnum]; in external calls [flag]
 * should be 0 for closed contour, 1 for open contour, 2 for scattered points
 */
void putimodflag(int *objnum, int *flag)
{
  if (nflags_put)
    flags_put = (int *)realloc(flags_put,
                               (nflags_put + 1) * 2 * sizeof(int));
  else
    flags_put = (int *)malloc(2 * sizeof(int));
      
  if (!flags_put) {
    nflags_put = 0;
    return;
  }

  flags_put[nflags_put * 2] = *objnum - 1;
  flags_put[nflags_put * 2 + 1] = *flag;
  nflags_put++;
}

/*!
 * Sets the Z-scale of the model to [zscale]
 */
void putimodzscale(float *zscale)
{
  zscale_put = *zscale;
}

/*!
 * Sets the display rotation angles of the model to [xrot], [yrot], [zrot]
 */
void putimodrotation(float *xrot, float *yrot, float *zrot)
{
  rotation_put.x = *xrot;
  rotation_put.y = *yrot;
  rotation_put.z = *zrot;
}

/* DNM 12/3/01: added so that old wimp models can be converted */
void fromvmsfloats(unsigned char *data, int *amt)
{
  imodFromVmsFloats(data, *amt);
}

/*!
 * Sets the 3D point size for [objnum] to [size]
 */
/* DNM 5/15/02: put scattered point sizes out in flags with an offset */
void putscatsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) | SCAT_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the symbol type of object [objnum] to [type]
 */
void putsymtype(int *objnum, int *type)
{
  int flag = (*type << FLAG_VALUE_SHIFT) | SYMBOL_TYPE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the color for object [objnum] to [red], [green], [blue], where values
 * range from 0 to 255.
 */
/* DNM 4/1/2/05: Add object color.  THIS WILL BREAK WITH TOO MANY FLAGS.
   This whole approach should be replaced with creating empty objects */
void putobjcolor(int *objnum, int *red, int *green, int *blue)
{
  int r = B3DMIN(255, B3DMAX(0, *red));
  int g = B3DMIN(255, B3DMAX(0, *green));
  int b = B3DMIN(255, B3DMAX(0, *blue));
  int flag = (r << FLAG_VALUE_SHIFT) | (g << FLAG_VALUE_SHIFT + 8) |
    (b << FLAG_VALUE_SHIFT + 16) | OBJECT_COLOR_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the symbol size of object [objnum] to [size]
 */
void putsymsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) | SYMBOL_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Returns information about nested contours for object [ob].
 * Only inside information is returned if [inOnly] is nonzero. ^
 * [level] is filled with nesting level or 0 if no inside/outside contours ^
 * [inCont] and [outCont] are filled with the inside and outside contours 
 * numbers from the nesting analysis, numered from 1 ^
 * [inIndex] and [outIndex] are filled with indices into these arrays for each 
 * contour, numbered from 1 ^
 * [arraySize] specifies limiting size of ALL arrays, but if [inOnly] is 
 * nonzero then [outCont] and [outIndex] are ignored and do not need to be 
 * arrays
 */
int getimodnesting(int *ob, int *inOnly, int *level, int *inIndex, 
                   int *inCont, int *outIndex, int *outCont, int *arraySize)
{
  Iobj *obj;
  Icont **scancont;
  Ipoint *pmin, *pmax;
  Nesting *nests, *nest;
  int *contz;
  int *numatz;
  int **contatz;
  int *zlist;
  int *nestind;
  int co;
  int zmin,zmax;
  int indz;
  int nummax, intot, outtot;
  int  kis, lis, zlsize;
  int numnests = 0;
  int eco;
  int numwarn = -1;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);
  if (*ob < 1 && *ob > Fimod->objsize)
    return(FWRAP_ERROR_BAD_OBJNUM);
  obj = &Fimod->obj[*ob - 1];
  if (*arraySize <= obj->contsize)
    return(FWRAP_ERROR_FILE_TO_BIG);
  if (imodContourMakeZTables(obj, 1, 0, &contz, &zlist, &numatz, &contatz, 
                             &zmin, &zmax, &zlsize, &nummax))
    return(FWRAP_ERROR_MEMORY);

  /* Allocate lists of mins, max's, and scan contours */
  pmin = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  scancont = (Icont **)malloc(obj->contsize * sizeof (Icont *)); 
  nestind = (int *)malloc(obj->contsize * sizeof(int));
  if (!pmin || !pmax || !scancont || !nestind)
    return(FWRAP_ERROR_MEMORY);

  /* Get mins, maxes, set addresses into scan contour list */
  for (co = 0; co < obj->contsize; co++) {
    level[co] = 0;
    nestind[co] = -1;
    if (obj->cont[co].psize) {
      imodContourGetBBox(&(obj->cont[co]), &(pmin[co]), &(pmax[co]));
      scancont[co] = &(obj->cont[co]);
    }
  }

  /* Loop on contours to find inside and oiutside pairs */
  for (indz = 0; indz < zmax + 1 - zmin; indz++) {
    for (kis = 0; kis < numatz[indz] - 1; kis++) {
      co = contatz[indz][kis];
      if (!obj->cont[co].psize)
        continue;
      for (lis = kis + 1; lis < numatz[indz]; lis++) {
        eco = contatz[indz][lis];
        if (!obj->cont[eco].psize)
          continue;

        if (imodContourCheckNesting(co, eco, scancont, pmin, pmax, &nests,
                                    nestind, &numnests, &numwarn))
          return(FWRAP_ERROR_MEMORY);
      }
    }
  }

  /* Analyze inside and outside contours to determine level */
  imodContourNestLevels(nests, nestind, numnests);

  intot = 0;
  outtot = 0;
  inIndex[0] = 1;
  if (! *inOnly)
    outIndex[0] = 1;

  /* Fill arrays with the inside and outside lists and indexes to lists */
  for (co = 0; co < obj->contsize; co++) {
    if (nestind[co] >= 0) {
      nest = &nests[nestind[co]];
      if (intot + nest->ninside >= *arraySize || 
          outtot + nest->noutside >= *arraySize)
        return(FWRAP_ERROR_FILE_TO_BIG);
      for (lis = 0; lis < nest->ninside; lis++)
        inCont[intot++] = nest->inside[lis] + 1;
      if (! *inOnly)
        for (lis = 0; lis < nest->noutside; lis++)
          outCont[outtot++] = nest->outside[lis] + 1;
      level[co] = nest->level;
    }
    inIndex[co + 1] = intot + 1;
    if (! *inOnly)
      outIndex[co + 1] = outtot + 1;
  }

  /* clean up everything */
  imodContourFreeNests(nests, numnests);
  for (co = 0; co < obj->contsize; co++)
    if (scancont[co]->flags & ICONT_SCANLINE)
      imodContourDelete(scancont[co]);
  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
  free(nestind);
  free(scancont);
  free(pmin);
  free(pmax);

  return FWRAP_NOERROR;
}

/*
$Log$
Revision 3.20  2005/05/24 18:00:34  mast
Implemented partial reading/writing mode

Revision 3.19  2005/05/20 06:06:13  mast
Fixed limits on number of points to match model.inc

Revision 3.18  2005/04/12 20:12:05  mast
Added function to set object color

Revision 3.17  2005/04/04 22:41:54  mast
Fixed problem with argument order to imdContourGetBBox

Revision 3.16  2005/03/20 19:56:49  mast
Eliminating duplicate functions

Revision 3.15  2005/01/30 17:44:39  mast
Make scanlines contours only when needed for nesting analysis

Revision 3.14  2005/01/29 20:26:19  mast
Added routine to return information on nested contours

Revision 3.13  2004/09/21 20:11:26  mast
Changes for new clipping plane structure

Revision 3.12  2004/06/17 17:48:12  mast
Added calls to set zscale and rotation

Revision 3.11  2003/10/24 03:03:04  mast
change to use correct f-to-c string converter

Revision 3.10  2003/08/08 16:25:17  mast
Added functions to get object name and # of objects

Revision 3.9  2003/07/31 22:07:42  mast
Complete set of object views before writing so new objects have the same
properties in all views

Revision 3.8  2003/07/17 14:27:24  mast
Set point size and object flags in current view as well as object itself

Revision 3.7  2003/06/10 23:20:38  mast
Add call to set symbol size and type

Revision 3.6  2003/02/21 23:58:10  mast
Open files in binary mode

Revision 3.5  2002/07/20 23:25:46  mast
Make getimodhead return image origin values so that programs can get
back to index coordinates of full-sized volume

Revision 3.4  2002/07/07 20:10:57  mast
Added getimodscales to return scaling factors for getting between model
and index coordinates

Revision 3.3  2002/05/20 15:53:39  mast
Added ability to set scattered point size, and get arrays of contour
time and surface values

Revision 3.2  2001/12/05 15:59:19  mast
Provide a function so that Wimp model reading routines can read VMS floats

*/
