/*
 *   FILE: imodel_fwrap.c
 *
 *   PURPOSE: Load an IMOD model file from Fortran code.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "imodel.h"

/* These values should match max_obj_num and max_pt in model.inc */
#define FWRAP_MAX_OBJECT 1000000
#define FWRAP_MAX_POINTS 20000000
#define FWRAP_MAX_CLIP_PLANES 100

#define FWRAP_NOERROR              0
#define FWRAP_ERROR_BAD_FILENAME  -1
#define FWRAP_ERROR_FILE_NOT_IMOD -2
#define FWRAP_ERROR_FILE_TO_BIG   -3
#define FWRAP_ERROR_NO_OBJECTS    -4
#define FWRAP_ERROR_NO_MODEL      -5
#define FWRAP_ERROR_BAD_OBJNUM    -6
#define FWRAP_ERROR_MEMORY        -7
#define FWRAP_ERROR_NO_VALUE      -8
#define FWRAP_ERROR_STRING_LEN    -9
#define FWRAP_ERROR_FROM_CALL     -10

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
#define getimodheado  GETIMODHEADO
#define getimodscales GETIMODSCALES
#define putimageref  PUTIMAGEREF
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
#define getscatsize  GETSCATSIZE
#define putscatsize  PUTSCATSIZE
#define putsymsize   PUTSYMSIZE
#define putsymtype   PUTSYMTYPE
#define putsymflags  PUTSYMFLAGS
#define putlinewidth PUTLINEWIDTH
#define getobjcolor GETOBJCOLOR
#define putobjcolor PUTOBJCOLOR
#define getpointvalue GETPOINTVALUE
#define getcontvalue GETCONTVALUE
#define putpointvalue PUTPOINTVALUE
#define putcontvalue PUTCONTVALUE
#define putvalblackwhite PUTVALBLACKWHITE
#define putimodzscale PUTIMODZSCALE
#define putimodrotation PUTIMODROTATION
#define getimodtimes  GETIMODTIMES
#define getcontpointsizes  GETCONTPOINTSIZES
#define putcontpointsizes  PUTCONTPOINTSIZES
#define getimodsurfaces  GETIMODSURFACES
#define getobjsurfaces  GETOBJSURFACES
#define getimodobjname  GETIMODOBJNAME
#define putimodobjname  PUTIMODOBJNAME
#define getmodelname  GETMODELNAME
#define putmodelname  PUTMODELNAME
#define getimodobjsize  GETIMODOBJSIZE
#define getimodnesting  GETIMODNESTING
#define imodpartialmode IMODPARTIALMODE
#define getimodobjlist  GETIMODOBJLIST
#define getimodobjrange GETIMODOBJRANGE
#define imodarraylimits IMODARRAYLIMITS
#define imodhasimageref IMODHASIMAGEREF
#define clearimodobjstore CLEARIMODOBJSTORE
#define deleteimodcont DELETEIMODCONT
#define deleteimodpoint DELETEIMODPOINT
#define getobjvaluethresh GETOBJVALUETHRESH
#define findaddminmax1value FINDADDMINMAX1VALUE
#else
#define newimod      newimod_
#define deleteimod   deleteimod_
#define deleteiobj   deleteiobj_
#define getimod      getimod_
#define putimod      putimod_
#define openimoddata openimoddata_
#define getimodhead  getimodhead_
#define getimodheado  getimodheado_
#define getimodscales getimodscales_
#define putimageref  putimageref_
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
#define getscatsize  getscatsize_
#define putscatsize  putscatsize_
#define putsymsize   putsymsize_
#define putsymtype   putsymtype_
#define putsymflags  putsymflags_
#define putlinewidth putlinewidth_
#define getobjcolor getobjcolor_
#define putobjcolor putobjcolor_
#define getpointvalue getpointvalue_
#define getcontvalue getcontvalue_
#define putpointvalue putpointvalue_
#define putcontvalue putcontvalue_
#define putvalblackwhite putvalblackwhite_
#define putimodzscale putimodzscale_
#define putimodrotation putimodrotation_
#define getimodtimes  getimodtimes_
#define getcontpointsizes  getcontpointsizes_
#define putcontpointsizes  putcontpointsizes_
#define getimodsurfaces  getimodsurfaces_
#define getobjsurfaces  getobjsurfaces_
#define getimodobjname  getimodobjname_
#define putimodobjname  putimodobjname_
#define getmodelname  getmodelname_
#define putmodelname  putmodelname_
#define getimodobjsize  getimodobjsize_
#define getimodnesting  getimodnesting_
#define imodpartialmode imodpartialmode_
#define getimodobjlist  getimodobjlist_
#define getimodobjrange getimodobjrange_
#define imodarraylimits imodarraylimits_
#define imodhasimageref imodhasimageref_
#define clearimodobjstore clearimodobjstore_
#define deleteimodcont deleteimodcont_
#define deleteimodpoint deleteimodpoint_
#define getobjvaluethresh getobjvaluethresh_
#define findaddminmax1value findaddminmax1value_
#endif

/* Declare anything that is going to be called internally! */
int getimodobjrange(int *objStart, int *objEnd, int ibase[], int npt[],       
                    float coord[][3], int color[][2], int *npoint, 
                    int *nobject);
int openimoddata(char *fname, int fsize);
int getimodobjlist(int objList[], int *ninList, int ibase[], int npt[],
                   float coord[][3], int color[][2], int *npoint, 
                   int *nobject);
int getpointvalue(int *ob, int *co, int *pt, float *value);
int putpointvalue(int *ob, int *co, int *pt, float *value);
static void deleteFimod();
static int getMeshTrans(Ipoint *trans);
void putimodflag(int *objnum, int *flag);
static int checkAssignObject(int *ob);
static int checkAssignObjCont(int *ob, int *co);

typedef struct {
  int ob;
  int co;
  int num;
  float *sizes;
} SizeStruct;

typedef struct {
  int ob;
  int co;
  int pt;
  float value;
} ValueStruct;

typedef struct {
  int ob;
  char *name;
} NameStruct;

static Imod *sImod = NULL;

static int sPartialMode = 0;
static int sMaxObjects = FWRAP_MAX_OBJECT;
static int sMaxPoints = FWRAP_MAX_POINTS;
static int sNumFlagsPut = 0;
static int *sFlagsPut = NULL;
static int sMaxesPut = 0;
static int sXmaxPut, sYmaxPut, sZmaxPut;
static float sZscalePut = NO_VALUE_PUT;
static Ipoint sRotationPut = {NO_VALUE_PUT, NO_VALUE_PUT, NO_VALUE_PUT};
static int sNumSizesPut = 0;
static SizeStruct *sSizesPut = NULL;
static int sNumValuesPut = 0;
static int sMaxValues = 0;
static ValueStruct *sValuesPut = NULL;
static NameStruct *sNamesPut = NULL;
static int sNumNamesPut = 0;

/* Convenience pointers assigned by checkAssignObject... */
static Iobj *sObj;
static Icont *sCont;

/* These are not bit flags so they can range up to 255 */
#define SCAT_SIZE_FLAG 3
#define SYMBOL_SIZE_FLAG 4
#define SYMBOL_TYPE_FLAG 5
#define OBJECT_COLOR_FLAG 6
#define USE_VALUE_FLAGS  7
#define VAL_BLACKWHITE_FLAG  8
#define PNT_ON_SEC_FLAG 9
#define THICKEN_CONT_FLAG 10
#define SYMBOL_FLAGS_FLAG 11
#define SYMBOL_WIDTH_FLAG 12

#define FLAG_VALUE_SHIFT 8

/* DNM: a common function to delete model and object flags */
static void deleteFimod()
{
  int i;
  if (sImod)
    imodDelete(sImod);
  sImod = NULL;
  if (sFlagsPut)
    free(sFlagsPut);
  sFlagsPut = NULL;
  sNumFlagsPut = 0;
  sMaxesPut = 0;
  sZscalePut = NO_VALUE_PUT;
  sRotationPut.x = sRotationPut.y = sRotationPut.z = NO_VALUE_PUT;
  for (i = 0; i < sNumSizesPut; i++)
    if (sSizesPut[i].sizes)
      free(sSizesPut[i].sizes);
  if (sNumSizesPut && sSizesPut)
    free(sSizesPut);
  sNumSizesPut = 0;
  sSizesPut = NULL;
  if (sValuesPut)
    free(sValuesPut);
  sValuesPut = NULL;
  sNumValuesPut = 0;
  sMaxValues = 0;
  for (i = 0; i < sNumNamesPut; i++)
    free(sNamesPut[i].name);
  if (sNamesPut)
    free(sNamesPut);
  sNumNamesPut = 0;
  sNamesPut = NULL;
}

static int checkAssignObject(int *ob)
{
  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);
  if (*ob < 1 || *ob > sImod->objsize || !sImod->obj)
    return(FWRAP_ERROR_BAD_OBJNUM);
  sObj = &sImod->obj[*ob - 1];
  return FWRAP_NOERROR;
}

static int checkAssignObjCont(int *ob, int *co)
{
  int err = checkAssignObject(ob);
  if (err)
    return err;
  if (*co < 1 || *co > sObj->contsize)
    return(FWRAP_ERROR_BAD_OBJNUM);
  sCont = &sObj->cont[*co - 1];
  return FWRAP_NOERROR;
}

static int getMeshTrans(Ipoint *trans)
{
  IrefImage *iref;

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  trans->x = trans->y = trans->z = 0.;
  iref = sImod->refImage;
  if (iref && (sImod->flags & IMODF_OTRANS_ORIGIN)) {
    trans->x = (iref->otrans.x - iref->ctrans.x) / iref->cscale.x;
    trans->y = (iref->otrans.y - iref->ctrans.y) / iref->cscale.y;
    trans->z = (iref->otrans.z - iref->ctrans.z) / iref->cscale.z;
  }
  return 0;
}

/*!
 * Sets limit on number of points and objects in calling arrays 
 */
void imodarraylimits(int *maxpt, int *maxob)
{
  sMaxObjects = *maxob;
  sMaxPoints = *maxpt;
}

/*!
 * Turns on mode for returning only part of model if [mode] is non-zero.
 */
void imodpartialmode(int *mode)
{
  sPartialMode = *mode;
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
  int one = 1;
  int err = openimoddata(fname, fsize);

  if (err)
    return err;

  *npoint = *nobject = 0;
  if (sPartialMode)
    return FWRAP_NOERROR;
  return (getimodobjrange(&one, &sImod->objsize, ibase, npt, coord, color,
                          npoint, nobject));
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

  sImod = model;
  /*
   *  Translate reference image coordinates to 
   *  the identity matrix.
   *  DNM 11/5/98: rearranged to correspond to proper conventions 
   */
  iref = sImod->refImage;

  if (sImod->flags & IMODF_FLIPYZ)
    imodFlipYZ(sImod);

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
         
    imodTransform(sImod, mat);
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

  if (!sImod)
    return FWRAP_ERROR_NO_MODEL;
  if (*ninList <= 0)
    return FWRAP_ERROR_NO_OBJECTS;

  /* Check object numbers and count contours and points */
  for (ind = 0; ind < *ninList; ind++) {
    ob = objList[ind] - 1;
    if (ob < 0 || ob >= sImod->objsize)
      return(FWRAP_ERROR_BAD_OBJNUM);
    obj = &(sImod->obj[ob]);
    ncontour += obj->contsize;
    for (co = 0; co < obj->contsize; co++)
      npoints += obj->cont[co].psize;
  }
  
  if (ncontour > sMaxObjects) {
    fprintf(stderr, "getimod: Too many contours in model for Fortran "
            "program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (npoints > sMaxPoints) {
    fprintf(stderr, "getimod: Too many points in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  *npoint  = npoints;
  *nobject = ncontour; 

  for (ind = 0; ind < *ninList; ind++) {
    ob = objList[ind] - 1;
    obj = &(sImod->obj[ob]);
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

  if (!sImod)
    return FWRAP_ERROR_NO_MODEL;
  if (ninList <= 0)
    return FWRAP_ERROR_NO_OBJECTS;
  objList = (int *)malloc(ninList * sizeof(int));
  if (!objList)
    return FWRAP_ERROR_MEMORY;
  for (i = 0; i < ninList; i++)
    objList[i] = *objStart + i;
  i = getimodobjlist(objList, &ninList, ibase, npt, coord, color, npoint, nobject);
  free(objList);
  return i;
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
  if (!sImod) return(FWRAP_ERROR_NO_MODEL);
  if (!sImod->objsize) return(FWRAP_ERROR_NO_OBJECTS);
  model = sImod;

  for (ob = 0; ob < model->objsize; ob++) {
    obj = &(model->obj[ob]);
    if (!iobjScat(obj->flags))
      continue;
    maxobj = ob+1;
    for (co = 0; co < obj->contsize; co++)
      npoints += obj->cont[co].psize;
  }

  if (maxobj > sMaxObjects) {
    fprintf(stderr, "getimodscat: Too many contours in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (npoints > sMaxPoints) {
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
 * maximum size is specified by [limindex].  The vertex
 * coordinates are shifted to correspond to image index coordinate coordinates
 * of the full volume.
 */
int  getimodmesh(int *objnum, float *verts, int *index, int *limverts, 
                  int *limindex)
{
  int i, m, vsum, lsum, resol;
  Iobj *obj;
  Imesh *mesh;
  Ipoint trans;

  if (getMeshTrans(&trans))
    return(FWRAP_ERROR_NO_MODEL);
  sImod->cindex.object = *objnum - 1;
  obj = &(sImod->obj[sImod->cindex.object]);
  if (!obj->meshsize)
    return(-1);

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
      for (i = 0; i < mesh[m].vsize; i += 2) {
        *verts++ = mesh[m].vert[i].x + trans.x;
        *verts++ = mesh[m].vert[i].y + trans.y; 
        *verts++ = mesh[m].vert[i].z + trans.z;
        *verts++ = mesh[m].vert[i + 1].x;
        *verts++ = mesh[m].vert[i + 1].y; 
        *verts++ = mesh[m].vert[i + 1].z;
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
 * [nindex] are returned with the number of vertices and indexes.  The vertex
 * coordinates are shifted to correspond to image index coordinate coordinates
 * of the full volume.
 */
int  getimodverts(int *objnum, float *verts, int *index, int *limverts, 
                   int *limindex, int *nverts, int *nindex)
{
  int i, j, mi, resol, vsum, m, normAdd, vertBase, listInc;
  Iobj *obj;
  Imesh *mesh;
  Ipoint trans;

  if (getMeshTrans(&trans))
    return(FWRAP_ERROR_NO_MODEL);
  sImod->cindex.object = *objnum - 1;
  obj = &(sImod->obj[sImod->cindex.object]);
  if (!obj->meshsize)
    return(-1);

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
      *verts = mesh->vert[i].x + trans.x;
      verts++;
      *verts = mesh->vert[i].y + trans.y; 
      verts++;
      *verts = mesh->vert[i].z + trans.z;
      verts++;
    }
    *nverts += mi / 2;

    mi = mesh->lsize;
    i = 0;
    while (mesh->list[i] != IMOD_MESH_END && i < mi) {
      while (mesh->list[i] != IMOD_MESH_END && 
             !imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase,
                                      &normAdd) && i < mi)
        i++;
      if (imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase,
                                  &normAdd)) {
        index[j++] = IMOD_MESH_BGNPOLYNORM;
        i++;
        while (mesh->list[i] != IMOD_MESH_ENDPOLY && i < mi) {
          if (j + 6 > *limindex) {
            printf ("%d %d %d %d\n",i,mi,j, *limindex);
            fprintf(stderr, "getimodverts: Too many indices"
                    " in mesh for Fortran program\n");
            return(FWRAP_ERROR_FILE_TO_BIG);
          }
          index[j++] = mesh->list[i+vertBase] / 2;
          index[j++] = mesh->list[i+vertBase + listInc] / 2;
          index[j++] = mesh->list[i+vertBase + 2 * listInc] / 2;
          i += 3 * listInc;
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
 * specifies the size of the array and the number of points is returned 
 * in [nsizes].
 */
int getimodsizes(int *ob, float *sizes, int *limsizes, int *nsizes)
{
  Icont *cont;
  int co, pt;
  *nsizes = 0;
  if ((co = checkAssignObject(ob)))
    return co;
  for (co = 0; co < sObj->contsize; co++) {
    cont = &sObj->cont[co];
    if (*nsizes + cont->psize > *limsizes) {
      fprintf(stderr, "getimodsizes: Model too large for Fortran program\n");
      return(FWRAP_ERROR_FILE_TO_BIG);
    }
    for (pt = 0; pt < cont->psize; pt++)
      *sizes++ = imodPointGetSize(sObj, cont, pt);
    *nsizes += cont->psize;
  }
     
  return FWRAP_NOERROR;
}          

/*!
 * Returns the values in the point size array for contour [co] of object [ob]
 * into array [sizes]; [limsizes] specifies the size of the array and the 
 * number of points is returned in [nsizes].  The array contains a -1 for 
 * points with the default size.  Returns no points if there is no
 * point size array.
 */
int getcontpointsizes(int *ob, int *co, float *sizes, int *limsizes, 
                      int *nsizes)
{
  int pt;
  *nsizes = 0;
  if ((pt = checkAssignObjCont(ob, co)))
    return pt;
  if (!sCont->sizes)
    return FWRAP_NOERROR;
    
  if (sCont->psize > *limsizes) {
      fprintf(stderr, "getcontpointsizes: Too many points for array\n");
      return(FWRAP_ERROR_FILE_TO_BIG);
  }
  for (pt = 0; pt < sCont->psize; pt++)
    *sizes++ = sCont->sizes[pt];
  *nsizes = sCont->psize;
  return FWRAP_NOERROR;
}          

/*!
 * Fills the point size array for contour [co] of object [ob] with the values
 * in the array [sizes]; [nsizes] specifies the number of values in the array.
 */
int putcontpointsizes(int *ob, int *co, float *sizes, int *nsizes)
{
  SizeStruct *sizetmp;

  /* Saves the sizes in a new element of the size structure array */
  if (!sNumSizesPut) 
    sizetmp = B3DMALLOC(SizeStruct, 1);
  else
    sizetmp = (SizeStruct *)realloc(sSizesPut, (sNumSizesPut + 1) * 
                                  sizeof(SizeStruct));
  if (!sizetmp)
    return FWRAP_ERROR_MEMORY;
  sSizesPut = sizetmp;
  sSizesPut[sNumSizesPut].ob = *ob - 1;
  sSizesPut[sNumSizesPut].co = *co - 1;
  sSizesPut[sNumSizesPut].num = *nsizes;
  sSizesPut[sNumSizesPut].sizes = B3DMALLOC(float, *nsizes);
  if (!sSizesPut[sNumSizesPut].sizes)
    return FWRAP_ERROR_MEMORY;
  memcpy(sSizesPut[sNumSizesPut++].sizes, sizes, *nsizes * sizeof(float));
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

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  for (ob = 0; ob < sImod->objsize; ob++) {
    obj = &(sImod->obj[ob]);
    for (co = 0; co < obj->contsize; co++, coi++)
      times[coi] = obj->cont[co].time;
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

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  for (ob = 0; ob < sImod->objsize; ob++) {
    obj = &(sImod->obj[ob]);
    for (co = 0; co < obj->contsize; co++, coi++)
      surfs[coi] = obj->cont[co].surf;
  }

  return FWRAP_NOERROR;
}

/*!
 * Returns surface numbers for all contours in object [ob] into the array [surfs].
 * If [sortSurfs] > 0, it analyzes a mesh to sort contours into surfaces using 
 * @@iobj.html#imodObjectSortSurf@.
 */
int getobjsurfaces(int *ob, int *sortSurfs, int *surfs)
{
  Icont *contSave = NULL;
  int retval = FWRAP_NOERROR;
  int co;

  if ((co = checkAssignObject(ob)))
    return co;
  if (*sortSurfs > 0 && sObj->contsize > 0) {

    /* Duplicate the contour array so that surfaces can be assigned in it */
    contSave = B3DMALLOC(Icont, sObj->contsize);
    if (!contSave)
      return FWRAP_ERROR_MEMORY;
    for (co = 0; co < sObj->contsize; co++)
      imodContourCopy(&sObj->cont[co], &contSave[co]);

    /* Sort the surfaces.  If no meshes, return all zeros; otherwise return surfaces */
    co = imodObjectSortSurf(sObj);
    if (co == 2)
      retval = FWRAP_ERROR_MEMORY;
    else if (co == 1)
      for (co = 0; co < sObj->contsize; co++)
        surfs[co] = 0;
    else
      for (co = 0; co < sObj->contsize; co++)
        surfs[co] = sObj->cont[co].surf;
    free(sObj->cont);
    sObj->cont = contSave;
  } else 

    /* If not sorting surfaces, return existing values */
    for (co = 0; co < sObj->contsize; co++)
      surfs[co] = sObj->cont[co].surf;
  return retval;
}

/*!
 * Returns the general value for contour [co] of object [ob] into [value].  The return 
 * value is -8 if there is no value.
 */
int getcontvalue(int *ob, int *co, float *value)
{
  int pt = 0;
  return getpointvalue(ob, co, &pt, value);
}

/*!
 * Returns the general value for point [pt] in contour [co] of object [ob]
 * into [value].  If [pt] <= 0, has the same effect as @@getcontvalue@.  The return 
 * value is -8 if there is no value.
 */
int getpointvalue(int *ob, int *co, int *pt, float *value)
{
  int index, after, i;
  Istore *item;
  Ilist *store;
  if ((i = checkAssignObjCont(ob, co)))
    return i;
  if (*pt < 1) {
    store = sObj->store;
    index = istoreLookup(store, *co - 1, &after);
  } else {
    if (*pt > sCont->psize)
      return(FWRAP_ERROR_BAD_OBJNUM);
    store = sCont->store;
    index = istoreLookup(store, *pt - 1, &after);
  }
  if (index < 0)
    return(FWRAP_ERROR_NO_VALUE);
  for (i = index; i < after; i++) {
    item = istoreItem(store, i);
    if (item->type == GEN_STORE_VALUE1) {
      *value = item->value.f;
      return FWRAP_NOERROR;
    }
  }
  return FWRAP_ERROR_NO_VALUE;
}

/*!
 * Stores a general value [value] for contour [co] of object [ob].
 */
int putcontvalue(int *ob, int *co, float *value)
{
  int pt = 0;
  return putpointvalue(ob, co, &pt, value);
}

/*!
 * Stores a general value [value] for point [pt] in contour [co] of object 
 * [ob].  If [pt] is <= 0 then the effect is the same as calling @@putcontvalue@.
 */
int putpointvalue(int *ob, int *co, int *pt, float *value)
{
  if (sNumValuesPut >= sMaxValues) {
    if (sMaxValues) {
      sMaxValues += 100;
      sValuesPut = (ValueStruct *)realloc
        (sValuesPut, sMaxValues * sizeof(ValueStruct));
    } else {
      sMaxValues += 100;
      sValuesPut = (ValueStruct *)malloc(sMaxValues * sizeof(ValueStruct));
    }
  }
  if (!sValuesPut)
    return FWRAP_ERROR_MEMORY;
  sValuesPut[sNumValuesPut].ob = *ob - 1;
  sValuesPut[sNumValuesPut].co = *co - 1;
  sValuesPut[sNumValuesPut].pt = *pt - 1;
  sValuesPut[sNumValuesPut++].value = *value;
  return FWRAP_NOERROR;
}

/*!
 * Deletes the store structure for the given object [ob], if any.
 */
int clearimodobjstore(int *ob)
{
  int err = checkAssignObject(ob);
  if (err)
    return err;
  if (sObj->store)
    ilistDelete(sObj->store);
  sObj->store = NULL;
  return FWRAP_NOERROR;
}

/*!
 * Deletes contour [co] in object [ob] using @imodel.html#imodDeleteContour, which will
 * adjust contour numbers in an object store, if any.
 */
int deleteimodcont(int *ob, int *co)
{
  int err = checkAssignObjCont(ob, co);
  if (err)
    return err;
  imodSetIndex(sImod, *ob - 1, *co - 1, -1);
  if (imodDeleteContour(sImod, *co - 1) < 0)
    return(FWRAP_ERROR_FROM_CALL);
  return FWRAP_NOERROR;
}

/*!
 * Deletes point [pt] in contour [co] of object [ob] using @imodel.html#imodPointDelete,
 *  which will adjust sizes, labels, and general storage items in the contour, if any.
 */
int deleteimodpoint(int *ob, int *co, int *pt)
{
  int err = checkAssignObjCont(ob, co);
  if (err)
    return err;
  if (*pt < 1 || *pt > sCont->psize)
    return(FWRAP_ERROR_BAD_OBJNUM);
  if (imodPointDelete(sCont, *pt - 1) < 0)
    return(FWRAP_ERROR_FROM_CALL);
  return FWRAP_NOERROR;
}

/*!
 * Returns the threshold value for object [ob] in [thresh].  This value is based on the
 * min and max contour values in the object as the value of the byte {valblack} as stored
 * by imodfindbeads and findbeads3d or adjusted in beadfixer.  The return value is -8 if
 * there is no min/max stored.
 */
int getobjvaluethresh(int *ob, float *thresh)
{
  float vmin, vmax;
  int err = checkAssignObject(ob);
  if (err)
    return err;
  if (!istoreGetMinMax(sObj->store, sObj->contsize, GEN_STORE_MINMAX1, &vmin, &vmax))
    return FWRAP_ERROR_NO_VALUE;
  *thresh = sObj->valblack * (vmax - vmin) / 255. + vmin;
  return FWRAP_NOERROR;
}

/*! 
 * Updates the min and max values for VALUE1 for contours and points of object [ob], using
 * @@istore.html#istoreFindAddMinMax1@.
 */
int findaddminmax1value(int *ob)
{
  int err = checkAssignObject(ob);
  if (err)
    return err;
  err = istoreFindAddMinMax1(sObj);
  if (err > 0)
    return FWRAP_ERROR_FROM_CALL;
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
 * mode is off, all existing contour data in all objects are deleted, and replaced by
 * the points in the contours being stored.  If partial
 * mode is on, existing contour data will be deleted only from objects included 
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

  if (!sImod) {
    sImod = imodNew();
    sImod->objsize = 0;
    sImod->obj = NULL;
  }

  mincolor = 256 - sImod->objsize;

  /* Find minimum color, and maximum object #; get arrays */
  /* Skip empty contours unless we are in partial mode, where they are needed
     to signal that an existing object is now empty */
  for (object = 0; object < *nobject; object++) {
    if (npt[object] == 0 && !sPartialMode) 
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
  for (ob = 0; ob < sImod->objsize; ob++) {
    objlookup[(255 - mincolor) - ob] = ob;
    nobj++;
    if (sPartialMode)
      nsaved[ob] = sImod->obj[ob].contsize;
  }

  /* For all non-empty contours passed back, mark an empty object as having
     data.  In partial mode, for ALL contours passed back for an existing 
     object, set nsaved back to
     0 so that if all contours got deleted they will be deleted from object */
  for (object = 0; object < *nobject; object++) {
    if (npt[object] == 0 && !sPartialMode)
      continue;
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
    objlookup[ob] = sImod->objsize;
    imodNewObject(sImod);
    wimpno = ob + mincolor;

    /* Just use top 6 colors and let new color scheme hold after that */
    if (wimpno >= 250) {
      sImod->obj[nobj].red   = Wmod_Colors[wimpno - 247][0];
      sImod->obj[nobj].green = Wmod_Colors[wimpno - 247][1];
      sImod->obj[nobj].blue  = Wmod_Colors[wimpno - 247][2];
        
    } /*else{
        sImod->obj[nobj].red   =  wimpno / 255.0f;
        sImod->obj[nobj].green = wimpno / 255.0f;
        sImod->obj[nobj].blue  = wimpno / 255.0f;
        } */
    sImod->obj[nobj].flags |= IMOD_OBJFLAG_OPEN;

    /* Find object name in list if any */
    ci = 0;
    for (pt = 0; pt < sNumNamesPut; pt++) {
      if (sNamesPut[pt].ob == 255 - (ob + mincolor)) {
        strncpy(sImod->obj[nobj].name, sNamesPut[pt].name, IOBJ_STRSIZE - 1);
        sImod->obj[nobj].name[IOBJ_STRSIZE - 1] = 0x00;
        ci = 1;
      }
    }
    if (!ci)
      sprintf(sImod->obj[nobj].name, "Wimp no. %d", wimpno);
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
    if (ob > sImod->objsize) {
      fprintf(stderr, "putimod warning: bad object bounds %d\n", ob);
      continue;
    }
    obj = &sImod->obj[ob];

    if (nsaved[ob] < obj->contsize) {

      /* Existing contour */
      cont = &(obj->cont[nsaved[ob]]);

      /* Possible criterion was to require that number of points match,
         but this lost surface numbers with point reduction */
      /* For empty contour, clear out existing data */
      if (!npt[object] && cont->psize) {
        imodContourClear(cont);
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
      free(cont);
    }
    nsaved[ob]++;
  }

  /*
   *  Remove old contours that were not replaced
   */     
  for (ob = 0; ob < sImod->objsize; ob++) {
    if (imodContoursDeleteToEnd(&(sImod->obj[ob]), nsaved[ob])) {
      free(objlookup);
      free(nsaved);
      return FWRAP_ERROR_MEMORY;
    }
  }   
  free(objlookup);
  free(nsaved);
  return FWRAP_NOERROR;
}

/*!
 * Writes the current model structure to the file given by [fname] after 
 * transforming back to index coordinates.  Called from fortran as 
 * [writeimod(fname)].
 */
int writeimod(char *fname, int fsize)
{
  int i, j, ob, flag, value, flagmask;
  int retcode = 0;
  Iobjview *objview;
  Iobj *obj;
  Icont *cont;
  Istore store;
  char *filename = f2cString(fname, fsize);
  if (!filename)
    return(FWRAP_ERROR_BAD_FILENAME);
  if (!sImod) {
    free(filename); 
    return(FWRAP_ERROR_NO_MODEL);
  }

  /*
   *  Translate coordinates to match reference image.
   *  DNM 11/5/98: rearranged to correspond to proper conventions 
   * 5/21/05: moved this up from putimod
   */
  if (sImod->refImage) {
    IrefImage *iref = sImod->refImage;
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
    
    imodTransform(sImod, mat);
    imodMatDelete(mat);


  }

  /* Reflip data back if necessary */

  if (sImod->flags & IMODF_FLIPYZ)
    imodFlipYZ(sImod);

  /* Put out the flag data */
  flagmask = (1 << FLAG_VALUE_SHIFT) - 1;
  for (i = 0; i < sNumFlagsPut; i++) {
    ob = sFlagsPut[2 * i];
    flag = sFlagsPut[2 * i + 1];
    value = flag >> FLAG_VALUE_SHIFT;
    flag &= flagmask;
    /* printf("object %d  flag %d  value %d\n", ob, flag, value); */
    if (ob >= 0 && ob < sImod->objsize) {

      /* Save some data in the current view if it has an object view for
         this object */
      objview = NULL;
      obj = &sImod->obj[ob];
      if (sImod->cview > 0 && sImod->view[sImod->cview].objvsize > ob)
        objview = &(sImod->view[sImod->cview].objview[ob]);

      switch (flag) {
      case 0:
        obj->flags &= ~IMOD_OBJFLAG_OPEN;
        obj->flags &= ~IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = obj->flags;
        break;
      case 1:
        obj->flags |= IMOD_OBJFLAG_OPEN;
        obj->flags &= ~IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = obj->flags;
        break;
      case 2:
        obj->flags &= ~IMOD_OBJFLAG_OPEN;
        obj->flags |= IMOD_OBJFLAG_SCAT;
        if (objview)
          objview->flags = obj->flags;
        break;
      
      case SCAT_SIZE_FLAG:
        obj->pdrawsize = value;
        if (objview)
          objview->pdrawsize = obj->pdrawsize;
        break;
        
      case SYMBOL_SIZE_FLAG:
        obj->symsize = value;
        break;
      case SYMBOL_TYPE_FLAG:
        obj->symbol = value;
        break;
      case SYMBOL_FLAGS_FLAG:
        obj->symflags = value;
        break;
      case SYMBOL_WIDTH_FLAG:
        obj->linewidth2 = value;
        break;
      case OBJECT_COLOR_FLAG:
        obj->red = (value & 255) / 255.;
        obj->green = ((value >> 8) & 255) / 255.;
        obj->blue = ((value >> 16) & 255) / 255.;
        if (objview) {
          objview->red = obj->red;
          objview->green = obj->green;
          objview->blue = obj->blue;
        }
        break;
      case USE_VALUE_FLAGS:
        obj->flags |= IMOD_OBJFLAG_USE_VALUE;
        obj->matflags2 |= MATFLAGS2_CONSTANT | MATFLAGS2_SKIP_LOW;
        if (objview) {
          objview->flags = obj->flags;
          objview->matflags2 = obj->matflags2;
        }
        break;
      case VAL_BLACKWHITE_FLAG:
        obj->valblack = value & 255;
        obj->valwhite = (value >> 8) & 255;
        if (objview) {
          objview->valblack = obj->valblack;
          objview->valwhite = obj->valwhite;
        }
        break;
      case PNT_ON_SEC_FLAG:
        obj->flags |= IMOD_OBJFLAG_PNT_ON_SEC;
        if (objview)
          objview->flags = obj->flags;
        break;
      case THICKEN_CONT_FLAG:
        obj->flags |= IMOD_OBJFLAG_THICK_CONT;
        if (objview)
          objview->flags = obj->flags;
        break;

      }
    }
  }
       
  if (sMaxesPut) {
    sImod->xmax = sXmaxPut;
    sImod->ymax = sYmaxPut;
    sImod->zmax = sZmaxPut;
  }

  /* Save Zscale, save rotation in current view if there is one */
  if (sZscalePut != NO_VALUE_PUT)
    sImod->zscale = sZscalePut;
  if (sImod->cview > 0 && sRotationPut.x != NO_VALUE_PUT)
    sImod->view[sImod->cview].rot = sRotationPut;

  /* Put out the sizes into the contours */
  for (i = 0; i < sNumSizesPut; i++) {

    /* Ignore illegal object or contour numbers */
    if (sSizesPut[i].ob < 0 || sSizesPut[i].ob >= sImod->objsize)
      continue;
    obj = &sImod->obj[sSizesPut[i].ob];
    if (sSizesPut[i].co < 0 || sSizesPut[i].co >= obj->contsize)
      continue;

    /* Fill up to the limit on the number of point in array and contour */
    cont = &obj->cont[sSizesPut[i].co];
    value = B3DMIN(sSizesPut[i].num, cont->psize);
    for (j = 0; j < value; j++)
      if (sSizesPut[i].sizes[j] >= 0.)
        imodPointSetSize(cont, j, sSizesPut[i].sizes[j]);
  }

  /* Put out general values */
  if (sNumValuesPut) {
    store.type = GEN_STORE_VALUE1;
    store.flags = (GEN_STORE_FLOAT << 2) | GEN_STORE_ONEPOINT;
    for (i = 0; i < sNumValuesPut; i++) {

      /* Ignore illegal object or contour numbers */
      if (sValuesPut[i].ob < 0 || sValuesPut[i].ob >= sImod->objsize)
        continue;
      obj = &sImod->obj[sValuesPut[i].ob];
      if (sValuesPut[i].co < 0 || sValuesPut[i].co >= obj->contsize)
        continue;

      /* Insert value into contour or object stores */
      store.value.f = sValuesPut[i].value;
      if (sValuesPut[i].pt < 0) {
        store.index.i = sValuesPut[i].co;
        istoreInsertChange(&obj->store, &store);
      } else {
        cont = &obj->cont[sValuesPut[i].co];
        if (sValuesPut[i].pt > cont->psize)
          continue;
        store.index.i = sValuesPut[i].pt;
        istoreInsertChange(&cont->store, &store);
      }
    }
    
    /* Now need to set min/maxes for all stores */
    for (ob = 0; ob < sImod->objsize; ob++)
      istoreFindAddMinMax1(&sImod->obj[ob]);
  }
     
  /* Complete the set of object views just before writing */
  imodObjviewComplete(sImod);
  sImod->file = fopen(filename, "wb");
  retcode = imodWriteFile(sImod);
  free(filename);
  return(retcode);
}

/*!
 * Puts scattered points back into object [ob] from array [verts].  The
 * number of points must be the same as the number in the original object.
 */
int putimodscat(int *ob, float *verts)
{
  Icont *cont;
  int co, pt, v;
     
  if ((co = checkAssignObject(ob)))
    return co;
  if (!iobjScat(sObj->flags)) return(2);

  for (co = 0, v = 0; co < sObj->contsize; co++) {
    cont = &sObj->cont[co];
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
  Iobj *obj = &(sImod->obj[sImod->cindex.object]);
  Ipoint *fvert;
  Ipoint trans;

  if (getMeshTrans(&trans))
    return(FWRAP_ERROR_NO_MODEL);

  obj->mesh->flag |= IMESH_FLAG_NMAG;
  mi = obj->mesh->vsize;
  fvert = obj->mesh->vert;

  for (i = 0; i < mi; i += 2) {
    fvert[i].x = *verts++ - trans.x;
    fvert[i].y = *verts++ - trans.y;
    fvert[i].z = *verts++ - trans.z;
    fvert[i + 1].x = *verts++;
    fvert[i + 1].y = *verts++;
    fvert[i + 1].z = *verts++;
  }
  return FWRAP_NOERROR;
}

/*!
 * Deletes current model and allocates a new, default model structure
 */
int newimod(void)
{
    deleteFimod();
    sImod = imodNew();
    if (!sImod)
      return FWRAP_ERROR_NO_MODEL;
    sImod->objsize = 0;
    sImod->obj = NULL;
    return FWRAP_NOERROR;
}

/*!
 * Deletes current model and frees all memory
 */
int deleteimod(void)
{
    if (!sImod)
      return FWRAP_ERROR_NO_MODEL;
    deleteFimod();
    return FWRAP_NOERROR;
}

/*!
 * Removes all objects from model.
 */
int deleteiobj(void)
{
    if (!sImod)
        return FWRAP_ERROR_NO_MODEL;

    imodObjviewsFree(sImod);
    imodObjectsDelete(sImod->obj, sImod->objsize);
    sImod->objsize = 0;
    sImod->obj = NULL;

    return FWRAP_NOERROR;
}


/*!
 * Returns number of IMOD objects in the model, or -1 for error.
 */
int getimodobjsize()
{
  if (!sImod)
    return -1;
  return sImod->objsize;
}

/*!
 * Returns pixel size in microns in [um] and Z-scale in [zscale]
 */
int getimodheado(float *um, float *zscale)
{
  int exval;

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  exval = sImod->units + 6;
  *um = sImod->pixsize * (pow(10.0, (double)exval));
  *zscale = sImod->zscale;
  return FWRAP_NOERROR;
}

/*!
 * Returns maximum X, Y, Z coordinates of image that model was loaded on
 */
int getimodmaxes(int *xmax, int *ymax, int *zmax)
{
  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  *xmax = sImod->xmax;
  *ymax = sImod->ymax;
  *zmax = sImod->zmax;
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

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  exval = sImod->units + 6;
  *um = sImod->pixsize * (pow(10.0, (double)exval));
  *zscale = sImod->zscale;

     

  iref = sImod->refImage;
     
  /* DNM 7/20/02: If image origin has been stored in otrans, return that
     because that is what is needed to get to full volume index coords */
  if (!iref) {
    *xoffset = 0;
    *yoffset = 0;
    *zoffset = 0;
  } else if (sImod->flags & IMODF_OTRANS_ORIGIN) {
    *xoffset = -iref->otrans.x;
    *yoffset = -iref->otrans.y;
    *zoffset = -iref->otrans.z;
  }else{
    *xoffset = -iref->ctrans.x;
    *yoffset = -iref->ctrans.y;
    *zoffset = -iref->ctrans.z;
  }


  if (sImod->flags & IMODF_FLIPYZ) {
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

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);

  iref = sImod->refImage;
     
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
 * Sets the image reference information of the model with the delta and origin
 * values from an MRC file header.
 */
int putimageref(float *delta, float *origin)
{
  IrefImage *iref;

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);
  if (!sImod->refImage) {
    sImod->refImage = (IrefImage *) malloc (sizeof(IrefImage));
    if (!sImod->refImage)
      return FWRAP_ERROR_MEMORY;
  }
  iref = sImod->refImage;
  iref->cscale.x = delta[0];
  iref->cscale.y = delta[1];
  iref->cscale.z = delta[2];
  iref->ctrans.x = origin[0];
  iref->ctrans.y = origin[1];
  iref->ctrans.z = origin[2];
  return FWRAP_NOERROR;
}

/*!
 * Returns 1 if a model has image reference information, 0 if it does not, or -5
 * for no model.
 */
int imodhasimageref()
{
  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);
  return sImod->refImage ? 1 : 0;
}

/*!
 * Return a flag for each object in array [flags]; [limflags] specifies the 
 * maximum size of the array.  The flag is the sum of 0, 1 or 2 for closed,
 * open or scattered point object, plus 4 if the object has a mesh.
 */
int getimodflags(int *flags, int *limflags)
{
  int i;

  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);
  if (sImod->objsize > *limflags) {
    fprintf(stderr, "getimodflags: Too many objects for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }
  for (i = 0; i < *limflags; i++)
    flags[i] = -1;
  for (i = 0; i < sImod->objsize; i++) {
    if (!sImod->obj[i].contsize)
      continue;
    flags[i]=0;
    if (sImod->obj[i].flags & IMOD_OBJFLAG_OPEN)
      flags[i] = 1;
    if (sImod->obj[i].flags & IMOD_OBJFLAG_SCAT)
      flags[i] = 2;
    if (sImod->obj[i].meshsize)
      flags[i] += 4;
  }
  return FWRAP_NOERROR;
}

/*!
 * Returns model name in [fname]; [fsize] is omitted in the call from fortran.
 */
int getmodelname(char *fname, int fsize)
{
  if (!sImod)
    return(FWRAP_ERROR_NO_MODEL);
  if (c2fString(sImod->name, fname, fsize))
    return FWRAP_ERROR_STRING_LEN;
  return FWRAP_NOERROR;
}

/*!
 * Returns name of object [ob] in [fname]; [fsize] is omitted in the call from
 * fortran.
 */
int getimodobjname(int *ob, char *fname, int fsize)
{
  int i;
  if ((i = checkAssignObject(ob)))
    return i;
  for (i = 0; i < fsize && i < IOBJ_STRSIZE && sObj->name[i] ; i++)
    fname[i] = sObj->name[i];
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
  int nout, i, iout = 0;
  if ((i = checkAssignObject(objnum)))
    return i;

  sImod->cindex.object = *objnum - 1;

  /* DNM 9/19/04: modified for multiple clip planes */
  nout = sObj->clips.count;
  if (nout > FWRAP_MAX_CLIP_PLANES)
    nout = FWRAP_MAX_CLIP_PLANES;

  for (i = 0; i < nout; i++) {
    clip[iout++] = sObj->clips.normal[i].x;
    clip[iout++] = sObj->clips.normal[i].y;
    clip[iout++] = sObj->clips.normal[i].z / sImod->zscale;
    clip[iout] = (sObj->clips.normal[i].x * sObj->clips.point[i].x) +
      (sObj->clips.normal[i].y * sObj->clips.point[i].y) +
      (sObj->clips.normal[i].z * sObj->clips.point[i].z);
    clip[iout++] *= sImod->pixsize;
  }
  return(nout);
}

/*!
 * Sets the maximum X, Y, Z coordinates for the model
 */
/* DNM 6/8/01: change this to save the values until a model is written */
int putimodmaxes(int *xmax, int *ymax, int *zmax)
{
  sXmaxPut = *xmax;
  sYmaxPut = *ymax;
  sZmaxPut = *zmax;
  sMaxesPut = 1;
  return FWRAP_NOERROR;
}

/*!
 * Specifies a flag or value for object [objnum]; in external calls [flag]
 * should be 0 for closed contour, 1 for open contour, 2 for scattered points,
 * 7 to set IMOD_OBJFLAG_USE_VALUE in {flags} and MATFLAGS2_CONSTANT and 
 * MATFLAGS2_SKIP_LOW in {matflags2}, 9 to set IMOD_OBJFLAG_PNT_ON_SEC, and 10 to set 
 * IMOD_OBJFLAG_THICK_CONT.
 */
void putimodflag(int *objnum, int *flag)
{
  if (sNumFlagsPut)
    sFlagsPut = (int *)realloc(sFlagsPut,
                               (sNumFlagsPut + 1) * 2 * sizeof(int));
  else
    sFlagsPut = (int *)malloc(2 * sizeof(int));
      
  if (!sFlagsPut) {
    sNumFlagsPut = 0;
    return;
  }

  sFlagsPut[sNumFlagsPut * 2] = *objnum - 1;
  sFlagsPut[sNumFlagsPut * 2 + 1] = *flag;
  sNumFlagsPut++;
}

/*!
 * Sets the Z-scale of the model to [zscale]
 */
void putimodzscale(float *zscale)
{
  sZscalePut = *zscale;
}

/*!
 * Sets the display rotation angles of the model to [xrot], [yrot], [zrot]
 */
void putimodrotation(float *xrot, float *yrot, float *zrot)
{
  sRotationPut.x = *xrot;
  sRotationPut.y = *yrot;
  sRotationPut.z = *zrot;
}

/* DNM 12/3/01: added so that old wimp models can be converted */
void fromvmsfloats(unsigned char *data, int *amt)
{
  imodFromVmsFloats(data, *amt);
}

/*!
 * Returns the 3D point size for [objnum] into [size]
 */
int getscatsize(int *objnum, int *size)
{
  int err = checkAssignObject(objnum);
  if (err)
    return err;
  *size = sObj->pdrawsize;
  return FWRAP_NOERROR;
}

/*!
 * Sets the 3D point size for [objnum] to [size]
 */
/* DNM 5/15/02: put scattered point sizes out in flags with an offset */
void putscatsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) + SCAT_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the symbol type of object [objnum] to [type]
 */
void putsymtype(int *objnum, int *type)
{
  int flag = (*type << FLAG_VALUE_SHIFT) + SYMBOL_TYPE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the symbol flags of object [objnum] to [flags], the sumof 1 for filled symbols and
 * 2 for marked ends.
 */
void putsymflags(int *objnum, int *flags)
{
  int flag = (*flags << FLAG_VALUE_SHIFT) + SYMBOL_FLAGS_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the 2D line width for [objnum] to [width]
 */
void putlinewidth(int *objnum, int *width)
{
  int flag = (*width << FLAG_VALUE_SHIFT) + SYMBOL_WIDTH_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Returns the color for object [objnum] into [red], [green], [blue], where 
 * values range from 0 to 255.
 */
int getobjcolor(int *objnum, int *red, int *green, int *blue)
{
  int err = checkAssignObject(objnum);
  if (err)
    return err;
  *red = B3DNINT(255. * sObj->red);
  *green = B3DNINT(255. * sObj->green);
  *blue = B3DNINT(255. * sObj->blue);
  return FWRAP_NOERROR;
}

/*!
 * Sets the color for object [objnum] to [red], [green], [blue], where values
 * range from 0 to 255.
 */
/* DNM 4/1/2/05: Add object color.  
   This whole approach should be replaced with creating empty objects */
void putobjcolor(int *objnum, int *red, int *green, int *blue)
{
  int r = B3DMIN(255, B3DMAX(0, *red));
  int g = B3DMIN(255, B3DMAX(0, *green));
  int b = B3DMIN(255, B3DMAX(0, *blue));
  int flag = ((r << FLAG_VALUE_SHIFT) | (g << FLAG_VALUE_SHIFT + 8) |
              (b << FLAG_VALUE_SHIFT + 16)) + OBJECT_COLOR_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the symbol size of object [objnum] to [size]
 */
void putsymsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) + SYMBOL_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the {valblack} and {valwhite} members of object [objnum] to [black] and
 * [white].
 */
void putvalblackwhite(int *objnum, int *black, int *white)
{
  int flag = (*black << FLAG_VALUE_SHIFT) + (*white << FLAG_VALUE_SHIFT + 8) +
    VAL_BLACKWHITE_FLAG;
  putimodflag(objnum, &flag);
}

/*!
 * Sets the model name to the string in [fname]
 */
int putmodelname(char *fname, int fsize)
{
  char *tmpstr;
  if (!sImod)
    return FWRAP_ERROR_NO_MODEL;
  tmpstr = f2cString(fname, fsize);
  if (!tmpstr)
    return FWRAP_ERROR_MEMORY;
  strncpy(sImod->name, tmpstr, IMOD_STRSIZE - 1);
  sImod->name[IMOD_STRSIZE - 1] = 0x00;
  free(tmpstr);
  return FWRAP_NOERROR;
}

/*!
 * Sets the name for object [objnum] to the string in [fname]
 */
void putimodobjname(int *objnum, char *fname, int fsize)
{
  if (!sNumNamesPut) 
    sNamesPut = (NameStruct *)malloc(sizeof(NameStruct));
  else
    sNamesPut = (NameStruct *)realloc(sNamesPut,
                                      (sNumNamesPut + 1) * sizeof(NameStruct));
  if (!sNamesPut) {
    sNumNamesPut = 0;
    return;
  }
  sNamesPut[sNumNamesPut].ob = *objnum - 1;
  sNamesPut[sNumNamesPut++].name = f2cString(fname, fsize);
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

  if ((co = checkAssignObject(ob)))
    return co;
  if (*arraySize <= sObj->contsize)
    return(FWRAP_ERROR_FILE_TO_BIG);
  if (imodContourMakeZTables(sObj, 1, 0, &contz, &zlist, &numatz, &contatz, 
                             &zmin, &zmax, &zlsize, &nummax))
    return(FWRAP_ERROR_MEMORY);

  /* Allocate lists of mins, max's, and scan contours */
  pmin = B3DMALLOC(Ipoint, sObj->contsize);
  pmax = B3DMALLOC(Ipoint, sObj->contsize);
  scancont = B3DMALLOC(Icont *, sObj->contsize); 
  nestind = B3DMALLOC(int, sObj->contsize);
  if (!pmin || !pmax || !scancont || !nestind)
    return(FWRAP_ERROR_MEMORY);

  /* Get mins, maxes, set addresses into scan contour list */
  for (co = 0; co < sObj->contsize; co++) {
    level[co] = 0;
    nestind[co] = -1;
    if (sObj->cont[co].psize) {
      imodContourGetBBox(&(sObj->cont[co]), &(pmin[co]), &(pmax[co]));
      scancont[co] = &(sObj->cont[co]);
    }
  }

  /* Loop on contours to find inside and oiutside pairs */
  for (indz = 0; indz < zmax + 1 - zmin; indz++) {
    for (kis = 0; kis < numatz[indz] - 1; kis++) {
      co = contatz[indz][kis];
      if (!sObj->cont[co].psize)
        continue;
      for (lis = kis + 1; lis < numatz[indz]; lis++) {
        eco = contatz[indz][lis];
        if (!sObj->cont[eco].psize)
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
  for (co = 0; co < sObj->contsize; co++) {
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
  for (co = 0; co < sObj->contsize; co++)
    if (sObj->cont[co].psize && (scancont[co]->flags & ICONT_SCANLINE))
      imodContourDelete(scancont[co]);
  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
  free(nestind);
  free(scancont);
  free(pmin);
  free(pmax);

  return FWRAP_NOERROR;
}
