/*****************************************************************************
 *                                                                           *
 *   FILE: imodel_fwrap.c                                                    *
 *                                                                           *
 *   PURPOSE: Load an IMOD model file from Wimp fortran code.                *
 *            Define F77FUNCAP, F77STRING to make for VMS.                   *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0                                 *
 *                                       *
 *       James Kremer  kremer@boulder.colorado.edu               *
 *****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine         *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
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
#include <stdio.h>
#include <math.h>
#include "imodel.h"

#define FWRAP_MAX_OBJECT 100000
#define FWRAP_MAX_POINTS 5000000
#define FWRAP_MAX_CLIP_PLANES 100

#define FWRAP_NOERROR              0
#define FWRAP_ERROR_BAD_FILENAME  -1
#define FWRAP_ERROR_FILE_NOT_IMOD -2
#define FWRAP_ERROR_FILE_TO_BIG   -3
#define FWRAP_ERROR_NO_OBJECTS    -4
#define FWRAP_ERROR_NO_MODEL      -5
#define FWRAP_ERROR_BAD_OBJNUM    -6
#define FWRAP_ERROR_MEMORY        -7

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
#define getimodtimes  GETIMODTIMES
#define getimodsurfaces  GETIMODSURFACES
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
#define getimodtimes  getimodtimes_
#define getimodsurfaces  getimodsurfaces_
#endif

typedef struct
{
  unsigned short length;
  char dum1;
  char dum2;
  char *string;
} F77String;

static Imod *Fimod = NULL;

static int nflags_put = 0;
static int *flags_put = NULL;
static int maxes_put = 0;
static int xmax_put, ymax_put, zmax_put;

#define SCAT_SIZE_FLAG  (1 << 2)
#define SYMBOL_SIZE_FLAG (1 << 3)
#define SYMBOL_TYPE_FLAG (1 << 4)
#define FLAG_VALUE_SHIFT 8

static char *fstrtoc(char *fstr, int flen)
{
  int i;
  char *cstr = (char *)malloc(flen + 2);
  cstr[flen] = 0x00;
  for (i = 0; i < flen; i++){
    cstr[i] = fstr[i];
    if (cstr[i] == ' '){
      cstr[i] = 0x00;
      break;
    }
  }
  return(cstr);
}

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
}


/* Read imod model data from the file given by fname.
 * Sets up a global imod model for further actions.
 * 
 */
int getimod (int ibase[],     /* index into coord array */
             int npt[],       /* contour sizes */
             float coord[][3],   /* coords */
             int   color[][2],   /* colors 255..., 1 */
             int   *npoint,   /* total number of points. */
             int   *nobject,  /* total number of objects. */
#ifdef F77STRING
             F77String *f77str
#else
             char  *fname, 
             int   fsize
#endif
             )
{
#ifdef F77STRING
  int fsize = f77str->length;
  char *fname = f77str->string;
#endif
  
  struct Mod_Model   *model;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  FILE *fin;
  char *cfilename;
  int i;
  int ob, co, pt;
  int ncontour = 0;
  int npoints = 0;
  int coord_index = 0;
  int ibase_val = 0;
  int coi = 0;

#ifdef FWRAP_DEBUG
  FILE *errfp;
  errfp = fopen("imod.err", "w");
  fprintf(errfp, "fsize = %d\n", fsize);
#endif     
  model = imodNew();
  if (!model)
    return(-1);

  cfilename = (char *)malloc(fsize + 2);
  cfilename[fsize] = 0x00;
  for (i = 0; i < fsize; i++){
    cfilename[i] = fname[i];
    if (cfilename[i] == ' '){
      cfilename[i] = 0x00;
      break;
    }
  }

  fin = fopen(cfilename, "rb");

#ifdef FWRAP_DEBUG
  fprintf(errfp, "filename = %s\n", cfilename);
#endif

  free(cfilename);

  if (!fin){
    return(FWRAP_ERROR_BAD_FILENAME);
  }

  model->file = fin;
  if (imodReadFile( model)){
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
  {
    IrefImage *iref = Fimod->refImage;
    Imat  *mat;
    Ipoint pnt;

    if (Fimod->flags & IMODF_FLIPYZ){
      imodFlipYZ(Fimod);
    }
    if (iref){

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


     

  }

  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    ncontour += obj->contsize;
    for(co = 0; co < obj->contsize; co++)
      npoints += obj->cont[co].psize;
  }

  if (ncontour > FWRAP_MAX_OBJECT) {
    fprintf(stderr, "getimod: Too many contours in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  if (npoints > FWRAP_MAX_POINTS) {
    fprintf(stderr, "getimod: Too many points in model for Fortran program\n");
    return(FWRAP_ERROR_FILE_TO_BIG);
  }

  *npoint  = npoints;
  *nobject = ncontour; 

  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    for(co = 0; co < obj->contsize; co++, coi++){
      cont = &(obj->cont[co]);
      ibase[coi] = ibase_val;
      ibase_val += cont->psize;
      npt[coi]   = cont->psize;
      color[coi][0] = 1;
      color[coi][1] = 255 - ob;

      for(pt = 0; pt < cont->psize; pt++, coord_index++){
        coord[coord_index][0] = cont->pts[pt].x;
        coord[coord_index][1] = cont->pts[pt].y;
        coord[coord_index][2] = cont->pts[pt].z;
      }
    }
  }
  return(FWRAP_NOERROR);
}

int newimod(void)
{
    deleteFimod();
    Fimod = imodNew();
    Fimod->objsize = 0;
    Fimod->obj = NULL;
    if (Fimod) return FWRAP_NOERROR;
    return FWRAP_ERROR_NO_MODEL;
}

/*
 * Free's all memory allocated by functions
 * in the library.
 */
int deleteimod(void)
{
    if (!Fimod)
    return FWRAP_ERROR_NO_MODEL;
    deleteFimod();
    return FWRAP_NOERROR;
}

/*
 * Remove all objects from model.
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

/* openimoddata: Read in an imod file into static global for further use. */
int openimoddata(
#ifdef F77STRING
             F77String *f77str)
{
     int fsize = f77str->length;
     char *fname = f77str->string;
#else
             char  *fname,
             int   fsize)
{
#endif
  char *filename = fstrtoc(fname, fsize);
  if (!filename) return(FWRAP_ERROR_BAD_FILENAME);
  
  deleteFimod();
  
  Fimod = imodRead(filename);
  free(filename);
  if (Fimod)
    return FWRAP_NOERROR;
  else
    return(FWRAP_ERROR_NO_MODEL);
}

/* same as getimod but only fill scat objects. */
/* other objects set to 0 length. */
/* combine all contours of each scat objects into one contour. */
/*getimodscat*/
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

  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    if (!iobjScat(obj->flags))
      continue;
    maxobj = ob+1;
    for(co = 0; co < obj->contsize; co++)
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

  for(ob = 0; ob < model->objsize; ob++, coi++){
    obj = &(model->obj[ob]);

    scont = imodContourNew(); /* Create empty contour. */

    if (iobjScat(obj->flags)){
      for(co = 0; co < obj->contsize; co++){
        cont = &(obj->cont[co]);
        for(pt = 0; pt < cont->psize; pt++)
          imodPointAppend(scont, &cont->pts[pt]);
      }
    }

    ibase[coi] = coord_index;
    npt[coi]   = scont->psize;

    color[coi][0] = 1;
    color[coi][1] = 255 - ob;
      
    for(pt = 0; pt < scont->psize; pt++, coord_index++){
      coord[coord_index][0] = scont->pts[pt].x;
      coord[coord_index][1] = scont->pts[pt].y;
      coord[coord_index][2] = scont->pts[pt].z;
    }
    imodContourDelete(scont);
  }
  return FWRAP_NOERROR;
}


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

int getimodmaxes(int *xmax, int *ymax, int *zmax)
{
  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  *xmax = Fimod->xmax;
  *ymax = Fimod->ymax;
  *zmax = Fimod->zmax;
  return FWRAP_NOERROR;
}

/* DNM 6/8/01: change this to save the values until a model is written */
int putimodmaxes(int *xmax, int *ymax, int *zmax)
{
  xmax_put = *xmax;
  ymax_put = *ymax;
  zmax_put = *zmax;
  maxes_put = 1;
  return FWRAP_NOERROR;
}

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
  if (!iref){
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


  if (Fimod->flags & IMODF_FLIPYZ){
    *ifflip = 1;
  }else{
    *ifflip = 0;
  }
  return FWRAP_NOERROR;
}

/* Return the image file scale values used to get from model to index coords */
int getimodscales(float *ximscale, float *yimscale, float *zimscale)
{
  IrefImage *iref;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  iref = Fimod->refImage;
     
  if (iref){
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

/* return a flag for each object; limflags is maximum size of array */

int getimodflags(int *flags, int *limflags)
{
  Iobj *obj;
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


/* return number of clipping planes for the given object #. 
 */
int getimodclip(int *objnum, float *clip)
{
  Iobj *obj;
  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  if ((*objnum > Fimod->objsize) || (*objnum < 1))
    return(FWRAP_ERROR_BAD_OBJNUM);
     
  Fimod->cindex.object = *objnum - 1;
  obj = &(Fimod->obj[Fimod->cindex.object]);

  if (!obj->clip)
    return(0);

  clip[0] = obj->clip_normal.x;
  clip[1] = obj->clip_normal.y;
  clip[2] = obj->clip_normal.z / Fimod->zscale;
  clip[3] = (obj->clip_normal.x * obj->clip_point.x) +
    (obj->clip_normal.y * obj->clip_point.y) +
    (obj->clip_normal.z * obj->clip_point.z);
  clip[3] *= Fimod->pixsize;
  return(1);
     
  /* don't send more then this number of clipping planes. */
  /*     FWRAP_MAX_CLIP_PLANES; */

}

/* fill in mesh data from the given object number. */
/* limverts should be maximum number of vertices and normals, limindex
   should be maximum number of indices */
int  getimodmesh (int *objnum, float *verts, int *index, int *limverts, 
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
      for (i = 0; i < mesh[m].vsize; i++){
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


/* Get only the vertices from the mesh data of the given object number. */
/* limverts should be maximum number of vertices and limindex
   should be maximum number of indices */
int  getimodverts (int *objnum, float *verts, int *index, int *limverts, 
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
    for(i = 0; i < mi; i += 2){
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

/* Return point sizes for the given object; limsizes should be size of array */
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
  for (co = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    if (*nsizes + cont->psize > *limsizes) {
      fprintf(stderr, "getimodsizes: Model too large for Fortran program\n");
      return(FWRAP_ERROR_FILE_TO_BIG);
    }
    for(pt = 0; pt < cont->psize; pt++)
      *sizes++ = imodPointGetSize(obj, cont, pt);
    *nsizes += cont->psize;
  }
     
  return FWRAP_NOERROR;
}          

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

  for(co = 0, v = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    for(pt = 0; pt < cont->psize; pt++){
      cont->pts[pt].x = verts[v++];
      cont->pts[pt].y = verts[v++];
      cont->pts[pt].z = verts[v++];
    }
  }
  return(0);
}

int putimodmesh(float *verts)
{
  int i,mi;
  Iobj *obj = &(Fimod->obj[Fimod->cindex.object]);
  float *fvert;

  obj->mesh->flag |= IMESH_FLAG_NMAG;
  mi = obj->mesh->vsize * 3;
  fvert = (float *) obj->mesh->vert;

  for(i = 0; i < mi; i++){
    fvert[i] = verts[i];
  }
  return FWRAP_NOERROR;
}

/* writeimod */
/*F77String*/
int writeimod(
#ifdef F77STRING
              F77String *f77str)
{
  int fsize = f77str->length;
  char *fname = f77str->string;
#else
  char  *fname,
    int   fsize)
{
#endif
  int i, ob, flag, value;
  int retcode = 0;
  Iobjview *objview;
  char *filename = fstrtoc(fname, fsize);
  if (!filename) return(FWRAP_ERROR_BAD_FILENAME);
  if (!Fimod) { free(filename); return(FWRAP_ERROR_NO_MODEL);}

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
        break;
      }
    }
  }
       
  if (maxes_put) {
    Fimod->xmax = xmax_put;
    Fimod->ymax = ymax_put;
    Fimod->zmax = zmax_put;
  }
     
  /* Complete the set of object views just before writing */
  imodObjviewComplete(Fimod);
  Fimod->file = fopen(filename, "wb");
  retcode = imodWriteFile(Fimod);
  free(filename);
  return(retcode);
}


/* putimod 
 * similar to getimod, but put a wimp model to an imod file 
 * uses similar contructs to the wmod2imod program.
 */
#define MAXOBJ      256
#define OBJ_EMPTY    -2
#define OBJ_HAS_DATA -1

static float Wmod_Colors[9][3]  =   { 0.90, 0.82, 0.37,  /* Dim Yellow  */
                                      0.54, 0.51, 0.01,  /* Olive Brown */
                                      0.94, 0.49, 0.0,   /* Orange      */
                                      1.00, 0.0,  0.0,   /* Red         */
                                      0.0,  1.0,  0.0,   /* Green       */
                                      0.0,  0.0,  1.0,   /* Blue        */
                                      1.0,  1.0,  0.0,   /* Yellow      */
                                      1.0,  0.0,  1.0,   /* Magenta     */
                                      0.0,  1.0,  1.0    /* Cyan        */
};

int putimod (int ibase[],     /* index into coord array */
             int npt[],       /* contour sizes */
             float coord[][3],   /* coords */
             int   cindex[],
             int   color[][2],   /* colors 255..., 1 */
             int   *npoint,   /* total number of points. */
             int   *nobject)  /* total number of objects. */
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

  if (!Fimod){
    Fimod = imodNew();
    Fimod->objsize = 0;
    Fimod->obj = NULL;
  }

  mincolor = 256 - Fimod->objsize;

  /* Find minimum color, and maximum object #; get arrays */
  for(object = 0; object < *nobject; object++){
    if (npt[object] == 0) continue;
    if (mincolor > color[object][1])
      mincolor = color[object][1];
  }
  maxobj = 256 - mincolor;

  objlookup = (int *)malloc(maxobj * sizeof(int));
  nsaved = (int *)malloc(maxobj * sizeof(int));

  /*
   * Mark obj lookup array to empty=-2 or used=-1.
   * or index >= 0.
   */
  for(ob = 0; ob < maxobj; ob++) {
    objlookup[ob] = OBJ_EMPTY;
    nsaved[ob] = 0;
  }
  for(ob = 0; ob < Fimod->objsize; ob++){
    objlookup[(255 - mincolor) - ob] = ob;
    nobj++;
  }
  for(object = 0; object < *nobject; object++){
    if (npt[object] == 0) continue;
    if (objlookup[color[object][1] - mincolor] == OBJ_EMPTY)
      objlookup[color[object][1] - mincolor] = OBJ_HAS_DATA;
  }

  /*
   *  Remove old object data. NOT ANYMORE
     
   for(ob = 0; ob < Fimod->objsize; ob++){
   imodContoursDelete(Fimod->obj[ob].cont, Fimod->obj[ob].contsize);
   Fimod->obj[ob].cont = NULL;
   Fimod->obj[ob].contsize = 0;
   }   
  */
  /*
   * Create additional imod objects if needed.
   */
  for(ob = maxobj - 1; ob >= 0; ob--){
    if (objlookup[ob] != OBJ_HAS_DATA)
      continue;
    objlookup[ob] = Fimod->objsize;
    imodNewObject(Fimod);
    wimpno = ob + mincolor;

    /* Just use top 6 colors and let new color scheme hold after that */
    if (wimpno >= 250){
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
  for(object = 0; object < *nobject; object++){
     
    /* Don't even look up if empty and out of range */
    if (npt[object] == 0 && (color[object][1] < mincolor ||
                             color[object][1] > 255))
      continue;
    ob  = objlookup[color[object][1] - mincolor];
    if (ob < 0){
      if (npt[object] != 0)
        fprintf(stderr, "putimod warning: bad object bounds %d\n",
                ob);
      continue;
    }
    if (ob > Fimod->objsize){
      fprintf(stderr, "putimod warning: bad object bounds %d\n", ob);
      continue;
    }
    obj = &Fimod->obj[ob];

    if (nsaved[ob] < obj->contsize) {
      cont = &(obj->cont[nsaved[ob]]);
      /* Possible criterion was to require that number of points match,
         but this lost surface numbers with point reduction */
      if (npt[object] != cont->psize) {
        if (!cont->psize)
          cont->pts = (Ipoint *)
            malloc(sizeof(Ipoint) * npt[object]);
        else {
          cont->pts = (Ipoint *)
            realloc(cont->pts, sizeof(Ipoint) * npt[object]);
          if (cont->sizes)
            cont->sizes = (float *) 
              realloc(cont->sizes,
                      sizeof(float) * npt[object]);
        }
      }
      cont->psize = npt[object];
      for(pt = 0; pt < cont->psize; pt++){
        ci = cindex[ibase[object] + pt] - 1;
        cont->pts[pt].x = coord[ci][0]; 
        cont->pts[pt].y = coord[ci][1]; 
        cont->pts[pt].z = coord[ci][2]; 
      }
    } else {

      /* if # saved = # of contours, add a new contour */
      cont = imodContourNew();
      if (!cont)
        return FWRAP_ERROR_MEMORY;
      cont->psize = npt[object];
      cont->pts = (Ipoint *)malloc(sizeof(Ipoint) * cont->psize);
      if (!cont->pts) 
        return FWRAP_ERROR_MEMORY;

      for(pt = 0; pt < cont->psize; pt++){
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
  for(ob = 0; ob < Fimod->objsize; ob++){
    if (imodContoursDeleteToEnd(&(Fimod->obj[ob]), nsaved[ob]))
      return FWRAP_ERROR_MEMORY;
  }   

  /*
   *  Translate coordinates to match reference image.
   *  DNM 11/5/98: rearranged to correspond to proper conventions 
   */
  if (Fimod->refImage){
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

  return FWRAP_NOERROR;
}

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

/* DNM 12/3/01: added so that old wimp models can be converted */
void fromvmsfloats(unsigned char *data, int *amt)
{
  imodFromVmsFloats(data, *amt);
}

/* DNM 5/15/02: put scattered point sizes out in flags with an offset */
void putscatsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) | SCAT_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

/* DNM 6/7/03: Add ability to set symbol type and size */
void putsymtype(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) | SYMBOL_TYPE_FLAG;
  putimodflag(objnum, &flag);
}

void putsymsize(int *objnum, int *size)
{
  int flag = (*size << FLAG_VALUE_SHIFT) | SYMBOL_SIZE_FLAG;
  putimodflag(objnum, &flag);
}

int getimodtimes(int *times)
{
  int ob, co;
  Iobj *obj;
  int coi = 0;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  for(ob = 0; ob < Fimod->objsize; ob++){
    obj = &(Fimod->obj[ob]);
    for(co = 0; co < obj->contsize; co++, coi++)
      times[coi] = obj->cont[co].type;
  }

  return FWRAP_NOERROR;
}

int getimodsurfaces(int *surfs)
{
  int ob, co;
  Iobj *obj;
  int coi = 0;

  if (!Fimod)
    return(FWRAP_ERROR_NO_MODEL);

  for(ob = 0; ob < Fimod->objsize; ob++){
    obj = &(Fimod->obj[ob]);
    for(co = 0; co < obj->contsize; co++, coi++)
      surfs[coi] = obj->cont[co].surf;
  }

  return FWRAP_NOERROR;
}
