/*
 *  imodel_files.c -- Open, Read and Write Imod files.
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
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"
#include "istore.h"
#include "objgroup.h"

#ifdef __vms
#define IMOD_DATA_SWAP
#define IMOD_FLOAT_CONVERT
#endif
#ifdef B3D_LITTLE_ENDIAN
#define IMOD_DATA_SWAP
#endif

#define MAXLINE 81
#define IMODEL_FILES_VERSION_02
#define IMODEL_FILES_VERSION_12
#define IMODEL_FILES_VERSION 12   /* imod 1.2 */

/* #define IMODEL_FILES_DEBUG */

static int imodel_read_v01(Imod *mod, FILE *fin);
static int imodel_read(Imod *imod, int version);
static int imodel_write(Imod *mod, FILE *fout);
static int imodel_write_object(Iobj *obj, FILE *fout, Ipoint *scale);
static int imodel_write_contour(Icont *cont, FILE *fout, Ipoint *scale);
static int imodel_write_mesh(Imesh *mesh, FILE *fout, Ipoint *scale);
static int imodel_read_header(Imod *imod, FILE *fin);
static int imodel_read_object(Iobj *obj, FILE *fin);
static int imodel_read_object_v01(Iobj *obj, FILE *fin);
static int imodel_read_contour(Icont *cont, FILE *fin);
static int imodel_read_contour_v01(Icont *cont, FILE *fin);
static int imodel_read_mesh(Imesh *mesh, FILE *fin);
static int imodel_read_clip(Iobj *obj, FILE *fin, b3dUInt32 flags);
static int imodel_read_imat(Iobj *obj, FILE *fin, b3dUInt32 flags);
static int imodel_read_ptsizes(Icont *cont, FILE *fin);
static int imodel_read_meshparm(Iobj *obj, FILE *fin);
static int imodel_read_meshskip(Iobj *obj, FILE *fin);
static int imodel_read_sliceang(Imod *imod, FILE *fin);
static void writeAsciiClips(Imod *imod, IclipPlanes *clips, char *prefix);
static void readAsciiClips(Imod *imod, char *line, IclipPlanes *clips, char *prefix);

#ifdef IMOD_DATA_SWAP
static void byteswap(void *ptr, unsigned size);
#endif

#ifdef IMOD_FLOAT_CONVERT
static void tovmsfloat(unsigned char *ptr);
#endif

/*!
 * Reads a model from file given by [filename] and returns created model 
 * structure or NULL for error.  (Unused, calls @imodRead).
 */
Imod *imodFileRead(const char *filename)
{
  return(imodRead(filename));
}

/*!
 * Writes the model [imod] to the file given by [filename].  Returns -1 for
 * error, 0 for success.  (Unused 4/23/05).
 */
int  imodFileWrite(Imod *imod, const char *filename)
{
  FILE *fout = fopen(filename, "wb");
  return(imodWrite(imod, fout));
}

/*!
 * Opens the file given by [filename] with the given [mode], sets the file 
 * pointer into imod->file and the filename into imod->filename.  Returns -1
 * for error or 0 for success.
 */
int imodOpenFile(const char *filename, const char *mode, Imod *imod)
{
  FILE *fp;
  int len;

  fp = fopen(filename, mode);
  if (!fp)
    return(-1);
  imod->file = fp;
  len = strlen(filename)+1;

  imod->fileName = malloc(len);
  if (imod->fileName)
    memcpy(imod->fileName, filename, len);
     
  return(0);
}

/*!
 * Closes the file for model [imod], using file pointer in imod->file.  Returns
 * -1 for error.
 */
int imodCloseFile(Imod *imod)
{
  if (!imod)
    return(-1);
  if (!imod->file)
    return(-1);
  fclose(imod->file);
  return(0);
}

/*!
 * Reads model from file pointer in imod->file and places model in [imod].
 * The model should be set to default settings before calling, either by 
 * allocating with imodNew or by calling imodDefault.
 * Returns -1 for error.
 */
int imodReadFile(Imod *imod)
{
  FILE *fp;
  int error = 0;
  int id;

  if (!imod)
    return(-1);

  if (!imod->file)
    return(-1);
  fp = imod->file;

  imod->file = fp;
  rewind(imod->file);

  id = imodGetInt(imod->file);

  if (id != ID_IMOD){
    rewind(imod->file);
    error = imodReadAscii(imod);
#ifdef IMODEL_FILES_DEBUG
    if (error){
      b3dError(stderr, "imodReadFile: imodReadAscii error %d.\n",
		       error);
    }
#endif
    return(error);
  }

  id = imodGetInt(imod->file);
  if ((error = imodel_read(imod, id)))
    return(error);
  return(0);
}

/*!
 * Reads a model from file given by [filename] and returns created model 
 * structure or NULL for error.
 */
Imod *imodRead(const char *filename)
{
  FILE *fin;
  Imod *imod;
  int error;

  imod = imodNew();
  if (!imod)
    return(NULL);

  fin = fopen(filename, "rb");
  if (!fin){
    imodDelete(imod);
    return(NULL);
  }
  imod->file = fin;
     
  error = imodReadFile(imod);
  if (error){
#ifdef IMODEL_FILES_DEBUG
    b3dError(stderr, "imodel_files.c error %d\n", error);
#endif
    imodDelete(imod);
    fclose(fin);
    return(NULL);
  }
  imod->file = NULL;
  fclose(fin);

  error = strlen(filename)+1;
  imod->fileName = malloc(error);
  if (imod->fileName)
    memcpy(imod->fileName, filename, error);
  return(imod);
}

/*!
 * Writes the model [imod] to the file pointer in imod->file.  Returns -1 for
 * error, 0 for success.
 */
int imodWriteFile(Imod *imod)
{
  if (!imod)
    return(-1);
  if (!imod->file)
    return(-1);
  return(imodel_write(imod, imod->file));
}

/*!
 * Writes the model [imod] to the file pointer in [fout].  Returns -1 for
 * error, 0 for success.
 */
int imodWrite(Imod *imod, FILE *fout)
{
  int error;
  FILE *temp;
  temp = imod->file;
  error = imodel_write(imod, fout);
  imod->file = temp;
  if (error)
    return(-1);
  else
    return(0);
}



/*****************************************************************************/
/* internal functions                                                        */
/*****************************************************************************/

static int imodel_write(Imod *mod, FILE *fout)
{
  int i, error;
  unsigned int id;
  Ipoint scale;
  SlicerAngles *slan;

  rewind(fout);
     
  /* DNM 9/4/02: set flag that mat1 and mat3 are written as bytes */
  mod->flags |= IMODF_MAT1_IS_BYTES | IMODF_MULTIPLE_CLIP;

  scale.x = mod->xybin;
  scale.y = mod->xybin;
  scale.z = mod->zbin;
  if (mod->flags & IMODF_FLIPYZ) {
    scale.z = mod->xybin;
    scale.y = mod->zbin;
  }

  id = ID_IMOD;
  imodPutInt(fout, &id);
  id = ID_VERSION;
  imodPutInt(fout, &(id)); 
  imodPutBytes(fout, (unsigned char *)mod->name, IMOD_STRSIZE);
  imodPutInts(fout, &mod->xmax, 9);

  imodPutFloats(fout, &mod->xoffset, 6);
  imodPutInts  (fout, &mod->cindex.object, 5);
  imodPutFloat (fout, &mod->pixsize);
  imodPutInt   (fout, &mod->units);
  imodPutInt  (fout, (unsigned int *)&mod->csum);
  imodPutFloats(fout, &mod->alpha, 3);
     
  for(i = 0; i < mod->objsize; i++)
    if ((error = imodel_write_object( &(mod->obj[i]), fout, &scale)))
      return(error);
     
  mod->file = fout;
  imodViewModelWrite(mod, &scale);
  imodIMNXWrite(mod);

  /* Write slicer angles */
  for (i = 0; i < ilistSize(mod->slicerAng); i++) {
    id = ID_SLAN;
    imodPutInt(fout, &id);
    id = SIZE_SLAN;
    imodPutInt(fout, &id);
    slan = (SlicerAngles *)ilistItem(mod->slicerAng, i);
    if (!slan)
      return IMOD_ERROR_FORMAT;
    imodPutInt(fout, &slan->time);
    imodPutFloats(fout, &slan->angles[0], 6);
    imodPutBytes(fout, (unsigned char *)(&slan->label[0]), ANGLE_STRSIZE);
  }
                      
  if ((id = objGroupListWrite(mod->groupList, fout)))
    return(id);

  if ((id = imodWriteStore(mod->store, ID_MOST, fout)))
    return(id);

  id = ID_IEOF;
  imodPutInt(fout,  &id);
     
  fflush(fout);
  return(ferror(fout));
}

static int imodel_write_object(Iobj *obj, FILE *fout,
                               Ipoint *scale)
{
  int i, error;
  int id;
  IclipPlanes *clips;
  Ipoint normScale;

  id = ID_OBJT;
  imodPutInt(fout, &id);
  imodPutBytes(fout, (unsigned char *)obj->name, IOBJ_STRSIZE);
  imodPutInts(fout, &obj->extra, IOBJ_EXSIZE + 4);
  imodPutFloats(fout, &obj->red, 3);
  imodPutInt(fout, &obj->pdrawsize);
  imodPutBytes(fout, &obj->symbol, 8);
  imodPutInt(fout, &obj->meshsize);

  imodPutInt(fout, &obj->surfsize);

  if (obj->label)
    imodLabelWrite(obj->label, ID_OLBL, fout);

  for(i = 0; i < obj->contsize; i++)
    if ((error = imodel_write_contour( &(obj->cont[i]), fout, scale)))
      return(error);

  for(i = 0; i < obj->meshsize; i++)
    if (imodel_write_mesh( &(obj->mesh[i]), fout, scale))
      return(-1);

  /* due to a bug, version 2 readers can't read extra data. 
   * Note version 2 readers were in IMOD 1.2
   */
  if (IMODEL_FILES_VERSION > 2){
    /* DNM 9/19/04: remove test on point and normal, clip no longer 0 if off */
    clips = &obj->clips;
    if (clips->count) {

      id = ID_CLIP;
      imodPutInt(fout, &id);
      id =  SIZE_CLIP + 24 * (clips->count - 1);
      imodPutInt(fout, &id);

      /* For backward compatibility, if there is one clip plane and it is
         off, set clip to 0 */
      /* 5/28/11: eliminate the backwards compatibility to IMOD 2.4! */
      /* clipOut = clips->count;
         if (clipOut == 1 && (clips->flags & 1) == 0)
         clipOut = 0; */
      imodPutBytes(fout, &clips->count, 1);
      imodPutBytes(fout, &clips->flags, 3);
      normScale.x = 1. / scale->x;
      normScale.y = 1. / scale->y;
      normScale.z = 1. / scale->z;
      imodPutScaledPoints(fout, &clips->normal[0], clips->count, 
                          &normScale);
      imodPutScaledPoints(fout, &clips->point[0], clips->count,
                          scale);
    }
	  
    /* Material data. */
    id = ID_IMAT;
    imodPutInt(fout, &id);
    id =  SIZE_IMAT;
    imodPutInt(fout, &id);
    imodPutBytes(fout, &obj->ambient, 4);

    /* DNM 9/4/02: write mat1 and mat3 as bytes */
    imodPutBytes(fout, &obj->fillred, 4);
    imodPutInts(fout, &obj->mat2, 1);
    imodPutBytes(fout, &obj->valblack, 4);

    /* Meshing parameters */
    if (obj->meshParam) {
      id = ID_MEPA;
      imodPutInt(fout, &id);
      id =  SIZE_MEPA;
      imodPutInt(fout, &id);
      imodPutInts(fout, &obj->meshParam->flags, 9);
      imodPutFloats(fout, &obj->meshParam->overlap, 10);
      
      if (obj->meshParam->capSkipNz) {
        id = ID_SKLI;
        imodPutInt(fout, &id);
        id =  4 * obj->meshParam->capSkipNz;
        imodPutInt(fout, &id);
        imodPutInts(fout, obj->meshParam->capSkipZlist, 
                    obj->meshParam->capSkipNz);
      }
    } 
  }
  if ((id = imodWriteStore(obj->store, ID_OBST, fout)))
    return(id);
  if ((error = ferror(fout)))
    return(IMOD_ERROR_WRITE);
  return(0);
}

static int imodel_write_contour(Icont *cont, FILE *fout, Ipoint *scale)
{
  int id;
     
  id = ID_CONT;
  imodPutInt(fout, &id);
  imodPutInts(fout, &cont->psize, 4);
  if (scale->x == 1.0 && scale->y == 1.0 && scale->z == 1.0)
    imodPutFloats(fout, (float *)cont->pts, cont->psize * 3);
  else {
    /* fprintf(stderr, "scale %f %f %f\n", scale->x, scale->y, scale->z); */
    imodPutScaledPoints(fout, cont->pts, cont->psize, scale);
  }
     
  if (cont->label)
    imodLabelWrite(cont->label, ID_LABL, fout);

  if (cont->sizes) {
    id = ID_SIZE;
    imodPutInt(fout, &id);
    id = cont->psize * sizeof(float);
    imodPutInt(fout, &id);
    imodPutFloats(fout, cont->sizes, cont->psize);
  }

  if ((id = imodWriteStore(cont->store, ID_COST, fout)))
    return(id);

  return(ferror(fout));
}

static int imodel_write_mesh(Imesh *mesh, FILE *fout, Ipoint *scale)
{
  int id = ID_MESH;
  int i;

  if (!mesh) return(0);
  imodPutInt(fout, &id);
  imodPutInts(fout, &mesh->vsize, 3);

  /* DNM 6/20/01: changed to put out type and pad as two shorts */
  /* 9/8/06: made these 16-bit in the structure and renamed */
  imodPutShort(fout, &mesh->time);
  imodPutShort(fout, &mesh->surf);
  if (scale->x == 1.0 && scale->y == 1.0 && scale->z == 1.0) {
    imodPutFloats(fout, (float *)mesh->vert, mesh->vsize * 3);
  } else {
    for (i = 0; i < mesh->vsize; i += 2) {
      imodPutScaledPoints(fout, &mesh->vert[i], 1, scale);
      imodPutFloats(fout, (float *)(&mesh->vert[i + 1]), 3);
    }
  }
  imodPutInts(fout, mesh->list, mesh->lsize);
  if ((id = imodWriteStore(mesh->store, ID_MEST, fout)))
    return(id);

  return(ferror(fout));;
}

/*****************************************************************************/
/* Load Functions                                                            */
/*****************************************************************************/
static int imodel_read_v01(Imod *mod, FILE *fin)
{

  int i;
  int id;
     
  rewind(fin);
  id = imodGetInt(fin);
  if (id != ID_IMOD){
    b3dError(stderr, "Read Imod: Not an imod file.\n");
    return(-1);
  }
  id = imodGetInt(fin);
  if (id != MakeID('V', '0', '.', '1')){
    b3dError(stderr, "Read Imod: Imod file version unknown.\n");
    return(-1);
  }

  imodel_read_header(mod, fin);

  mod->obj = imodObjectsNew(mod->objsize);

  for (i = 0; i < mod->objsize; i++){
    if (imodel_read_object_v01( &(mod->obj[i]), fin))
      return(-1);
  }
     
  return(0);
}
     
static int imodel_read(Imod *imod, int version)
{
  FILE  *fin;
  Iobj  *obj;
  Icont *cont = NULL;
  Imesh *mesh = NULL;
  int error;
  int ieof = FALSE;
  int id;
#ifdef IMODEL_FILES_DEBUG
  char badid[8];
  int *badp = (int *)badid;
#endif

  if (version == IMOD_01){
    error = imodel_read_v01(imod, imod->file);
    if (error) return(error);
    imodCleanSurf(imod);
    return(error);
  }
	  
#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "enter: imodel_read\n");
#endif

  if (!imod)
    return(-1);
  if (!imod->file)
    return(-1);

#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "imodel_read_v02: input ok.\n");
#endif

  fin = imod->file;
  rewind(fin);
  clearerr(fin);

  id = imodGetInt(fin);
  if (id != ID_IMOD)
    return(IMOD_ERROR_FORMAT);
#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "imodel_read_v02: format ok.\n");
#endif     
     
  id = imodGetInt(fin);
     
  if (ferror(fin)){
#ifdef IMODEL_FILES_DEBUG
    perror("error before reading header:");
#endif
    return(IMOD_ERROR_READ);
  }

  if ((error = imodel_read_header(imod, fin))) {
#ifdef IMODEL_FILES_DEBUG
    fprintf(stderr, "error %d reading header\n", error);
#endif
    return(error);
  }

  if (imod->objsize){
    imod->obj = imodObjectsNew(imod->objsize);
    if (!imod->obj)
      return(IMOD_ERROR_MEMORY);
  }else{
    imod->obj = NULL;
  }

  obj = imod->obj;
  obj--;

#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "imodel_read_v02: header ok.\n");
#endif

  while((!feof(fin)) && (!ieof)){

    id = imodGetInt(fin);

    switch(id){
    case ID_OBJT:
      obj++;
      if ((error = imodel_read_object(obj, imod->file))) {
        return(error);
      }
      if (obj->contsize){
        obj->cont = imodContoursNew(obj->contsize);
        if (!obj->cont)
          return(IMOD_ERROR_MEMORY);
      }else{
        obj->cont = NULL;
      }  
      cont = obj->cont;
      cont--;
      if (obj->meshsize){
        obj->mesh = imodMeshesNew(obj->meshsize);
        if (!obj->mesh)
          return(IMOD_ERROR_MEMORY);
      }
      mesh = obj->mesh;
      mesh--;
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read: obj ok %x c = %d, m = %d.\n",
              id, obj->contsize, obj->meshsize);
#endif
      break;
	       
    case ID_OLBL:
      obj->label = imodLabelRead(imod->file, &error);
      if (!obj->label)
        return(error);
      break;

    case ID_CONT:
      cont++;
      if ((error = imodel_read_contour(cont, imod->file)))
        return(error);
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read: cont ok. %x\n", id);
#endif
      break;

    case ID_LABL:
      cont->label = imodLabelRead(imod->file, &error);
      if (!cont->label)
        return(error);
      break;

    case ID_SIZE:
      if ((error = imodel_read_ptsizes(cont, imod->file)))
        return(error);
      break;

    case ID_MESH:
      mesh++;
      if ((error = imodel_read_mesh(mesh, imod->file))) {
#ifdef IMODEL_FILES_DEBUG
        fprintf(stderr, "imodel_read: mesh error %d.\n", error);
#endif
        return(error);
      }
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read: mesh ok.\n");
#endif
      break;

    case ID_IEOF:
      fflush(fin);
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read_v02: exit chunk %x.\n", id);
#endif
      ieof = TRUE;
      break;

    case ID_CLIP:
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read_v02: clip %x.\n", id);
#endif
      imodel_read_clip(obj, imod->file, imod->flags); 
      break; 

      /* DNM 9/4/02: pass flags so mat1 & mat3 can be read two ways */
    case ID_IMAT:
      imodel_read_imat(obj, imod->file, imod->flags); 
      break; 

    case ID_VIEW:
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read_v02: view %x.\n", id);
#endif
	       
      imodViewModelRead(imod);
      break;

    case ID_MCLP:
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read_v02: mclp %x.\n", id);
#endif
      imodViewClipRead(imod);
      break;

    case ID_IMNX: /* image nat. transform */
      imodIMNXRead(imod);
      break;

    case ID_MOST: /* Model general storage */
      imod->store = imodReadStore(fin, &error);
      if (error)
        return(error);
      break;

    case ID_OBST: /* Object general storage */
      obj->store = imodReadStore(fin, &error);
      if (error)
        return(error);
      break;

    case ID_COST: /* Contour general storage */
      cont->store = imodReadStore(fin, &error);
      if (error)
        return(error);
      break;

    case ID_MEST: /* Mesh general storage */
      mesh->store = imodReadStore(fin, &error);
      if (error)
        return(error);
      break;

    case ID_MEPA:
      if ((error = imodel_read_meshparm(obj, fin)))
        return(error);
      break;

    case ID_SKLI:
      if ((error = imodel_read_meshskip(obj, fin)))
        return(error);
      break;

    case ID_SLAN:
      if ((error = imodel_read_sliceang(imod, fin)))
        return(error);
      break;

    case ID_OGRP:
      if ((error = objGroupRead(&imod->groupList, fin)))
        return(error);
      break;

    default:
#ifdef IMODEL_FILES_DEBUG
      *badp = id;
      badid[4] = 0;
      fprintf(stderr, "imodel_read: unknown data. %x, %f\n%s\n", 
              id, (float)id,badid);
#endif
      id = imodGetInt(fin);
      if (fseek(fin, id, SEEK_CUR)){
        return(IMOD_ERROR_READ);
      }
      break;
    }
  }

#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "imodel_read_v02: model ok.\n");
#endif

  fflush(fin);
  if (version < IMOD_V12)
    imodCleanSurf(imod);

  /* Make sure the current index is valid */
  if (imod->objsize > 0)
    B3DCLAMP(imod->cindex.object, 0, imod->objsize - 1);
  else
    imod->cindex.object = -1;
  if (imod->cindex.object < 0)
    imod->cindex.contour = imod->cindex.point = -1;
  else
    B3DCLAMP(imod->cindex.contour, -1, imod->obj[imod->cindex.object].contsize - 1);
  if (imod->cindex.contour < 0)
    imod->cindex.point = -1;
  else
    B3DCLAMP(imod->cindex.point, -1, 
             imod->obj[imod->cindex.object].cont[imod->cindex.contour].psize - 1);
  return(0);
}


static int imodel_read_header(Imod *imod, FILE *fin)
{
  imodGetBytes(fin,  (unsigned char *)imod->name, IMOD_STRSIZE); 
  imodGetInts(fin,  &imod->xmax, 9);
  imodGetFloats(fin, &imod->xoffset, 3);

  imodGetFloats(fin, &imod->xscale,  3);
  imodGetInts(fin,  &imod->cindex.object, 5);
  imod->pixsize = imodGetFloat(fin);
  imod->units   = imodGetInt(fin);
  imod->csum    = imodGetInt(fin);
  imodGetFloats(fin, &imod->alpha, 3);

  if (ferror(fin)){
    imod->objsize = 0;
#ifdef IMODEL_FILES_DEBUG
    perror("error in imodel_read_header: ");
#endif
    return(IMOD_ERROR_READ);
  }else{
    return(0);
  }
}


static int imodel_read_object(Iobj *obj, FILE *fin)
{
  imodGetBytes(fin, (unsigned char *)obj->name, IOBJ_STRSIZE);
  imodGetInts(fin, (unsigned int *)obj->extra, IOBJ_EXSIZE);
  obj->contsize   = imodGetInt(fin);
  obj->flags      = imodGetInt(fin);
  obj->axis       = imodGetInt(fin);
  obj->drawmode   = imodGetInt(fin);
  obj->red        = imodGetFloat(fin);
  obj->green      = imodGetFloat(fin);
  obj->blue       = imodGetFloat(fin);
  obj->pdrawsize  = imodGetInt(fin);
  obj->symbol     = imodGetByte(fin);
  obj->symsize    = imodGetByte(fin);
  if (obj->symsize == 0) obj->symsize = 3;
  obj->linewidth2 = imodGetByte(fin);
  if (obj->linewidth2 == 0) obj->linewidth2 = 1;
  obj->linewidth  = imodGetByte(fin);
  obj->linesty    = imodGetByte(fin);
  obj->symflags   = imodGetByte(fin);
  obj->sympad     = imodGetByte(fin);
  obj->trans      = imodGetByte(fin);
  obj->meshsize   = imodGetInt(fin);
  obj->surfsize   = imodGetInt(fin);
  if (ferror(fin))
    return(IMOD_ERROR_READ);
  else
    return(0);
}



static int imodel_read_object_v01(Iobj *obj, FILE *fin)
{
  int i, error,tmp;
  int id;

  while(1){
    /* allow for extra chunks after model data */
    id = imodGetInt(fin);
    if (id == ID_OBJT) break;
    id = imodGetInt(fin);
    if (fseek(fin, id, SEEK_CUR))
      return(-1);
  }

  imodGetBytes(fin, (unsigned char *)obj->name, IMOD_STRSIZE);
  obj->name[IOBJ_STRSIZE - 1] = 0x00;
  for(i = 0; i < IOBJ_EXSIZE; i++)
    obj->extra[i] = 0;

  imodGetInts(fin, &obj->contsize, 4);
  imodGetFloats(fin, &obj->red, 3);
  obj->pdrawsize = imodGetInt(fin);

  /* Combatibility with old imod files. */
  if (obj->pdrawsize < 1)
    obj->pdrawsize = 1;

  tmp = imodGetInt(fin);
  obj->linewidth = tmp;
  if (obj->linewidth < 1)
    obj->linewidth = 1;
  obj->linewidth2 = 1;
  obj->symbol = 0;
  obj->symsize = 3;

  obj->linesty = 0;
  obj->symflags = 0;
  obj->sympad = 0;

  tmp = imodGetInt(fin);
  obj->trans = tmp;

  obj->meshsize = imodGetInt(fin);

  if (obj->contsize){
    obj->cont = imodContoursNew(obj->contsize);
    if (obj->cont == NULL){
      obj->contsize = 0;
      return(IMOD_ERROR_MEMORY);
    }
  }

  for(i = 0; i < obj->contsize; i++)
    if ((error = imodel_read_contour_v01( &(obj->cont[i]), fin)))
      return(error);

  return(0);
}


static int imodel_read_contour_v01(Icont *cont, FILE *fin)
{
  int id;

  while(1){
    /* allow for extra chunks after object data */
    id = imodGetInt(fin);
    if (id == ID_CONT) break;
    id = imodGetInt(fin);
    if (fseek(fin, id, SEEK_CUR))
      return(-1);
  }
  imodGetInts(fin, (int *)&cont->psize, 4);
  id = imodGetInt(fin);
  if (id != ID_PNTS){
    b3dError(stderr, "IMOD: Error Reading Points.\n");
    return(-1);
  }
  cont->pts = NULL;
  cont->pts = (struct Mod_Point *)
    malloc( cont->psize * sizeof(struct Mod_Point));

  if (cont->pts == NULL){
    b3dError(stderr, "IMOD: Error getting memory for points.\n");
    cont->psize = 0;
    return(-1);
  }

  imodGetFloats(fin, (float *)cont->pts, cont->psize * 3);
  return(0);
}

static int imodel_read_contour(Icont *cont, FILE *fin)
{
  int error = 1;

  cont->psize = imodGetInt(fin);
  cont->flags = imodGetInt(fin);
  cont->time  = imodGetInt(fin);
  cont->surf  = imodGetInt(fin);

  if (ferror(fin)){
    cont->psize = 0;
#ifdef IMODEL_FILES_DEBUG
    fprintf(stderr, "imodel_read_contour: error reading cont head.\n");
#endif
    return(error);
  }
     
  if (cont->psize){
    cont->pts = (Ipoint *)malloc( cont->psize * sizeof(Ipoint));
    if (cont->pts == NULL){
      cont->psize = 0;
      return(IMOD_ERROR_MEMORY);
    }
  }
  error = imodGetFloats(fin, (float *)cont->pts, 3 * cont->psize);
    
  if (error){
    cont->psize = 0;
    free(cont->pts);
    cont->pts = NULL;
    return(error);
  }
  return(0);
}


/* DNM: read point sizes for scattered points, or other per-point data */

static int imodel_read_ptsizes(Icont *cont, FILE *fin)
{
  int i, error = 1;
  i = imodGetInt(fin);
  if (ferror(fin))
    return(error);
  if (cont->psize){
    cont->sizes = (float *)malloc( cont->psize * sizeof(float));
    if (cont->sizes == NULL)
      return(IMOD_ERROR_MEMORY);
    error = imodGetFloats(fin, cont->sizes, cont->psize);
    
    if (error){
      free(cont->sizes);
      cont->sizes = NULL;
      return(error);
    }
  }
  return(0);
}

static int imodel_read_mesh(Imesh *mesh, FILE *fin)
{
  int error = 0;

  mesh->vsize = imodGetInt(fin);
  mesh->lsize = imodGetInt(fin);
  mesh->flag  = imodGetInt(fin);

  /* DNM 6/20/01: start reading pad also, but read both type and pad as
     shorts.  This is OK because both type and pad have been zero before
     now, so there is no need to read old models with non-zero types */
  mesh->time  = imodGetShort(fin);
  mesh->surf   = imodGetShort(fin);
     
  if ((error = ferror(fin))) {
    mesh->vsize = 0;
    mesh->lsize = 0;
    return(error);
  }

  if (mesh->vsize){
    mesh->vert = (struct Mod_Point *)
      malloc(mesh->vsize * sizeof(struct Mod_Point));
    if (!mesh->vert){
      mesh->vsize = 0;
      mesh->lsize = 0;
      return(IMOD_ERROR_MEMORY);
    }
    if ((error = imodGetFloats(fin, (float *)mesh->vert,
                              mesh->vsize * 3))) {
      mesh->vsize = 0;
      mesh->lsize = 0;
      free(mesh->vert);
      return(IMOD_ERROR_READ);
    }
  }

  if (mesh->lsize){
    mesh->list = (int *)malloc(mesh->lsize * sizeof(int));
    if (!mesh->list){
      mesh->vsize = 0;
      mesh->lsize = 0;
      free(mesh->vert);
      return(IMOD_ERROR_MEMORY);
    }
    if ((error = imodGetInts(fin, mesh->list, mesh->lsize))) {
      mesh->vsize = 0;
      mesh->lsize = 0;
      if (mesh->vert)
        free(mesh->vert);
      free(mesh->list);
      return(IMOD_ERROR_READ);
    }
  }
  return(0);
}

static int imodel_read_clip(Iobj *obj, FILE *fin, b3dUInt32 flags)
{
  int error = imodClipsRead(&obj->clips, fin);
  imodClipsFixCount(&obj->clips, flags);
  return error;
}

static int imodel_read_imat(Iobj *obj, FILE *fin, b3dUInt32 flags)
{
  int size;

  size = imodGetInt(fin);

  imodGetBytes(fin, (unsigned char *)&obj->ambient, 4);

  /* DNM 9/4/03: read mat1 and mat3 as bytes, or as ints for old model */
  if (flags & IMODF_MAT1_IS_BYTES) {
    imodGetBytes(fin, &obj->fillred, 4);     
    imodGetInts(fin, &obj->mat2, 1);     
    imodGetBytes(fin, &obj->valblack, 4);
  } else
    imodGetInts(fin, (int *)&obj->fillred, 3);     
  return 0;
}

static int imodel_read_meshparm(Iobj *obj, FILE *fin)
{
  int size, error;
  size = imodGetInt(fin);
  obj->meshParam = imeshParamsNew();
  if (!obj->meshParam)
    return IMOD_ERROR_MEMORY;
  imodGetInts(fin, &obj->meshParam->flags, 9);
  imodGetFloats(fin, &obj->meshParam->overlap, 10);
  if ((error = ferror(fin)))
    return(error);
  return 0;
}

static int imodel_read_meshskip(Iobj *obj, FILE *fin)
{
  int size, error;
  size = imodGetInt(fin) / 4;
  if (!obj->meshParam || size != obj->meshParam->capSkipNz)
    return(IMOD_ERROR_FORMAT);
  obj->meshParam->capSkipZlist = (b3dInt32 *)malloc(size * sizeof(b3dInt32));
  if (!obj->meshParam->capSkipZlist)
    return IMOD_ERROR_MEMORY;
  imodGetInts(fin, obj->meshParam->capSkipZlist, size);
  if ((error = ferror(fin)))
    return(error);
  return 0;
}  
  
static int imodel_read_sliceang(Imod *imod, FILE *fin)
{
  int size, error;
  SlicerAngles slan;
  size = imodGetInt(fin);
  if (!imod->slicerAng)
    imod->slicerAng = ilistNew(sizeof(SlicerAngles), 4);
  if (!imod->slicerAng)
    return IMOD_ERROR_MEMORY;
  slan.time = imodGetInt(fin);
  imodGetFloats(fin, &slan.angles[0], 6);
  imodGetBytes(fin, (unsigned char *)(&slan.label[0]), sizeof(slan.label));
  if ((error = ferror(fin)))
    return(error);
  if (ilistAppend(imod->slicerAng, &slan))
    return IMOD_ERROR_MEMORY;
  return 0;
}

/***************************************************************************/
/*!
 * Reads an ascii model from file point in imod->file.  This is called
 * automatically by general model reading routines when the file is not
 * recognized as IMOD binary format.  Returns -1 for error.
 */
int imodReadAscii(Imod *imod)
{
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
  Iview *view = &imod->view[0];
  int i, ob, co, surf, pt;
  int conts, meshes, pts;
  int mh, vsize, lsize;
  char line[MAXLINE];
  char *strptr;
  int len, idata, idata2, idata3, activeVal, nval, gotObjMM;
  SlicerAngles slan;
  float value, size, valmin, valmax, objvmin, objvmax;
  Istore store;

#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "#imodReadAscii: Entry\n");
#endif
  imod->cindex.object = -1;
  imod->cindex.contour = -1;
  imod->cindex.point = -1;
  valmin = 1.e30;
  valmax = -1.e30;
  gotObjMM = 0;
  ob = -1;

  len = imodFgetline(imod->file,line, MAXLINE);
  if (len < 1) return(-1);
  if (!substr(line, "imod"))
    return -1;

  sscanf(line, "imod %d", &(imod->objsize));
  imod->obj = imodObjectsNew(imod->objsize);
  if (imod->objsize > 0)
    imod->cindex.object = 0;
     
  store.type = GEN_STORE_VALUE1;
  store.flags = GEN_STORE_FLOAT << 2;
  obj = imod->obj;

  while ( ((len = imodFgetline(imod->file,line, MAXLINE)) > 0)){
	  
    if (substr(line, "contour")){
      nval = sscanf(line, "contour %d %d %d %g", &co, &surf, &pts, &value);
      cont = &(obj->cont[co]);
      cont->surf = surf;
      cont->psize = pts;
      if (pts)
        cont->pts = (Ipoint *)malloc(pts * sizeof(Ipoint));
      cont->flags = 0;
      cont->time  = 0;
      if (nval > 3) {
        valmin = B3DMIN(valmin, value);
        valmax = B3DMAX(valmax, value);
        store.index.i = co;
        store.value.f = value;
        if (istoreInsert(&obj->store, &store))
          return(-1);
      }
      activeVal = 0;
      for(pt = 0; pt < pts; pt++){
        imodFgetline(imod->file, line, MAXLINE);
        nval = sscanf(line, "%g %g %g %g %g", 
                      &(cont->pts[pt].x), 
                      &(cont->pts[pt].y), 
                      &(cont->pts[pt].z), &size, &value);
        if (nval > 3 && size >= 0)
          imodPointSetSize(cont, pt, size);
        if (nval > 4) {
          store.index.i = pt;
          store.value.f = value;
          if (istoreInsertChange(&cont->store, &store))
            return -1;
          activeVal = 1;
          valmin = B3DMIN(valmin, value);
          valmax = B3DMAX(valmax, value);
        } else if (activeVal) {
          istoreEndChange(cont->store, GEN_STORE_VALUE1, pt);
          activeVal = 0;
        }
      }
      continue;
    }


    if (substr(line, "mesh")){
      sscanf(line, "mesh %d %d %d", &mh, &vsize, &lsize);
      mesh = &(obj->mesh[mh]);
      mesh->flag = mesh->time = mesh->surf = 0;
      mesh->vert = (Ipoint *)malloc(vsize * sizeof(Ipoint));
      mesh->list = (int *)malloc(lsize * sizeof(int));
      mesh->vsize = vsize;
      mesh->lsize = lsize;
      for(pt = 0; pt < vsize; pt++){
        imodFgetline(imod->file, line, MAXLINE);
        sscanf(line, "%g %g %g",
			   &(mesh->vert[pt].x),
			   &(mesh->vert[pt].y),
			   &(mesh->vert[pt].z));
      }
      for(i = 0; i < lsize; i++){
        imodFgetline(imod->file, line, MAXLINE);
        sscanf(line, "%d", &(mesh->list[i]));
      }
      continue;
    }

    if (substr(line, "object ")){

      /* new object: put out min/max for last one if any */
      if (ob >= 0 && (gotObjMM || valmin <= valmax))
        istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, gotObjMM ? 
                        objvmin : valmin, gotObjMM ? objvmax : valmax);
      sscanf(line, "object %d %d %d", &ob, &conts, &meshes);

#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "#imodReadAscii: read object %d %d %d\n",
              ob, conts, meshes);
#endif

      if (ob < 0) {
        return (-1);
      }
      if (ob > imod->objsize) return -1;
      obj = &(imod->obj[ob]);
      obj->contsize = conts;
      obj->meshsize = meshes;
      if (conts)
        obj->cont = imodContoursNew(conts);

      if (meshes)
        obj->mesh = imodMeshesNew(meshes);
      mh = -1;
      co = -1;
      valmin = 1.e30;
      valmax = -1.e30;
      gotObjMM = 0;
      continue;
    }

    if (substr(line, "color ")){
      sscanf(line, "color %g %g %g %d",
             &(obj->red), &(obj->green), &(obj->blue), &idata);
      obj->trans = idata;
    }
    if (substr(line, "Fillcolor ")){
      sscanf(line, "Fillcolor %d %d %d", &idata, &idata2, &idata3);
      obj->fillred = idata;
      obj->fillgreen = idata2;
      obj->fillblue = idata3;
    }

    if (substr(line, "open"))
      obj->flags |= IMOD_OBJFLAG_OPEN;

    if (substr(line, "closed"))
      obj->flags &= ~IMOD_OBJFLAG_OPEN;

    if (substr(line, "fill"))
      obj->flags |= IMOD_OBJFLAG_FILL;

    if (substr(line, "scattered"))
      obj->flags |= IMOD_OBJFLAG_SCAT;

    if (substr(line, "insideout"))
      obj->flags |= IMOD_OBJFLAG_OUT;

    if (substr(line, "drawmesh"))
      obj->flags |= IMOD_OBJFLAG_MESH;

    if (substr(line, "nolines"))
      obj->flags |= IMOD_OBJFLAG_NOLINE;

    if (substr(line, "bothsides"))
      obj->flags |= IMOD_OBJFLAG_TWO_SIDE;

    if (substr(line, "usefill"))
      obj->flags |= IMOD_OBJFLAG_FCOLOR;

    if (substr(line, "pntusefill"))
      obj->flags |= IMOD_OBJFLAG_FCOLOR_PNT;

    if (substr(line, "pntonsec"))
      obj->flags |= IMOD_OBJFLAG_PNT_ON_SEC;

    if (substr(line, "antialias"))
      obj->flags |= IMOD_OBJFLAG_ANTI_ALIAS;

    if (substr(line, "hastimes"))
      obj->flags |= IMOD_OBJFLAG_TIME;

    if (substr(line, "usevalue"))
      obj->flags |= IMOD_OBJFLAG_USE_VALUE;

    if (substr(line, "valcolor"))
      obj->flags |= IMOD_OBJFLAG_MCOLOR;

    if (substr(line, "offsets "))
      sscanf(line, "offsets %g %g %g",
             &(imod->xoffset), &(imod->yoffset), &(imod->zoffset));

    if (substr(line, "angles "))
      sscanf(line, "angles %g %g %g",
             &(imod->alpha), &(imod->beta), &(imod->gamma));

    if (substr(line, "refcurscale ")) {
      if (!imod->refImage)
        imod->refImage = imodIMNXNew();
      sscanf(line, "refcurscale %g %g %g", &imod->refImage->cscale.x,
             &imod->refImage->cscale.y, &imod->refImage->cscale.z);
    }
    if (substr(line, "refcurtrans ")) {
      if (!imod->refImage)
        imod->refImage = imodIMNXNew();
      sscanf(line, "refcurtrans %g %g %g", &imod->refImage->ctrans.x,
             &imod->refImage->ctrans.y, &imod->refImage->ctrans.z);
    }
    if (substr(line, "refcurrot ")) {
      if (!imod->refImage)
        imod->refImage = imodIMNXNew();
      imod->flags |= IMODF_TILTOK;
      sscanf(line, "refcurrot %g %g %g", &imod->refImage->crot.x,
             &imod->refImage->crot.y, &imod->refImage->crot.z);
    }
    if (substr(line, "refoldtrans ")) {
      if (!imod->refImage)
        imod->refImage = imodIMNXNew();
      imod->flags |= IMODF_OTRANS_ORIGIN;
      sscanf(line, "refoldtrans %g %g %g", &imod->refImage->otrans.x,
             &imod->refImage->otrans.y, &imod->refImage->otrans.z);
    }

    if (substr(line, "nodraw"))
      obj->flags |= IMOD_OBJFLAG_OFF;

    if (substr(line, "scale "))
      sscanf(line, "scale %g %g %g", 
             &(imod->xscale), &(imod->yscale), &(imod->zscale));

    if (substr(line, "max "))
      sscanf(line, "max %d %d %d",
             &(imod->xmax), &(imod->ymax), &(imod->zmax));

    if (substr(line, "mousemode"))
      imod->mousemode = atoi(&(line[9]));

    if (substr(line, "drawmode"))
      imod->drawmode = atoi(&(line[8]));

    if (substr(line, ""))
      sscanf(line, "b&w_level %d%*c%d", 
             &(imod->blacklevel), &(imod->whitelevel));

    if (substr(line, "resolution"))
      imod->res = (float)atof(&(line[10]));

    if (substr(line, "threshold"))
      imod->thresh = atoi(&(line[9]));

    if (substr(line, "pixsize"))
      imod->pixsize = (float)atof(&(line[7]));

    if (substr(line, "flipped"))
      if (atoi(&(line[7])))
        imod->flags |= IMODF_FLIPYZ;

    if (substr(line, "units")){
      if (strstr(line, "mm"))
        imod->units = IMOD_UNIT_MM;
      if (strstr(line, "um"))
        imod->units = IMOD_UNIT_UM;
      if (strstr(line, "nm"))
        imod->units = IMOD_UNIT_NM;
    }
	  
    if (substr(line, "currentview"))
      imod->cview = atoi(&(line[11]));

    if (substr(line, "slicerAngle")) {
      if (!imod->slicerAng)
        imod->slicerAng = ilistNew(sizeof(SlicerAngles), 4);
      sscanf(line, "slicerAngle %d %g %g %g %g %g %g\n", &slan.time,
             &slan.angles[0], &slan.angles[1], &slan.angles[2],
             &slan.center.x, &slan.center.y, &slan.center.z);
      strptr = &line[0];
      slan.label[0] = 0x00;
      for (i = 0; i < 8 && strptr; i++)
        strptr = strchr(strptr + 1, ' ');
      if (strptr)
        strncpy(slan.label, strptr + 1, ANGLE_STRSIZE);
      slan.label[ANGLE_STRSIZE - 1] = 0x00;
      ilistAppend(imod->slicerAng, &slan);
    }
    
    if (substr(line, "name")) {
      for (i = 0; i < strlen(line) - 5 && i < IOBJ_STRSIZE && line[i+5] != '\r' && 
             line[i+5] != '\n'; i++)
        obj->name[i] = line[i + 5];
      obj->name[i] = 0x00;
    }

    if (substr(line, "linewidth"))
      obj->linewidth = atoi(&(line[9]));

    if (substr(line, "surfsize"))
      obj->surfsize = atoi(&(line[8]));

    if (substr(line, "pointsize"))
      obj->pdrawsize = atoi(&(line[9]));

    if (substr(line, "axis"))
      obj->axis = atoi(&(line[4]));

    if (substr(line, "drawmode"))
      obj->drawmode = atoi(&(line[8]));

    if (substr(line, "width2D"))
      obj->linewidth2 = atoi(&(line[7]));

    if (substr(line, "symbol"))
      obj->symbol = atoi(&(line[6]));

    if (substr(line, "symsize"))
      obj->symsize = atoi(&(line[7]));

    if (substr(line, "symflags"))
      obj->symflags = atoi(&(line[8]));

    if (substr(line, "ambient"))
      obj->ambient = atoi(&(line[7]));

    if (substr(line, "diffuse"))
      obj->diffuse = atoi(&(line[7]));

    if (substr(line, "specular"))
      obj->specular = atoi(&(line[8]));

    if (substr(line, "shininess"))
      obj->shininess = atoi(&(line[9]));

    if (substr(line, "obquality"))
      obj->quality = atoi(&(line[9]));

    if (substr(line, "valblack"))
      obj->valblack = atoi(&(line[8]));

    if (substr(line, "valwhite"))
      obj->valwhite = atoi(&(line[8]));

    if (substr(line, "matflags2"))
      obj->matflags2 = atoi(&(line[9]));

    if (substr(line, "valminmax")) {
      sscanf(line, "valminmax %g %g", &objvmin, &objvmax);
      gotObjMM = 1;
    }

    if (substr(line, "objclips"))
      readAsciiClips(imod, line, &obj->clips, "objclips");

    if (substr(line, "contflags") && co >= 0 && co < obj->contsize)
      obj->cont[co].flags = (b3dUInt32)atof(&(line[9]));

    if (substr(line, "conttime") && co >= 0 && co < obj->contsize)
      obj->cont[co].time = atoi(&(line[8]));

    if (substr(line, "Meshflags") && mh >= 0 && mh < obj->meshsize)
      obj->mesh[mh].flag = (b3dUInt32)atof(&(line[9]));

    if (substr(line, "Meshtime") && mh >= 0 && mh < obj->meshsize)
      obj->mesh[mh].time = atoi(&(line[8]));

    if (substr(line, "Meshsurf") && mh >= 0 && mh < obj->meshsize)
      obj->mesh[mh].surf = atoi(&(line[8]));

    if (substr(line, "view ")) {
      if (imodViewModelNew(imod))
        return(-1);
      view = &imod->view[imod->viewsize - 1];
    }

    if (substr(line, "globalclips"))
      readAsciiClips(imod, line, &view->clips, "globalclips");

    if (substr(line, "viewfovy"))
      view->fovy = atof(&(line[8]));

    if (substr(line, "viewcnear"))
      view->cnear = atof(&(line[9]));

    if (substr(line, "viewcfar"))
      view->cfar = atof(&(line[8]));

    if (substr(line, "viewflags"))
      view->world = (b3dUInt32)atof(&(line[9]));

    if (substr(line, "viewscale "))
      sscanf(line, "viewscale %g %g %g", &view->scale.x, &view->scale.y, &view->scale.z);

    if (substr(line, "viewtrans "))
      sscanf(line, "viewtrans %g %g %g", &view->trans.x, &view->trans.y, &view->trans.z);

    if (substr(line, "viewrot "))
      sscanf(line, "viewrot %g %g %g", &view->rot.x, &view->rot.y, &view->rot.z);

    if (substr(line, "viewlight "))
      sscanf(line, "viewlight %g %g", &view->lightx, &view->lighty);

    if (substr(line, "depthcue "))
      sscanf(line, "depthcue %g %g", &view->dcstart, &view->dcend);

    if (substr(line, "viewlabel")) {
      for (i = 0; i < strlen(line) - 10 && i < VIEW_STRSIZE && line[i+10] != '\r' && 
             line[i+10] != '\n'; i++)
        view->label[i] = line[i+10];
      view->label[i] = 0x00;
    }

  }
  if (ob >= 0 && (gotObjMM || valmin <= valmax))
    istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, gotObjMM ? 
                    objvmin : valmin, gotObjMM ? objvmax : valmax);
  B3DCLAMP(imod->cview, 0, imod->viewsize - 1);
  return(0);
}

static void readAsciiClips(Imod *imod, char *line, IclipPlanes *clips, char *prefix)
{
  int idata, idata2, idata3, idata4, i;
  sscanf(line + strlen(prefix), " %d %d %d %d", &idata, &idata2, &idata3, &idata4);
  clips->count = idata;
  clips->flags = idata2;
  clips->trans = idata3;
  clips->plane = idata4;
  for (i = 0; i < clips->count; i++) {
    imodFgetline(imod->file, line, MAXLINE);
    sscanf(line, "%g %g %g %g %g %g", &clips->normal[i].x,
           &clips->normal[i].y, &clips->normal[i].z,
           &clips->point[i].x, &clips->point[i].y, &clips->point[i].z);
  }
}

/*!
 * Writes the model [imod] in ascii format to the file pointer in imod->file.
 * Returns 0.
 */
int imodWriteAscii(Imod *imod)
{
  int ob, co, pt, iv;
  int mh, i;
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
  Iview *view;
  IrefImage *ref = imod->refImage;
  SlicerAngles *slanp;
  DrawProps defProps, contProps, ptProps;
  int contState, surfState, stateFlags, changeFlags, nextChange;
  float valmin, valmax, size;

  rewind(imod->file);
  fprintf(imod->file, "# imod ascii file version 2.0\n\n");
  fprintf(imod->file, "imod %d\n", imod->objsize);
  fprintf(imod->file, "max %d %d %d\n", imod->xmax, imod->ymax, imod->zmax);
  fprintf(imod->file, "offsets %g %g %g\n",
          imod->xoffset, imod->yoffset, imod->zoffset);
  fprintf(imod->file, "angles %g %g %g\n",
          imod->alpha, imod->beta, imod->gamma);
  fprintf(imod->file, "scale %g %g %g\n",
          imod->xscale, imod->yscale, imod->zscale);
  fprintf(imod->file, "mousemode  %d\n", imod->mousemode);
  fprintf(imod->file, "drawmode   %d\n", imod->drawmode);
  fprintf(imod->file, "b&w_level  %d,%d\n", 
          imod->blacklevel, imod->whitelevel);
  fprintf(imod->file, "resolution %d\n", imod->res);
  fprintf(imod->file, "threshold  %d\n", imod->thresh);
  fprintf(imod->file, "pixsize    %g\n", imod->pixsize);
  fprintf(imod->file, "units      %s\n", imodUnits(imod));
  fprintf(imod->file, "flipped    %d\n", (imod->flags & IMODF_FLIPYZ) ? 1 : 0);
  if (ref) {
    fprintf(imod->file, "refcurscale %g %g %g\n",
            ref->cscale.x, ref->cscale.y, ref->cscale.z);
    fprintf(imod->file, "refcurtrans %g %g %g\n",
            ref->ctrans.x, ref->ctrans.y, ref->ctrans.z);
    if (imod->flags & IMODF_TILTOK)
      fprintf(imod->file, "refcurrot %g %g %g\n",
              ref->crot.x, ref->crot.y, ref->crot.z);
    if (imod->flags & IMODF_OTRANS_ORIGIN)
      fprintf(imod->file, "refoldtrans %g %g %g\n",
              ref->otrans.x, ref->otrans.y, ref->otrans.z);
  }
  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    fprintf(imod->file, "slicerAngle %d %g %g %g %g %g %g %s\n", slanp->time,
            slanp->angles[0], slanp->angles[1], slanp->angles[2],
            slanp->center.x, slanp->center.y, slanp->center.z, slanp->label);
  }

  fprintf(imod->file, "currentview  %d\n", imod->cview);
  for (iv = 1; iv < imod->viewsize; iv++) {
    fprintf(imod->file, "view %d\n", iv);
    view = &imod->view[iv];
    fprintf(imod->file, "viewfovy  %g\n", view->fovy);
    fprintf(imod->file, "viewcnear %g\n", view->cnear);
    fprintf(imod->file, "viewcfar  %g\n", view->cfar);
    fprintf(imod->file, "viewflags %u\n", view->world);
    /* fprintf(imod->file, "viewscale %g %g %g\n",
       view->scale.x, view->scale.y, view->scale.z); Not used, not needed! */
    fprintf(imod->file, "viewtrans %g %g %g\n",
            view->trans.x, view->trans.y, view->trans.z);
    fprintf(imod->file, "viewrot %g %g %g\n", view->rot.x, view->rot.y, view->rot.z);
    fprintf(imod->file, "viewlight %g %g\n", view->lightx, view->lighty);
    fprintf(imod->file, "depthcue %g %g\n", view->dcstart, view->dcend);
    fprintf(imod->file, "viewlabel %s\n", view->label);
    writeAsciiClips(imod, &view->clips, "globalclips");
  }

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    fprintf(imod->file, "\nobject %d %d %d\n", ob, obj->contsize,
            obj->meshsize);
    fprintf(imod->file, "name %s\n", obj->name);
    fprintf(imod->file, "color %g %g %g %d\n",
            obj->red, obj->green, obj->blue, obj->trans);
    if (obj->fillred || obj->fillgreen || obj->fillblue)
      fprintf(imod->file, "Fillcolor %d %d %d\n",
              obj->fillred, obj->fillgreen, obj->fillblue);
    if (obj->flags & IMOD_OBJFLAG_OPEN)
      fprintf(imod->file, "open\n");
    if (obj->flags & IMOD_OBJFLAG_SCAT)
      fprintf(imod->file, "scattered\n");
    if (obj->flags & IMOD_OBJFLAG_OFF)
      fprintf(imod->file, "nodraw\n");
    if (obj->flags & IMOD_OBJFLAG_OUT)
      fprintf(imod->file, "insideout\n");
    if (obj->flags & IMOD_OBJFLAG_FILL)
      fprintf(imod->file, "fill\n");
    if (obj->flags & IMOD_OBJFLAG_MESH)
      fprintf(imod->file, "drawmesh\n");
    if (obj->flags & IMOD_OBJFLAG_NOLINE)
      fprintf(imod->file, "nolines\n");
    if (obj->flags & IMOD_OBJFLAG_TWO_SIDE)
      fprintf(imod->file, "bothsides\n");
    if (obj->flags & IMOD_OBJFLAG_FCOLOR)
      fprintf(imod->file, "usefill\n");
    if (obj->flags & IMOD_OBJFLAG_FCOLOR_PNT)
      fprintf(imod->file, "pntusefill\n");
    if (obj->flags & IMOD_OBJFLAG_PNT_ON_SEC)
      fprintf(imod->file, "pntonsec\n");
    if (obj->flags & IMOD_OBJFLAG_ANTI_ALIAS)
      fprintf(imod->file, "antialias\n");
    if (obj->flags & IMOD_OBJFLAG_TIME)
      fprintf(imod->file, "hastimes\n");
    if (obj->flags & IMOD_OBJFLAG_USE_VALUE)
      fprintf(imod->file, "usevalue\n");
    if (obj->flags & IMOD_OBJFLAG_MCOLOR)
      fprintf(imod->file, "valcolor\n");

    fprintf(imod->file, "linewidth %d\n", obj->linewidth);
    fprintf(imod->file, "surfsize  %d\n", obj->surfsize);
    fprintf(imod->file, "pointsize %d\n", obj->pdrawsize);
    fprintf(imod->file, "axis      %d\n", obj->axis);
    fprintf(imod->file, "drawmode  %d\n", obj->drawmode);
    fprintf(imod->file, "width2D   %d\n", obj->linewidth2);
    fprintf(imod->file, "symbol    %d\n", obj->symbol);
    fprintf(imod->file, "symsize   %d\n", obj->symsize);
    fprintf(imod->file, "symflags  %d\n", obj->symflags);
    fprintf(imod->file, "ambient   %d\n", obj->ambient);
    fprintf(imod->file, "diffuse   %d\n", obj->diffuse);
    fprintf(imod->file, "specular  %d\n", obj->specular);
    fprintf(imod->file, "shininess %d\n", obj->shininess);
    fprintf(imod->file, "obquality %d\n", obj->quality);
    fprintf(imod->file, "valblack  %d\n", obj->valblack);
    fprintf(imod->file, "valwhite  %d\n", obj->valwhite);
    fprintf(imod->file, "matflags2 %d\n", obj->matflags2);
    writeAsciiClips(imod, &obj->clips, "objclips");

    if (istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &valmin,
                        &valmax))
      fprintf(imod->file, "valminmax %g %g\n", valmin, valmax);

    istoreDefaultDrawProps(obj, &defProps);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      istoreContSurfDrawProps(obj->store, &defProps, &contProps, co, 
                              cont->surf, &contState, &surfState);
      fprintf(imod->file, "contour %d %d %d", co, cont->surf, cont->psize);
      if (contState & CHANGED_VALUE1)
        fprintf(imod->file, " %g", contProps.value1);
      fprintf(imod->file, "\n");
      
      nextChange = istoreFirstChangeIndex(cont->store);
      stateFlags = 0;
      for(pt = 0; pt < cont->psize; pt++){
        fprintf(imod->file, "%g %g %g",
			    cont->pts[pt].x, cont->pts[pt].y, cont->pts[pt].z);
        if (pt == nextChange)
          nextChange = istoreNextChange(cont->store, &contProps, &ptProps,
                                        &stateFlags, &changeFlags);

        size = -1.;
        if (cont->sizes && cont->sizes[pt] >= 0)
          size = cont->sizes[pt];

        /* Output size if any, or size and value */
        if (size >= 0 && !(stateFlags & CHANGED_VALUE1))
          fprintf(imod->file, " %g", size);
        else if (stateFlags & CHANGED_VALUE1)
          fprintf(imod->file, " %g %g", size, ptProps.value1);
        fprintf(imod->file, "\n");
      }
      if (cont->flags)
        fprintf(imod->file, "contflags %u\n", cont->flags);
      if (cont->time)
        fprintf(imod->file, "conttime %d\n", cont->time);
    }

    for(mh = 0; mh < obj->meshsize; mh++){
      mesh = &(obj->mesh[mh]);
      fprintf(imod->file, "mesh %d %d %d\n", 
              mh, mesh->vsize, mesh->lsize);
      for(pt = 0; pt < mesh->vsize; pt++){
        fprintf(imod->file, "%g %g %g\n",
			    mesh->vert[pt].x, mesh->vert[pt].y, 
			    mesh->vert[pt].z);
      }
      for(i = 0; i < mesh->lsize; i++)
        fprintf(imod->file, "%d\n", mesh->list[i]);
      if (mesh->flag)
        fprintf(imod->file, "Meshflags %u\n", mesh->flag);
      if (mesh->time)
        fprintf(imod->file, "Meshtime %d\n", mesh->time);
      if (mesh->surf)
        fprintf(imod->file, "Meshsurf %d\n", mesh->surf);
    }
  }

  fprintf(imod->file, "# end of IMOD model\n");
  return(0);
}

static void writeAsciiClips(Imod *imod, IclipPlanes *clips, char *prefix)
{
  int i;
  if (clips->count) {
    fprintf(imod->file, "%s %d %d %d %d\n", prefix, clips->count,
            clips->flags, clips->trans, clips->plane);
    for (i = 0; i < clips->count; i++)
      fprintf(imod->file, "%g %g %g %g %g %g\n", clips->normal[i].x,
              clips->normal[i].y, clips->normal[i].z,
              clips->point[i].x, clips->point[i].y, clips->point[i].z);
  }
}

int imodFgetline(FILE *fp, char s[],int limit)
{
  int c, i, length;
     
  if (fp == NULL){
    return(-1);
  }
     
  if (limit < 3){
    return(-1);
  }

  i = 0;
  do{
    c = getc(fp);
    if (c != '\r')
      s[i++] = c;
  }while( (c != EOF) && (i < (limit-1)) && (c != '\n'));
     
  if (i == 1){
    if (c == EOF){
      return(0);
    }
    if (c == '\n'){
      s[++i] = '\0';
      return(imodFgetline(fp, s, limit));
    }
  }
     
  if (s[0] == '#'){
    return(imodFgetline(fp, s, limit));
  }

  s[i]='\0';
  length = i;
     
  if (c == EOF)
    return (-1 * length);
  else{
    return (length);
  }
}

/*****************************************************************************/
/** file i/o **/
#ifdef IMOD_DATA_SWAP
static void byteswap(void *iptr, unsigned size)

{
  unsigned char *ptr = iptr;
  unsigned char *begin;
  unsigned char *end;
  unsigned char tmp;
  int           i;
     
  if ((size % 2) != 0)
    size--;
     
  begin = ( unsigned char *)ptr;
  end   = ( unsigned char *)ptr + (size - 1);
     
  for (i = 0; i < (size/2); i++) {
    tmp = *begin;
    *begin = *end;
    *end   = tmp;
	  
    begin++;
    end--;
  }
}

static void swap_longs(unsigned char *data, int amt)
{
  register unsigned char *ldata = (unsigned char *)data + (amt * 4);
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char tmp;
  while(ptr < ldata){
    tmp = ptr[0];
    ptr[0] = ptr[3];
    ptr[3] = tmp;
    ptr++;
    tmp = *ptr;
    *ptr = ptr[1];
    ptr[1] = tmp;
    ptr+=3;
  }
}

#define BUF_SIZE 256
static int buf[BUF_SIZE];

static void convert_write(void (*convertFunc)(), FILE *fp, unsigned char *data,
                          int size)
{
  int chunk;
  int base = 0;
  while (size) {
    chunk = size < BUF_SIZE ? size : BUF_SIZE;
    memcpy(&buf, &data[base], 4 * chunk);
    convertFunc(&buf, chunk);
    fwrite(&buf, 4, chunk, fp);
    size -= chunk;
    base += 4 * chunk;
  }
}

#endif

#ifdef IMOD_FLOAT_CONVERT
static void tovmsfloat(unsigned char *data, int amt)
{
  unsigned char exp, temp;
  int i;
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char *maxptr = (unsigned char *)data + (amt * 4);
     
  while (ptr < maxptr){
    if ((exp = (ptr[0] << 1) | (ptr[1] >> 7 & 0x01)) < 253 && exp != 0)
      ptr[0] += 1;
    else if (exp >= 253) /*must also max out the exp & mantissa*/
      {
        /*we want manitssa all 1 & exponent 255*/
        ptr[0] |= 0x7F;
        ptr[1] = 0xFF;
        ptr[2] = ptr[3] = 0xFF;
      }
	  
    temp = ptr[0];
    ptr[0] = ptr[1];
    ptr[1] = temp;
    temp = ptr[2];
    ptr[2] = ptr[3];
    ptr[3] = temp;
    ptr+=4;
  }
}
#endif

/* DNM 12/3/01: turn this from conditionally compiled static fromvmsfloat
   to a globally compiled function so it is available in imodel_fwrap */
void imodFromVmsFloats(unsigned char *data, int amt)
{
  unsigned char exp, temp;
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char *maxptr = (unsigned char *)data + (amt * 4);
     
  while (ptr < maxptr){

    if ((exp = (ptr[1] << 1) | (ptr[0] >> 7 & 0x01)) > 3 &&
        exp != 0)
      ptr[1] -= 1;
    else if (exp <= 3 && exp != 0)  /*must zero out the mantissa*/
      {
        /*we want manitssa 0 & exponent 1*/
        ptr[0] = 0x80;
        ptr[1] &= 0x80;
        ptr[2] = ptr[3] = 0;
      }
	  
    temp = ptr[0];
    ptr[0] = ptr[1];
    ptr[1] = temp;
    temp = ptr[2];
    ptr[2] = ptr[3];
    ptr[3] = temp;
    ptr+=4;
  }
}


/* imod files store floats as 4 bytes 4321 in IEEE format. */
float imodGetFloat(FILE *fp)
{
  float buf;
  imodGetFloats(fp, &buf, 1);
  return(buf);
}

int imodGetFloats(FILE *fp, float *buf, int size)
{
  fread(buf, 4, size, fp);

#ifdef IMOD_FLOAT_CONVERT
  tovmsfloat((unsigned char *)buf, size);
#else
#ifdef IMOD_DATA_SWAP
  swap_longs((unsigned char *)buf, size);
#endif
#endif
  return(ferror(fp));
}

int imodPutFloat(FILE *fp, float *dat)
{
  return(imodPutFloats(fp, dat, 1));
}

int imodPutFloats(FILE *fp, float *buf, int size)
{
#ifdef IMOD_FLOAT_CONVERT 
  convert_write(imodConvertVmsFloat, fp, (unsigned char *)buf, size);
#else
#ifdef IMOD_DATA_SWAP
  convert_write(swap_longs, fp, (unsigned char *)buf, size);
#else
  fwrite(buf, 4, size, fp);
#endif /* SWAP */
#endif /* CONVERT */
  return(ferror(fp));
}

int imodPutScaledPoints(FILE *fp, Ipoint *buf, int size, Ipoint *scale)
{
  float xyz[3];
  int i, ierr;
  for (i = 0; i < size; i++) {
    xyz[0] = buf[i].x * scale->x;
    xyz[1] = buf[i].y * scale->y;
    xyz[2] = buf[i].z * scale->z;
    ierr = imodPutFloats(fp, xyz, 3);
    if (ierr)
      return ierr;
  }
  return 0;
}

/* imod file stores ints as 4 bytes 4321 */
int imodGetInt(FILE *fp)
{
  int buf;
  imodGetInts(fp, &buf, 1);
  return(buf);
}

int imodGetInts(FILE *fp, void *buf, int size)
{
  fread(buf, 4, size, fp);
#ifdef IMOD_DATA_SWAP
  swap_longs(buf, size);
#endif
  return(ferror(fp));
}

int imodPutInt(FILE *fp, void *dat)
{
  return(imodPutInts(fp, dat, 1));
}

int imodPutInts(FILE *fp, void *buf, int size)
{
#ifdef IMOD_DATA_SWAP
  convert_write(swap_longs, fp, (unsigned char *)buf, size);
#else
  fwrite(buf, 4, size, fp);
#endif
  return(ferror(fp));
}

/* imod file stores USHORT as 2 bytes 21 */
b3dInt16 imodGetShort(FILE *fp)
{
  short buf;
  fread(&buf, 2, 1, fp);
#ifdef IMOD_DATA_SWAP
  byteswap(&buf, 2);
#endif
  return(buf);
}
int imodPutShort(FILE *fp, void *buf)
{
#ifdef IMOD_DATA_SWAP
  unsigned short *usdat = buf;
  unsigned short rdat = *usdat;
  byteswap(&rdat, 2);
  fwrite(&rdat, 2, 1, fp);
#else
  fwrite(buf, 2, 1, fp);
#endif
  return(ferror(fp));
}

/* lucky: bytes are bytes */
unsigned char imodGetByte(FILE *fp)
{
  unsigned char buf;
  fread(&buf, 1, 1, fp);
  return(buf);
}
int imodGetBytes(FILE *fp, unsigned char *buf, int size)
{
  fread(buf, 1, size, fp);

  return(ferror(fp));
}
int imodPutBytes(FILE *fp, unsigned char *buf, int size)
{
  fwrite(buf, 1, size, fp);
  return(ferror(fp));
}
int imodPutByte(FILE *fp, unsigned char *dat)
{
  fwrite(dat, 1, 1, fp);
  return(ferror(fp));
}

/*

$Log$
Revision 3.37  2011/05/28 23:02:16  mast
Stop writing clip->counts as 0 when there is one off plane.

Revision 3.36  2009/01/15 15:23:03  mast
More consts

Revision 3.35  2009/01/02 05:18:48  mast
const char * for Qt 4 port

Revision 3.34  2008/12/09 23:26:42  mast
Changed flag from line to noline

Revision 3.33  2008/11/20 20:13:29  mast
Added refimage info to ascii model

Revision 3.32  2008/05/31 23:02:48  mast
Put contour and point general values into ascii model

Revision 3.31  2008/03/03 17:46:15  mast
Rename run scripts to program names

Revision 3.30  2008/01/27 06:20:15  mast
Changes for object groups

Revision 3.29  2007/11/01 16:47:02  mast
Fixed reading of ascii modelwith DOS line endings

Revision 3.28  2007/06/12 05:39:25  mast
Upgraded ascii format

Revision 3.27  2007/05/25 05:18:26  mast
Changes for slicer angle storage

Revision 3.26  2006/09/13 23:52:43  mast
Fixed reading of skip list

Revision 3.25  2006/09/13 02:43:30  mast
Stopped calling imodDefault twice when reading a model

Revision 3.24  2006/09/12 15:22:39  mast
Added mesh parameters

Revision 3.23  2006/08/31 21:11:29  mast
Changed mat1 and mt3 to real names

Revision 3.22  2005/10/14 21:45:22  mast
Fixed casting in calls to imodPutScaledPoints

Revision 3.21  2005/10/13 20:05:43  mast
Handle clip plane writing properly from binned data

Revision 3.20  2005/09/09 18:38:04  mast
Fixed fatal bug in writing mesh from model loaded on binned data

Revision 3.19  2005/06/20 22:24:58  mast
Added calls to read/write general storage

Revision 3.18  2005/04/23 23:37:31  mast
Documented functions

Revision 3.17  2005/03/20 19:56:49  mast
Eliminating duplicate functions

Revision 3.16  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.15  2004/11/20 04:15:58  mast
Eliminated virtual stiff

Revision 3.14  2004/09/21 20:12:01  mast
Changes for multiple and global clip planes, surface labels

Revision 3.13  2004/09/10 21:33:46  mast
Eliminated long variables

Revision 3.12  2004/01/05 18:30:26  mast
Removed print statement about scaling

Revision 3.11  2004/01/05 17:28:09  mast
Added "unbinning" of model on output to undo scaling by 3dmod on input

Revision 3.10  2003/11/01 16:41:56  mast
changed to use new error processing routine

Revision 3.9  2003/03/28 05:08:13  mast
Use new unique little endian flag

Revision 3.8  2003/02/27 00:34:40  mast
add projects' *.dsw *.dsp

Revision 3.7  2003/02/21 23:58:29  mast
Open files in binary mode

Revision 3.6  2003/02/21 22:21:35  mast
Use new b3d types

Revision 3.5  2002/11/25 19:04:42  mast
Added "int" in front of imodFgetline

Revision 3.4  2002/09/04 23:12:50  mast
Read mat1 and mat3 the old way if a new flag is not set.  Set the flag
when writing a file.

Revision 3.3  2002/09/03 20:05:14  mast
Changed some casts in the mat1 and mat3 calls

Revision 3.2  2002/09/03 19:38:52  mast
Made it save and restore mat1 and mat3 as 4 bytes instead of integers

Revision 3.1  2001/12/05 16:00:46  mast
Make conversion of VMS floats be a globally available function instead of
conditionally compiled static

*/
