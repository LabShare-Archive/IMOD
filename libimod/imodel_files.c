/*
 *  imodel_files.c -- Open, Read and Write Imod files.
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

/*
 * Functions                           Discription
 * ---------------------------------------------------------------------------
 * int   imodOpenFile(char *filename,  Open a file with the given mode for
 *                    char *mode,      the given model.
 *                    Imod *imod); 
 * int   imodCloseFile(Imod *imod);    Close imod file.
 * int   imodReadFile(Imod *imod);     Read model using opened file.
 * Imod *imodRead(char *filename);     Read model into memory and close file.
 *
 * int imodWriteFile(Imod *imod);      Write an imod file.
 * int imodWrite(Imod *imod,           Write a model to given file.
 *               FILE *fout);
 * int imodWriteAscii(Imod *imod);     Write an imod file in ascii format.
 *
 */

#include <stdio.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

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

static int imodel_read_v01(struct Mod_Model *mod, FILE *fin);
static int imodel_read(Imod *imod, int version);
static int imodel_write(struct Mod_Model *mod, FILE *fout);
static int imodel_write_object(Iobj *obj, FILE *fout, Ipoint *scale);
static int imodel_write_contour(Icont *cont, FILE *fout, Ipoint *scale);
static int imodel_write_mesh(Imesh *mesh, FILE *fout, Ipoint *scale);
static int imodel_read_header(Imod *imod, FILE *fin);
static int imodel_read_object(struct Mod_Object *obj, FILE *fin);
static int imodel_read_object_v01(struct Mod_Object *obj, FILE *fin);
static int imodel_read_contour(Icont *cont, FILE *fin);
static int imodel_read_contour_v01(Icont *cont, FILE *fin);
static int imodel_read_mesh(Imesh *mesh, FILE *fin);
static int imodel_read_clip(Iobj *obj, FILE *fin, b3dUInt32 flags);
static int imodel_read_imat(Iobj *obj, FILE *fin, b3dUInt32 flags);
static int imodel_read_ptsizes(Icont *cont, FILE *fin);

#ifdef IMOD_DATA_SWAP
static void byteswap(void *ptr, unsigned size);
#endif

#ifdef IMOD_FLOAT_CONVERT
static void tovmsfloat(unsigned char *ptr);
#endif

Imod *imodFileRead(char *filename)
{
  return(imodRead(filename));
}

int  imodFileWrite(Imod *imod, char *filename)
{
  FILE *fout = fopen(filename, "wb");
  return(imodWrite(imod, fout));
}


int imodOpenFile(char *filename, char *mode, Imod *imod)
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

int imodCloseFile(Imod *imod)
{
  if (!imod)
    return(-1);
  if (!imod->file)
    return(-1);
  fclose(imod->file);
  return(0);
}

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

  imodDefault(imod);
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

Imod *imodRead(char *filename)
{
  FILE *fin;
  Imod *imod;
  int error;

  imod = imodNew();
  if (!imod)
    return(NULL);

  fin = fopen(filename, "rb");
  if (!fin){
    imodFree(imod);
    return(NULL);
  }
  imod->file = fin;
     
  error = imodReadFile(imod);
  if (error){
#ifdef IMODEL_FILES_DEBUG
    b3dError(stderr, "imodel_files.c error %d\n", error);
#endif
    imodFree(imod);
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

int imodWriteFile(Imod *imod)
{
  if (!imod)
    return(-1);
  if (!imod->file)
    return(-1);
  return(imodel_write(imod, imod->file));
}

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

static int imodel_write(struct Mod_Model *mod, FILE *fout)
{
  int i, error;
  unsigned int id;
  int count;
  Ipoint scale;

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
     
#ifndef IMODEL_FILES_VERSION_01
  mod->file = fout;
  imodViewModelWrite(mod);
  imodIMNXWrite(mod);

  id = ID_IEOF;
  imodPutInt(fout,  &id);
#endif
     
  fflush(fout);
  return(ferror(fout));
}

static int imodel_write_object(struct Mod_Object *obj, FILE *fout,
                               Ipoint *scale)
{
  int i, error;
  int id;
  b3dUByte clipOut;
  IclipPlanes *clips;

  id = ID_OBJT;
  imodPutInt(fout, &id);
  imodPutBytes(fout, (unsigned char *)obj->name, IOBJ_STRSIZE);
  imodPutInts(fout, &obj->extra, IOBJ_EXSIZE + 4);
  imodPutFloats(fout, &obj->red, 3);
  imodPutInt(fout, &obj->pdrawsize);
  imodPutBytes(fout, &obj->symbol, 8);
  imodPutInt(fout, &obj->meshsize);

#ifndef IMODEL_FILES_VERSION_01
  imodPutInt(fout, &obj->surfsize);
#endif        

  if (obj->label)
    imodLabelWrite(obj->label, ID_OLBL, fout);

  for(i = 0; i < obj->contsize; i++)
    if ((error = imodel_write_contour( &(obj->cont[i]), fout, scale)))
      return(error);

  for(i = 0; i < obj->meshsize; i++)
    if (imodel_write_mesh( &(obj->mesh[i]), fout, scale))
      return(-1);

  /* due to a bug, version 2 readers can't read extra data. 
   * wait for later version to come out before adding data. 
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
      clipOut = clips->count;
      if (clipOut == 1 && (clips->flags & 1) == 0)
        clipOut = 0;
      imodPutBytes(fout, &clipOut, 1);
      imodPutBytes(fout, &clips->flags, 3);
      imodPutFloats(fout, (float *)&clips->normal[0], 3 * clips->count);
      imodPutFloats(fout, (float *)&clips->point[0], 3 * clips->count);
    }
	  
    /* Matierial data. */
    id = ID_IMAT;
    imodPutInt(fout, &id);
    id =  SIZE_IMAT;
    imodPutInt(fout, &id);
    imodPutBytes(fout, &obj->ambient, 4);

    /* DNM 9/4/02: write mat1 and mat3 as bytes */
    imodPutBytes(fout, (unsigned char *)&obj->mat1, 4);
    imodPutInts(fout, (int *)&obj->mat2, 1);
    imodPutBytes(fout, (unsigned char *)&obj->mat3, 4);
  }
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
#ifdef IMODEL_FILES_VERSION_01
  id = ID_PNTS; 
  imodPutInt(fout, &id);
#endif
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

  return(ferror(fout));
}

static int imodel_write_mesh(Imesh *mesh, FILE *fout, Ipoint *scale)
{
  int id = ID_MESH;
  short temp = mesh->type;
  int i;

  if (!mesh) return(0);
  imodPutInt(fout, &id);
  imodPutInts(fout, &mesh->vsize, 3);

  /* DNM 6/20/01: changed to put out type and pad as two shorts */
  imodPutShort(fout, &temp);
  temp = mesh->pad;
  imodPutShort(fout, &temp);
  if (scale->x == 1.0 && scale->y == 1.0 && scale->z == 1.0) {
    imodPutFloats(fout, (float *)mesh->vert, mesh->vsize * 3);
  } else {
    for (i = 0; i < mesh->vsize; i += 2)
      imodPutScaledPoints(fout, &mesh->vert[i], 1, scale);
  }
  imodPutInts(fout, mesh->list, mesh->lsize);
  return(ferror(fout));;
}


/*****************************************************************************/
/* Load Functions                                                            */
/*****************************************************************************/
static int imodel_read_v01(struct Mod_Model *mod, FILE *fin)
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

    default:
#ifdef IMODEL_FILES_DEBUG
      fprintf(stderr, "imodel_read: unknown data. %x, %s\n", 
              id, &id);
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
  return(0);
}


static int imodel_read_header(Imod *imod, FILE *fin)
{
  double tst;

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
    return(IMOD_ERROR_READ);
#ifdef IMODEL_FILES_DEBUG
    perror("error in imodel_read_header: ");
#endif
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



static int imodel_read_object_v01(struct Mod_Object *obj, FILE *fin)
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


static int imodel_read_contour_v01( struct Mod_Contour *cont, FILE *fin)
{
  int i;
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
  int i, error = 1;

  cont->psize = imodGetInt(fin);
  cont->flags = imodGetInt(fin);
  cont->type  = imodGetInt(fin);
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

static int imodel_read_mesh( struct Mod_Mesh *mesh, FILE *fin)
{
  int error = 0;

  mesh->vsize = imodGetInt(fin);
  mesh->lsize = imodGetInt(fin);
  mesh->flag  = imodGetInt(fin);

  /* DNM 6/20/01: start reading pad also, but read both type and pad as
     shorts.  This is OK because both type and pad have been zero before
     now, so there is no need to read old models with non-zero types */
  mesh->type  = imodGetShort(fin);
  mesh->pad   = imodGetShort(fin);
     
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
    imodGetBytes(fin, (unsigned char *)&obj->mat1, 4);     
    imodGetInts(fin, (int *)&obj->mat2, 1);     
    imodGetBytes(fin, (unsigned char *)&obj->mat3, 4);
  } else
    imodGetInts(fin, (int *)&obj->mat1, 3);     
  return 0;
}

/***************************************************************************/

int imodReadAscii(Imod *imod)
{
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
  int i, ob, co, surf, pt;
  int conts, meshes, pts;
  int mh, vsize, lsize;
  char line[MAXLINE];
  int len, idata;

#ifdef IMODEL_FILES_DEBUG
  fprintf(stderr, "#imodReadAscii: Entry\n");
#endif
  imod->cindex.object = -1;
  imod->cindex.contour = -1;
  imod->cindex.point = -1;

  len = imodFgetline(imod->file,line, MAXLINE);
  if (len < 1) return(-1);
  if (!substr(line, "imod"))
    return -1;

  sscanf(line, "imod %d", &(imod->objsize));
  imod->obj = imodObjectsNew(imod->objsize);
  if (imod->objsize > 0)
    imod->cindex.object = 0;
     
  obj = imod->obj;

  while ( ((len = imodFgetline(imod->file,line, MAXLINE)) > 0)){
	  
    if (substr(line, "contour")){
      sscanf(line, "contour %d %d %d", &co, &surf, &pts);
      cont = &(obj->cont[co]);
      cont->surf = surf;
      cont->psize = pts;
      if (pts)
        cont->pts = (Ipoint *)malloc(pts * sizeof(Ipoint));
      cont->flags = 0;
      cont->type  = 0;
      for(pt = 0; pt < pts; pt++){
        imodFgetline(imod->file, line, MAXLINE);
        sscanf(line, "%g %g %g", 
			   &(cont->pts[pt].x), 
			   &(cont->pts[pt].y), 
			   &(cont->pts[pt].z));
      }
      continue;
    }


    if (substr(line, "mesh")){
      sscanf(line, "mesh %d %d %d", &mh, &vsize, &lsize);
      mesh = &(obj->mesh[mh]);
      mesh->flag = mesh->type = mesh->pad = 0;
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
      continue;
    }

    if (substr(line, "color ")){
      sscanf(line, "color %g %g %g %d",
             &(obj->red), &(obj->green), &(obj->blue), &idata);
      obj->trans = idata;
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

    if (substr(line, "offsets "))
      sscanf(line, "offsets %g %g %g",
             &(imod->xoffset), &(imod->yoffset), &(imod->zoffset));

    if (substr(line, "angles "))
      sscanf(line, "angles %g %g %g",
             &(imod->alpha), &(imod->beta), &(imod->gamma));

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

    if (substr(line, "units")){
      if (strstr(line, "mm"))
        imod->units = IMOD_UNIT_MM;
      if (strstr(line, "um"))
        imod->units = IMOD_UNIT_UM;
      if (strstr(line, "nm"))
        imod->units = IMOD_UNIT_NM;
    }
	  
    if (substr(line, "name"))
      for(i = 0; i < strlen(line) - 5; i++)
        obj->name[i] = line[i + 5];

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
  }
     
  return(0);
}


int imodWriteAscii(Imod *imod)
{
  int ob, co, pt;
  int mh, i;
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;

  rewind(imod->file);
  fprintf(imod->file, "# imod ascii file version 1.0\n\n");
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

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    fprintf(imod->file, "\nobject %d %d %d\n", ob, obj->contsize,
            obj->meshsize);
    fprintf(imod->file, "name %s\n", obj->name);
    fprintf(imod->file, "color %g %g %g %d\n",
            obj->red, obj->green, obj->blue, obj->trans);
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

    fprintf(imod->file, "linewidth %d\n", obj->linewidth);
    fprintf(imod->file, "surfsize  %d\n", obj->surfsize);
    fprintf(imod->file, "pointsize %d\n", obj->pdrawsize);
    fprintf(imod->file, "axis      %d\n", obj->axis);
    fprintf(imod->file, "drawmode  %d\n", obj->drawmode);
	  

    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      fprintf(imod->file, "contour %d %d %d\n",
              co, cont->surf, cont->psize);
      for(pt = 0; pt < cont->psize; pt++){
        fprintf(imod->file, "%g %g %g\n",
			    cont->pts[pt].x, cont->pts[pt].y, cont->pts[pt].z);
      }
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
	       
    }
  }

  fprintf(imod->file, "# thats all folks!\n");
  return(0);
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
  int           len;
     
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
  int i;
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
