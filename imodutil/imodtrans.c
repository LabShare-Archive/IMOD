/*  IMOD VERSION 3.4.8
 *
 *  imodtrans.c -- Translate, scale and rotate imod model files.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Fine    *
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
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"


static int fgetline(FILE *fp, char s[],int limit);
static int filetrans(char *filename, Imod *model, int mode, Ipoint newCen,
                     float zscale, int doflip);
static int trans_model_slice(struct Mod_Model *model, float *mat, int slice,
                             Ipoint newCen);
static int trans_model_3d(Imod *model, Imat *mat, Ipoint newCen, float zscale,
                          int doflip);


static void usage(char *progname)
{
  imodVersion(progname);
  imodCopyright();
  fprintf(stderr, "Usage: %s [options] <input file> <output file>\n", progname);
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "\t-tx #\tTranslate in X by #\n");
  fprintf(stderr, "\t-ty #\tTranslate in Y by #\n");
  fprintf(stderr, "\t-tz #\tTranslate in Z by #\n");
  fprintf(stderr, "\t-sx #\tScale in X by #\n");
  fprintf(stderr, "\t-sy #\tScale in Y by #\n");
  fprintf(stderr, "\t-sz #\tScale in Z by #\n");
  fprintf(stderr, "\t-rx #\tRotate around X axis by #\n");
  fprintf(stderr, "\t-ry #\tRotate around Y axis by #\n");
  fprintf(stderr, "\t-rz #\tRotate around Z axis by #\n");
  fprintf(stderr, "\t-2 file\tApply 2D transformations from file, one per"
          " section\n");
  fprintf(stderr, "\t-3 file\tApply 3D transformation from file\n");
  fprintf(stderr, "\t-n #,#,#\tSet X,Y,Z size of transformed volume\n");
  fprintf(stderr, "\t-z\tTransform Z-scaled coordinates using model's Z-scale"
          "\n");
  fprintf(stderr, "\t-f\tTransform flipped instead of native coordinates if Y-Z"
          " flipped\n");
  exit(3);
}

int main(int argc, char *argv[])
{
  struct Mod_Model   model;
  FILE   *fin, *fout;
  char   *filename = NULL;
  int    i;
  int    mode = 0;
  int    useZscale = 0;
  int    doflip = 1;
  int    transopt = 0;
  float  zscale = 1.;
  float  rx = 0., ry = 0., rz = 0.;
  int newNx = 0, newNy = 0, newNz = 0;
  char *progname = imodProgName(argv[0]);
  Ipoint newCen, tmpPt;
  Imat *mat = imodMatNew(3);

  if (argc < 3){
    usage(progname);
  }
     

  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){

      case '2':
        filename = argv[++i];
        mode += 2;
        break;
            
      case '3':
        filename = argv[++i];
        mode += 3;
        break;

      case 'z':
        useZscale = 1;
        break;

      case 'f':
        doflip = 0;
        break;

      case 'n':
        sscanf(argv[++i], "%d%*c%d%*c%d", &newNx, &newNy, &newNz);
        break;
            
      case 't':
        tmpPt.x = tmpPt.y = tmpPt.z = 0.;
        transopt = 1;
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tx%f", &tmpPt.x);
          else
            sscanf(argv[++i], "%f", &tmpPt.x);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ty%f", &tmpPt.y);
          else
            sscanf(argv[++i], "%f", &tmpPt.y);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tz%f", &tmpPt.z);
          else
            sscanf(argv[++i], "%f", &tmpPt.z);
          break;
        default:
          fprintf(stderr, "ERROR: %s - Invalid option %s\n", progname, 
                  argv[i]);
          exit(3);
          break;
        }
        imodMatTrans(mat, &tmpPt);
        break;

      case 's':
        tmpPt.x = tmpPt.y = tmpPt.z = 1.;
        transopt = 1;
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sx%f", &tmpPt.x);
          else
            sscanf(argv[++i], "%f", &tmpPt.x);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sy%f", &tmpPt.y);
          else
            sscanf(argv[++i], "%f", &tmpPt.y);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sz%f", &tmpPt.z);
          else
            sscanf(argv[++i], "%f", &tmpPt.z);
          break;
        default:
          fprintf(stderr, "ERROR: %s - Invalid option %s\n", progname, 
                  argv[i]);
          exit(3);
          break;
        }
        imodMatScale(mat, &tmpPt);
        break;

      case 'r':
        transopt = 1;
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rx%f", &rx);
          else
            sscanf(argv[++i], "%f", &rx);
          imodMatRot(mat, (double)rx, b3dX);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ry%f", &ry);
          else
            sscanf(argv[++i], "%f", &ry);
          imodMatRot(mat, (double)ry, b3dY);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rz%f", &rz);
          else
            sscanf(argv[++i], "%f", &rz);
          imodMatRot(mat, (double)rz, b3dZ);
          break;
        default:
          fprintf(stderr, "ERROR: %s - Invalid option %s\n", progname, 
                  argv[i]);
          exit(3);
          break;
        }
        break;

      default:
        fprintf(stderr, "ERROR: %s - Invalid option %s\n", progname, argv[i]);
        exit(3);
        break;
      }

    }else{
      break;
    }
  }

  if (mode && mode / 2 != 1) {
    fprintf(stderr, "ERROR: %s - You cannot enter both -2 and -3\n", progname);
    exit(3);
  }

  if (mode && transopt) { 
    fprintf(stderr, "ERROR: %s - You cannot enter -r, -s, or -t options with "
            "-2 and -3\n", progname);
    exit(3);
  }

  if (i != argc - 2) {
    fprintf(stderr, "ERROR: %s - Command line should end with two non-option "
            "arguments\n", progname);
    usage(progname);
  }

  fin = fopen(argv[i], "rb");
  if (!fin){
    fprintf(stderr, "ERROR: %s - error opening input file %s\n", progname, 
            argv[i]);
    exit(3);
  }
      
  if (imodBackupFile(argv[i + 1])) {
    fprintf(stderr, "ERROR: %s - couldn't create backup file", progname);
    exit(3);
  }

  fout = fopen(argv[i + 1], "wb");
  if (!fin){
    fprintf(stderr, "ERROR: %s - error opening output file %s\n", progname, 
            argv[i + 1]);
    exit(3);
  }

  model.file = fin;
  if (imodReadFile(&model)){
    fprintf(stderr, "ERROR: %s - Error reading imod model. (%s)\n", progname, 
            argv[i]);
    exit(3);
  }

  /* Use zscale if user indicated it */
  if (useZscale && model.zscale > 0.)
    zscale = model.zscale;

  /* Do flipping if model flipped and user did not turn it off */
  if (!(model.flags & IMODF_FLIPYZ))
    doflip = 0;

  /* Warning every time is too noxious!
  if (!newNx)
    fprintf(stderr, "Assuming transformed images have same size as original "
            "images\n (use -n option to specify size of transformed "
            "images)\n");
  */
  newCen.x = (newNx ? newNx : model.xmax) * 0.5f;
  newCen.y = (newNy ? newNy : model.ymax) * 0.5f;
  newCen.z = (newNz ? newNz : model.zmax) * 0.5f;

  if (filename){
    if (filetrans(filename, &model, mode, newCen, zscale, doflip)) {
      fprintf(stderr, "ERROR: %s - Error transforming model.\n", progname);
      exit(3);
    }

  } else {
    trans_model_3d(&model, mat, newCen, zscale, doflip);
  }

  imodWrite(&model, fout);
  exit(0);
}

/* 
 * Transform by 2D or 3D transforms from a file
 *  filename  has the name of the file with transforms
 *  model     is the model
 *  mode      is 2 for 2D, 3 for 3D transforms
 *  newCen    has the new center coordinates of the volume
 *  zscale    is the z scale factor, or 1 not to use any
 */
static int filetrans(char *filename, Imod *model, int mode, Ipoint newCen,
                     float zscale, int doflip)
{
  FILE *fin;
  char line[80];
  float mat[6];
  int k = 0;
  float xc, yc, dx, dy;
  Imat *mat3d = imodMatNew(3);

  fin = fopen(filename, "r");

  if (!fin){
    fprintf(stderr, "ERROR: Couldn't open %s\n", filename);
    return(-1);
  }
     
  if (mode == 2) {
    while (fgetline(fin, line, 80)){
      
      sscanf(line, "%f %f %f %f %f %f", 
             &(mat[0]), &(mat[1]), 
             &(mat[2]), &(mat[3]),
             &(mat[4]), &(mat[5]));
      trans_model_slice(model, mat, k, newCen);
      k++;
    }
  } else {
    for (k = 0; k < 3; k++) {
      if (!fgetline(fin, line, 80)) {
        fprintf(stderr, "ERROR: Reading line %d from  %s\n", k, filename);
        return(-1);
      }
      sscanf(line, "%f %f %f %f", &mat3d->data[k], &mat3d->data[k + 4],
             &mat3d->data[k + 8], &mat3d->data[k + 12]);
    }
    trans_model_3d(model, mat3d, newCen, zscale, doflip);
  }
  return(0);
}

/*
 * Translate one slice of the model
 *  model  is the model 
 *  mat    is a 6-element matrix with transformation
 *  slice  is the slice number
 *  newCen has the new X and Y center coordinates
 */
static int trans_model_slice(Imod *model, float *mat, int slice, Ipoint newCen)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int zval;
  float x, y;

  float xcen = model->xmax * 0.5f;
  float ycen = model->ymax * 0.5f;

  for (ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++){
        zval = (cont->pts[pt].z + 0.5f);
        if (zval == slice){
          x = cont->pts[pt].x;
          y = cont->pts[pt].y;
          cont->pts[pt].x = 
            mat[0] * (x - xcen) +
            mat[1] * (y - ycen) +
            mat[4] + newCen.x;
          cont->pts[pt].y =
            mat[2] * (x - xcen) +
            mat[3] * (y - ycen) +
            mat[5] + newCen.y;
        }
      }
    }
  }
  return(0);
}

static void exchangef(float *a, float *b)
{
  float tmp = *a;
  *a = *b;
  *b = tmp;
}

/*
 * Transform the model with the given 3D matrix 
 *  model  is the model 
 *  mat    is a 3D matrix inclusing translations
 *  newCen are the center coordinates of the new volume
 *  zscale is the z scale factor, or 1 to not apply any 
 */
static int trans_model_3d(Imod *model, Imat *mat, Ipoint newCen, float zscale,
                          int doflip)
{
  int ob, co, pt, me;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  Imesh *mesh;
  Imat *matWork = imodMatNew(3);
  Imat *matUse = imodMatNew(3);
  Ipoint oldCen, tmpPt;
  oldCen.x = -model->xmax * 0.5f;
  oldCen.y = -model->ymax * 0.5f;
  oldCen.z = -model->zmax * 0.5f;

  /* If data are flipped, exchange Y and Z columns then Y and Z rows */
  if (doflip) {
    exchangef(&mat->data[4], &mat->data[8]);
    exchangef(&mat->data[1], &mat->data[2]);
    exchangef(&mat->data[6], &mat->data[9]);
    exchangef(&mat->data[5], &mat->data[10]);
    exchangef(&mat->data[13], &mat->data[14]);
  }

  /* Compute a transformation by translating to origin, applying the given
     transform, then translating back to new center.
     Apply the zscale after translating, then de-scale after applying the
     transform. */
  imodMatTrans(matWork, &oldCen);
  tmpPt.x = 1.;
  tmpPt.y = 1.;
  tmpPt.z = zscale;
  imodMatScale(matWork, &tmpPt);
  imodMatMult(matWork, mat, matUse);
  tmpPt.z = 1. / zscale;
  imodMatScale(matUse, &tmpPt);
  imodMatTrans(matUse, &newCen);

  /* Set up transform for normals as copy of original transform, no shifts*/
  imodMatCopy(mat, matWork);
  matWork->data[12] = 0.;
  matWork->data[13] = 0.;
  matWork->data[14] = 0.;
  matWork->data[15] = 0.;

  for (ob = 0; ob < model->objsize; ob++) {
    obj = &(model->obj[ob]);

    /* Transform points in contours */
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for (pt = 0; pt < cont->psize; pt++){
        imodMatTransform(matUse, &cont->pts[pt], &tmpPt);
        cont->pts[pt] = tmpPt;
      }
    }

    /* Transform points and normals in meshes */
    for (me = 0; me < obj->meshsize; me++) {
      mesh = &obj->mesh[me];
      for (pt = 0; pt < mesh->vsize; pt++) {
        imodMatTransform(matUse, &mesh->vert[pt], &tmpPt);
        mesh->vert[pt++] = tmpPt;
        imodMatTransform(matWork, &mesh->vert[pt], &tmpPt);
        imodPointNormalize(&tmpPt);
        mesh->vert[pt] = tmpPt;
      }
    }
  }
  return 0;
}

static int fgetline(FILE *fp, char s[],int limit)
{
  int c, i, length;
     
  for (i=0; (((c = getc(fp)) != EOF) &&(i < (limit-1)) && (c != '\n')); i++)
    s[i]=c;
     
  s[i]='\0';
  length = i;
  if (c == EOF) return (length - (2 * length));
  else return length;
}
