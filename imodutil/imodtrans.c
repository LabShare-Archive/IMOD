/*
 *  imodtrans.c -- Translate, scale and rotate imod model files.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "mrcfiles.h"
#include "parse_params.h"


static int filetrans(char *filename, Imod *model, int mode, int oneLine,
                     Ipoint newCen, float zscale, int doflip, float transScale, 
                     float transx, float transy, float transz);
static int trans_model_slice(Imod *model, float *mat, int slice,
                             Ipoint newCen);
static void usage(char *progname);


static void usage(char *progname)
{
  imodVersion(progname);
  imodCopyright();
  printf("Usage: %s [options] <input file> <output file>\n", 
          progname);
  printf("Options:\n");
  printf("\t-tx #\tTranslate in X by #\n");
  printf("\t-ty #\tTranslate in Y by #\n");
  printf("\t-tz #\tTranslate in Z by #\n");
  printf("\t-sx #\tScale in X by #\n");
  printf("\t-sy #\tScale in Y by #\n");
  printf("\t-sz #\tScale in Z by #\n");
  printf("\t-rx #\tRotate around X axis by #\n");
  printf("\t-ry #\tRotate around Y axis by #\n");
  printf("\t-rz #\tRotate around Z axis by #\n");
  printf("\t-2 file\tApply 2D transformations from file, one per"
          " section\n");
  printf("\t-l #\tLine number of single 2D transformation to apply"
          " (from 0)\n");
  printf("\t-3 file\tApply 3D transformation from file\n");
  printf("\t-S #\tScale dx/dy[/dz] values in 2D/3D transformations by #\n");
  printf("\t-n #,#,#\tSet X,Y,Z size of transformed volume\n");
  printf("\t-z\tTransform Z-scaled coordinates using model's Z-scale"
          "\n");
  printf("\t-f\tTransform flipped instead of native coordinates if "
          "Y-Z flipped\n");
  printf("\t-i file\tTransform to match given image file coordinate"
          " system\n");
  printf("\t-Y\tFlip model in Y and Z (without toggling flipped"
          " flag)\n");
  printf("\t-T\tToggle flag that model is flipped in Y and Z\n");

  exit(3);
}

int main(int argc, char *argv[])
{
  Imod   model;
  FILE   *fin, *fout;
  char   *filename = NULL;
  int    i;
  int    mode = 0;
  int    useZscale = 0;
  int    doflip = 1;
  int    transopt = 0;
  int    rotScaleopt = 0;
  int    toImage = 0;
  int    oneLine = -1;
  int    toggleFlip = 0;
  int    flipModel = 0;
  float  zscale = 1.;
  float transScale = 1.;
  float  rx = 0., ry = 0., rz = 0.;
  float transx = 0., transy = 0., transz = 0.;
  int multiTrans = 0;
  int newNx = 0, newNy = 0, newNz = 0;
  char *progname = imodProgName(argv[0]);
  Ipoint newCen, tmpPt;
  Imat *mat = imodMatNew(3);
  Imat *normMat = imodMatNew(3);
  IrefImage useRef, *modRefp;
  MrcHeader hdata;
  Ipoint unitPt = {1., 1., 1.};
  char prefix[100];
  sprintf(prefix, "ERROR: %s - ", progname);
  setExitPrefix(prefix);

  if (argc < 3){
    usage(progname);
  }
     
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {

      case '2':
        filename = argv[++i];
        mode += 2;
        break;
            
      case '3':
        filename = argv[++i];
        mode += 3;
        break;

      case 'S':
        sscanf(argv[++i], "%f", &transScale);
        break;

      case 'i':
        toImage = 1;
        if (NULL == (fin = fopen(argv[++i], "rb")))
          exitError("Couldn't open %s", argv[i]);

        if (mrc_head_read(fin, &hdata)) 
          exitError("Reading header from %s", argv[i]);
        fclose(fin);

      case 'z':
        useZscale = 1;
        break;

      case 'f':
        doflip = 0;
        break;

      case 'Y':
        flipModel = 1;
        break;

      case 'T':
        toggleFlip = 1;
        break;

      case 'n':
        sscanf(argv[++i], "%d%*c%d%*c%d", &newNx, &newNy, &newNz);
        break;
            
      case 'l':
        oneLine = atoi(argv[++i]);
        break;

      case 't':  /* Translations */
        tmpPt.x = tmpPt.y = tmpPt.z = 0.;
        transopt = 1;
        switch (argv[i][2]) {
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tx%f", &tmpPt.x);
          else
            sscanf(argv[++i], "%f", &tmpPt.x);
          if (transx)
            multiTrans = 1;
          transx = tmpPt.x;
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ty%f", &tmpPt.y);
          else
            sscanf(argv[++i], "%f", &tmpPt.y);
          if (transy)
            multiTrans = 1;
          transy = tmpPt.y;
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tz%f", &tmpPt.z);
          else
            sscanf(argv[++i], "%f", &tmpPt.z);
          if (transz)
            multiTrans = 1;
          transz = tmpPt.z;
          break;
        default:
          exitError("Invalid option %s", argv[i]);
        }
        imodMatTrans(mat, &tmpPt);
        break;

      case 's':  /* Scaling */
        tmpPt.x = tmpPt.y = tmpPt.z = 1.;
        transopt = 1;
        rotScaleopt = 1;
        switch (argv[i][2]) {
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
          exitError("Invalid option %s", argv[i]);
        }
        imodMatScale(mat, &tmpPt);
        tmpPt.x = 1. / tmpPt.x;
        tmpPt.y = 1. / tmpPt.y;
        tmpPt.z = 1. / tmpPt.z;
        imodMatScale(normMat, &tmpPt);
        break;

      case 'r':  /* Rotations */
        transopt = 1;
        rotScaleopt = 1;
        switch (argv[i][2]) {
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rx%f", &rx);
          else
            sscanf(argv[++i], "%f", &rx);
          imodMatRot(mat, (double)rx, b3dX);
          imodMatRot(normMat, (double)rx, b3dX);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ry%f", &ry);
          else
            sscanf(argv[++i], "%f", &ry);
          imodMatRot(mat, (double)ry, b3dY);
          imodMatRot(normMat, (double)ry, b3dY);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rz%f", &rz);
          else
            sscanf(argv[++i], "%f", &rz);
          imodMatRot(mat, (double)rz, b3dZ);
          imodMatRot(normMat, (double)rz, b3dZ);
          break;
        default:
          exitError("Invalid option %s", argv[i]);
        }
        break;

      default:
        exitError("Invalid option %s",  argv[i]);
      }

    }else{
      break;
    }
  }

  if (mode && mode / 2 != 1) 
    exitError("You cannot enter both -2 and -3");
  
  if (mode && rotScaleopt)  
    exitError("You cannot enter -r or -s options with -2 and -3");
  if (transopt && mode == 2 && oneLine < 1 && transz)
    exitError("You cannot enter -tz option with -2 unless transforming with one line");
  if (mode && multiTrans)
    exitError("You cannot enter -t options more than once with -2 and -3");
  
  if (i != argc - 2) {
    printf("ERROR: %s - Command line should end with two non-option "
            "arguments\n", progname);
    usage(progname);
  }

  fin = fopen(argv[i], "rb");
  if (!fin)
    exitError("Opening input file %s", argv[i]);
      
  imodDefault(&model);
  model.file = fin;
  if (imodReadFile(&model))
    exitError("Reading imod model (%s)", argv[i]);
  fclose(fin);

  if (imodBackupFile(argv[i + 1])) 
    exitError("Couldn't create backup file");

  fout = fopen(argv[i + 1], "wb");
  if (!fout)
    exitError("Opening output file %s", argv[i + 1]);

  /* Do flipping operations first */
  if (flipModel)
    imodFlipYZ(&model);
  if (toggleFlip) {
    if (model.flags & IMODF_FLIPYZ)
      model.flags &= ~IMODF_FLIPYZ;
    else
      model.flags |= IMODF_FLIPYZ;
  }

  /* Do scaling to image reference next */
  if (toImage) {

    modRefp = model.refImage;
    if (modRefp && !(model.flags & IMODF_OTRANS_ORIGIN)) 
      exitError("Model has no image origin information; -i option invalid");

    /* get the target transformation */
    useRef.ctrans.x = hdata.xorg;
    useRef.ctrans.y = hdata.yorg;
    useRef.ctrans.z = hdata.zorg;
    useRef.crot.x = hdata.tiltangles[3];
    useRef.crot.y = hdata.tiltangles[4];
    useRef.crot.z = hdata.tiltangles[5];
    useRef.cscale = unitPt;
    if (hdata.xlen && hdata.mx)
      useRef.cscale.x = hdata.xlen/(float)hdata.mx;
    if (hdata.ylen && hdata.my)
      useRef.cscale.y = hdata.ylen/(float)hdata.my;
    if (hdata.zlen && hdata.mz)
      useRef.cscale.z = hdata.zlen/(float)hdata.mz;
    
    if (modRefp) {

      /* If there is a refImage, scale the data as when reading into 3dmod */
      useRef.otrans = modRefp->ctrans;
      useRef.orot = modRefp->crot;
      useRef.oscale = modRefp->cscale;
      imodTransFromRefImage(&model, &useRef, unitPt);
    } else {

      /* If there is no refImage, do not scale data but assign all the information
         just as if reading into 3dmod */
      model.refImage = (IrefImage *)malloc (sizeof(IrefImage));
      modRefp = model.refImage;
      if (!modRefp) 
        exitError("Allocating a IrefImage structure");
      model.flags |= IMODF_OTRANS_ORIGIN;
    }
    *modRefp = useRef;
    modRefp->otrans = useRef.ctrans;

    /* Adjust the maxes for this change */
    model.xmax = hdata.nx;
    model.ymax = hdata.ny;
    model.zmax = hdata.nz;
  }

  /* Use zscale if user indicated it */
  if (useZscale && model.zscale > 0.)
    zscale = model.zscale;

  /* Do flipping if model flipped and user did not turn it off */
  if (!(model.flags & IMODF_FLIPYZ))
    doflip = 0;

  /* Warning every time is too noxious!
  if (!newNx)
    printf("Assuming transformed images have same size as original "
            "images\n (use -n option to specify size of transformed "
            "images)\n");
  */
  newCen.x = (newNx ? newNx : model.xmax) * 0.5f;
  newCen.y = (newNy ? newNy : model.ymax) * 0.5f;
  newCen.z = (newNz ? newNz : model.zmax) * 0.5f;

  if (filename){
    if (filetrans(filename, &model, mode, oneLine, newCen, zscale, doflip, transScale,
                  transx, transy, transz)) 
      exitError("Transforming model.");

  } else if (transopt) {
    imodTransModel3D(&model, mat, normMat, newCen, zscale, doflip);
  }

  model.xmax = newNx ? newNx : model.xmax;
  model.ymax = newNy ? newNy : model.ymax;
  model.zmax = newNz ? newNz : model.zmax;
  imodWrite(&model, fout);
  exit(0);
}

/* 
 * Transform by 2D or 3D transforms from a file
 *  filename  has the name of the file with transforms
 *  model     is the model
 *  mode      is 2 for 2D, 3 for 3D transforms
 *  oneLine   is line number for 2D applied to whole model
 *  newCen    has the new center coordinates of the volume
 *  zscale    is the z scale factor, or 1 not to use any
 *  doflip    indicates transform native form of flipped data
 */
static int filetrans(char *filename, Imod *model, int mode, int oneLine,
                     Ipoint newCen, float zscale, int doflip, float transScale,
                     float transx, float transy, float transz)
{
  FILE *fin;
  char line[80];
  float mat[6];
  int k = 0;
  int nread;
  Imat *mat3d = imodMatNew(3);

  fin = fopen(filename, "r");

  if (!fin){
    printf("ERROR: Couldn't open %s\n", filename);
    return(-1);
  }
     
  if (mode == 2) {
    while (fgetline(fin, line, 80) >= 0){
      
      nread = sscanf(line, "%f %f %f %f %f %f", 
                     &(mat[0]), &(mat[1]), 
                     &(mat[2]), &(mat[3]),
                     &(mat[4]), &(mat[5]));
      if (nread <= 0)
        continue;
      mat[4] = mat[4] * transScale + transx;
      mat[5] = mat[5] * transScale + transy;
      if (oneLine < 0) {
        trans_model_slice(model, mat, k, newCen);
      } else if (k == oneLine) {
        mat3d->data[0] = mat[0];
        mat3d->data[4] = mat[1];
        mat3d->data[12] = mat[4];
        mat3d->data[1] = mat[2];
        mat3d->data[5] = mat[3];
        mat3d->data[13] = mat[5];
        mat3d->data[8] = 0.;
        mat3d->data[9] = 0.;
        mat3d->data[10] = 1.;
        mat3d->data[2] = 0.;
        mat3d->data[6] = 0.;
        mat3d->data[14] = 0.;
        mat3d->data[15] = 1.;
        imodTransModel3D(model, mat3d, NULL, newCen, zscale, doflip);
        return(0);
      }
      k++;
    }

    if (oneLine >= 0) {
      printf("ERROR: End of file or error before line %d of %s\n", oneLine, filename);
      return(-1);
    }

  } else {
    for (k = 0; k < 3; k++) {
      if (fgetline(fin, line, 80) <= 0) {
        printf("ERROR: Reading line %d from  %s\n", k, filename);
        return(-1);
      }
      sscanf(line, "%f %f %f %f", &mat3d->data[k], &mat3d->data[k + 4],
             &mat3d->data[k + 8], &mat3d->data[k + 12]);
      mat3d->data[k + 12] *= transScale;
    }
    mat3d->data[12] += transx;
    mat3d->data[13] += transy;
    mat3d->data[14] += transz;
    imodTransModel3D(model, mat3d, NULL, newCen, zscale, doflip);
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
  Iobj *obj;
  Icont *cont;
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

