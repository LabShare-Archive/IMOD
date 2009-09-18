/*
 *  imodtrans.c -- Translate, scale and rotate imod model files.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*  $Author$

    $Date$

    $Revision$
    Log at end
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "mrcfiles.h"


static int filetrans(char *filename, Imod *model, int mode, int oneLine,
                     Ipoint newCen, float zscale, int doflip);
static int trans_model_slice(Imod *model, float *mat, int slice,
                             Ipoint newCen);
static int trans_model_3d(Imod *model, Imat *mat, Imat *normMat, Ipoint newCen,
                          float zscale, int doflip);
static void exchangef(float *a, float *b);
static void usage(char *progname);


static void usage(char *progname)
{
  imodVersion(progname);
  imodCopyright();
  fprintf(stderr, "Usage: %s [options] <input file> <output file>\n", 
          progname);
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
  fprintf(stderr, "\t-l #\tLine number of single 2D transformation to apply"
          " (from 0)\n");
  fprintf(stderr, "\t-3 file\tApply 3D transformation from file\n");
  fprintf(stderr, "\t-n #,#,#\tSet X,Y,Z size of transformed volume\n");
  fprintf(stderr, "\t-z\tTransform Z-scaled coordinates using model's Z-scale"
          "\n");
  fprintf(stderr, "\t-f\tTransform flipped instead of native coordinates if "
          "Y-Z flipped\n");
  fprintf(stderr, "\t-i file\tTransform to match given image file coordinate"
          " system\n");
  fprintf(stderr, "\t-Y\tFlip model in Y and Z (without toggling flipped"
          " flag)\n");
  fprintf(stderr, "\t-T\tToggle flag that model is flipped in Y and Z\n");

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
  int    toImage = 0;
  int    oneLine = -1;
  int    toggleFlip = 0;
  int    flipModel = 0;
  float  zscale = 1.;
  float  rx = 0., ry = 0., rz = 0.;
  int newNx = 0, newNy = 0, newNz = 0;
  char *progname = imodProgName(argv[0]);
  Ipoint newCen, tmpPt;
  Imat *mat = imodMatNew(3);
  Imat *normMat = imodMatNew(3);
  IrefImage useRef, *modRefp;
  MrcHeader hdata;
  Ipoint unitPt = {1., 1., 1.};

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

      case 'i':
        toImage = 1;
        if (NULL == (fin = fopen(argv[++i], "rb"))){
          fprintf(stderr, "ERROR: %s - Couldn't open %s\n", progname, 
                  argv[i]);
          exit(3);
        }

        if (mrc_head_read(fin, &hdata)) {
          fprintf(stderr, "ERROR: %s - Reading header from %s.\n", progname,
                  argv[i]);
          exit(3);
        }
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

      case 's':  /* Scaling */
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
        tmpPt.x = 1. / tmpPt.x;
        tmpPt.y = 1. / tmpPt.y;
        tmpPt.z = 1. / tmpPt.z;
        imodMatScale(normMat, &tmpPt);
        break;

      case 'r':  /* Rotations */
        transopt = 1;
        switch (argv[i][2]){
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

  imodDefault(&model);
  model.file = fin;
  if (imodReadFile(&model)){
    fprintf(stderr, "ERROR: %s - Error reading imod model. (%s)\n", progname, 
            argv[i]);
    exit(3);
  }

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

    if (!model.refImage) {
      fprintf(stderr, "ERROR: %s - Model has no image scaling information; "
              "-i option invalid\n", progname);
      exit(3);
    }

    if (!(model.flags & IMODF_OTRANS_ORIGIN)) {
      fprintf(stderr, "ERROR: %s - Model has no image origin information; "
              "-i option invalid\n", progname);
      exit(3);
    }

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
    
    modRefp = model.refImage;
    useRef.otrans = modRefp->ctrans;
    useRef.orot = modRefp->crot;
    useRef.oscale = modRefp->cscale;
    imodTransFromRefImage(&model, &useRef, unitPt);
    *modRefp = useRef;
    modRefp->otrans = useRef.ctrans;
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
    if (filetrans(filename, &model, mode, oneLine, newCen, zscale, doflip)) {
      fprintf(stderr, "ERROR: %s - Error transforming model.\n", progname);
      exit(3);
    }

  } else if (transopt) {
    trans_model_3d(&model, mat, normMat, newCen, zscale, doflip);
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
                     Ipoint newCen, float zscale, int doflip)
{
  FILE *fin;
  char line[80];
  float mat[6];
  int k = 0;
  int nread;
  float xc, yc, dx, dy;
  Imat *mat3d = imodMatNew(3);

  fin = fopen(filename, "r");

  if (!fin){
    fprintf(stderr, "ERROR: Couldn't open %s\n", filename);
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
        trans_model_3d(model, mat3d, NULL, newCen, zscale, doflip);
        return(0);
      }
      k++;
    }

    if (oneLine >= 0) {
      fprintf(stderr, "ERROR: imodtrans - End of file or error before line %d "
              "of %s\n", oneLine, filename);
      return(-1);
    }

  } else {
    for (k = 0; k < 3; k++) {
      if (fgetline(fin, line, 80) <= 0) {
        fprintf(stderr, "ERROR: imodtrans - Reading line %d from  %s\n", k,
                filename);
        return(-1);
      }
      sscanf(line, "%f %f %f %f", &mat3d->data[k], &mat3d->data[k + 4],
             &mat3d->data[k + 8], &mat3d->data[k + 12]);
    }
    trans_model_3d(model, mat3d, NULL, newCen, zscale, doflip);
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

static void exchangef(float *a, float *b)
{
  float tmp = *a;
  *a = *b;
  *b = tmp;
}

/*
 * Transform the model with the given 3D matrix 
 *  model  is the model 
 *  mat    is a 3D matrix including translations
 *  normMat is a 3D matrix for normal transformations, or NULL if none
 *  newCen are the center coordinates of the new volume
 *  zscale is the z scale factor, or 1 to not apply any 
 */
static int trans_model_3d(Imod *model, Imat *mat, Imat *normMat, Ipoint newCen,
                          float zscale, int doflip)
{
  int i, ob, co, pt, me;
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
  Imat *matWork = imodMatNew(3);
  Imat *matWork2 = imodMatNew(3);
  Imat *matUse = imodMatNew(3);
  Imat *clipMat = imodMatNew(3);
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
    if (normMat) {
      exchangef(&normMat->data[4], &normMat->data[8]);
      exchangef(&normMat->data[1], &normMat->data[2]);
      exchangef(&normMat->data[6], &normMat->data[9]);
      exchangef(&normMat->data[5], &normMat->data[10]);
      exchangef(&normMat->data[13], &normMat->data[14]);
    }
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

  /* If no normal transform supplied, set up transform for normals as copy of 
     original transform, no shifts, then take the inverse and transpose it. */
  if (!normMat) {
    imodMatCopy(mat, matWork);
    matWork->data[12] = 0.;
    matWork->data[13] = 0.;
    matWork->data[14] = 0.;
    matWork->data[15] = 1.;
    normMat = imodMatInverse(matWork);
    exchangef(&normMat->data[1], &normMat->data[4]);
    exchangef(&normMat->data[2], &normMat->data[8]);
    exchangef(&normMat->data[6], &normMat->data[9]);
  }

  /* The mesh normals already contain the Z scaling, but the clip normals 
     require a matrix that is prescaled by 1/zscale and post-scaled by 
     z-scale */
  imodMatScale(matWork2, &tmpPt);
  imodMatMult(matWork2, normMat, clipMat);
  tmpPt.z = zscale;
  imodMatScale(clipMat, &tmpPt);

  imodTransFromMats(model, matUse, normMat, clipMat);

  return 0;
}

/*
    $Log$
    Revision 3.8  2006/10/05 19:44:47  mast
    Set the model maxes if the -n optionis used

    Revision 3.7  2006/09/13 02:37:08  mast
    Call imodDefault since imodRead will not

    Revision 3.6  2005/10/31 20:09:35  mast
    Removed transformation code that had been turned into a function.

    Revision 3.5  2005/10/19 16:00:36  mast
    Needed to transpose inverse to apply normal matrix in usual direction

    Revision 3.4  2005/10/19 14:30:01  mast
    Fixed handling of normals, and added options for transforming whole volume
    with 2D transform, manipulating flip state, and transforming to match an
    image file

    Revision 3.3  2004/09/17 16:27:13  mast
    Rewrote to do a general 3D transformation from a file, to transform mesh
    vertices and normals, to set up a transformation matrix as it reads in
    option arguments, to take the arguments in any order and repeated if
    desired, and to handle situations of flipped data, Z-scaled data, and
    different destination image file size.

*/
