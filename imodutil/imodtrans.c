/*  IMOD VERSION 2.10
 *
 *  imodtrans.c -- Translate, scale and rotate imod model files.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"

struct trans{
  float x;
  float y;
  float z;
  float xout;
  float yout;
  float zout;
  float xtrans;
  float ytrans;
  float ztrans;
  float xrot;
  float yrot;
  float zrot;
  float scale;
};

static int fgetline(FILE *fp, char s[],int limit);
static int filetrans(char *filename, struct Mod_Model *model, int mode);
static int trans_model_slice(struct Mod_Model *model, float *mat, int slice);

static void transform(struct trans *tr)
{
  float cosx, cosy, cosz;
  float sinx, siny, sinz;
  float x, y, z;

  cosx = acos(tr->xrot);
  cosy = acos(tr->yrot);
  cosz = acos(tr->zrot);
  sinx = asin(tr->xrot);
  siny = asin(tr->yrot);
  sinz = asin(tr->zrot);

  /* was x = x + tr->xtrans; ??? */
  x = tr->x + tr->xtrans;
  y = tr->y + tr->ytrans;
  z = tr->z + tr->ztrans;

  tr->xout = (x * ((cosx * cosy * cosz) - (sinx * sinz))) -
    (y * ((sinx * cosy * cosz) + (cosx * sinz))) +
    (z *   siny * cosy);

  tr->yout = (x * -((sinx * cosy * cosz) + (cosx * sinz))) +
    (y * (-(sinx * cosy * sinz) + (cosx * cosz))) +
    (z *    sinx * siny);

  tr->zout = (x * siny * cosz) +
    (y * siny * sinz) +
    (z * cosz);

  tr->xout *= tr->scale;
  tr->yout *= tr->scale;
  tr->zout *= tr->scale;
     
  return;
}


static int getint(char *prompt)
{
  int c;
  int done = 0;
  char line[80];
     
  while (!done){
    printf("%s (Enter number) > ", prompt);
    fflush(stdout);
    fgetline(stdin, line, 79);
    if (line[0])
      done = sscanf(line, "%d", &c);
  }
  return(c);
}


static double getdouble(char *prompt)
{
  float d;
  int done = 0;
  char line[80];

  while (!done){
    printf("%s > ", prompt);
    fflush(stdout);
    fgetline(stdin, line, 79);
    if (line[0])
      done = sscanf(line, "%f", &d);
  }
  return((double)d);
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

int main(int argc, char *argv[])
{
  struct Mod_Model   model;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  struct Mod_Point   pnt;
  struct trans tr;
  FILE   *fin, *fout;
  char   *filename = NULL;
  int    ob, co, pt;
  int    i;
  int    mode = 0;
  float  x, y, z;
  float  tx = 0, ty = 0, tz = 0;
  float  sx = 1, sy = 1, sz = 1;
  float  rx = 0, ry = 0, rz = 0;
  float  rot, rotx = 0, roty = 0, rotz = 0;
  float  sinx,cosx,siny,cosy,sinz,cosz;
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    imodVersion(progname);
    imodCopyright();
    fprintf(stderr, "%s: Usage\n", progname);
    fprintf(stderr, 
            "%s <input file> <output file> [-2 filename] [-tx #] [-ty #] [-tz #] [-sx #] [-sy #] [-sz #] [-rx #] [-ry #] [-rz #]\n", progname);
    fprintf(stderr, "Where -tx, -ty & -tz translate x y & z\n");
    fprintf(stderr, "Where -sx, -sy & -sz scale     x y & z\n");
    fprintf(stderr, "Where -rx, -ry & -rz rotate    x y & z\n");
    fprintf(stderr, "Where -2 filename is a transformation file for a 2-d stack transformation.\n");
    exit(3);
  }
     
  fin = fopen(argv[1], "rb");
  if (!fin){
    fprintf(stderr, "%s: error reading input file %s\n",
            progname, argv[1]);
    exit(3);
  }
      
  fout = fopen(argv[2], "wb");
  if (!fin){
    fprintf(stderr, "%s: error opening output file %s\n",
            progname, argv[2]);
    exit(3);
  }

  model.file = fin;
  if (imodReadFile(&model)){
    fprintf(stderr, "%s: Error reading imod model. (%s)\n", 
            progname, argv[1]);
    exit(3);
  }

  for (i = 3; i < argc; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){

      case '2':
        filename = argv[++i];
        mode = 2;
        break;
            
      case 't':
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tx%f", &tx);
          else
            sscanf(argv[++i], "%f", &tx);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ty%f", &ty);
          else
            sscanf(argv[++i], "%f", &ty);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-tz%f", &tz);
          else
            sscanf(argv[++i], "%f", &tz);
          break;
        default:
          fprintf(stderr, "%s: Invalid option %s\n",
                  progname, argv[i]);
          exit(3);
          break;
        }
        break;

      case 's':
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sx%f", &sx);
          else
            sscanf(argv[++i], "%f", &sx);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sy%f", &sy);
          else
            sscanf(argv[++i], "%f", &sy);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-sz%f", &sz);
          else
            sscanf(argv[++i], "%f", &sz);
          break;
        default:
          fprintf(stderr, "%s: Invalid option %s\n",
                  progname, argv[i]);
          exit(3);
          break;
        }
        break;

      case 'r':
        switch (argv[i][2]){
        case 'x':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rx%f", &rx);
          else
            sscanf(argv[++i], "%f", &rx);
          break;
        case 'y':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-ry%f", &ry);
          else
            sscanf(argv[++i], "%f", &ry);
          break;
        case 'z':
          if (argv[i][3] != 0x00)
            sscanf(argv[i], "-rz%f", &rz);
          else
            sscanf(argv[++i], "%f", &rz);
          break;
        default:
          fprintf(stderr, "%s: Invalid option %s\n",
                  progname, argv[i]);
          exit(3);
          break;
        }
        break;

      default:
        fprintf(stderr, "%s: Invalid option %s\n",
                progname, argv[i]);
        exit(3);
        break;
      }

    }else{
      fprintf(stderr, "%s: Invalid option %s\n",
              progname, argv[i]);
      exit(3);
    }
  }

  if (filename){
    if (filetrans(filename, &model, mode)){
      fprintf(stderr, "%s: Error transforming model.\n", progname);
      exit(3);
    }
    imodWrite(&model, fout);
    exit(0);
  }
     
  rx *= 0.017453293;
  ry *= 0.017453293;
  rz *= 0.017453293;

  sinx = sin(rx);
  siny = sin(ry);
  sinz = sin(rz);
  cosx = cos(rx);
  cosy = cos(ry);
  cosz = cos(rz);

  for(ob = 0; ob < model.objsize; ob++){
    obj = &(model.obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);

      for(pt = 0; pt < cont->psize; pt++){
        /* translate */
        pnt.x = cont->pts[pt].x + tx;
        pnt.y = cont->pts[pt].y + ty;
        pnt.z = cont->pts[pt].z + tz;
            
        /* scale */
        x = pnt.x * sx;
        y = pnt.y * sy;
        z = pnt.z * sz;
            
        /* rotate x */
        pnt.x = x;
        pnt.y = (y * cosx) - (z * sinx);
        pnt.z = (y * sinx) + (z * cosx);
        x = pnt.x;
        y = pnt.y;
        z = pnt.z;

        /* rotate y */
        pnt.x = (x * cosy) + (z * siny);
        pnt.y = y;
        pnt.z = (z * cosy) - (x * siny);
        x = pnt.x;
        y = pnt.y;
        z = pnt.z;
            
        /* rotate z */
        cont->pts[pt].x = (x * cosz) - (y * sinz);
        cont->pts[pt].y = (x * sinz) + (y * cosz);
        cont->pts[pt].z = z;
      }
    }
  }

  imodWrite(&model, fout);
  exit(0);
}


static int filetrans(char *filename, Imod *model, int mode)
{
  FILE *fin;
  char line[80];
  float mat[6];
  int k = 0;
  float xc, yc, dx, dy;

  fin = fopen(filename, "r");

  if (!fin){
    fprintf(stderr, "Couldn't open %s\n", filename);
    return(-1);
  }
     
  while (fgetline(fin, line, 80)){

    sscanf(line, "%f %f %f %f %f %f", 
           &(mat[0]), &(mat[1]), 
           &(mat[2]), &(mat[3]),
           &(mat[4]), &(mat[5]));
    trans_model_slice(model, mat, k);
    k++;
  }
  return(0);
}

static int trans_model_slice(struct Mod_Model *model, float *mat, int slice)
{
  int ob, co, pt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int zval;
  float x, y;

  float xcen = model->xmax * 0.5f;
  float ycen = model->ymax * 0.5f;

  for(ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
        zval = (cont->pts[pt].z + 0.5f);
        if (zval == slice){
          x = cont->pts[pt].x;
          y = cont->pts[pt].y;
          cont->pts[pt].x = 
            mat[0] * (x - xcen) +
            mat[1] * (y - ycen) +
            mat[4] + xcen;
          cont->pts[pt].y =
            mat[2] * (x - xcen) +
            mat[3] * (y - ycen) +
            mat[5] + ycen;
        }
      }
    }
  }
  return(0);
}




