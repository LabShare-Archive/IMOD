/*
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.7  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.6  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2004/04/28 05:29:31  mast
Set flag to draw current contour thicker

Revision 3.4  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2002/12/23 21:32:55  mast
Fixed exit status and made residual model have end-markers set

Revision 3.2  2002/07/27 23:50:47  mast
Eliminated line for test output

Revision 3.1  2002/07/27 06:00:43  mast
Added ability to convert a residual listing from Tiltalign

*/

#include <stdio.h>
#include <string.h>

#include "imodel.h"


static int fgetline(FILE *fp, char s[],int limit);
struct Mod_Model *imod_from_patches(FILE *fin, float scale);

int main( int argc, char *argv[])
{
  int i;
  FILE *fin, *fout;
  struct Mod_Model *Model;
  float scale = 10.0;

  if (argc < 2){
          
    printf("patch2imod version 1.0 usage:\n");
    printf("patch2imod [-s scale] patch_file imod_model\n");
    printf("    Displacements are multiplied by \"scale\" (default %.1f)"
       " to make vectors.\n", scale);
    exit(1);

  }

  
  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
        
      case 's':
        sscanf(argv[++i], "%f", &scale);
        break;

      default:
        printf("ERROR: patch2imod - Illegal argument %s\n", argv[i]);
        exit(1);
        break;
      }
    } else
      break;
  }
  if (i > (argc - 2)){
    printf("ERROR: patch2imod - wrong # of arguments\n");
    printf("patch2imod version 1.0 usage:\n");
    printf("patch2imod [-s scale] patch_file imod_model\n");
    exit(1);
  }


  fin = fopen(argv[i++], "r");
  if (!fin){
    printf("ERROR: patch2imod - Couldn't open %s\n", argv[--i]);
    exit(3);
  }

  if (imodBackupFile(argv[i])) {
    printf( "ERROR: patch2imod - renaming existing output file "
            "to %s~\n", argv[i]);
    exit(1);
  }
  fout = fopen(argv[i], "wb");
  if (!fout){
    printf("ERROR: patch2imod - couldn't open %s\n", argv[i]);
    exit(3);
  }
  Model = (struct Mod_Model *)imod_from_patches(fin, scale);
     
  imodWrite(Model, fout);

  imodDelete(Model);
  exit(0);
}





#define MAXLINE 128

struct Mod_Model *imod_from_patches(FILE *fin, float scale)
{
  int len;
  int i, npatch;
     
  char line[MAXLINE];
  struct Mod_Model *mod;
  Ipoint *pts;
  int ix, iy, iz;
  int xmin, ymin, zmin, xmax, ymax, zmax;
  float dx, dy, dz, xx, yy;
  int residuals = 0;

  fgetline(fin,line,MAXLINE);
  sscanf(line, "%d", &npatch);
  if (npatch < 1) {
    fprintf(stderr, "Error - implausible number of patches = %d.\n",
        npatch);
    exit(1);
  }

  if (strstr(line, "residuals") != NULL)
    residuals = 1;

  mod = imodNew();     
  if (!mod){
    fprintf(stderr, "Couldn't get new model\n");
    return(NULL);
  }
  imodNewObject(mod);
  mod->obj->contsize = npatch;
  mod->obj->cont = imodContoursNew(npatch);
  mod->obj->flags |= IMOD_OBJFLAG_OPEN;
  mod->obj->symbol = IOBJ_SYM_CIRCLE;
  if (!residuals)
    mod->flags |= IMODF_FLIPYZ;
  mod->pixsize = scale;
  xmin = ymin= zmin = 1000000;
  xmax = ymax = zmax = -1000000;
  dz = 0.;
  for (i = 0; i < npatch; i++) {
    pts = (Ipoint *)malloc(2 * sizeof(Ipoint));
    mod->obj->cont[i].pts = pts;
    mod->obj->cont[i].psize = 2;
    len = fgetline(fin,line, MAXLINE);
    if (len < 3) {
      fprintf(stderr, "Error reading file at line %d.\n", i + 1);
      exit(1);
    }

    /* DNM 7/26/02: read in residuals as real coordinates, without a 
       flip */
    if (residuals) {
      sscanf(line, "%f %f %d %f %f", &xx, &yy, &iz, &dx, &dy);
    } else {
      /* DNM 11/15/01: have to handle either with commas or without,
     depending on whether it was produced by patchcorr3d or 
     patchcrawl3d */
      if (strchr(line, ','))
    sscanf(line, "%d %d %d %f, %f, %f", &ix, &iz, &iy, &dx,
           &dz, &dy);
      else
    sscanf(line, "%d %d %d %f %f %f", &ix, &iz, &iy, &dx, &dz,
           &dy);
      xx = ix;
      yy = iy;
    }

    pts[0].x = xx;
    pts[0].y = yy;
    pts[0].z = iz;
    pts[1].x = xx + scale * dx;
    pts[1].y = yy + scale * dy;
    pts[1].z = iz + scale * dz;
    if (xx < xmin)
      xmin = xx;
    if (xx > xmax)
      xmax = xx;
    if (yy < ymin)
      ymin = yy;
    if (yy > ymax)
      ymax = yy;
    if (iz < zmin)
      zmin = iz;
    if (iz > zmax)
      zmax = iz;
  }
     
  if (residuals) {
    mod->obj->symflags = IOBJ_SYMF_ENDS;
    mod->obj->symbol = IOBJ_SYM_NONE;
    mod->obj->symsize = 10;
  }

  mod->xmax = xmax + xmin;
  mod->ymax = ymax + ymin;
  mod->zmax = zmax + zmin;

  /* Set current thicken contour flag to aid deleting patches */
  mod->obj->flags |= IMOD_OBJFLAG_THICK_CONT;

  return(mod);
     
}

static int fgetline(FILE *fp, char s[],int limit)
{
  int c, i, length;

  if (fp == NULL){
    fprintf(stderr, "fgetline: file pointer not valid\n");
    return(0);
  }

  if (limit < 3){
    fprintf(stderr, "fgetline: limit (%d) > 2,\n", limit);
    return(0);
  }
     
  for (i=0; ( ((c = getc(fp)) != EOF) && (i < (limit-1)) && (c != '\n') ); i++)
    s[i]=c;
     
  if (i == 1){
    if (c == EOF){
      return(0);
    }
    if (c == '\n'){
      s[++i] = '\0';
      return(1);
    }
  }
               

  s[i]='\0';
  length = i;

  if (c == EOF)
    return (-1 * length);
  else
    return (length);
}

