/*  IMOD VERSION 2.50
 *
 *  imodcmopp.c --  Create an color mrc image file from a 
 *                  model and an mrc image file.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include "mrcfiles.h"
#include "imodel.h"

#define NTSCX 688
#define NTSCY 488

extern struct Mod_Contour *mop_scan(struct Mod_Contour *ocont);
int rgbmodpaint(struct Mod_Model *model, 
                struct MRCheader *hdata, 
                unsigned char **idata, 
                FILE *rtom, FILE *gtom, FILE *btom);
int cmopp_zscale(int zscale, char *fname);
int cmopp_write_mrc(char *moviename, FILE *rfin, FILE *gfin, FILE *bfin);
int cmopp_write_movie(char *moviename, 
                      FILE *rfin, FILE *gfin, FILE *bfin, 
                      char *rgb, int ntsc);
int rgbslice_paint(struct Mod_Model *mod,
                   void *idata,
                   void **pdata,
                   struct MRCheader *hdata,
                   int z);
int contour_paintrgb(struct Mod_Contour *cont,
                     unsigned char *idata,
                     unsigned char **pdata,
                     struct MRCheader *hdata,
                     struct Mod_Object *obj);
int slice_paint(struct Mod_Model *mod,
                void *idata,
                void *pdata,
                struct MRCheader *hdata,
                int z);     
int Mop_red, Mop_green, Mop_blue;
int Cmopp_zscale = 0;
int Cmopp_debug = FALSE;

static void putbyte(FILE *outf, unsigned char val)
{
  unsigned char buf[1];
  buf[0] = val;
  fwrite(buf,1,1,outf);
  return;
}

static void rgb_putshort(FILE *outf, unsigned short val)
{
  unsigned char buf[2];
     
  buf[0] = (val>>8);
  buf[1] = (val>>0);
  fwrite(buf,2,1,outf);
  return;
}

static int putlong(FILE *outf, unsigned int val)
{
  unsigned char buf[4];

  buf[0] = (val>>24);
  buf[1] = (val>>16);
  buf[2] = (val>>8);
  buf[3] = (val>>0);
  return fwrite(buf,4,1,outf);
}

int main( int argc, char *argv[])
{
  FILE *rfin = NULL,  *gfin = NULL,  *bfin = NULL;
  FILE *rfout = NULL, *gfout = NULL,  *bfout = NULL;
  FILE *mfin = NULL;
  FILE *fin = NULL;
  FILE *fout = NULL;
  FILE *of;

  Imod   *imod;
  /*     struct Mod_Model model; */
  struct Mod_Point maxpt;
  struct Mod_Point minpt;
  struct MRCheader hdata;
  struct LoadInfo  li;

  unsigned char **idata;
  unsigned char bdata;
  short sdata;
  short xsize, ysize;
  float ramp, rramp, gramp, bramp;
  int tfill, bfill, lfill, rfill;
  int xmax, ymax, zmax;
  int prosize;
  int i,j,k;
  int pid;
  int mrc = TRUE;
  int xaxis = 0;
  int zaxis = 0;
  int tilt = 0;
  int ntsc = 0;
  int start_tilt = -45;
  int end_tilt  = 45;
  int inc_tilt = 5;

  char command[256];
  char iname[128];
  char tom[64], rtom[64], gtom[64], btom[64],
    rshort[64], gshort[64], bshort[64],
    xyz[64], rxyz[64], gxyz[64], bxyz[64],
    rgb[64];

  char *theTempDir = ".";
  char *progname = imodProgName(argv[0]);

  if (argc < 4){
    imodVersion(progname);
    imodCopyright();
    fprintf(stderr,
            "Usage: %s <model file> <tomogram> <movie file>\n", progname);
    fprintf(stderr, "[-x] [-z] [-n] [-m] [-T <temp>] "
            "-t <start, end, increment>]\n");
    exit(1);
  }
     
  for (i = 4; i < argc; i++)
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
      case 'd':
        Cmopp_debug = TRUE;
        break;
      case 'n':
        ntsc = TRUE;
        break;
      case 'x':
        xaxis = 1;
        break;
      case 'z':
        zaxis = 1;
        break;
      case 'Z':
        sscanf(argv[++i], "%d", &Cmopp_zscale);
        break;
      case 'm':
        mrc = FALSE;
        break;
      case 't':
        tilt = 1;
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-t%d%*c%d%*c%d", &start_tilt,
                 &end_tilt, &inc_tilt);
        else
          sscanf(argv[++i], "%d%*c%d%*c%d", &start_tilt,
                 &end_tilt, &inc_tilt);
        break;

      case 'T':
        theTempDir = argv[++i];
        break;

      default:
        break;
      }
    }


  pid = getpid();
  sprintf(tom, "%s/%d.tom.mrc",    theTempDir, pid);
  sprintf(rtom, "%s/%d.red.mrc",   theTempDir, pid);
  sprintf(gtom, "%s/%d.green.mrc", theTempDir, pid);
  sprintf(btom, "%s/%d.blue.mrc",  theTempDir, pid);
  sprintf(rshort, "%s/%d.rs.mrc",  theTempDir, pid);
  sprintf(gshort, "%s/%d.gs.mrc",  theTempDir, pid);
  sprintf(bshort, "%s/%d.bs.mrc",  theTempDir, pid);
  sprintf(xyz,    "%s/%d.xyz",     theTempDir, pid);
  sprintf(rxyz,   "%s/%d.rxyz",    theTempDir, pid);
  sprintf(gxyz,   "%s/%d.gxyz",    theTempDir, pid);
  sprintf(bxyz,   "%s/%d.bxyz",    theTempDir, pid);
  sprintf(rgb,    "%s/%d",         theTempDir, pid);

  if ( (xaxis) && (zaxis) ){
    fprintf(stderr, "rgbmop: Only x or z can be selected\n.");
    exit(1);
  }

  if ((start_tilt > end_tilt) && (inc_tilt > 0)){
    fprintf(stderr, "%s: Error, bad tilt input.\n", progname);
    exit(3);
  }
  if ((end_tilt > start_tilt) && (inc_tilt < 0)){
    fprintf(stderr, "%s: Error, bad tilt input.\n", progname);
    exit(3);
  }
  if (!inc_tilt){
    fprintf(stderr, "%s: Error, bad tilt input.\n", progname);
    exit(3);
  }
  if (! (end_tilt - start_tilt)){
    fprintf(stderr, "%s: Error, bad tilt input.\n", progname);
    exit(3);
  }

  printf("Reading model %s.\n", argv[1]);

  imod = imodRead(argv[1]);
  if (!imod){
    fprintf(stderr, "rgbmop: Couldn't open/read %s\n", argv[1]);
    exit(3);
  }

  /*     mfin = fopen(argv[1], "rb");
         if (mfin == NULL){
         fprintf(stderr, "rgbmop: Couldn't open %s\n", argv[1]);
         exit(3);
         }
  */
  gfin = fopen(argv[2], "rb");
  if (gfin == NULL){
    fprintf(stderr, "rgbmop: Couldn't open %s\n", argv[2]);
    exit(3);
  }

  /*     if (ReadImod(&model, mfin)){ */
  /*        fprintf(stderr, "rgbmop: Error reading modelfile %s.\n", argv[1]); */
  /*        exit(3); */
  /*     } */

  imodel_maxpt(imod, &maxpt);
  imodel_minpt(imod, &minpt);

  if (mrc_head_read(gfin, &hdata)){
    fprintf(stderr, "rgbmop: Can't read tomogram header, %s.\n",
            argv[2]);
    exit(3);
  }
     
  li.white = imod->whitelevel;
  li.black = imod->blacklevel;
  li.xmax = hdata.nx;
  li.xmin = 0;
  li.ymax = hdata.ny;
  li.ymin = 0;
  li.zmax = hdata.nz - 1;
  li.zmin = 0;

  idata = mrc_read_byte(gfin, &hdata, &li, mrc_default_status);

  if (!idata){
    fprintf(stderr, "%s: Error reading image data\n", progname);
    exit(3);
  }
  rfout = fopen(rtom, "wb");
  gfout = fopen(gtom, "wb");
  bfout = fopen(btom, "wb");
     
  rgbmodpaint(imod, &hdata, idata, rfout, gfout, bfout);

  for (k = 0; k < hdata.nz; k++)
    free(idata[k]);
  free(idata);
  fclose(gfin);
     
  fclose(rfout);
  fclose(gfout);
  fclose(bfout);
  fclose(gfin);
  /*     fclose(mfin); */

  fin = fopen(rtom, "rb");
  mrc_head_read(fin, &hdata);
  xmax = hdata.nx - 1;
  ymax = hdata.ny - 1;
  zmax = hdata.nz - 1;
  fclose(fin);

  if (xaxis)
    prosize = ymax;
  else if (zaxis){
    if (xmax > ymax)
      prosize = xmax;
    else 
      prosize = ymax;
  }
  else
    prosize = xmax;
     
  prosize /= 4;
  prosize *= 4;
  prosize += 1;

  /***********/
  /* Project */

  if (Cmopp_zscale)
    zmax *= Cmopp_zscale;

  printf("\nProjecting red channel.\n");

  if (Cmopp_zscale)
    cmopp_zscale(Cmopp_zscale, rtom);

  fout = fopen(xyz, "wb");
  fprintf(fout, "%s\n%s\n", rtom, rxyz);
  fprintf(fout, "0,%d,0,%d,0,%d\n", xmax, ymax, zmax);
  if (xaxis)
    fprintf(fout, "x\n");
  else if (zaxis)
    fprintf(fout, "z\n");
  else
    fprintf(fout, "y\n"); 
  if (tilt)
    fprintf(fout, "%d,%d,%d\n", start_tilt, end_tilt, inc_tilt);
  else
    fprintf(fout, "0,175,5\n");
  fprintf(fout, "%d\n", prosize);
  fprintf(fout, "1\n0,1\n0\n");
  fclose(fout);
  fout = NULL;
  sprintf(command, "xyzproj < %s", xyz);
  system(command);
  remove(rshort); 

  printf("Projecting green channel.\n");
  if (Cmopp_zscale)
    cmopp_zscale(Cmopp_zscale, gtom);
  fout = fopen(xyz, "wb");
  fprintf(fout, "%s\n%s\n", gtom, gxyz);
  fprintf(fout, "0,%d,0,%d,0,%d\n", xmax, ymax, zmax);
  if (xaxis)
    fprintf(fout, "x\n");
  else if (zaxis)
    fprintf(fout, "z\n");
  else     
    fprintf(fout, "y\n");
  if (tilt)
    fprintf(fout, "%d,%d,%d\n", start_tilt, end_tilt, inc_tilt);
  else
    fprintf(fout, "0,175,5\n");
  fprintf(fout, "%d\n", prosize);
  fprintf(fout, "1\n0,1\n0\n");
  fclose(fout);
  fout = NULL; 
  sprintf(command, "xyzproj < %s", xyz);
  system(command);
  remove(gshort); 

  printf("Projecting blue channel.\n");
  if (Cmopp_zscale)
    cmopp_zscale(Cmopp_zscale, btom);

  fout = fopen(xyz, "wb");
  fprintf(fout, "%s\n%s\n", btom, bxyz);
  fprintf(fout, "0,%d,0,%d,0,%d\n", xmax, ymax, zmax);
  if (xaxis)
    fprintf(fout, "x\n");
  else if (zaxis)
    fprintf(fout, "z\n");
  else
    fprintf(fout, "y\n");
  if (tilt)
    fprintf(fout, "%d,%d,%d\n", start_tilt, end_tilt, inc_tilt);
  else
    fprintf(fout, "0,175,5\n");
  fprintf(fout, "%d\n", prosize);
  fprintf(fout, "1\n0,1\n0\n");
  fclose(fout);
  sprintf(command, "xyzproj < %s", xyz);
  system(command);
  remove(bshort); 
  remove(xyz);
  remove(rtom);
  remove(gtom);
  remove(btom);


  /*******************************/
  /* write movie file.           */

  printf("creating rgb files\n");     

  rfin = fopen(rxyz, "rb");
  gfin = fopen(gxyz, "rb");
  bfin = fopen(bxyz, "rb");

  if (mrc)
    cmopp_write_mrc(argv[3], rfin, gfin, bfin);
  else
    cmopp_write_movie(argv[3], rfin, gfin, bfin, rgb, ntsc);


  fclose(rfin);
  fclose(gfin);
  fclose(bfin);
  remove(rxyz);
  remove(gxyz);
  remove(bxyz);
  exit(0);
}


int cmopp_write_mrc(char *moviename, FILE *rfin, FILE *gfin, FILE *bfin)
{
  FILE *of;
  struct MRCheader hdata;
  float ramp, rramp = 256.0f, gramp = 256.0f, bramp = 256.0f;
  b3dInt16 sdata;
  unsigned char bdata;
  int i, j, k;
  float min, max, mean;

  of = fopen(moviename, "wb");
  if (!of)
    return(-1);

  mrc_head_read(rfin, &hdata);
  if (hdata.amax)
    rramp = 255.0 / hdata.amax;
  mrc_head_read(gfin, &hdata);
  if (hdata.amax)
    gramp = 255.0 / hdata.amax;
  mrc_head_read(bfin, &hdata);
  if (hdata.amax)
    bramp = 255.0 / hdata.amax;
  ramp = rramp;
  if (gramp < ramp)
    ramp = gramp;
  if (bramp < ramp)
    ramp = bramp;

  hdata.mode = MRC_MODE_RGB;
  hdata.swapped = 0;
  mrc_head_write(of, &hdata);
  b3dFseek(rfin, 1024, 0);
  b3dFseek(gfin, 1024, 0);
  b3dFseek(bfin, 1024, 0);
  for (k = 0; k < hdata.nz; k++)
    for(j = 0; j < hdata.ny; j++)
      for(i = 0; i < hdata.nx; i++){
        b3dFread(&sdata, 2, 1, rfin);

        bdata = (unsigned char)((float)sdata * ramp);
        b3dFwrite(&bdata, 1, 1, of);
        b3dFread(&sdata, 2, 1, gfin);

        bdata = (unsigned char)((float)sdata * ramp);
        b3dFwrite(&bdata, 1, 1, of);
        b3dFread(&sdata, 2, 1, bfin);

        bdata = (unsigned char)((float)sdata * ramp);
        b3dFwrite(&bdata, 1, 1, of);
      }

  fclose(of);
  return(0);
}


int cmopp_write_movie(char *moviename, 
                      FILE *rfin, FILE *gfin, FILE *bfin, 
                      char *rgb, int ntsc)
{
  struct MRCheader hdata;
  FILE *of;
  float ramp, rramp = 256.0f, gramp = 256.0f, bramp = 256.0f;
  short xsize, ysize;
  int tfill, bfill, lfill, rfill;
  int i, j, k;
  unsigned char bdata;
  short sdata;
  char command[128],iname[128];

  mrc_head_read(rfin, &hdata);
  if (hdata.amax)
    rramp = 255.0 / hdata.amax;
  mrc_head_read(gfin, &hdata);
  if (hdata.amax)
    gramp = 255.0 / hdata.amax;
  mrc_head_read(bfin, &hdata);
  if (hdata.amax)
    bramp = 255.0 / hdata.amax;
  ramp = rramp;
  if (gramp < ramp)
    ramp = gramp;
  if (bramp < ramp)
    ramp = bramp;
     
  tfill = 0;
  bfill = 0;
  rfill = 0;
  lfill = 0;
  xsize = hdata.nx;
  ysize = hdata.ny;

  if (ntsc){
    if (xsize < NTSCX)
      xsize = NTSCX;
    if (ysize < NTSCY)
      ysize = NTSCY;
  }

  /* keep xsize and ysize multipules of 4 bytes in length. */
  /* outdated, needed for movieplayer. */
  if (xsize % 4)
    xsize += (4 - (xsize % 4));

  if (ysize % 4)
    ysize += (4 - (ysize % 4));

  if (xsize != hdata.nx){
    lfill = (xsize - hdata.nx) / 2;
    rfill = lfill + ((xsize - hdata.nx) % 2 );
  }

  if (ysize != hdata.ny){
    tfill = (ysize - hdata.ny) / 2;
    bfill = tfill + ((ysize - hdata.ny) % 2);
  }

  b3dFseek(rfin, 1024, 0);
  b3dFseek(gfin, 1024, 0);
  b3dFseek(bfin, 1024, 0);
  for (k = 0; k < hdata.nz; k++){
    printf(".");
    fflush(stdout);
    sprintf(iname, "%s.%3.3d.rgb", moviename, k);
          
    if (NULL == (of  = fopen(iname, "wb"))){
      fprintf(stderr, "rgbmop: Couldn't open rgb file\n");
      exit(3);
    }

    rgb_putshort(of, (short)474);       /* MAGIC                */
    putbyte(of, (char)0);          /* STORAGE is VERBATIM  */
    putbyte(of, (char)1);          /* BPC is 1             */
    rgb_putshort(of, (short)3);         /* DIMENSION is 3 */
    rgb_putshort(of, xsize);     /* XSIZE                */
    rgb_putshort(of, ysize);     /* YSIZE                */
    rgb_putshort(of, (short)3);         /* ZSIZE                */
    putlong(of, (long)0);          /* PIXMIN is 0          */
    putlong(of, (long)255);        /* PIXMAX is 255        */
    for(i = 0; i < 4; i++)      /* DUMMY 4 bytes        */
      putbyte(of,0);
    fwrite(iname,80,1,of);  /* IMAGENAME            */
    putlong(of,0l);          /* COLORMAP is 0        */
    for(i = 0; i < 404; i++)    /* DUMMY 404 bytes      */
      putbyte(of,0);

    bdata = 0;
    for (j = 0; j < tfill; j++)
      for (i = 0; i < xsize; i++)
        putbyte(of,0);
    for (j = 0; j < hdata.ny; j++){
      for (i = 0; i < lfill; i++)
        putbyte(of,0);
      for(i = 0; i < hdata.nx; i++){
        b3dFread(&sdata, 2, 1, rfin);
        bdata = (float)sdata * ramp;
        fwrite(&bdata, 1, 1, of);
      }
      bdata = 0;
      for (i = 0; i < rfill; i++)
        putbyte(of,0);
    }
    bdata = 0;
    for (j = 0; j < bfill; j++)
      for (i = 0; i < xsize; i++)
        putbyte(of,0);


    /* Write green data. */
    bdata = 0;
    for (j = 0; j < tfill; j++)
      for (i = 0; i < xsize; i++)
        fwrite(&bdata, 1, 1, of);
    for (j = 0; j < hdata.ny; j++){
      for (i = 0; i < lfill; i++)
        fwrite(&bdata, 1, 1, of);
      for(i = 0; i < hdata.nx; i++){
        b3dFread(&sdata, 2, 1, gfin);
        bdata = (float)sdata * ramp;
        fwrite(&bdata, 1, 1, of);
      }
      bdata = 0;
      for (i = 0; i < rfill; i++)
        fwrite(&bdata, 1, 1, of);
    }
    bdata = 0;
    for (j = 0; j < bfill; j++)
      for (i = 0; i < xsize; i++)
        fwrite(&bdata, 1, 1, of);
          


    /* Write blue data. */
    bdata = 0;
    for (j = 0; j < tfill; j++)
      for (i = 0; i < xsize; i++)
        fwrite(&bdata, 1, 1, of);
    for (j = 0; j < hdata.ny; j++){
      for (i = 0; i < lfill; i++)
        fwrite(&bdata, 1, 1, of);
      for(i = 0; i < hdata.nx; i++){
        b3dFread(&sdata, 2, 1, bfin);
        bdata = (float)sdata * ramp;
        fwrite(&bdata, 1, 1, of);
      }
      bdata = 0;
      for (i = 0; i < rfill; i++)
        fwrite(&bdata, 1, 1, of);
    }
    bdata = 0;
    for (j = 0; j < bfill; j++)
      for (i = 0; i < xsize; i++)
        fwrite(&bdata, 1, 1, of);
          
    fclose(of);
          
  }
  /*
    printf("\ncreating movie file %s\n", moviename);
    sprintf(command, "moviemaker -l 2 -R -o %s %s.???.rgb", moviename, rgb);
    system(command);
    sprintf(command, "/bin/rm -r %s.???.rgb", rgb);
  */
  printf("\n");
  return(0);
}

int rgbmodpaint(struct Mod_Model *model, 
                struct MRCheader *hdata, 
                unsigned char **idata, 
                FILE *rtom, FILE *gtom, FILE *btom)
{
  int xmin, xmax, ymin, ymax, zmin, zmax;
  struct Mod_Point maxpt, minpt;
  struct MRCheader hdout;
  unsigned char **pdata;
  short sdata;
  int i,j,k;
  int xysize;
  imodel_maxpt(model, &maxpt);
  imodel_minpt(model, &minpt);
     
     
  pdata    = (unsigned char **)malloc(3 * sizeof(unsigned char *));
  pdata[0] = (unsigned char *)
    malloc(hdata->nx * hdata->ny * sizeof(unsigned char));
  pdata[1] = (unsigned char *)
    malloc(hdata->nx * hdata->ny * sizeof(unsigned char));
  pdata[2] = (unsigned char *)
    malloc(hdata->nx * hdata->ny * sizeof(unsigned char));

  zmax = hdata->nz - 1;
  zmin = 0;
  ymax = maxpt.y;
  ymin = minpt.y;
  xmax = maxpt.x;
  xmin = minpt.x;

  if ( (minpt.z < hdata->nz) && (minpt.z > 0))
    zmin = minpt.z;
  if ( (maxpt.z < (hdata->nz - 1))  &&  (maxpt.z > minpt.z) )
    zmax = maxpt.z;
     
  mrc_head_new(&hdout, xmax - xmin + 1, ymax - ymin + 1, zmax - zmin + 1,
               MRC_MODE_SHORT);
  mrc_head_write(rtom, &hdout);
  mrc_head_write(gtom, &hdout);
  mrc_head_write(btom, &hdout);
  xysize = hdata->nx * hdata->ny;

  for( k = zmin; k <= zmax; k++){
    for (i = 0; i < xysize; i++){
      pdata[0][i] = 0;
      pdata[1][i] = 0;
      pdata[2][i] = 0;
    }
    printf("Mopping section %d ", k);
    fflush(stdout);
    rgbslice_paint(model, (void *)&(idata[k][0]), 
                   (void **)pdata, hdata, k); 
    slice_paint(model, NULL, &(pdata[0][0]), hdata, k); 
    slice_paint(model, NULL, &(pdata[1][0]), hdata, k); 
    slice_paint(model, NULL, &(pdata[2][0]), hdata, k); 
          
    for (j = ymin; j <= ymax; j++){
      for (i = xmin; i <= xmax; i++){
        sdata = pdata[0][(j * hdata->nx) + i];
        b3dFwrite(&sdata, 2, 1, rtom);
        sdata = pdata[1][(j * hdata->nx) + i]; 
        b3dFwrite(&sdata, 2, 1, gtom);
        sdata = pdata[2][(j * hdata->nx) + i];
        b3dFwrite(&sdata, 2, 1, btom);
      }
    }
    printf("\r");
  }

  free( (pdata[0]));
  free( (pdata[1]));
  free( (pdata[2]));
  free(pdata);
  return(0);
}


int rgbslice_paint(struct Mod_Model *mod,
                   void *idata,
                   void **pdata,
                   struct MRCheader *hdata,
                   int z)
{

  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  struct Mod_Contour *fcont = NULL;
  int objnum, contnum;

  for (objnum = 0; objnum < mod->objsize; objnum++){
    obj = &(mod->obj[objnum]);
    if (obj->flags & IMOD_OBJFLAG_OPEN)
      continue;
    if (obj->flags & IMOD_OBJFLAG_OUT)
      continue;
    if (obj->flags & IMOD_OBJFLAG_OFF)
      continue;
    for(contnum = 0; contnum < obj->contsize; contnum++){
      cont = &(obj->cont[contnum]);
      if (cont == NULL)
        continue;
      if (cont->pts == NULL)
        continue;
      if (cont->pts[0].z == z){
        fcont = imodel_contour_scan(cont);  
        contour_paintrgb(fcont, (unsigned char *)idata, 
                         (unsigned char **)pdata, hdata, obj);    
      }
      if (fcont) {
        imodel_contour_delete(fcont);  
        fcont = NULL;
      }
    }
  }
  return(0);
}

int contour_paintrgb(struct Mod_Contour *cont,
                     unsigned char *idata,
                     unsigned char **pdata,
                     struct MRCheader *hdata,
                     struct Mod_Object *obj)
{
  int next, lx;
  int i, x, y;
  int xmin, xmax;
  unsigned char mopbyte;
  float r,g,b;
  int bgnpt, endpt, j;

  if (!cont)
    return(-1);
  if (!obj)
    return(-1);
  if (!hdata)
    return(-1);

  for(i = 0; i < cont->psize - 1; i++){
    y = cont->pts[i].y;
    bgnpt = i;
    while (cont->pts[i].y == cont->pts[i+1].y){
      ++i;
      if (i >= cont->psize){
        i = cont->psize - 1;
        break;
      }
    }
    endpt = i;

    /* ERROR: odd number of edge points,      */
    /*        just draw line across all data. */
    if (! ( (endpt-bgnpt)% 2)){
      xmax = cont->pts[bgnpt].x;
      xmin = cont->pts[endpt].x;
      if (xmin >= cont->surf)
        for(x = xmin; 
            ((x <= xmax)&&( x < hdata->nx)&&(y < hdata->ny)); 
            x++){
          mopbyte = idata[x + (y * hdata->nx)];
          r = (float)mopbyte * obj->red;
          g = (float)mopbyte * obj->green;
          b = (float)mopbyte * obj->blue;
          pdata[0][x + (y * hdata->nx)] = r;
          pdata[1][x + (y * hdata->nx)] = g;
          pdata[2][x + (y * hdata->nx)] = b;
                         
        }
    }
    /* even number of edge points. */
    else{
      for(j = bgnpt; j < endpt; j++){
        xmin = cont->pts[j].x;
        xmax = cont->pts[j+1].x;
        if (xmin >= cont->surf)
          for(x = xmin; 
              ((x <= xmax)&&( x < hdata->nx)&&(y < hdata->ny));
              x++){
            mopbyte = idata[x + (y * hdata->nx)];
            r = (float)mopbyte * obj->red;
            g = (float)mopbyte * obj->green;
            b = (float)mopbyte * obj->blue;
            pdata[0][x + (y * hdata->nx)] = r;
            pdata[1][x + (y * hdata->nx)] = g;
            pdata[2][x + (y * hdata->nx)] = b;
          }
        j++;
      }    
    }
  }
  return(0);
}

int cmopp_zscale(int zscale, char *fname)
{
  FILE *fp;
  int i,z,k,xysize;
  b3dInt16 sdata;
  unsigned char **idata;
  struct MRCheader hdata;

  fp = fopen(fname, "rb+");
  mrc_head_read(fp, &hdata);
  idata = mrc_read_byte(fp, &hdata, NULL, NULL);
  hdata.nz *= zscale;
  xysize = hdata.nz * hdata.ny;
  hdata.swapped = 0;
  mrc_head_write(fp, &hdata);

  for( k = 0; k < hdata.nz; k++){
    for(z = 0; z < zscale; z++){
      for(i = 0; i < xysize; i++){
        /* write short */
        sdata = idata[k][i];
        b3dFwrite(&sdata, 2, 1, fp);
      }
    }
    free(idata[k]);
  }
  free(idata);
  return(0);
}     
