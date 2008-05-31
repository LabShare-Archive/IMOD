/*
 *  imodauto.c -- Automatic model generation program.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
 */

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <limits.h>
#include <stdio.h>
#include "mrcc.h"
#include "imodel.h"
#include "sliceproc.h"

#define AUTO_BLANK 0
#define AUTO_FLOOD 1
#define AUTO_PATCH (1 << 2)
#define AUTO_FILL  (AUTO_FLOOD | AUTO_PATCH)
#define AUTO_CHECK (1 << 5)


static Iobj *imodaObjectCreateThresholdData
(MrcHeader *hdata, IloadInfo *li, float ksigma,   
 double highthresh, double lowthresh, int dim,
 int minsize, int maxsize, int followdiag, int inside,
 double shave, double tol, int delete_edge, int smoothflags);

static void auto_patch(unsigned char *data, int xsize, int ysize);

static void auto_patch_fill_outside(unsigned char *data, int xsize, int xmin,
                                    int xmax, int ymin, int ymax, int x, 
                                    int y);
static int imoda_object_bfill_2d(unsigned char *idata, int *tdata,
                                 int nx, int ny, int x, int y, int lt,
                                 int ht, int diagonal, int col);
static int nay8(unsigned char *data, int xsize, int ysize, int i, int j);
static void auto_shrink(unsigned char *data, int imax, int jmax);
static void auto_expand(unsigned char *data, int imax, int jmax);


static void usage(void)
{
  printf("Usage: imodauto [options] <image file> <model file>\n");
  printf("Options:\n");
  printf("\t-l #\tLow threshold level.\n");
  printf("\t-h #\tHigh threshold level.\n");
  printf("\t-d #\tThreshold flag 1=absolute 2=section-mean 3=stack-mean.\n");
  printf("\t-u\tInterpret threshold levels as unscaled intensities.\n");
  printf("\t-n\tFind inside contours at same threshold level.\n");
  printf("\t-f #\tFollow diagonals: 0=never, 1=above high, 2=below low, 3=always.\n");

  printf("\t-m #\tMinimum contour area mask.\n");
  printf("\t-M #\tMaximum contour area mask.\n");
  printf("\t-e #\tEdge mask: eliminate contours touching that # of edges.\n");
  printf("\t-s min,max\tIntensity scaling: scale min to 0 and max to 255.\n");
  printf("\t-X min,max\tLoad subset in X from min to max (numbered from 0).\n");
  printf("\t-Y min,max\tLoad subset in Y from min to max (numbered from 0).\n");
  printf("\t-Z min,max\tLoad subset in Z from min to max (numbered from 0).\n");
  printf("\t-k #\tSmooth with kernel filter of given sigma.\n");
  printf("\t-z #\tSet model z scale.\n");
  printf("\t-x \tExpand areas before enclosing in contours.\n");
  printf("\t-i \tShrink areas before enclosing in contours.\n");
  printf("\t-o \tSmooth areas (expand then shrink).\n");
  printf("\t-r #\tResolution factor (pixels) for shaving points.\n");
  printf("\t-R #\tTolerance (maximum error) for point reduction.\n");
  printf("\t-c r,g,b   Color of model object (r,g,b between 0 and 1 or 0 and 255).\n");
}

int main(int argc, char *argv[])
{

  Iobj *obj, *tobj;
  Imod *imod = NULL;
  MrcHeader hdata;
  IloadInfo li;
  IrefImage ref;
  
  FILE *fin, *fout;
  double ht = 255.0, lt = 0.0;
  double shave = 0.0;
  double tol = 0.0;
  int dim = 1;
  int minsize = 10;
  int maxsize = -1;
  int delete_edge = 0;
  int i, co, pt;
  float zscale = 1.0f;
  float finalarea;
  float ksigma = -1;
  int smoothflags = 0;
  int followdiag = 0;
  int inside = 0;
  float red = 0.;
  float green = 1.;
  float blue = 0.;
  int unscaled = 0;
  int hentered = 0;
  int lentered = 0;
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    imodVersion(progname);
    imodCopyright();
    usage();
    exit(3);
  }

  /* Make library error output to stderr go to stdout */
  b3dSetStoreError(-1);

  mrc_init_li(&li, NULL);
  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-')
      switch (argv[i][1]){
      case 'm':
        minsize = atoi(argv[++i]);
        break;
      case 'M':
        maxsize = atoi(argv[++i]);
        break;
      case 'd':
        dim = atoi(argv[++i]);
        break;
      case 'h':
        ht = atof(argv[++i]);
        hentered = 1;
        break;
      case 'k':
        ksigma = atof(argv[++i]);
        break;
      case 'l':
        lt = atof(argv[++i]);
        lentered = 1;
        break;
      case 'r':
        shave = atof(argv[++i]);
        break;
      case 'R':
        tol = atof(argv[++i]);
        break;
      case 'e':
        delete_edge = atoi(argv[++i]);
        break;
      case 'z':
        zscale = atof(argv[++i]);
        break;
      case 's':
        sscanf(argv[++i], "%f%*c%f", &(li.smin), &(li.smax));
        break;
      case 'x':
        if (smoothflags)
          smoothflags = -1;
        else
          smoothflags = 2;
        break;
      case 'o':
        if (smoothflags)
          smoothflags = -1;
        else
          smoothflags = 3;
        break;
      case 'i':
        if (smoothflags)
          smoothflags = -1;
        else
          smoothflags = 1;
        break;
      case 'n':
        inside = 1;
        break;
      case 'u':
        unscaled = 1;
        break;
      case 'f':
        followdiag = atoi(argv[++i]);
        break;
      case 'c':
        sscanf(argv[++i], "%f%*c%f%*c%f", &red, &green, &blue);
        if (red > 1. || green > 1. || blue > 1.) {
          red /= 255.;
          green /= 255.;
          blue /= 255.;
        }
        break;
      case 'X':
        sscanf(argv[++i], "%d%*c%d", &(li.xmin), &(li.xmax));
        break;
      case 'Y':
        sscanf(argv[++i], "%d%*c%d", &(li.ymin), &(li.ymax));
        break;
      case 'Z':
        sscanf(argv[++i], "%d%*c%d", &(li.zmin), &(li.zmax));
        break;
      default:
        usage();
        exit(3);
        break;

      }
    else
      break;
  }

  if ((dim < 0) || (dim > 3)){
    usage();
    exit(1);
  }

  if (i >= argc - 1){
    usage();
    exit(3);
  }
  if (smoothflags < 0) {
    printf("ERROR: %s - Only one of -x, -i and -o may be entered.\n",progname);
    exit(3);
  }

  if (inside) {
    if (hentered && lentered) {
      printf("ERROR: %s - Only a high or a low threshold, not both, "
              "may be entered when using\n the -n flag to find "
             "inside contours.\n", progname);
      exit (3);
    }
    /* Set thresholds equal, and set to follow diagonals only on the
       primary threshold */
    if (hentered) {
      lt = ht;
      followdiag = 1;
      lentered = 1;
    } else {
      ht = lt;
      followdiag = 2;
      hentered = 1;
    }
  }

  fin = fopen(argv[i++], "rb");
  if (fin == NULL){
    printf("ERROR: %s - Opening image file %s.\n", progname, argv[i-1]);
    if (errno)
      perror("imodauto open image");
    exit(3);
  }
  /* read in graphic header and image data */
  if (mrc_head_read(fin, &hdata)){
    printf("ERROR: %s - Reading input file header.\n", progname);
    exit(3);
  }

  if (li.smin == li.smax){
    li.smin = hdata.amin;
    li.smax = hdata.amax;
  }
  mrc_fix_li(&li, hdata.nx, hdata.ny, hdata.nz);

  /* This sets the scaling */
  mrcContrastScaling(&hdata, li.smin, li.smax, li.black, li.white, 
                     MRC_RAMP_LIN, &li.slope, &li.offset);

  /* DNM 6/19/02: scale entered values if unscaled option taken */
  if (unscaled) {
    if (hentered)
      ht = 255. * (ht - li.smin) / (li.smax - li.smin);
    if (lentered)
      lt = 255. * (lt - li.smin) / (li.smax - li.smin);
  }

  /* Rename existing file if any */
  imodBackupFile(argv[i]);

  fout = fopen(argv[i], "wb");
  if (!fout){
    printf("ERROR: %s - Opening %s\n", progname, argv[i]);
    if (errno)
      perror("imodauto open model");
    exit(3);
  }
     
  imod = imodNew();
  if (!imod){
    printf("ERROR: %s - creating model %s.\n", progname, argv[i]);
    exit(3);
  }
  imod->file = fout;
   
  obj = imodaObjectCreateThresholdData 
    (&hdata, &li, ksigma, ht, lt, dim,
     minsize, maxsize, followdiag, inside,
     shave, tol, delete_edge, smoothflags);
  if (!obj) {
    printf("ERROR: %s - Allocating memory or reading data", progname);
    exit(1);
  }
     
  obj->red = red;
  obj->green = green;
  obj->blue = blue;

  /* Shift all points in the object by the load-in min values */
  for (co = 0; co < obj->contsize; co++) {
    for (pt = 0; pt < obj->cont[co].psize; pt++) {
      obj->cont[co].pts[pt].x += li.xmin;
      obj->cont[co].pts[pt].y += li.ymin;
      obj->cont[co].pts[pt].z += li.zmin;
    }
  }

  if (imod->objsize){
    tobj = (Iobj *)realloc(imod->obj, sizeof(Iobj *) * 
                           (imod->objsize + 1));
    imod->obj = tobj;
    memcpy(&(imod->obj[imod->objsize]), obj, sizeof(Iobj));
  }else{
    imod->obj = obj;
  }
  imod->objsize++;
  imod->zscale = zscale;
  imod->xmax = hdata.nx;
  imod->ymax = hdata.ny;
  imod->zmax = hdata.nz;
  imod->cindex.object = imod->objsize - 1;

  /* Set up image reference information. */
  imodSetRefImage(imod, &hdata);

  imodWrite(imod, fout);
  fclose(fout);
  /*     imodDelete(imod); */
  exit(0);
}

#define KERNEL_MAXSIZE 7

static int listsize;
static int *xlist, *ylist;

/* creates an object from a 3-D image array */

static Iobj *imodaObjectCreateThresholdData
(MrcHeader *hdata, IloadInfo *li, float ksigma,   
 double highthresh, double lowthresh, int dim,
 int minsize, int maxsize, int followdiag, int inside,
 double shave, double tol, int delete_edge, int smoothflags)
{
  Iobj *obj, *nobj;
  Icont *cont, *newconts, *tmpcont;
  Ipoint pt, *tmpt;
  int nco, co, cpt, cz, ncont;
  int *tdata;
  int nx = li->xmax + 1 - li->xmin;
  int ny = li->ymax + 1 - li->ymin;
  int nz = li->zmax + 1 - li->zmin;
  unsigned char *idata;
  unsigned char *fdata;
  unsigned char **linePtrs;
  int i,j,k;
  int col = 1;
  float mean;
  double tsum;
  int thresh, t1, t2;
  int nxy = nx * ny;
  int dx, dy;
  float dist, area, threshUsed;
  int nobjsize = 0;
  int onobjsize = 0;
  Ipoint pmin, pmax;
  Islice slice;
  int nedge, critedge;
  int diagonal, reverse;
  float kernel[KERNEL_MAXSIZE*KERNEL_MAXSIZE];
  int kerdim;
  Islice *sout;

  tdata = (int *)malloc(sizeof(int) * nx * ny);
  idata = (unsigned char *)malloc(nxy);
  fdata = (unsigned char *)malloc(nxy);
  listsize = 4 * (nx + ny);
  xlist = (int *)malloc(listsize * sizeof(int));
  ylist = (int *)malloc(listsize * sizeof(int));
  linePtrs = (unsigned char **)malloc(sizeof(unsigned char *) * ny);
  nobj = imodObjectNew();
  nobj->red = 0.;
  nobj->green = 1.;
  nobj->blue = 0.;

  if (!tdata || !fdata || !xlist || !ylist || !linePtrs || !idata)
    return NULL;

  /* Get whole-file mean for dim = 2.  5/30/08: Trust file mean! */
  if (dim == 2) {
    mean = hdata->amean;
  }     

  for(k = 0; k < nz; k++){
    if (mrcReadZByte(hdata, li, idata, k + li->zmin))
      return NULL;

    /* Filter first if sigma entered */
    if (ksigma >= 0.) {
      sliceInit(&slice, nx, ny, SLICE_MODE_BYTE, idata);
      slice.min = slice.max = 0.;
      if (ksigma > 0.) {
        scaledGaussianKernel(kernel, &kerdim, KERNEL_MAXSIZE, ksigma);
        sout = slice_mat_filter(&slice, kernel, kerdim);
        sliceScaleAndFree(sout, &slice);
      } else {
        sliceByteSmooth(&slice);
      }
    }

    /* Get per-section mean for dim = 3 */
    if (dim == 3) {
      for (tsum = 0, i = 0; i < nxy; i++)
        tsum += idata[i];
      mean = tsum / nxy;
    }

    /* Use the thresholds literally for dim = 1 */
    if (dim == 1){
      t1 = lowthresh;
      t2 = highthresh;
    } else {
      t1 = (int) mean * lowthresh;
      t2 = (int) mean * highthresh;
    }
    /*  printf("z %d  t1 %d  t2 %d\n", k, t1, t2); */

    /* To match imod auto, increment t1 and test for >= that, or <=
       low threshold.  But also set t1 to -1 if it's 0, to enforce an
       exclusion of 0's */
    t2++;
    if (!t1)
      t1 = -1;

    col = 1;
    /* init tdata and fill with out of bounds data */
    for (i = 0; i < nxy; i++)
      tdata[i] = 0;
    for (i = 0; i < nxy; i++)
      if ( (idata[i] > t1) && (idata[i] < t2) )
        tdata[i] = -1;

    for(j = 0; j < ny; j++)
      for(i = 0; i < nx; i++){
        if (tdata[i + (j * nx)] != 0)
          continue;
        if (followdiag <= 0)
          diagonal = 0;
        else if (followdiag >= 3)
          diagonal = 1;
        else if (followdiag == 1)
          diagonal = (idata[i + (j * nx)] >= t2);
        else
          diagonal = (idata[i + (j * nx)] <= t1);

        if (imoda_object_bfill_2d(idata, tdata, nx, ny, i, j,
                                  t1, t2, diagonal, col) > 1 || 
            minsize < 2){
                        
          /* printf("\nfilled c %d at %d,%d,%d\n", 
             col, i, j, k); */
                               
          col++;
        } else {
          /* If single pixel, and minsize > 1, just eliminate
             this pixel */
          tdata[i + (j * nx)] = -1; 
        }
      }
         

    /* sort the points into contours */
    obj = imodObjectNew();
    obj->contsize = col-1;
    obj->cont = imodContoursNew(obj->contsize);
         
    for(j = 0; j < ny; j++)
      for(i = 0; i < nx; i++){
        co = (int) tdata[i + (j * nx)];
        if (co > 0){
          pt.x = i; pt.y = j; pt.z = k;
          imodPointAppend(&(obj->cont[co-1]), &pt);
        }
      }

    nobjsize = nobj->contsize;
    onobjsize = nobjsize;
    /* eliminate contours with # of points outside the bounds */
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      nedge = 0;
      critedge = delete_edge;
      /* If doing inside, set up to eliminate any contour touching
         an edge if it is the wrong polarity */
      if (inside) {
        i = cont->pts->x;
        j = cont->pts->y;
        if ((followdiag == 1 && idata[i + (j * nx)] <= t1) ||
            (followdiag == 2 && idata[i + (j * nx)] >= t2))
          critedge = 1;
      }
      if (critedge) {
        /* count the edges that the contour touches */
        imodContourGetBBox(cont, &pmin, &pmax);
        if (pmin.x == 0)
          nedge++;
        if (pmax.x == nx - 1)
          nedge++;
        if (pmin.y == 0)
          nedge++;
        if (pmax.y == ny - 1)
          nedge++;
      }
      if ((cont->psize < minsize) ||
          (cont->psize > maxsize && maxsize > 0) ||
          (critedge && (nedge >= critedge))){
        if (cont->psize){
          cont->psize = 0;
          free(cont->pts);
        }
        continue;
      }
                   
      nobjsize++;
    }

    for(nco = onobjsize, co = 0; co < obj->contsize; co++){
              
      cont = &(obj->cont[co]);
      if (!cont->psize) continue;

      printf("\rsection %d contour %6d of %6d size = %6d", 
             k, nco-onobjsize + 1, nobjsize-onobjsize,
             cont->psize);
      fflush(stdout);
             
      cz = cont->pts->z;

      /* Clear fdata array and mark pixels in this contour as FLOOD */
      for(i = 0; i < nxy; i++)
        fdata[i] = 0;

      for(cpt = 0; cpt < cont->psize; cpt++){
        i = cont->pts[cpt].x;
        j = cont->pts[cpt].y;
        fdata[i + (j * nx)] = AUTO_FLOOD;
      }

      if (followdiag <= 0)
        diagonal = 0;
      else if (followdiag >= 3)
        diagonal = 1;
      else if (followdiag == 1)
        diagonal = (idata[i + (j * nx)] >= t2);
      else
        diagonal = (idata[i + (j * nx)] <= t1);
             
      auto_patch(fdata, nx, ny);

      /* Set the reverse flag and set threshold based on which one is passed */
      reverse = idata[i + (j * nx)] <= t1 ? 1 : 0;
      threshUsed = -1.;
      if (idata[i + (j * nx)] <= t1)
        threshUsed = t1 + 0.5;
      if (idata[i + (j * nx)] >= t2)
        threshUsed = t2 - 0.5;

      /* If we do an expand, shrink, or smooth, run the patch again and set 
         the threshold to be found by the routine */
      if (smoothflags > 1)
        auto_expand(fdata, nx, ny);
      if (smoothflags % 2)
        auto_shrink(fdata, nx, ny);
      if (smoothflags) {
        auto_patch(fdata, nx, ny);
        threshUsed = -1.;
      }

      /* Make line pointers for image array */
      for (j = 0; j < ny; j++)
        linePtrs[j] = &idata[j * nx];
             
      newconts = imodContoursFromImagePoints(fdata, linePtrs, nx, ny, cz, 
                                             AUTO_FLOOD, diagonal, threshUsed,
                                             reverse, &ncont);
      for (i = 0; i < ncont; i++) {
                  
        /* Just check the area and eliminate again */
        area = imodContourArea(&(newconts[i]));
        if (area < minsize || (maxsize > 0 && area > maxsize))
          continue;
                  
        imodContourStrip(&(newconts[i]));
        if (tol != 0.0)
          imodContourReduce(&(newconts[i]), tol);
        if (shave != 0.0)
          imodContourShave(&(newconts[i]), shave);
        tmpcont = imodContourNew();
        imodObjectAddContour(nobj, tmpcont);
        free(tmpcont);
        imodContourCopy(&newconts[i], 
                        &(nobj->cont[nobj->contsize - 1]));
      }
      nco++;
      if (newconts)
        free(newconts);
    }
         
    imodObjectDelete(obj);
         
  }
     
  free(idata);
  free(tdata);
  free(fdata);
  free(xlist);
  free(ylist);
  printf("\ndone\n");
  return(nobj);
}

static int imoda_object_bfill_2d(unsigned char *idata, int *data,
                                 int xsize, int ysize, int x, int y, int t1,
                                 int t2, int diagonal, int col)
{
  int threshold;
  int ringnext = 0;
  int ringfree = 1;
  int pixind;
  int direction;
  int nadded = 0;

  if (idata[x + (y * xsize)] <= t1) {
    threshold = t1;
    direction = -1;
  } else {
    threshold = t2;
    direction = 1;
  }

  /* initialize the ring buffer */
  xlist[0] = x;
  ylist[0] = y;
  data[x + y * xsize] = -2;

  while (ringnext != ringfree) {

    /* check next point on list */
    x = xlist[ringnext];
    y = ylist[ringnext];
    pixind = x + y * xsize;
    if (direction * (idata[pixind] - threshold) >= 0) {

      /* If point passes test, mark as flood */ 
      data[pixind] = col;
      nadded++;

      /* add each of four neighbors on list if coordinate is legal
         and they are not already on list or in flood */
      if (x > 0 && !data[pixind - 1]) {
        xlist[ringfree] = x - 1;
        ylist[ringfree++] = y;
        ringfree %= listsize;
        data[pixind - 1] = -2;
      }
      if (x < xsize - 1 && !data[pixind + 1]) {
        xlist[ringfree] = x + 1;
        ylist[ringfree++] = y;
        ringfree %= listsize;
        data[pixind + 1] = -2;
      }
      if (y > 0 && !data[pixind - xsize]) {
        xlist[ringfree] = x;
        ylist[ringfree++] = y - 1;
        ringfree %= listsize;
        data[pixind - xsize] = -2;
      }
      if (y < ysize - 1 && !data[pixind + xsize]) {
        xlist[ringfree] = x;
        ylist[ringfree++] = y + 1;
        ringfree %= listsize;
        data[pixind + xsize] = -2;
      }
      if (diagonal) {
        if (x > 0 && y > 0 && !data[pixind - 1 - xsize]) {
          xlist[ringfree] = x - 1;
          ylist[ringfree++] = y - 1;
          ringfree %= listsize;
          data[pixind - 1 - xsize] = -2;
        }
        if (x < xsize - 1 && y > 0 && !data[pixind + 1 - xsize]) {
          xlist[ringfree] = x + 1;
          ylist[ringfree++] = y - 1;
          ringfree %= listsize;
          data[pixind + 1 - xsize] = -2;
        }
        if (x > 0 && y < ysize - 1 && !data[pixind - 1 + xsize]) {
          xlist[ringfree] = x - 1;
          ylist[ringfree++] = y + 1;
          ringfree %= listsize;
          data[pixind - 1 + xsize] = -2;
        }
        if (x < xsize - 1 && y < ysize - 1 &&
            !data[pixind + 1 + xsize]) {
          xlist[ringfree] = x + 1;
          ylist[ringfree++] = y + 1;
          ringfree %= listsize;
          data[pixind + 1 + xsize] = -2;
        }
      }
    }

    /* Take point off list, advance next pointer */
    if (data[pixind] == -2)
      data[pixind] = 0;
    ringnext++;
    ringnext %= listsize;
  }

  return(nadded);
}

/* auto_patch fills area outside the flood with the patch flag, then makes
   unmarked pixels be part of the flood */
static void auto_patch(unsigned char *data, int xsize, int ysize)
{
  int i, x, y;
  int xysize;
  int xmax = -1;
  int xmin = xsize;
  int ymax = -1;
  int ymin = ysize;

  /* get min and max of flooded area */
  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      if (data[x + y * xsize] & AUTO_FLOOD) {
        if (x < xmin)
          xmin = x;
        if (x > xmax)
          xmax = x;
        if (y < ymin)
          ymin = y;
        if (y > ymax)
          ymax = y;
      }

  /* Start a patch from every point along the four sides, because there
     may be isolated patches */
  for(x = xmin ; x <= xmax; x++){
    auto_patch_fill_outside(data, xsize, xmin, xmax, ymin, ymax, x,
                            ymin);
    auto_patch_fill_outside(data, xsize, xmin, xmax, ymin, ymax, x, 
                            ymax);
  }
  for (y = ymin ; y <= ymax; y++){
    auto_patch_fill_outside(data, xsize, xmin, xmax, ymin, ymax, xmin, 
                            y);
    auto_patch_fill_outside(data, xsize, xmin, xmax, ymin, ymax, xmax, 
                            y);
  }

  xysize = xsize * ysize;

  /* Mark everything now not in a patch as in the flood */
  for(y = ymin; y <= ymax; y++)
    for(x = xmin; x <= xmax; x++){
      i = x + y * xsize;
      if (!(data[i] & (AUTO_FLOOD | AUTO_PATCH)))
        data[i] |= AUTO_FLOOD;
    }
     
  /* Clear the patch flags */
  for ( i = 0; i < xysize; i++)
    if (data[i] & AUTO_PATCH)
      data[i] &= ~AUTO_PATCH;
}

/* To build a patch from a single point */
static void auto_patch_fill_outside(unsigned char *data, int xsize, int xmin,
                                    int xmax, int ymin, int ymax, int x, int y)
{
  int ringnext = 0;
  int ringfree = 1;
  int pixind;
  unsigned char neighflag;
 
  /* Don't even start if this point is a patch or a flood */
  pixind = x + y * xsize;
  if (data[pixind] & (AUTO_FLOOD | AUTO_PATCH))
    return;

  /* initialize the ring buffer */
  xlist[0] = x;
  ylist[0] = y;
  data[pixind] |= (AUTO_CHECK | AUTO_PATCH);
  neighflag  = AUTO_FLOOD | AUTO_CHECK | AUTO_PATCH;

  while (ringnext != ringfree) {

    /* the next point on list got there by being neither patch nor
       flood, so it needs no checking or marking */
    x = xlist[ringnext];
    y = ylist[ringnext];
    pixind = x + y * xsize;

    /* add each of four neighbors on list if coordinate is legal
       and they are not already on list or in flood or patch. 
       Mark each as on list and in patch */
    if (x > xmin && !(data[pixind - 1] & neighflag)) {
      xlist[ringfree] = x - 1;
      ylist[ringfree++] = y;
      ringfree %= listsize;
      data[pixind - 1] |= (AUTO_CHECK | AUTO_PATCH);
    }
    if (x < xmax && !(data[pixind + 1] & neighflag)) {
      xlist[ringfree] = x + 1;
      ylist[ringfree++] = y;
      ringfree %= listsize;
      data[pixind + 1] |= (AUTO_CHECK | AUTO_PATCH);
    }
    if (y > ymin && !(data[pixind - xsize] & neighflag)) {
      xlist[ringfree] = x;
      ylist[ringfree++] = y - 1;
      ringfree %= listsize;
      data[pixind - xsize] |= (AUTO_CHECK | AUTO_PATCH);
    }
    if (y < ymax && !(data[pixind + xsize] & neighflag)) {
      xlist[ringfree] = x;
      ylist[ringfree++] = y + 1;
      ringfree %= listsize;
      data[pixind + xsize] |= (AUTO_CHECK | AUTO_PATCH);
    }
          
    /* Take point off list, advance next pointer */
    data[pixind] &= ~AUTO_CHECK;
    ringnext++;
    ringnext %= listsize;
  }
}

static int nay8(unsigned char *data, int xsize, int ysize, int i, int j)
{
  int n, m, k = 0;
  int x, y;

  if (!(data[i + (j * xsize)] & AUTO_FLOOD))
    return(0);
     
  for (n = -1; n <= 1; n++){
    y = n + j;
    for(m = -1; m <= 1 ; m++){
      x = m + i;
      if ((x >= 0) && (y >= 0) && (x < xsize) && (y < ysize))
        if (data[x + (y * xsize)] & AUTO_FLOOD)
          k++; 
    }
  }
  return(k-1);
}

static void auto_shrink(unsigned char *data, int imax, int jmax)
{
  int i, j, m, n;
  int tval;
  int white = 0;
     
  /* DNM: tried testing on fill flag before checking neighbors and it
     didn't work. */
  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if (nay8(data, imax, jmax, i, j) < 7)
        data[i + (j * imax)] |= AUTO_CHECK;
    }

  /* DNM: clear check flag after use, not before */
  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if (data[i + (j * imax)] & AUTO_CHECK)
        data[i + (j * imax)] &= ~(AUTO_FLOOD | AUTO_CHECK);
    }

  return;

}

static void auto_expand(unsigned char *data, int imax, int jmax)
{
  int i, j, m, n, x, y;
     
  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if (!(data[i + (j * imax)] & AUTO_FILL)) continue;

      for(m = -1; m <= 1; m++){
        y = j + m;
        if ((y < 0) || (y >= jmax)) continue;
        for(n = -1; n <= 1; n++){
          x = n + i;
          if ((x == i) && (y == j)) continue;
          if ((x < 0) || (x >= imax)) continue;
          data[x + (y * imax)] |= AUTO_CHECK;
        }
      }
    }

  /* DNM: clear check flag in this loop, not before use */
  for(i = 0; i < imax * jmax; i++)
    if (data[i] & AUTO_CHECK) {
      data[i] |= AUTO_FLOOD;
      data[i] &= ~AUTO_CHECK;
    } 
  return;

}

/*

$Log$
Revision 3.12  2008/05/27 05:30:12  mast
Added kernel smoothing and changed to use new interpolation

Revision 3.11  2008/04/04 21:21:28  mast
Free contour after adding to object

Revision 3.10  2007/09/25 15:44:10  mast
Set up image reference information in model

Revision 3.9  2005/04/04 22:41:32  mast
Fixed problem with argument order to imdContourGetBBox

Revision 3.8  2005/03/20 19:56:05  mast
Eliminating duplicate functions

Revision 3.7  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.6  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2004/06/24 15:36:39  mast
Added -X, -Y, -Z options for doing subvolume

Revision 3.4  2003/10/24 03:05:23  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2002/12/23 21:38:08  mast
fixed exit status

Revision 3.2  2002/06/20 00:09:21  mast
Added option to interpret thresholds as unscaled values

Revision 3.1  2002/06/18 17:23:03  mast
Added option to set color of model object

*/
