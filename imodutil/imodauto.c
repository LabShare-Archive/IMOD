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
 */

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <limits.h>
#include <stdio.h>
#include "iimage.h"
#include "imodel.h"
#include "sliceproc.h"
#include "parse_params.h"


static Iobj *imodaObjectCreateThresholdData
(MrcHeader *hdata, IloadInfo *li, float ksigma, double highthresh, double lowthresh,
 int exact, int dim, int minsize, int maxsize, int followdiag, int inside, double shave,
 double tol, int delete_edge, int smoothflags, Iobj *boundObj, int nearestBound);

static int imoda_object_bfill_2d(unsigned char *idata, int *tdata,
                                 int nx, int ny, int x, int y, int lt,
                                 int ht, int exact, int diagonal, int col);
static int findBoundaryConts(int z, Iobj *boundObj, int nearestBound, Ilist *contList);


static void usage(void)
{
  printf("Usage: imodauto [options] <image file> <model file>\n");
  printf("Options:\n");
  printf("\t-l #\tLow threshold level.\n");
  printf("\t-h #\tHigh threshold level.\n");
  printf("\t-E #\tExact value to make contours around.\n");
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
  printf("\t-B file\tName of model file with boundary contours.\n");
  printf("\t-O file\tNumber of object with boundary contours in boundary model.\n");
  printf("\t-S file\tApply boundary contours only on the same Z as the contour.\n");
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
  
  FILE *fin, *fout;
  char *boundFile = NULL;
  Imod *boundMod = NULL;
  Iobj *boundObj = NULL;
  int nearestBound = 1;
  int boundObjNum = -1;
  double ht = 255.0, lt = 0.0;
  double shave = 0.0;
  double tol = 0.0;
  int exact = -2;
  double exactIn = -2.;
  int dim = 1;
  int minsize = 10;
  int maxsize = -1;
  int delete_edge = 0;
  int i, co, pt, ob;
  float zscale = 1.0f;
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
  int xentered = 0;
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    imodVersion(progname);
    imodCopyright();
    usage();
    exit(3);
  }

  setExitPrefix("ERROR: imodauto - ");
  
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
      case 'E':
        xentered = 1;
        exactIn = atof(argv[++i]);
        exact = B3DNINT(exactIn);
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
      case 'B':
        boundFile = strdup(argv[++i]);
        break;
      case 'O':
        boundObjNum = atoi(argv[++i]);
        break;
      case 'S':
        nearestBound = 0;
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
  if (smoothflags < 0)
    exitError("Only one of -x, -i and -o may be entered.");

  if (!hentered && !lentered && !xentered)
    exitError("You must enter at least one threshold with -l or -h or an exact "
              "value with -E");

  if (xentered && dim > 1)
    exitError("You cannot enter -d with -E");

  if (xentered && (hentered || lentered))
    exitError("You cannot enter -l or -h with -E");

  if (inside) {
    if (hentered && lentered)
      exitError("Only a high or a low threshold, not both, "
              "may be entered when using -n to find inside contours.");

    /* Set thresholds equal, and set to follow diagonals only on the
       primary threshold */
    if (!xentered) {
      if (hentered) {
        followdiag = 1;
        lt = ht;
        lentered = 1;
      } else {
        followdiag = 2;
        ht = lt;
        hentered = 1;
      }
    }
  }

  fin = iiFOpen(argv[i++], "rb");
  if (fin == NULL){
    printf("ERROR: %s - Opening image file %s.\n", progname, argv[i-1]);
    if (errno)
      perror("imodauto open image");
    exit(3);
  }
  /* read in graphic header and image data */
  if (mrc_head_read(fin, &hdata))
    exitError("Reading input file header.");

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
    if (xentered)
      exact = B3DNINT(255. * (exactIn - li.smin) / (li.smax - li.smin));
  }

  /* Now that the true exact value is known, set the other thresholds for doing inside */
  if (xentered) {
    if (inside) {
      lt = exact - 1.;
      ht = exact + 1.;
      followdiag = 1;
    } else {
      lt = -1.;
      if (followdiag)
        followdiag = 3;
    }
  }

  /* Open boundary model if any */
  if (boundFile) {
    boundMod = imodRead(boundFile);
    if (!boundMod)
      exitError("Reading boundary model file %s", boundFile);
    boundObjNum = B3DMIN(boundObjNum, boundMod->objsize);
    if (boundObjNum <= 0) {

      /* Find first closed contour object */
      for (ob = 0; ob < boundMod->objsize; ob++) {
        if (iobjClose(boundMod->obj[ob].flags)) {
          boundObjNum = ob + 1;
          break;
        }
      }
      if (boundObjNum <= 0)
        exitError("No closed contour objects were found in boundary model %s", boundFile);
    } else if (!iobjClose(boundMod->obj[boundObjNum - 1].flags))
      exitError("Object %d in boundary model %s is not a closed contour object",
                boundObjNum, boundFile);

    boundObj = &boundMod->obj[boundObjNum - 1];

    /* Shift the model if it was loaded on a subset and also adjust for the current
       restriction on loading */
    imodTransForSubsetLoad(boundMod, &hdata, &li);
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
  if (!imod)
    exitError("Creating model %s.\n", argv[i]);
  imod->file = fout;
   
  obj = imodaObjectCreateThresholdData 
    (&hdata, &li, ksigma, ht, lt, exact, dim,
     minsize, maxsize, followdiag, inside,
     shave, tol, delete_edge, smoothflags, boundObj, nearestBound);
  if (!obj)
    exitError("Allocating memory or reading data");
     
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
 double highthresh, double lowthresh, int exact, int dim,
 int minsize, int maxsize, int followdiag, int inside,
 double shave, double tol, int delete_edge, int smoothflags, 
 Iobj *boundObj, int nearestBound)
{
  Iobj *obj, *nobj;
  Icont *cont, *newconts, *tmpcont;
  Ipoint pt;
  Ilist *boundConts = ilistNew(sizeof(int), 4);
  int nco, co, cpt, cz, ncont, incont;
  int *nump;
  int *tdata;
  int nx = li->xmax + 1 - li->xmin;
  int ny = li->ymax + 1 - li->ymin;
  int nz = li->zmax + 1 - li->zmin;
  unsigned char *idata;
  unsigned char *fdata;
  unsigned char **linePtrs;
  int i,j,k, ind;
  int col = 1;
  float mean;
  double tsum;
  int t1, t2;
  int nxy = nx * ny;
  float area, threshUsed;
  int nobjsize = 0;
  int onobjsize = 0;
  Ipoint pmin, pmax, pim;
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

    /* Get list of boundary contours for this section */
    if (boundObj && findBoundaryConts(k, boundObj, nearestBound, boundConts))
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
      t1 = (int)lowthresh;
      t2 = (int)highthresh;
    } else {
      t1 = (int) mean * lowthresh;
      t2 = (int) mean * highthresh;
    }
    /* printf("z %d  t1 %d  t2 %d exact %d\n", k, t1, t2, exact); */

    /* To match imod auto, increment t1 and test for >= that, or <=
       low threshold.  But also set t1 to -1 if it's 0, to enforce an
       exclusion of 0's.  Also, if doing exact, these values are already set up */
    if (exact < 0) {
      t2++;
      if (!t1)
        t1 = -1;
    } 

    col = 1;
    /* init tdata and fill with out of bounds data */
    for (i = 0; i < nxy; i++)
      tdata[i] = 0;
    for (i = 0; i < nxy; i++)
      if (idata[i] > t1 && idata[i] < t2 && idata[i] != exact)
        tdata[i] = -1;

    /* If there are boundary contours, check each point not marked out yet and mark
       the ones that are not inside any contour */
    if (ilistSize(boundConts)) {
      pim.z = k;
      for (j = 0; j < ny; j++) {
        for (i = 0; i < nx; i++) {
          if (tdata[i + (j * nx)] != 0)
            continue;
          pim.x = i + 0.5;
          pim.y = j + 0.5;
          incont = 0;
          for (ind = 0; ind < ilistSize(boundConts) && !incont; ind++) {
            nump = (int *)ilistItem(boundConts, ind);
            incont = imodPointInsideCont(&boundObj->cont[*nump], &pim);
          }
          if (!incont)
            tdata[i + (j * nx)] = -1;
        }
      }
    }

    /* Loop on each pixel that hasn't been marked somehow, and fill a patch from that
       point */
    for (j = 0; j < ny; j++)
      for (i = 0; i < nx; i++) {
        if (tdata[i + (j * nx)] != 0)
          continue;
        if (followdiag <= 0)
          diagonal = 0;
        else if (followdiag >= 3)
          diagonal = 1;
        else if (followdiag == 1)
          diagonal = ((exact < 0 && idata[i + (j * nx)] >= t2) || 
                      idata[i + (j * nx)] == exact);
        else
          diagonal = ((exact < 0 && idata[i + (j * nx)] <= t1) ||
                      idata[i + (j * nx)] == exact);

        if (imoda_object_bfill_2d(idata, tdata, nx, ny, i, j, t1, t2, exact, diagonal,
                                  col) > 1 || minsize < 2) {
                        
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
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      nedge = 0;
      critedge = delete_edge;
      /* If doing inside, set up to eliminate any contour touching
         an edge if it is the wrong polarity */
      if (inside) {
        i = cont->pts->x;
        j = cont->pts->y;
        if ((followdiag == 1 && ((exact < 0 && idata[i + (j * nx)] <= t1) || 
                                 (exact >= 0 && idata[i + (j * nx)] != exact))) ||
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

        /* If there are boundary conts, find distance of each point to each contour and
           set edge flag if it is ever close */
        if (ilistSize(boundConts)) {
          for (ind = 0; ind < ilistSize(boundConts) && !incont; ind++) {
            nump = (int *)ilistItem(boundConts, ind);
            for (i = 0; i < cont->psize; i++) {
              pim.x = cont->pts[i].x + 0.5;
              pim.y = cont->pts[i].y + 0.5;
              if (imodPointContDistance(&boundObj->cont[*nump], &pim, 0, 0, &j)
                  < 0.8) {
                nedge++;
                break;
              }
            }
            if (i < cont->psize)
              break;
          }
        }
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

    for (nco = onobjsize, co = 0; co < obj->contsize; co++) {
              
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
        fdata[i + (j * nx)] = AUTOX_FLOOD;
      }

      if (followdiag <= 0)
        diagonal = 0;
      else if (followdiag >= 3)
        diagonal = 1;
      else if (followdiag == 1)
        diagonal = ((exact < 0 && idata[i + (j * nx)] >= t2) || 
                    idata[i + (j * nx)] == exact);
      else
        diagonal = ((exact < 0 && idata[i + (j * nx)] <= t1) ||
                    idata[i + (j * nx)] == exact);
              
      imodAutoPatch(fdata, xlist, ylist, listsize, nx, ny);

      /* Set the reverse flag and set threshold based on which one is passed */
      /* These won't be used for exact work */
      reverse = idata[i + (j * nx)] <= t1 ? 1 : 0;
      threshUsed = -1.;
      if (idata[i + (j * nx)] <= t1)
        threshUsed = t1 + 0.5;
      if (idata[i + (j * nx)] >= t2)
        threshUsed = t2 - 0.5;

      /* If we do an expand, shrink, or smooth, run the patch again and set 
         the threshold to be found by the routine.
         Probably should forbid this for exact */
      if (smoothflags > 1)
        imodAutoExpand(fdata, nx, ny);
      if (smoothflags % 2)
        imodAutoShrink(fdata, nx, ny);
      if (smoothflags) {
        imodAutoPatch(fdata, xlist, ylist, listsize, nx, ny);
        threshUsed = -1.;
      }

      /* Make line pointers for image array */
      for (j = 0; j < ny; j++)
        linePtrs[j] = &idata[j * nx];
             
      newconts = imodContoursFromImagePoints(fdata, exact < 0 ? linePtrs : NULL, nx, ny,
                                             cz, AUTOX_FLOOD, diagonal, threshUsed,
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
                                 int t2, int exact, int diagonal, int col)
{
  int threshold;
  int ringnext = 0;
  int ringfree = 1;
  int pixind;
  int direction;
  int nadded = 0;
  int testExact = 0;

  if (exact >= 0) {
    testExact = idata[x + (y * xsize)] == exact ? 1 : 0;
  } else if (idata[x + (y * xsize)] <= t1) {
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
    if ((exact < 0 && direction * (idata[pixind] - threshold) >= 0) || 
        (exact >= 0 && ((testExact && idata[pixind] == exact) || 
                        (!testExact && (idata[pixind] <= t1 || idata[pixind] >= t2))))) {

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

/* Find the boundary contours for a given Z value.  Returns the contour numbers in the 
 * Ilist of ints.  Returns only contours at the given Z value if nearestBound is 0,
 * otherwise returns contours at the nearest Z value */
static int findBoundaryConts(int z, Iobj *boundObj, int nearestBound, Ilist *contList)
{
  int co, minDiff, diff, zmin, zcont;
  ilistTruncate(contList, 0);
  minDiff = 100000000;
  for (co = 0; co < boundObj->contsize; co++) {
    zcont = B3DNINT(boundObj->cont[co].pts->z);
    diff = zcont - z;
    if ((diff < 0 && -diff < minDiff) || (diff >= 0 && diff < minDiff)) {
      minDiff = diff >= 0 ? diff : -diff;
      zmin = zcont;
    }
  }
  if (!nearestBound && minDiff > 0)
    return 0;
  for (co = 0; co < boundObj->contsize; co++) {
    zcont = B3DNINT(boundObj->cont[co].pts->z);
    if (zmin == zcont) {
      if (ilistAppend(contList, &co))
        return 1;
    }
  }
  return 0;
}
