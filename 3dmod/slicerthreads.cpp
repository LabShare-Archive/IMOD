/*
 *  slicerthreads.cpp -- Array filling functions and threads for slicer
 * Was part of slicer_classes prior to 12/21/13
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qthread.h>
#include <qmutex.h>
#include <QTime>
#include "imod.h"
#include "sslice.h"
#include "pyramidcache.h"
#include "xcramp.h"
#include "b3dgfx.h"
#include "xcorr.h"
#include "preferences.h"
#include "sliceproc.h"

static void fillArraySegment(int jstart, int jlimit);
static void findIndexLimits(int isize, int xsize, float xo, float xsx,
                            float offset, float *fstart, float *fend);

#define MAX_THREADS 16

// The thread class saves the limits in Y and run calls the fill with limits
class SlicerThread : public QThread
{
 public:
  SlicerThread(int jStart, int jLimit);
  ~SlicerThread() {};

 protected:
  void run();

 private:
  int mJstart, mJlimit;
};

SlicerThread::SlicerThread(int jStart, int jLimit)
{
  mJstart = jStart;
  mJlimit = jLimit;
}

void SlicerThread::run()
{
  fillArraySegment(mJstart, mJlimit);
}

static QMutex sFillMutex;

// Statics are read-only data for the threads to use
static float sXsx, sYsx, sZsx; /* steps for moving x in zap. */
static float sXsy, sYsy, sZsy; /* steps for moving y in zap. */
static float sXsz, sYsz, sZsz; /* steps for moving z in zap. */
static int sXsize;
static int sYsize;
static int sZsize;
static float sXZoffStart, sYZoffStart, sZZoffStart;
static int sIzoom, sShortcut, sIlimShort, sJlimShort;
static int sIsize, sJsize, sKsize;
static int sNoDataVal;
static b3dInt32 *sCidata;
static int sMaxVal, sMinVal;
static int sSsHQ, sSsWinx;


// The top-level routine called to fill the image array
void SlicerFuncs::fillImageArray(int panning, int meanOnly, int rgbChannel)
{
  float maxPanPixels = 1000. * ImodPrefs->slicerPanKb();
  int i, j, k;
  unsigned short rbase;
  float xo, yo, zo;  /* coords of the lower left zap origin. */
  float xs = 1.0f, ys = 1.0f, zs = 1.0f;  /* scale factors.  */
  float zoffset, sample, scale, offset, sumSD, sumMean, edge, fftBaseScale;
  float matt = 0.1;
  int ixStart, iyStart, nxUse, nyUse, ntaper;
  float numPosSD = 4., numNegSD = 3.;

  float xzoom, yzoom, zzoom;
  float zoom = mZoom;
  int iz, maxSlice;
  float extrashift, numpix;
  int crossget, crosswant;
  Ipoint pnt, tpnt;
  Imat *mat = mMat;
  int tval;

  int cindex, pixSize;
  unsigned int *cmap = App->cvi->cramp->ramp;
  unsigned char *bmap = App->cvi->cramp->bramp;
  b3dUByte **imdata;
  b3dUByte *bdata;
  b3dUInt32 *idata;
  b3dUInt16 *usidata;
  b3dUByte *ubidata;
  Islice *slice;
  //unsigned char *sclmap = NULL;
  unsigned char *fftmap;
  b3dUInt16 *usfftmap;
  unsigned char **linePtrs;
  int vmnullvalue;
  int numThreads = 1;
  char *procChar = NULL;
  SlicerThread *threads[MAX_THREADS];

  QTime fillTime;
  fillTime.start();
  if (!mImage)
      return;
  sCidata = (b3dInt32 *)mImage->id1;
  idata = (b3dUInt32 *)sCidata;
  usidata = (b3dUInt16 *)sCidata;
  ubidata = (b3dUByte *)sCidata;
  pixSize  = b3dGetImageType(NULL, NULL);

  sXsize = mVi->xsize;
  sYsize = mVi->ysize;
  sZsize = mVi->zsize;
  sNoDataVal = 0;
  sMaxVal = mVi->ushortStore ? 65535 : 255;
  fftBaseScale = mVi->ushortStore ? 256. : 1.;
  sMinVal = 0;
  sSsWinx = mWinx;
  sIzoom = (int) zoom;

  // Turn off hq drawing if mean only.
  // Set flag for filling each window pixel if HQ or fractional zoom, unless
  // it is FFT at higher zoom
  // Set flag for doing shortcut HQ method
  // Set number of slices to draw
  sSsHQ = meanOnly ? 0 : mHq;
  mNoPixelZoom = (sSsHQ || sIzoom != mZoom) &&
    (!mFftMode || mZoom < 1.);

  // 12/16/10: There seems to be nothing wrong with doing the shortcut for
  // non-integer zooms!  And it looks a lot nicer
  sShortcut = (sSsHQ && (zoom == sIzoom || zoom > 2.0) &&  (zoom > 1.0) && 
              !mFftMode) ? 1 : 0;
  sKsize = (mVi->colormapImage || meanOnly) ? 1 : mNslice;
 
  // Set up number of threads early so pan limit can be modified
  // Start with ideal number of threads and modify with environment variable
  numThreads = QThread::idealThreadCount();
#ifdef QT_THREAD_SUPPORT
  procChar = getenv("IMOD_PROCESSORS");
  if (procChar)
    numThreads = atoi(procChar);
  numThreads = B3DMIN(MAX_THREADS, B3DMAX(1, numThreads));
  maxPanPixels *= numThreads;
#endif

  // When panning, drop HQ and/or reduce number of slices to maintain
  // a maximum number of pixel have to be found
  if (panning) {

    // Get number of pixels to be computed: number in window, divided by
    // zoom**2 if pixel zoom or shortcut
    numpix = mWinx * mWiny;
    if (!mNoPixelZoom || sShortcut)
      numpix /= mZoom * mZoom;

    // If one slice and it is hq, it is 4 times slower unless shortcut, where
    // it may still be 2 times slower.  Cancel hq if too many pixels
    if (sKsize == 1 && sSsHQ) {
      if (sShortcut)
        numpix *= 2;
      else
        numpix *= 4;
      if (numpix > maxPanPixels) {
        sSsHQ = 0;
        sShortcut = 0;
        mNoPixelZoom = sIzoom != mZoom && (!mFftMode || mZoom < 1.);
      }
    } else if (sKsize > 1) {

      // If multiple slices, assume shortcut is fully effective and increase
      // work just for the HQ interpolation
      if (sSsHQ)
        numpix *= 4;
      maxSlice = B3DMAX(1, (int)(maxPanPixels / numpix));

      // Cancel HQ if too many pixels, or if the maximum slices is dropping
      // by more than 25%
      if (sSsHQ && (maxPanPixels / numpix < 0.75 || 
                   ((float)maxSlice / sKsize < 0.75))) {
        sSsHQ = 0;
        sShortcut = 0;

        // Re-evaluate flag and number of slices
        mNoPixelZoom = sIzoom != mZoom && (!mFftMode || mZoom < 1.);
        numpix = mWinx * mWiny;
        if (!mNoPixelZoom)
          numpix /= mZoom * mZoom;
        maxSlice = B3DMAX(1, (int)(maxPanPixels / numpix));
      }
      sKsize = B3DMIN(sKsize, maxSlice);
    }
    if (imodDebug('s'))
      imodPrintStderr("panning HQ %d sKsize %d\n", sSsHQ, sKsize);
  }
  zoffset = (float)(sKsize - 1) * 0.5;

  /* DNM 5/16/02: force a cache load of the current z slice at least */
  iz = B3DNINT(mCz);
  ivwGetZSection(mVi, iz);

  /* Set up image pointer tables */
  vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
  if (mVi->pyrCache) {
    if (ivwSetupFastTileAccess(mVi, mVi->pyrCache->getBaseIndex(), vmnullvalue, i))
      return;
  } else {
    if (ivwSetupFastAccess(mVi, &imdata, vmnullvalue, &i, 
                           mTimeLock ? mTimeLock : mVi->curTime))
    return;
  }

  ivwSetRGBChannel(rgbChannel);
  sNoDataVal = vmnullvalue;

  transStep();
  rbase = mVi->rampbase;
  if (!App->rgba && App->depth == 8){
    sMinVal = mVi->rampbase;
    sMaxVal = sMinVal + mVi->rampsize;
    sNoDataVal = (unsigned char)sMinVal;
  }


  /* DNM 5/5/03: set lx, ly, lz when cx, cy, cz used to fill array */
  xzoom = yzoom = zzoom = mZoom;
  xo = mLx = mCx;
  yo = mLy = mCy;
  zo = mLz = mCz;
  mLang[0] = mTang[0];
  mLang[1] = mTang[1];
  mLang[2] = mTang[2];

  sXsx = mXstep[b3dX];
  sYsx = mXstep[b3dY];
  sZsx = mXstep[b3dZ];
  sXsy = mYstep[b3dX];
  sYsy = mYstep[b3dY];
  sZsy = mYstep[b3dZ];
  sXsz = mZstep[b3dX];
  sYsz = mZstep[b3dY];
  sZsz = mZstep[b3dZ];

  zs = 1.0f / getZScaleBefore();
  // if ((mScalez) && (mVi->imod->zscale > 0))
  //  zs  = 1.0f/mVi->imod->zscale;

  if (mScalez == SLICE_ZSCALE_AFTER){
    if (mVi->imod->zscale > 0)
      zs = mVi->imod->zscale;
    xzoom = zoom * sqrt((double)
                        ((sXsx * sXsx + sYsx * sYsx + sZsx * sZsx * zs * zs)/
                         (sXsx * sXsx + sYsx * sYsx + sZsx * sZsx)));
    yzoom = zoom * sqrt((double)
                        ((sXsy * sXsy + sYsy * sYsy + sZsy * sZsy * zs * zs)/
                         (sXsy * sXsy + sYsy * sYsy + sZsy * sZsy)));
    zzoom = zoom * sqrt((double)
                        ((sXsz * sXsz + sYsz * sYsz + sZsz * sZsz * zs * zs)/
                         (sXsz * sXsz + sYsz * sYsz + sZsz * sZsz)));
          
    xs = zoom / xzoom;
    ys = zoom / yzoom;
    zs = 1.0;
  }

  /* size of 2-D loop for i, j */
  /* DNM: don't use xzoom, yzoom; make pixels be zoom x zoom */
  sIsize = (int)(mWinx / zoom + 0.9);
  sJsize = (int)(mWiny / zoom + 0.9);

  if (mNoPixelZoom) {
    /* high quality image or fractional zoom */
    sIsize = mWinx; /* calculate each pixel for zoom unless FFT. */
    sJsize = mWiny;
    sXsx /= xzoom;
    sYsx /= xzoom;
    sZsx /= xzoom / zs; 
    sXsy /= yzoom;
    sYsy /= yzoom;
    sZsy /= yzoom / zs;
    xo -= (sIsize / 2) * sXsx;
    yo -= (sIsize / 2) * sYsx;
    zo -= (sIsize / 2) * sZsx;
    xo -= (sJsize / 2) * sXsy;
    yo -= (sJsize / 2) * sYsy;
    zo -= (sJsize / 2) * sZsy;
    mXshift = 0;
    mYshift = 0;
  }else{
    sXsx *= zoom / xzoom;
    sYsx *= zoom / xzoom;
    sZsx *= zs * zoom / xzoom;
    sXsy *= zoom / yzoom;
    sYsy *= zoom / yzoom;
    sZsy *= zs * zoom / yzoom;

    /* Take fractional location of data point within a pixel and
       rotate in 3D to find location in display pixel */

    setForwardMatrix();

    pnt.x = mCx - (int)mCx;
    pnt.y = mCy - (int)mCy;
    pnt.z = mCz - (int)mCz;
    imodMatTransform3D(mat, &pnt, &tpnt);
          
    if (tpnt.x < 0.0)
      tpnt.x += 1.0;
    if (tpnt.y < 0.0)
      tpnt.y += 1.0;

    /* Compute where we want the crosshair to come out in the central
       pixel, and where it will fall with no raster offset, use 
       difference to set raster offset */

    crosswant = (int)(zoom * tpnt.x);  /* don't take nearest int here! */
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (mWinx / 2) % (int)zoom;
    mXshift = crossget - crosswant;
    if (mXshift < 0)
      mXshift += zoom;

    crosswant = (int)(zoom * tpnt.y);
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (mWiny / 2) % (int)zoom;
    mYshift = crossget - crosswant;
    if (mYshift < 0)
      mYshift += zoom;

    extrashift = 0.5;      /* Needed for proper sampling */
    if (zoom == 1.0)
      extrashift = 0.0;

    xo -= ((mWinx / 2 - mXshift) / zoom - extrashift) * sXsx;
    yo -= ((mWinx / 2 - mXshift) / zoom - extrashift) * sYsx;
    zo -= ((mWinx / 2 - mXshift) / zoom - extrashift) * sZsx;
    xo -= ((mWiny / 2 - mYshift) / zoom - extrashift) * sXsy;
    yo -= ((mWiny / 2 - mYshift) / zoom - extrashift) * sYsy;
    zo -= ((mWiny / 2 - mYshift) / zoom - extrashift) * sZsy;
  }

  /* steps per step in Z are independent of HQ versus regular */
  sXsz *= zoom / zzoom;
  sYsz *= zoom / zzoom;
  sZsz *= zs * zoom / zzoom;

  /* Save values of starting position */
  mXo = xo;
  mYo = yo;
  mZo = zo;

  /* Adjust for multiple slices */
  xo -= zoffset * sXsz;
  yo -= zoffset * sYsz;
  zo -= zoffset * sZsz;
  sXZoffStart = xo; 
  sYZoffStart = yo;
  sZZoffStart = zo;

  if (sShortcut) {
    sIlimShort = sIzoom * ((sIsize - 1) / sIzoom - 1);
    sJlimShort = sIzoom * ((sJsize - 1) / sIzoom - 1);
  } else if (!sIzoom) {
    /* DNM 11/22/01: workaround to Intel compiler bug - it insists on
       doing j % sIzoom even when shortcut is 0 */ 
    sIzoom = 1;
  }

  if (imodDebug('s'))
    imodPrintStderr("winx %d winy %d sIsize %d sJsize %d shortcut %d sIlimShort"
                    " %d sJlimShort %d\n", mWinx, mWiny, sIsize, sJsize, 
                    sShortcut, sIlimShort, sIlimShort);
  //  int timeStart = imodv_sys_time();
  /* DNM: don't need to clear array in advance */

#ifdef QT_THREAD_SUPPORT

  // Use threads if image is big enough
  if (numThreads > 1 && sJsize > 8 * numThreads && 
      sIsize * sKsize > 200000 / sJsize) {
    if (imodDebug('s'))
      imodPrintStderr("%d threads - ", numThreads);
    for (i = 0; i < numThreads; i++) {
      threads[i] = new SlicerThread((i * sJsize) / numThreads, 
                                    ((i + 1) * sJsize) / numThreads);
      threads[i]->start();
    }

    // Wait for all threads to finish
    for (i = 0; i < numThreads; i++)
      threads[i]->wait();

  } else
    fillArraySegment(0, sJsize);

#else
  fillArraySegment(0, sJsize);
#endif

  if (sShortcut) {

    /* DNM 1/9/03: deleted quadratic interpolation code, turned cubic code
       into a routine that can be used by tumbler */
    slicerCubicFillin((b3dUInt16 *)sCidata, mWinx, mWiny, sIzoom, sIlimShort, sJlimShort,
		      sMinVal * sKsize, sMaxVal * sKsize, 1);
  }

  // If computing mean only or scaling to mean, get the mean and SD of the
  // slice with some edges cut off
  if (meanOnly || (mScaleToMeanSD && sKsize > 1) || mFftMode) {
    linePtrs = ivwMakeLinePointers(mVi, (unsigned char *)sCidata, mWinx, 
                                   sJsize, MRC_MODE_FLOAT);
    sumSD = 0.;
    if (linePtrs) {
      ixStart = matt * sIsize;
      nxUse = sIsize - 2 * ixStart;
      iyStart = matt * sJsize;
      nyUse = sJsize - 2 * iyStart;
      sample = 10000.0/(((double)nxUse) * nyUse);
      if (sample > 1.0)
        sample = 1.0;
      if (sampleMeanSD(linePtrs, 7, sIsize, sJsize, sample, ixStart, iyStart,
                       nxUse, nyUse, &sumMean, &sumSD))
        sumSD = 0.;
      if (imodDebug('s'))
        imodPrintStderr("Array %d x %d mean %f Sd %f\n", sIsize, sJsize, 
                        sumMean, sumSD);
    }

    // If getting mean only, set values and flag and return now
    if (meanOnly) {
      mOneSliceMean = sumMean;
      mOneSliceSD = sumSD;
      mScaleToMeanSD = true;
      if (imodDebug('s'))
        imodPrintStderr("Mean/SD time %d\n", fillTime.elapsed());
      return;
    }
  }

  // imodPrintStderr("%d msec\n", imodv_sys_time() - timeStart);
  cindex = mImage->width * mImage->height;
  k = sKsize;

  scale = 1. / k;
  offset = 0.;
  if (k > 1) {
    // If scaling to match one slice, set the scaling and adjust the mean and
    // SD to be after the scaling, for FFT scaling
    if (mScaleToMeanSD && sumSD > 0.1 && mOneSliceSD > 0.1) {
      scale = mOneSliceSD / sumSD;
      offset  = mOneSliceMean - sumMean * scale;
      sumMean = mOneSliceMean;
      sumSD = mOneSliceSD;
    } else if (mFftMode) {

      // If just doing FFT, divide mean and SD by # of slices
      sumMean /= k;
      sumSD /= k;
    }
    /*sclmap = get_short_map(scale, offset, 0, 255, MRC_RAMP_LIN, 0, 0);
    if (!sclmap) {
      wprint("\aMemory error getting mapping array.\n");
      return;
      }*/
  }

  // Take FFT if flag is set
  if (mFftMode) {
    slice = sliceCreate(sIsize, sJsize, SLICE_MODE_BYTE);
    if (slice) {

      // Pack data into a byte slice
      bdata = slice->data.b;
      if (mVi->ushortStore) {
        scale /= 256.;
        offset /= 256.;
      }
      for (j = 0; j < sJsize; j++) {
        for (i = j * mWinx; i < j * mWinx + sIsize; i++) {
          tval = (int)(sCidata[i] * scale + offset);
          *bdata++ = (b3dUByte)B3DMAX(0, B3DMIN(255, tval));
        }
      }
      slice->min = 0;
      slice->max = 255;
      if (imodDebug('s'))
        imodPrintStderr("Fill time %d\n", fillTime.elapsed());
      fillTime.start();

      // Taper the edges 
      ntaper = (int)(0.05 * B3DMAX(sIsize, sJsize));
      if (ntaper)
        sliceTaperAtFill(slice, ntaper, 1);
      if (imodDebug('s'))
        imodPrintStderr("Taper time %d  extent %d\n", fillTime.elapsed(),
                        ntaper);
      fillTime.start();

      // Take the FFT
      if (sliceByteBinnedFFT(slice, 1, 0, sIsize - 1, 0, sJsize - 1, &i, &j) > 0) {

        if (imodDebug('s'))
          imodPrintStderr("FFT time %d\n", fillTime.elapsed());
        fillTime.start();

        // Get the edge mean
        bdata = slice->data.b;
        edge = 0.;
        for (i = 0; i < sIsize; i++)
          edge += bdata[i] + bdata[i + sIsize * (sJsize - 1)];
        for (j = 0; j < sJsize; j++)
          edge += bdata[sIsize * j] + bdata[sIsize - 1 + sIsize * j];
        edge /= 2. * (sIsize + sJsize);

        // Unpack the FFT data into the integer array
        // Map edge to mean - numNeg SD's, 255 to mean + numPos SD's
        scale = fftBaseScale;
        offset = 0.;
        if (sumSD > 0.1 && edge < 254.5) {
          scale = (numPosSD + numNegSD) * sumSD / (255 - edge);
          offset = (sumMean - numNegSD * sumSD) - scale * edge;
        }
        if (imodDebug('s'))
          imodPrintStderr("FFT edge %f  scale %f  offset %f\n", edge, scale,
                          offset);
        fftmap = get_byte_map(scale, offset, 0, sMaxVal, 0);
        usfftmap = (b3dUInt16 *)fftmap;
        for (j = 0; j < sJsize; j++) {
          if (!mVi->ushortStore)
            for (i = j * mWinx; i < j * mWinx + sIsize; i++)
              sCidata[i] = fftmap[*bdata++];
          else
            for (i = j * mWinx; i < j * mWinx + sIsize; i++)
              sCidata[i] = usfftmap[*bdata++];
        }

      }
      sliceFree(slice);
      k = 1;
    }
  }

  /* for 8-bit displays, range is less then 256 gray scales. */
  if (!App->rgba && App->depth == 8){
    if (k > 1)
      for (j = 0; j < sJsize; j++)
        for(i = j * mWinx; i < j * mWinx + sIsize; i++){
          tval = (int)(sCidata[i] * scale + offset);
          if (tval > sMaxVal) tval = sMaxVal;
          if (tval < sMinVal) tval = sMinVal;
          ubidata[i] = tval;
        }
    else
      for (j = 0; j < sJsize; j++)
        for(i = j * mWinx; i < j * mWinx + sIsize; i++){
          if (sCidata[i] > sMaxVal) sCidata[i] = sMaxVal;
          if (sCidata[i] < sMinVal) sCidata[i] = sMinVal;
          ubidata[i] = sCidata[i];
        }

  }else{
    switch (pixSize){
    case 1:
      if (k > 1)
        for (j = 0; j < sJsize; j++)
          for(i = j * mWinx; i < j * mWinx + sIsize; i++){
            tval = (int)(sCidata[i] * scale + offset);
            ubidata[i]= (b3dUByte)B3DMAX(0., B3DMIN(255., tval));
          }
      else
        for (j = 0; j < sJsize; j++)
          for(i = j * mWinx; i < j * mWinx + sIsize; i++)
            ubidata[i] = sCidata[i];
      break;
    case 2:
      if (k > 1)
        for (j = 0; j < sJsize; j++)
          for(i = j * mWinx; i < j * mWinx + sIsize; i++){
            tval = (int)(sCidata[i] * scale + offset);
            usidata[i]= (b3dUInt16)B3DMAX(0., B3DMIN(255., tval)) + rbase;
          }
      else
        for (j = 0; j < sJsize; j++)
          for(i = j * mWinx; i < j * mWinx + sIsize; i++)
            usidata[i] = sCidata[i] + rbase;
      break;
    case 4:
      if (mVi->rgbStore) {
        if (k > 1)
          for (j = sJsize - 1; j >= 0; j--)
            for(i = j * mWinx + sIsize - 1; i >= j * mWinx; i--){
              tval = (int)(sCidata[i] * scale + offset);
              sCidata[i] = bmap[B3DMAX(0, B3DMIN(255, tval))];
            }
        else
          for (j = sJsize - 1; j >= 0; j--)
            for(i = j * mWinx + sIsize - 1; i >= j * mWinx; i--)
              sCidata[i] = bmap[sCidata[i]];

      } else {
        if (k > 1)
          for (j = sJsize - 1; j >= 0; j--)
            for(i = j * mWinx + sIsize - 1; i >= j * mWinx; i--){
              tval = (int)(sCidata[i] * scale + offset);
              idata[i] = cmap[B3DMAX(0, B3DMIN(sMaxVal, tval))];
            }
        else
          for (j = sJsize - 1; j >= 0; j--)
            for(i = j * mWinx + sIsize - 1; i >= j * mWinx; i--)
              idata[i] = cmap[sCidata[i]];
      }
    }
  }
  mXzoom = xzoom;
  mYzoom = yzoom;
  /*  if (sclmap)
      free(sclmap); */
  if (imodDebug('s'))
    imodPrintStderr("Fill time %d\n", fillTime.elapsed());
  return;
}

// Find the limits that need to be filled for a particular line
static void findIndexLimits(int isize, int xsize, float xo, float xsx,
                            float offset, float *fstart, float *fend)
{
  float flower, fupper, ftmp;
  float endCoord = xo + (isize - 1) * xsx + offset;
  float startCoord = xo + offset;
 
  /*if (imodDebug('s')) 
    imodPrintStderr("xo = %f, xsx = %f, start = %f, end = %f\n", xo,xsx,startCoord, endCoord); */
  /* If start and end is all to one side of data, set limits to middle to skip
     the line.  3/17/10: it needs an epsilon because of errors at Z -180 deg */
  if ((startCoord < 0.1 && endCoord < 0.1) || 
      (startCoord >= xsize - 0.1 && endCoord >= xsize - 0.1)) {
    *fstart = isize / 2.;
    *fend = *fstart;
 
    /* Otherwise evaluate place where line cuts volume for this coordinate */
  } else if (xsx > 1.e-6 || xsx < -1.e-6) {
    flower = (0.1 - startCoord) / xsx;
    fupper = (xsize - 0.1 - startCoord) / xsx;
    if (xsx < 0) {
      ftmp = flower;
      flower = fupper;
      fupper = ftmp;
    }
    /* if (imodDebug('s')) 
       imodPrintStderr("lower = %f, upper = %f\n", flower, fupper); */
    if (flower > *fstart)
      *fstart = flower;
    if (fupper < *fend)
      *fend = fupper;
  }
}

// Fill a portion of the array from jstart to jlimit - 1
// This routine is called by the threads
static void fillArraySegment(int jstart, int jlimit)
{
  int i, j, k, cindex, ishort;
  int xi, yi, zi;
  float xo, yo, zo;  /* coords of the lower left origin. */
  float x, y, z; /* coords of pixel in 3-D image block. */
  float xzo, yzo,zzo;
  int innerStart, innerEnd, outerStart, outerEnd;
  float fstart, fend;

  /* for 3-D quadratic interpolation */
  float dx, dy, dz;
  float x1, x2, y1, y2, z1, z2;
  float a, b, c, d, e, f;
  float ival;
  int pxi, nxi, pyi, nyi, pzi, nzi;
  int val;

  xzo = sXZoffStart;
  yzo = sYZoffStart;
  zzo = sZZoffStart;


  for (k = 0; k < sKsize; k++) {
    xo = xzo;
    yo = yzo;
    zo = zzo;

    // advance coordinates to get to j start
    for (j = 0; j < jstart; j++) {
      xo += sXsy;
      yo += sYsy;
      zo += sZsy;
    }    
          
    /* (i,j) location in zap window data. */
    for (j = jstart; j < jlimit; j++){

      // Take the lock for working on first or last line (superstition)
      if (j == jstart || j == jlimit - 1)
        sFillMutex.lock();

      /* Compute starting and ending index that intersects data volume
         for each dimension, and find smallest range of indexes */
      fstart = 0;
      fend = sIsize;
      findIndexLimits(sIsize, sXsize, xo, sXsx, 0., &fstart, &fend);
      findIndexLimits(sIsize, sYsize, yo, sYsx, 0., &fstart, &fend);
      findIndexLimits(sIsize, sZsize, zo, sZsx, 0.5, &fstart, &fend);

      /* If there is no range, set up for fills to cover the range */
      if (fstart >= fend) {
        outerStart = sIsize / 2;
        innerStart = innerEnd = outerEnd = outerStart;
      } else {

        /* Otherwise, set outer region safely outside the index limits */
        outerStart = fstart - 2.;
        if (outerStart < 0)
          outerStart = 0;
        outerEnd = fend + 2.;
        if (outerEnd > sIsize)
          outerEnd = sIsize;

        /* If not doing HQ, compute inner limits of region that needs no
           testing */
        if (!sSsHQ) {
          innerStart = outerStart + 4;
          innerEnd = outerEnd - 4;
          if (innerStart >= innerEnd)
            innerStart = innerEnd = outerStart;
          
        } else if (sShortcut) {
          /* If doing shortcuts, set up for whole line if it is a line to skip
             or make sure start is a multiple of the zoom */
          if (j >= sIzoom && j < sJlimShort && j % sIzoom) {
            outerStart = 0;
            outerEnd = sIsize;
          } else
            outerStart = sIzoom * (outerStart / sIzoom);
        }

      }

      cindex = j * sSsWinx;
      
      /* Fill outer regions */
      
      if (k) {
        for (i = 0; i < outerStart; i++)
          sCidata[i + cindex] += sNoDataVal;
        for (i = outerEnd; i < sIsize; i++)
          sCidata[i + cindex] += sNoDataVal;
      } else {
        for (i = 0; i < outerStart; i++)
          sCidata[i + cindex] = sNoDataVal;
        for (i = outerEnd; i < sIsize; i++)
          sCidata[i + cindex] = sNoDataVal;
      }

      x = xo + outerStart * sXsx;
      y = yo + outerStart * sYsx;
      z = zo + outerStart * sZsx;

      if (sSsHQ) {
        /* For HQ, do tests all the time since they are minor component */
        // Also use the increments to step between points, because computing from 
        // i * sXsx etc is > 20% slower for some reason here
        for (i = outerStart; i < outerEnd; i++) {

          /* DNM & RJG 2/12/03: remove floor calls - they are dog-slow only
             Pentium 4 below 2.6 GHz... */
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < sXsize && yi >= 0 && yi < sYsize &&
              z > -0.5 && zi < sZsize) {
            val = (*ivwFastGetValue)(xi, yi, zi);

            /* do quadratic interpolation. */
            dx = x - xi - 0.5;
            dy = y - yi - 0.5;
            dz = z - zi;
                              
            pxi = xi - 1;
            nxi = xi + 1;
            pyi = yi - 1;
            nyi = yi + 1;
            pzi = zi - 1;
            nzi = zi + 1;
                              
            if (pxi < 0) pxi = 0;
            if (nxi >= sXsize) nxi = xi;
            if (pyi < 0) pyi = 0;
            if (nyi >= sYsize) nyi = yi;
            if (pzi < 0) pzi = 0;
            if (nzi >= sZsize) nzi = zi;
                            
            x1 = (*ivwFastGetValue)(pxi,  yi,  zi);
            x2 = (*ivwFastGetValue)(nxi,  yi,  zi);
            y1 = (*ivwFastGetValue)( xi, pyi,  zi);
            y2 = (*ivwFastGetValue)( xi, nyi,  zi);
            z1 = (*ivwFastGetValue)( xi,  yi, pzi);
            z2 = (*ivwFastGetValue)( xi,  yi, nzi);
                              
            a = (x1 + x2) * 0.5f - (float)val;
            b = (y1 + y2) * 0.5f - (float)val;
            c = (z1 + z2) * 0.5f - (float)val;
            d = (x2 - x1) * 0.5f;
            e = (y2 - y1) * 0.5f;
            f = (z2 - z1) * 0.5f;
            ival = (a * dx * dx) + 
              (b * dy * dy) + 
              (c * dz * dz) +
              (d * dx) + (e * dy) + 
              (f * dz) + (float)val;
            if (ival > sMaxVal)
              ival = sMaxVal;
            if (ival < sMinVal)
              ival = sMinVal;
            val = (int)(ival + 0.5f);
                              
          } else
            val = sNoDataVal;
                    
          if (k)
            sCidata[i + cindex] += val;
          else
            sCidata[i + cindex] = val;
                    
          x += sXsx;
          y += sYsx;
          z += sZsx;

          if (sShortcut != 0 && i >= sIzoom && i < sIlimShort && 
              j >= sIzoom && j < sJlimShort) {
            ishort = sIzoom - 1;
            if (j % sIzoom)
              ishort = sIlimShort - sIzoom - 1;
            x += sXsx * ishort;
            y += sYsx * ishort;
            z += sZsx * ishort;
            i += ishort;
          }
        }
      } else {

        /* Non HQ data */
        // Here we should compute coordinates with multiplication since there is no
        // testing, but it costs < 10% for some reason.
        for (i = outerStart; i < innerStart; i++) {
          x = xo + i * sXsx;
          y = yo + i * sYsx;
          z = zo + i * sZsx;
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < sXsize && yi >= 0 && yi < sYsize &&
              z > -0.5 && zi < sZsize)
            val = (*ivwFastGetValue)(xi, yi, zi);
          else
            val = sNoDataVal;
                    
          if (k)
            sCidata[i + cindex] += val;
          else
            sCidata[i + cindex] = val;
                    
        }

        if (k) {
          for (i = innerStart; i < innerEnd; i++) {
            x = xo + i * sXsx;
            y = yo + i * sYsx;
            z = zo + i * sZsx;
            xi = (int)x;
            yi = (int)y;
            zi = (int)(z + 0.5);
            val = (*ivwFastGetValue)(xi, yi, zi);
            sCidata[i + cindex] += val;
          }
        } else {
          for (i = innerStart; i < innerEnd; i++) {
            x = xo + i * sXsx;
            y = yo + i * sYsx;
            z = zo + i * sZsx;
            xi = (int)x;
            yi = (int)y;
            zi = (int)(z + 0.5);
            /*if (xi >= 0 && xi < sXsize && yi >= 0 && yi < sYsize &&
              zi >= 0 && zi < sZsize) */
            val = (*ivwFastGetValue)(xi, yi, zi);
            /* else {
              imodPrintStderr("BAD %d %d %d %d %d %f %f %d %d\n", i, j, xi, yi,
                              zi, fstart, fend, innerStart, innerEnd);
              fstart = 0;
              fend = sIsize;
              findIndexLimits(sIsize, sXsize, xo, sXsx, 0., &fstart, &fend);
              imodPrintStderr("X %f %f %f\n", sXsx, fstart, fend); 
              fstart = 0;
              fend = sIsize;
              findIndexLimits(sIsize, sYsize, yo, sYsx, 0., &fstart, &fend);
              imodPrintStderr("Y %f %f %f %f\n", yo, sYsx, fstart, fend); 
              fstart = 0;
              fend = sIsize;
              findIndexLimits(sIsize, sZsize, zo, sZsx, 0.5, &fstart, &fend);
              imodPrintStderr("Z %f %f %f\n", sZsx, fstart, fend); 
              } */
            sCidata[i + cindex] = val;
          }
        }

        for (i = innerEnd; i < outerEnd; i++) {
          x = xo + i * sXsx;
          y = yo + i * sYsx;
          z = zo + i * sZsx;
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < sXsize && yi >= 0 && yi < sYsize &&
              z > -0.5 && zi < sZsize)
            val = (*ivwFastGetValue)(xi, yi, zi);
          else
            val = sNoDataVal;
                    
          if (k)
            sCidata[i + cindex] += val;
          else
            sCidata[i + cindex] = val;
                    
        }

      }
      xo += sXsy;
      yo += sYsy;
      zo += sZsy;

      if (j == jstart || j == jlimit - 1)
        sFillMutex.unlock();
    }
    xzo += sXsz;
    yzo += sYsz;
    zzo += sZsz;
  }
}
