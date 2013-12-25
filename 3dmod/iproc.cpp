/*
 *  iproc.cpp -- image processing for 3dmod.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qlabel.h>
#include <QStackedWidget>
#include <qlayout.h>
#include <qtooltip.h>
#include <qcombobox.h>
#include <qlistwidget.h>
#include <qspinbox.h>
#include <QDoubleSpinBox>
#include <qcheckbox.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <QButtonGroup>
#include <QGroupBox>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QTimerEvent>
#include <QKeyEvent>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "dia_qtutils.h"
#include "tooledit.h"
#include "multislider.h"
#include "imod.h"
#include "display.h"
#include "iproc.h"
#include "sliceproc.h"
#include "xcorr.h"
#include "xzap.h"
#include "info_setup.h"
#include "info_cb.h"
#include "control.h"
#include "preferences.h"

/* internal functions. */
static void clearsec(ImodIProc *ip);
static void savesec(ImodIProc *ip);
static void cpdslice(Islice *sl, ImodIProc *ip);
static void copyAndDisplay();
static void imageToBuffer(ImodIProc *ip, unsigned char **image, unsigned char *buf);
static int savedToImage(ImodIProc *ip);
static void setSliceMinMax(bool actual);
static void freeArrays(ImodIProc *ip);
static void  setUnscaledK();

static void edge_cb();
static void mkedge_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void thresh_cb();
static void mkthresh_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void smooth_cb();
static void sharpen_cb();
static void mkFourFilt_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void fourFilt_cb();
static void mkFFT_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void fft_cb();
static void mkMedian_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void mkSmooth_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void median_cb();
static void mkAnisoDiff_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void anisoDiff_cb();

#define NO_KERNEL_SIGMA 0.4f
#define KERNEL_MAXSIZE 7

/* The table of entries and callbacks */
ImodIProcData proc_data[] = {
  {"FFT", fft_cb, mkFFT_cb, NULL},
  {"Fourier filter", fourFilt_cb, mkFourFilt_cb, NULL},
  {"smooth", smooth_cb, mkSmooth_cb, NULL},
  {"median", median_cb, mkMedian_cb, NULL},
  {"diffusion", anisoDiff_cb, mkAnisoDiff_cb, NULL},
  {"edge", edge_cb, mkedge_cb, NULL},
  {"sharpen", sharpen_cb, NULL, "Sharpen Edges."},
  {"threshold", thresh_cb, mkthresh_cb, NULL},
  {NULL, NULL, NULL, NULL}
};

/* Static variables for proc structure and a slice */
static ImodIProc proc = {0,0,0,0,0,0,{0,0},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static Islice s;

/*
 * CALLBACK FUNCTIONS FOR THE VARIOUS FILTERS
 */

// New rule 11/07/04: Set the desired output min and max before calling 
// routines in sliceproc or xcorr to either the data range with 
// setSliceMinMax(false) or the existing input range with  setSliceMinMax(true)

// Edge enhancement
static void edge_cb()
{
  ImodIProc *ip = &proc;
  Islice *gs;

  switch (ip->edge){
  case 0:
    setSliceMinMax(false);
    sliceByteEdgeSobel(&s);
    break;

  case 1:
    setSliceMinMax(false);
    sliceByteEdgePrewitt(&s);
    break;

  case 2:
    setSliceMinMax(false);
    sliceByteEdgeLaplacian(&s);
    break;
	  
  case 3:
    setSliceMinMax(false);
    sliceByteGraham(&s);
    break;

  case 4:
    gs = sliceGradient(&s);
    if (!gs) return;
    cpdslice(gs, ip);
    break;

  default:
    break;
  }
}

// Threshold
static void thresh_cb()
{
  ImodIProc *ip = &proc;
  int xysize, thresh, minv, maxv;
  unsigned char *idat, *last;
     
  setSliceMinMax(false);
  thresh = ip->threshold;
     
  if (App->depth == 8){
    thresh = (int)
      ((((float)ip->vi->rampsize/256.0f)*thresh) + ip->vi->rampbase);
    minv = ip->vi->rampbase;
    maxv = ip->vi->rampsize + minv - 1;
  }else{
    minv = 0; maxv = 255;
  }

  xysize = ip->vi->xsize * ip->vi->ysize;
  idat = ip->iwork;
  for(last = idat + xysize; idat != last; idat++){
    if (*idat > thresh)
      *idat = maxv;
    else
      *idat = minv;
  }

  if (ip->threshGrow)
    sliceByteGrow(&s,  (int)s.max);
  if (ip->threshShrink)
    sliceByteShrink(&s,  (int)s.max);
}

// Smoothing
static void smooth_cb()
{
  float kernel[KERNEL_MAXSIZE*KERNEL_MAXSIZE];
  int dim;
  Islice *sout;
  s.min = s.max = 0.;
  if (proc.rescaleSmooth)
    setSliceMinMax(true);
  if (proc.kernelSigma > NO_KERNEL_SIGMA + 0.001) {
    // Why was this one using:     sliceMMM(&s);
    scaledGaussianKernel(kernel, &dim, KERNEL_MAXSIZE, proc.kernelSigma);
    sout = slice_mat_filter(&s, kernel, dim);
    sliceScaleAndFree(sout, &s);
  } else {
    sliceByteSmooth(&s);
  }
}

// Sharpening
static void sharpen_cb()
{
  setSliceMinMax(true);
  sliceByteSharpen(&s);
}

// Fourier filter
static void fourFilt_cb()
{
  ImodIProc *ip = &proc;
  setSliceMinMax(true);
  sliceFourierFilter(&s, ip->sigma1, ip->sigma2, ip->radius1, ip->radius2);
}

// FFT
static void fft_cb()
{
  int ix0, ix1, iy0, iy1, nxuse, nyuse;
  ix0 = iy0 = 0;
  nxuse = s.xsize;
  nyuse = s.ysize;
  if (proc.fftSubset)
    zapSubsetLimits(proc.vi, ix0, iy0, nxuse, nyuse);
  ix1 = ix0 + nxuse - 1;
  iy1 = iy0 + nyuse - 1;
  setSliceMinMax(false);
  proc.fftScale = sliceByteBinnedFFT(&s, proc.fftBinning, ix0, ix1, iy0, iy1,
                                     &proc.fftXcen, &proc.fftYcen);
}

// Median filter
static void median_cb()
{
  unsigned char *to;
  unsigned char **from;
  int i, j, z;
  ImodIProc *ip = &proc;
  int depth = ip->median3D ? ip->medianSize : 1;
  int zst = B3DMAX(0, ip->idatasec - depth / 2);
  int znd = B3DMIN(ip->vi->zsize - 1, ip->idatasec + (depth - 1) / 2);

  ip->medianVol.zsize = depth = znd + 1 - zst;
  ip->medianVol.vol = (Islice **)malloc(depth * sizeof(Islice *));
  if (!ip->medianVol.vol)
    return;

  for (i = 0, z = zst; z <= znd; z++, i++) {

    // Get a slice and get the array of pointers
    ip->medianVol.vol[i] = sliceCreate(ip->vi->xsize, ip->vi->ysize, 
                                       SLICE_MODE_BYTE);
    from = ivwGetZSectionTime(ip->vi, z, ip->idatatime);

    // If either is in error, clean up what is already allocated
    if (!from || !ip->medianVol.vol[i]) {
      for (j = 0; j <= i; j++)
        if (ip->medianVol.vol[j])
          sliceFree(ip->medianVol.vol[j]);
      free(ip->medianVol.vol);
      return;
    }

    // Copy data
    to = ip->medianVol.vol[i]->data.b;
    imageToBuffer(ip, from, to);
  }

  sliceMedianFilter(&s, &ip->medianVol, ip->medianSize);

  // Clean up
  for (j = 0; j < depth; j++)
    sliceFree(ip->medianVol.vol[j]);
  free(ip->medianVol.vol);
}

// Anisotropic diffusion
static void anisoDiff_cb()
{
  ImodIProc *ip = &proc;

  // Get this memory only when needed because it is so big
  if (!ip->andfImage) {
    ip->andfImage = allocate2D_float(ip->vi->ysize + 2, ip->vi->xsize + 2);
    if (ip->andfImage) {
      ip->andfImage2 = allocate2D_float(ip->vi->ysize + 2, ip->vi->xsize + 2);
      if (!ip->andfImage2) {
        free(ip->andfImage[0]);
        free(ip->andfImage);
        ip->andfImage = NULL;
      }
    }
  }
  if (!ip->andfImage) {
    wprint("\aCould not get memory for diffusion\n");
    return;
  }
  ip->andfK = ip->andfKEdit->text().toDouble();
  ip->andfLambda = ip->andfLambdaEdit->text().toDouble();
  sliceByteAnisoDiff(&s, ip->andfImage, ip->andfImage2, ip->andfStopFunc + 2,
                     ip->andfK, ip->andfLambda, ip->andfIterations, 
                     &ip->andfIterDone);
}

// Set the min and max of the static slice to full range, or actual values
static void setSliceMinMax(bool actual)
{
  if (actual) {
    sliceMinMax(&s);
  } else if (App->depth == 8){
    s.min = proc.vi->rampbase;
    s.max = proc.vi->rampsize + s.min - 1;
  } else {
    s.min = 0;
    s.max = 255;
  }
}


/* Reset and get new data buffer */
int iprocRethink(struct ViewInfo *vi)
{
  if (proc.dia){
    if (proc.isaved) {
      clearsec(&proc);
      proc.idatasec = -1;
      freeArrays(&proc);
    }
    proc.isaved = (unsigned char *)malloc(vi->xsize * vi->ysize);
    proc.iwork = (unsigned char *)malloc(vi->xsize * vi->ysize);

    proc.andfIterDone = 0;
    proc.andfDoneLabel->setText("0 done");

    if (!proc.isaved || !proc.iwork) {
      freeArrays(&proc);
      wprint("\aCannot get new memory for processing window!\n");
      proc.dia->close();
      return 1;
    }

    proc.dia->limitFFTbinning();
  }
  return 0;
}

/* Update for changes in the system (i.e., autoapply if section changed */
void iprocUpdate(void)
{
  if (!proc.dia || !proc.autoApply)
    return;
  if (proc.vi->loadingImage || proc.dia->mRunningProc)
    return;

  /* If time or section has changed, do an apply */
  if (B3DNINT(proc.vi->zmouse) != proc.idatasec || 
      proc.vi->curTime != proc.idatatime) 
    proc.dia->apply();
}

/* Open the processing dialog box */
int inputIProcOpen(struct ViewInfo *vi)
{
  size_t dataSize = (size_t)vi->xsize * (size_t)vi->ysize;
  if (dataSize > 2147000000) {
    wprint("\aData are too large too apply image processing to\n");
    return 1;
  }
  dataSize *= ivwGetPixelBytes(vi->rawImageStore);
  if (!proc.dia){
    if (!proc.vi) {
      proc.procnum = 0;
      proc.autoApply = false;
      proc.threshold = 128;
      proc.threshGrow = false;
      proc.threshShrink = false;
      proc.edge = 0;
      proc.kernelSigma = NO_KERNEL_SIGMA;
      proc.rescaleSmooth = true;
      proc.sigma1 = 0.;
      proc.sigma2 = 0.05f;
      proc.radius1 = 0.;
      proc.radius2 = 0.5f;
      proc.fftBinning = 1;
      proc.fftScale = 0.;
      proc.fftSubset = false;
      proc.medianSize = 3;
      proc.median3D = true;
      proc.andfK = 2.;
      proc.andfStopFunc = 0;
      proc.andfLambda = 0.2;
      proc.andfIterations = 5;
      proc.isaved = NULL;
      proc.iwork = NULL;
      proc.andfImage = NULL;
      proc.andfImage2 = NULL;
    }
    proc.vi = vi;
    proc.idatasec = -1;
    proc.idatatime = 0;
    proc.modified = 0;
    proc.andfIterDone = 0;

    proc.isaved = (unsigned char *)malloc(dataSize);
    proc.iwork = (unsigned char *)malloc(dataSize);

    if (!proc.isaved || !proc.iwork) {
      freeArrays(&proc);
      wprint("\aCannot get memory for processing window!\n");
      return(-1);
    }

    proc.dia = new IProcWindow(imodDialogManager.parent(IMOD_DIALOG), NULL);
    imodDialogManager.add((QWidget *)proc.dia, IMOD_DIALOG);
    adjustGeometryAndShow((QWidget *)proc.dia, IMOD_DIALOG);

  }else{
    proc.dia->raise();
  }
  return(0);
}

static void freeArrays(ImodIProc *ip)
{
  if (ip->isaved)
    free(ip->isaved);
  ip->isaved = NULL;
  if (ip->iwork)
    free(ip->iwork);
  ip->iwork = NULL;
  if (ip->andfImage) {
    free(ip->andfImage[0]);
    free(ip->andfImage);
    ip->andfImage = NULL;
  }
  if (ip->andfImage2) {
    free(ip->andfImage2[0]);
    free(ip->andfImage2);
    ip->andfImage2 = NULL;
  }
}

// Inform other program components if thread is busy
bool iprocBusy(void)
{
#ifdef QT_THREAD_SUPPORT
  if (!proc.dia)
    return false;
  return proc.dia->mRunningProc;
#else
  return false;
#endif
}

// If the thread is busy, save the callback function; otherwise call it now
void iprocCallWhenFree(void (*func)())
{
  if (proc.dia && iprocBusy())
    proc.dia->mCallback = func;
  else
    func();
}

/*
 * DATA COPYING FUNCTIONS
 */

/* Copy data from a generated slice into the working buffer and free slice */
static void cpdslice(Islice *sl, ImodIProc *ip)
{
  register unsigned char *from, *to, *last;
  int rampbase = ip->vi->rampbase;
  from = sl->data.b;
  to = ip->iwork;
  if (!to) return;

  last = to + (ip->vi->xsize * ip->vi->ysize);
  if (App->depth > 8){
    do{
      *to++ = *from++;
    }while (to !=  last);
  }else{
    do{
      *to++ = *from++ + rampbase;
    }while (to !=  last);
  }
  sliceFree(sl);
}

/* Copy the working buffer back to the display memory and draw */
static void copyAndDisplay()
{
  ImodIProc *ip = &proc;
  unsigned char **to = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  unsigned char *from = ip->iwork;
  b3dUInt16 **usto = (b3dUInt16 **)to;
  int i, j;
  float slope;
  b3dUInt16 *usmap = NULL;
  int cz =  (int)(ip->vi->zmouse + 0.5f);
  
  if (ip->vi->ushortStore) {
    slope = (ip->rangeHigh - ip->rangeLow) / 255.;
    usmap = (b3dUInt16 *)get_byte_map(slope, (float)ip->rangeLow, 0, 65535, 0);
  }
  for (j = 0; j < ip->vi->ysize; j++) {
    if (usmap)
      for (i = 0; i < ip->vi->xsize; i++)

        usto[j][i] = usmap[*from++];
    else
      for (i = 0; i < ip->vi->xsize; i++)
        to[j][i] = *from++;
  }

  imod_info_float_clear(cz, ip->vi->curTime);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
}


/* clear the section back to original data. */
static void clearsec(ImodIProc *ip)
{
  unsigned char **savePtrs;
     
  if (ip->idatasec < 0 || !ip->modified)
    return;

  savePtrs = ivwMakeLinePointers(ip->vi, ip->isaved, ip->vi->xsize, ip->vi->ysize,
                                 ip->vi->rawImageStore);
  if (!savePtrs)
    return;
  imageToBuffer(ip, savePtrs, ip->iwork);
  if (savedToImage(ip))
    return;

  ip->modified = 0;
  imod_info_float_clear(ip->idatasec, ip->idatatime);
}

/* save the displayed image to saved and working buffers. */
static void savesec(ImodIProc *ip)
{
  unsigned char **image;
  int j;
  unsigned char *isaved = ip->isaved;
  int numbytes = ivwGetPixelBytes(ip->vi->rawImageStore) * ip->vi->xsize;
     
  if (ip->idatasec < 0)
    return;

  image = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  if (!image) 
    return;
  imageToBuffer(ip, image, ip->iwork);
  for (j = 0; j < ip->vi->ysize; j++) {
    memcpy(isaved, image[j], numbytes);
    isaved += numbytes;
  }
}

/* Copy an image described by line pointers to a byte buffer */
static void imageToBuffer(ImodIProc *ip, unsigned char **image, unsigned char *buf)
{
  if (ivwCopyImageToByteBuffer(ip->vi, image, buf))
    return;
  if (ip->vi->ushortStore) {
    ip->rangeLow = ip->vi->rangeLow;
    ip->rangeHigh = ip->vi->rangeHigh;
  }
}

/* Copy saved buffer back to current image */
static int savedToImage(ImodIProc *ip)
{
  unsigned char *isaved = ip->isaved;
  int numbytes = ivwGetPixelBytes(ip->vi->rawImageStore) * ip->vi->xsize;
  int j;
  unsigned char **image = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  if (!image)
    return 1;
  for (j = 0; j < ip->vi->ysize; j++) {
    memcpy(image[j], isaved, numbytes);
    isaved += numbytes;
  }
  return 0;
}

/*
 * FUNCTIONS TO MAKE THE WIDGETS FOR PARTICULAR FILTERS
 */
static void mkedge_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  diaLabel("Edge Enhancement Filter Type:", parent, layout);
  QComboBox *edgeBox = new QComboBox(parent);
  layout->addWidget(edgeBox);
  QStringList items;
  items << "Sobel" << "Prewitt" << "Laplacian" << "Graham" << "Gradient";
  edgeBox->addItems(items);
  edgeBox->setFocusPolicy(Qt::NoFocus);
  edgeBox->setCurrentIndex(proc.edge);
  QObject::connect(edgeBox, SIGNAL(activated(int)), win, 
                   SLOT(edgeSelected(int)));
}

static void mkSmooth_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  diaLabel("Smoothing", parent, layout);
  diaLabel(" Uses standard 3x3 kernel", parent, layout);
  diaLabel(" or Gaussian kernel if sigma set", parent, layout);
  QHBoxLayout *hLayout = diaHBoxLayout(layout);
  proc.kernelSpin = (QDoubleSpinBox *)diaLabeledSpin
    (2, NO_KERNEL_SIGMA, 10., 0.1f, "Kernel sigma", parent, hLayout);
  diaLabel("pixels", parent, hLayout);
  proc.kernelSpin->setSpecialValueText("None");
  proc.kernelSpin->setValue(proc.kernelSigma);
  QObject::connect(proc.kernelSpin, SIGNAL(valueChanged(double)), win, 
                   SLOT(kernelChanged(double)));

  QCheckBox *check = diaCheckBox("Rescale to match min/max", parent, layout);
  diaSetChecked(check, proc.rescaleSmooth);
  QObject::connect(check, SIGNAL(toggled(bool)), win,
                   SLOT(scaleSmthToggled(bool)));
  check->setToolTip("Rescale smoothed slice so its min/max matches original"
                " slice");

}

static void mkthresh_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  const char *sliderLabel[] = {"Threshold filter value" };
  MultiSlider *slider = new MultiSlider(parent, 1, sliderLabel, 0, 254);
  slider->setValue(0, proc.threshold);
  QObject::connect(slider, SIGNAL(sliderChanged(int, int, bool)), win, 
          SLOT(threshChanged(int, int, bool)));
  layout->addLayout(slider->getLayout());
  QCheckBox *check = diaCheckBox("Grow thresholded area", parent, layout);
  diaSetChecked(check, proc.threshGrow);
  QObject::connect(check, SIGNAL(toggled(bool)), win, SLOT(growChanged(bool)));
  check->setToolTip("Apply dilation to grow area selected area ");
  check = diaCheckBox("Shrink thresholded area", parent, layout);
  diaSetChecked(check, proc.threshShrink);
  QObject::connect(check, SIGNAL(toggled(bool)), win, 
                   SLOT(shrinkChanged(bool)));
  check->setToolTip("Apply erosion to shrink selected area");
}

static void mkFourFilt_cb(IProcWindow *win, QWidget *parent,
                          QVBoxLayout *layout)
{
  const char *sliderLabel[] = {"Low-frequency sigma", "High-frequency cutoff",
                         "High-frequency falloff"};
  diaLabel("Filtering in Fourier Space", parent, layout);
  MultiSlider *slider = new MultiSlider(parent, 3, sliderLabel, 0, 200, 3);
  slider->setRange(1, 0, 500);
  slider->setRange(2, 1, 200);
  slider->setValue(0, (int)(1000. * proc.sigma1));
  slider->setValue(1, (int)(1000. * proc.radius2));
  slider->setValue(2, (int)(1000. * proc.sigma2));
  QObject::connect(slider, SIGNAL(sliderChanged(int, int, bool)), win, 
          SLOT(fourFiltChanged(int, int, bool)));
  layout->addLayout(slider->getLayout());
  slider->getSlider(0)->setToolTip("Sigma for inverted Gaussian"
                " high-pass filter (0 at origin)");
  slider->getSlider(1)->setToolTip("Cutoff radius for Gaussian low-pass filter");
  slider->getSlider(2)->setToolTip("Sigma for Gaussian low-pass filter starting at cutoff"
                                   );
}

static void mkFFT_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  diaLabel("Fourier transform", parent, layout);
  
  QHBoxLayout *hLayout = diaHBoxLayout(layout);
  proc.fftBinSpin = (QSpinBox *)diaLabeledSpin(0, 1, 8, 1, "Binning", parent,
                                               hLayout);
  QObject::connect(proc.fftBinSpin, SIGNAL(valueChanged(int)), win, 
                   SLOT(binningChanged(int)));
  QCheckBox *check = diaCheckBox("Use Zap window subarea", parent, layout);
  diaSetChecked(check, proc.fftSubset);
  QObject::connect(check, SIGNAL(toggled(bool)), win,
                   SLOT(subsetChanged(bool)));
  check->setToolTip("Do FFT of area displayed or within rubber band "
                "in active Zap window");

  proc.fftLabel1 = diaLabel("  ", parent, layout);
  proc.fftLabel2 = diaLabel("  ", parent, layout);

  hLayout = diaHBoxLayout(layout);
  hLayout->addStretch(0);
  proc.freqButton = diaPushButton("Report frequency", parent, hLayout);
  QObject::connect(proc.freqButton, SIGNAL(clicked()), win,
                   SLOT(reportFreqClicked()));
  proc.freqButton->setToolTip(
                "Compute resolution at current marker or model point");
  proc.freqButton->setEnabled(false);
  hLayout->addStretch(0);

  proc.fftLabel3 = diaLabel("  ", parent, layout);
  win->limitFFTbinning();
}

static void mkMedian_cb(IProcWindow *win, QWidget *parent, 
                        QVBoxLayout *layout)
{
  diaLabel("Median filter", parent, layout);
  QHBoxLayout *hLayout = diaHBoxLayout(layout);
  QSpinBox *sizeSpin = (QSpinBox *)diaLabeledSpin(0., 2., 9., 1., "Size",
                                                  parent, hLayout);
  diaSetSpinBox(sizeSpin, proc.medianSize);
  QObject::connect(sizeSpin, SIGNAL(valueChanged(int)), win, 
                   SLOT(medSizeChanged(int)));
  QCheckBox *check = diaCheckBox("Compute median in 3D cube", parent, layout);
  diaSetChecked(check, proc.median3D);
  QObject::connect(check, SIGNAL(toggled(bool)), win,
                   SLOT(med3DChanged(bool)));
  check->setToolTip("Take median in 3D cube instead of median in 2D within"
                " this section");
}

static void mkAnisoDiff_cb(IProcWindow *win, QWidget *parent,
                           QVBoxLayout *layout)
{
  QString str;
  diaLabel("Anisotropic diffusion", parent, layout);

  // The edge stopping radio buttons
  QGroupBox *gbox = new QGroupBox("Edge Stopping Function", parent);
  layout->addWidget(gbox);
  QVBoxLayout *gbLayout = new QVBoxLayout(gbox);
  gbLayout->setSpacing(0);
  gbLayout->setContentsMargins(5, 2, 5, 5);
  QButtonGroup *stopGroup = new QButtonGroup(parent);
  QRadioButton *radio = diaRadioButton
    ("Rational", gbox, stopGroup, gbLayout, 0, "Use rational edge stopping "
     "function; may require smaller K values");
  radio = diaRadioButton("Tukey biweight", gbox, stopGroup, gbLayout, 1,
                         "Use Tukey biweight edge stopping function; may "
                         "require larger K values");
  diaSetGroup(stopGroup, proc.andfStopFunc);
  QObject::connect(stopGroup, SIGNAL(buttonClicked(int)), win, 
                   SLOT(andfFuncClicked(int)));

  // Iteration spin box and report of # done
  QBoxLayout *hLayout = diaHBoxLayout(layout);
  
  QSpinBox *iterSpin = (QSpinBox *)diaLabeledSpin
    (0, 1., 1000., 1., "Iterations", parent, hLayout);
  diaSetSpinBox(iterSpin, proc.andfIterations);
  iterSpin->setToolTip("Number of time steps to take in one run");
  QObject::connect(iterSpin, SIGNAL(valueChanged(int)), win, 
                   SLOT(andfIterChanged(int)));
  proc.andfDoneLabel = new QLabel("0 done", parent);
  hLayout->addWidget(proc.andfDoneLabel);

  // K edit box and report of unscaled K value
  hLayout = diaHBoxLayout(layout);
  QLabel *label = new QLabel("K", parent);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hLayout->addWidget(label);
  proc.andfKEdit = new ToolEdit(parent, 6);
  hLayout->addWidget(proc.andfKEdit);
  proc.andfKEdit->setToolTip("Gradient threshold parameter controlling "
                "edge stopping function");
  str.sprintf("%.5g", proc.andfK);
  proc.andfKEdit->setText(str);
  proc.andfKEdit->setFocusPolicy(Qt::ClickFocus);
  proc.andfScaleLabel = new QLabel(" ", parent);
  setUnscaledK();
  hLayout->addWidget(proc.andfScaleLabel);
  QObject::connect(proc.andfKEdit, SIGNAL(returnPressed()), win,
                   SLOT(setFocus()));
  QObject::connect(proc.andfKEdit, SIGNAL(returnPressed()), win,
                   SLOT(andfKEntered()));
  QObject::connect(proc.andfKEdit, SIGNAL(focusLost()), win,
                   SLOT(andfKEntered()));

  // Lambda edit box
  hLayout = diaHBoxLayout(layout);
  label = new QLabel("Lambda", parent);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hLayout->addWidget(label);
  proc.andfLambdaEdit = new ToolEdit(parent, 6);
  hLayout->addWidget(proc.andfLambdaEdit);
  proc.andfLambdaEdit->setToolTip("Size of time step");
  str.sprintf("%g", proc.andfLambda);
  proc.andfLambdaEdit->setText(str);
  proc.andfLambdaEdit->setFocusPolicy(Qt::ClickFocus);
  QObject::connect(proc.andfLambdaEdit, SIGNAL(returnPressed()), win,
                   SLOT(setFocus()));
  hLayout->addStretch();
}

static void setUnscaledK()
{
  QString str;
  str.sprintf("unscaled: %.5g",  proc.andfK / proc.vi->image->slope);
  proc.andfScaleLabel->setText(str);
}

/* THE WINDOW CLASS CONSTRUCTOR */
static const char *buttonLabels[] = {"Apply", "More", "Toggle", "Reset", "Save", 
                               "Done", "Help"};
static const char *buttonTips[] = {"Operate on current section (hot key A)",
                             "Reiterate operation on current section (hot key"
                             " B)",
                             "Toggle between processed and original image",
                             "Reset section to unprocessed image",
                             "Replace section in memory with processed image",
                             "Close dialog box", "Open help window"};

IProcWindow::IProcWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 7, 1, buttonLabels, buttonTips, false, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  int i;
  QString str;
  QVBoxLayout *vLayout;
  QWidget *control;
  mRunningProc = false;

  // Put an H layout inside the main layout, then fill that with the
  // List box and the widget stack
  QHBoxLayout *hLayout = diaHBoxLayout(mLayout);
  vLayout = diaVBoxLayout(hLayout);
  mListBox = new QListWidget(this);
  vLayout->addWidget(mListBox);
  mListBox->setFocusPolicy(Qt::NoFocus);
  mListBox->setSelectionMode(QAbstractItemView::SingleSelection);

  vLayout->addStretch();
  QCheckBox *check = diaCheckBox("Autoapply", this, vLayout);
  diaSetChecked(check, proc.autoApply);
  QObject::connect(check, SIGNAL(toggled(bool)), this,
                   SLOT(autoApplyToggled(bool)));
  check->setToolTip("Apply current process automatically when changing" 
                " section");

  mStack = new QStackedWidget(this);
  hLayout->addWidget(mStack);

  // Put a spacer on the right to keep the list box position from changing
  hLayout->addStretch(0);

  for (i = 0; (proc_data[i].name); i++) {

    // For each item, add to list box, make a widget and give it a V layout
    mListBox->addItem(proc_data[i].name);
    control = new QWidget(this);
    vLayout = new QVBoxLayout(control);
    vLayout->setContentsMargins(3, 3, 3, 3);
    vLayout->setSpacing(6);

    // Call the make widget function or just add a label
    if (proc_data[i].mkwidget)
      proc_data[i].mkwidget (this, control, vLayout);
    else {
      diaLabel(proc_data[i].label, control, vLayout);
    }
    vLayout->addStretch(0);

    // Add widget to stack and set size policy to ignored
    mStack->addWidget(control);
    control->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,
                                       QSizePolicy::Ignored));
  }

  // Finalize list box setting and connections
  manageListSize();
  filterHighlighted(proc.procnum);
  mListBox->setCurrentRow(proc.procnum);
  connect(mListBox, SIGNAL(currentRowChanged(int)), this,
          SLOT(filterHighlighted(int)));
  connect(mListBox, SIGNAL(itemDoubleClicked(QListWidgetItem *)), this, 
          SLOT(filterSelected(QListWidgetItem *)));
  
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonClicked(int)));
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setWindowTitle(imodCaption("3dmod Image Processing"));
}

/* Action functions */

void IProcWindow::autoApplyToggled(bool state)
{
  setFocus();
  proc.autoApply = state;
}

void IProcWindow::threshChanged(int which, int value, bool dragging)
{
  proc.threshold = value;
}

void IProcWindow::fourFiltChanged(int which, int value, bool dragging)
{
  if (!which)
    proc.sigma1 = 0.001 * value;
  else if (which == 1)
    proc.radius2 = 0.001 * value;
  else if (which == 2)
    proc.sigma2 = 0.001 * value;
}

void IProcWindow::kernelChanged(double val)
{
  setFocus();
  proc.kernelSigma = val;
}

void IProcWindow::scaleSmthToggled(bool state)
{
  setFocus();
  proc.rescaleSmooth = state;
}

void IProcWindow::binningChanged(int val)
{
  setFocus();
  proc.fftBinning = val;
}
void IProcWindow::subsetChanged(bool state)
{
  proc.fftSubset = state;
}

void IProcWindow::reportFreqClicked()
{
  double dx, dy, xpt, ypt, dist;
  Ipoint *curpt;
  QString str;
  Imod *imod = proc.vi->imod;
    
  if (proc.fftScale <= 0.)
    return;

  // Use the current model point if defined and in model mode, or use 
  // current marker point
  curpt = imodPointGet(imod);
  if (proc.vi->imod->mousemode == IMOD_MMODEL && curpt) {
    xpt = curpt->x - 0.5f;
    ypt = curpt->y - 0.5f;
  } else {
    xpt = (int)proc.vi->xmouse;
    ypt = (int)proc.vi->ymouse;
  }
  dx = xpt - proc.fftXcen;
  dy = ypt - proc.fftYcen;
  dist = proc.fftScale * sqrt(dx * dx + dy * dy) / 
    (imod->pixsize * proc.vi->xybin);
  if (dist) 
    str.sprintf("Freq: %.4g/%s  (%.4g %s)", dist, imodUnits(imod), 
                1. / dist, imodUnits(imod));
  else
    str.sprintf("Freq: 0/%s", imodUnits(imod));
  proc.fftLabel3->setText(str);
}


void IProcWindow::growChanged(bool state)
{
  proc.threshGrow = state;
}
void IProcWindow::shrinkChanged(bool state)
{
  proc.threshShrink = state;
}

// To switch filters, set the size policy of the current widget back to ignored
// raise the new widget, set its size policy, make the stack process geometry
// again then adjust window size
void IProcWindow::filterHighlighted(int which)
{
  QWidget *control = mStack->currentWidget();
  if (control)
    control->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,
                                       QSizePolicy::Ignored));
  proc.procnum = which;
  mStack->setCurrentIndex(which);
  control = mStack->currentWidget();
  control->setSizePolicy(QSizePolicy(QSizePolicy::Expanding,
                                     QSizePolicy::Expanding));
  imod_info_input();
  mStack->adjustSize();
  imod_info_input();
  adjustSize();
}

void IProcWindow::filterSelected(QListWidgetItem *item)
{
  int which = mListBox->row(item);
  filterHighlighted(which);
  if (!proc.dia->mRunningProc)
    apply();
}

void IProcWindow::edgeSelected(int which)
{
  proc.edge = which;
}

void IProcWindow::medSizeChanged(int val)
{ 
  setFocus();
  proc.medianSize = val;
}

void IProcWindow::med3DChanged(bool state)
{
  proc.median3D = state;
}

void IProcWindow::andfIterChanged(int val)
{
  setFocus();
  proc.andfIterations = val;
}
void IProcWindow::andfFuncClicked(int val)
{
  proc.andfStopFunc = val;
}

void IProcWindow::andfKEntered()
{
  proc.andfK = proc.andfKEdit->text().toDouble();
  setUnscaledK();
}


// Respond to button click (release)
void IProcWindow::buttonClicked(int which)
{
  ImodIProc *ip = &proc;

  int cz =  (int)(ip->vi->zmouse + 0.5f);
  setFocus();

  if (which < 5 && ip->vi->loadingImage)
    return;

  switch (which) {
  case 0:  // Apply
    apply();
    break;

  case 1:  // More
    /* If this is not the same section, treat it as an Apply */
    if (cz != ip->idatasec || ip->vi->curTime != ip->idatatime) {
      apply();
      break;
    }

    /* Otherwise operate on the current data without restoring it */
    if (proc_data[ip->procnum].cb)
      startProcess();
    break;

  case 2:  // Toggle
    if (ip->modified && cz == ip->idatasec && ip->vi->curTime == ip->idatatime)
      copyAndDisplay();
    break;

  case 3: // reset
    clearsec(ip);
    imodDraw(ip->vi, IMOD_DRAW_IMAGE);
    break;

  case 4: // save
    ip->modified = 0;
    ip->idatasec = -1;
    break;

  case 5: // Done
    close();
    break;

  case 6: // Help
    imodShowHelpPage("imageProc.html#TOP");
    break;
  }
}

// Respond to button press for toggle button only - redisplay original data
// but re-mark as modified
void IProcWindow::buttonPressed(int which)
{
  ImodIProc *ip = &proc;
  int cz =  (int)(ip->vi->zmouse + 0.5f);

  if (which != 2 || !ip->modified || cz != ip->idatasec || 
      ip->vi->curTime != ip->idatatime)
    return;
     
  if (savedToImage(ip))
    return;
  imod_info_float_clear(ip->idatasec, ip->idatatime);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
}

// Apply the current filter
void IProcWindow::apply()
{
  ImodIProc *ip = &proc;
  sliceInit(&s, ip->vi->xsize, ip->vi->ysize, 0, ip->iwork);

  int cz =  (int)(ip->vi->zmouse + 0.5f);

  /* Unconditionally restore data if modified */
  clearsec(ip);

  ip->andfIterDone = 0;
  ip->andfDoneLabel->setText("0 done");

  /* If this is a new section, save the data */
  if (cz != ip->idatasec || ip->vi->curTime != ip->idatatime) {
    ip->idatasec = cz;
    ip->idatatime = ip->vi->curTime;
    savesec(ip);
  }

  // Make sure there is floating info for this data so it can be saved when
  // it is cleared
  imod_info_bwfloat(ip->vi, ip->idatasec, ip->idatatime);
  imodInfoSaveNextClear();
    
  /* Operate on the original data */
  startProcess();
}

void IProcWindow::startProcess()
{
  ImodIProc *ip = &proc;
  int i;
  if (!proc_data[ip->procnum].cb)
    return;
  ip->fftScale = 0.;
  ip->freqButton->setEnabled(false);
  mCallback = NULL;

#ifdef QT_THREAD_SUPPORT

  // If running in a thread, set flag, disable buttons except help,
  // start timer and start thread
  mRunningProc = true;
  for (i = 0; i < mNumButtons - 1; i++)
    mButtons[i]->setEnabled(false);
  ImodInfoWin->manageMenus();
  mTimerID = startTimer(50);
  mProcThread = new IProcThread;

  // Priorities not available in Qt 3.1
#if QT_VERSION >= 0x030200
  mProcThread->start(QThread::LowPriority);
#else
  mProcThread->start();
#endif

#else

  // Otherwise just start the process directly and do finishing tasks
  proc_data[ip->procnum].cb();
  finishProcess();
#endif
}

void IProcWindow::finishProcess()
{
  ImodIProc *ip = &proc;
  QString str;
  float xrange, yrange;
  ip->modified = 1;
  copyAndDisplay();
  if (ip->fftScale < 0.) {
    wprint("\aMemory error trying to do FFT!\n");
  } else if (ip->fftScale > 0.) {
    str.sprintf("Scale: %.3g/pixel per FFT pixel", ip->fftScale);
    ip->fftLabel1->setText(str);
    xrange = 0.5 * ip->fftScale * ip->vi->xsize;
    xrange = xrange <= 0.5 ? xrange : 0.5f;
    yrange = 0.5 * ip->fftScale * ip->vi->ysize;
    yrange = yrange <= 0.5 ? yrange : 0.5f;
    str.sprintf("Range: +/- %.4f in X, +/- %.4f in Y", xrange, yrange);
    ip->fftLabel2->setText(str);
    ip->freqButton->setEnabled(true);
  }
  if (ip->andfIterDone) {
    str.sprintf("%d done", ip->andfIterDone);
    ip->andfDoneLabel->setText(str);
    setUnscaledK();
  }
}

// Check for whether the thread is running, if not, finish up and call callback
void IProcWindow::timerEvent(QTimerEvent *e)
{
#ifdef QT_THREAD_SUPPORT
  int i;
  if (mProcThread->isRunning())
    return;
  killTimer(mTimerID);
  for (i = 0; i < mNumButtons - 1; i++)
    mButtons[i]->setEnabled(true);
  delete mProcThread;
  mRunningProc = false;
  finishProcess();
  ImodInfoWin->manageMenus();
  if (mCallback)
    mCallback();
#endif
}

void IProcWindow::limitFFTbinning()
{
  ImodIProc *ip = &proc;
  int limit = 16;
  if (limit > ip->vi->xsize)
    limit = ip->vi->xsize;
  if (limit > ip->vi->ysize)
    limit = ip->vi->ysize;
  if (ip->fftBinning > limit)
    ip->fftBinning = limit;
  diaSetSpinMMVal(ip->fftBinSpin, 1, limit, ip->fftBinning);
}

void IProcWindow::manageListSize()
{
  int maxwidth = 0, width, i, height;
  for (i = 0; i < mListBox->count(); i++) {
    width = mListBox->fontMetrics().width(mListBox->item(i)->text());
    maxwidth = B3DMAX(maxwidth, width);
  }
  mListBox->setFixedWidth(maxwidth + 12);
  height = (mListBox->fontMetrics().height() + 1.5) *  mListBox->count();
  mListBox->setFixedHeight(height);
}

void IProcWindow::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    manageListSize();
}

// The window is closing, clean up and remove from manager
void IProcWindow::closeEvent ( QCloseEvent * e )
{
  ImodIProc *ip = &proc;
  if (!ip->dia || mRunningProc)
    return;
  clearsec(ip);
  imodDialogManager.remove((QWidget *)ip->dia);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
  freeArrays(ip);
  ip->dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void IProcWindow::keyPressEvent ( QKeyEvent * e )
{
  int modkey = e->modifiers() & (Qt::ShiftModifier | Qt::ControlModifier);
  if (e->key() == Qt::Key_A && !modkey) {
    if (!iprocBusy() && !proc.vi->loadingImage)
      apply();
  } else if (e->key() == Qt::Key_B && !modkey) {
    if (!iprocBusy() && !proc.vi->loadingImage)
      buttonClicked(1);
  } else if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void IProcWindow::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

#ifdef QT_THREAD_SUPPORT
// A very simple thread run command!
void IProcThread::run()
{
  proc_data[proc.procnum].cb();
}
#endif
