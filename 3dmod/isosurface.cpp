/*
*  isosurface.cpp - Makes an isosurface of selected volume in extra object
*
*  Author: Quanren Xiong, modified by David Mastronarde
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  $Id$
*/

#include <algorithm> //std::sort
#include <vector>
#include <qcheckbox.h>
#include <qgroupbox.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qlayout.h>
#include <qgl.h>
#include <qslider.h>
#include <qtooltip.h>
#include <qpushbutton.h>
#include <qspinbox.h>
#include <qdatetime.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QCloseEvent>
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "mkmesh.h"
#include "pyramidcache.h"
#include "mv_gfx.h"
#include "isosurface.h"
#include "surfpieces.h"
#include "histwidget.h"
#include "isothread.h"
#include "xzap.h"
#include "mv_input.h"
#include "display.h"
#include "mv_objed.h"
#include "control.h"
#include "cachefill.h"


enum {IIS_X_COORD = 0, IIS_Y_COORD, IIS_Z_COORD, IIS_X_SIZE, IIS_Y_SIZE, IIS_Z_SIZE};
enum {FIND_DEFAULT_DIM=0, FIND_MAXIMAL_DIM};
enum {MASK_NONE = 0, MASK_CONTOUR, MASK_OBJECT, MASK_SPHERE, MASK_ELLIPSOID, MASK_LASSO};

static int xDrawSize = -1;
static int yDrawSize = -1;
static int zDrawSize = -1;
static int lastYsize = -1;

static QTime isoTime;

#define IIS_CENTER_VOLUME 1
#define IIS_VIEW_BOX (1 << 1)
#define IIS_VIEW_USER_MODEL (1 << 2)
#define IIS_VIEW_ISOSURFACE (1 << 3)
#define IIS_LINK_XYZ (1 << 4)
#define IIS_DELETE_PIECES (1 << 5)
#define IIS_CLOSE_FACES (1 << 6)
#define IIS_PAINT_OBJECT (1 << 7)

#define MAXIMAL_VOXELS 128*512*512  //the number of voxles the bounding box is limited to;
#define DEFAULT_VOXELS 96*96*96 //the number of voxles the initial bounding box;
#define PERCENTILE 0.07
#define MAXIMAL_ITERATION 20
#define MAXIMAL_BINNING 4
#define TOL 0.0001

struct imodvIsosurfaceDataStruct {
  ImodvIsosurface *dia;
  ImodvApp  *a;
  int itNum;
  int binningNum;
  int minTNum;
  int maskType;
  int paintObj;
  b3dUInt32 flags;
};

static struct imodvIsosurfaceDataStruct iisData = 
  {0, 0, 0, 1, 100, 0, 0, IIS_CENTER_VOLUME | IIS_VIEW_BOX | IIS_VIEW_USER_MODEL |
   IIS_VIEW_ISOSURFACE | IIS_VIEW_ISOSURFACE | IIS_LINK_XYZ};

static bool isBoxChanged(const int *start, const int *end);
static void setCoordLimits(int cur, int maxSize, int drawSize, int &str, int &end);
static void findDimLimits(int which, int &xdim, int &ydim, int &zdim, int *sizes);
static int doDumps = 0;

// Open, close, or raise the dialog box
void imodvIsosurfaceEditDialog(ImodvApp *a, int state)
{
  if (!state){
    if (iisData.dia)
      iisData.dia->close();
    return;
  }
  if (iisData.dia) {
    iisData.dia->raise();
    return;
  }

  iisData.a = a;
  iisData.dia = new ImodvIsosurface(a->vi, a->vi->pyrCache != NULL,
                                    imodvDialogManager.parent(IMODV_DIALOG),
                                    "isosurface view");
  imodvDialogManager.add((QWidget *)iisData.dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)iisData.dia, IMODV_DIALOG);
  if (!iisData.dia->mVolume) {
    iisData.dia->close();
    return;
  }
}

// The external call to update the dialog and isosurface
bool imodvIsosurfaceUpdate(int drawFlags)
{
  Iobj *obj;
  Icont *cont;
  int ob, co, pt;
  bool lassoMask = iisData.maskType == MASK_LASSO;
  bool objMask = iisData.maskType == MASK_OBJECT;
  bool contMask = iisData.maskType == MASK_CONTOUR;
  double checksum = 0.;
  ImodvIsosurface *dia = iisData.dia;

  if (dia && dia->mVolume) { 
    dia->updateCoords(iisData.flags & IIS_LINK_XYZ);
    if ((drawFlags & IMOD_DRAW_IMAGE) || isBoxChanged(dia->mBoxOrigin, dia->mBoxEnds)) {
      dia->setBoundingBox();
      if (iisData.flags & IIS_CENTER_VOLUME)
        dia->setViewCenter();
      dia->setBoundingObj();
      int stackIdx = dia->getCurrStackIdx();
      dia->mCurrStackIdx = stackIdx;

      // Set threshold for current stack, but first time, set threshold from data
      dia->mThreshold = dia->mStackThresholds[stackIdx];
      dia->mOuterLimit = dia->mStackOuterLims[stackIdx];
      dia->fillAndProcessVols(dia->mStackThresholds[stackIdx]<0);
      dia->setIsoObj(true);
      //imodvDraw(Imodv);
      return true;
    } else if (!(iisData.flags & IIS_LINK_XYZ) && (objMask || contMask || lassoMask)) {
      if (lassoMask) {
        cont = getTopZapLassoContour(false);
      } else {
        imodGetIndex(Imodv->imod, &ob, &co, &pt);
        obj = imodObjectGet(Imodv->imod);
        cont = imodContourGet(Imodv->imod);
      }
      if (objMask && obj) {
        checksum = imodObjectChecksum(obj, ob);
      } else if (cont && cont->psize == dia->mMaskPsize) {
        for (pt = 0; pt < cont->psize; pt++)
          checksum += cont->pts[pt].x + cont->pts[pt].y;
      }
      if ((objMask && (ob != dia->mMaskObj || checksum != dia->mMaskChecksum)) ||
          (contMask && cont && 
           (ob != dia->mMaskObj || co != dia->mMaskCont || 
            cont->psize != dia->mMaskPsize || checksum != dia->mMaskChecksum)) ||
          (lassoMask && cont && 
           (cont->psize != dia->mMaskPsize || checksum != dia->mMaskChecksum))) {
        dia->resizeToContours(false);
        return true;
      }
    }

    // Keep the painting object spin box up to date
    if (dia->mLastObjsize != Imodv->imod->objsize)
      dia->managePaintObject();

    // Check if repainting is needed
    if (dia->fillPaintVol()) {
      dia->paintMesh();
      return true;
    }
    return false;

  } else
    return false;
}

// External call to get limits being displayed in X and Y
bool imodvIsosurfaceBoxLimits(int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  ImodvIsosurface *dia = iisData.dia;
  if (!dia || !dia->mVolume)
    return false;
  ixStart = dia->mBoxOrigin[0];
  iyStart = dia->mBoxOrigin[1];
  nxUse = dia->mBoxEnds[0] + 1 - ixStart;
  nyUse = dia->mBoxEnds[1] + 1 - iyStart;
  return true;
} 

// Compute starting and ending draw coordinates from center coordinate, desired
// size and maximum size, shifting center if necessary
static void setCoordLimits(int cur, int maxSize, int drawSize, int &str, int &end)
{
  str = cur - drawSize / 2;
  if (str < 0)
    str = 0;
  end = str + drawSize;
  if (end > maxSize) {
    end = maxSize;
    str = end - drawSize;
  }
}

static bool isBoxChanged(const int *start, const int *end)
{
  int newStart, newEnd, currTime;

  ivwGetTime(Imodv->vi, &currTime);
  if(iisData.dia->mCurrTime != currTime) {
    iisData.dia->mCurrTime = currTime;
    return true;
  }
  if (Imodv->vi->ushortStore && (iisData.dia->mRangeLow != Imodv->vi->rangeLow || 
                                 iisData.dia->mRangeHigh != Imodv->vi->rangeHigh))
    return true;
  setCoordLimits(iisData.dia->mLocalX, Imodv->vi->xsize, xDrawSize,
                 newStart, newEnd);
  if (start[0] != newStart || end[0] != newEnd) 
    return true;
  setCoordLimits(iisData.dia->mLocalY, Imodv->vi->ysize, yDrawSize,
                 newStart, newEnd);
  if (start[1] != newStart || end[1] != newEnd)
    return true;
  setCoordLimits(iisData.dia->mLocalZ, Imodv->vi->zsize, zDrawSize,
                 newStart, newEnd);
  if (start[2] != newStart || end[2] != newEnd) 
    return true;
  return false;
}

static void findDimLimits(int which, int &xdim, int &ydim, int &zdim, int *sizes)
{  
  int specMinDim;
  int tempMin;
  int voxelMax;
  int boxInit = ImodPrefs->isoBoxInitial();
  int boxLimit = ImodPrefs->isoBoxLimit();
  int maximal = boxLimit * boxLimit * boxLimit;
  int deflt = boxInit * boxInit * boxInit;

  /* xdim = 512;
  ydim = 85;
  zdim = 512;
  return;*/ 

  if (which == FIND_DEFAULT_DIM) {
    specMinDim = powf((float)deflt, 1.0/3.0);
    voxelMax = deflt;
  } else if (which == FIND_MAXIMAL_DIM) {
    specMinDim = powf((float)maximal, 1.0/3.0);
    voxelMax = maximal;
  } else {
    imodPrintStderr("imodv_isosurface: illegal usage\n");
    return;
  }

  if (sizes[0] <= sizes[1] && sizes[0] <= sizes[2]) { 
    xdim = B3DMIN(specMinDim, sizes[0]);
    tempMin = (int)sqrt((double)( voxelMax/(xdim)) );
    ydim = B3DMIN(tempMin, sizes[1]);
    zdim = B3DMIN(tempMin, sizes[2]);
  } else if(sizes[1] <= sizes[0] && sizes[1] <= sizes[2]) {
    ydim = B3DMIN(specMinDim, sizes[1]);
    tempMin = (int)sqrt((double)(voxelMax/(ydim)) );
    xdim = B3DMIN(tempMin, sizes[0]);
    zdim = B3DMIN(tempMin, sizes[2]);
  } else if(sizes[2] <= sizes[0] && sizes[2] <= sizes[1]) {
    zdim = B3DMIN(specMinDim, sizes[2]);
    tempMin = (int)sqrt((double)(voxelMax/(zdim)));
    xdim = B3DMIN(tempMin, sizes[0]);
    ydim = B3DMIN(tempMin, sizes[1]);
  }
}

// THE ImodvIsosurface CLASS IMPLEMENTATION

static const char *buttonLabels[] = {"Save to Object", "Done", "Help", " Fill "};
static const char *buttonTips[] = {"Save isosurfaces and bounding box as Imod objects",
                                   "Close dialog box", "Open help window", 
                                   "Fill cache for current displayed area"};
static const char *sliderLabels[] = {"X", "Y", "Z", "X size", "Y size", "Z size"};
static const char *histLabels[] = {"Threshold", "Outer limit"};

ImodvIsosurface::ImodvIsosurface(ImodView *vi, bool fillBut, QWidget *parent, 
                                 const char *name)
  : DialogFrame(parent, fillBut ? 4 : 3, 1, buttonLabels, buttonTips, false, 
      ImodPrefs->getRoundedStyle(), "3dmodv Isosurface View", "", name)
{
  mCtrlPressed = false;
  mVi = vi;
  //reserve a number;
  mExtraObjNum = ivwGetFreeExtraObjectNumber(vi);
  mBoxObjNum = ivwGetFreeExtraObjectNumber(vi);
  ivwGetTime(vi, &mCurrTime);
  mVolume = NULL;
  mTrueBinVol = NULL;
  mPaintVol = NULL;
  mCurrMax = 0;
  mSurfPieces = NULL;
  mOrigMesh = NULL;
  mFilteredMesh = NULL;
  mMadeFiltered = false;
  mLayout->setSpacing(4);
  QHBoxLayout *horizLayout = new QHBoxLayout();
  QVBoxLayout *leftLayout = new QVBoxLayout();
  QVBoxLayout *rightLayout = new QVBoxLayout();
  if (!ImodPrefs->getRoundedStyle()) {
    leftLayout->setSpacing(4);
    rightLayout->setSpacing(4);
  }
  horizLayout->setSpacing(6);
  mLayout->addLayout(horizLayout);
  horizLayout->addLayout(leftLayout);
  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::VLine );
  line->setFrameShadow( QFrame::Sunken );
  horizLayout->addWidget(line);
  horizLayout->addLayout(rightLayout);

  // Make view checkboxes
  mViewIso = diaCheckBox("View isosurfaces", this, rightLayout);
  mViewIso->setChecked(iisData.flags & IIS_VIEW_ISOSURFACE);
  connect(mViewIso, SIGNAL(toggled(bool)), this, SLOT(viewIsoToggled(bool)));
  mViewModel = diaCheckBox("View user model", this, rightLayout);
  mViewModel->setChecked(iisData.flags & IIS_VIEW_USER_MODEL);
  connect(mViewModel, SIGNAL(toggled(bool)), this, SLOT(viewModelToggled(bool)));
  mViewBoxing = diaCheckBox("View bounding box", this, rightLayout);
  mViewBoxing->setChecked(iisData.flags & IIS_VIEW_BOX);
  connect(mViewBoxing, SIGNAL(toggled(bool)), this, SLOT(viewBoxingToggled(bool)));
  mCenterVolume = diaCheckBox("Keep box centered", this, rightLayout);
  mCenterVolume->setChecked(iisData.flags & IIS_CENTER_VOLUME);
  connect(mCenterVolume,
      SIGNAL(toggled(bool)),this,SLOT(centerVolumeToggled(bool)));
  mViewIso->setToolTip("Display isosurfaces");
  mViewModel->setToolTip("Display user model");
  mViewBoxing->setToolTip("Display the bounding box");
  mCenterVolume->setToolTip("Keep isosurfaces centered in the model view window");
  mLinkXYZ = diaCheckBox("Link to global X/Y/Z", this, rightLayout);
  mLinkXYZ->setChecked(iisData.flags & IIS_LINK_XYZ);
  connect(mLinkXYZ, SIGNAL(toggled(bool)), this, SLOT(
        linkXYZToggled(bool)));
  mLinkXYZ->setToolTip("Link global XYZ and  the XYZ of isosurface center");


  QHBoxLayout *hLayout = new QHBoxLayout;
  mBinningBox = (QSpinBox *)diaLabeledSpin(0, 1., (float)MAXIMAL_BINNING, 1.,
                                           "Binning:", this, hLayout);
  mBinningBox->setValue(iisData.binningNum);
  rightLayout->addLayout(hLayout);
  connect(mBinningBox, SIGNAL(valueChanged(int)), this,
      SLOT(binningNumChanged(int)));
  mBinningBox->setToolTip("Set the binning level");

  hLayout = new QHBoxLayout;
  mSmoothBox = (QSpinBox *)diaLabeledSpin(0, 0., (float)MAXIMAL_ITERATION, 1.,
                                        "Smoothing:", this, hLayout);
  mSmoothBox->setValue(iisData.itNum);
  rightLayout->addLayout(hLayout);
  connect(mSmoothBox, SIGNAL(valueChanged(int)), this, SLOT(iterNumChanged(int)));
  mSmoothBox->setToolTip("Set the iteration number for smoothing");

  mDeletePieces = diaCheckBox("Delete small pieces", this, rightLayout);
  mDeletePieces->setChecked(iisData.flags & IIS_DELETE_PIECES);
  connect(mDeletePieces, SIGNAL(toggled(bool)), this, SLOT(
        deletePiecesToggled(bool)));
  mDeletePieces->setToolTip("Remove small isosurface pieces");
  hLayout = new QHBoxLayout;
  mPiecesBox = (QSpinBox *)diaLabeledSpin(0, 10., 9999., 10., "min size:",
                                        this, hLayout);
  mPiecesBox->setValue(iisData.minTNum);
  rightLayout->addLayout(hLayout);
  connect(mPiecesBox, SIGNAL(valueChanged(int)), this,
      SLOT(numOfTrianglesChanged(int)) );
  mPiecesBox->setToolTip(
      "Set the # of triangles the smallest piece must have");

  mHistPanel = new HistWidget(this);
  mHistPanel->setMinimumSize(160, 80);
  mHistSlider = new MultiSlider(this, 2, histLabels);
  connect(mHistSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(histChanged(int, int, bool)));

  leftLayout->addWidget(mHistPanel);
  leftLayout->setStretchFactor(mHistPanel, 100);
  leftLayout->addLayout(mHistSlider->getLayout());
  (mHistSlider->getLayout())->setSpacing(4);

  QCheckBox *check = diaCheckBox("Cap at box faces", this, rightLayout);
  check->setChecked(iisData.flags & IIS_CLOSE_FACES);
  connect(check, SIGNAL(toggled(bool)), this, SLOT(closeFacesToggled(bool)));
  check->setToolTip("Close surfaces that are cut by edge of box");

  // Make multisliders
  mSliders = new MultiSlider(this, 6, sliderLabels);
  //temporarily store input stack size in mBoxSize for calling findDimLimits();
  mBoxSize[0] = vi->xsize;
  mBoxSize[1] = vi->ysize;
  mBoxSize[2] = vi->zsize;
  findDimLimits(FIND_DEFAULT_DIM, xDrawSize, yDrawSize, zDrawSize, mBoxSize);
  int xMax, yMax, zMax;
  findDimLimits(FIND_MAXIMAL_DIM, xMax, yMax, zMax, mBoxSize);

  mSliders->setRange(IIS_X_COORD, 1, vi->xsize);
  mSliders->setRange(IIS_X_SIZE, 1, xMax );
  mSliders->setRange(IIS_Y_SIZE, 1, yMax );
  mSliders->setRange(IIS_Z_SIZE, 1, zMax );
  lastYsize = mVi->ysize;

  bool flagDrawXYZ = false;
  if (mVi->xmouse == 0.0 &&  mVi->ymouse == 0.0) { 
    mVi->xmouse = mVi->xsize/2.0;
    mVi->ymouse = mVi->ysize/2.0;
    flagDrawXYZ = true;
  }

  updateCoords(true);
  mSliders->setValue(IIS_X_SIZE, xDrawSize);
  mSliders->setValue(IIS_Y_SIZE, yDrawSize);
  mSliders->setValue(IIS_Z_SIZE, zDrawSize);
  leftLayout->addLayout(mSliders->getLayout());
  (mSliders->getLayout())->setSpacing(4);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(sliderMoved(int, int, bool)));
  mSliders->getSlider(IIS_X_COORD)->setToolTip("Set X coordinate of the box center");
  mSliders->getSlider(IIS_Y_COORD)->setToolTip("Set Y coordinate of the box center");
  mSliders->getSlider(IIS_Z_COORD)->setToolTip("Set Z coordinate of the box center");
  mSliders->getSlider(IIS_X_SIZE)->setToolTip("Set bounding box size in X");
  mSliders->getSlider(IIS_Y_SIZE)->setToolTip("Set bounding box size in Y");
  mSliders->getSlider(IIS_Z_SIZE)->setToolTip("Set bounding box size in Z");

  QGroupBox *gbox = new QGroupBox("Mask", this);
  rightLayout->addWidget(gbox);
  QVBoxLayout *gbLayout = new QVBoxLayout(gbox);
  QButtonGroup *maskGroup = new QButtonGroup(this);

  connect(maskGroup, SIGNAL(buttonClicked(int)), this, SLOT(maskSelected(int)));
  if (!ImodPrefs->getRoundedStyle())
    gbLayout->setSpacing(0);
  gbLayout->setContentsMargins(5, 0, 5, 4);

  QRadioButton *radio = diaRadioButton("None", gbox, maskGroup, gbLayout, 0,
                                       "Use data in full box with no masking");
  radio = diaRadioButton("Current contour", gbox, maskGroup, gbLayout, 1,
                         "Use data inside current contour");
  radio = diaRadioButton("Current object", gbox, maskGroup, gbLayout, 2,
                         "Use data inside contours of current object");
  radio = diaRadioButton("Sphere", gbox, maskGroup, gbLayout, 3,
                         "Use data inside the largest sphere that fits in the box");
  radio = diaRadioButton("Ellipsoid", gbox, maskGroup, gbLayout, 4,
                         "Use data inside the largest ellipsoid that fits in the box");
  radio = diaRadioButton("Zap lasso", gbox, maskGroup, gbLayout, 5,
                         "Use data inside the lasso in the top Zap window");
  diaSetGroup(maskGroup, iisData.maskType);

  gbox = new QGroupBox("Set X/Y area from", this);
  leftLayout->addWidget(gbox);
  hLayout = new QHBoxLayout(gbox);
  hLayout->setSpacing(3);
  hLayout->setContentsMargins(3, 1, 3, 3);

  //rightLayout->addStretch();

  mUseRubber = diaPushButton("Zap Band", this, hLayout);
  mUseRubber->setToolTip("Show isosurfaces of area enclosed by the Zap rubberband");
  connect(mUseRubber, SIGNAL(clicked()), this, SLOT(showRubberBandArea()));

  mSizeContours = diaPushButton(iisData.maskType == MASK_OBJECT ? "Object" : 
                                (iisData.maskType == MASK_LASSO ? "Lasso" : "Contour"),
                                this, hLayout);
  mSizeContours->setToolTip("Make X/Y sizes as big as masking contours if possible");
  connect(mSizeContours, SIGNAL(clicked()), this, SLOT(areaFromContClicked()));
  mSizeContours->setEnabled(iisData.maskType == MASK_CONTOUR || 
                            iisData.maskType == MASK_OBJECT ||
                            iisData.maskType == MASK_LASSO);

  hLayout = new QHBoxLayout;
  rightLayout->addLayout(hLayout);
  mPaintCheck = diaCheckBox("Paint obj:", this, hLayout);
  mPaintCheck->setChecked(iisData.flags & IIS_PAINT_OBJECT);
  connect(mPaintCheck, SIGNAL(toggled(bool)), this, SLOT(paintObjToggled(bool)));
  mPaintCheck->setToolTip("Color parts of mesh inside spheres in selected object");
  hLayout->setStretchFactor(mPaintCheck, 100);

  mPaintObjSpin = (QSpinBox *)diaLabeledSpin(0, 1., 10., 1., NULL, this, hLayout);
  managePaintObject();
  connect(mPaintObjSpin, SIGNAL(valueChanged(int)), this, SLOT(paintObjChanged(int)));
  mPaintObjSpin->setToolTip("Select a scattered point object to color mesh with");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
  setFontDependentWidths();

  mInitNThreads = QThread::idealThreadCount();
  char *procChar = getenv("IMOD_PROCESSORS");
  if (procChar) 
    mInitNThreads = atoi(procChar);
  mInitNThreads = B3DMIN(MAX_THREADS, B3DMAX(1, mInitNThreads));

  for (int i = 0; i<mInitNThreads; i++) 
    threads[i] = new IsoThread(i, this);

  setBoundingBox();
  if (iisData.flags & IIS_CENTER_VOLUME) 
    setViewCenter();
  setBoundingObj();
  for (int i = 0; i<mVi->numTimes+1; i++) {
    mStackThresholds.push_back(-1.0);
    mStackOuterLims.push_back(-1);
  }
  mOuterLimit = -1;
  mCurrStackIdx = getCurrStackIdx();

  bool failed = allocArraysIfNeeded();
  if (!failed)
    fillAndProcessVols(true);

  Iobj *xobj = ivwGetAnExtraObject(mVi, mExtraObjNum);
  xobj->flags |= IMOD_OBJFLAG_EXTRA_EDIT;
  if (iisData.flags & IIS_VIEW_ISOSURFACE ) {
    xobj->flags= xobj->flags | IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_FILL |
      IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |IMOD_OBJFLAG_TWO_SIDE;
  } else {
    xobj->flags = xobj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
  }
  strcpy(xobj->name, "Isosurface extra object");

  // Original proposal was 115-79-26
  xobj->red = 115. / 255.;
  xobj->green = 79. / 255.;
  xobj->blue = 26. / 255.;
  xobj->ambient = 126;
  xobj->diffuse = 118;
  xobj->specular = 61;

  Iobj *boxObj = ivwGetAnExtraObject(mVi, mBoxObjNum);
  boxObj->flags |= IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_EXTRA_EDIT; 
  if (iisData.flags & IIS_VIEW_BOX) { 
    boxObj->flags &= ~IMOD_OBJFLAG_OFF;
  } else {
    boxObj->flags |= IMOD_OBJFLAG_OFF;
  }
  strcpy(boxObj->name, "Bounding box of isosurface");

  imodvObjedNewView();

  if (iisData.flags & IIS_VIEW_USER_MODEL) {
    iisData.a->drawExtraOnly = 0;
  } else {
    iisData.a->drawExtraOnly = 1;
  }

  if (!failed)
    setIsoObj(true);
  //mHistSlider->setValue(0,mThreshold);

  if(flagDrawXYZ || (iisData.flags & IIS_VIEW_BOX) )
    imodDraw(mVi, IMOD_DRAW_XYZ|IMOD_DRAW_MOD);
  else imodvDraw(Imodv);

  if(imodDebug('U') ){
    imodPrintStderr("In constructor: rendering time=%d \n", isoTime.elapsed() );
    isoTime.start();
  }
}

ImodvIsosurface::~ImodvIsosurface()
{
}

bool ImodvIsosurface::allocArraysIfNeeded()
{
  int newSize = mBoxSize[0] * mBoxSize[1] * mBoxSize[2];
  if (newSize > mCurrMax) {
    B3DFREE(mVolume);
    B3DFREE(mTrueBinVol);
    B3DFREE(mPaintVol);
    mCurrMax = newSize;
    mVolume = (unsigned char*)malloc(newSize * sizeof(unsigned char));
    mTrueBinVol = (unsigned char*)malloc(newSize * sizeof(unsigned char) / 8);
    if (iisData.flags & IIS_PAINT_OBJECT)
      mPaintVol = B3DMALLOC(unsigned char, newSize);
    if(!mVolume || !mTrueBinVol || ((iisData.flags & IIS_PAINT_OBJECT) && !mPaintVol)) {
      B3DFREE(mVolume);
      B3DFREE(mTrueBinVol);
      B3DFREE(mPaintVol);
      mVolume = NULL;
      mTrueBinVol = NULL;
      mPaintVol = NULL;
      wprint("\aFailed to allocate memory for %d voxels\n", newSize);
      return true;
    }//else imodPrintStderr("allocate mem for %d voxels\n", newSize);
  }
  return false;
}

/*!
 * Returns the maximum time index, the current time is returned in [outTime].
 * When there is only a single image file, the maximum index and time index 
 * are both 0; when there are multiple times the index is numbered from 1.
 */
int ImodvIsosurface::getCurrStackIdx()
{
  if(mVi->numTimes == 0)
    return 0;
  else
    return mVi->curTime - 1;
}

/*
 * Do the full sequence of operations on the volume to be rendered: fill, bin, mask,
 * remove pixels above outer threshold
 */
void ImodvIsosurface::fillAndProcessVols(bool setThresh)
{
  float threshold = fillVolumeArray();
  if (doDumps)
    dumpVolume("isovol1.st");
  // Maintain threshold and outer limit values and sliders
  if (setThresh) {
    mThreshold = threshold;
    mStackThresholds[mCurrStackIdx] = mThreshold;
  }    
  mHistSlider->setMinMaxVal(0, mVolMin, mVolMax, (int)mThreshold);
  if (mOuterLimit <= mVolMin || mOuterLimit >= mVolMax) {
    mOuterLimit = -1;
    mHistSlider->setMinMaxVal(1, mVolMin, mVolMax, 
                              mThreshold < mMedian ? mVolMin : mVolMax);
  } else 
    mHistSlider->setMinMaxVal(1, mVolMin, mVolMax, mOuterLimit);
  applyMask();
  fillBinVolume();
  closeBoxFaces();
  
  removeOuterPixels();
  if (doDumps)
    dumpVolume("isovol2.st");
  doDumps = 0;
}

/*
 * Bin the volume if needed, set bin volume pointer to full volume if not
 */
void ImodvIsosurface::fillBinVolume()
{
  int binNum = iisData.binningNum;
  float denom = binNum*binNum*binNum;
  float value;
  if (binNum == 1) {
    mBinVolume = mVolume;
    return;
  }
  mBinVolume = mTrueBinVol;

  for (int zi =0; zi<mBinBoxSize[2]; zi++)
    for (int yi = 0; yi<mBinBoxSize[1]; yi++)
      for (int xi = 0; xi<mBinBoxSize[0]; xi++) {

         value = 0.0;
         for (int zii = 0; zii<binNum; zii++)
           for (int yii = 0; yii<binNum; yii++)
             for (int xii = 0; xii<binNum; xii++) {
               value += mVolume[(zi*binNum+zii)*mBoxSize[0]*mBoxSize[1]
                              + (yi*binNum+yii)*mBoxSize[0]
                              +  xi*binNum+xii ];
              }
         mBinVolume[zi*mBinBoxSize[0]*mBinBoxSize[1] + yi*mBinBoxSize[0] + xi]
           = value/denom;
      }
}

/*
 * Fill the unbinned volume array from the loaded image data and get the min, max, median
 * and return a potential threshold
 */
float ImodvIsosurface::fillVolumeArray()
{
  unsigned char **imdata;
  int tempCacheSum;
  int vmnullvalue = (mVi->white + mVi->black) / 2;
  unsigned char *bmap = NULL;
  int i;
  int value;

  /* Set up image pointer tables */
  if (mVi->pyrCache) {
    if (ivwSetupFastTileAccess(mVi, mVi->pyrCache->getBaseIndex(), vmnullvalue, 
                               tempCacheSum))
      return -1;
  } else {
    if ( ivwSetupFastAccess(mVi, &imdata, vmnullvalue, &tempCacheSum) )
      return -1;
  }
  if (mVi->ushortStore) {
    bmap = ivwUShortInRangeToByteMap(mVi);
    if (!bmap)
      return -1;
    mRangeLow = mVi->rangeLow;
    mRangeHigh = mVi->rangeHigh;
  }

  //mVolume is a 3d array of Fortran type, i.e., column-major;
  //stride should be set to {1, mBoxSize[0], mBoxSize[0]*mBoxSize[1]};
  mVolMin = 255;
  mVolMax = 0;
  float *hist = mHistPanel->getHist();
  for(i = 0; i<256; i++)
    hist[i] = 0.0;

  for (int zi = mBoxOrigin[2]; zi<mBoxEnds[2]; zi++)
    for (int yi = mBoxOrigin[1]; yi<mBoxEnds[1]; yi++)
      for (int xi = mBoxOrigin[0]; xi<mBoxEnds[0]; xi++)
      {
        value = (*ivwFastGetValue)(xi, yi, zi);
        if (bmap)
          value = bmap[value];
        mVolume[(zi-mBoxOrigin[2])*mBoxSize[0]*mBoxSize[1]
                + (yi-mBoxOrigin[1])*mBoxSize[0]
                + xi-mBoxOrigin[0]] = value;
        hist[value] += 1.0;
        if(value<mVolMin) mVolMin = value;
        if(value>mVolMax) mVolMax = value;
      }

  if ((mVolMax-mVolMin)<10 ) {
    mVolMin = B3DMIN(0, mVolMin-5);
    mVolMax = B3DMAX(255, mVolMax+5);
  }
  mHistPanel->setMinMax(mVolMin, mVolMax);

  for (i = 0; i<256; i++) 
    hist[i] = hist[i]/(mBoxSize[0]*mBoxSize[1]*mBoxSize[2]);
  mHistPanel->setHistMinMax();

  mHistPanel->update();
  mMedian = mHistPanel->computePercentile(0.5);
  B3DFREE(bmap);
  //imodPrintStderr("Min=%d, Max=%d, Median=%.1f\n", mVolMin, mVolMax, mMedian);
  return mHistPanel->computePercentile(ImodPrefs->isoHighThresh() ? 
                                       1. - PERCENTILE : PERCENTILE);
}

/*
 * Apply a mask from one or many contours, or from a sphere or ellipsoid
 */
void ImodvIsosurface::applyMask()
{
  Iobj *obj = imodObjectGet(Imodv->imod);
  Icont *cont = imodContourGet(Imodv->imod);
  int co, zmin, zmax, zlsize, nummax, znlast, znear, otherZ;
  int *contz, *zlist, *numatz, **contatz;
  Icont *scan,*scant, *scanCum = NULL;
  Imesh *interpMesh = NULL;
  int ix, iy, iz, xst, xnd, bco, tco;
  Ipoint scale = {1., 1., 1.};
  int nx = mBoxSize[0];
  int ny = mBoxSize[1];
  int nz = mBoxSize[2];
  float aa, bb, cc, dx, dy, dxasq, dz, xcen, ycen, zcen;
  bool useLasso = iisData.maskType == MASK_LASSO;
  xcen = (nx - 1.) / 2.;
  ycen = (ny - 1.) / 2.;
  zcen = (nz - 1.) / 2.;

  if (iisData.maskType == MASK_NONE) {
    return;
  } else if (iisData.maskType == MASK_CONTOUR || useLasso) {

    // Contour masking is simple, just call the routine on each Z plane
    if (useLasso)
      cont = getTopZapLassoContour(false);
    if ((!useLasso && (!obj || !obj->contsize || !iobjClose(obj->flags))) || !cont)
      return;
    scan = imodel_contour_scan(cont);
    for (iz = 0; iz < nz; iz++)
      maskWithContour(scan, iz);
    imodContourDelete(scan);

  } else if (iisData.maskType == MASK_OBJECT) {

    // Masking with an object requires the Z tables
    if (!obj || !obj->contsize || !iobjClose(obj->flags))
      return;
    if (imodContourMakeZTables(obj, 1, 0, &contz, &zlist, &numatz, &contatz, &zmin, &zmax,
                               &zlsize, &nummax))
      return;
    if (zlsize) {
      znlast = zmin - 100;
      for (iz = 0; iz < mBoxSize[2]; iz++) {

        // Find the closest contour in Z, and the one on the other side if any
        znear = findClosestZ(iz, zlist, zlsize, contatz, numatz, zmin, otherZ);
        if (znear == INT_MAX)
          continue;
        if (znear != iz + mBoxOrigin[2] && otherZ < INT_MAX && numatz[znear-zmin] == 1 &&
            numatz[otherZ-zmin] == 1) {

          // If this Z is between two and only two contours, then reset the scan contours
          // from particular sections, get a mesh between the two contours if it hasn't
          // been gotten yet, then get an interpolated contour as in imodfillin
          imodContourDelete(scanCum);
          scanCum = NULL;
          znlast = zmin - 100;
          if (!interpMesh) {
            bco = contatz[znear-zmin][0];
            tco = contatz[otherZ-zmin][0];
            if (otherZ < znear) {
              tco = contatz[znear-zmin][0];
              bco = contatz[otherZ-zmin][0];
            }
            interpMesh = imeshContoursCost(obj, &obj->cont[bco], &obj->cont[tco], &scale,
                                           0, bco, tco);
          }
          if (!interpMesh || interpMesh->lsize < 5)
            continue;
          cont = imodContourNew();
          if (!cont)
            continue;
          imodMeshInterpCont(interpMesh->list, interpMesh->vert,
                             (interpMesh->lsize - 3) / 3, 1, 1, iz + mBoxOrigin[2], cont);
          
          // Mask with the interpolated contour and delete it
          maskWithContour(cont, iz);
          imodContourDelete(cont);

        } else {

          // If not interpolating, then combine all the scan contours on this Z plane
          // and mask with that.  Reuse this on nearby planes.
          if (znear != znlast) {
            imodContourDelete(scanCum);
            scanCum = NULL;
            imodMeshDelete(interpMesh);
            interpMesh = NULL;
            for (co = 0; co < numatz[znear-zmin]; co++) {
              cont = &obj->cont[contatz[znear-zmin][co]];
              if (cont->psize < 3)
                continue;
              scan = imodel_contour_scan(cont);
              if (scanCum) {
                scant = scanCum;
                scanCum = imodContourScanAdd(scanCum, scan);
                imodContourDelete(scan);
                imodContourDelete(scant);
              } else
                scanCum = scan;
            }
            znlast = znear;
          }

          maskWithContour(scanCum, iz);
        }
      }
    }
    imodContourDelete(scanCum);
    imodMeshDelete(interpMesh);
    imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);


  } else if (iisData.maskType == MASK_SPHERE || iisData.maskType == MASK_ELLIPSOID) {

    // For spherical or elliptical masking, get the axis lengths then solve for the
    // X coordinate at each Y and Z
    if (iisData.maskType == MASK_SPHERE) {
      aa = bb = cc = b3dIMin(3, nx, ny, nz) / 2.;
      xcen = B3DMAX(aa, B3DMIN(nx - aa, mLocalX - mBoxOrigin[0])) - 0.5;
      ycen = B3DMAX(aa, B3DMIN(ny - aa, mLocalY - mBoxOrigin[1])) - 0.5;
      zcen = B3DMAX(aa, B3DMIN(nz - aa, mLocalZ - mBoxOrigin[2])) - 0.5;
    } else {
      aa = nx / 2.;
      bb = ny / 2.;
      cc = nz / 2.;
    }
    for (iz = 0; iz < nz; iz++) {
      dz = (iz - zcen) / cc;
      for (iy = 0; iy < ny; iy++) {
        dy = (iy - ycen) / bb;
        dxasq = 1. - dz * dz - dy * dy;
        xst = xcen + 1;
        xnd = xst - 1;
        if (dxasq > 0) {
          dx = aa * sqrt((double)dxasq);
          xst = B3DNINT(xcen - dx);
          xnd = B3DNINT(xcen + dx);
        }
        for (ix = 0; ix < xst; ix++)
          mVolume[ix + iy * nx + iz * nx * ny] = mMedian;
        for (ix = xnd + 1; ix < nx; ix++)
          mVolume[ix + iy * nx + iz * nx * ny] = mMedian;
      }
    }
  }
}

/*
 * Remove pixels above the outer threshold and adjacent ones between threshold and 
 * outer threshold
 */
void ImodvIsosurface::removeOuterPixels()
{
  IsoPoint3D *neighbors = NULL;
  int dx[] = { 0, -1,  0,  1,  0, -1,  0,  1, -1,  1, -1,  0,  1,  1,  1,  1,  1,  1};
  int dy[] = {-1,  0,  0,  0,  1, -1, -1, -1,  0,  0,  1,  1,  1, -1,  0,  0,  0,  1};
  int dz[] = {-1, -1, -1, -1, -1,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1};
  int nx = mBinBoxSize[0];
  int ny = mBinBoxSize[1];
  int nz = mBinBoxSize[2];
  int maxShell = 16;  // WHAT TO SET THIS TO?
  int indNeigh, maxNeigh = 0, numNeigh = 0, numOrig, shell, i,ix, iy, iz,  median;
  int ind, dir = mThreshold > mMedian ? 1 : -1;
  if (mOuterLimit <= mVolMin || mOuterLimit >= mVolMax)
    return;
  unsigned char *onList = (unsigned char *)malloc(nx * ny *nz);
  if (!onList)
    return;
  for (i = 0; i < nx * ny * nz; i++)
    onList[i] = 0;

  median = B3DNINT(mMedian);
  median = mThreshold - 2 * dir;
  for (iz = 0; iz < nz; iz++)
    for (iy = 0; iy < ny; iy++)
      for (ix = 0; ix < nx; ix++) {
        ind = ix + iy * nx + iz * nx * ny;
        if (dir * (mBinVolume[ind] - mOuterLimit) > 0) {

          // For each point above limit, mark as level 1, set to median, and 
          // put on list of points to examine neighbors of
          onList[ind] = 1;
          mBinVolume[ind] = median;
          addToNeighborList(&neighbors, numNeigh, maxNeigh, ix, iy, iz);
        }
      }

  if (!numNeigh)
    return;

  // Loop on shells progressively farther out, making list of neighbors to check
  for (shell = 1; shell <= maxShell; shell++) {
    numOrig = numNeigh;

    // Check each neighbor, set to neutral value if above threshold, and add to list
    // to check
    for (indNeigh = 0; indNeigh < numOrig ; indNeigh++) {
      ix = neighbors[indNeigh].ix;
      iy = neighbors[indNeigh].iy;
      iz = neighbors[indNeigh].iz;
      if (iz > 0 && iz < nz - 1 && iy > 0 && iy < ny - 1 && ix > 0 && ix < nx - 1) {
        
        // Rapid loop with no testing
        for (i = 0; i < 18; i++) {
          ix = neighbors[indNeigh].ix + dx[i];
          iy = neighbors[indNeigh].iy + dy[i];
          iz = neighbors[indNeigh].iz + dz[i];
          ind = ix + iy * nx + iz * nx * ny;
          if (!onList[ind] && dir * (mBinVolume[ind] - mThreshold) > 0) {
            onList[ind] = shell + 1;
            mBinVolume[ind] = median;
            addToNeighborList(&neighbors, numNeigh, maxNeigh, ix, iy, iz);
            if (!maxNeigh)
              return;
          }
        }
      } else {

        // Slower loop with testing for points at edge
        for (i = 0; i < 18; i++) {
          ix = neighbors[indNeigh].ix + dx[i];
          iy = neighbors[indNeigh].iy + dy[i];
          iz = neighbors[indNeigh].iz + dz[i];
          if (iz >= 0 && iz < nz && iy >= 0 && iy < ny && ix >= 0 && ix < nx) {
            ind = ix + iy * nx + iz * nx * ny;
            if (!onList[ind] && dir * (mBinVolume[ind] - mThreshold) > 0) {
              onList[ind] = shell + 1;
              mBinVolume[ind] = median;
              addToNeighborList(&neighbors, numNeigh, maxNeigh, ix, iy, iz);
              if (!maxNeigh)
                return;
            }
          }
        }
      }
    }

    // Shift neighbor list down
    for (i = 0; i < numNeigh - numOrig; i++)
      neighbors[i] = neighbors[i + numOrig];
    numNeigh -= numOrig;
    if (!numNeigh)
      break;
  }

  B3DFREE(neighbors);
  free(onList);
}

void ImodvIsosurface::addToNeighborList(IsoPoint3D **neighp, int &numNeigh, int &maxNeigh,
                                        int ix, int iy, int iz)
{
  int quantum = 10000;
  if (numNeigh >= maxNeigh) {
    maxNeigh += quantum;
    if (*neighp)
      *neighp = (IsoPoint3D *)realloc(*neighp, maxNeigh * sizeof(IsoPoint3D));
    else
      *neighp = (IsoPoint3D *)malloc(maxNeigh * sizeof(IsoPoint3D));
    if (!*neighp) {
      numNeigh = 0;
      maxNeigh = 0;
      return;
    }
  }
  (*neighp)[numNeigh].ix = ix;
  (*neighp)[numNeigh].iy = iy;
  (*neighp)[numNeigh++].iz = iz;
}

/*
 * Set all points outside the contour at the given Z plane to the median
 */
int ImodvIsosurface::maskWithContour(Icont *inCont, int iz)
{
  int ix, iy, ycur, cpt, xstart, segStart, segEnd, yzbase;
  Icont *cont = inCont;

  if (!inCont || inCont->psize < 3)
    return 1;

  // Make a scan contour if it is not already
  if (!(cont->flags & ICONT_SCANLINE)) {
    cont = imodel_contour_scan(inCont);
    if (!cont || !cont->psize)
      return 1;
  }

  // Initialize current point and Y level
  cpt = 0;
  ycur = (int)cont->pts[cpt].y - mBoxOrigin[1];
  
  // Loop on the lines on this plane
  for (iy = 0; iy < mBoxSize[1]; iy++) {
    yzbase = iz * mBoxSize[0] * mBoxSize[1] + iy * mBoxSize[0];

    // Advance to a segment matching this line if possible/necessary
    while (ycur < iy && cpt < cont->psize - 3) {
      cpt += 2;
      ycur = (int)cont->pts[cpt].y - mBoxOrigin[1];
    }

    // If no segments at this Y, mask out the line
    if (ycur != iy) {
      for (ix = 0; ix < mBoxSize[0]; ix++)
        mVolume[ix + yzbase] = mMedian;
    } else {
      
      // Otherwise loop on the segments on this line
      xstart = 0;
      do {
        segStart = B3DNINT(cont->pts[cpt].x) - mBoxOrigin[0];
        segEnd = B3DNINT(cont->pts[cpt+1].x) - mBoxOrigin[0];

        // If segment starts after current point in X, mask out up to the start
        if (segStart > xstart)
          for (ix = xstart; ix < segStart; ix++)
            mVolume[ix + yzbase] = mMedian;

        // Advance the start to past the end of this segment
        xstart = B3DMAX(segEnd + 1, xstart);
        cpt += 2;
        if (cpt >= cont->psize - 1)
          break;
        ycur = (int)cont->pts[cpt].y - mBoxOrigin[1];
      } while (ycur == iy);

      // Mask out rest of line if segment ended before end
      for (ix = xstart; ix < mBoxSize[0]; ix++)
        mVolume[ix + yzbase] = mMedian;
    }
  }
  if (!(inCont->flags & ICONT_SCANLINE))
    imodContourDelete(cont);
  return 0;
}

/*
 * Set the faces of the binned box to the median, which will close openings
 */
void ImodvIsosurface::closeBoxFaces()
{
  int ix, iy, iz;
  int nx = mBinBoxSize[0];
  int ny = mBinBoxSize[1];
  int nz = mBinBoxSize[2];
  if (!(iisData.flags & IIS_CLOSE_FACES))
    return;
  for (iy = 0; iy < ny; iy++) {
    for (ix = 0; ix < nx; ix++) {
      mBinVolume[ix + iy * nx] = mMedian;
      mBinVolume[ix + iy * nx + (nz - 1) * nx * ny] = mMedian;
    }
  }
  for (iz = 0; iz < nz; iz++) {
    for (ix = 0; ix < nx; ix++) {
      mBinVolume[ix + iz * nx * ny] = mMedian;
      mBinVolume[ix + (ny - 1) * nx + iz * nx * ny] = mMedian;
    }
    for (iy = 0; iy < ny; iy++) {
      mBinVolume[iy * nx + iz * nx * ny] = mMedian;
      mBinVolume[ix -1 + iy * nx + iz * nx * ny] = mMedian;
    }
  }
}

/*
 * Compute the isosurface and place it into the extra object
 */
void ImodvIsosurface::setIsoObj(bool fillPaint)
{
  int i, ii, j, jj, k;
  b3dInt32 *triangle[MAX_THREADS];
  b3dInt32 *finalTriangle;
  int nTriangle[MAX_THREADS];
  Ipoint *vertex_xyz[MAX_THREADS];
  Ipoint *finalVertex_xyz;
  Ipoint *lastVert, *curVert;
  int *lastTri, *curTri;
  int nVertex[MAX_THREADS];
  Imesh *mcubeMesh;
  int validNTriangle[MAX_THREADS];
  int skipNTriangle[MAX_THREADS];
  int validNVertex[MAX_THREADS];
  int skipNVertex[MAX_THREADS];
  int offset[MAX_THREADS];
  int vertStart[MAX_THREADS];
  int triStart[MAX_THREADS];
  int validVertexSum = 0;
  bool flagFind;

  // Added for multi-processing with binning;
  int cOrigZ[MAX_THREADS];
  float newThreshold;
  int binNum = iisData.binningNum;
  cOrigZ[0] = mSubZEnds[0];
  for (i = 1; i<mNThreads; i++)
    cOrigZ[i] = mSubZEnds[0]+binNum*(mSubZEnds[i]-mSubZEnds[0])-binNum;


  offset[0] = 0; //will always be 0; so are skipNVertex[0] and skipNTriangle[0];

  //free all memory used by the old mesh; 
  ivwClearAnExtraObject(mVi, mExtraObjNum);
  Iobj *extraObj = ivwGetAnExtraObject(mVi, mExtraObjNum);

  if(imodDebug('U') ){
    imodPrintStderr("Using %d threads\n", mNThreads);
    isoTime.start();
  }
  for(i = 0; i<mNThreads; i++){
    threads[i]->start();
  }
  for(i = 0; i<mNThreads; i++){
    threads[i]->wait();
  }

  if(imodDebug('U') ){
    imodPrintStderr("mcube time=%d \n", isoTime.elapsed() );
    isoTime.start();
  } 
  for(i = 0; i<mNThreads; i++){

    nVertex[i] = threads[i]->getNVertex();
    nTriangle[i] = threads[i]->getNTriangle();
    vertex_xyz[i] = threads[i]->getVertex_xyz();
    triangle[i] = threads[i]->getTriangle();

    skipNVertex[i] = 0;
    validNVertex[i] = nVertex[i];
    skipNTriangle[i] = 0;
    validNTriangle[i] = nTriangle[i];
    vertStart[0] = 0;
    triStart[0] = 1;
    if (i>0){// starting from the second subslice
      if(imodDebug('U') ) imodPrintStderr("\n *************loop=%d\n", i);

      flagFind = false;

      for(ii = 0; ii < 2 * nVertex[i]; ii += 2){
        if (vertex_xyz[i][ii].z > cOrigZ[i] + binNum + binNum - TOL) 
        //if (vertex_xyz[i][ii].z>mSubZEnds[i]+1.0-TOL ) //old code for without binning; 
          break;
      }
      skipNVertex[i] = ii/2;

      /* Remove redundant vertices in the vertex list of previous subslices.
         Searching for the vertex that has the same coordinates as the last 
         skipped vertex of the current subslices.
         */
      jj = 0;
      if (skipNVertex[i]) {
        for(j = 2*validNVertex[i-1]-2; j >= 0; j -= 2)
          if ( fabs(vertex_xyz[i-1][j].x - vertex_xyz[i][ii-2].x) > TOL ||
               fabs(vertex_xyz[i-1][j].y - vertex_xyz[i][ii-2].y) > TOL ||
               fabs(vertex_xyz[i-1][j].z - vertex_xyz[i][ii-2].z) > TOL )
            jj++;
          else 
            break;
      }
      validNVertex[i-1] -= jj + skipNVertex[i-1];

      if(imodDebug('U') )
        imodPrintStderr("skipNVertex[%d]=%d remove %d redundant vertices  %d\n"
                        , i-1, skipNVertex[i-1], jj, isoTime.elapsed());

      validVertexSum += validNVertex[i-1];
      offset[i] = validVertexSum - skipNVertex[i];

      k = -1;
      int lowz, highz;
      lowz = -1;
      highz = -1;

      // Find the first triangle in the current subslice with at least one 
      // vertex above the dividing Z value
      for (ii = 0; ii<nTriangle[i]; ii++) {  
        newThreshold = cOrigZ[i]+binNum +TOL;

        // Here is code to detect whether polygons are ever out of order
        /* int minz = floor((B3DMIN(vertex_xyz[i][ 2*triangle[i][3*ii]].z, 
                      B3DMIN(vertex_xyz[i][ 2*triangle[i][3*ii+1]].z,
                             vertex_xyz[i][ 2*triangle[i][3*ii+2]].z)))/binNum);
        int maxz = ceil((B3DMAX(vertex_xyz[i][ 2*triangle[i][3*ii]].z,
                      B3DMAX(vertex_xyz[i][ 2*triangle[i][3*ii+1]].z,
                             vertex_xyz[i][ 2*triangle[i][3*ii+2]].z))) / binNum);
        if (lowz < 0) {
          lowz = minz;
          highz = maxz;
        } else if (minz < lowz || maxz < highz) {
            imodPrintStderr("triangle out of order %d %d %d %d\n",lowz,highz,
            minz,maxz);
        } else if (minz != lowz || maxz != highz) {
          //imodPrintStderr("new polygon %d %d\n",minz,maxz);
          lowz = minz;
          highz = maxz;
          } */

        if (vertex_xyz[i][ 2*triangle[i][3*ii]].z> newThreshold ||
            vertex_xyz[i][ 2*triangle[i][3*ii+1]].z>newThreshold ||
            vertex_xyz[i][ 2*triangle[i][3*ii+2]].z>newThreshold ) {
          break;

        } else
          k = ii; //skipNTriangle[i]++;
      }

      // k is left pointing to the last triangle before this criterion is met
      if(k>-1) 
        skipNTriangle[i] = k+1;
      /*if(imodDebug('U') )
        imodPrintStderr("Got skipNTriangle at %d \n", isoTime.elapsed()); */

      /* Remove redundant triangles in the triangle list of previous subslices.
         Searching for the triangle that indexes into the same vertices as the
         last skipped triangle of the current subslice.
         */
      jj = 0;
      if (skipNTriangle[i]) {
        k = 3*(skipNTriangle[i]-1);
        lastVert = vertex_xyz[i-1];
        lastTri = triangle[i-1];
        if (i == 1)
          lastTri += 1;
        curVert = vertex_xyz[i];
        curTri = triangle[i];
        for (j = nTriangle[i-1]-1; j >= 0; j--)
          if (fabs(lastVert[2*lastTri[3*j]].x - curVert[2*curTri[k]].x) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j]].y - curVert[2*curTri[k]].y) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j]].z - curVert[2*curTri[k]].z) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+1]].x - curVert[2*curTri[k+1]].x) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+1]].y - curVert[2*curTri[k+1]].y) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+1]].z - curVert[2*curTri[k+1]].z) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+2]].x - curVert[2*curTri[k+2]].x) <
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+2]].y - curVert[2*curTri[k+2]].y) < 
              TOL  &&
              fabs(lastVert[2*lastTri[3*j+2]].z - curVert[2*curTri[k+2]].z) < 
              TOL) {
            flagFind = true; 
            break;
          } else
            jj++;
      }
      if(!flagFind) jj = 0;
      validNTriangle[i-1] -= jj+skipNTriangle[i-1];
      vertStart[i] = vertStart[i-1] + 2*validNVertex[i-1];
      triStart[i] = triStart[i-1] + 3*validNTriangle[i-1];

      if(imodDebug('U') )
        imodPrintStderr("skipNTriangle[%d]=%d remove %d redundant triangles  %d\n", i-1, skipNTriangle[i-1], jj, isoTime.elapsed());

    }

  }//for mNThreads 


  //imodPrintStderr("skipNTriangle[%d]=%d skipNVertex[%d]=%d \n", mNThreads-1,
  //    skipNTriangle[mNThreads-1], mNThreads-1, skipNVertex[mNThreads-1]);
  
  // Code for dumping the indexes and vertices to file
  /*char fnVertex[50];
    char fnTriangle[50];
    FILE *fpV;
    FILE *fpT;
    for(i = 0; i<mNThreads; i++){
    imodPrintStderr("mSubZEnds[%d]=%d\n", i, mSubZEnds[i]);
    imodPrintStderr("nVertex[%d]=%d skipNVertex[%d]=%d, validNVertex[%d]=%d \n",
    i, nVertex[i], i, skipNVertex[i], i, validNVertex[i] );
    imodPrintStderr("nTriangle[%d]=%d, skipNTriangle[%d]=%d, validNTriangle[%d]=%d\n",
    i, nTriangle[i], i, skipNTriangle[i], i, validNTriangle[i]);

    sprintf(fnVertex, "%d%s", i, "_vertex.txt");
    fpV = fopen(fnVertex,"w");
    for(ii = 0; ii<2*nVertex[i]; ii += 2){
    fprintf(fpV, "vertex[%d] x y z= %f \t %f \t %f \n", ii/2, vertex_xyz[i][ii].x,
    vertex_xyz[i][ii].y,  vertex_xyz[i][ii].z);
    }
    fclose(fpV);

    sprintf(fnTriangle, "%d%s", i, "_triangle.txt");
    fpT = fopen(fnTriangle,"w");
    for(ii = 0; ii<nTriangle[i]; ii++){
    fprintf(fpT, "triange[%d] vertex 1 2 3= %d \t %d \t %d \n", 
    ii,triangle[i][3*ii] ,triangle[i][3*ii+1], triangle[i][3*ii+2] );
    }
    fclose(fpT);
    }*/

  //merge vertex and triangle lists after setting numbers for the last subslice
  validNVertex[mNThreads-1] = nVertex[mNThreads-1] - skipNVertex[mNThreads-1];
  validNTriangle[mNThreads-1] = nTriangle[mNThreads-1] - 
    skipNTriangle[mNThreads-1];
  int totalVertex = 0;
  int totalTriangle = 0;
  for(i = 0; i<mNThreads; i++) {
    totalTriangle += validNTriangle[i];
    totalVertex += validNVertex[i];
  }

  if (imodDebug('U') )
    imodPrintStderr("***totalVertex=%d totalTriangle=%d   %d\n", totalVertex,
                    totalTriangle, isoTime.elapsed());

  if(mNThreads>1){
    finalVertex_xyz = (Ipoint *)malloc(2*totalVertex*sizeof(Ipoint));
    finalTriangle = (b3dInt32*)malloc( (3*totalTriangle+3)*sizeof(b3dInt32) );

#pragma omp parallel for \
  shared(validNTriangle, finalTriangle, triStart, triangle, validNVertex, \
         vertStart, vertex_xyz, skipNVertex, skipNTriangle, offset)     \
  private(i, ii)
    for (i = 0; i<mNThreads; i++) {
      for (ii = 0; ii<2*validNVertex[i]; ii++)
        finalVertex_xyz[vertStart[i]+ii] = vertex_xyz[i][ii+2*skipNVertex[i]];

      if(i == 0) {

        // Indexes in first chunk are offset by 1
        for (ii = 0; ii<3*validNTriangle[i]; ii++)
          finalTriangle[triStart[i]+ii] = 2*(triangle[i][ii+1]);
      } else {
        for (ii = 0; ii<3*validNTriangle[i]; ii++)
          finalTriangle[triStart[i]+ii] = 2*(triangle[i][ii+3*skipNTriangle[i]]
                                             + offset[i]);
      }
    }

    for(i = 0; i<mNThreads; i++){
      free(vertex_xyz[i]);
      free(triangle[i]);
    }
  } else {
    
    // For one thread, still need to multiply indices by 2
    finalVertex_xyz = vertex_xyz[0];
    finalTriangle = triangle[0];
    for(i = 0; i<3*totalTriangle; i++)
      finalTriangle[1+i] *= 2;
  }
    
  /*if(imodDebug('U') )
    imodPrintStderr("indexes copied at %d \n", isoTime.elapsed()); */
  finalTriangle[0] = -25;
  finalTriangle[3*totalTriangle+1] = -22;
  finalTriangle[3*totalTriangle+2] = -1;
  
  //create a Imesh structure; 
  mcubeMesh = imodMeshNew();
  mcubeMesh->vert = finalVertex_xyz;
  mcubeMesh->lsize= 3*totalTriangle+3;
  mcubeMesh->vsize= 2*totalVertex;
  mcubeMesh->list = finalTriangle;

  if(imodDebug('U') ){
    imodPrintStderr("merge time=%d \n", isoTime.elapsed());
    isoTime.start();
  }

  imodMeshDelete(mOrigMesh);
  mOrigMesh = mcubeMesh;

  //filter out small pieces;
  filterMesh( iisData.flags & IIS_DELETE_PIECES );
  if(imodDebug('U') ){
    imodPrintStderr("filter time=%d \n", isoTime.elapsed());
    isoTime.start();
  }
  
  //need to create a dup and put in the extraObject;
  Imesh *dup = imodMeshDup(mFilteredMesh);

  //apply smoothing;
  if (iisData.itNum) {
    smoothMesh(dup, iisData.itNum);
    if(imodDebug('U') ){
      imodPrintStderr("smooth time=%d \n", isoTime.elapsed());
      isoTime.start();
    }
  }

  //attach the new mesh to the extra obj;
  imodObjectAddMesh(extraObj, dup);
  free(dup);

  // Refill the painting volume if called for, then paint mesh
  if (fillPaint)
    fillPaintVol();
  paintMesh();

  /*for(i = 0; i<mNThreads; i++){
    imodPrintStderr("mSubZEnds[%d]=%d\n", i, mSubZEnds[i]);
    imodPrintStderr("nVertex[%d]=%d skipNVertex[%d]=%d, validNVertex[%d]=%d \n",
    i, nVertex[i], i, skipNVertex[i], i, validNVertex[i] );
    imodPrintStderr("nTriangle[%d]=%d, skipNTriangle[%d]=%d, validNTriangle[%d]=%d\n",
    i, nTriangle[i], i, skipNTriangle[i], i, validNTriangle[i]);
    }
    imodPrintStderr("Threshold=%4.1f, x=%4.1f, y=%4.1f, z=%4.1f xSize=%d ySize=%d zSize=%d\n",
    mThreshold, mVi->xmouse, mVi->ymouse, mVi->zmouse,
    mBoxSize[0], mBoxSize[1], mBoxSize[2]);
    imodPrintStderr("\n");*/

  /*FILE *fp = fopen("combinedVertex.txt","w");
    for(ii = 0; ii<2*totalVertex; ii += 2){
    fprintf(fp, "vertex[%d] x y z= %f \t %f \t %f \n", ii/2, finalVertex_xyz[ii].x,
    finalVertex_xyz[ii].y,  finalVertex_xyz[ii].z);
    }
    fclose(fp);

    fp = fopen("combinedTriangle.txt","w");
    for(ii = 0; ii<totalTriangle; ii++){
    fprintf(fp, "triange[%d] vertex 1 2 3= %d \t %d \t %d \n", 
    ii,finalTriangle[3*ii+1]/2 ,finalTriangle[3*ii+2]/2,
    finalTriangle[3*ii+3]/2 );
    }
    fclose(fp);
    */
}

//Using current mouse position and drawSize to deduce the origin and ends 
//of the bounding box.
void ImodvIsosurface::setBoundingBox()
{
  setCoordLimits(mLocalX, mVi->xsize, xDrawSize, mBoxOrigin[0], mBoxEnds[0]);
  setCoordLimits(mLocalY, mVi->ysize, yDrawSize, mBoxOrigin[1], mBoxEnds[1]);
  setCoordLimits(mLocalZ, mVi->zsize, zDrawSize, mBoxOrigin[2], mBoxEnds[2]);
  mBoxSize[0] = xDrawSize;
  mBoxSize[1] = yDrawSize;
  mBoxSize[2] = zDrawSize;
  
  //set binning parameters
  int binNum = iisData.binningNum;
  int i;
  for (i = 0; i<3; i++) {
    mBinBoxSize[i] = mBoxSize[i]/binNum;
    mBinBoxEnds[i] = mBoxOrigin[i]+mBinBoxSize[i];
  }
  
  int fairShare = mBinBoxSize[2]/mInitNThreads;

  if (fairShare<8) { 
    mNThreads = 1;
    fairShare = mBinBoxSize[2];
  } else
    mNThreads = mInitNThreads;

  mSubZEnds[0] = mBoxOrigin[2];
  for(i = 1; i<mNThreads; i++)
    mSubZEnds[i] = mSubZEnds[i-1]+fairShare;
  mSubZEnds[mNThreads] = mBinBoxEnds[2]-1;

  if (imodDebug('U') ) 
    for(i = 0; i<mNThreads+1; i++)
      imodPrintStderr("mSubZEnds[%d]=%d\n", i, mSubZEnds[i]);
}

void ImodvIsosurface::setViewCenter()
{
  Imodv->imod->view->trans.x = -( (mBoxOrigin[0]+mBoxEnds[0])*0.5f);
  Imodv->imod->view->trans.y = -( (mBoxOrigin[1]+mBoxEnds[1])*0.5f);
  Imodv->imod->view->trans.z = -( (mBoxOrigin[2]+mBoxEnds[2])*0.5f);
}

void ImodvIsosurface::setBoundingObj()
{
  int i;
  Icont *contours = imodContoursNew(4);
  Ipoint corners[8];

  corners[0].x = mBoxOrigin[0];
  corners[0].y = mBoxOrigin[1];
  corners[0].z = mBoxOrigin[2];
  corners[1].x = mBoxEnds[0];
  corners[1].y = mBoxOrigin[1];
  corners[1].z = mBoxOrigin[2];
  corners[2].x = mBoxEnds[0];
  corners[2].y = mBoxEnds[1];
  corners[2].z = mBoxOrigin[2];
  corners[3].x = mBoxOrigin[0];
  corners[3].y = mBoxEnds[1];
  corners[3].z = mBoxOrigin[2];
  corners[4].x = mBoxOrigin[0];
  corners[4].y = mBoxOrigin[1];
  corners[4].z = mBoxEnds[2];
  corners[5].x = mBoxEnds[0];
  corners[5].y = mBoxOrigin[1];
  corners[5].z = mBoxEnds[2];
  corners[6].x = mBoxEnds[0];
  corners[6].y = mBoxEnds[1];
  corners[6].z = mBoxEnds[2];
  corners[7].x = mBoxOrigin[0];
  corners[7].y = mBoxEnds[1];
  corners[7].z = mBoxEnds[2];

  for(i = 0; i<4; i++){
    imodPointAdd(&contours[0], &corners[i], i);
    imodPointAdd(&contours[1], &corners[i+4], i);
  }
  imodPointAdd(&contours[2], &corners[0], 0);
  imodPointAdd(&contours[2], &corners[1], 1);
  imodPointAdd(&contours[2], &corners[5], 2);
  imodPointAdd(&contours[2], &corners[4], 3);
  imodPointAdd(&contours[3], &corners[3], 0);
  imodPointAdd(&contours[3], &corners[2], 1);
  imodPointAdd(&contours[3], &corners[6], 2);
  imodPointAdd(&contours[3], &corners[7], 3);

  contours[0].flags |= ICONT_DRAW_ALLZ;
  ivwClearAnExtraObject(mVi, mBoxObjNum);
  Iobj *bObj = ivwGetAnExtraObject(mVi, mBoxObjNum);
  imodObjectSetColor(bObj, 1.0, 1.0, 0.0);
  for(i = 0; i<4; i++) {
    imodObjectAddContour(bObj, &contours[i]);
  }
  free(contours);
}

// Update the current coordinate sliders and their ranges, update the ranges
// of the Y and Z size sliders and swap y and Z size if flipped
void ImodvIsosurface::updateCoords(bool setLocal)
{
  if (setLocal) {
    mLocalX = (int)mVi->xmouse;
    mLocalY = (int)mVi->ymouse;
    mLocalZ = (int)mVi->zmouse;
  }

  // This was a nint of mouse which would create disparity between mLocal and slider
  mSliders->setValue(IIS_X_COORD, mLocalX + 1);
  mSliders->setMinMaxVal(IIS_Y_COORD, 1, mVi->ysize, mLocalY + 1);
  mSliders->setMinMaxVal(IIS_Z_COORD, 1, mVi->zsize, mLocalZ + 1);

  if (lastYsize != mVi->ysize) {
    int tmpSize = yDrawSize;
    yDrawSize = zDrawSize;
    mBoxSize[1] = yDrawSize;
    zDrawSize = tmpSize;
    mBoxSize[2] = zDrawSize;

    int xMax, yMax, zMax;
    int stackSize[3];
    stackSize[0] = mVi->xsize;
    stackSize[1] = mVi->ysize;
    stackSize[2] = mVi->zsize;
    findDimLimits(FIND_MAXIMAL_DIM, xMax, yMax, zMax, stackSize);
    mSliders->setMinMaxVal(IIS_Y_SIZE, 1, yMax, yDrawSize);
    mSliders->setMinMaxVal(IIS_Z_SIZE, 1, zMax, zDrawSize);

    lastYsize = mVi->ysize;
  }
}

void ImodvIsosurface::viewIsoToggled(bool state)
{
  Iobj *xobj = ivwGetAnExtraObject(mVi, mExtraObjNum);
  setOrClearFlags(&iisData.flags, IIS_VIEW_ISOSURFACE, state ? 1 : 0);
  if (state) {
    xobj->flags= xobj->flags | IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_FILL |
      IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |IMOD_OBJFLAG_TWO_SIDE;
  } else {
    xobj->flags = xobj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
  }
  imodvDraw(Imodv);
}

void ImodvIsosurface::centerVolumeToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_CENTER_VOLUME, state ? 1 : 0);
  if(state){
    setViewCenter();
    imodvDraw(Imodv);
  }
}


void ImodvIsosurface::viewModelToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_VIEW_USER_MODEL, state ? 1 : 0);
  if(state){
    iisData.a->drawExtraOnly = 0;
  }else{
    iisData.a->drawExtraOnly = 1;
  }
  imodvDraw(Imodv);
}

void ImodvIsosurface::viewBoxingToggled(bool state)
{
  Iobj *boxObj = ivwGetAnExtraObject(mVi, mBoxObjNum);
  setOrClearFlags(&iisData.flags, IIS_VIEW_BOX, state ? 1 : 0);
  if(state)
  { 
    //boxObj->flags= boxObj->flags | IMOD_OBJFLAG_EXTRA_MODV; 
    boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_OFF;
  }else{
    //boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
    boxObj->flags= boxObj->flags | IMOD_OBJFLAG_OFF; 
  }
  //imodvDraw(Imodv);
  imodDraw(mVi, IMOD_DRAW_MOD);
}

void ImodvIsosurface::deletePiecesToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_DELETE_PIECES, state ? 1 : 0);
  filterMesh(state);
  iterNumChanged(iisData.itNum);
}

//Setting up mFilteredMesh using mOrigMesh;
void ImodvIsosurface::filterMesh(bool state)
{
  if (mMadeFiltered)
    imodMeshDelete(mFilteredMesh);
  mMadeFiltered = state;

  int totalTriangle = (mOrigMesh->lsize -3)/3;
  Ipoint *finalVertex_xyz = mOrigMesh->vert;
  b3dInt32 *finalTriangle = mOrigMesh->list;

  if (state) {
    mFilteredMesh = imodMeshDup(mOrigMesh);
    mPiecesBox->setEnabled(true);
    delete mSurfPieces;
    mSurfPieces = new Surface_Pieces(finalVertex_xyz, finalTriangle+1,
        totalTriangle, mFilteredMesh->list+1);

    int trNum = iisData.minTNum;
    int includedTriangle = 0;
    b3dInt32 *newTList;
    b3dInt32 *start;
    int totalCC= mSurfPieces->pieces.size();
    for(int ci = 0; ci<totalCC; ++ci){
      if (mSurfPieces->pieces[totalCC-ci-1].area > trNum )
        includedTriangle += mSurfPieces->pieces[totalCC-ci-1].tList->size();
      else break;
    }

    start = mFilteredMesh->list+3*(totalTriangle-includedTriangle);
    *(start) = -25;
    newTList = (b3dInt32*)malloc( (3*includedTriangle+3)*sizeof(b3dInt32) );
    for (int i = 0; i < 3*includedTriangle+3; i++) 
      *(newTList+i) = *(start+i);
    free(mFilteredMesh->list);
    mFilteredMesh->list = newTList;
    mFilteredMesh->lsize = 3*includedTriangle+3;

  }else{
    mPiecesBox->setEnabled(false);
    mFilteredMesh = mOrigMesh;
  }
}

void ImodvIsosurface::linkXYZToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_LINK_XYZ, state ? 1 : 0);
  if(state) {
    imodvIsosurfaceUpdate(0);
    imodDraw(mVi, IMOD_DRAW_MOD);
  }
}

void ImodvIsosurface::closeFacesToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_CLOSE_FACES, state ? 1 : 0);
  fillAndProcessVols(false);
  setIsoObj(false);
  imodvDraw(Imodv);
}

void ImodvIsosurface::smoothMesh(Imesh *mcubeMesh, int iterNum)
{
  Ipoint *vertex_xyz = mcubeMesh->vert;
  b3dInt32 *triangles = mcubeMesh->list +1; //skip the -25 code; 
  int nVertex = mcubeMesh->vsize/2;
  int nTriangle= (mcubeMesh->lsize-3)/3;
  smooth_vertex_positions((float *)vertex_xyz, nVertex, (Index *)triangles, nTriangle, 
                          0.3, iterNum);
}

void ImodvIsosurface::iterNumChanged(int iterNum)
{
  iisData.itNum = iterNum;
  ivwClearAnExtraObject(mVi, mExtraObjNum);
  Iobj *extraObj = ivwGetAnExtraObject(mVi, mExtraObjNum);
  Imesh *dup = imodMeshDup(mFilteredMesh);
  if(iterNum)
    smoothMesh(dup, iterNum);
  imodObjectAddMesh(extraObj, dup);
  setFocus();
  free(dup);
  paintMesh();
  imodvDraw(Imodv);
}

void ImodvIsosurface::binningNumChanged(int binningNum)
{
  int oldBin = iisData.binningNum;
  iisData.binningNum = binningNum;
  setBoundingBox();
  if (oldBin == 1 && ((iisData.flags & IIS_CLOSE_FACES) || 
                      (mOuterLimit > mVolMin && mOuterLimit < mVolMax))) {
    fillAndProcessVols(false);
  } else {
    fillBinVolume();
    closeBoxFaces();
    removeOuterPixels();
  }
  setIsoObj(false);
  setFocus();
  imodvDraw(Imodv);
}

int ImodvIsosurface::getBinning()
{
  return iisData.binningNum;
}

void ImodvIsosurface::numOfTrianglesChanged(int trNum)
{
  iisData.minTNum = trNum;
  deletePiecesToggled(true);
}

void ImodvIsosurface::paintObjToggled(bool state)
{
  setOrClearFlags(&iisData.flags, IIS_PAINT_OBJECT, state ? 1 : 0);
  if (state) {
    mPaintVol = B3DMALLOC(unsigned char, mBoxSize[0] *  mBoxSize[1] * mBoxSize[2]);
    if (!mPaintVol) {
      wprint("\aFailed to get memory for color mask volume\n");
      setOrClearFlags(&iisData.flags, IIS_PAINT_OBJECT, 0);
      mPaintCheck->setChecked(false);
    }
  } else {
    B3DFREE(mPaintVol);
    mPaintVol = NULL;
  }
  managePaintObject();
  fillPaintVol();
  paintMesh();
  imodvDraw(Imodv);
}

void ImodvIsosurface::paintObjChanged(int value)
{
  iisData.paintObj = value - 1;
  setFocus();
  fillPaintVol();
  paintMesh();
  imodvDraw(Imodv);
}

void ImodvIsosurface::managePaintObject()
{
  mLastObjsize = Imodv->imod->objsize;
  iisData.paintObj = B3DMAX(0, B3DMIN(mLastObjsize - 1, iisData.paintObj));
  diaSetSpinMMVal(mPaintObjSpin, 1, mLastObjsize, iisData.paintObj + 1);
  mPaintObjSpin->setEnabled(iisData.flags & IIS_PAINT_OBJECT);
}


void  ImodvIsosurface::histChanged(int which, int value, bool dragging)
{ 
  bool refill = false;
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)){ 
    if (which == 0) {
      mThreshold = value + 0.5;

      // If threshold changed, maintain outer limit to be outside threshold and set it out
      // of range if threshold crossed through median
      if (mOuterLimit >= 0) {
        if (mThreshold < mMedian) {
          if (mOuterLimit >= mMedian)
            mOuterLimit = -1;
          else
            mOuterLimit = B3DMIN(mOuterLimit, value);
        } else {
          if (mOuterLimit < mMedian)
            mOuterLimit = -1;
          else
            mOuterLimit = B3DMAX(mOuterLimit, value + 1);
        }
        if (mOuterLimit <= mVolMin || mOuterLimit >= mVolMax)
          mOuterLimit = -1;
        refill = true;
      }
    } else {

      // If outer limit changed, keep it outside the threshold
      if (mThreshold < mMedian)
        mOuterLimit = B3DMIN(value, (int)mThreshold);
      else
        mOuterLimit = B3DMAX(value, (int)mThreshold + 1);
      if (mOuterLimit <= mVolMin || mOuterLimit >= mVolMax)
        mOuterLimit = -1;
      refill = true;
    }
    if (refill)
      fillAndProcessVols(false);
    else {
      if (mOuterLimit < 0)
        mHistSlider->setValue(1, mThreshold < mMedian ? mVolMin : mVolMax);
      mHistSlider->setValue(0,mThreshold);
    }
    setIsoObj(false);
    mCurrStackIdx = getCurrStackIdx();
    mStackThresholds[mCurrStackIdx] = mThreshold;
    mStackOuterLims[mCurrStackIdx] = mOuterLimit;
    imodvDraw(Imodv);

    if(imodDebug('U') ){
      imodPrintStderr("In histChanged: rendering time=%d \n", isoTime.elapsed());
      isoTime.start();
    } 
  }
}

// respond to a change of size or center coordinate
void ImodvIsosurface::sliderMoved(int which, int value, bool dragging)
{

  switch (which) {
    case IIS_X_COORD:
      mLocalX = value - 1;
      break;
    case IIS_Y_COORD:
      mLocalY = value - 1;
      break;
    case IIS_Z_COORD:
      mLocalZ = value - 1;
      break;
    case IIS_X_SIZE:
      xDrawSize = value;
      break;
    case IIS_Y_SIZE:
      yDrawSize = value;
      break;
    case IIS_Z_SIZE:
      zDrawSize = value;
      break;
  }

  if ((iisData.flags & IIS_LINK_XYZ) && which <= IIS_Z_COORD ){
    mVi->xmouse = mLocalX;
    mVi->ymouse = mLocalY;
    mVi->zmouse = mLocalZ;
    ivwBindMouse(mVi);
  }

  // draw if slider clicked or is in hot state
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)) {
    bool boxChanged = isBoxChanged(mBoxOrigin, mBoxEnds);
    if (boxChanged || (iisData.maskType == MASK_SPHERE)) {
      setBoundingBox();
      if (iisData.flags & IIS_CENTER_VOLUME)
        setViewCenter();
      setBoundingObj();

      if (allocArraysIfNeeded()) {
        close();
        return;
      }


      fillAndProcessVols(false);
      setIsoObj(true);

      imodvDraw(Imodv); 
      if (which <= IIS_Z_COORD || (iisData.flags & IIS_VIEW_BOX))
        imodDraw(mVi, IMOD_DRAW_XYZ |IMOD_DRAW_SKIPMODV );

      if(imodDebug('U') ){
        imodPrintStderr("In sliderMoved: rendering time=%d \n", isoTime.elapsed());
        isoTime.start();
      }
    } else if (which <= IIS_Z_COORD)
      imodDraw(mVi, IMOD_DRAW_XYZ);
  }


}

void ImodvIsosurface::maskSelected(int which)
{
  iisData.maskType = which;
  mSizeContours->setEnabled(which == MASK_CONTOUR || which == MASK_OBJECT || 
                            which == MASK_LASSO);
  mSizeContours->setText(iisData.maskType == MASK_OBJECT ? "Object" : 
                         (iisData.maskType == MASK_LASSO ? "Lasso" : "Contour"));
  if (which == MASK_CONTOUR || which == MASK_OBJECT || which == MASK_LASSO) {
    resizeToContours(true);
  } else {
    fillAndProcessVols(false);
    setIsoObj(false);
    imodvDraw(Imodv);
  }
}

void ImodvIsosurface::areaFromContClicked()
{
  resizeToContours(true);
}

// Try to set the size from one or many contours
void ImodvIsosurface::resizeToContours(bool draw)
{
  Ipoint pmin, pmax, cmin, cmax;
  Iobj *obj = imodObjectGet(Imodv->imod);
  Icont *cont = imodContourGet(Imodv->imod);
  int curob, curco, co, otherZ, iz, zmin, zmax, zlsize, nummax, znear, znlast, found = 0;
  int *contz, *zlist, *numatz, **contatz;
  bool useLasso = iisData.maskType == MASK_LASSO;
  imodGetIndex(Imodv->imod, &curob, &curco, &co);

  if (!useLasso && (!obj || !obj->contsize || !iobjClose(obj->flags)))
    return;
  if (iisData.maskType == MASK_CONTOUR || useLasso) {
    if (useLasso) 
      cont = getTopZapLassoContour(false);
    if (!cont || cont->psize < 3)
      return;
    imodContourGetBBox(cont, &pmin, &pmax);
    mMaskPsize = cont->psize;
    mMaskObj = curob;
    mMaskCont = curco;
    mMaskChecksum = 0.;
    for (co = 0; co < cont->psize; co++)
      mMaskChecksum += cont->pts[co].x + cont->pts[co].y;

  } else if (iisData.maskType == MASK_OBJECT) {
    if (imodContourMakeZTables(obj, 1, 0, &contz, &zlist, &numatz, &contatz, &zmin, &zmax,
                               &zlsize, &nummax))
      return;
    if (zlsize) {
      znlast = zmin - 100;
      for (iz = 0; iz < mBoxSize[2]; iz++) {
        znear = findClosestZ(iz, zlist, zlsize, contatz, numatz, zmin, otherZ);
        if (znear != znlast && znear < INT_MAX) {
          for (co = 0; co < numatz[znear - zmin]; co++) {
            cont = &obj->cont[contatz[znear-zmin][co]];
            if (cont->psize < 3)
              continue;
            imodContourGetBBox(cont, &cmin, &cmax);
            if (!found) {
              pmin = cmin;
              pmax = cmax;
              found = 1;
            } else {
              pmin.x = B3DMIN(pmin.x, cmin.x);
              pmin.y = B3DMIN(pmin.y, cmin.y);
              pmax.x = B3DMAX(pmax.x, cmax.x);
              pmax.y = B3DMAX(pmax.y, cmax.y);
            }
          }
          znlast = znear;
        }
      }
    }
    imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);

    if (!found)
      return;
    mMaskObj = curob;
    mMaskChecksum = imodObjectChecksum(obj, curob);
  } else
    return;
  showDefinedArea(pmin.x, pmax.x, pmin.y, pmax.y, draw);
}

// Find the closest Z value with contours of at least 3 points and also return the
// closest one in the other direction if any
int ImodvIsosurface::findClosestZ(int iz, int *listz, int zlsize, 
                                  int **contatz, int *numatz, int zmin, int &otherSide)
{
  int nearz = INT_MAX, minAbove = INT_MAX, minBelow = INT_MAX;
  int delz, i, co, nearBelow, nearAbove;
  Icont *cont;
  Iobj *obj = imodObjectGet(Imodv->imod);
  iz += mBoxOrigin[2];
  otherSide = INT_MAX;
  for (i = 0; i < zlsize; i++) {
    delz = iz - listz[i];
    if ((delz >= 0 && delz < minBelow) || (delz < 0 && -delz < minAbove)) {
        
      // Replace the nearest Z only if there is a big enough contour
      for (co = 0; co < numatz[listz[i]-zmin]; co++) {
          cont = &obj->cont[contatz[listz[i]-zmin][co]];
        if (cont->psize >= 3) {
          if (delz >= 0) {
            minBelow = delz;
            nearBelow = listz[i];
          } else {
            minAbove = -delz;
            nearAbove = listz[i];
          }
          break;
        }
      }
    }
  }
  if (minBelow <= minAbove && minBelow < INT_MAX) {
    nearz = nearBelow;
    if (minAbove < INT_MAX)
      otherSide = nearAbove;
  } else if (minAbove < minBelow) {
    nearz = nearAbove;
    if (minBelow < INT_MAX)
      otherSide = nearBelow;
  }
  return nearz;
}


void ImodvIsosurface::showRubberBandArea()
{
  float x0, x1, y0, y1;
  if (zapRubberbandCoords(x0, x1, y0, y1))
    showDefinedArea(x0, x1, y0, y1, true);
}

// Reset the nox and center coordinates in best attempt to show the specified area
void ImodvIsosurface::showDefinedArea(float x0, float x1, float y0, float y1, bool draw)
{
  int tempSize[3];
  tempSize[0] = mVi->xsize;
  tempSize[1] = mVi->ysize;
  tempSize[2] = mVi->zsize;
  int xMax, yMax, zMax;
  findDimLimits(FIND_MAXIMAL_DIM, xMax, yMax, zMax, tempSize);

  if (xMax>x1-x0 )
    xDrawSize = x1-x0;
  else
    xDrawSize = xMax;

  if (yMax>y1-y0 )
    yDrawSize = y1-y0;
  else 
    yDrawSize = yMax;

  int xCenter= (x0+x1)/2.0;
  int yCenter = (y0+y1)/2.0;

  setCoordLimits( xCenter, mVi->xsize, xDrawSize, mBoxOrigin[0], mBoxEnds[0]);
  setCoordLimits( yCenter, mVi->ysize, yDrawSize, mBoxOrigin[1], mBoxEnds[1]);
  mBoxSize[0] = xDrawSize;
  mBoxSize[1] = yDrawSize;

  if (iisData.flags & IIS_CENTER_VOLUME)
    setViewCenter();

  if (allocArraysIfNeeded()) {
    close();
    return;
  }

  mLocalX = xCenter;
  mLocalY = yCenter;

  if(iisData.flags & IIS_LINK_XYZ){
    mVi->xmouse = mLocalX;
    mVi->ymouse = mLocalY;
    ivwBindMouse(mVi);
  }

  mSliders->setValue(IIS_X_COORD, xCenter + 1);
  mSliders->setValue(IIS_Y_COORD, yCenter + 1);
  mSliders->setValue(IIS_X_SIZE, xDrawSize);
  mSliders->setValue(IIS_Y_SIZE, yDrawSize);

  setBoundingBox();
  setBoundingObj();
  fillAndProcessVols(false);
  setIsoObj(true);
  if (!draw)
    return;

  imodvDraw(Imodv);
  if ( iisData.flags & IIS_VIEW_BOX )
    imodDraw(mVi, IMOD_DRAW_XYZ |IMOD_DRAW_SKIPMODV );
}

// Action buttons
void ImodvIsosurface::buttonPressed(int which)
{
  if (which == 2)
    imodShowHelpPage("modvIsosurface.html#TOP");
  else if (which == 3)
    imodCacheFill(mVi, 0);
  else if (which == 1)
    close();
  else if (which == 0) {
    float red, green, blue;
    Imod *userModel = ivwGetModel(mVi);

    imodNewObject(userModel);
    Iobj *userObj = imodObjectGet(userModel);
    imodObjectGetColor(userObj, &red, &green, &blue);

    Iobj *isoObj = ivwGetAnExtraObject(mVi, mExtraObjNum);
    Iobj *dup = imodObjectDup(isoObj);

    // Tone the colors down here also to prevent washout
    imodObjectSetColor(dup, 0.6 * red, 0.6 * green, 0.6 * blue);
    imodObjectCopy(dup, userObj);
    strcpy(userObj->name, "Saved isosurface");

    imodNewObject(userModel);
    Iobj *userBoxObj = imodObjectGet(userModel);

    Iobj *boxObj = ivwGetAnExtraObject(mVi, mBoxObjNum);
    Iobj *dupBox = imodObjectDup(boxObj);
    imodObjectCopy(dupBox, userBoxObj);
    strcpy(userBoxObj->name, "Saved bounding box");
    Icont *firstCont = imodObjectGetContour(userBoxObj, 0);
    firstCont->flags &= ~ICONT_DRAW_ALLZ;    
    imodDraw(mVi, IMOD_DRAW_MOD);
    imodvObjedNewView();
    free(dup);
    free(dupBox);
  } 
}

bool ImodvIsosurface::fillPaintVol()
{
  Iobj *obj = NULL;
  std::vector<IsoColor> colors;
  std::vector<IsoPaintPoint> paintPts;
  std::vector<int> cluster, excludeList, listIndex, numExclude, excludeFlags; 
  DrawProps objProps, contProps, ptProps;
  IsoPaintPoint paint;
  IsoColor color;
  Icont *cont;
  Ipoint *pts;
  IsoPaintPoint *paintp, *paintp2;
  IsoColor *colorp, *ocolorp;
  bool retval;
  int co, stateFlags, surfState, nextChange, pt, pt2, red, green, blue, i, j, indClust;
  int numClust, ixst, ixnd, iyst, iynd, ix, iy, iz, yzbase, minInd, changeFlags, found;
  int izst, iznd;
  float drawsize, dx, dy, dz, radsum, xmin, xmax, ymin, ymax, zmin, zmax, dzsq;
  float dxsq, dist, minDist, radsq;
  float zscale = Imodv->imod->zscale;
  int nx = mBoxSize[0];
  int ny = mBoxSize[1];
  int nz = mBoxSize[2];

  // Get the object if painting, amke sure it is valid
  if (iisData.flags & IIS_PAINT_OBJECT) {
    if (iisData.paintObj >= Imodv->imod->objsize)
      managePaintObject();
    obj = &Imodv->imod->obj[iisData.paintObj];
  }

  // Return if not painting or not a scattered object; clear the arrays
  if (!obj || !iobjScat(obj->flags)) {
    retval = mColorList.size() > 0 || mPaintPoints.size() > 0;
    mPaintPoints.clear();
    mColorList.clear();
    return retval;
  }

  if (imodDebug('U'))
    isoTime.start();

  istoreDefaultDrawProps(obj, &objProps);
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    istoreContSurfDrawProps(obj->store, &objProps, &contProps, co, cont->surf, 
                            &stateFlags, &surfState);
    ptProps = contProps;
    nextChange = istoreFirstChangeIndex(cont->store);
    if (contProps.gap)
      continue;
    pts = cont->pts;
    for (pt = 0; pt < cont->psize; pt++, pts++) {
      ptProps.gap = 0;
      if (nextChange == pt)
        nextChange = istoreNextChange(cont->store, &contProps, &ptProps, &stateFlags, 
                                      &changeFlags);
      drawsize = imodPointGetSize(obj, cont, pt);
      if (!drawsize || ptProps.gap)
	continue;
      if (pts->x >= mBoxOrigin[0] - drawsize && pts->x <= mBoxEnds[0] + drawsize &&
          pts->y >= mBoxOrigin[1] - drawsize && pts->y <= mBoxEnds[1] + drawsize &&
          pts->z >= mBoxOrigin[2] - drawsize && pts->z <= mBoxEnds[2] + drawsize) {

        // Get the color and look it up on the color list
        red = B3DNINT(ptProps.red * 255.);
        red = B3DMAX(0, B3DMIN(255, red));
        green = B3DNINT(ptProps.green * 255.);
        green = B3DMAX(0, B3DMIN(255, green));
        blue = B3DNINT(ptProps.blue * 255.);
        blue = B3DMAX(0, B3DMIN(255, blue));
        found = -1;
        for (i = 0; i < colors.size(); i++) {
          colorp = &colors[i];
          if (red == colorp->r && green == colorp->g && blue == colorp->b && 
              ptProps.trans == colorp->trans) {
            found = i;
            break;
          }
        }

        // If color not found, add to the list, bail out if 255 colors already
        if (found < 0) {
          if (colors.size() >= 255)
            break;
          found = colors.size();
          color.r = red;
          color.g = green;
          color.b = blue;
          color.trans = ptProps.trans;
          colors.push_back(color);
        }

        paint.x = (pts->x - mBoxOrigin[0]);
        paint.y = (pts->y - mBoxOrigin[1]);
        paint.z = (pts->z - mBoxOrigin[2]);
        paint.size = drawsize;
        paint.colorInd = found + 1;
        paint.drawn = false;
        paintPts.push_back(paint);
      }
    }
    if (colors.size() >= 255)
      break;
  }

  // If there is nothing on the lists, leave, clear lists, signal if this is a change
  if (!colors.size() || !paintPts.size()) {
    retval = mColorList.size() > 0 || mPaintPoints.size() > 0;
    mPaintPoints.clear();
    mColorList.clear();
    return retval;
  }

  //imodPrintStderr("Points %d  colors %d\n", paintPts.size(), colors.size());

  // If mask exists and sizes match what was used when it was made, need to compare
  if (mColorList.size() == colors.size() && mPaintPoints.size() == paintPts.size() && 
      mPaintSize[0] == nx && mPaintSize[1] == ny && mPaintSize[2] == nz) {
    found = 0;
    for (i = 0; i < colors.size(); i++) {
      colorp = &colors[i];
      ocolorp = &mColorList[i];
      if (colorp->r != ocolorp->r || colorp->g != ocolorp->g || colorp->b != ocolorp->b ||
          colorp->trans != ocolorp->trans) {
        found = 1;
        break;
      }
    }
    if (!found) {
      for (i = 0; i < paintPts.size(); i++) {
        paintp = &paintPts[i];
        paintp2 = &mPaintPoints[i];
        if (paintp->x != paintp2->x || paintp->y != paintp2->y || paintp->z != paintp2->z
            || paintp->size != paintp2->size || paintp->colorInd != paintp2->colorInd) {
          found = 1;
          break;
        }
      }
    }
    if (!found)
      return false;
  }
  
  // Clear the mask
  for (i = 0; i < nx * ny * nz; i++)
    mPaintVol[i] = 0;
  mPaintSize[0] = nx;
  mPaintSize[1] = ny;
  mPaintSize[2] = nz;

  // Loop on the points
  for (pt = 0; pt < paintPts.size(); pt++) {
    paintp = &paintPts[pt];
    if (paintp->drawn)
      continue;

    // Build a cluster of overlapping points 
    paintp->drawn = true;
    cluster.resize(1);
    cluster[0] = pt;
    indClust = 0;
    xmin = paintp->x - paintp->size;
    ymin = paintp->y - paintp->size;
    zmin = paintp->z - paintp->size;
    xmax = paintp->x + paintp->size;
    ymax = paintp->y + paintp->size;
    zmax = paintp->z + paintp->size;
    while (indClust < cluster.size()) {
      paintp = &paintPts[cluster[indClust]];
      for (pt2 = pt + 1; pt2 < paintPts.size(); pt2++) {
        paintp2 = &paintPts[pt2];

        // Skip points already drawn or ones with matching color
        if ((!paintp2->drawn) && (paintp->colorInd != paintp2->colorInd)) {
          dx = paintp->x - paintp2->x;
          dy = paintp->y - paintp2->y;
          dz = paintp->z - paintp2->z;
          radsum = paintp->size + paintp2->size;
          if (dx * dx + dy * dy + dz * dz < radsum * radsum) {
            cluster.push_back(pt2);
            xmin = B3DMIN(xmin, paintp2->x - paintp2->size);
            ymin = B3DMIN(ymin, paintp2->y - paintp2->size);
            zmin = B3DMIN(zmin, paintp2->z - paintp2->size);
            xmax = B3DMAX(xmax, paintp2->x + paintp2->size);
            ymax = B3DMAX(ymax, paintp2->y + paintp2->size);
            zmax = B3DMAX(zmax, paintp2->z + paintp2->size);
            paintp2->drawn = true;
          }
        }
      }
      indClust++;
    }

    // Get limits of box to be filled
    numClust = cluster.size();
    ixst = B3DMAX(0, (int)(xmin - 0.5));
    ixnd = B3DMIN(nx - 1, (int)(xmax + 0.5));
    iyst = B3DMAX(0, (int)(ymin - 0.5));
    iynd = B3DMIN(ny - 1, (int)(ymax + 0.5));
    izst = B3DMAX(0, (int)(zmin - 0.5));
    iznd = B3DMIN(nz - 1, (int)(zmax + 0.5));
    //imodPrintStderr("cluster of %d, limits X %d %d  Y %d %d  Z %d %d\n", numClust, 
    //              ixst, ixnd, iyst, iynd, izst, iznd);
    if (numClust == 1) {

      // If there are no overlapping points, fill lines
      radsq = paintp->size * paintp->size;
      for (iz = izst; iz <= iznd; iz++) {
        dz = zscale * (iz + 0.5 - paintp->z);
        dzsq = dz * dz;
        for (iy = iyst; iy <= iynd; iy++) {
          dy = iy + 0.5 - paintp->y;
          dxsq = radsq - dy * dy - dzsq;
          yzbase = iy * nx + iz * nx * ny;
          if (dxsq >= 0) {
            dx = (float)sqrt((double)dxsq);
            ixst = B3DNINT(paintp->x - 0.5 - dx);
            ixnd = B3DNINT(paintp->x - 0.5 + dx);
            ixst = B3DMAX(0, ixst);
            ixnd = B3DMIN(nx - 1, ixnd);
            for (ix = ixst; ix <= ixnd; ix++)
              mPaintVol[ix + yzbase] = paintp->colorInd;
          }
        }
      }
    } else {

      // Now sort the cluster by size
      for (i = 0; i < numClust - 1; i++) {
        for (j = i + 1; j < numClust; j++) {
          paintp = &paintPts[cluster[i]];
          paintp2 = &paintPts[cluster[j]];
          if (paintp2->size < paintp->size) {
            ix = cluster[i];
            cluster[i] = cluster[j];
            cluster[j] = ix;
          }
        }
      }

      // Make lists of larger points to exclude
      excludeList.resize(0);
      listIndex.resize(numClust);
      numExclude.resize(numClust);
      excludeFlags.resize(numClust);
      for (i = 0; i < numClust; i++) {
        listIndex[i] = excludeList.size();
        paintp = &paintPts[cluster[i]];
        for (j = i + 1; j < numClust; j++) {
          paintp2 = &paintPts[cluster[j]];
          dx = paintp->x - paintp2->x;
          dy = paintp->y - paintp2->y;
          dz = paintp->z - paintp2->z;
          dist = (float)sqrt((double)dx*dx + dy*dy + dz* dz);
          if (dist >= paintp2->size + paintp->size || dist <= paintp2->size -paintp->size)
            excludeList.push_back(j);
        }
        numExclude[i] = excludeList.size() - listIndex[i];
      }

      // Loop on the points in the volume
      for (iz = izst; iz <= iznd; iz++) {
        for (iy = iyst; iy <= iynd; iy++) {
          yzbase = iy * nx + iz * nx * ny;
          for (ix = ixst; ix <= ixnd; ix++) {
            minDist = 1.e30;
            minInd = 0;

            // Clear the exclude flags and loop on the spheres
            for (i = 0; i < numClust; i++)
              excludeFlags[i] = 0;
            for (i = 0; i < numClust; i++) {

              // Skip if its marked as include or if point is outside sphere
              if (excludeFlags[i])
                continue;
              paintp = &paintPts[cluster[i]];
              dz = zscale * (iz + 0.5 - paintp->z);
              dy = iy + 0.5 - paintp->y;
              dx = ix + 0.5 - paintp->x;
              dist = (dx * dx + dy * dy + dz * dz) / (paintp->size  * paintp->size);
              if (dist > 1.)
                continue;

              // Mark other bigger ones to exclude, keep track of color of sphere that
              // point is closest to center of relative to radius
              for (j = 0; j < numExclude[i]; j++)
                excludeFlags[excludeList[listIndex[i] + j]] = 1;
              if (dist < minDist) {
                minDist = dist;
                minInd = paintp->colorInd;
              }
            }
            if (minInd)
              mPaintVol[ix + yzbase] = minInd;
          }
        }
      }
    }
  }

  mColorList.swap(colors);
  mPaintPoints.swap(paintPts);
  if (imodDebug('U'))
    imodPrintStderr("fillPaintVol time %d\n", isoTime.elapsed());
  return true;
}

/*
 * Paint the mesh by going through the indices and adding color items to mesh store
 */
void ImodvIsosurface::paintMesh()
{
  Iobj *extraObj = ivwGetAnExtraObject(mVi, mExtraObjNum);
  Imesh *mesh = extraObj->mesh;
  int ind, i, j, li, ix, iy, iz, numThreads, numColors, thr, quantum = 10000;
  unsigned char *paintVol = mPaintVol;
  if (!mesh)
    return;
  int *list = mesh->list;
  Ipoint *vert = mesh->vert;
  Ipoint *pnt;
  Istore *allStores, *storep;
  Istore tranStore;
  IsoColor *colorp;
  int anyTrans = 0;
  int numTrans, isTrans[3];
  Ilist *threadLists[MAX_THREADS];
  int threadStart[MAX_THREADS + 1];
  int nx = mBoxSize[0];
  int ny = mBoxSize[1];
  int orix = mBoxOrigin[0];
  int oriy = mBoxOrigin[1];
  int oriz = mBoxOrigin[2];

  // Clear out the existing store if any, this unpaints if painting turned off
  ilistDelete(mesh->store);
  mesh->store = NULL;
  numColors = mColorList.size();
  if (!(iisData.flags & IIS_PAINT_OBJECT) || !numColors)
    return;

  if (imodDebug('U'))
    isoTime.start();

  numThreads = B3DMIN(mInitNThreads, 1 + mesh->lsize / 10000);
  if (imodDebug('U'))
    imodPrintStderr("paintMesh lsize %d numThreads %d  \n", mesh->lsize, numThreads);
  for (i = 0; i < numThreads; i++) {
    threadLists[i] = ilistNew(sizeof(Istore), quantum);
    if (!threadLists[i])
      return;
    ilistQuantum(threadLists[i], quantum);
    threadStart[i] = 1 + 3 * (i * (mesh->lsize - 3) / (3 * numThreads));
  }
  threadStart[numThreads] = mesh->lsize - 2;

  allStores = B3DMALLOC(Istore, 2 * numColors * numThreads);
  if (!allStores)
    return;

  // Prepare the store items with the different colors
  for (thr = 0; thr < numThreads; thr++) {
    for (i = 0; i < numColors; i++) {
      colorp = &mColorList[i];
      storep = &allStores[i + thr * numColors];
      storep->type = GEN_STORE_COLOR;
      storep->flags = GEN_STORE_BYTE << 2;
      storep->value.i = 0;
      storep->value.b[0] = colorp->r;
      storep->value.b[1] = colorp->g;
      storep->value.b[2] = colorp->b;
      if (colorp->trans) {
        anyTrans = 1;
        storep->value.b[3] = 1;
        storep = &allStores[i + (thr + numThreads) * numColors];
        storep->type = GEN_STORE_TRANS;
        storep->flags = 0;
        storep->value.i = colorp->trans;
      }
    }
  }

#pragma omp parallel for                                         \
  shared(threadStart, paintVol, nx, ny, orix, oriy, oriz, allStores, \
         threadLists, vert, list, numThreads, numColors, anyTrans)      \
  private(thr, li, pnt, ix, iy, iz, ind, i, storep, tranStore, numTrans, isTrans, j)
  for (thr = 0; thr < numThreads; thr++) {
    if (anyTrans) {
      tranStore.type = GEN_STORE_TRANS;
      tranStore.flags = 0;
      tranStore.value.i = 1;
      for (li = threadStart[thr]; li < threadStart[thr+1]; li += 3) {
        numTrans = 0;
        for (j = 0; j < 3; j++) {
          pnt = &vert[list[li+j]];
          ix = (int)(pnt->x - orix);
          iy = (int)(pnt->y - oriy);
          iz = (int)(pnt->z - oriz);
          ind = ix + iy * nx + iz * nx * ny;
          i = paintVol[ind];
          if (i > 0) {
            storep = &allStores[i - 1 + thr * numColors];
            storep->index.i = li + j;
            ilistAppend(threadLists[thr], storep);
            if (storep->value.b[3]) {
              isTrans[numTrans++] = j;
              storep = &allStores[i - 1 + (thr + numThreads) * numColors];
              storep->index.i = li + j;
              ilistAppend(threadLists[thr], storep);
            }
          }
        }

        if (numTrans % 3) {
          for (j = 0; j < 3; j++) {
            if (!(isTrans[0] == j || (numTrans == 2 && isTrans[1] == j))) {
              tranStore.index.i = li + j;
              istoreInsert(&threadLists[thr], &tranStore);
            }
          }
        }
      }
    } else {
      for (li = threadStart[thr]; li < threadStart[thr+1]; li++) {
        pnt = &vert[list[li]];
        ix = (int)(pnt->x - orix);
        iy = (int)(pnt->y - oriy);
        iz = (int)(pnt->z - oriz);
        ind = ix + iy * nx + iz * nx * ny;
        i = paintVol[ind];
        if (i > 0) {
          storep = &allStores[i - 1 + thr * numColors];
          storep->index.i = li;
          ilistAppend(threadLists[thr], storep);
        }
      }
    }
  }

  // Concatenate the lists from the threads
  if (numThreads > 1) {
    iz = 0;
    for (thr = 0; thr < numThreads; thr++)
      iz += ilistSize(threadLists[thr]);
    mesh->store = ilistNew(sizeof(Istore), iz);
    if (!mesh->store)
      return;
    mesh->store->size = iz;
    ind = 0;
    for (thr = 0; thr < numThreads; thr++) {
      i = ilistSize(threadLists[thr]);
      if (i) {
        storep = istoreItem(mesh->store, ind);
        memcpy(storep, threadLists[thr]->data, i * sizeof(Istore));
      }
      ilistDelete(threadLists[thr]);
      ind += i;
    }
    
  } else
    mesh->store = threadLists[0];
  // istoreDump(mesh->store);

  free(allStores);
  if (imodDebug('U'))
    imodPrintStderr("paintMesh time %d\n", isoTime.elapsed());
}

void ImodvIsosurface::setFontDependentWidths()
{
  diaSetButtonWidth(mUseRubber, ImodPrefs->getRoundedStyle(), 1.2, "Zap Band");
  diaSetButtonWidth(mSizeContours, ImodPrefs->getRoundedStyle(), 1.2, "Contour");
}

void ImodvIsosurface::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

// Accept a close event and set dia to null
void ImodvIsosurface::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)iisData.dia);
  iisData.dia = NULL;
  e->accept();

  ivwFreeExtraObject(mVi, mBoxObjNum);
  ivwFreeExtraObject(mVi, mExtraObjNum);
  imodvObjedNewView();
  iisData.a->drawExtraOnly = 0; //enable user model drawing;

  imodDraw(mVi, IMOD_DRAW_MOD);

  B3DFREE(mVolume);
  B3DFREE(mTrueBinVol);
  B3DFREE(mPaintVol);
  imodMeshDelete(mOrigMesh);
  if (mMadeFiltered)
    imodMeshDelete(mFilteredMesh);
  delete mSurfPieces;
}

// Close on escape; watch for the hot slider key; pass on keypress
void ImodvIsosurface::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else {
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    /*if (e->key() == Qt::Key_D) {
      doDumps = 1;
      fillAndProcessVols(false);
      } else */
    imodvKeyPress(e);
  }
}

// pass on key release; watch for hot slider release
void ImodvIsosurface::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}

void ImodvIsosurface::dumpVolume(const char *filename)
{
  unsigned char **data;
  MrcHeader hdata;
  FILE *fout = fopen(filename, "wb");
  if (!fout)
    return;
  data = (unsigned char **)malloc(mBoxSize[2] * sizeof(unsigned char *));
  if (!data)
    return;
  for (int iz = 0; iz < mBoxSize[2]; iz++)
    data[iz] = &mVolume[iz * mBoxSize[0] * mBoxSize[1]];
  mrc_head_new(&hdata,  mBoxSize[0],  mBoxSize[1],  mBoxSize[2], MRC_MODE_BYTE);
  hdata.amin = 0;
  hdata.amax = 255;
  hdata.amean = 128;
  mrc_head_write(fout, &hdata);
  mrc_write_idata(fout, &hdata, (void **)data);
  fclose(fout);
  free(data);
}


