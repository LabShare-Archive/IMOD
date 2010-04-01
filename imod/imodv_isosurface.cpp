/*
*  imodv_isosurface.c - Makes an isosurface of selected volume in extra object
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  $Id$
*  Log at end of file
*/

#include <algorithm> //std::sort
#include <vector>
#include <qcheckbox.h>
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
#include "imodv_gfx.h"
#include "imodv_isosurface.h"
#include "imodv_surfpieces.h"
#include "histwidget.h"
#include "isothread.h"
#include "xzap.h"
#include "imodv_input.h"
#include "imod_display.h"
#include "imodv_objed.h"
#include "control.h"


enum {IIS_X_COORD = 0, IIS_Y_COORD, IIS_Z_COORD, IIS_X_SIZE, IIS_Y_SIZE,
  IIS_Z_SIZE};
enum {FIND_DEFAULT_DIM=0, FIND_MAXIMAL_DIM};


static int xDrawSize = -1;
static int yDrawSize = -1;
static int zDrawSize = -1;
static int lastYsize = -1;

static QTime isoTime;

#define MAXIMAL_VOXELS 128*512*512  //the number of voxles the bounding box is limited to;
#define DEFAULT_VOXELS 96*96*96 //the number of voxles the initial bounding box;
#define PERCENTILE 0.07
#define MAXIMAL_ITERATION 20
#define MAXIMAL_BINNING 4
#define TOL 0.0001
#define NUM_THREADS 1

typedef unsigned int Index;
extern void smooth_vertex_positions(float *varray, Index nv,
    const Index *tarray, Index nt,
    float smoothing_factor, int smoothing_iterations);
struct imodvIsosurfaceDataStruct {
  ImodvIsosurface *dia;
  ImodvApp  *a;
  int itNum;
  int binningNum;
  int minTNum;

  int    flags;

  /* DNM 12/30/02: unused currently */
  /* int    xsize, ysize, zsize;
     int    *xd, *yd, *zd; */

}imodvIsosurfaceData = {0, 0, 0, 1, 100,  ~(0|IMODV_DELETE_PIECES) };

static bool isBoxChanged(const int *start, const int *end);

// Open, close, or raise the dialog box
void imodvIsosurfaceEditDialog(ImodvApp *a, int state)
{
  if (!state){
    if (imodvIsosurfaceData.dia)
      imodvIsosurfaceData.dia->close();
    return;
  }
  if (imodvIsosurfaceData.dia) {
    imodvIsosurfaceData.dia->raise();
    return;
  }

  imodvIsosurfaceData.a = a;
  imodvIsosurfaceData.dia = new ImodvIsosurface
    (a->vi, imodvDialogManager.parent(IMODV_DIALOG), "isosurface view");
  imodvDialogManager.add((QWidget *)imodvIsosurfaceData.dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)imodvIsosurfaceData.dia, IMODV_DIALOG);
}

bool imodvIsosurfaceUpdate(void)
{
  if(imodvIsosurfaceData.dia )
  { 
    ImodvIsosurface *dia=imodvIsosurfaceData.dia;
    if (dia->mLinkXYZ->isChecked()) 
      dia->updateCoords();
    if (isBoxChanged(dia->mBoxOrigin, dia->mBoxEnds)) {
      dia->setBoundingBox();
      if (dia->mCenterVolume->isChecked())
        dia->setViewCenter();
      dia->setBoundingObj();
      int stackIdx=dia->getCurrStackIdx();
      dia->mCurrStackIdx=stackIdx;
      if (dia->mStackThresholds[stackIdx]<0) { //first time, set it;
        dia->mStackThresholds[stackIdx]=dia->fillVolumeArray();
        dia->mThreshold=dia->mStackThresholds[stackIdx];
      } else {//use threshold for the current stack;
        dia->fillVolumeArray();
        dia->mThreshold=dia->mStackThresholds[stackIdx];
      }
      dia-> mHistSlider->setValue(0,(int)dia->mThreshold);
      imodvIsosurfaceData.dia->fillBinVolume();
      imodvIsosurfaceData.dia->setIsoObj();
      //imodvDraw(Imodv);
      return true;
    }    
    return false;

  } else
    return false;
}

// Compute starting and ending draw coordinates from center coordinate, desired
// size and maximum size, shifting center if necessary
static void setCoordLimits(int cur, int maxSize, int drawSize, 
    int *str, int *end)
{
  *str = cur - drawSize / 2;
  if (*str < 0)
    *str = 0;
  *end = *str + drawSize;
  if (*end > maxSize) {
    *end = maxSize;
    *str = *end - drawSize;
  }
}

static bool isBoxChanged(const int *start, const int *end)
{
  int newStart, newEnd, currTime;

  ivwGetTime(Imodv->vi, &currTime);
  if(imodvIsosurfaceData.dia->mCurrTime!=currTime) {
    imodvIsosurfaceData.dia->mCurrTime=currTime;
    return true;
  }
  setCoordLimits(imodvIsosurfaceData.dia->mLocalX, Imodv->vi->xsize, xDrawSize,
                 &newStart, &newEnd);
  if (*start!=newStart || *end!=newEnd) 
    return true;
  setCoordLimits(imodvIsosurfaceData.dia->mLocalY, Imodv->vi->ysize, yDrawSize,
                 &newStart, &newEnd);
  if (*(start+1)!=newStart || *(end+1)!=newEnd)
    return true;
  setCoordLimits(imodvIsosurfaceData.dia->mLocalZ, Imodv->vi->zsize, zDrawSize,
                 &newStart, &newEnd);
  if (*(start+2)!=newStart || *(end+2)!=newEnd) 
    return true;
  return false;
}

static void findDimLimits(int which, int *xdim, int *ydim, int *zdim, int
    *sizes)
{  
  int specMinDim;
  int tempMin;
  int voxelMax;

  /* *xdim=512;
  *ydim=85;
  *zdim=512;
  return;*/ 

  if (which==FIND_DEFAULT_DIM) {
    specMinDim=powf(DEFAULT_VOXELS, 1.0/3.0);
    voxelMax=DEFAULT_VOXELS;
  } else if (which==FIND_MAXIMAL_DIM) {
    specMinDim=powf(MAXIMAL_VOXELS, 1.0/3.0);
    voxelMax=MAXIMAL_VOXELS;
  } else {
    imodPrintStderr("imodv_isosurface: illegal usage\n");
    return;
  }

  if (sizes[0]<=sizes[1] && sizes[0]<=sizes[2]) { 
    *xdim=B3DMIN(specMinDim, sizes[0]);
    tempMin=(int)sqrt((double)( voxelMax/(*xdim)) );
    *ydim=B3DMIN(tempMin, sizes[1]);
    *zdim=B3DMIN(tempMin, sizes[2]);
  } else if(sizes[1]<=sizes[0] && sizes[1]<=sizes[2]) {
    *ydim=B3DMIN(specMinDim, sizes[1]);
    tempMin=(int)sqrt((double)(voxelMax/(*ydim)) );
    *xdim=B3DMIN(tempMin, sizes[0]);
    *zdim=B3DMIN(tempMin, sizes[2]);
  } else if(sizes[2]<=sizes[0] && sizes[2]<=sizes[1]) {
    *zdim=B3DMIN(specMinDim, sizes[2]);
    tempMin=(int)sqrt((double)(voxelMax/(*zdim)));
    *xdim=B3DMIN(tempMin, sizes[0]);
    *ydim=B3DMIN(tempMin, sizes[1]);
  }
}

// THE ImodvIsosurface CLASS IMPLEMENTATION

static char *buttonLabels[] = {"Save", "Done", "Help"};
static char *buttonTips[] = {"Save isosurfaces and bounding box as Imod objects", "Close dialog box", "Open help window"};
static char *sliderLabels[] = {"X", "Y", "Z", "X size", "Y size", "Z size"};
static char *histLabels[]={"Threshold:"};

ImodvIsosurface::ImodvIsosurface(struct ViewInfo *vi, QWidget *parent, const char *name)
  : DialogFrame(parent, 3, 1, buttonLabels, buttonTips, true, 
      ImodPrefs->getRoundedStyle(), "3dmodv Isosurface View", "", name)
{
  mCtrlPressed = false;
  mIsoView = vi;
  //reserve a number;
  mExtraObjNum=ivwGetFreeExtraObjectNumber(vi);
  mBoxObjNum=ivwGetFreeExtraObjectNumber(vi);
  ivwGetTime(vi, &mCurrTime);
  mVolume=NULL;
  mSurfPieces=NULL;
  mOrigMesh=NULL;
  mFilteredMesh=NULL;

  // Make view checkboxes
  mViewIso = diaCheckBox("View isosurfaces", this, mLayout);
  mViewIso->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_ISOSURFACE);
  connect(mViewIso, SIGNAL(toggled(bool)), this, SLOT(viewIsoToggled(bool)));
  mViewModel = diaCheckBox("View user model", this, mLayout);
  mViewModel->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_USER_MODEL);
  connect(mViewModel, SIGNAL(toggled(bool)), this, SLOT(viewModelToggled(bool)));
  mViewBoxing = diaCheckBox("View bounding box", this, mLayout);
  mViewBoxing->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_BOX);
  connect(mViewBoxing, SIGNAL(toggled(bool)), this, SLOT(viewBoxingToggled(bool)));
  mCenterVolume=diaCheckBox("Keep box centered", this, mLayout);
  mCenterVolume->setChecked(imodvIsosurfaceData.flags & IMODV_CENTER_VOLUME);
  connect(mCenterVolume,
      SIGNAL(toggled(bool)),this,SLOT(centerVolumeToggled(bool)));
  mViewIso->setToolTip("Display isosurfaces");
  mViewModel->setToolTip("Display user model");
  mViewBoxing->setToolTip("Display the bounding box");
  mCenterVolume->setToolTip("Keep isosurfaces centered in the model view window");

  QHBoxLayout *binningLayout=new QHBoxLayout;
  mBinningBox = (QSpinBox *)diaLabeledSpin(0, 1., (float)MAXIMAL_BINNING, 1.,
                                           "Binning:", this, binningLayout);
  mBinningBox->setValue(imodvIsosurfaceData.binningNum);
  mLayout->addLayout(binningLayout);
  connect(mBinningBox, SIGNAL(valueChanged(int)), this,
      SLOT(binningNumChanged(int)));
  mBinningBox->setToolTip("Set the binning level");

  QHBoxLayout *smoothLayout=new QHBoxLayout;
  mSmoothBox=(QSpinBox *)diaLabeledSpin(0, 0., (float)MAXIMAL_ITERATION, 1.,
                                        "Smoothing:", this, smoothLayout);
  mSmoothBox->setValue(imodvIsosurfaceData.itNum);
  mLayout->addLayout(smoothLayout);
  connect(mSmoothBox, SIGNAL(valueChanged(int)), this, SLOT(iterNumChanged(int)));
  mSmoothBox->setToolTip("Set the iteration number for smoothing");

  mDeletePieces=diaCheckBox("Delete small pieces", this, mLayout);
  mDeletePieces->setChecked(imodvIsosurfaceData.flags & IMODV_DELETE_PIECES);
  connect(mDeletePieces, SIGNAL(toggled(bool)), this, SLOT(
        deletePiecesToggled(bool)));
  mDeletePieces->setToolTip("Remove small isosurface pieces");
  QHBoxLayout *piecesLayout=new QHBoxLayout;
  mPiecesBox=(QSpinBox *)diaLabeledSpin(0, 10., 9999., 10., "min size:",
                                        this, piecesLayout);
  mPiecesBox->setValue(imodvIsosurfaceData.minTNum);
  mLayout->addLayout(piecesLayout);
  connect(mPiecesBox, SIGNAL(valueChanged(int)), this,
      SLOT(numOfTrianglesChanged(int)) );
  mPiecesBox->setToolTip(
      "Set the # of triangles the smallest piece must have");

  mHistPanel=new HistWidget(this);
  mHistPanel->setMinimumSize(size().width(), 80);
  mHistSlider=new MultiSlider(this, 1, histLabels);
  connect(mHistSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(histChanged(int, int, bool)));

  mLayout->addWidget(mHistPanel);
  mLayout->setStretchFactor(mHistPanel, 100);
  mLayout->addLayout(mHistSlider->getLayout());

  mLinkXYZ=diaCheckBox("Link to global X, Y, Z", this, mLayout);
  mLinkXYZ->setChecked(imodvIsosurfaceData.flags & IMODV_LINK_XYZ);
  connect(mLinkXYZ, SIGNAL(toggled(bool)), this, SLOT(
        linkXYZToggled(bool)));
  mLinkXYZ->setToolTip("Link global XYZ and  the XYZ of isosurface center");

  // Make multisliders
  mSliders = new MultiSlider(this, 6, sliderLabels);
  //temporarily store input stack size in mBoxSize for calling findDimLimits();
  mBoxSize[0]=vi->xsize;
  mBoxSize[1]=vi->ysize;
  mBoxSize[2]=vi->zsize;
  findDimLimits(FIND_DEFAULT_DIM, &xDrawSize, &yDrawSize, &zDrawSize, mBoxSize);
  int xMax, yMax, zMax;
  findDimLimits(FIND_MAXIMAL_DIM, &xMax, &yMax, &zMax, mBoxSize);

  mSliders->setRange(IIS_X_COORD, 1, vi->xsize);
  mSliders->setRange(IIS_X_SIZE, 1, xMax );
  mSliders->setRange(IIS_Y_SIZE, 1, yMax );
  mSliders->setRange(IIS_Z_SIZE, 1, zMax );
  if (lastYsize < 0) {
    lastYsize = Imodv->vi->ysize;
  }

  bool flagDrawXYZ=false;
  if (Imodv->vi->xmouse==0.0 &&  Imodv->vi->ymouse==0.0) { 
    Imodv->vi->xmouse=Imodv->vi->xsize/2.0;
    Imodv->vi->ymouse=Imodv->vi->ysize/2.0;
    flagDrawXYZ=true;
  }

  updateCoords();
  mSliders->setValue(IIS_X_SIZE, xDrawSize);
  mSliders->setValue(IIS_Y_SIZE, yDrawSize);
  mSliders->setValue(IIS_Z_SIZE, zDrawSize);
  mLayout->addLayout(mSliders->getLayout());
  (mSliders->getLayout())->setSpacing(4);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(sliderMoved(int, int, bool)));
  mSliders->getSlider(IIS_X_COORD)->setToolTip(
      "Set X coordinate of the box center");
  mSliders->getSlider(IIS_Y_COORD)->setToolTip(
      "Set Y coordinate of the box center");
  mSliders->getSlider(IIS_Z_COORD)->setToolTip(
      "Set Z coordinate of the box center");
  mSliders->getSlider(IIS_X_SIZE)->setToolTip(
      "Set bounding box size in X");
  mSliders->getSlider(IIS_Y_SIZE)->setToolTip(
      "Set bounding box size in Y");
  mSliders->getSlider(IIS_Z_SIZE)->setToolTip(
      "Set bounding box size in Z");

  mUseRubber=diaPushButton("Use Rubber Band", this, mLayout);
  mUseRubber->setToolTip("Show isosurfaces of area enclosed by the rubber band");
  connect(mUseRubber, SIGNAL(clicked()), this, SLOT(showRubberBandArea()));
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  mInitNThreads=QThread::idealThreadCount();
  char *procChar = getenv("IMOD_PROCESSORS");
  if (procChar) {
    mInitNThreads = atoi(procChar);
    mInitNThreads = B3DMIN(MAX_THREADS, B3DMAX(1, mInitNThreads));
  }

  for (int i=0; i<mInitNThreads; i++) 
    threads[i]=new IsoThread(i, this);

  setBoundingBox();
  if (mCenterVolume->isChecked()) 
    setViewCenter();
  setBoundingObj();

  mCurrMax=mBoxSize[0]*mBoxSize[1]*mBoxSize[2];
  mVolume=(unsigned char*) (malloc( mCurrMax*sizeof(unsigned char) ));
  mBinVolume=(unsigned char*) (malloc( mCurrMax*sizeof(unsigned char) ));

  mThreshold=fillVolumeArray();
  //init mStackThreads
  for (int i=0; i<mIsoView->nt+1; i++) {
    mStackThresholds.push_back(-1.0);
  }
  mCurrStackIdx=getCurrStackIdx();
  mStackThresholds[mCurrStackIdx]=mThreshold;
  fillBinVolume();

  //mThreshold=198.0;

  mHistSlider->setValue(0,(int)mThreshold);

  Iobj *xobj = ivwGetAnExtraObject(mIsoView, mExtraObjNum);
  xobj->flags |= IMOD_OBJFLAG_EXTRA_EDIT;
  if (mViewIso->isChecked() ) {
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

  Iobj *boxObj=ivwGetAnExtraObject(mIsoView, mBoxObjNum);
  boxObj->flags |= IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_EXTRA_EDIT; 
  if (mViewBoxing->isChecked()) { 
    boxObj->flags &= ~IMOD_OBJFLAG_OFF;
  } else {
    boxObj->flags |= IMOD_OBJFLAG_OFF;
  }
  strcpy(boxObj->name, "Bounding box of isosurface");

  imodvObjedNewView();

  if (mViewModel->isChecked()) {
    imodvIsosurfaceData.a->drawExtraOnly=0;
  } else {
    imodvIsosurfaceData.a->drawExtraOnly=1;
  }

  setIsoObj();
  //mHistSlider->setValue(0,mThreshold);

  if(flagDrawXYZ || mViewBoxing->isChecked() )
    imodDraw(Imodv->vi, IMOD_DRAW_XYZ|IMOD_DRAW_MOD);
  else imodvDraw(Imodv);

  if(imodDebug('U') ){
    imodPrintStderr("In constructor: rendering time=%d \n", isoTime.elapsed() );
    isoTime.start();
  }
}

ImodvIsosurface::~ImodvIsosurface()
{
  free(mVolume);
  if(mOrigMesh) 
    imodMeshDelete(mOrigMesh);
}


/*!
 * Returns the maximum time index, the current time is returned in [outTime].
 * When there is only a single image file, the maximum index and time index 
 * are both 0; when there are multiple times the index is numbered from 1.
 */
int ImodvIsosurface::getCurrStackIdx()
{
  if(mIsoView->nt == 0)
    return 0;
  else
    return mIsoView->ct - 1;
}

void ImodvIsosurface::fillBinVolume()
{
  int binNum=mBinningBox->text().toInt();
  float denom=binNum*binNum*binNum;
  float value;

  for (int zi =0; zi<mBinBoxSize[2]; zi++)
    for (int yi=0; yi<mBinBoxSize[1]; yi++)
      for (int xi=0; xi<mBinBoxSize[0]; xi++) {

         value=0.0;
         for (int zii=0; zii<binNum; zii++)
           for (int yii=0; yii<binNum; yii++)
             for (int xii=0; xii<binNum; xii++) {
               value+=mVolume[(zi*binNum+zii)*mBoxSize[0]*mBoxSize[1]
                              + (yi*binNum+yii)*mBoxSize[0]
                              +  xi*binNum+xii ];
              }
         mBinVolume[zi*mBinBoxSize[0]*mBinBoxSize[1] + yi*mBinBoxSize[0] + xi]
           = value/denom;
      }
}

float ImodvIsosurface::fillVolumeArray()
{
  unsigned char **imdata;
  int tempCacheSum;
  /* Set up image pointer tables */
  int vmnullvalue = (mIsoView->white + mIsoView->black) / 2;
  if ( ivwSetupFastAccess(mIsoView, &imdata, vmnullvalue, &tempCacheSum) )
    return -1;

  int i;
  unsigned char value;
  //mVolume is a 3d array of C type, i.e., row-major;
  //stride should be set to ( mBoxSize[1]*mBoxSize[2], mBoxSize[2], 1 );
  /*for( int xi=mBoxOrigin[0];xi<mBoxEnds[0];xi++)
    for( int yi=mBoxOrigin[1];yi<mBoxEnds[1];yi++)
    for( int zi =mBoxOrigin[2];zi<mBoxEnds[2];zi++)
    {
    value=(*ivwFastGetValue)(xi, yi, zi);
    *( mVolume
    + (xi-mBoxOrigin[0])*mBoxSize[1]*mBoxSize[2]
    + (yi-mBoxOrigin[1])*mBoxSize[2]
    + zi-mBoxOrigin[2] ) = value;
    meanValue+=value;
    hist[value]+=1.0;
    }*/

  //mVolume is a 3d array of Fortune type, i.e., column-major;
  //stride should be set to {1, mBoxSize[0], mBoxSize[0]*mBoxSize[1]};
  int min=255;
  int max=0;
  float *hist=mHistPanel->getHist();
  for(i=0;i<256;i++) hist[i]=0.0;    // DNM removed int declaration

  for (int zi =mBoxOrigin[2];zi<mBoxEnds[2];zi++)
    for (int yi=mBoxOrigin[1];yi<mBoxEnds[1];yi++)
      for (int xi=mBoxOrigin[0];xi<mBoxEnds[0];xi++)
      {
        value=(*ivwFastGetValue)(xi, yi, zi);
        mVolume[(zi-mBoxOrigin[2])*mBoxSize[0]*mBoxSize[1]
                + (yi-mBoxOrigin[1])*mBoxSize[0]
                + xi-mBoxOrigin[0]] = value;
        hist[value]+=1.0;
        if(value<min) min=value;
        if(value>max) max=value;
      }

  if ((max-min)<10 ) {
    min=B3DMIN(0, min-5);
    max=B3DMAX(255, max+5);
  }
  mHistSlider->setRange(0, min, max);
  mHistPanel->setMinMax(min, max);

  for (i=0; i<256; i++) 
    hist[i]=hist[i]/(mBoxSize[0]*mBoxSize[1]*mBoxSize[2]);
  mHistPanel->setHistMinMax();

  mHistPanel->update();
  return mHistPanel->computePercentile(PERCENTILE);
}


void ImodvIsosurface::setIsoObj()
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
  int validVertexSum=0;
  bool flagFind;

  // Added for multi-processing with binning;
  int cOrigZ[MAX_THREADS];
  float newThreshold;
  int binNum=mBinningBox->text().toInt();
  cOrigZ[0]=mSubZEnds[0];
  for (i=1; i<mNThreads; i++)
    cOrigZ[i]=mSubZEnds[0]+binNum*(mSubZEnds[i]-mSubZEnds[0])-binNum;


  offset[0]=0; //will always be 0; so are skipNVertex[0] and skipNTriangle[0];

  //free all memory used by the old mesh; 
  ivwClearAnExtraObject(mIsoView, mExtraObjNum);
  Iobj *extraObj=ivwGetAnExtraObject(mIsoView, mExtraObjNum);

  if(imodDebug('U') ){
    imodPrintStderr("Using %d threads\n", mNThreads);
    isoTime.start();
  }
  for(i=0; i<mNThreads; i++){
    threads[i]->start();
  }
  for(i=0; i<mNThreads; i++){
    threads[i]->wait();
  }

  if(imodDebug('U') ){
    imodPrintStderr("mcube time=%d \n", isoTime.elapsed() );
    isoTime.start();
  } 
  for(i=0; i<mNThreads; i++){

    nVertex[i]=threads[i]->getNVertex();
    nTriangle[i]=threads[i]->getNTriangle();
    vertex_xyz[i]=threads[i]->getVertex_xyz();
    triangle[i]=threads[i]->getTriangle();

    skipNVertex[i]=0;
    validNVertex[i]=nVertex[i];
    skipNTriangle[i]=0;
    validNTriangle[i]=nTriangle[i];
    vertStart[0] = 0;
    triStart[0] = 1;
    if (i>0){// starting from the second subslice
      if(imodDebug('U') ) imodPrintStderr("\n *************loop=%d\n", i);

      flagFind=false;

      for(ii=0;ii<2*nVertex[i];ii+=2){
        if (vertex_xyz[i][ii].z>cOrigZ[i]+binNum+binNum-TOL ) 
        //if (vertex_xyz[i][ii].z>mSubZEnds[i]+1.0-TOL ) //old code for without binning; 
          break;
      }
      skipNVertex[i]=ii/2;

      /* Remove redundant vertices in the vertex list of previous subslices.
         Searching for the vertex that has the same coordinates as the last 
         skipped vertex of the current subslices.
         */
      jj=0;
      if (skipNVertex[i]) {
        for(j=2*validNVertex[i-1]-2;j>=0;j-=2)
          if ( fabs(vertex_xyz[i-1][j].x - vertex_xyz[i][ii-2].x)>TOL ||
               fabs(vertex_xyz[i-1][j].y - vertex_xyz[i][ii-2].y)>TOL ||
               fabs(vertex_xyz[i-1][j].z - vertex_xyz[i][ii-2].z)>TOL )
            jj++;
          else 
            break;
      }
      validNVertex[i-1]-=jj+skipNVertex[i-1];

      if(imodDebug('U') )
        imodPrintStderr("skipNVertex[%d]=%d remove %d redundant vertices  %d\n"
                        , i-1, skipNVertex[i-1], jj, isoTime.elapsed());

      validVertexSum+=validNVertex[i-1];
      offset[i]=validVertexSum-skipNVertex[i];

      k=-1;
      int lowz, highz, minz, maxz;
      lowz = -1;
      highz = -1;

      // Find the first triangle in the current subslice with at least one 
      // vertex above the dividing Z value
      for (ii=0;ii<nTriangle[i];ii++) {  
        newThreshold=cOrigZ[i]+binNum +TOL;

        // Here is code to detect whether polygons are ever out of order
        /* minz = floor((B3DMIN(vertex_xyz[i][ 2*triangle[i][3*ii]].z, 
                      B3DMIN(vertex_xyz[i][ 2*triangle[i][3*ii+1]].z,
                             vertex_xyz[i][ 2*triangle[i][3*ii+2]].z)))/binNum);
        maxz = ceil((B3DMAX(vertex_xyz[i][ 2*triangle[i][3*ii]].z,
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
          k=ii; //skipNTriangle[i]++;
      }

      // k is left pointing to the last triangle before this criterion is met
      if(k>-1) 
        skipNTriangle[i]=k+1;
      /*if(imodDebug('U') )
        imodPrintStderr("Got skipNTriangle at %d \n", isoTime.elapsed()); */

      /* Remove redundant triangles in the triangle list of previous subslices.
         Searching for the triangle that indexes into the same vertices as the
         last skipped triangle of the current subslice.
         */
      jj=0;
      if (skipNTriangle[i]) {
        k = 3*(skipNTriangle[i]-1);
        lastVert = vertex_xyz[i-1];
        lastTri = triangle[i-1];
        if (i == 1)
          lastTri += 1;
        curVert = vertex_xyz[i];
        curTri = triangle[i];
        for (j=nTriangle[i-1]-1; j>=0; j--)
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
            flagFind=true; 
            break;
          } else
            jj++;
      }
      if(!flagFind) jj=0;
      validNTriangle[i-1]-=jj+skipNTriangle[i-1];
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
    for(i=0;i<mNThreads;i++){
    imodPrintStderr("mSubZEnds[%d]=%d\n", i, mSubZEnds[i]);
    imodPrintStderr("nVertex[%d]=%d skipNVertex[%d]=%d, validNVertex[%d]=%d \n",
    i, nVertex[i], i, skipNVertex[i], i, validNVertex[i] );
    imodPrintStderr("nTriangle[%d]=%d, skipNTriangle[%d]=%d, validNTriangle[%d]=%d\n",
    i, nTriangle[i], i, skipNTriangle[i], i, validNTriangle[i]);

    sprintf(fnVertex, "%d%s", i, "_vertex.txt");
    fpV=fopen(fnVertex,"w");
    for(ii=0;ii<2*nVertex[i];ii+=2){
    fprintf(fpV, "vertex[%d] x y z= %f \t %f \t %f \n", ii/2, vertex_xyz[i][ii].x,
    vertex_xyz[i][ii].y,  vertex_xyz[i][ii].z);
    }
    fclose(fpV);

    sprintf(fnTriangle, "%d%s", i, "_triangle.txt");
    fpT=fopen(fnTriangle,"w");
    for(ii=0;ii<nTriangle[i];ii++){
    fprintf(fpT, "triange[%d] vertex 1 2 3= %d \t %d \t %d \n", 
    ii,triangle[i][3*ii] ,triangle[i][3*ii+1], triangle[i][3*ii+2] );
    }
    fclose(fpT);
    }*/

  //merge vertex and triangle lists after setting numbers for the last subslice
  validNVertex[mNThreads-1] = nVertex[mNThreads-1] - skipNVertex[mNThreads-1];
  validNTriangle[mNThreads-1] = nTriangle[mNThreads-1] - 
    skipNTriangle[mNThreads-1];
  int totalVertex=0;
  int totalTriangle=0;
  for(i=0; i<mNThreads; i++) {
    totalTriangle += validNTriangle[i];
    totalVertex += validNVertex[i];
  }

  if (imodDebug('U') )
    imodPrintStderr("***totalVertex=%d totalTriangle=%d   %d\n", totalVertex,
                    totalTriangle, isoTime.elapsed());

  if(mNThreads>1){
    finalVertex_xyz=(Ipoint *)malloc(2*totalVertex*sizeof(Ipoint));
    finalTriangle=(b3dInt32*)malloc( (3*totalTriangle+3)*sizeof(b3dInt32) );

#pragma omp parallel for \
  shared(validNTriangle, finalTriangle, triStart, triangle, validNVertex, \
         vertStart, vertex_xyz, skipNVertex, skipNTriangle, offset)     \
  private(i, ii)
    for (i=0; i<mNThreads; i++) {
      for (ii=0; ii<2*validNVertex[i]; ii++)
        finalVertex_xyz[vertStart[i]+ii] = vertex_xyz[i][ii+2*skipNVertex[i]];

      if(i==0) {

        // Indexes in first chunk are offset by 1
        for (ii=0; ii<3*validNTriangle[i]; ii++)
          finalTriangle[triStart[i]+ii] = 2*(triangle[i][ii+1]);
      } else {
        for (ii=0; ii<3*validNTriangle[i]; ii++)
          finalTriangle[triStart[i]+ii] = 2*(triangle[i][ii+3*skipNTriangle[i]]
                                             + offset[i]);
      }
    }

    for(i=0;i<mNThreads;i++){
      free(vertex_xyz[i]);
      free(triangle[i]);
    }
  } else {
    
    // For one thread, still need to multiply indices by 2
    finalVertex_xyz=vertex_xyz[0];
    finalTriangle=triangle[0];
    for(i=0;i<3*totalTriangle;i++)
      finalTriangle[1+i]*=2;
  }
    
  /*if(imodDebug('U') )
    imodPrintStderr("indexes copied at %d \n", isoTime.elapsed()); */
  finalTriangle[0]=-25;
  finalTriangle[3*totalTriangle+1]=-22;
  finalTriangle[3*totalTriangle+2]=-1;
  
  //create a Imesh structure; 
  mcubeMesh=imodMeshNew();
  mcubeMesh->vert=finalVertex_xyz;
  mcubeMesh->lsize= 3*totalTriangle+3;
  mcubeMesh->vsize= 2*totalVertex;
  mcubeMesh->list=finalTriangle;

  if(imodDebug('U') ){
    imodPrintStderr("merge time=%d \n", isoTime.elapsed());
    isoTime.start();
  }

  if(mOrigMesh) imodMeshDelete(mOrigMesh);
  mOrigMesh=mcubeMesh;

  //filter out small pieces;
  filterMesh( mDeletePieces->isChecked() );
  
  //need to create a dup and put in the extraObject;
  struct Mod_Mesh *dup=imodMeshDup(mFilteredMesh);

  //apply smoothing;
  int itrNum=mSmoothBox->text().toInt();
  if(itrNum) smoothMesh(dup, itrNum);
  //attch the new mesh to the extra obj;
  imodObjectAddMesh(extraObj, dup);
  free(dup);

  /*for(i=0;i<mNThreads;i++){
    imodPrintStderr("mSubZEnds[%d]=%d\n", i, mSubZEnds[i]);
    imodPrintStderr("nVertex[%d]=%d skipNVertex[%d]=%d, validNVertex[%d]=%d \n",
    i, nVertex[i], i, skipNVertex[i], i, validNVertex[i] );
    imodPrintStderr("nTriangle[%d]=%d, skipNTriangle[%d]=%d, validNTriangle[%d]=%d\n",
    i, nTriangle[i], i, skipNTriangle[i], i, validNTriangle[i]);
    }
    imodPrintStderr("Threshold=%4.1f, x=%4.1f, y=%4.1f, z=%4.1f xSize=%d ySize=%d zSize=%d\n",
    mThreshold, Imodv->vi->xmouse, Imodv->vi->ymouse, Imodv->vi->zmouse,
    mBoxSize[0], mBoxSize[1], mBoxSize[2]);
    imodPrintStderr("\n");*/

  /*FILE *fp=fopen("combinedVertex.txt","w");
    for(ii=0;ii<2*totalVertex;ii+=2){
    fprintf(fp, "vertex[%d] x y z= %f \t %f \t %f \n", ii/2, finalVertex_xyz[ii].x,
    finalVertex_xyz[ii].y,  finalVertex_xyz[ii].z);
    }
    fclose(fp);

    fp=fopen("combinedTriangle.txt","w");
    for(ii=0;ii<totalTriangle;ii++){
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
  setCoordLimits(mLocalX, Imodv->vi->xsize, xDrawSize, mBoxOrigin, mBoxEnds);
  setCoordLimits(mLocalY, Imodv->vi->ysize, yDrawSize, mBoxOrigin+1, mBoxEnds+1);
  setCoordLimits(mLocalZ, Imodv->vi->zsize, zDrawSize, mBoxOrigin+2, mBoxEnds+2);
  mBoxSize[0]=xDrawSize;
  mBoxSize[1]=yDrawSize;
  mBoxSize[2]=zDrawSize;
  
  //set binning parameters
  int binNum=mBinningBox->text().toInt();
  int i;
  for(i=0;i<3;i++) {
    mBinBoxSize[i]=mBoxSize[i]/binNum;
    mBinBoxEnds[i]=mBoxOrigin[i]+mBinBoxSize[i];
  }
  
  int fairShare=mBinBoxSize[2]/mInitNThreads;

  if(fairShare<8)
  { 
    mNThreads=1;
    fairShare=mBinBoxSize[2];
  }else mNThreads=mInitNThreads;

  mSubZEnds[0]=mBoxOrigin[2];
  for(i=1;i<mNThreads;i++)
    mSubZEnds[i]=mSubZEnds[i-1]+fairShare;
  mSubZEnds[mNThreads]=mBinBoxEnds[2]-1;

  if (imodDebug('U') ) 
    for(i=0;i<mNThreads+1;i++)
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
  Icont *contours=imodContoursNew(4);
  Ipoint corners[8];

  corners[0].x=mBoxOrigin[0];     corners[0].y=mBoxOrigin[1];    corners[0].z=mBoxOrigin[2];
  corners[1].x=mBoxEnds[0];       corners[1].y=mBoxOrigin[1];    corners[1].z=mBoxOrigin[2];
  corners[2].x=mBoxEnds[0];       corners[2].y=mBoxEnds[1];      corners[2].z=mBoxOrigin[2];
  corners[3].x=mBoxOrigin[0];     corners[3].y=mBoxEnds[1];     corners[3].z=mBoxOrigin[2];

  corners[4].x=mBoxOrigin[0];     corners[4].y=mBoxOrigin[1];    corners[4].z=mBoxEnds[2];
  corners[5].x=mBoxEnds[0];       corners[5].y=mBoxOrigin[1];    corners[5].z=mBoxEnds[2];
  corners[6].x=mBoxEnds[0];       corners[6].y=mBoxEnds[1];      corners[6].z=mBoxEnds[2];
  corners[7].x=mBoxOrigin[0];     corners[7].y=mBoxEnds[1];      corners[7].z=mBoxEnds[2];

  for(i=0;i<4;i++){
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

  contours[0].flags |=ICONT_DRAW_ALLZ;
  ivwClearAnExtraObject(Imodv->vi, mBoxObjNum);
  Iobj *bObj=ivwGetAnExtraObject(Imodv->vi, mBoxObjNum);
  imodObjectSetColor(bObj, 1.0, 1.0, 0.0);
  for(i=0;i<4;i++) {
    imodObjectAddContour(bObj, &contours[i]);
  }
  free(contours);
}

// Update the current coordinate sliders and their ranges, update the ranges
// of the Y and Z size sliders and swap y and Z size if flipped
void ImodvIsosurface::updateCoords()
{
  mSliders->setValue(IIS_X_COORD, (int)(Imodv->vi->xmouse + 1.5f));
  mSliders->setRange(IIS_Y_COORD, 1, Imodv->vi->ysize);
  mSliders->setValue(IIS_Y_COORD, (int)(Imodv->vi->ymouse + 1.5f));
  mSliders->setRange(IIS_Z_COORD, 1, Imodv->vi->zsize);
  mSliders->setValue(IIS_Z_COORD, (int)(Imodv->vi->zmouse + 1.5f));

  mLocalX=(int)Imodv->vi->xmouse;
  mLocalY=(int)Imodv->vi->ymouse;
  mLocalZ=(int)Imodv->vi->zmouse;

  if (lastYsize != Imodv->vi->ysize) {
    int tmpSize = yDrawSize;
    yDrawSize = zDrawSize;
    mBoxSize[1]=yDrawSize;
    zDrawSize = tmpSize;
    mBoxSize[2]=zDrawSize;

    int xMax, yMax, zMax;
    int stackSize[3];
    stackSize[0]=Imodv->vi->xsize;
    stackSize[1]=Imodv->vi->ysize;
    stackSize[2]=Imodv->vi->zsize;
    findDimLimits(FIND_MAXIMAL_DIM, &xMax, &yMax, &zMax, stackSize);
    mSliders->setRange(IIS_Y_SIZE, 1, yMax);
    mSliders->setValue(IIS_Y_SIZE, yDrawSize);
    mSliders->setRange(IIS_Z_SIZE, 1, zMax);
    mSliders->setValue(IIS_Z_SIZE, zDrawSize);

    lastYsize = Imodv->vi->ysize;
  }
}

void ImodvIsosurface::viewIsoToggled(bool state)
{
  Iobj *xobj = ivwGetAnExtraObject(mIsoView, mExtraObjNum);
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
  if(state){
    setViewCenter();
    imodvDraw(Imodv);
  }
}


void ImodvIsosurface::viewModelToggled(bool state)
{
  if(state){
    imodvIsosurfaceData.a->drawExtraOnly=0;
  }else{
    imodvIsosurfaceData.a->drawExtraOnly=1;
  }
  imodvDraw(Imodv);
}

void ImodvIsosurface::viewBoxingToggled(bool state)
{
  Iobj *boxObj=ivwGetAnExtraObject(mIsoView, mBoxObjNum);
  if(state)
  { 
    //boxObj->flags= boxObj->flags | IMOD_OBJFLAG_EXTRA_MODV; 
    boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_OFF;
  }else{
    //boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
    boxObj->flags= boxObj->flags | IMOD_OBJFLAG_OFF; 
  }
  //imodvDraw(Imodv);
  imodDraw(Imodv->vi, IMOD_DRAW_MOD);
}

void ImodvIsosurface::deletePiecesToggled(bool state)
{
  filterMesh(state);
  int iterNum=mSmoothBox->text().toInt();
  iterNumChanged(iterNum);
}

//Setting up mFilteredMesh using mOrigMesh;
void ImodvIsosurface::filterMesh(bool state)
{
  if(mFilteredMesh) imodMeshDelete(mFilteredMesh);
  mFilteredMesh=imodMeshDup(mOrigMesh);

  int totalTriangle=(mOrigMesh->lsize -3)/3;
  Ipoint *finalVertex_xyz=mOrigMesh->vert;
  b3dInt32 *finalTriangle=mOrigMesh->list;

  if(state){
    mPiecesBox->setEnabled(true);
    if(mSurfPieces) delete mSurfPieces;
    mSurfPieces=new Surface_Pieces(finalVertex_xyz, finalTriangle+1,
        totalTriangle, mFilteredMesh->list+1);

    int trNum=mPiecesBox->text().toInt();
    int includedTriangle=0;
    b3dInt32 *newTList;
    b3dInt32 *start;
    int totalCC= mSurfPieces->pieces.size();
    for(int ci=0;ci<totalCC;++ci){
      if (mSurfPieces->pieces[totalCC-ci-1].area > trNum )
        includedTriangle+=mSurfPieces->pieces[totalCC-ci-1].tList->size();
      else break;
    }

    start=mFilteredMesh->list+3*(totalTriangle-includedTriangle);
    *(start)=-25;
    newTList=(b3dInt32*)malloc( (3*includedTriangle+3)*sizeof(b3dInt32) );
    for(int i=0;i<3*includedTriangle+3;i++) *(newTList+i)=*(start+i);
    free(mFilteredMesh->list);
    mFilteredMesh->list=newTList;
    mFilteredMesh->lsize=3*includedTriangle+3;

  }else{
    mPiecesBox->setEnabled(false);
  }
}

void ImodvIsosurface::linkXYZToggled(bool state)
{
  if(state) {
    imodvIsosurfaceUpdate();
    imodDraw(Imodv->vi, IMOD_DRAW_MOD);
  }
}

void ImodvIsosurface::smoothMesh(struct Mod_Mesh *mcubeMesh, int iterNum)
{
  Ipoint *vertex_xyz=mcubeMesh->vert;
  b3dInt32 *triangles=mcubeMesh->list +1; //skip the -25 code; 
  int nVertex=mcubeMesh->vsize/2;
  int nTriangle= (mcubeMesh->lsize-3)/3;
  smooth_vertex_positions((float *)vertex_xyz, nVertex, (Index *)triangles, nTriangle, 0.3, iterNum);
}

void ImodvIsosurface::iterNumChanged(int iterNum)
{
  ivwClearAnExtraObject(mIsoView, mExtraObjNum);
  Iobj *extraObj=ivwGetAnExtraObject(mIsoView, mExtraObjNum);
  struct Mod_Mesh *dup=imodMeshDup(mFilteredMesh);
  if(iterNum) smoothMesh(dup, iterNum);
  imodObjectAddMesh(extraObj, dup);
  setFocus();
  free(dup);
  setFocus();
  imodvDraw(Imodv);
}

void ImodvIsosurface::binningNumChanged(int binningNum)
{
    setBoundingBox();
    fillBinVolume();
    setIsoObj();
    setFocus();
    imodvDraw(Imodv);
}

void ImodvIsosurface::numOfTrianglesChanged(int trNum)
{
    deletePiecesToggled(true);
}

void  ImodvIsosurface::histChanged(int which, int value, bool dragging)
{ 
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)){ 
    mThreshold=value+0.5;
    setIsoObj();
    mHistSlider->setValue(0,mThreshold);
    mCurrStackIdx=getCurrStackIdx();
    mStackThresholds[mCurrStackIdx]=mThreshold;
    imodvDraw(Imodv);

    if(imodDebug('U') ){
      imodPrintStderr("In histChanged: rendering time=%d \n", isoTime.elapsed());
      isoTime.start();
    } 
  }
}

// respond to a change of transparency or contrast
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

  if(mLinkXYZ->isChecked() && which<=IIS_Z_COORD ){
    Imodv->vi->xmouse=mLocalX;
    Imodv->vi->ymouse=mLocalY;
    Imodv->vi->zmouse=mLocalZ;
    ivwBindMouse(Imodv->vi);
  }

  // draw if slider clicked or is in hot state
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)) 
  {
    bool boxChanged=isBoxChanged(mBoxOrigin, mBoxEnds);
    if (boxChanged)
    {
      setBoundingBox();
      if(mCenterVolume->isChecked() ) setViewCenter();
      setBoundingObj();

      int newSize=xDrawSize*yDrawSize*zDrawSize;
      if (newSize>mCurrMax)
      {
        free(mVolume);
        free(mBinVolume);
        mCurrMax=(double)newSize;
        mVolume=(unsigned char*)(malloc( newSize*sizeof(unsigned char) ));
        mBinVolume=(unsigned char*)(malloc( newSize*sizeof(unsigned char) ));
        if(!mVolume || !mBinVolume){
          imodPrintStderr("fail to allocate mem for %d voxles\n", newSize);
          return;
        }//else imodPrintStderr("allocate mem for %d voxels\n", newSize);
      }

      fillVolumeArray();
      fillBinVolume();
      setIsoObj();

      imodvDraw(Imodv); 
      if (which <= IIS_Z_COORD || mViewBoxing->isChecked() )
        imodDraw(Imodv->vi, IMOD_DRAW_XYZ |IMOD_DRAW_SKIPMODV );

      if(imodDebug('U') ){
        imodPrintStderr("In sliderMoved: rendering time=%d \n", isoTime.elapsed());
        isoTime.start();
      }
    }else if(which<=IIS_Z_COORD)
      imodDraw(Imodv->vi, IMOD_DRAW_XYZ);
  }


}

void ImodvIsosurface::showRubberBandArea()
{
  float x0, x1, y0, y1;
  if (zapRubberbandCoords(x0, x1, y0, y1) )
  {
    int tempSize[3];
    tempSize[0]=mIsoView->xsize;
    tempSize[1]=mIsoView->ysize;
    tempSize[2]=mIsoView->zsize;
    int xMax, yMax, zMax;
    findDimLimits(FIND_MAXIMAL_DIM, &xMax, &yMax, &zMax, tempSize);

    if (xMax>x1-x0 ) xDrawSize=x1-x0;
    else xDrawSize=xMax;

    if (yMax>y1-y0 ) yDrawSize=y1-y0;
    else yDrawSize=yMax;

    int xCenter= (x0+x1)/2.0;
    int yCenter=(y0+y1)/2.0;

    setCoordLimits( xCenter, Imodv->vi->xsize, xDrawSize, mBoxOrigin, mBoxEnds);
    setCoordLimits( yCenter, Imodv->vi->ysize, yDrawSize, mBoxOrigin+1, mBoxEnds+1);
    mBoxSize[0]=xDrawSize;
    mBoxSize[1]=yDrawSize;

    if(mCenterVolume->isChecked() ) setViewCenter();

    int newSize=xDrawSize*yDrawSize*zDrawSize;
    if (newSize>mCurrMax)
    {
      free(mVolume);
      free(mBinVolume);
      mCurrMax=(double)newSize;
      mVolume=(unsigned char*)(malloc( newSize*sizeof(unsigned char) ));
      mBinVolume=(unsigned char*)(malloc( newSize*sizeof(unsigned char) ));
      if(!mVolume || !mBinVolume){
        imodPrintStderr("fail to allocate mem for %d voxles\n", newSize);
        return;
      }//else imodPrintStderr("allocate mem for %d voxels\n", newSize);
    }

    Imodv->vi->xmouse = xCenter;
    Imodv->vi->ymouse = yCenter;

    if(mLinkXYZ->isChecked()){
      mLocalX=Imodv->vi->xmouse;
      mLocalY=Imodv->vi->ymouse;
    }

    mSliders->setValue(IIS_X_COORD, xCenter + 1);
    mSliders->setValue(IIS_Y_COORD, yCenter + 1);
    mSliders->setValue(IIS_X_SIZE, xDrawSize);
    mSliders->setValue(IIS_Y_SIZE, yDrawSize);

    setBoundingBox();
    setBoundingObj();
    fillVolumeArray();
    fillBinVolume();
    setIsoObj();

    imodvDraw(Imodv);
    if ( mViewBoxing->isChecked() )
      imodDraw(Imodv->vi, IMOD_DRAW_XYZ |IMOD_DRAW_SKIPMODV );
  }

}
// Action buttons
void ImodvIsosurface::buttonPressed(int which)
{
  if (which==2) imodShowHelpPage("modvIsosurface.html");
  else if (which==1) close();
  else if (which==0){
    float red, green, blue;
    Imod *userModel=ivwGetModel(mIsoView);

    imodNewObject(userModel);
    Iobj *userObj=imodObjectGet(userModel);
    imodObjectGetColor(userObj, &red, &green, &blue);

    Iobj *isoObj = ivwGetAnExtraObject(mIsoView, mExtraObjNum);
    Iobj *dup=imodObjectDup(isoObj);

    // Tone the colors down here also to prevent washout
    imodObjectSetColor(dup, 0.6 * red, 0.6 * green, 0.6 * blue);
    imodObjectCopy(dup, userObj);
    strcpy(userObj->name, "Saved isosurface");

    imodNewObject(userModel);
    Iobj *userBoxObj=imodObjectGet(userModel);

    Iobj *boxObj=ivwGetAnExtraObject(mIsoView, mBoxObjNum);
    Iobj *dupBox=imodObjectDup(boxObj);
    imodObjectCopy(dupBox, userBoxObj);
    strcpy(userBoxObj->name, "Saved bounding box");
    Icont *firstCont=imodObjectGetContour(userBoxObj, 0);
    firstCont->flags &=~ICONT_DRAW_ALLZ;    
    imodDraw(Imodv->vi, IMOD_DRAW_MOD);
    imodvObjedNewView();
    free(dup);
    free(dupBox);
  } 
}

void ImodvIsosurface::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// Accept a close event and set dia to null
void ImodvIsosurface::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)imodvIsosurfaceData.dia);
  imodvIsosurfaceData.dia = NULL;
  e->accept();

  ivwFreeExtraObject(mIsoView, mBoxObjNum);
  ivwFreeExtraObject(mIsoView, mExtraObjNum);
  imodvObjedNewView();
  imodvIsosurfaceData.a->drawExtraOnly=0; //enable user model drawing;

  imodDraw(Imodv->vi, IMOD_DRAW_MOD);

  if(!mViewIso->isChecked() ) imodvIsosurfaceData.flags &=~IMODV_VIEW_ISOSURFACE;
  else  imodvIsosurfaceData.flags |=IMODV_VIEW_ISOSURFACE;

  if(!mViewModel->isChecked())imodvIsosurfaceData.flags &=~IMODV_VIEW_USER_MODEL;
  else imodvIsosurfaceData.flags |=IMODV_VIEW_USER_MODEL;

  if(!mViewBoxing->isChecked() ) imodvIsosurfaceData.flags &=~IMODV_VIEW_BOX;
  else  imodvIsosurfaceData.flags |=IMODV_VIEW_BOX;

  if(!mCenterVolume->isChecked())imodvIsosurfaceData.flags &=~IMODV_CENTER_VOLUME;
  else  imodvIsosurfaceData.flags |=IMODV_CENTER_VOLUME;

  if(!mLinkXYZ->isChecked() ) imodvIsosurfaceData.flags &=~IMODV_LINK_XYZ;
  else  imodvIsosurfaceData.flags |=IMODV_LINK_XYZ;

  if(!mDeletePieces->isChecked() ) 
    imodvIsosurfaceData.flags &=~IMODV_DELETE_PIECES;
  else  imodvIsosurfaceData.flags |=IMODV_DELETE_PIECES;

  imodvIsosurfaceData.itNum=mSmoothBox->text().toInt();
  imodvIsosurfaceData.binningNum=mBinningBox->text().toInt();
  imodvIsosurfaceData.minTNum=mPiecesBox->text().toInt();
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

/*

   $Log$
   Revision 4.18  2010/03/30 02:21:29  mast
   Made the merging of results from multiple threads run faster and used
   OpenMP for big array copies - turned on multithreading with ideal thread
   count limited to 4

   Revision 4.17  2009/03/22 19:54:25  mast
   Show with new geometry adjust routine for Mac OS X 10.5/cocoa

   Revision 4.16  2009/01/15 16:33:18  mast
   Qt 4 port

   Revision 4.15  2008/12/19 00:42:46  mast
   Change color again, change the names for saved object, scale down
   automatically selected color for saved object

   Revision 4.14  2008/12/16 00:00:01  mast
   Set a color and material properties to reduce washout

   Revision 4.13  2008/12/09 23:26:57  mast
   Changed flag from line to noline

   Revision 4.12  2008/11/15 22:03:25  mast
   Added object names to show up in object edit window

   Revision 4.11  2008/11/12 18:56:07  xiongq
   call setFocus() for slots of spinners

   Revision 4.10  2008/11/07 23:48:53  xiongq
   seperate threshold for each stack

   Revision 4.9  2008/10/02 16:27:00  xiongq
   add small piece filter, binning, and local XYZ functions

   Revision 4.8  2008/08/19 15:17:16  mast
   Changed for loop declarations for old intel compiler

   Revision 4.7  2008/05/27 16:41:01  xiongq
   add Use Rubber Band button

   Revision 4.6  2008/05/23 19:23:32  xiongq
   Use multithreads to compute isosurface. Move the calling of imodvIsosurfaceUpdate() from imod_info_cb.cpp to imod_display.cpp.

   Revision 4.5  2008/05/03 00:47:31  mast
   Fixed mOrigMesh memory leak
*/
