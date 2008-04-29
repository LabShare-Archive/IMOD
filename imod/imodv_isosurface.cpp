#include <qcheckbox.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qlayout.h>
#include <qgl.h>
#include <qslider.h>
#include <qtooltip.h>
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "imodv_gfx.h"
#include "mkmesh.h"
#include "imodv_mcubes.h"
#include "imodv_isosurface.h"
#include "histwidget.h"
#include "imodv_input.h"
#include "imod_display.h"
#include "control.h"
#include "preferences.h"
#include "xcramp.h"


enum {IIS_X_COORD = 0, IIS_Y_COORD, IIS_Z_COORD, IIS_X_SIZE, IIS_Y_SIZE,
  IIS_Z_SIZE};
enum {FIND_DEFAULT_DIM=0, FIND_MAXIMAL_DIM};


static int xDrawSize = -1;
static int yDrawSize = -1;
static int zDrawSize = -1;
static int lastYsize = -1;

#define MAXIMAL_VOXELS 128*512*512  //the number of voxles the bounding box is limited to;
#define DEFAULT_VOXELS 96*96*96 //the number of voxles the initial bounding box;
#define PERCENTILE 0.07

struct{
  ImodvIsosurface *dia;
  ImodvApp  *a;

  int    flags;

  /* DNM 12/30/02: unused currently */
  /* int    xsize, ysize, zsize;
     int    *xd, *yd, *zd; */

}imodvIsosurfaceData = {0, 0, ~0};

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
  imodvIsosurfaceData.dia = new ImodvIsosurface(a->vi, imodvDialogManager.parent(IMODV_DIALOG),
      "isosurface view");
  imodvDialogManager.add((QWidget *)imodvIsosurfaceData.dia, IMODV_DIALOG);
  imodvIsosurfaceData.dia->show();
}

void imodvIsosurfaceUpdate(void)
{
  if(imodvIsosurfaceData.dia)
  {
    imodvIsosurfaceData.dia->updateCoords();
    if( isBoxChanged(imodvIsosurfaceData.dia->mBoxOrigin, imodvIsosurfaceData.dia->mBoxEnds) )
    {
      imodvIsosurfaceData.dia->setBoundingBox();
      if(imodvIsosurfaceData.dia->mCenterVolume->isChecked() )
          imodvIsosurfaceData.dia->setViewCenter();
      imodvIsosurfaceData.dia->setBoundingObj();
      imodvIsosurfaceData.dia->fillVolumeArray();
      imodvIsosurfaceData.dia->setIsoObj();
    }    
    imodvDraw(Imodv);
  }else return;
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
  int newStart, newEnd;
  setCoordLimits(Imodv->vi->xmouse, Imodv->vi->xsize, xDrawSize, &newStart, &newEnd);
  if( *start!=newStart || *end!=newEnd) return true;
  setCoordLimits(Imodv->vi->ymouse, Imodv->vi->ysize, yDrawSize, &newStart, &newEnd);
  if( *(start+1)!=newStart || *(end+1)!=newEnd) return true;
  setCoordLimits(Imodv->vi->zmouse, Imodv->vi->zsize, zDrawSize, &newStart, &newEnd);
  if( *(start+2)!=newStart || *(end+2)!=newEnd) return true;
  return false;
}

static void findDimLimits(int which, int *xdim, int *ydim, int *zdim, int
    *sizes)
{  
  int specMinDim;
  int tempMin;
  int voxelMax;

  if(which==FIND_DEFAULT_DIM){
    specMinDim=powf(DEFAULT_VOXELS, 1.0/3.0);
    voxelMax=DEFAULT_VOXELS;
  }
  else if(which==FIND_MAXIMAL_DIM)
  {
    specMinDim=powf(MAXIMAL_VOXELS, 1.0/3.0);
    voxelMax=MAXIMAL_VOXELS;
  }
  else
  {
    fprintf(stderr, "imodv_isosurface: illegal usage\n");
    return;
  }

  if(sizes[0]<=sizes[1] && sizes[0]<=sizes[2])
  { 
    *xdim=B3DMIN(specMinDim, sizes[0]);
    tempMin=(int)sqrt((double)( voxelMax/(*xdim)) );
    *ydim=B3DMIN(tempMin, sizes[1]);
    *zdim=B3DMIN(tempMin, sizes[2]);
  }
  else if(sizes[1]<=sizes[0] && sizes[1]<=sizes[2])
  {
    *ydim=B3DMIN(specMinDim, sizes[1]);
    tempMin=(int)sqrt((double)(voxelMax/(*ydim)) );
    *xdim=B3DMIN(tempMin, sizes[0]);
    *zdim=B3DMIN(tempMin, sizes[2]);
  }
  else if(sizes[2]<=sizes[0] && sizes[2]<=sizes[1])
  {
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
  //temporarily store input stack size, will be reset at the end;
  mBoxSize[0]=vi->xsize;
  mBoxSize[1]=vi->ysize;
  mBoxSize[2]=vi->zsize;
  mVolume=NULL;

  // Make view checkboxes
  mViewIso = diaCheckBox("View isosurfaces", this, mLayout);
  mViewIso->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_ISOSURFACE);
  connect(mViewIso, SIGNAL(toggled(bool)), this, SLOT(viewIsoToggled(bool)));
  mViewModel = diaCheckBox("View user models", this, mLayout);
  mViewModel->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_USER_MODEL);
  connect(mViewModel, SIGNAL(toggled(bool)), this, SLOT(viewModelToggled(bool)));
  mViewBoxing = diaCheckBox("View bounding box", this, mLayout);
  mViewBoxing->setChecked(imodvIsosurfaceData.flags & IMODV_VIEW_BOX);
  connect(mViewBoxing, SIGNAL(toggled(bool)), this, SLOT(viewBoxingToggled(bool)));
  mCenterVolume=diaCheckBox("Keep isosurfaces centered", this, mLayout);
  mCenterVolume->setChecked(imodvIsosurfaceData.flags & IMODV_CENTER_VOLUME);
  connect(mCenterVolume,
      SIGNAL(toggled(bool)),this,SLOT(centerVolumeToggled(bool)));
  QToolTip::add(mViewIso, "Display isosurfaces");
  QToolTip::add(mViewModel, "Display user models");
  QToolTip::add(mViewBoxing, "Display the bounding box");
  QToolTip::add(mCenterVolume, "Keep isosurfaces centered in the model view window"); 

  mHistPanel=new HistWidget(this);
  mHistPanel->setMinimumSize(size().width(), 80);
  mHistSlider=new MultiSlider(this, 1, histLabels);
  mHistSlider->setRange(0, 0, 255);
  connect(mHistSlider, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(histChanged(int, int, bool)));
  
  mLayout->addWidget(mHistPanel);
  mLayout->setStretchFactor(mHistPanel, 100);
  mLayout->addLayout(mHistSlider->getLayout());

  // Make multisliders
  mSliders = new MultiSlider(this, 6, sliderLabels);
  findDimLimits(FIND_DEFAULT_DIM, &xDrawSize, &yDrawSize, &zDrawSize, mBoxSize);
  int xMax, yMax, zMax;
  findDimLimits(FIND_MAXIMAL_DIM, &xMax, &yMax, &zMax, mBoxSize);

  mBoxSize[0]=xDrawSize;
  mBoxSize[1]=yDrawSize;
  mBoxSize[2]=zDrawSize;

  mSliders->setRange(IIS_X_COORD, 1, vi->xsize);
  mSliders->setRange(IIS_X_SIZE, 1, xMax );
  mSliders->setRange(IIS_Y_SIZE, 1, yMax );
  mSliders->setRange(IIS_Z_SIZE, 1, zMax );
  if (lastYsize < 0) {
    lastYsize = Imodv->vi->ysize;
  }

  if( Imodv->vi->xmouse==0.0 &&  Imodv->vi->ymouse==0.0)
  { 
    Imodv->vi->xmouse=Imodv->vi->xsize/2.0;
    Imodv->vi->ymouse=Imodv->vi->ysize/2.0;
    imodDraw(Imodv->vi, IMOD_DRAW_XYZ|IMOD_DRAW_SKIPMODV);
  }

  updateCoords();
  mSliders->setValue(IIS_X_SIZE, xDrawSize);
  mSliders->setValue(IIS_Y_SIZE, yDrawSize);
  mSliders->setValue(IIS_Z_SIZE, zDrawSize);
  mLayout->addLayout(mSliders->getLayout());
  (mSliders->getLayout())->setSpacing(4);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
      SLOT(sliderMoved(int, int, bool)));
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_X_COORD),
      "Set X coordinate of the box center");
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_Y_COORD),
      "Set Y coordinate of the box center");
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_Z_COORD),
      "Set Z coordinate of the box center");
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_X_SIZE),
      "Set bounding box size in X");
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_Y_SIZE),
      "Set bounding box size in Y");
  QToolTip::add((QWidget *)mSliders->getSlider(IIS_Z_SIZE),
      "Set bounding box size in Z");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  setBoundingBox();
  if(mCenterVolume->isChecked() ) setViewCenter();
  setBoundingObj();

  mVolume=(unsigned char*)
    (malloc( mBoxSize[0]*mBoxSize[1]*mBoxSize[2]*sizeof(unsigned char) ));
  mCurrMax=(double)(mBoxSize[0]*mBoxSize[1]*mBoxSize[2]);
  
  mThreshold=fillVolumeArray();

  mHistSlider->setValue(0,(int)mThreshold);

  Iobj *xobj = ivwGetAnExtraObject(mIsoView, mExtraObjNum);
  if (mViewIso->isChecked() ) {
    xobj->flags= xobj->flags | IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_FILL |
      IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_LINE |IMOD_OBJFLAG_TWO_SIDE;
  } else {
    xobj->flags = xobj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
  }

  Iobj *boxObj=ivwGetAnExtraObject(mIsoView, mBoxObjNum);
  if( mViewBoxing->isChecked())
  { 
    boxObj->flags= boxObj->flags | IMOD_OBJFLAG_EXTRA_MODV; 
  }else{
    boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
  }
  
  if(mViewModel->isChecked() ){
    imodvIsosurfaceData.a->drawExtraOnly=0;
  }else{
    imodvIsosurfaceData.a->drawExtraOnly=1;
  }
  
  histChanged(0, (int)mThreshold, false);
}

ImodvIsosurface::~ImodvIsosurface()
{
  free(mVolume);
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
  float *hist=mHistPanel->getHist();
  for(int i=0;i<256;i++) hist[i]=0.0;
  for( int zi =mBoxOrigin[2];zi<mBoxEnds[2];zi++)
    for( int yi=mBoxOrigin[1];yi<mBoxEnds[1];yi++)
      for( int xi=mBoxOrigin[0];xi<mBoxEnds[0];xi++)
      {
        value=(*ivwFastGetValue)(xi, yi, zi);
        *( mVolume
            + (zi-mBoxOrigin[2])*mBoxSize[0]*mBoxSize[1]
            + (yi-mBoxOrigin[1])*mBoxSize[0]
            + xi-mBoxOrigin[0] ) = value;
        hist[value]+=1.0;
      }

  for( i=0;i<256;i++) hist[i]=hist[i]/(mBoxSize[0]*mBoxSize[1]*mBoxSize[2]);
  mHistPanel->setMinMax();

  mHistPanel->update();
  return mHistPanel->computePercentile(PERCENTILE);
}

void ImodvIsosurface::setIsoObj()
{
  //int stride[3]={mBoxSize[1]*mBoxSize[2], mBoxSize[2], 1};
  int stride[3]={1, mBoxSize[0], mBoxSize[0]*mBoxSize[1]};
  //call marching cubes routine;
  Contour_Surface *cs=surface(mVolume, (Index *)mBoxSize, (Index *)stride, mThreshold, false);

  int nVertex=cs->vertex_count();
  int nTriangle=cs->triangle_count();
  //twice as big to store normals;
  Ipoint *vertex_xyz=(Ipoint *)malloc(2*nVertex*sizeof(Ipoint));

  b3dInt32 *triangles=(b3dInt32*)malloc( (3*nTriangle+3)*sizeof(b3dInt32) );

  cs->geometry((float *)vertex_xyz, (Index *)triangles+1, mBoxOrigin);
  cs->normals( (float *)vertex_xyz );

  triangles[0]=-25;
  //change the index since vertex_xyz includes normals;
  for(int i=1;i<3*nTriangle+1;i++) triangles[i]=2*triangles[i];
  triangles[3*nTriangle+1]=-22;
  triangles[3*nTriangle+2]=-1;

  //create a Imesh structure; 
  Imesh *mcubeMesh=imodMeshNew();
  mcubeMesh->vert=vertex_xyz;
  mcubeMesh->list=triangles;
  mcubeMesh->vsize= 2*nVertex;
  mcubeMesh->lsize= 3*nTriangle+3;

  //free all memory used by the old mesh; 
  ivwClearAnExtraObject(mIsoView, mExtraObjNum);
  Iobj *extraObj=ivwGetAnExtraObject(mIsoView, mExtraObjNum);
  //attch the new mesh to the extra obj;
  imodObjectAddMesh(extraObj, mcubeMesh);
  free(mcubeMesh);
  delete cs;
}

//Using current mouse position and drawSize to deduce the origin and ends 
//of the bounding box.
void ImodvIsosurface::setBoundingBox()
{
  setCoordLimits(Imodv->vi->xmouse, Imodv->vi->xsize, xDrawSize, mBoxOrigin, mBoxEnds);
  setCoordLimits(Imodv->vi->ymouse, Imodv->vi->ysize, yDrawSize, mBoxOrigin+1, mBoxEnds+1);
  setCoordLimits(Imodv->vi->zmouse, Imodv->vi->zsize, zDrawSize, mBoxOrigin+2, mBoxEnds+2);
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

  ivwClearAnExtraObject(Imodv->vi, mBoxObjNum);
  Iobj *bObj=ivwGetAnExtraObject(Imodv->vi, mBoxObjNum);
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
      IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_LINE |IMOD_OBJFLAG_TWO_SIDE;
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
    boxObj->flags= boxObj->flags | IMOD_OBJFLAG_EXTRA_MODV; 
  }else{
    boxObj->flags = boxObj->flags & ~IMOD_OBJFLAG_EXTRA_MODV;
  }
  imodvDraw(Imodv);
}


void  ImodvIsosurface::histChanged(int which, int value, bool dragging)
{ 
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)){ 
    mThreshold=value;
    setIsoObj();
    mHistSlider->setValue(0,mThreshold);
    imodvDraw(Imodv);
  }
}


// respond to a change of transparency or contrast
void ImodvIsosurface::sliderMoved(int which, int value, bool dragging)
{
  switch (which) {
    case IIS_X_COORD:
      Imodv->vi->xmouse = value - 1;
      ivwBindMouse(Imodv->vi);
      break;
    case IIS_Y_COORD:
      Imodv->vi->ymouse = value - 1;
      ivwBindMouse(Imodv->vi);
      break;
    case IIS_Z_COORD:
      Imodv->vi->zmouse = value - 1;
      ivwBindMouse(Imodv->vi);
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
      if( newSize>mCurrMax)
      {
        free(mVolume);
        mCurrMax=(double)newSize;
        mVolume=(unsigned char*)(malloc( newSize*sizeof(unsigned char) ));
        if(!mVolume){
          fprintf(stderr, "fail to allocate mem for %d voxles\n", newSize);
          return;
        }//else printf("allocate mem for %d voxels\n", newSize);
      }
      mBoxSize[0]=xDrawSize;
      mBoxSize[1]=yDrawSize;
      mBoxSize[2]=zDrawSize;

      fillVolumeArray();
      setIsoObj();

      if (which > IIS_Z_COORD)
      {
        imodvDraw(Imodv); 
      }else{
        imodDraw(Imodv->vi, IMOD_DRAW_XYZ);
      }

    }else if(which<=IIS_Z_COORD)
      imodDraw(Imodv->vi, IMOD_DRAW_XYZ);
  }
}

// Action buttons
void ImodvIsosurface::buttonPressed(int which)
{
  if (which==2) imodShowHelpPage("modvIsosurface.html");
  else if( which==1) close();
  else if( which==0){
    float red, green, blue;
    Imod *userModel=ivwGetModel(mIsoView);

    imodNewObject(userModel);
    Iobj *userObj=imodObjectGet(userModel);
    imodObjectGetColor(userObj, &red, &green, &blue);

    Iobj *isoObj = ivwGetAnExtraObject(mIsoView, mExtraObjNum);
    Iobj *dup=imodObjectDup(isoObj);
    imodObjectSetColor(dup, red, green, blue);
    imodObjectCopy(dup, userObj);
    
    imodNewObject(userModel);
    Iobj *userBoxObj=imodObjectGet(userModel);

    Iobj *boxObj=ivwGetAnExtraObject(mIsoView, mBoxObjNum);
    Iobj *dupBox=imodObjectDup(boxObj);
    imodObjectCopy(dupBox, userBoxObj);

    imodDraw(Imodv->vi, IMOD_DRAW_MOD);
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
  imodvDraw(Imodv);
  
  if(!mViewIso->isChecked() ) imodvIsosurfaceData.flags &=~IMODV_VIEW_ISOSURFACE;
  else  imodvIsosurfaceData.flags |=IMODV_VIEW_ISOSURFACE;

  if(!mViewModel->isChecked())imodvIsosurfaceData.flags &=~IMODV_VIEW_USER_MODEL;
  else imodvIsosurfaceData.flags |=IMODV_VIEW_USER_MODEL;

  if(!mViewBoxing->isChecked() ) imodvIsosurfaceData.flags &=~IMODV_VIEW_BOX;
  else  imodvIsosurfaceData.flags |=IMODV_VIEW_BOX;

  if(!mCenterVolume->isChecked())imodvIsosurfaceData.flags &=~IMODV_CENTER_VOLUME;
  else  imodvIsosurfaceData.flags |=IMODV_CENTER_VOLUME;
}

// Close on escape; watch for the hot slider key; pass on keypress
void ImodvIsosurface::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
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
