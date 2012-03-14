#include "imodel.h"
#include "isothread.h" 
#include "isosurface.h"
#include "mcubes.h"

IsoThread::IsoThread(int subsliceIndex, ImodvIsosurface *iso)
{ 
  mWhichSubslice = subsliceIndex;
  mIso = iso;
}

void IsoThread::run()
{
  int *boxSize = mIso->mBinBoxSize;
  int *boxOrigin = mIso->mBoxOrigin;
  int *subZEnds = mIso->mSubZEnds;
  unsigned char *volume = mIso->mBinVolume;

  int currSize[3];
  currSize[0] = boxSize[0];
  currSize[1] = boxSize[1];
  int currOrigin[3];
  currOrigin[0] = boxOrigin[0]; 
  currOrigin[1] = boxOrigin[1];
  int stride[3] = {1, boxSize[0], boxSize[0]*boxSize[1]};
  unsigned char *subVolumePtr;
  Contour_Surface *cs;

  int i = mWhichSubslice;
  int binNum = mIso->getBinning();
  if(mIso->mNThreads==1) // Only one thread
    currSize[2] = subZEnds[i+1]-subZEnds[i]+1;
  else if(i==0 || i==mIso->mNThreads-1)
    currSize[2] = subZEnds[i+1]-subZEnds[i]+1+1;
  else
    currSize[2] = subZEnds[i+1]-subZEnds[i]+1+1+1;

  if(i==0){
    currOrigin[2] = subZEnds[i];
    subVolumePtr = volume;
  }else{
    //currOrigin[2] = subZEnds[i];
    currOrigin[2] = subZEnds[0]+binNum*(subZEnds[i]-subZEnds[0])-binNum;
    subVolumePtr = volume+boxSize[0]*boxSize[1]*(subZEnds[i]-subZEnds[0]-1);
  }

  cs = surface(subVolumePtr, (Index *)currSize, (Index *)stride, mIso->mThreshold, false);
  mNVertex = cs->vertex_count();
  mNTriangle = cs->triangle_count();
  mVertex_xyz = (Ipoint *)malloc(2*mNVertex*sizeof(Ipoint));
  if(i==0) { //treat first subslice differently in case only 1 thread;
    mTriangle = (b3dInt32 *)malloc( (3*mNTriangle+3)*sizeof(b3dInt32) );
    cs->geometry( (float *)mVertex_xyz, (Index *)(mTriangle+1), currOrigin,
        binNum);
  } else {
    mTriangle = (b3dInt32 *)malloc( (3*mNTriangle)*sizeof(b3dInt32) );
    cs->geometry( (float *)mVertex_xyz, (Index *)mTriangle, currOrigin, binNum);
  }
  cs->normals( (float *)mVertex_xyz );
  
  delete cs;
}
