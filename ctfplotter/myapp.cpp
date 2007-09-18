#include <qlabel.h>
#include <qfile.h>
#include <qtoolbutton.h>

#include <stdio.h>
#include <math.h>

#include "plotter.h"
#include "myapp.h"
#include "simplexfitting.h"
#include "linearfitting.h"

#include "b3dutil.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include "sliceproc.h"
#include "cfft.h"

#define MY_PI 3.1415926
#define MIN_ANGLE 1.0e-6  //tilt Angle less than this is treated as 0.0;

int MyApp::nDim=0;
int MyApp::tileSize=0;

  MyApp::MyApp(int &argc, char *argv[], int volt, double pSize, 
      double ampRatio, float cs, char *defFn,
      int dim, double focusTol, int tSize, double tAxisAngle, double lAngle,
      double hAngle, double expDefocus, double leftTol, double rightTol)
:QApplication(argc, argv), defocusFinder(volt, pSize, ampRatio, cs, dim, expDefocus)
{
  nDim=dim;
  defocusTol=focusTol;
  tileSize=tSize;
  tiltAxisAngle=tAxisAngle;
  pixelSize=pSize;
  voltage=volt;
  lowAngle=lAngle;
  highAngle=hAngle;
  sliceNum=MAXSLICENUM;
  for(int i=0;i<MAXSLICENUM;i++) slice[i]=NULL;
  x1MethodIndex=1; //0:Linear, 1:Simplex;
  x2MethodIndex=1; //0:Linear, 1:Simplex;
  defocusOption=0; //use expected defocus;
  initialTileOption=0; //only use central tiles initially;
  leftDefTol=leftTol;
  rightDefTol=rightTol;
  saveFp=NULL;
  fnDefocus=defFn;
}

//plot and fit the PS stored in 'rAverage', find the defocus and update display;
void MyApp::plotFitPS()
{
    double *ps=(double*)malloc(nDim*sizeof(double) );
    CurveData data[3];
    double *resSimplex=(double*)malloc(nDim*sizeof(double));
    double *resLinear=(double *)malloc(nDim*sizeof(double));
    double model[2]={0,1.0};
    double err;
    int ii;
    //Dividing the Noise PS;
    double *nps=(double*)malloc(nDim*sizeof(double));
    for(ii=0;ii<nDim;ii++){
      nps[ii]=lowPs[ii]+(highPs[ii]-lowPs[ii])*
             (stackMean-lowMean)/(highMean-lowMean);
      ps[ii]=rAverage[ii]/nps[ii];
    }

    Plotter *plotter=(Plotter *)mainWidget();
    for(ii=0;ii<nDim;ii++){ 
      ps[ii]=log(ps[ii]);
      data[0].push_back(ii/(float)(nDim-1) );
      data[0].push_back(ps[ii]);
    }
    plotter->setCurveData(0, data[0]);

    simplexEngine->setRaw(&ps[0]);
    linearEngine->setRaw(&ps[0]);

    switch(x1MethodIndex){
      case 0:
        if(!linearEngine->computeFitting(resSimplex, model, 2, x1Idx1, x1Idx2)){
        }else  printf("linearEngine error\n");
        break;
      case 1:
        simplexEngine->setRange(x1Idx1, x1Idx2);
        if( !simplexEngine->computeFitting(resSimplex, &err, 0) ){
        }else printf("simplexEngine error\n");
        break;
      default:
        printf("unknown fitting method chosen\n"); 
    }
    for(ii=0;ii<nDim;ii++){
      data[1].push_back(ii/(float)(nDim-1) );
      data[1].push_back(resSimplex[ii]);
    }
    plotter->setCurveData(1, data[1]);

    switch(x2MethodIndex){
      case 0:
        if( !linearEngine->computeFitting(resLinear, model, 2, x2Idx1, x2Idx2)){
        }else printf("linearEngine error\n");
        break;
      case 1:
        simplexEngine->setRange(x2Idx1, x2Idx2);
        if( !simplexEngine->computeFitting(resLinear, &err, 1) ){
        }else printf("simplexEngine error\n");
        break;
      default:
        printf("unknown fitting method chosen\n"); 
    }
    for(ii=0;ii<nDim;ii++){
      data[2].push_back(ii/(float)(nDim-1));
      data[2].push_back(resLinear[ii]);
    }
    plotter->setCurveData(2, data[2]);

    //calculate defocus and update display;
    double zero;
    double defocus;
    char zeroString[20]="Z: NA ";
    char defocusString[20]="D: NA ";
    if( !defocusFinder.findZero(resSimplex, resLinear, x1Idx1, 
        (x2Idx1+x2Idx2)/2, &zero) ){
      sprintf(zeroString, "Z: %4.3f", zero);
      defocusFinder.findDefocus(&defocus);
      sprintf(defocusString, "D: %4.2f", defocus);
    }else defocusFinder.setDefocus(-2000.0);
    plotter->zeroLabel->setText(zeroString);
    plotter->defocusLabel->setText(defocusString);
    free(ps);
    free(resSimplex);
    free(resLinear);
    free(nps);
}

void MyApp::setSlice(char *stackFile, char *angleFile)
{
  if( stackFile || angleFile){  
    // if both are NULL, do not change file names; used in angleChanged();
    fnStack=stackFile;
    fnAngle=angleFile;
  }
  //free old slices before setting new slices;
  for(int i=0;i<sliceNum;i++) if(slice[i]) sliceFree(slice[i]);

  FILE *fpStack, *fpAngle=NULL;
  MrcHeader header;
  int sliceMode;
  char angleStr[30];

  if( (fpStack=fopen(fnStack, "rb"))==0 ){
    printf("ERROR: - could not open input file %s\n", fnStack);
    exit (1);
  }
  if(fnAngle){
    if( (fpAngle=fopen(fnAngle, "r"))==0 ){
      printf("could not open angle file %s, tiltAngle is set to 0.0\n",fnAngle);
    }
  }

  /* read header */	
  if (mrc_head_read(fpStack, &header)) {
    printf("ERROR: - reading header of input file %s\n",  fnStack);
    exit(1);
  }
  // Check if it is the correct data type and set slice type
  if (header.mode == MRC_MODE_BYTE)
    sliceMode = SLICE_MODE_BYTE;
  else if (header.mode == MRC_MODE_SHORT)
    sliceMode = SLICE_MODE_SHORT;
  else if (header.mode == MRC_MODE_FLOAT)
    sliceMode = SLICE_MODE_FLOAT;
  else {
    printf("ERROR:- File mode is %d; only byte, short, integer allowed\n", 
        header.mode);
    exit(1);
  }

  int nx = header.nx;
  int ny = header.ny;
  int nz = header.nz;
  int sIndex;
  float currAngle;
  int startingSlice=-1;
  sliceNum=nz;
  nxx=nx;
  nyy=ny; 
  int k;
  float stripPixelNum=0.0;
  for(k=0;k<MAXSLICENUM;k++) slice[k]=NULL;

  sIndex=0;
  for(k=0;k<sliceNum;k++){
    if( fpAngle && fgets(angleStr, 30, fpAngle) ){
      sscanf(angleStr, "%f", &currAngle);
      printf("Slice %d, tilt angle is %f degrees. \n", k, currAngle);
    }else{
      currAngle=0.0;
      printf("No angle is specified, set to 0.0\n");
    }

    if(sIndex>=MAXSLICENUM){
      printf("Stack has more than maximal number of slice in \
          the specified angle range. exit.\n");
      fclose(fpStack);
      fclose(fpAngle);
      sliceNum=sIndex-1;
      return ;
    }

    if( currAngle<lowAngle || currAngle>highAngle) continue;
    if( startingSlice==-1) startingSlice=k+1;
    slice[sIndex]=sliceCreate(nx, ny, sliceMode);
    if(!slice[sIndex]){
      printf("ERROR: -creating slice for input\n");
      exit(1);
    }

    if( mrc_read_slice(slice[sIndex]->data.b, fpStack, &header, k, 'Z') ){
      printf("ERROR: -reading slice\n");
      exit(1);
    }
    printf("Slice %d of stack %s is included\n", k, fnStack);

    //convert slice to floats
    if(sliceMode !=SLICE_MODE_FLOAT)
      if( sliceNewMode(slice[sIndex], SLICE_MODE_FLOAT)<0 ){
        printf("ERROR: -converting slice to float\n");
        exit(1);
      }

    tiltAngle[sIndex]=currAngle*MY_PI/180.0;
    if( fabs(tiltAngle[sIndex]) > MIN_ANGLE ) 
      stripPixelNum=fabs( defocusTol/tan(tiltAngle[sIndex]) )/pixelSize;
    else stripPixelNum=nx;
    if( stripPixelNum>nx) stripPixelNum=nx;

    // tiles are overlapping by half of their size
    sIndex++;
  }//k slice
  sliceNum=sIndex;
  initialSlice=startingSlice;
  fclose(fpStack);
  if(fpAngle) fclose(fpAngle);
}

int  MyApp::computeInitPS()
{
  float stripPixelNum=0.0;
  int halfSize=tileSize/2;
  int idir=0; //FFT transform;
  int tileNum=0;
  int xOffset=0;
  float *tile=(float *)malloc(tileSize*(tileSize+2)*sizeof(float) );
  double *fftSum=(double*)malloc( tileSize*((tileSize+2)/2)*sizeof(double) );
  int counter;
  double localMean, tmpMean;
  int i,j,k,ii,jj;
  double diagonal=sqrt((double)(nxx*nxx+nyy*nyy) );
  double axisXAngle, centerX, centerY, r, alpha,d;

  //tilt axis angle with X coordinate, [0, 2*pi];
  axisXAngle=90+tiltAxisAngle; 
  axisXAngle=axisXAngle*MY_PI/180.0;
  //if(axisXAngle<0) axisXAngle=2.0*M_PI+axisXAngle;

  counter=0;
  localMean=0.0;
  for(i=0;i<tileSize;i++)
    for(j=0;j<(tileSize+2)/2;j++) *(fftSum+ i*(tileSize+2)/2+j)=0;
  for(i=0;i<sliceNum;i++) tileIncluded[i]=0;

  for(k=0;k<sliceNum;k++){
    //'stripPixelNum' is the strip width in pixels
    if(fabs(tiltAngle[k])>MIN_ANGLE) 
      stripPixelNum=fabs( defocusTol/tan(tiltAngle[k]) )/pixelSize;
    else stripPixelNum=diagonal;
    if(stripPixelNum>diagonal) stripPixelNum=diagonal;

    xOffset=0;
    for(i=0;i<nxx/halfSize-1;i++)
      for(j=0;j<nyy/halfSize-1;j++){
        centerX=i*halfSize+halfSize-nxx/2;//center coordinate of the tile;
        centerY=j*halfSize+halfSize-nyy/2;
        r=sqrt((double)(centerX*centerX+centerY*centerY));
        alpha=atan2(centerY, centerX);
        //if(alpha<0) alpha=2.0*M_PI+alpha;//convert to [0,2*pi];
        d=r*fabs(sin(alpha-axisXAngle));// distance to the tilt axis in pixels;

        //outside of the strip, continue;
        if(d>stripPixelNum/2.0) continue;

        counter++;
        tileIncluded[k]++;
        sliceTaperInPad(slice[k]->data.f, SLICE_MODE_FLOAT, nxx, 
            i*halfSize+xOffset, i*halfSize+xOffset+tileSize-1, j*halfSize,
            j*halfSize+tileSize-1,tile,tileSize+2,tileSize,tileSize,9,9);

        tmpMean=0.0;
        for(ii=0;ii<tileSize;ii++)
          for(jj=0;jj<tileSize;jj++)tmpMean+=*(tile+ii*(tileSize+2)+jj);
        tmpMean=tmpMean/(tileSize*tileSize);
        localMean+=tmpMean;

        todfft(tile, &tileSize, &tileSize, &idir);

        for(ii=0;ii<tileSize;ii++)
          for(jj=0;jj<(tileSize+2)/2;jj++)
            *(fftSum+ii*(tileSize+2)/2+jj)+=
              *(tile+ii*(tileSize+2)+2*jj) * ( *(tile+ii*(tileSize+2)+2*jj)) + 
              *(tile+ii*(tileSize+2)+2*jj+1)* (*(tile+ii*(tileSize+2)+2*jj+1));
      }
  }//k loop;

  printf("computeInitPS() includes %d tiles\n", counter);
  totalTileIncluded=counter;

  if(counter){
    for(ii=0;ii<tileSize;ii++)
      for(jj=0;jj<(tileSize+2)/2;jj++) *(fftSum+ii*(tileSize+2)/2+jj)/=counter;
    //two sided rotational averaging;
    float freqInc=1.0/(nDim-1);
    int nyquist=(tileSize+2)/2;
    int rIndex;
    int *freqCounter=(int*)malloc(nDim*sizeof(int));

    for(i=0;i<nDim;i++){
      freqCounter[i]=0;
      rAverage[i]=0.0;
    }
    for(i=0;i<nyquist-1;i++)
      for(j=0;j<nyquist;j++){
        rIndex=ceil(sqrt((double)(i*i+j*j))/(nyquist-1)/freqInc );
        if(rIndex>=nDim) continue;
        // two side average;
        rAverage[rIndex]+=(*(fftSum+i*(tileSize+2)/2+j)+
            *(fftSum+(tileSize-1-i)*(tileSize+2)/2+j) );
        freqCounter[rIndex]+=2;
      }
    //return the PS
    for(i=0;i<nDim;i++) {
      rAverage[i]=rAverage[i]/freqCounter[i];
    }
    stackMean=localMean/counter; //stack is Not noise, set stackMean;
    free(freqCounter);
    free( tile);
    free(fftSum);
    return -1;
  }else{//need to compute mean;
    for(i=0;i<nDim;i++) rAverage[i]=0.1; //set it to 0.1 to make log valid
    tileNum=floor((float)nxx/(float)halfSize);
    xOffset=0;
    for(k=0;k<sliceNum;k++){
      for(i=0;i<tileNum-1;i++)
        for(j=0;j<nyy/halfSize-1;j++){
          counter++;
          sliceTaperInPad(slice[k]->data.f, SLICE_MODE_FLOAT, nxx,
              i*halfSize+xOffset, i*halfSize+xOffset+tileSize-1, j*halfSize,
              j*halfSize+tileSize-1, tile, tileSize+2, tileSize, 
              tileSize, 9, 9);
          tmpMean=0.0;
          for(ii=0;ii<tileSize;ii++)
            for(jj=0;jj<tileSize;jj++)tmpMean+=*(tile+ii*(tileSize+2)+jj);
          tmpMean=tmpMean/(tileSize*tileSize);
          localMean+=tmpMean;
        }
    }//k
    if(counter) stackMean=localMean/counter;
    else{
      printf("No slice is in the angle range. stackMean is set to 500.0\n");
      stackMean=500.0;
    }
    printf("No tile is included. Please change defocus tolerance \
        or angle range. \n");
    free( tile);
    free(fftSum);
    return 0;
  }// else
  free( tile);
  free(fftSum);
}

void MyApp::moreTile()
{
  int halfSize=tileSize/2;
  float *tile=(float *)malloc(tileSize*(tileSize+2)*sizeof(float) );
  int leftCounter, rightCounter;
  int k, ii,jj;
  int idir=0; //FFT transform;
  double *leftFftSum=(double *)malloc(tileSize*((tileSize+2)/2)*sizeof(double));
  double *rightFftSum=(double *)malloc(tileSize*((tileSize+2)/2)*sizeof(double));
  double deltaZ; // in microns;
  float freqInc=1.0/(nDim-1);
  int stripRIndex;
  int *stripCounter=(int*)malloc(nDim*sizeof(int));;
  double *stripAvg=(double*)malloc(nDim*sizeof(double));
  double *nps=(double*)malloc(nDim*sizeof(double));
  double coef;
  double leftMean, rightMean, tmpMean, effectiveDefocus, effectiveZero;
  int plusShift, minusShift, xShift;
  //coefficient for conversion between relative frequency and theta*; 
  //zeroCrossing*coef=theta*
  coef=0.5*defocusFinder.wavelength*defocusFinder.csTwo/pixelSize;

  int tileMax=floor((float)nxx/(float)halfSize);
  double axisXAngle; //tilt axis angle with X coordinate, [0, 2*pi];
  double diagonal=sqrt((double)(nxx*nxx+nyy*nyy));
  double stripPixelNum; 
  double centerX, centerY, r, alpha,d;
  int x, y, scanCounter;
  bool isOnLeft; // is the tile center on the left of the tilt axis;

  double *psRatio=(double*)malloc(nDim*sizeof(double));

  ((Plotter *)mainWidget())->tileButton->setEnabled(false);
  //below set 'rAverage' to be PS ratio, i.e., rAverage[i]=rAverage[i]/nps[i];
  //will be restored at the end of this function;  
  for(ii=0;ii<nDim;ii++){
    psRatio[ii]=lowPs[ii]+(highPs[ii]-lowPs[ii])*(stackMean-lowMean)/
              (highMean-lowMean);
    rAverage[ii]=rAverage[ii]/psRatio[ii];
  }
 
  if(defocusOption && defocusFinder.getDefocus()<0){
    printf("Warning invalid defocus, computation halts\n");
    free(tile);
    free(leftFftSum);
    free(rightFftSum);
    free(stripCounter);
    free(stripAvg);
    free(nps);
    free(psRatio);
    return;
  }
  //defocusFinder.setDefocus(defocusFinder.getExpDefocus());
  tileMax=(tileMax-1)*(tileMax-1);
  
  //tilt axis angle with X coordinate, [0, 2*pi];
  axisXAngle=90+tiltAxisAngle; 
  axisXAngle=axisXAngle*MY_PI/180.0;
  if(axisXAngle<0) axisXAngle=2.0*MY_PI+axisXAngle;

  if(defocusOption) {
    effectiveDefocus=defocusFinder.getDefocus();
    effectiveZero=defocusFinder.getZero();
  } else {
    effectiveDefocus=defocusFinder.getExpDefocus();
    effectiveZero=defocusFinder.getExpZero();
  }  

  int itrNum;
  for(k=0;k<sliceNum;k++){
    itrNum=1;
    //'stripPixelNum' is the strip width in pixels
    if(fabs(tiltAngle[k])>MIN_ANGLE) 
      stripPixelNum=fabs( defocusTol/tan(tiltAngle[k]) )/pixelSize;
    else stripPixelNum=diagonal;
    if(stripPixelNum>diagonal) stripPixelNum=diagonal;
    
    scanCounter=tileIncluded[k];
    while( scanCounter<tileMax ){
      deltaZ=(itrNum+0.5)*(stripPixelNum/2.0)*pixelSize*
             fabs(tan(tiltAngle[k]))/1000.0; // in microns;
      tmpMean=effectiveDefocus+deltaZ;
      tmpMean=tmpMean/defocusFinder.csOne; //convert to Z*;
      tmpMean=sqrt(1.0/tmpMean); // convert to  theta*;
      tmpMean=tmpMean/coef; // highDef is the new zero;
      plusShift=B3DNINT( (effectiveZero-tmpMean)/freqInc );
      tmpMean=effectiveDefocus-deltaZ;
      tmpMean=tmpMean/defocusFinder.csOne; //convert to Z*;
      tmpMean=sqrt(1.0/tmpMean); // convert to  theta*;
      tmpMean=tmpMean/coef; // highDef is the new zero;
      minusShift=B3DNINT( (effectiveZero-tmpMean)/freqInc);
      //printf("Original Zero=%f new zero=%f plusShift=%d minusShift=%d\n", 
      //    defocusFinder.getExpZero(), tmpMean, plusShift, minusShift); 
                         
      for(ii=0;ii<tileSize;ii++)
        for(jj=0;jj<(tileSize+2)/2;jj++){
          *(leftFftSum+ii*(tileSize+2)/2+jj)=0.0;
          *(rightFftSum+ii*(tileSize+2)/2+jj)=0.0;
        }
      leftCounter=0;
      rightCounter=0;
      leftMean=0.0;
      rightMean=0.0;

      for(x=0;x<nxx/halfSize-1;x++)
        for(y=0;y<nyy/halfSize-1;y++){
          centerX=x*halfSize+halfSize-nxx/2;//center coordinate of the tile;
          centerY=y*halfSize+halfSize-nyy/2;
          r=sqrt((double)(centerX*centerX+centerY*centerY));
          alpha=atan2(centerY, centerX);
          if(alpha<0) alpha=2.0*MY_PI+alpha;//convert to [0,2*pi];
          d=r*fabs(sin(alpha-axisXAngle));//distance to the tilt axis in pixels;

          //outside of the strip, continue;
          if(d<itrNum*stripPixelNum/2.0 || d>(itrNum+1)*stripPixelNum/2.0) 
            continue;
          scanCounter++;

          if(axisXAngle<=MY_PI){
            if( alpha>axisXAngle && alpha<axisXAngle+MY_PI) isOnLeft=true;
            else isOnLeft=false;
          }else{
            if( alpha>axisXAngle && alpha<axisXAngle-MY_PI) isOnLeft=true;
            else isOnLeft=false;
          }

          d=d*pixelSize*fabs(tan(tiltAngle[k])); //defocus difference in nm;
          if( isOnLeft && d>leftDefTol ) continue;
          if( !isOnLeft && d>rightDefTol) continue;

          sliceTaperInPad(slice[k]->data.f, SLICE_MODE_FLOAT, nxx, x*halfSize, 
              x*halfSize+tileSize-1, y*halfSize, y*halfSize+tileSize-1,
              tile, tileSize+2, tileSize, tileSize, 9, 9);
          tmpMean=0.0;
          for(ii=0;ii<tileSize;ii++)
            for(jj=0;jj<tileSize;jj++) tmpMean+=*(tile+ii*(tileSize+2)+jj);
          tmpMean=tmpMean/(tileSize*tileSize);
          todfft(tile, &tileSize, &tileSize, &idir);

          if(isOnLeft){
            leftCounter++;
            leftMean+=tmpMean;
            for(ii=0;ii<tileSize;ii++)
              for(jj=0;jj<(tileSize+2)/2;jj++)
                *(leftFftSum+ii*(tileSize+2)/2+jj)+=
                *(tile+ii*(tileSize+2)+2*jj)* (*(tile+ii*(tileSize+2)+2*jj))+ 
                *(tile+ii*(tileSize+2)+2*jj+1)*(*(tile+ii*(tileSize+2)+2*jj+1));
          }else{ //right side;
            rightCounter++;
            rightMean+=tmpMean;
            for(ii=0;ii<tileSize;ii++)
              for(jj=0;jj<(tileSize+2)/2;jj++)
                *(rightFftSum+ii*(tileSize+2)/2+jj)+=
                *(tile+ii*(tileSize+2)+2*jj)* (*(tile+ii*(tileSize+2)+2*jj))+ 
                *(tile+ii*(tileSize+2)+2*jj+1)*(*(tile+ii*(tileSize+2)+2*jj+1));
          }
        } //for x-y loop;

      if(leftCounter){ //left side has tiles included;
        leftMean=leftMean/leftCounter;
        for(ii=0;ii<tileSize;ii++)
          for(jj=0;jj<(tileSize+2)/2;jj++) 
            *(leftFftSum+ii*(tileSize+2)/2+jj)/=leftCounter;

        for(ii=0;ii<nDim;ii++){
          stripCounter[ii]=0;
          stripAvg[ii]=0.0;
        }
        for(ii=0;ii<tileSize/2;ii++)
          for(jj=0;jj<(tileSize+2)/2;jj++){
            stripRIndex=ceil( sqrt((double)(ii*ii+jj*jj))/halfSize/freqInc );
            if(stripRIndex>=nDim) continue;
            stripAvg[stripRIndex]+=
              *(leftFftSum+ii*(tileSize+2)/2+jj)+
              *(leftFftSum+(tileSize-1-ii)*(tileSize+2)/2+jj);
            stripCounter[stripRIndex]+=2;
          }
        for(ii=0;ii<nDim;ii++) {
          stripAvg[ii]=stripAvg[ii]/stripCounter[ii];
          nps[ii]=lowPs[ii]+(highPs[ii]-lowPs[ii])*(leftMean-lowMean)/(highMean-lowMean);
          stripAvg[ii]=stripAvg[ii]/nps[ii];
        }
        if(tiltAngle[k]>=0) xShift=minusShift;
        else xShift=plusShift; 
        for(ii=0;ii<nDim;ii++){
          jj=ii+xShift;
          if(jj<0 || jj>=nDim) continue;
          rAverage[jj]=(totalTileIncluded*rAverage[jj]+leftCounter*
              stripAvg[ii])/(totalTileIncluded+leftCounter);
        }
        totalTileIncluded+=leftCounter;
        tileIncluded[k]+=leftCounter;
      }
      //right side
      if(rightCounter){
        rightMean=rightMean/rightCounter;
        for(ii=0;ii<tileSize;ii++)
          for(jj=0;jj<(tileSize+2)/2;jj++) 
            *(rightFftSum+ii*(tileSize+2)/2+jj)/=rightCounter;

        for(ii=0;ii<nDim;ii++){
          stripCounter[ii]=0;
          stripAvg[ii]=0.0;
        }
        for(ii=0;ii<tileSize/2;ii++)
          for(jj=0;jj<(tileSize+2)/2;jj++){
            stripRIndex=ceil( sqrt((double)(ii*ii+jj*jj))/halfSize/freqInc );
            if(stripRIndex>=nDim) continue;
            stripAvg[stripRIndex]+=
              *(rightFftSum+ii*(tileSize+2)/2+jj)+
                *(rightFftSum+(tileSize-1-ii)*(tileSize+2)/2+jj);
            stripCounter[stripRIndex]+=2;
          }
        for(ii=0;ii<nDim;ii++) {
          stripAvg[ii]=stripAvg[ii]/stripCounter[ii];
          nps[ii]=lowPs[ii]+(highPs[ii]-lowPs[ii])*(rightMean-lowMean)/
            (highMean-lowMean);
          stripAvg[ii]=stripAvg[ii]/nps[ii];
        }
        if(tiltAngle[k]>=0) xShift=plusShift;
        else xShift=minusShift;
        for(ii=0;ii<nDim;ii++){
          jj=ii+xShift;
          if(jj<0 || jj>=nDim) continue;
          rAverage[jj]=(totalTileIncluded*rAverage[jj]+rightCounter*
              stripAvg[ii])/(totalTileIncluded+rightCounter);
        }
        totalTileIncluded+=rightCounter;
        tileIncluded[k]+=rightCounter;
      }
      //printf("scanCounter=%d leftCounter=%d rightCounter=%d tileIncluded[%d]=%d\n",
     // scanCounter, leftCounter, rightCounter, k, tileIncluded[k]);
      itrNum++;
    }//while loop;
    printf("***itrNum=%d deltaZ=%f(microns) scanCounter=%d xShift=%d \
        tileIncluded[%d]=%d \n",
        itrNum, deltaZ, scanCounter, xShift, k, tileIncluded[k] );
  }// the k-th slice

  //Restore rAverage[i] so it stores the PS;
  for(ii=0;ii<nDim;ii++) rAverage[ii]=rAverage[ii]*psRatio[ii];  
  //plot and fit the modified PS; 
  plotFitPS();

  free(tile);
  free(leftFftSum);
  free(rightFftSum);
  free(stripCounter);
  free(stripAvg);
  free(nps);
  free(psRatio);

}  

MyApp::~MyApp()
{
  delete simplexEngine;
  delete linearEngine;
  //if(saveFp) fclose(saveFp);
  for(int k=0;k<MAXSLICENUM;k++) if(slice[k]) sliceFree(slice[k]);
}

void MyApp::rangeChanged(double x1_1, double x1_2, double x2_1, double x2_2)
{
  double inc=1.0/(nDim-1);
  int n1=x1_1*(nDim-1) ;
  int n2=x1_2*(nDim-1) ;
  int start=n1;

  int i;
  CurveData data;
  double *result=(double *)malloc(nDim*sizeof(double) );
  double model_0[2]={0, 1.0};
  //double model_0[4]={0, 0.5, 1.0, 2.0};
  double err;

  x1Idx1=n1; x1Idx2=n2;
  switch(x1MethodIndex){
    case 0:
      if( !linearEngine->computeFitting(result, model_0, 2, n1, n2) ){
        for(i=0;i<nDim;i++){
          data.push_back(i*inc);
          data.push_back(result[i]);
        }
        ( (Plotter *)mainWidget() )->setCurveData(1, data);
      }else  printf("linearEngine error\n");
      break;
    case 1:
      simplexEngine->setRange(n1, n2);
      if( !simplexEngine->computeFitting(result, &err, 0) ){
        for(i=0;i<nDim;i++){
          data.push_back(i*inc );
          data.push_back(result[i]);
        }
        ( (Plotter *)mainWidget() )->setCurveData(1, data);
      }else printf("simplexEngine error\n");
      break;
    default:
      printf("unknown fitting method chosen\n"); 
  }


  //nDim=linearEngine->getDim();
  //inc=1.0/(nDim-1);
  n1= x2_1*(nDim-1) ;
  n2= x2_2*(nDim-1) ;
  CurveData data1;
  double *result1=(double *)malloc(nDim*sizeof(double) );

  x2Idx1=n1; x2Idx2=n2;
  switch(x2MethodIndex){
    case 0:
      if( !linearEngine->computeFitting(result1, model_0, 2, n1, n2) ){
        for(i=0;i<nDim;i++){
          data1.push_back(i*inc );
          data1.push_back(result1[i]);
        }
        ( (Plotter *)mainWidget() )->setCurveData(2, data1);
      }else printf("linearEngine error\n");
      break;
    case 1:
      simplexEngine->setRange(n1, n2);
      if( !simplexEngine->computeFitting(result1, &err, 1) ){
        for(i=0;i<nDim;i++){
          data1.push_back(i*inc );
          data1.push_back(result1[i]);
        }
        ( (Plotter *)mainWidget() )->setCurveData(2, data1);
      }else printf("simplexEngine error\n");
      break;
    default:
      printf("unknown fitting method chosen\n"); 
  }

  double zero;
  double defocus;
  char zeroString[20]="Z: NA ";
  char defocusString[20]="D: NA ";
  if( !defocusFinder.findZero(result, result1, start, (n1+n2)/2, &zero) ){
    sprintf(zeroString, "Z: %4.3f", zero);
    defocusFinder.findDefocus(&defocus);
    sprintf(defocusString, "D: %4.2f", defocus);
  }
  //printf("myApp zeroString=%s \n", zeroString);
  ( (Plotter *)mainWidget() )->zeroLabel->setText(zeroString);
  ( (Plotter *)mainWidget() )->defocusLabel->setText(defocusString);

  free(result);
  free(result1);
}


void MyApp::angleChanged(double lAngle, double hAngle, double expDefocus, 
   double defTol, int tSize, double axisAngle, double leftTol, double rightTol)
{  
  
   defocusTol=defTol;
   leftDefTol=leftTol;
   rightDefTol=rightTol;
   tileSize=tSize;
   tiltAxisAngle=axisAngle;
   defocusFinder.setExpDefocus(expDefocus);
   //If tilt angle range does not change, do not need to reload slices;
   if(lAngle!=lowAngle || hAngle!=highAngle){
     setLowAngle(lAngle);
     setHighAngle(hAngle);
     setSlice();
   }
   computeInitPS();
   //plot and fit the new initial PS;
   if(initialTileOption) moreTile(); //include other tiles and plot;
   else {
     ((Plotter *)mainWidget())->tileButton->setEnabled(true);
     plotFitPS(); // only plot;
   }
}

void MyApp::setInitTileOption(int index){
     ((Plotter *)mainWidget())->tileButton->setEnabled(false);
     initialTileOption=index;
}
