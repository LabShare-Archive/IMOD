/*
 *  ctfphaseflip.cpp  -  CTF correction of tilted images
 *
 *  Author: Quanren Xiong
 *
 *  Copyright (C) 2007-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <limits>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "b3dutil.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include "sliceproc.h"
#include "cfft.h"
#include "parse_params.h"

#define MIN_ANGLE 1.0e-6  //tilt Angle less than this is treated as 0.0;
#define MY_PI 3.1415926
#define UNUSED_DEFOCUS -2000000.0

using namespace std;

int main(int argc, char *argv[])
{
  int numOptArgs, numNonOptArgs;

  // Fallbacks from   ../manpages/autodoc2man 2 1 ctfphaseflip
  int numOptions = 16;
  char *options[] = {
    "input:InputStack:FN:", "output:OutputFileName:FN:",
    "angleFn:AngleFile:FN:", "invert:InvertTiltAngles:B:",
    "defFn:DefocusFile:FN:", "defTol:DefocusTol:I:",
    "iWidth:InterpolationWidth:I:", "pixelSize:PixelSize:F:",
    "volt:Voltage:I:", "cs:SphericalAberration:F:",
    "ampContrast:AmplitudeContrast:F:", "views:StartingEndingViews:IP:",
    "totalViews:TotalViews:IP:", "boundary:BoundaryInfoFile:FN:",
    "aAngle:AxisAngle:F:", "param:Parameter:PF:"};

  char *stackFn, *angleFn, *outFn, *defFn;
  char *boundFn = NULL;
  int volt, iWidth, defocusTol;
  float tiltAxisAngle, pixelSize, cs, ampContrast, stripDefocus;
  int startingView, endingView, startingTotal, endingTotal;
  bool isSingleRun=false;
  int invertAngles = 0;
  double angleSign;
  char *progname = imodProgName(argv[0]);

  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
      1, 0, 0, &numOptArgs, &numNonOptArgs, NULL);

  if (PipGetString("InputStack", &stackFn))
    exitError("No stack specified");
  if (PipGetString("AngleFile", &angleFn))
  {
    angleFn=NULL;
    printf("No angle file is specified, tilt angle is assumed to be 0.0\n");
  }
  if( PipGetString("DefocusFile", &defFn) )
    exitError("No defocus file is specified");
  //if( PipGetFloat("AxisAngle", &tiltAxisAngle) )
  //  exitError("No AxisAngle specified"); 
  if (PipGetInteger("DefocusTol", &defocusTol))
    exitError("No DefousTol specified");
  if (PipGetInteger("InterpolationWidth", &iWidth) )
    exitError("No interpolationWidth specified");
  if (PipGetFloat("PixelSize", &pixelSize))
    exitError("No PixelSize specified");
  if (PipGetInteger("Voltage", &volt))
    exitError("Voltage is not specified");
  if( PipGetFloat("SphericalAberration",&cs) )
    exitError("SphericalAberration is not specified");
  if (PipGetFloat("AmplitudeContrast", &ampContrast))
    exitError("No AmplitudeContrast is specified");
  if( PipGetTwoIntegers("TotalViews", &startingTotal, &endingTotal) )
    isSingleRun=true; // TotalViews is not specified;
  if( PipGetString("OutputFileName", &outFn) )
    exitError("OutputFileName is not specified");
  PipGetString("BoundaryInfoFile", &boundFn);
  PipGetBoolean("InvertTiltAngles", &invertAngles);
  angleSign = invertAngles ? -1. : 1.;
  
  printf("stackFn=%s, angleFn=%s,  invertAngles=%d\n", stackFn, angleFn, 
         invertAngles);
  printf("volt=%d Kv, interpolationWidth=%d pixels, defocusTol=%d nm \n", 
      volt, iWidth, defocusTol);
  printf("tiltAxisAngle=%f, pixelSize=%f nm, cs=%f mm, ampContrast=%f, \
   \n", tiltAxisAngle, pixelSize, cs, ampContrast);

  FILE *fpStack, *fpDef, *fpAngle=NULL;
  if( (fpStack=fopen(stackFn, "rb"))==0 )
    exitError("could not open input file %s", stackFn);
  if( (fpDef=fopen(defFn, "r"))==0 )
    exitError("could not open defocus file %s", defFn);

 
  FILE *foutput;

  MrcHeader header;
  MrcHeader outHeader;
  int sliceMode;
  /* read header */	
  if (mrc_head_read(fpStack, &header)) 
    exitError("reading header of input file %s",  stackFn);
  
  if (mrc_head_read(fpStack, &outHeader)) 
    exitError("reading header of input file %s",  stackFn);

   if( PipGetTwoIntegers("StartingEndingViews", &startingView, &endingView) )
   {//not specified, set to defaults
     startingView=1;
     endingView=header.nz;
   }
  /*  Check if it is the correct data type and set slice type
  if (header.mode == MRC_MODE_BYTE)
    sliceMode = SLICE_MODE_BYTE;
  else if (header.mode == MRC_MODE_SHORT)
    sliceMode = SLICE_MODE_SHORT;
  else if (header.mode == MRC_MODE_FLOAT)
    sliceMode = SLICE_MODE_FLOAT;
  else if(header.mode == MRC_MODE_USHORT)
    sliceMode = SLICE_MODE_USHORT;
  else 
    exitError("File mode is %d; only byte, short,integer allowed\n", 
        header.mode);
  */
  sliceMode=sliceModeIfReal(header.mode);
  if (sliceMode < 0)
    exitError("File mode is %d; only byte, short integer, or real allowed",
              header.mode);

  //The number of slices this run deals with;
  int currNz=endingView-startingView+1;
  if(isSingleRun){
    outHeader.nz=currNz;
    outHeader.mz=currNz;
  } else {
    outHeader.nz=endingTotal-startingTotal+1;
    outHeader.mz=endingTotal-startingTotal+1;
  }
  outHeader.zlen=header.zlen*outHeader.nz/header.nz;
  outHeader.next=0;
  outHeader.headerSize=1024;
  outHeader.swapped=0;
  mrc_head_label(&outHeader, "ctfPhaseFlip: CTF correction "
                 "with phase flipping only");
  
  if( (startingView==-1 && endingView==-1) || isSingleRun ){
      imodBackupFile(outFn);
      foutput=fopen(outFn,"wb");
  }else foutput=fopen(outFn,"r+b");
  if(!foutput) exitError("fopen() failed to open %s", outFn);

  if( startingView==-1 && endingView==-1 && !isSingleRun){
      if( mrc_head_write(foutput, &outHeader) )
          exitError("Error when write out header");
      fclose(fpStack);
      fclose(foutput);
      return 0;
  }

  if(angleFn){
    if( (fpAngle=fopen(angleFn, "r"))==0 ){
      printf("could not open angle file %s, tiltAngle is set to 0.0\n",
             angleFn);
    }
  }

  int err = parWrtInitialize(boundFn, header.nx, header.ny);
  if (err)
    exitError("Initializing parallel writing with boundary info file %s "
              "(error %d)", boundFn, err);
    
  int nx = header.nx;
  int ny = header.ny;
  int nz = header.nz;
  float currAngle;
  Islice *currSlice;
  char angleStr[64], defStr[100];
  int stripPixelNum, interPixelNum;
  int k, row, column;
  int stripIdx, fy, fyy,  fx;
  int dir=0;
  int idir=1;
  float WL, C1, C2, f2, ctf, freq_scalex, freq_scaley;
  float waveAberration, sign;

  //Get detected defocus for each slice;
  int beginNum, endNum;
  float beginAngle, endAngle, rangeDefocus;
  float *defocus=(float *)malloc(nz*sizeof(float));
  
  //sets to UNUSED_DEFOCUS since ctfplotter saves defocus with that value
  // when defocus is not computed.
  for(k=0;k<nz;k++) defocus[k]=UNUSED_DEFOCUS;
  while( fgets(defStr, 100, fpDef) ) {
      sscanf(defStr, "%d%d%f%f%f", &beginNum, &endNum, &beginAngle, 
          &endAngle, &rangeDefocus);
      k=(beginNum+endNum)/2-1;
      if( k<0 || k>=nz) exitError("slice numbers are out of range");
      //convert to microns;
      defocus[k]=rangeDefocus/1000.0;
      //printf("beginNum=%d endNum=%d k=%d defocus=%f\n", beginNum, endNum, k,\
      //defocus[k]);
  }

  //defocus interpolation; 
   int first=-1,second=0; 
   for(k=0;k<nz;k++){
     if(defocus[k]==UNUSED_DEFOCUS) continue;
     second=k;
     if(first==-1){
       for(row=0;row<second;row++) defocus[row]=defocus[second];
     }else{
       for(row=first+1;row<second;row++)
         defocus[row]=((row-first)*defocus[second]+(second-row)*defocus[first])
           /(float)(second-first);
     }
     first=second;
   }

   for(k=nz-1;k>=0;k--)
     if(defocus[k]==UNUSED_DEFOCUS) defocus[k]=defocus[second];
     else break;

   for(k=0;k<nz;k++)
    printf("defocus[%d]=%f microns\n",k ,defocus[k]); 

  WL=12.3/sqrt(volt*1000.0+volt*volt); //wavelength;
  C1=MY_PI*WL;
  C2=-C1*cs*1000000.0*WL*WL/2.0;


  int stripDist[2];
  float *restoredArray;
  double meanSum=0.0;
  double amin = 0.1 * numeric_limits<double>::max();
  double amax = -amin;

  Islice *outSlice;

  //skip the tilt angles before the starting view;
  if(fpAngle){
    for( k=0;k<startingView-1;k++) fgets(angleStr, 30, fpAngle);
  }


  int currK=0;
  if(!isSingleRun) currK=startingView-startingTotal;
  fflush(stdout);

  for(k=startingView;k<=endingView;k++){
    if( fpAngle && fgets(angleStr, 30, fpAngle) ){
      sscanf(angleStr, "%f", &currAngle);
      currAngle *= angleSign;
      printf("Slice %d, tilt angle is %f degrees. \n", k, currAngle);
    }else{
      currAngle=0.0;
      printf("No angle is specified, set to 0.0\n");
    }

    if( defocus[k-1]==UNUSED_DEFOCUS )
      exitError("specified defocus is wrong for slice %d", k);

    currSlice=sliceCreate(nx, ny, sliceMode);
    outSlice=sliceCreate(nx, ny, SLICE_MODE_FLOAT);
    if(!currSlice || !outSlice)
      exitError("creating outslice or currSlice");
    restoredArray=outSlice->data.f;

    //startingView starts at 1, the API starts 0;
    if( mrc_read_slice(currSlice->data.b, fpStack, &header, k-1, 'Z') )
      exitError("reading slice");
    printf("Slice %d of stack %s is included\n", k, stackFn);

    //convert slice to floats
    if(sliceMode !=SLICE_MODE_FLOAT)
      if( sliceNewMode(currSlice, SLICE_MODE_FLOAT)<0 )
       exitError("converting slice to float");

    currAngle=currAngle*MY_PI/180.0; 
    if( fabs(currAngle)>MIN_ANGLE ) {
      stripPixelNum=fabs(defocusTol/tan(currAngle))/pixelSize;
      interPixelNum=fabs(iWidth/tan(currAngle))/pixelSize;
    } else {
      stripPixelNum=nx;
      interPixelNum=nx;
    }
    if( stripPixelNum>nx) stripPixelNum=nx;
    if( interPixelNum>stripPixelNum) interPixelNum=stripPixelNum; 
    
    stripPixelNum=niceFrame(stripPixelNum, 2 , 19);
    if(stripPixelNum>256) stripPixelNum=256;
    if(stripPixelNum<128) stripPixelNum=128;
    interPixelNum=iWidth;
        
    //stripPixelNum=256;
    //interPixelNum=36; // must be less than stripPixelNum/2;
    
    //interPixelNum must be less than stripPixelNum/2;
    if( interPixelNum>=stripPixelNum/2) 
      exitError("interPixelNum is bigger than stripPixleNum/2 ");

    printf("stripPixelNum=%d interPixelNum=%d \n", stripPixelNum,
        interPixelNum);
    //Allocat 2 strips, even and odd strip;
    float *strip=(float *)malloc(2*ny*(stripPixelNum+2)*sizeof(float));
    bool finished=false;
    int stripBegin;
    int stripEnd;
    int stripStride;
    
    // convert pixelSize to Angston;
    freq_scalex=1.0/(pixelSize*10.0*stripPixelNum);
    freq_scaley=1.0/(pixelSize*10.0*ny);
    stripIdx=0;
    while(!finished){
      
      if(stripIdx*interPixelNum+stripPixelNum-1<nx){
        stripBegin=stripIdx*interPixelNum;
        stripEnd=stripBegin+stripPixelNum-1;
        stripStride=interPixelNum;
      }else{
        stripStride=nx-(stripPixelNum+1)/2-(stripBegin+stripEnd)/2;
        stripBegin=nx-stripPixelNum;
        stripEnd=nx-1;
        finished=true;
      }
      //printf("stripIdx=%d stripBegin=%d stripEnd=%d \n", 
      //stripIdx, stripBegin, stripEnd); 
      
      stripDefocus=defocus[k-1]*1000.0 - ( nx/2-(stripBegin+stripEnd)/2 )*
        tan(currAngle)*pixelSize; //in nm;
      //printf("defocus is %6.1f\n", stripDefocus);

        sliceTaperInPad(currSlice->data.f, SLICE_MODE_FLOAT, nx, stripBegin, 
            stripEnd, 0, ny-1, strip+(stripIdx%2)*ny*(stripPixelNum+2), 
            stripPixelNum+2, stripPixelNum, ny, 9, 9);
        todfft(strip+(stripIdx%2)*ny*(stripPixelNum+2),&stripPixelNum,&ny,&dir);

        //fipping the phase;
        for(fy=0;fy<ny;fy++){
          fyy=fy;
          if(fy>ny/2) fyy-=ny;
          for(fx=0;fx<(stripPixelNum+2)/2;fx++){
            f2=fx*fx*freq_scalex*freq_scalex + fyy*fyy*freq_scaley*freq_scaley;
            //convert defocus to Angston;
            waveAberration=(C1*stripDefocus*10.0+C2*f2)*f2;
            ctf=-(sqrt(1-ampContrast*ampContrast))*sin(waveAberration)
              -ampContrast*cos(waveAberration);
            if( ctf <0) sign=1.0;
            else sign=-1.0; 
            //strip[stripIdx%2][fy][2*fx]*=sign;
            *(strip+(stripIdx%2)*ny*(stripPixelNum+2)+
                fy*(stripPixelNum+2)+2*fx)*=sign;
            //strip[stripIdx%2][fy][2*fx+1]*=sign;
            *(strip+(stripIdx%2)*ny*(stripPixelNum+2)+
                fy*(stripPixelNum+2)+2*fx+1)*=sign;
          }
        }
        //inverse FFT;
      todfft(strip+(stripIdx%2)*ny*(stripPixelNum+2),&stripPixelNum,&ny,&idir);

      if(stripIdx==0){ //The starting strip needs special handling;
        for( row=0;row<ny;row++)
          for(column=0;column<stripPixelNum/2;column++)
           *(restoredArray+row*nx+column)=*(strip+row*(stripPixelNum+2)+column);
        //printf("column=1 ... %d \n", stripPixelNum/2);
      }else{
          for( row=0;row<ny;row++)
            for(column=(stripBegin+stripEnd)/2-stripStride+1;
                column<(stripBegin+stripEnd)/2+1;column++)
            {
              stripDist[0]=column-(stripBegin+stripEnd)/2+stripStride-1;
              stripDist[1]=(stripBegin+stripEnd)/2+1-column;
              *(restoredArray+row*nx+column)     =   (
                                                      stripDist[0]*(  
                                                     *(strip+
                           (stripIdx%2)*ny*(stripPixelNum+2)+
                                       row*(stripPixelNum+2)+
                                stripPixelNum/2-stripDist[1])      
                                                                     ) + 
                                                       stripDist[1]*(
                                                      *(strip+
                        ((stripIdx-1)%2)*ny*(stripPixelNum+2)+
                                        row*(stripPixelNum+2)+
                                  stripPixelNum/2+ stripDist[0]) 
                                                                    )
                                                    )
                /(float)stripStride;
            }
         // printf("column=%d ... %d \n", (stripBegin+stripEnd)/2-
          // stripStride+1+1, (stripBegin+stripEnd)/2+1 );
      }

      if(finished){ //last strip
        //printf("finished: starting column=%d \n", (stripBegin+stripEnd)/2+1 );

        for( row=0;row<ny;row++)
            for(column=(stripBegin+stripEnd)/2+1;column<nx;column++)
              *(restoredArray+row*nx+column)  = *(strip+
                      (stripIdx%2)*ny*(stripPixelNum+2)+
                                  row*(stripPixelNum+2)+
        stripPixelNum/2+column-(stripBegin+stripEnd)/2-1);
        //printf("column=%d ... %d \n", (stripBegin+stripEnd)/2+1+1, nx);
      }

      //printf("stripIdx=%d\n", stripIdx);
      stripIdx++;
    }//while strip loop

    free(strip);
    sliceMMM(outSlice);
    if( outSlice->min < amin) amin=outSlice->min;
    if( outSlice->max > amax) amax=outSlice->max;
    meanSum+=outSlice->mean;
     if(sliceMode !=SLICE_MODE_FLOAT)
      if( sliceNewMode(outSlice, sliceMode)<0 )
        exitError("converting slice to original mode");

    if( parallelWriteSlice(outSlice->data.b, foutput, &outHeader, currK) )
      exitError("Writing slice %d error", currK);
    currK++;
    sliceFree(currSlice);
    sliceFree(outSlice);
    fflush(stdout);
  }//k slice
  
  if(isSingleRun){
    outHeader.amin=amin;
    outHeader.amax=amax;
    outHeader.amean=meanSum/(double)currNz;
    if ( mrc_head_write(foutput, &outHeader) )
      exitError("Writing slice header error");
  }else{//for collectmmm
    printf("min, max, mean, # pixels= %f  %f  %f %d \n", 
        amin, amax, meanSum/(double)currNz, nx*ny*currNz);
  }
  fclose(foutput);
  fclose(fpStack);
  free(defocus);
  if(fpAngle) fclose(fpAngle);
}

/*

$Log$
Revision 3.13  2009/03/24 00:37:00  mast
Fixed initialization of max at 0

Revision 3.12  2009/02/16 06:22:48  mast
Modified to use new parallel write stuff

Revision 3.11  2008/11/20 01:32:31  mast
Restored \n on 3 printf's including critical min/max/mean statement

Revision 3.10  2008/11/07 22:25:08  xiongq
add fflush calls

Revision 3.9  2008/09/04 15:21:59  mast
Fixed program name in call to PIP and fallback options


*/
