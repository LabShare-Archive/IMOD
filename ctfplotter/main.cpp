/*
* main.cpp - main function of ctfplotter, a GUI program for estimating the first
*            CTF zero of a tilt series.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*/

#include <math.h>
#include <stdio.h>
#include <QtGui>

#include "simplexfitting.h"
#include "linearfitting.h"
#include "plotter.h"
#include "imod_assistant.h"
#include "myapp.h"

#include "b3dutil.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include "cfft.h"
#include "parse_params.h"

#define CACHESIZE 1000   // max cache size in Megs;
#define MIN_ANGLE 1.0e-6  //tilt Angle less than this is treated as 0.0;
ImodAssistant *ctfHelp=NULL;
int debugLevel;

int main(int argc, char *argv[])
{
  // Fallbacks from   ../manpages/autodoc2man 2 1 ctfplotter
  int numOptArgs, numNonOptArgs;
  int numOptions = 21;
  char *options[] = {
    "input:InputStack:FN:", "angleFn:AngleFile:FN:", "invert:InvertTiltAngles:B:",
    "offset:OffsetToAdd:F:", "config:ConfigFile:FN:", "defFn:DefocusFile:FN:",
    "aAngle:AxisAngle:F:", "psRes:PSResolution:I:", "tileSize:TileSize:I:",
    "volt:Voltage:I:", "cache:MaxCacheSize :I:", "cs:SphericalAberration:F:",
    "defTol:DefocusTol:I:", "pixelSize:PixelSize:F:",
    "ampContrast:AmplitudeContrast:F:", "expDef:ExpectedDefocus:F:",
    "leftTol:LeftDefTol:F:", "rightTol:RightDefTol:F:", "range:AngleRange:FP:",
    "debug:DebugLevel :I:", "param:Parameter:PF:"};

  char *cfgFn, *stackFn, *angleFn, *defFn;
  float tiltAxisAngle;
  int volt, nDim, tileSize, cacheSize, ifAutofit, ifAngleRange, ifFreqRange;
  float defocusTol, pixelSize, lowAngle, highAngle, autoRange, autoStep;
  float autoFromAngle, autoToAngle, x1Start, x2End;
  float expectedDef, leftDefTol, rightDefTol;
  float ampContrast, cs, dataOffset = 0.;
  int invertAngles = 0, ifOffset = 0, saveAndExit = 0, varyExp = 0;
  QString noiseCfgDir, noiseFile;
  SliceCache *cache;
  MrcHeader *header;

  // Set some sensible defaults
  defocusTol = 200;
  leftDefTol = rightDefTol = 2000;
  ampContrast = 0.07;
  nDim = 101;
  tileSize = 256;

  // These are used for a pointless initial fit if doing auto and no range entered
  lowAngle = -10.;
  highAngle = 10.;

  // This amount of hyperresolution in the stored PS gave < 0.1% difference
  // in the final PS from ones computed directly from summed FFTs for 
  // frequencies higher than 0.2.
  int hyperRes = 20;

  //PipReadOrParseOptions(argc, argv, options, numOptions, argv[0], 
  PipReadOrParseOptions(argc, argv, options, numOptions, "ctfplotter", 
      1, 0, 0, &numOptArgs, &numNonOptArgs, NULL);

  if (PipGetInteger("DebugLevel", &debugLevel) )
    debugLevel=1;

  if (PipGetString("ConfigFile", &cfgFn))
    exitError("No Config file specified");
  if (PipGetString("InputStack", &stackFn))
    exitError("No stack specified");
  if (PipGetString("AngleFile", &angleFn))
  {
    angleFn=NULL;
    if( debugLevel>=1)
     printf("No angle file is specified, tilt angle is assumed to be 0.0\n");
  }
  if ( PipGetString("DefocusFile", &defFn) )
    exitError("output defocus file is not specified ");
  if (PipGetInteger("Voltage", &volt))
    exitError("Voltage is not specified");
  if (PipGetInteger("MaxCacheSize", &cacheSize) ){
     if( debugLevel>=1)
      printf("No MaxCacheSize is specified, set to default %d Megs\n",
          CACHESIZE);
     cacheSize=CACHESIZE; 
  }else{
    if( debugLevel>=1)
     printf(" MaxCacheSize is set to %d \n", cacheSize);
  }
  if (PipGetFloat("SphericalAberration", &cs) )
      exitError("Spherical Aberration is not specified");
  PipGetInteger("PSResolution", &nDim);
  PipGetInteger("TileSize", &tileSize);
  PipGetFloat("DefocusTol", &defocusTol);
  if (PipGetFloat("PixelSize", &pixelSize))
    exitError("No PixelSize specified");
  PipGetFloat("AmplitudeContrast", &ampContrast);
  if( PipGetFloat("AxisAngle", &tiltAxisAngle) )
     exitError("No AxisAngle specified"); 
  if(PipGetFloat("ExpectedDefocus", &expectedDef))
    exitError("No expected defocus is specified");
  PipGetFloat("LeftDefTol", &leftDefTol);
  PipGetFloat("RightDefTol", &rightDefTol);
  ifAutofit = 1 - PipGetTwoFloats("AutoFitRangeAndStep", &autoRange, &autoStep);
  if (ifAutofit) {
    ifAngleRange = 1 - PipGetTwoFloats("AngleRange", &autoFromAngle, &autoToAngle);
  } else {
    if (PipGetTwoFloats("AngleRange", &lowAngle, &highAngle))
      exitError("No AngleRange specified");
    autoStep = (float)fabs((double)highAngle - lowAngle) / 2.f;
  }
  PipGetBoolean("InvertTiltAngles", &invertAngles);
  ifOffset = 1 - PipGetFloat("OffsetToAdd", &dataOffset);
  ifFreqRange = 1 - PipGetTwoFloats("FrequencyRangeToFit", &x1Start, &x2End);
  if (ifFreqRange && (x1Start < 0.01 || x2End > 0.48 || x2End - x1Start < 0.03))
    exitError("Fitting range values are too extreme, out of order, or too close together"
              );
  PipGetBoolean("SaveAndExit", &saveAndExit);
  PipGetBoolean("VaryExponentInFit", &varyExp);
 
  double *rAvg=(double *)malloc(nDim*sizeof(double));

  ctfHelp = new ImodAssistant("html","IMOD.adp", "ctfguide");
  MyApp app(argc, argv, volt, pixelSize, (double)ampContrast, cs, defFn,
            (int)nDim, hyperRes, (double)defocusTol, tileSize, 
            (double)tiltAxisAngle, -90.0, 90.0, (double)expectedDef, 
            (double)leftDefTol, (double)rightDefTol, cacheSize, invertAngles);
  //set the angle range for noise PS computing;
  app.setPS(rAvg);
  app.setRangeStep((double)autoStep);
  app.setSaveAndExit(ifAutofit != 0 && saveAndExit != 0);
  app.setVaryCtfPowerInFit(varyExp != 0);
  
  QMainWindow mainWin;
  Plotter plotter(&mainWin);
  plotter.setWindowTitle(QObject::tr("CTF Plot"));
  app.setPlotter(&plotter);
  if (ifAutofit)
    app.setInitTileOption(1);
  
  /*****begin of computing noise PS;**********/
  FILE *fpCfg;
  if( (fpCfg=fopen(cfgFn, "r"))==0 )
    exitError(" could not open config file %s", cfgFn);
  char p[1024];
  int read;
  int noiseFileCounter=0;

  // It could be just a label, but this makes it bigger and lets the whole
  // title show up
  QWidget *splash= new QWidget();
  QVBoxLayout *layout = new QVBoxLayout;
  splash->setLayout(layout);
  layout->setMargin(30);
  QLabel *label = new QLabel("Loading noise files ...", splash);
  layout->addWidget(label);
  splash->setWindowTitle("ctfplotter");
  QSize hint = splash->sizeHint();
  splash->move(QApplication::desktop()->width() / 2 - hint.width() / 2,
               QApplication::desktop()->height() / 2 - hint.height() / 2);
  splash->show();

  // At least 4 of these are needed to make it work reliably!
  qApp->flush();
  qApp->syncX();
  qApp->processEvents();
  splash->repaint(0,0,-1,-1);
  qApp->processEvents();
 
  // only to find how many noise files are provided;
  while( (read=fgetline(fpCfg, p, 1024)) >= 0 ) 
    if (read)
      noiseFileCounter++;
  rewind(fpCfg);
  if(debugLevel>=1)
   printf("There are %d noise files specified\n", noiseFileCounter);
  if (noiseFileCounter < 2)
    exitError("There must be at least two noise files");
  
  double *noisePs=(double *)malloc( noiseFileCounter*nDim*sizeof(double));
  double *noiseMean=(double *)malloc( noiseFileCounter*sizeof(double) );
  double *currPS;
  int *index=(int *)malloc(noiseFileCounter*sizeof(double) );
  int i, j;

  fflush(stdout);
  
  noiseCfgDir = QDir::fromNativeSeparators(QString(cfgFn));
  printf("noiseCfgDir read in: %s\n", (const char *)noiseCfgDir.toLatin1());
  i = noiseCfgDir.lastIndexOf('/');
  if (i >= 0)
    noiseCfgDir = noiseCfgDir.left(i + 1);
  else
    noiseCfgDir = "./";
  printf("i  %d noiseCfgDir used: %s\n", i, (const char *)noiseCfgDir.toLatin1());
  noiseFileCounter=0;
  while ((read=fgetline(fpCfg, p, 1024)) >= 0) {
    if (!read)
      continue;
    //if(p[read-1]=='\n') p[read-1]='\0';  //remove '\n' at the end;
    noiseFile = QString(p);
    if (QDir::isRelativePath(noiseFile))
      noiseFile = noiseCfgDir + noiseFile;
    app.setSlice((const char *)noiseFile.toLatin1(), NULL);
    app.computeInitPS();
    currPS=app.getPS();
    //for(i=0;i<nDim;i++) noisePs[noiseFileCounter][i]=*(currPS+i);
    for (i = 0; i < nDim; i++)
      noisePs[noiseFileCounter*nDim + i] = currPS[i];
    noiseMean[noiseFileCounter]=app.getStackMean();
    if (noiseMean[noiseFileCounter] < 0.)
      exitError("The mean of noise file %d is negative.  All noise files must have "
                "positive means, with 0 mean corresponding to 0 exposure",
                noiseFileCounter + 1);
    noiseFileCounter++;
  }
  for(i=0;i<noiseFileCounter;i++){
    if( debugLevel>=1)
      printf("noiseMean[%d]=%f\n", i, noiseMean[i]);
    index[i]=i;
  }
  fflush(stdout);

  //sorting;
  double tempMean;
  double tempIndex;
  for(i=0;i<noiseFileCounter;i++)
    for(j=i+1;j<noiseFileCounter;j++)
      if( noiseMean[i]>noiseMean[j]){
         tempMean=noiseMean[i];
         noiseMean[i]=noiseMean[j];
         noiseMean[j]=tempMean;

         tempIndex=index[i];
         index[i]=index[j];
         index[j]=tempIndex;
      }
  app.setNumNoiseFiles(noiseFileCounter);
  app.setNoiseIndexes(index);
  app.setNoiseMeans(noiseMean);
  app.setAllNoisePS(noisePs);
  /****end of computing noise PS; ******/
  
  label->setText("Loading slices ...");
  qApp->processEvents();
  app.setLowAngle(lowAngle);
  app.setHighAngle(highAngle);
  app.setSlice(stackFn, angleFn);

  // If doing auto and range was entered, now fix the auto from and to angles
  if (ifAutofit && ifAngleRange) {
    app.setAutoFromAngle((double)autoFromAngle);
    app.setAutoToAngle((double)autoToAngle);
  }

  // Now determine and set data offset if necessary
  cache = app.getCache();
  if (!ifOffset) {
    header = cache->getHeader();
    if (strstr(header->labels[0], "Fei Company")) {
      if (header->amin < 0.) {
        ifOffset = 1;
      } else {
        QMessageBox::warning (0, "Warning: Data Offset May Be Needed", 
           "The image stack originated from FEI software,\nbut the usual offset of "
           "32768 is not being added\nbecause the minimum of the file is positive.\n\n"
           "You may need to specify an offset to make the\n"
           "values be proportional to recorded electrons.",
           QMessageBox::Ok, QMessageBox::Ok);
        qApp->processEvents();
      }
    } else {
      if (header->amin < -20000 && header->amean < -1000) {
        ifOffset = 1;
        QMessageBox::warning (0, "Warning: Data Offset Assumed", 
           "The image stack contains negative values\nunder -20000 and has a negative "
           "mean,\nso an offset of 32768 is being added.\n\n"
           "You may need to specify a different offset to make\n"
           "the values be proportional to recorded electrons.",
           QMessageBox::Ok, QMessageBox::Ok);
        qApp->processEvents();
      } else if (header->amean < 0)
        exitError("The mean of the input stack is negative.  You need to specify an "
                  "offset to add to make values be proportional to recorded electrons");
    }
    if (ifOffset)
      dataOffset = 32768;
  }
  if (ifOffset)
    cache->setDataOffset(dataOffset);
  app.computeInitPS();

  // Removed setting of stack mean and noise files from here

  if (app.defocusFinder.getExpDefocus() <= 0)
    exitError("Invalid expected defocus, it must be >0");

  // Switch to calling routine for zero, scale correctly, go back only 12
  double firstZero, secondZero;
  app.defocusFinder.getTwoZeros(app.defocusFinder.getExpDefocus(), firstZero,
                                secondZero);
  int secZeroIndex=B3DMIN(nDim - 1, B3DNINT(secondZero * (nDim - 1)));
  int firstZeroIndex=B3DMIN(secZeroIndex - 3, B3DNINT(firstZero * (nDim - 1)));
  if (ifFreqRange) {
    app.setX1Range(B3DNINT(2. * x1Start * (nDim - 1)), firstZeroIndex-1);
    app.setX2Range(firstZeroIndex+1, B3DNINT(2. * x2End * (nDim - 1)));
  } else {
    app.setX1Range(firstZeroIndex-12, firstZeroIndex-1);
    app.setX2Range(firstZeroIndex+1, secZeroIndex);
  }
  app.simplexEngine=new SimplexFitting(nDim);
  app.linearEngine=new LinearFitting(nDim);
  app.plotFitPS(true); //fit and plot the stack PS;

  fflush(stdout);

  delete splash;
  mainWin.setCentralWidget(&plotter);
  mainWin.resize(768, 624);
  mainWin.statusBar()->showMessage(QObject::tr("Ready"), 2000);

  mainWin.show();
  plotter.angleDiag();
  if (ifAutofit) {
    app.autoFitToRanges(app.getAutoFromAngle(), app.getAutoToAngle(), autoRange, autoStep,
                        3);
    if (saveAndExit) {
      app.writeDefocusFile();
      exit(0);
    }
  }
  app.exec();
  if (app.getSaveModified()) {
    int retval =   QMessageBox::information
      (0, "Save File?", "There are unsaved changes to the defocus table - "
       "save before exiting?", QMessageBox::Yes | QMessageBox::No,
       QMessageBox::NoButton);
    if (retval == QMessageBox::Yes)
      app.writeDefocusFile();
  }
  free(rAvg);
  free(noisePs);
  free(noiseMean);
  free(index);
}

int ctfShowHelpPage(const char *page)
{
  if(ctfHelp)
    return (ctfHelp->showPage(page)>0 ? 1 : 0);
  else
    return 1;
}

