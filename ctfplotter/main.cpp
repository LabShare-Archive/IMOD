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
*  Log at end of file
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
  int numOptions = 20;
  char *options[] = {
    "input:InputStack:FN:", "angleFn:AngleFile:FN:",
    "invert:InvertTiltAngles:B:", "config:ConfigFile:FN:",
    "defFn:DefocusFile:FN:", "aAngle:AxisAngle:F:", "psRes:PSResolution:I:",
    "tileSize:TileSize:I:", "volt:Voltage:I:", "cache:MaxCacheSize :I:",
    "debug:DebugLevel :I:", "cs:SphericalAberration:F:",
    "defTol:DefocusTol:I:", "pixelSize:PixelSize:F:",
    "ampContrast:AmplitudeContrast:F:", "expDef:ExpectedDefocus:F:",
    "leftTol:LeftDefTol:F:", "rightTol:RightDefTol:F:",
    "range:AngleRange:FP:", "param:Parameter:PF:"};

  char *cfgFn, *stackFn, *angleFn, *defFn;
  float tiltAxisAngle;
  int volt, nDim, tileSize, cacheSize;
  float defocusTol, pixelSize, lowAngle, highAngle;
  float expectedDef, leftDefTol, rightDefTol;
  float ampContrast, cs;
  int invertAngles = 0;
  QString noiseCfgDir, noiseFile;

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
  if (PipGetInteger("PSResolution", &nDim))
    exitError("PS Resolution is not specified");
  if (PipGetInteger("TileSize", &tileSize))
    exitError("No TileSize specified");
  if (PipGetFloat("DefocusTol", &defocusTol))
    exitError("No DefousTol specified");
  if (PipGetFloat("PixelSize", &pixelSize))
    exitError("No PixelSize specified");
  if (PipGetFloat("AmplitudeContrast", &ampContrast))
    exitError("No AmplitudeContrast is specified");
  if( PipGetFloat("AxisAngle", &tiltAxisAngle) )
     exitError("No AxisAngle specified"); 
  if(PipGetFloat("ExpectedDefocus", &expectedDef))
    exitError("No expected defocus is specified");
  if(PipGetFloat("LeftDefTol", &leftDefTol))
    exitError("No left defocus  tolerance is specified");
  if(PipGetFloat("RightDefTol", &rightDefTol))
    exitError("No right defocus  tolerance is specified");
  if(PipGetTwoFloats("AngleRange", &lowAngle, &highAngle))
    exitError("No AngleRange specified");
  PipGetBoolean("InvertTiltAngles", &invertAngles);
 
  double *rAvg=(double *)malloc(nDim*sizeof(double));

  ctfHelp = new ImodAssistant("html","IMOD.adp", "ctfguide");
  MyApp app(argc, argv, volt, pixelSize, (double)ampContrast, cs, defFn,
            (int)nDim, hyperRes, (double)defocusTol, tileSize, 
            (double)tiltAxisAngle, -90.0, 90.0, (double)expectedDef, 
            (double)leftDefTol, (double)rightDefTol, cacheSize, invertAngles);
  //set the angle range for noise PS computing;
  app.setPS(rAvg);
  
  QMainWindow mainWin;
  Plotter plotter(&mainWin);
  plotter.setWindowTitle(QObject::tr("CTF Plot"));
  app.setPlotter(&plotter);
  
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
  app.computeInitPS();

  // Removed setting of stack mean and noise files from here

  if (app.defocusFinder.getExpDefocus() <= 0)
    exitError("Invalid expected defocus, it must be >0");

  // Switch to calling routine for zero, scale correctly, go back only 12
  double firstZero, secondZero;
  app.defocusFinder.getTwoZeros(app.defocusFinder.getExpDefocus(), firstZero,
                                secondZero);
  int firstZeroIndex=B3DNINT(firstZero * (nDim - 1));
  app.setX1Range(firstZeroIndex-12, firstZeroIndex-1);
  int secZeroIndex=B3DNINT(secondZero * (nDim - 1));
  app.setX2Range(firstZeroIndex+1, secZeroIndex);
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
  app.exec();
  if (app.getSaveModified()) {
    int retval =   QMessageBox::information
      (0, "Save File?", "There are unsaved changes to the defocus table - "
       "save before exiting?", QMessageBox::Yes, QMessageBox::No,
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

/*

$Log$
Revision 1.17  2010/03/14 19:32:57  mast
Ask about writing defocus file before exiting

Revision 1.16  2010/03/09 06:24:22  mast
Allow use of relative paths in config file

Revision 1.15  2009/09/18 14:56:13  mast
Changed to skip blank lines in noise file

Revision 1.14  2009/08/10 22:14:59  mast
Adjust to changes in other modules, add inversion option

Revision 1.13  2009/01/15 16:31:36  mast
Qt 4 port

Revision 1.12  2008/11/11 16:19:20  xiongq
delete qsplashscreen.h

Revision 1.11  2008/11/10 22:43:44  xiongq
improved splash display

Revision 1.10  2008/11/08 21:54:04  xiongq
adjust plotter setting for initializaion

Revision 1.9  2008/11/07 20:34:34  xiongq
call fflush to sync log  for each slice

Revision 1.8  2008/11/07 20:20:41  xiongq
add splash screen

Revision 1.7  2008/11/07 17:26:24  xiongq
add the copyright heading

*/
