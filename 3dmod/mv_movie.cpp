/*  mv_movie.cpp -- Movie creation dialog for imodv.
 *
 *  Original Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qapplication.h>
#include <QTime>
#include "formv_movie.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "imodview.h"
#include "b3dgfx.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "mv_movie.h"
#include "mv_image.h"
#include "mv_objed.h"
#include "mv_views.h"
#include "mv_window.h"
#include "mv_modeled.h"
#include "formv_sequence.h"
#include "preferences.h"
#include "control.h"
#include "scalebar.h"

// Static variables with new naming convention
static imodvMovieForm *sDia = NULL;
static MovieSequenceForm *sSequenceDia = NULL;
static ImodvApp  *sApp = NULL;
static int sSaved = 0;
static int sReverse = 0;
static int sLongway = 0;
static int sMontage = 0;
static int sFile_format = 0;
static int sFullaxis = 0;
static int sAbort = 0;
static int sFrames = 10;
static int sMontFrames = 2;
static int sOverlap = 4;
static int sTrialFPS = 20;
static IclipPlanes sStartClips;
static IclipPlanes sEndClips;
std::vector<MovieSegment> sSegments;
std::vector<unsigned char> sStartObjTrans;
std::vector<unsigned char> sEndObjTrans;

/* Local functions */
static void makeMontage(int frames, int overlap);
static int makeMovie(int frames, bool fromSequence);
static void setstep(int index, int frame, int loLim, int hiLim, float *start,
                    float *step);
static void xinput(void);
static void setOneStartOrEnd(int startEnd, int index, float value);
static void setAllStartOrEnd(int startEnd, std::vector<unsigned char> &objTrans);
static void readStartEndInts(int index, int &startVal, int &endVal);
static void setSegmentState(MovieSegment &segment, MovieTerminus *term);


void mvMovieHelp()
{
  imodShowHelpPage("modelMovie.html#TOP");
}

static void xinput(void)
{
  QApplication::flush();
  qApp->processEvents();
}

static void setOneStartOrEnd(int startEnd, int index, float value)
{
  if (startEnd == IMODV_MOVIE_START_STATE)
    sDia->setStart(index, value);
  else
    sDia->setEnd(index, value);
}

static void readStartEndInts(int index, int &startVal, int &endVal)
{
  float sv, ev;
  sDia->readStartEnd(index, sv, ev);
  startVal = B3DNINT(sv);
  endVal = B3DNINT(ev);
}

static void setAllStartOrEnd(int startEnd, std::vector<unsigned char> &objTrans)
{
  Iview *vw = &Imodv->imod->view[0];
  int obNum, numObj = Imodv->imod->objsize;
  sFullaxis = 0;

  setOneStartOrEnd(startEnd, 0, vw->rot.x);
  setOneStartOrEnd(startEnd, 1, vw->rot.y);
  setOneStartOrEnd(startEnd, 2, vw->rot.z);
  setOneStartOrEnd(startEnd, 3, vw->trans.x);
  setOneStartOrEnd(startEnd, 4, vw->trans.y);
  setOneStartOrEnd(startEnd, 5, vw->trans.z);
  setOneStartOrEnd(startEnd, 6, vw->rad);
  if (!Imodv->standalone) {
    setOneStartOrEnd(startEnd, 7, (int)(Imodv->vi->xmouse + 1.5));
    setOneStartOrEnd(startEnd, 8, (int)(Imodv->vi->ymouse + 1.5));
    setOneStartOrEnd(startEnd, 9, (int)(Imodv->vi->zmouse + 1.5));
    setOneStartOrEnd(startEnd, 10, mvImageGetTransparency());
    setOneStartOrEnd(startEnd, 11, mvImageGetThickness());
  }
  objTrans.resize(numObj);
  for (obNum = 0; obNum < numObj; obNum++)
    objTrans[obNum] = Imodv->imod->obj[obNum].trans;
}

// Set the starting values to the current display values
void mvMovieSetStart()
{
  Iview *vw = &Imodv->imod->view[0];
  setAllStartOrEnd(IMODV_MOVIE_START_STATE, sStartObjTrans);
  imodClipsCopy(&vw->clips, &sStartClips);
}

// Set the ending values to the current display values
void mvMovieSetEnd()
{
  Iview *vw = &Imodv->imod->view[0];
  setAllStartOrEnd(IMODV_MOVIE_END_STATE, sEndObjTrans);
  imodClipsCopy(&vw->clips, &sEndClips);
}

// Do full axis rotation: set start and end both to same values
void mvMovieFullAxis(int ixy)
{
  mvMovieSetStart();
  mvMovieSetEnd();
  sFullaxis = ixy;
}

// The dialog reports movie/montage switch so sequence dialog can be updated
void mvMovieMontSelection(int mont)
{
  sMontage = mont;
  if (sSequenceDia)
    sSequenceDia->updateEnables(sMontage != 0, sAbort == 0);
}

// The dialog say it wants to close, so send it close signal
void mvMovieQuit()
{
  sDia->close();
}

// When the dialog actually closes, get button states, clean up and stop movie
void mvMovieClosing()
{
  sDia->getButtonStates(sLongway, sReverse, sMontage,
                        sFile_format, sSaved, sTrialFPS);
  sDia->getFrameBoxes(sFrames, sMontFrames);
  imodvDialogManager.remove((QWidget *)sDia);
  sDia = NULL;
  sAbort = 1;
  if (sSequenceDia)
    sSequenceDia->updateEnables(false, false);
}

void mvMovieStop()
{
  sAbort = 1;
}

int mvMovieMake(bool fromSequence)
{
  sDia->getButtonStates(sLongway, sReverse, sMontage,
                        sFile_format, sSaved, sTrialFPS);
  sDia->getFrameBoxes(sFrames, sMontFrames);

  /* DNM: only make if not already making */
  if (sAbort) {
    if (sMontage)
      makeMontage(sMontFrames, sOverlap);
    else
      return makeMovie(sFrames, fromSequence);
  }
  return 0;
}

void mvMovieDialog(ImodvApp *a, int state)
{
  if (!state){
    if (sDia) 
      sDia->close();
    return;
  }
  if (sDia){
    sDia->raise();
    return;
  }

  // Initialize these every time
  sApp = a;
  sSaved   = 0;
  sAbort = 1;   /* DNM: make this a flag that not making movie */

  sDia = new imodvMovieForm(imodvDialogManager.parent(IMODV_DIALOG), 
                                  Qt::Window);
  if (!sDia){
    dia_err("Failed to create 3dmodv movie window!");
    return;
  }

  // Set title bar
  setModvDialogTitle(sDia, "3dmodv Movie: ");

  // Set the states
  mvMovieSetStart();
  mvMovieSetEnd();
  sDia->setButtonStates(sLongway, sReverse, sMontage,
                        sFile_format, sSaved, sTrialFPS);
  sDia->setFrameBoxes(sFrames, sMontFrames);
  imodvDialogManager.add((QWidget *)sDia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)sDia, IMODV_DIALOG);
  sDia->sequenceOpen(sSequenceDia != NULL);
  if (sSequenceDia)
    sSequenceDia->updateEnables(!sMontage, false);
}

void mvMovieSequenceDialog(ImodvApp *a, int state)
{
  if (!state) {
    if (sSequenceDia) 
      sSequenceDia->close();
    return;
  }
  if (sSequenceDia) {
    sSequenceDia->raise();
    return;
  }

  sSequenceDia = new MovieSequenceForm(imodvDialogManager.parent(IMODV_DIALOG), 
                                       Qt::Window);
  if (!sSequenceDia) {
    dia_err("Failed to create 3dmodv movie sequence window!");
    return;
  }

  // Set title bar
  setModvDialogTitle(sSequenceDia, "3dmodv Movie Sequence: ");

  imodvDialogManager.add(sSequenceDia, IMODV_DIALOG);

  // Height is managed in the init function
  adjustGeometryAndShow(sSequenceDia, IMODV_DIALOG, false);
  sSequenceDia->updateEnables(sDia != NULL && !sMontage, sAbort == 0);
  if (sDia)
    sDia->sequenceOpen(true);
}

void mvMovieSequenceClosing()
{
  imodvDialogManager.remove(sSequenceDia);
  sSequenceDia = NULL;
  if (sDia)
    sDia->sequenceOpen(false);
}

// Get all the properties that define a movie segment
void mvMovieGetSegment(MovieSegment &segment)
{
  int i, ind, numObj;
  sDia->getFrameBoxes(sFrames, sMontFrames);
  sDia->readStartEnd(0, segment.start.rotation.x, segment.end.rotation.x);
  sDia->readStartEnd(1, segment.start.rotation.y, segment.end.rotation.y);
  sDia->readStartEnd(2, segment.start.rotation.z, segment.end.rotation.z);
  sDia->readStartEnd(3, segment.start.translate.x, segment.end.translate.x);
  sDia->readStartEnd(4, segment.start.translate.y, segment.end.translate.y);
  sDia->readStartEnd(5, segment.start.translate.z, segment.end.translate.z);
  sDia->readStartEnd(6, segment.start.zoomRad, segment.end.zoomRad);
  if (Imodv->standalone) {
    segment.imgAxisFlags = 0;
  } else {
    mvImageGetMovieState(segment);
    readStartEndInts(7, segment.start.imgXcenter, segment.end.imgXcenter);
    readStartEndInts(8, segment.start.imgYcenter, segment.end.imgYcenter);
    readStartEndInts(9, segment.start.imgZcenter, segment.end.imgZcenter);
    readStartEndInts(10, segment.start.imgTransparency, segment.end.imgTransparency);
    readStartEndInts(11, segment.start.imgSlices, segment.end.imgSlices);
  }
  segment.numFrames = sFrames;
  segment.viewNum = Imodv->imod->cview;
  segment.fullAxis = sFullaxis;
  segment.numClips = B3DMIN(sStartClips.count, sEndClips.count);
  segment.clipFlags = sStartClips.flags;
  for (i = 0; i < segment.numClips; i++) {
    segment.clipNormal[i] = sStartClips.normal[i];
    segment.start.clipPoint[i] = sStartClips.point[i];
    segment.end.clipPoint[i] = sEndClips.point[i];
  }
  numObj = B3DMIN(MAX_OBJ_ONOFF, Imodv->imod->objsize);
  //CLEAR_RESIZE(segment.objStates, unsigned char, numObj);
  segment.objStates.resize(numObj);
  for (i = 0; i < numObj; i++)
    segment.objStates[i] = iobjOff(Imodv->imod->obj[i].flags) ? 0 : 1;

  // Build list of objects with changed trans
  numObj = B3DMIN(sStartObjTrans.size(), sEndObjTrans.size());
  segment.transChangeObjs.clear();
  for (i = 0; i < numObj; i++) {
    ind = segment.transChangeObjs.size();
    if (sStartObjTrans[i] != sEndObjTrans[i] && ind < VMOVIE_MAX_TRANS_CHANGES) {
      segment.start.objTrans[ind] = sStartObjTrans[i];
      segment.end.objTrans[ind] = sEndObjTrans[i];
      segment.transChangeObjs.push_back(i);
    }
  }

  if (sFullaxis && segment.label.isEmpty()) {
    segment.label = QString("Full 360 ") + 
      (sFullaxis == IMODV_MOVIE_FULLAXIS_X ? "X" : "Y");
  }
  
}

// Set the movie drawing parameters completely for a segment
void mvMovieSetSegment(MovieSegment &segment)
{
  int se, i, numObj, obNum;
  MovieTerminus *term = &segment.start;

  // Move each endpoint into the dialog
  for (se = 0; se < 2; se++) {
    setOneStartOrEnd(se, 0, term->rotation.x);
    setOneStartOrEnd(se, 1, term->rotation.y);
    setOneStartOrEnd(se, 2, term->rotation.z);
    setOneStartOrEnd(se, 3, term->translate.x);
    setOneStartOrEnd(se, 4, term->translate.y);
    setOneStartOrEnd(se, 5, term->translate.z);
    setOneStartOrEnd(se, 6, term->zoomRad);
    if (segment.imgAxisFlags) {
      setOneStartOrEnd(se, 7, term->imgXcenter);
      setOneStartOrEnd(se, 8, term->imgYcenter);
      setOneStartOrEnd(se, 9, term->imgZcenter);
      setOneStartOrEnd(se, 10, term->imgTransparency);
      setOneStartOrEnd(se, 11, term->imgSlices);
    }
    term = &segment.end;
  }

  // Make sure image drawing is set up fully
  mvImageSetMovieDrawState(segment);
  sFrames = segment.numFrames;
  sFullaxis = segment.fullAxis;
  sDia->setFrameBoxes(sFrames, sMontFrames);

  // Do common display setup, set clip planes to start point
  setSegmentState(segment, &segment.start);
 
  // Set both sets of clip planes consistently
  sStartClips.count = sEndClips.count = segment.numClips;
  sStartClips.flags = sEndClips.flags = segment.clipFlags;
  for (i = 0; i < segment.numClips; i++) {
    sStartClips.normal[i] = sEndClips.normal[i] = segment.clipNormal[i];
    sStartClips.point[i] = segment.start.clipPoint[i];
    sEndClips.point[i] = segment.end.clipPoint[i];
  }

  // And set the full list of object trans values on both ends
  numObj = Imodv->imod->objsize;
  sStartObjTrans.resize(numObj);
  sEndObjTrans.resize(numObj);
  
  // First set the trans for all objects, then modify the ones in the segment
  for (obNum = 0; obNum < numObj; obNum++)
    sStartObjTrans[i] = sEndObjTrans[i] = Imodv->imod->obj[obNum].trans;
  for (i = 0; i < segment.transChangeObjs.size(); i++) {
    obNum = segment.transChangeObjs[i];
    if (obNum < numObj) {
      sStartObjTrans[obNum] = segment.start.objTrans[i];
      sEndObjTrans[obNum] = segment.end.objTrans[i];
    }
  }

  imodvDraw(Imodv);
  imodvDrawImodImages();
  imodvObjedNewView();
}

// Set the display to the state at one end of a movie segment without affecting the 
// current movie parameters any more than possible
void mvMovieSetTerminus(int startEnd, MovieSegment &segment)
{
  Iview *vw = &Imodv->imod->view[0];
  MovieTerminus *term = &segment.start;
  if (startEnd == IMODV_MOVIE_END_STATE)
    term = &segment.end;

  // Set the model display state
  setSegmentState(segment, term);
  vw->rot.x = term->rotation.x;
  vw->rot.y = term->rotation.y;
  vw->rot.z = term->rotation.z;
  vw->trans.x = term->translate.x;
  vw->trans.y = term->translate.y;
  vw->trans.z = term->translate.z;
  vw->rad = term->zoomRad;

  // Set image drawing state
  mvImageSetMovieEndState(startEnd, segment);

  imodvDraw(Imodv);
  imodvDrawImodImages();
  imodvObjedNewView();
}

// Common state-setting when setting a movie segment or display to an end state
// Go to the view, set the object on/off states
// Set the actual clip planes to the appropriate endpoint
static void setSegmentState(MovieSegment &segment, MovieTerminus *term)
{
  int i, obNum;
  Iview *vw = &Imodv->imod->view[0];
  imodvAutoStoreView(Imodv);
  imodvViewsSetView(Imodv, B3DMIN(segment.viewNum, Imodv->imod->viewsize), false, true,
                    false);
  for (i = 0; i < B3DMIN(segment.objStates.size(), Imodv->imod->objsize); i++)
    setOrClearFlags(&Imodv->imod->obj[i].flags, IMOD_OBJFLAG_OFF, 
                    segment.objStates[i] ? 0 : 1);
  vw->clips.count = segment.numClips;
  vw->clips.flags = segment.clipFlags;
  for (i = 0; i < segment.numClips; i++) {
    vw->clips.normal[i] = segment.clipNormal[i];
    vw->clips.point[i] = term->clipPoint[i];
  }

  // set transparency directly from the terminus values
  for (i = 0; i < segment.transChangeObjs.size(); i++) {
    obNum = segment.transChangeObjs[i];
    if (obNum < Imodv->imod->objsize)
      Imodv->imod->obj[obNum].trans = term->objTrans[i];
  }
}

std::vector<MovieSegment> *mvMovieSegmentArray() {
  return &sSegments;
}

static void setstep(int index, int frame, int loLim, int hiLim, float *start,
                    float *step)
{
  float tmin, tmax;
  sDia->readStartEnd(index, tmin, tmax);

  // If the item has a limit, make sure it is between 1 and limit
  if (hiLim && tmin < loLim) {
    tmin = (float)loLim;
    sDia->setStart(index, tmin);
  }
  if (hiLim && tmin > hiLim) {
    tmin = (float)hiLim;
    sDia->setStart(index, tmin);
  } 
  if (hiLim && tmax < loLim) {
    tmax = (float)loLim;
    sDia->setEnd(index, tmax);
  }
  if (hiLim && tmax > hiLim) {
    tmax = (float)hiLim;
    sDia->setEnd(index, tmax);
  }

  if (sReverse){
    *start = tmax;
    *step  = (tmin - tmax) / (float)frame;
  }else{
    *start = tmin;
    *step = (tmax - tmin) / (float)frame;
  }
}

void mvMovieUpdate()
{
  if (sDia)
    sDia->setNonTifLabel();
}

static int makeMovie(int frames, bool fromSequence)
{
  ImodvApp *a = sApp;
  Iview *vw;
  static int lastMakeNothing = 0;
  int frame, pl, nsteps, interval, obNum, numObj, objTransChange;
  float astart, astep;
  float bstart, bstep;
  float gstart, gstep;
  float zstart, zstep, zfac;
  float xtstart, ytstart, ztstart;
  float xtstep, ytstep, ztstep;
  float xImStart, yImStart, zImStart;
  float xImStep, yImStep, zImStep;
  float thickStart, thickStep, transpStart, transpStep;
  double angle, delangle;
  double alpha, beta, gamma;
  Ipoint v;
  Imat *mat, *mati, *matp;
  QTime drawTimer;

  if (frames <= 0)
    return 0;
  nsteps = frames - 1;
  if (!nsteps)
    nsteps = 1;

  if (sSequenceDia)
    sSequenceDia->updateEnables(true, true);
  xImStep = yImStep = zImStep = 0.;
  setstep(0, nsteps, 0, 0, &astart, &astep);
  setstep(1, nsteps, 0, 0, &bstart, &bstep);
  setstep(2, nsteps, 0, 0, &gstart, &gstep);
  setstep(3, nsteps, 0, 0, &xtstart, &xtstep);
  setstep(4, nsteps, 0, 0, &ytstart, &ytstep);
  setstep(5, nsteps, 0, 0, &ztstart, &ztstep);
  setstep(6, nsteps, 0, 0, &zstart, &zstep);
  if (!a->standalone) {
    setstep(7, nsteps, 1, a->vi->xsize, &xImStart, &xImStep);
    setstep(8, nsteps, 1, a->vi->ysize, &yImStart, &yImStep);
    setstep(9, nsteps, 1, a->vi->zsize, &zImStart, &zImStep);
    setstep(10, nsteps, 0, 100, &transpStart, &transpStep);
    setstep(11, nsteps, 1, a->vi->zsize, &thickStart, &thickStep);
  }

  a->xrotMovie = a->yrotMovie = a->zrotMovie = 0;
  a->movie = 0;
  a->moveall = 0;

  zfac = pow ((double)(zstart + zstep * nsteps) / zstart,
              1.0 / (double)nsteps);

  vw = a->imod->view;
  vw->rad   = zstart;
  vw->rot.x = astart;
  vw->rot.y = bstart;
  vw->rot.z = gstart;
  vw->trans.x = xtstart;
  vw->trans.y = ytstart;
  vw->trans.z = ztstart;
  imodClipsCopy(&sStartClips, &vw->clips);
  imodvObjedNewView();
  mat = imodMatNew(3);
  mati = imodMatNew(3);
  matp = imodMatNew(3);

  if (!a->standalone && a->texMap) {
    a->vi->xmouse = (int)(xImStart - 0.5);
    a->vi->ymouse = (int)(yImStart - 0.5);
    a->vi->zmouse = (int)(zImStart - 0.5);
    mvImageSetThickTrans((int)(thickStart + 0.5), (int)(transpStart + 0.5));
  }

  objTransChange = 0;
  numObj = b3dIMin(3, Imodv->imod->objsize, sStartObjTrans.size(), sEndObjTrans.size());
  for (obNum = 0; obNum < numObj; obNum++) {
    Imodv->imod->obj[obNum].trans = sStartObjTrans[obNum];
    if (sStartObjTrans[obNum] != sEndObjTrans[obNum])
      objTransChange = 1;
  }

  /* get incremental rotation matrix */
  delangle = 360. / frames;
  if (sReverse)
    delangle *= -1.0;

  if(sFullaxis == IMODV_MOVIE_FULLAXIS_X)
    imodMatRot(mati, delangle, b3dX);
  else if(sFullaxis == IMODV_MOVIE_FULLAXIS_Y)
    imodMatRot(mati, delangle, b3dY);
  else {

    /* In general case, net change is final matrix times inverse of starting
       matrix - then find the vector and angle for that rotation and
       divide angle by # of frames to get incremental matrix */

    imodMatRot(mat, (double)-astart, b3dX);
    imodMatRot(mat, (double)-bstart, b3dY);
    imodMatRot(mat, (double)-gstart, b3dZ);
    imodMatRot(mat, (double)(gstart + nsteps * gstep), b3dZ);
    imodMatRot(mat, (double)(bstart + nsteps * bstep), b3dY);
    imodMatRot(mat, (double)(astart + nsteps * astep), b3dX);
    imodMatFindVector(mat, &angle, &v);
    delangle = angle / nsteps;
    if (sLongway)
      delangle = (angle - 360.) / nsteps;
    imodMatRotateVector(mati, delangle, &v);
  }

  /* Evaluate whether clip planes change and return if nothing is going to change */
  frame = 0;
  for (pl = 0; pl < B3DMIN(sStartClips.count, sEndClips.count); pl++)
    if (sEndClips.point[pl].x != sStartClips.point[pl].x ||
        sEndClips.point[pl].y != sStartClips.point[pl].y ||
        sEndClips.point[pl].z != sStartClips.point[pl].z)
      frame = 1;

  if (fabs((double)delangle) < 1.e-3 && !frame && !zstep && !xtstep && !ytstep &&
      !ztstep && !xImStep && !yImStep && !zImStep && !thickStep && !transpStep && 
      !fromSequence && !objTransChange && lastMakeNothing < 2) {
    lastMakeNothing = dia_ask_forever("The display will not change.\nAre you sure you"
                                      " want to make a movie with duplicate pictures?");
    if (!lastMakeNothing)
      return 0;
  }

  sAbort = 0;
  interval = B3DNINT(1000. / sTrialFPS);
  for(frame = 1; frame <= frames; frame++){
    if (sSaved) {
      if (sFile_format == 2)
        ImodPrefs->set2ndSnapFormat();
      imodv_auto_snapshot(QString::null, sFile_format ? SnapShot_RGB : 
                          SnapShot_TIF);
      if (sFile_format == 2)
        ImodPrefs->restoreSnapFormat();
    } else {
      drawTimer.start();
      imodvDraw(a);
      pl = interval - drawTimer.elapsed();
      if (pl > 0)
        b3dMilliSleep(pl);
    }

    xinput(); 

    if (sAbort)
      break;

    /* DNM: don't change the angle after the last step */
    if (frame < frames){

      /* change zoom by a factor, not an increment */
      vw->rad   *= zfac;

      /* Get current rotation matrix, multiply by increment rotation,
         and convert back to angles */
      imodMatId(mat);
      imodMatRot(mat, (double)vw->rot.z, b3dZ);
      imodMatRot(mat, (double)vw->rot.y, b3dY);
      imodMatRot(mat, (double)vw->rot.x, b3dX);
      imodMatMult(mat, mati, matp);
      imodMatGetNatAngles(matp, &alpha, &beta, &gamma);

      vw->rot.x = alpha;
      vw->rot.y = beta;
      vw->rot.z = gamma;
      vw->trans.x += xtstep;
      vw->trans.y += ytstep;
      vw->trans.z += ztstep;
      if (!a->standalone && a->texMap) {
        a->vi->xmouse = (int)(xImStart + frame * xImStep - 0.5);
        a->vi->ymouse = (int)(yImStart + frame * yImStep - 0.5);
        a->vi->zmouse = (int)(zImStart + frame * zImStep - 0.5);
        mvImageSetThickTrans((int)(thickStart + frame * thickStep + 0.5),
                                (int)(transpStart + frame * transpStep + 0.5));
      }
      for (pl = 0; pl < B3DMIN(sStartClips.count, sEndClips.count); pl++) {
        vw->clips.point[pl].x = sStartClips.point[pl].x + 
          frame * (sEndClips.point[pl].x - sStartClips.point[pl].x) / frames;
        vw->clips.point[pl].y = sStartClips.point[pl].y + 
          frame * (sEndClips.point[pl].y - sStartClips.point[pl].y) / frames;
        vw->clips.point[pl].z = sStartClips.point[pl].z + 
          frame * (sEndClips.point[pl].z - sStartClips.point[pl].z) / frames;
      }

      for (obNum = 0; obNum < numObj; obNum++) {
        if (sStartObjTrans[obNum] != sEndObjTrans[obNum]) {
          Imodv->imod->obj[obNum].trans = 
            (unsigned char)B3DNINT(sStartObjTrans[obNum] + frame * 
                                   (sEndObjTrans[obNum] - sStartObjTrans[obNum]) /frames);
        }
      }
    }
  }

  pl = sAbort;
  sAbort = 1;
  if (sSequenceDia)
    sSequenceDia->updateEnables(true, false);

  imodMatDelete(mat);
  imodMatDelete(mati);
  imodMatDelete(matp);
  return pl;
}

typedef struct {
  Ipoint transave;
  float radsave;
  Ipoint xunit;
  Ipoint yunit;
} MontModelData;

/* Routine to make a montage */
static void makeMontage(int frames, int overlap)
{
  ImodvApp *a = sApp;
  Iview *vw;
  MontModelData *mmd;
  Imat *mat;
  Ipoint ipt;
  float scrnscale;
  int ix, iy, xFullSize, yFullSize, numChunks, m, mstart, mend;
  float zoom, yzoom;
  unsigned char *framePix = NULL;
  unsigned char **fullPix = NULL;
  unsigned char **linePtrs = NULL;
  ScaleBar *barReal = scaleBarGetParams();
  ScaleBar barSaved;

  /* limit the overlap */
  if (frames <= 1)
    return;
  if (overlap < 0)
    overlap = 0;
  if (overlap > a->winx / 2)
    overlap = a->winx / 2;
  if (overlap > a->winy / 2)
    overlap = a->winy / 2;

  xFullSize = frames * a->winx - (frames - 1) * overlap;
  yFullSize = frames * a->winy - (frames - 1) * overlap;
  imodvModelDrawRange(a, mstart, mend);

  // Check for perspective and give error message
  for (m = mstart; m <= mend; m++) {
    vw = a->mod[m]->view;
    if (vw->fovy >= 1.0f) {
      imodError(NULL, "%s model has a perspective setting of %d (see "
                "Edit-Controls window).\nThe montage will not work right "
                "with perspective.", a->numMods > 1 ? "One" : "This", (int)vw->fovy);
      return;
    }
  }

  a->xrotMovie = a->yrotMovie = a->zrotMovie = 0;
  a->movie = 0;
  a->moveall = 0;
  mmd = B3DMALLOC(MontModelData, a->numMods);
  if (!mmd) {
    imodError(NULL, "Failed to get memory for saving data per model.\n");
    return;
  }

  /* new zoom is minimum of zoom needed to get each dimension to work */
  zoom = (a->winx + (frames - 1) * (a->winx - overlap)) / (float)a->winx;
  yzoom = (a->winy + (frames - 1) * (a->winy - overlap)) / (float)a->winy;
  if (zoom > yzoom)
    zoom = yzoom;

  // Set up memory allocations and scale bar stuff now that zoom is known
  if (utilStartMontSnap(a->winx, a->winy, xFullSize, yFullSize, zoom, barSaved,
                        numChunks, &framePix, &fullPix, &linePtrs)) {
    imodError(NULL, "Failed to get memory for snapshot buffers.\n");
    free(mmd);
    return;
  }
  sAbort = 0;

  for (m = mstart; m <= mend; m++) {
    vw = a->mod[m]->view;

    /* Save current zoom and translations */
    mmd[m].radsave = vw->rad;
    mmd[m].transave = vw->trans;

    vw->rad /= zoom;

    /* Compute translation offsets implied by the given pixel shifts in X and
       Y in the display, using same code as imodv_translated */
    mat = imodMatNew(3);
    imodvRotScaleMatrix(a, mat, a->mod[m]);
    scrnscale = 0.5 * B3DMIN(a->winx, a->winy) / vw->rad;
    
    ipt.x = a->winx - overlap;
    ipt.y = 0.;
    ipt.z = 0.;
    imodMatTransform(mat, &ipt, &mmd[m].xunit);
    mmd[m].xunit.x *= (1.0/ vw->scale.x);
    mmd[m].xunit.y *= (1.0/ vw->scale.y);
    mmd[m].xunit.z *= (1.0/ vw->scale.z);

    ipt.x = 0.;
    ipt.y = a->winy - overlap;
    imodMatTransform(mat, &ipt, &mmd[m].yunit);
    mmd[m].yunit.x *= (1.0/ vw->scale.x);
    mmd[m].yunit.y *= (1.0/ vw->scale.y);
    mmd[m].yunit.z *= (1.0/ vw->scale.z);
    
    /* do initial displacement to lower left corner */
    vw->trans.x += 0.5 * (frames - 1.) * (mmd[m].xunit.x + mmd[m].yunit.x) ;
    vw->trans.y += 0.5 * (frames - 1.) * (mmd[m].xunit.y + mmd[m].yunit.y) ;
    vw->trans.z += 0.5 * (frames - 1.) * (mmd[m].xunit.z + mmd[m].yunit.z) ;
  }

  /* 12/28/03: Disabling the auto swap and reading from back buffer for db 
     did not work here for protecting from occluding stuff (nor does it work 
     in regular snapshot) */
  // But 4/6/05: It was needed to prevent getting an out-of-date image for
  // one machine under xorg-6.7.0
  if (a->dblBuf)
    a->mainWin->mCurGLw->setBufferSwapAuto(false);
  glReadBuffer(a->dblBuf ? GL_BACK : GL_FRONT);

  for (iy = 0; iy < frames; iy++) {
    for (ix = 0; ix < frames; ix++) {

      // Set up for scale bar if it is the right corner
      utilMontSnapScaleBar(ix, iy, frames, a->winx, a->winy, scrnscale,
                           barSaved.draw);
      imodvDraw(a);

      // Print scale bar length if it was drawn
      if (a->scaleBarSize > 0)
        imodPrintStderr("Scale bar for montage is %g %s\n", a->scaleBarSize,
                        imodUnits(a->imod));
      glFlush();
      glReadPixels(0, 0, a->winx, a->winy, GL_RGBA, GL_UNSIGNED_BYTE, 
                   framePix);
      glFlush();
      memLineCpy(linePtrs, framePix, a->winx, a->winy, 4, ix * 
                 (a->winx - overlap), iy * (a->winy - overlap), a->winx, 0, 0);
        
      xinput(); 
      if (ImodvClosed) {
        free(mmd);
        // Shouldn't it do this too?
        utilFreeMontSnapArrays(fullPix, numChunks, framePix, linePtrs);
        return;
      }

      if (sAbort)
        break;

      /* Each X, advance along row */
      for (m = mstart; m <= mend; m++) {
        vw = a->mod[m]->view;
        vw->trans.x -= mmd[m].xunit.x;
        vw->trans.y -= mmd[m].xunit.y;
        vw->trans.z -= mmd[m].xunit.z;
      }
    }

    /* End of row: advance in Y, move X back to start of next row */
    for (m = mstart; m <= mend; m++) {
      vw = a->mod[m]->view;
      vw->trans.x -= mmd[m].yunit.x - frames * mmd[m].xunit.x;
      vw->trans.y -= mmd[m].yunit.y - frames * mmd[m].xunit.y;
      vw->trans.z -= mmd[m].yunit.z - frames * mmd[m].xunit.z;
    }
    if (sAbort)
      break;
  }

  /* If not aborted, then get snapshot name and save data */
  if (!sAbort)
    utilFinishMontSnap(linePtrs, xFullSize, yFullSize, 
                       sFile_format, a->snap_fileno, 4, zoom,
                       "modv", "3dmodv: Saving");
  
  utilFreeMontSnapArrays(fullPix, numChunks, framePix, linePtrs);
  
  if (a->dblBuf) {
    imodv_swapbuffers(a);
    a->mainWin->mCurGLw->setBufferSwapAuto(true);
  }
  sAbort = 1;
  for (m = mstart; m <= mend; m++) {
    vw = a->mod[m]->view;
    vw->rad = mmd[m].radsave;
    vw->trans = mmd[m].transave;
  }
  *barReal = barSaved;
  imodvDraw(a);
  imodMatDelete(mat);
  free(mmd);
  return;
}
