/*  IMOD VERSION 2.42
 *
 *  imodv_movie.c -- Movie creation dialog for imodv.
 *
 *  Original Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <qapplication.h>
#include "formv_movie.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "b3dgfx.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_movie.h"
#include "control.h"

/* The movie control structure  */
struct imodvMovieDialogStruct
{
  imodvMovieForm *dia;
  ImodvApp  *a;
  int saved;
  int reverse;
  int longway;
  int montage;
  int file_format;
  int fullaxis;
  int abort;
  int frames;
  int montFrames;
  int overlap;
};

/* The resident structure and pointer to it */
static struct imodvMovieDialogStruct movieStruct;
static struct imodvMovieDialogStruct *movie = &movieStruct;

/* Local functions */
static void imodvMakeMontage(int frames, int overlap);
static void imodvMakeMovie(int frames);
static void setstep(int index, int frame, float *start, float *step);


void imodvMovieHelp()
{
  dia_vasmsg
    ("Make Movie Dialog Help.\n\n",
     "Movie Making\n\n",
     "The upper controls make a movie by stepping through "
     "from the values in the start set to the values in the end set.\n\n"
     "Select the [Set Start] button to set the starting values to the "
     "values of the current display, or the [Set End] button to set "
     "the ending values.\n\n"
     "Selecting the [Make] button will cause imodv to "
     "display the number of frames given.  The rotation between the "
     "starting and ending positions will be resolved into a rotation "
     "around a single axis, and the rotation will occur at even "
     "increments around that axis.\n\n"
     "Select the [Stop] button to stop after the next display.\n\n"
     "If [Write Files] is selected then a snapshot will "
     "be taken of each image.  The file will be an RGB or a TIFF file, "
     "depending on which radio button is selected.\n\n"
     "To make a movie through 360 degrees around the X or the Y axis, "
     "select the [Full 360 X] or [Full 360 Y] button.  Then select the "
     "[Make] button.  The number of frames can be set before or "
     "after selecting a Full 360 button.  Selecting [Set Start] or "
     "[Set End] will cancel the Full 360 selection.\n\n",
     "If [Reverse] is selected, the movie will run in reverse, from "
     "the ending to the starting position, or rotate in the opposite "
     "direction for a Full 360 movie.\n\n"
     "If [Long way] is selected, the rotation will go the long way "
     "around, through an angle greater instead of less than 180 "
     "degrees.\n\n\n",
     "Montage Making\n\n",
     "The lower controls allow one to save a montage of zoomed-up views "
     "of the model.  The model will be zoomed up by an appropriate "
     "factor, then translated to a regular array of positions.  "
     "The collection of "
     "images will form a montage of whatever is currently in the window "
     "before you start.  Perspective must be set to zero in order for "
     "this to work correctly.\n\n"
     "Set the number of frames to the desired zoom factor.  Set the "
     "amount of overlap to whatever is most convenient for reassembling "
     "the montage.  "
     "If [Write Files] is selected then a snapshot will "
     "be taken of each image.  The file will be an RGB or a TIFF file, "
     "depending on which radio button is selected.\n",
     NULL);
                
  return;
}

static void xinput(void)
{
  QApplication::flush();
  qApp->processEvents();
}

// Set the starting values to the current display values
void imodvMovieSetStart()
{
  Iview *vw = &Imodv->imod->view[0];
  movie->fullaxis = 0;

  movie->dia->setStart(0, vw->rot.x);
  movie->dia->setStart(1, vw->rot.y);
  movie->dia->setStart(2, vw->rot.z);
  movie->dia->setStart(3, vw->trans.x);
  movie->dia->setStart(4, vw->trans.y);
  movie->dia->setStart(5, vw->trans.z);
  movie->dia->setStart(6, vw->rad);
}

// Set the ending values to the current display values
void imodvMovieSetEnd()
{
  Iview *vw = &Imodv->imod->view[0];
  movie->fullaxis = 0;

  movie->dia->setEnd(0, vw->rot.x);
  movie->dia->setEnd(1, vw->rot.y);
  movie->dia->setEnd(2, vw->rot.z);
  movie->dia->setEnd(3, vw->trans.x);
  movie->dia->setEnd(4, vw->trans.y);
  movie->dia->setEnd(5, vw->trans.z);
  movie->dia->setEnd(6, vw->rad);
}

// Do full axis rotation: set start and end both to same values
void imodvMovieFullAxis(int ixy)
{
  imodvMovieSetStart();
  imodvMovieSetEnd();
  movie->fullaxis = ixy;
}

// The dialog say it wants to close, so send it close signal
void imodvMovieQuit()
{
  movie->dia->close();
}

// When the dialog actually closes, get button states, clean up and stop movie
void imodvMovieClosing()
{
  int format;
  movie->dia->getButtonStates(movie->longway, movie->reverse, movie->montage,
                              format, movie->saved);
  movie->file_format = format ? SnapShot_TIF : SnapShot_RGB; 
  movie->dia->getFrameBoxes(movie->frames, movie->montFrames, movie->overlap);
  imodvDialogManager.remove((QWidget *)movie->dia);
  movie->dia = NULL;
  movie->abort = 1;
}

void imodvMovieStop()
{
  movie->abort = 1;
}

void imodvMovieMake()
{
  int format;

  movie->dia->getButtonStates(movie->longway, movie->reverse, movie->montage,
                              format, movie->saved);
  movie->file_format = format ? SnapShot_TIF : SnapShot_RGB; 
  movie->dia->getFrameBoxes(movie->frames, movie->montFrames, movie->overlap);

  /* DNM: only make if not already making */
  if (movie->abort) {
    if (movie->montage)
      imodvMakeMontage(movie->montFrames, movie->overlap);
    else
      imodvMakeMovie(movie->frames);
  }
}

void imodvMovieDialog(ImodvApp *a, int state)
{
  QString qstr;
  char *window_name;
  static int first = 1;

  // Initialize first time, save between invocations
  if (first){
    movie->dia = NULL;
    movie->reverse = 0;
    movie->longway = 0;
    movie->file_format = SnapShot_TIF;
    movie->montage = 0;
    movie->frames = 10;
    movie->montFrames = 2;
    movie->overlap = 4;
    first = 0;
  }

  if (!state){
    if (movie->dia) 
      movie->dia->close();
    return;
  }
  if (movie->dia){
    movie->dia->raise();
    return;
  }

  // Initialize these every time
  movie->a = a;
  movie->saved   = 0;
  movie->abort = 1;   /* DNM: make this a flag that not making movie */

  movie->dia = new imodvMovieForm(NULL, NULL, //false, 
                                  Qt::WDestructiveClose | Qt::WType_TopLevel);
  if (!movie->dia){
    dia_err("Failed to create imodv movie window!");
    return;
  }

  // Set title bar
  window_name = imodwEithername("Imodv Movie: ", a->imod->fileName, 1);
  qstr = window_name;
  if (window_name)
    free(window_name);
  if (!qstr.isEmpty())
    movie->dia->setCaption(qstr);

  // Set the states
  imodvMovieSetStart();
  imodvMovieSetEnd();
  movie->dia->setButtonStates(movie->longway, movie->reverse, movie->montage,
                              movie->file_format == SnapShot_TIF ? 1 : 0, 
                              movie->saved);
  movie->dia->setFrameBoxes(movie->frames, movie->montFrames, movie->overlap);
  imodvDialogManager.add((QWidget *)movie->dia, IMODV_DIALOG);
  movie->dia->show();
}

static void setstep(int index, int frame, float *start, float *step)
{
  float tmin, tmax;
  movie->dia->readStartEnd(index, tmin, tmax);
  if (movie->reverse){
    *start = tmax;
    *step  = (tmin - tmax) / (float)frame;
  }else{
    *start = tmin;
    *step = (tmax - tmin) / (float)frame;
  }
}


static void imodvMakeMovie(int frames)
{
  ImodvApp *a = movie->a;
  Iview *vw;
  
  int frame;
  float astart, astep;
  float bstart, bstep;
  float gstart, gstep;
  float zstart, zstep, zfac;
  float xtstart, ytstart, ztstart;
  float xtstep, ytstep, ztstep;
  double angle, delangle;
  double alpha, beta, gamma;
  Ipoint v;
  Imat *mat, *mati, *matp;

  if (frames <= 0)
    return;
  frame = frames - 1;
  if (!frame)
    frame = 1;

  setstep(0, frame, &astart, &astep);
  setstep(1, frame, &bstart, &bstep);
  setstep(2, frame, &gstart, &gstep);
  setstep(3, frame, &xtstart, &xtstep);
  setstep(4, frame, &ytstart, &ytstep);
  setstep(5, frame, &ztstart, &ztstep);
  setstep(6, frame, &zstart, &zstep);

  a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
  a->movie = 0;
  a->moveall = 0;

  zfac = pow ((double)(zstart + zstep * frame) / zstart,
              1.0 / (double)frame);

  vw = a->imod->view;
  vw->rad   = zstart;
  vw->rot.x = astart;
  vw->rot.y = bstart;
  vw->rot.z = gstart;
  vw->trans.x = xtstart;
  vw->trans.y = ytstart;
  vw->trans.z = ztstart;
  mat = imodMatNew(3);
  mati = imodMatNew(3);
  matp = imodMatNew(3);

  /* get incremental rotation matrix */

  delangle = 360. / frames;
  if (movie->reverse)
    delangle *= -1.0;

  if(movie->fullaxis == IMODV_MOVIE_FULLAXIS_X)
    imodMatRot(mati, delangle, b3dX);
  else if(movie->fullaxis == IMODV_MOVIE_FULLAXIS_Y)
    imodMatRot(mati, delangle, b3dY);
  else {

    /* In general case, net change is final matrix times inverse of starting
       matrix - then find the vector and angle for that rotation and
       divide angle by # of frames to get incremental matrix */

    imodMatRot(mat, (double)-astart, b3dX);
    imodMatRot(mat, (double)-bstart, b3dY);
    imodMatRot(mat, (double)-gstart, b3dZ);
    imodMatRot(mat, (double)(gstart + frame * gstep), b3dZ);
    imodMatRot(mat, (double)(bstart + frame * bstep), b3dY);
    imodMatRot(mat, (double)(astart + frame * astep), b3dX);
    imodMatFindVector(mat, &angle, &v);
    delangle = angle / frame;
    if (movie->longway)
      delangle = (angle - 360.) / frame;
    imodMatRotateVector(mati, delangle, &v);
  }

  /* Return if nothing is going to change */
  if(fabs((double)delangle) < 1.e-3 && zstep == 0. && xtstep == 0. &&
     ytstep == 0. && ztstep == 0.)
    return;


  movie->abort = 0;
  for(frame = 0; frame < frames; frame++){
    if (movie->saved)
      imodv_auto_snapshot(NULL, movie->file_format);
    else
      imodvDraw(a);

    xinput(); 

    if (movie->abort)
      break;

    /* DNM: don't change the angle after the last step */
    if (frame < frames - 1){

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
    }
  }
  movie->abort = 1;

  imodMatDelete(mat);
  imodMatDelete(mati);
  imodMatDelete(matp);
  return;
}

/* Routine to make a montage */
static void imodvMakeMontage(int frames, int overlap)
{
  ImodvApp *a = movie->a;
  Iview *vw;
  Ipoint transave;
  Imat *mat;
  Ipoint ipt, spt, xunit, yunit;
  float scrnscale, radsave;
  int ix, iy;
  float zoom, yzoom;

  /* limit the overlap */
  if (frames <= 1)
    return;
  if (overlap < 0)
    overlap = 0;
  if (overlap > a->winx / 2)
    overlap = a->winx / 2;
  if (overlap > a->winy / 2)
    overlap = a->winy / 2;

  a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
  a->movie = 0;
  a->moveall = 0;
  movie->abort = 0;

  /* Save current zoom and translations */
  vw = a->imod->view;
  radsave = vw->rad;
  transave = vw->trans;

  /* new zoom is minimum of zoom needed to get each dimension to work */
  zoom = (a->winx + (frames - 1) * (a->winx - overlap)) / (float)a->winx;
  yzoom = (a->winy + (frames - 1) * (a->winy - overlap)) / (float)a->winy;
  if (zoom > yzoom)
    zoom = yzoom;
  vw->rad /= zoom;

  /* Compute translation offsets implied by the given pixel shifts in X and
     Y in the display, using same code as imodv_translated */
  mat = imodMatNew(3);
  imodMatId(mat);
  imodMatRot(mat, -(double)vw->rot.x, b3dX);
  imodMatRot(mat, -(double)vw->rot.y, b3dY);
  imodMatRot(mat, -(double)vw->rot.z, b3dZ);
     
  scrnscale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) / vw->rad;
    
  spt.x = 1.0f/scrnscale;
  spt.y = 1.0f/scrnscale;
  spt.z = 1.0f/scrnscale * 1.0f/a->imod->zscale;
  imodMatScale(mat, &spt);
    
  ipt.x = a->winx - overlap;
  ipt.y = 0.;
  ipt.z = 0.;
  imodMatTransform(mat, &ipt, &xunit);
  xunit.x *= (1.0/ vw->scale.x);
  xunit.y *= (1.0/ vw->scale.y);
  xunit.z *= (1.0/ vw->scale.z);

  ipt.x = 0.;
  ipt.y = a->winy - overlap;
  imodMatTransform(mat, &ipt, &yunit);
  yunit.x *= (1.0/ vw->scale.x);
  yunit.y *= (1.0/ vw->scale.y);
  yunit.z *= (1.0/ vw->scale.z);
    
  /* do initial displacement to lower left corner */
  vw->trans.x += 0.5 * (frames - 1.) * (xunit.x + yunit.x) ;
  vw->trans.y += 0.5 * (frames - 1.) * (xunit.y + yunit.y) ;
  vw->trans.z += 0.5 * (frames - 1.) * (xunit.z + yunit.z) ;


  for(iy = 0; iy < frames; iy++){
    for(ix = 0; ix < frames; ix++){
      if (movie->saved)
        imodv_auto_snapshot(NULL, movie->file_format);
      else
        imodvDraw(a);

      xinput(); 

      if (movie->abort)
        break;

      /* Each X, advance along row */
      vw->trans.x -= xunit.x;
      vw->trans.y -= xunit.y;
      vw->trans.z -= xunit.z;
    }

    /* End of row: advance in Y, move X back to start of next row */
    vw->trans.x -= yunit.x - frames * xunit.x;
    vw->trans.y -= yunit.y - frames * xunit.y;
    vw->trans.z -= yunit.z - frames * xunit.z;
    if (movie->abort)
      break;
  }
  movie->abort = 1;
  vw->rad = radsave;
  vw->trans = transave;
  imodvDraw(a);
  imodMatDelete(mat);
  return;
}
