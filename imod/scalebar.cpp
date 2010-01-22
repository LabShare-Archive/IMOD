/*
 *  scalebar.cpp - To draw scale bars and manage scale bar dialog
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include "imod.h"
#include "imodv.h"
#include "form_scalebar.h"
#include "scalebar.h"
#include "control.h"
#include "xzap.h"
#include "xxyz.h"
#include "sslice.h"
#include "xcramp.h"
#include "imod_display.h"

// The resident parameters, accessible by other modules
static ScaleBar params = {true, false, 50, 8, false, 0, 20, 20, false, 25, 
                          false, false};

// The instance of the dialog
static ScaleBarForm *sbDia = NULL;

// Open the dialog
void scaleBarOpen()
{
  int type = IMOD_DIALOG;
  DialogManager *manager = &imodDialogManager;
  if (sbDia) {
    sbDia->raise();
    return;
  }
  if (imodvStandalone()) {
    type = IMODV_DIALOG;
    manager = &imodvDialogManager;
  }
  sbDia = new ScaleBarForm(manager->parent(type), Qt::Window);
  if (!sbDia) {
    wprint("/aCould not open Scale Bar dialog\n");
    return;
  }

  manager->add((QWidget *)sbDia, type);
  scaleBarRedraw();
  scaleBarUpdate();
  sbDia->adjustSize();
  sbDia->show();
}

// Process closing event
void scaleBarClosing()
{
  DialogManager *manager = imodvStandalone() ? &imodvDialogManager :
    &imodDialogManager;  
  manager->remove((QWidget *)sbDia);
  sbDia = NULL;
  scaleBarRedraw();
}

/*
 * Assess the scale bar length and start and size for pixel drawing 
 */
float scaleBarAssess(int winx, int winy, float zoom, int &pixlen, int &xst,
                     int &yst, int &xsize, int &ysize)
{
  Imod *imod;
  double expon, minlen, loglen, normlen, custlen;
  float truelen, pixsize;
  int i, j;
  if (!params.draw || !sbDia)
    return -1.;

  imod = imodvStandalone() ? Imodv->imod : App->cvi->imod;
  pixsize = imod->pixsize * (imodvStandalone() ? 1. : App->cvi->xybin);

  // Get minimum length in units, then reduce that to number between 0 and 1.
  minlen = pixsize * params.minLength / zoom;
  loglen = log10(minlen);
  expon = floor(loglen);
  normlen = pow(10., loglen - expon);
  
  // If a custom length is specified, just use it; adjust by 10 either way
  if (params.useCustom) {
    custlen = params.customVal / 10.;
    if (custlen < normlen)
      custlen *= 10.;
    if (custlen >= 10. * normlen)
      custlen /= 10.;
    normlen = custlen;
  } else {

    // Otherwise set to next higher standard number
    if (normlen < 2.)
      normlen = 2.;
    else if (normlen < 5.)
      normlen = 5.;
    else
      normlen = 10.;
  }

  // Get real length then pixel length, starting points
  truelen = (float)normlen * pow(10., expon);
  pixlen = B3DNINT(truelen * zoom / pixsize);
  xsize = params.vertical ? params.thickness : pixlen;
  ysize = params.vertical ? pixlen : params.thickness;
  xst = params.indentX;
  if (params.position == 0 || params.position == 3)
    xst = winx - params.indentX - xsize;
  yst = params.indentY;
  if (params.position == 2 || params.position == 3)
    yst = winy - params.indentY - ysize;
  //imodPrintStderr("SBA: minLength  %d  indentX  %d  normlen %f truelen %f zoom %f\n", params.minLength,
  //              params.indentX, normlen, truelen, zoom);
  return truelen;
}

/*
 * Test size of scale bar for a montage snapshot and adjust to fit in one
 * panel
 */
void scaleBarTestAdjust(int winx, int winy, float zoom)
{
  int pixlen,barXst,barYst, barXsize, barYsize, minIndent, lengthLim;
  float truelen;
  truelen = scaleBarAssess(winx, winy, zoom, pixlen,
                           barXst, barYst, barXsize, barYsize);
  if (truelen > 0 && (barXst < 0 || barYst < 0 || barXst + barXsize >= 
                      winx || barYst + barYsize >= winy)) {
          
    // Need to get the bar all in one panel: first adjust the indent
    minIndent = B3DMIN(params.indentX, params.indentY);
    while ((barXst < 0 || barYst < 0 || barXst + barXsize >= winx ||
            barYst + barYsize >= winy) && 
           (params.indentX >minIndent || params.indentY > minIndent)) {
      if (params.indentX > minIndent)
        params.indentX--;
      if (params.indentY > minIndent)
        params.indentY--;
      scaleBarAssess(winx, winy, zoom, pixlen, barXst,
                     barYst, barXsize, barYsize);
    }
    lengthLim = params.minLength / 2;
    while ((barXst < 0 || barYst < 0 || barXst + barXsize >= winx ||
            barYst + barYsize >= winy) && params.minLength > lengthLim) {
      params.minLength--;
      scaleBarAssess(winx, winy, zoom, pixlen, barXst,
                     barYst, barXsize, barYsize);
    }
    if (barXst < 0 || barYst < 0 || barXst + barXsize >= winx ||
        barYst + barYsize >= winy) {
      imodPrintStderr("Scale bar cannot be adjusted to fit in one "
                      "panel\n");
      params.draw = false;
    } else
      imodPrintStderr("Scale bar position or size was adjusted to fit "
                      "in one panel\n");
  }
}

/*
 * Draw a scale bar for a window, called from inside its paint routine
 */
float scaleBarDraw(int winx, int winy, float zoom, int background)
{
  Imod *imod;
  float truelen;
  int xst, yst, color, pixlen, xsize, ysize, i, j, red, green, blue, index;
  GLboolean depthEnabled;

  truelen = scaleBarAssess(winx, winy, zoom, pixlen, xst, yst, xsize, ysize);
  if (truelen < 0)
    return truelen;
  /*imodPrintStderr("Actual zoom in draw call %f   truelen  %f\n", zoom, truelen);
    imodPrintStderr("SBD: %d %d %d %d %d %d %d\n", winx, winy, pixlen, xst, yst, xsize, ysize);*/

  // Disable depth test and enable at end
  depthEnabled = glIsEnabled(GL_DEPTH_TEST);
  if (depthEnabled)
    glDisable(GL_DEPTH_TEST);

  if (!params.colorRamp) {

    // If a background color is set, take the opposite; otherwise follow option
    color = params.white ? 255 : 0;
    if (background)
      color = background > 0 ? 0 : 255;
    customGhostColor(color, color, color);
    b3dDrawFilledRectangle(xst, yst, xsize, ysize);
  } else {
    
    // Drawing a color ramp
    pixlen = B3DMAX(1, pixlen);
    for (i = 0; i <= pixlen; i++) {
      j = params.invertRamp ? pixlen - i : i;
      index = B3DNINT((255. * j) / pixlen);
      xcramp_mapfalsecolor(index, &red, &green, &blue);
      customGhostColor(red, green, blue);
      if (params.vertical)
        b3dDrawLine(xst, yst + i, xst + xsize, yst + i);
      else
        b3dDrawLine(xst + i, yst, xst + i, yst + ysize);
    }
  }
  resetGhostColor();
  if (depthEnabled)
    glEnable(GL_DEPTH_TEST);

  // Start timer every time this routine draws a bar so updates occur
  sbDia->startUpdateTimer();
  return truelen;
}

/*
 * Update the dialog's listing of scale bars for each kind of window
 */
void scaleBarUpdate()
{
  SlicerStruct *ss;
  ZapStruct *zap;
  Imod *imod;
  float slicerv = -1., zapv = -1., multiZv = -1., modvv = -1., xyzv = -1.;
  if (!sbDia)
    return;

  imod = imodvStandalone() ? Imodv->imod : App->cvi->imod;
  
  if (!imodvStandalone()) {
    ss = getTopSlicer();
    if (ss)
      slicerv = ss->scaleBarSize;
    zap = getTopZapWindow(false, ZAP_WINDOW_TYPE);
    if (zap)
      zapv = zap->scaleBarSize;
    zap = getTopZapWindow(false, MULTIZ_WINDOW_TYPE);
    if (zap)
      multiZv = zap->scaleBarSize;
    xyzv = xyzScaleBarSize();
  }
  if (!ImodvClosed)
    modvv = Imodv->scaleBarSize;
  sbDia->updateValues(zapv, multiZv, slicerv, xyzv, modvv, imodUnits(imod));
}

ScaleBar *scaleBarGetParams()
{
  return &params;
}

// Dialog change calls this to redraw
void scaleBarRedraw()
{
  if (!imodvStandalone())
    imodDraw(App->cvi, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);
  imodv_draw();
}

/*

$Log$
Revision 1.8  2009/05/08 02:18:21  mast
Fixed for binned data

Revision 1.7  2009/01/15 16:33:18  mast
Qt 4 port

Revision 1.6  2008/11/28 06:37:08  mast
Made it redraw when closing to bars go away

Revision 1.5  2008/06/12 22:49:49  mast
Disable depth test to make scalebar come out on top

Revision 1.4  2008/05/27 01:52:11  mast
Added color ramp options

Revision 1.3  2008/03/06 00:11:55  mast
Added option to make scale bars vertical

Revision 1.2  2008/02/15 00:16:30  mast
Make it draw bars when window opens

Revision 1.1  2008/01/25 20:22:02  mast
Added to program



*/
