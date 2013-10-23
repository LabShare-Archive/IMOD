/*
 *  slots.c -- Slot functions for Qt signals, and actions that they cause
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include "midas.h"
#include <qdialog.h>
#include <qlayout.h>
#include <qmessagebox.h>
#include <qfiledialog.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QLabel>
#include "dia_qtutils.h"
#include "arrowbutton.h"
#include "b3dutil.h"
#include "imod_assistant.h"

static int incDecimals[3] = {2, 4, 2};
static int paramDecimals[5] = {2, 4, 4, 2, 2};
static int incDigits[3] = {7, 6, 8};
static int paramDigits[5] = {7, 6, 6, 8, 8};

static float increments[MAX_INCREMENTS][3] =
  {{.05, 1.0005, .1},
   {.1, 1.001, .2},
   {.2, 1.002, .5},
   {.5, 1.005, 1},
   {1, 1.01, 2},
   {2, 1.02, 4 }};
static int paramIncIndex[5] = {0, 1, 1, 2, 2};

MidasSlots::MidasSlots()
{
  mBlackPressed = false;
  mWhitePressed = false;
  mImodHelp = new ImodAssistant("html", "IMOD.adp", "midas");
}

MidasSlots::~MidasSlots()
{
}

/* routines needed to update the menu widgets */

int MidasSlots::index_to_edgeno(int index, int &xory)
{
  int  ipclo, pcx, pcy, edge;
  xory = 0;
  if (index < 0)
    return 0;
  ipclo = VW->piecelower[index];
  xory = index % 2;
  pcx = (VW->xpclist[ipclo] - VW->minxpiece) / (VW->hin->nx - VW->nxoverlap)+1;
  pcy = (VW->ypclist[ipclo] - VW->minypiece) / (VW->hin->ny - VW->nyoverlap)+1;
  edge = lower_piece_to_edgeno(pcx, pcy, xory);
  return edge;
}

// Converts piece numbers numbered from 1 to edge number
int MidasSlots::lower_piece_to_edgeno(int pcx, int pcy, int xory)
{
  if (xory)
    return (pcx - 1) * (VW->nypieces - 1) + pcy;
  return (pcy - 1) * (VW->nxpieces - 1) + pcx;
}

// Converts an edge number to lower piece numbers, all numbered from 1
void MidasSlots::edgeno_to_lower_piece(int edge, int xory, int &pcx, int &pcy)
{
  if (xory) {
    pcx = (edge - 1) / (VW->nypieces - 1) + 1;
    pcy = (edge - 1) % (VW->nypieces - 1) + 1;
  } else {
    pcx = (edge - 1) % (VW->nxpieces - 1) + 1;
    pcy = (edge - 1) / (VW->nxpieces - 1) + 1;
  }
}


void MidasSlots::update_parameters()
{
  QString str;
  int i, first;
  char string[32];
  float param[5], xc, yc;
  float meanerr, toperr[8], meanleave, topleave, curerrx, curerry;
  int topleavind, topxy, edge;
  float *xControl, *yControl, *xVector, *yVector;
  float *mat = VW->tr[VW->cz].mat;
  amatToRotmagstr(mat[0], mat[3], mat[1], mat[4], &param[0], &param[1],
                    &param[2], &VW->phi);

  if (VW->xtype != XTYPE_MONT) {
    xc = (float)VW->xsize * 0.5f;
    yc = (float)VW->ysize * 0.5f;
    param[3] = mat[6];
    param[4] = mat[7];
    first = 0;
    if (VW->editWarps) {
      i = VW->numChunks ? VW->curChunk : VW->cz;
      str.sprintf("Linear trans: %.1f, %.1f", mat[6], mat[7]);
      VW->wLinearTrans->setText(str);
      param[3] = 0.;
      param[4] = 0.;
      if (VW->curControl >= 0 &&
          !getWarpPointArrays(i, &xControl, &yControl, &xVector, &yVector)) {
        param[3] = -xVector[VW->curControl] / VW->warpScale;
        param[4] = -yVector[VW->curControl] / VW->warpScale;
      }
    }
  } else  {
    param[3] = VW->edgedx[VW->edgeind * 2 + VW->xory];
    param[4] = VW->edgedy[VW->edgeind * 2 + VW->xory];
    diaSetChecked(VW->wExcludeEdge, 
                  VW->skippedEdge[VW->edgeind * 2 + VW->xory] != 0);
    first = 3;
    if (!VW->skipErr) {
      find_best_shifts(VW, 0, VW->numTopErr, &meanerr, toperr, VW->topind,
                       &curerrx, &curerry, VW->mousemoving);
      find_best_shifts(VW, 1, 1, &meanleave, &topleave, &topleavind,
                       &VW->curleavex, &VW->curleavey, VW->mousemoving);
      
      str.sprintf("Mean error: %.2f", meanerr);
      VW->wMeanerr->setText(str);
      
      for (i = 0; i < VW->numTopErr; i++) {
        edge = index_to_edgeno(VW->topind[i], topxy);
        str.sprintf("%s %d: %.2f  ", topxy ? "Y" : "X", edge, toperr[i]);
        VW->wToperr[i]->setText(str);
      }
      
      str.sprintf("This edge: %.2f, %.2f", curerrx, curerry);
      VW->wCurerr->setText(str);
      
      str.sprintf("Leave-out: %.2f, %.2f", VW->curleavex, VW->curleavey);
      VW->wLeaverr->setText(str);
    }
  }

  for (i = first; i < 5; i++) {
    if (param[i] == VW->paramstate[i])
      continue;
    VW->paramstate[i] = param[i];
    sprintf_decimals(string, paramDecimals[i], paramDigits[i], param[i]);
    str = string;
    VW->wParameter[i]->setText( str);
  }
}

// Update the section number and current edge number
// DNM 9/25/03: change to coordinates numbered from 1
void MidasSlots::update_sections()
{
  QString str;
  int pcx, pcy;
 
  if (VW->numChunks) {
    diaSetSpinBox(VW->chunkSpin, VW->curChunk + 1);
    diaSetSpinMMVal(VW->curSpin, VW->chunk[VW->curChunk].start + 1, 
                    VW->chunk[VW->curChunk].maxCurSec + 1, VW->cz + 1);
    diaSetSpinMMVal(VW->refSpin, VW->chunk[VW->curChunk - 1].minRefSec + 1, 
                    VW->chunk[VW->curChunk].start, VW->refz + 1);
    return;
  }

  diaSetSpinBox(VW->curSpin, (VW->xtype == XTYPE_MONT ? VW->montcz : VW->cz) + 1);
  if (VW->xtype == XTYPE_MONT) {
    diaSetSpinBox(VW->edgeSpin, VW->curedge);
    edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
    diaSetSpinBox(VW->lowerXspin, pcx);
    diaSetSpinBox(VW->lowerYspin, pcy);
    return;
  }
  diaSetSpinBox(VW->refSpin, VW->refz + 1);
    
  if (!VW->difftoggle)
    return;
  str.sprintf("Keep Cur - Ref diff = %d", VW->cz - VW->refz);
  VW->difftoggle->setText( str);
}

void MidasSlots::updateWarpEdit()
{
  bool state = false;
  int nControl = 0;
  int iz = VW->numChunks ? VW->curChunk : VW->cz;
  if (VW->curWarpFile >= 0 && iz < VW->warpNz && !getNumWarpPoints(iz, &nControl) &&
    nControl > 0) {
    state = true;
    VW->curControl = B3DMAX(0, B3DMIN(nControl - 1, VW->curControl));
  }
  if (VW->warpingOK) {
    VW->warpToggle->setEnabled(nControl < 4);
    VW->wSelectLabel->setEnabled(nControl >= 4);
    VW->wSelectBiggest->setEnabled(nControl >= 4);
    VW->wSelectMore->setEnabled(nControl >= 4);
    VW->wSelectLess->setEnabled(nControl >= 4);
    VW->wDrawVectors->setEnabled(nControl >= 4);
  }
  if ((state ? 1 : 0) != (VW->editWarps ? 1 : 0)) {
    diaSetChecked(VW->warpToggle, state);
    slotEditWarp(state);
  }
}

void MidasSlots::update_overlay()
{
  VW->overlaytoggle->blockSignals(true);
  VW->overlaytoggle->setChecked(VW->vmode == MIDAS_VIEW_COLOR);
  VW->overlaytoggle->blockSignals(false);
}

/* To transform the current slice with a new transform */
void MidasSlots::retransform_slice()
{
  //midasGetSlice(VW, MIDAS_SLICE_CURRENT);  // Redundant! - first thing next call does
  VW->midasGL->update_slice_view();
}

/* General routine for saving transforms in existing or new file */
int MidasSlots::save_transforms()
{
  char *filename;
  struct stat buf;
  int renamedold = 0;

  if (!VW->xname) {
    QString qname = QFileDialog::getSaveFileName
      (VW->midasWindow, "Select name of file to save transforms in");
    if (qname.isEmpty())
      return 1;
    filename = strdup(LATIN1(qname));
    if (!filename)
      return 1;
    if (filename[0] == 0x00){
      free(filename);
      return 1;
    }
    VW->xname = filename;
  }

  /* The first time saving, rename existing file to backup if any */
  if (!VW->didsave && !stat(VW->xname, &buf)) {
    filename = (char *)malloc(strlen(VW->xname) + 2);
    sprintf(filename, "%s~", VW->xname);
    if (!stat(filename, &buf))
      remove(filename);
    if (rename(VW->xname, filename)) {
      midas_error("Error renaming existing file to", filename, 0);
      free(filename);
      free(VW->xname);
      VW->xname = NULL;
      return 1;
    } 
    renamedold = 1;
  }

  if (write_transforms(VW, VW->xname)) {

    /* If error writing, rename old file back, and reset the filename */
    if (renamedold) {
      remove(VW->xname);
      rename(filename, VW->xname);
      free(filename);
    }
    free(VW->xname);
    VW->xname = NULL;
    return 1;
  }

  /* success: mark that save was done, no unsaved changes, and "store" current xform */
  dia_puts("Transforms saved to file.");
  VW->didsave = 1;
  VW->changed = 0;
  backup_current_mat();
  if (renamedold)
    free(filename);
  return 0;
}

void MidasSlots::slotMidas_quit()
{
  int done;

  if (VW->xtype == XTYPE_MONT && !VW->quiet)
    dia_puts("Remember to build new edge functions the next time you "
	       "run Blendmont");
  if (VW->changed) {
    done = dia_choice("Save transforms before quitting?", "Yes", "No", 
                      "Cancel");
    if (done == 3)
      return;
    if (done == 1) {
      if (save_transforms()) {
	midas_error("Error saving transforms to file. ", 
		    "Quit cancelled", 0);
	return;
      }
    }
  }
  if (mImodHelp)
    delete mImodHelp;
  qApp->quit();
}


void MidasSlots::slotFilemenu(int item)
{
  char *filename;
  QString qname, inName;
  const char *filters[] = {"Transform files (*.*xf *.*xg)"};
  switch(item){
  case FILE_MENU_LOAD: /* load transforms */
    if (VW->xtype == XTYPE_MONT) {
      dia_puts("New displacements cannot be loaded "
	       "in montage fixing mode");
      break;
    }
    if (VW->changed) {
      if (dia_ask("Save existing transforms to file?")) {
	if (save_transforms()) {
	  midas_error("Existing transforms not saved. ", 
		      "Load aborted", 0);
	  break;
	}
      }
    }	    
    qname = diaOpenFileName(NULL, "Select file to load transforms from", 1, filters);
    if (qname.isEmpty())
      break;
    filename = strdup(LATIN1(qname));
    if (!filename)
      break;
    if (filename[0] == 0x00){
      free(filename);
      break;
    }
    if (load_transforms(VW, filename)) {
      midas_error("Error opening new file. ", 
		  "Existing transforms are still in effect", 0);
      free(filename);
      break;
    }

    /* If successful, free an old name and save the new one, mark as
       not saved and not changed */
    if (VW->xname)
      free(VW->xname);
    VW->xname = filename;
    VW->didsave = 0;
    VW->changed = 0;
    update_parameters();
    VW->midasGL->draw();
    break;

  case FILE_MENU_SAVE: /* save transforms */
    if (save_transforms())
      midas_error("Error saving to file. ", "Transforms not saved",
		  0);
    break;

  case FILE_MENU_SAVE_AS: /* save transforms as ...*/
	  /* save existing filename, in case of failure */
    filename = VW->xname;
    VW->xname = NULL;
    if (save_transforms()) {
      VW->xname = filename;
      midas_error("Error saving to new file. ", "Transforms not "
		  "saved and existing file, if any, still open", 0);
      break;
    }
    if (filename) 
      free(filename);
    break;

  case FILE_MENU_SAVE_IMAGE: /* Save contrast-adjusted image */
    if (VW->xtype == XTYPE_MONT) {
      dia_puts("Contrast-adjusted image cannot be saved "
	       "in montage fixing mode");
      break;
    }
    qname = QFileDialog::getSaveFileName
       (VW->midasWindow, "Enter file to save");
    if (qname.isEmpty())
      break;
    filename = strdup(LATIN1(qname));
    if (!filename)
      break;
    if (filename[0] == 0x00){
      free(filename);
      break;
    }
#ifdef WAITFUNC
    int cpid;
    if ((cpid = fork()) == 0) {
      save_view(VW, filename);
      exit(0);
    }else{
      dia_abort("Saving file.", cpid);
    }
#else
    dia_puts("Please wait until the \"Image file saved\" "
	     "window pops up");
    save_view(VW, filename);
    dia_puts("Image file saved.");
#endif
    free(filename);
    break; 

  case FILE_MENU_TRANSFORM: /* transform model */
    if (VW->xtype == XTYPE_XF || VW->xtype == XTYPE_MONT || VW->curWarpFile >= 0) {
      dia_puts("Models may not be transformed in local "
	       "alignment or montage fixing mode or when warping");
      break;
    }
    inName = diaOpenFileName
      (VW->midasWindow, "Select model file to transform", 0, NULL);
    if (inName.isEmpty())
      break;
    qname = QFileDialog::getSaveFileName
      (VW->midasWindow, "Enter file to save transformed model into");
    if (qname.isEmpty())
      break;

    transform_model(LATIN1(inName), LATIN1(qname), VW);
    break;

case FILE_MENU_QUIT: /* Quit */
    slotMidas_quit();
    break;
  }
  return;
}

void MidasSlots::slotEditmenu(int item)
{
  struct Midas_transform *tr;
  int i, ist, ind, nControl;
  float prod[9];
  float *inv;
     
  tr = &(VW->tr[VW->cz]);

  switch(item){
  case EDIT_MENU_STORE: /* store transform, means refresh the backup */
    backup_current_mat();
    break;

  case EDIT_MENU_RESET: /* reset to unit transform */
	  /* get inverse of current one and multiply by that */
    inv = tramat_inverse(tr->mat);
    getChangeLimits(&ist, &ind);

    for (i = ist; i <= ind; i++) {
      tr = &(VW->tr[i]);
      tramat_multiply(tr->mat, inv, prod);
      tramat_copy(prod, tr->mat);
    }
    tramat_free(inv);
    /*  tramat_idmat(tr->mat); */

    if (VW->xtype == XTYPE_MONT) {
      VW->edgedx[VW->edgeind * 2 + VW->xory] = 0.;
      VW->edgedy[VW->edgeind * 2 + VW->xory] = 0.;
      set_mont_pieces(VW);
    }

    if (VW->curWarpFile >= 0) {
      i = VW->numChunks ? VW->curChunk : VW->cz;
      setWarpPoints(i, 0, NULL, NULL, NULL, NULL);
    }

    synchronizeChunk(VW->cz);
    update_parameters();
    updateWarpEdit();
    retransform_slice();
    VW->changed = 1;
    break;
	  
  case EDIT_MENU_REVERT: /* Revert to backup transform */
	  /* get inverse of current one, times backup matrix, and multiply by 
	     that */
    inv = tramat_inverse(tr->mat);
    tramat_multiply(inv, VW->backup_mat, prod);
    getChangeLimits(&ist, &ind);
    for (i = ist; i <= ind; i++) {
      tr = &(VW->tr[i]);
      tramat_multiply(tr->mat, prod, inv);
      tramat_copy(inv, tr->mat);
    }
    tramat_free(inv);
    /* for (i = 0; i < 9; i++)
       tr->mat[i] = VW->backup_mat[i]; */

    if (VW->xtype == XTYPE_MONT) {
      VW->edgedx[VW->edgeind * 2 + VW->xory] = VW->backup_edgedx;
      VW->edgedy[VW->edgeind * 2 + VW->xory] = VW->backup_edgedy;
      set_mont_pieces(VW);
    }

    if (VW->curWarpFile >= 0) {
      i = VW->numChunks ? VW->curChunk : VW->cz;
      if (setWarpPoints(i, VW->numWarpBackup, VW->backupXcontrol, VW->backupYcontrol,
                        VW->backupXvector, VW->backupYvector))
        midas_error("An error occurred restoring the warping points", "", 0);
    }

    synchronizeChunk(VW->cz);
    update_parameters();
    updateWarpEdit();
    retransform_slice();
    VW->changed = 1;
    break;

  case EDIT_MENU_MIRROR: /* Mirror around X axis  */
	  /* get unit transform and modify to mirror */
    i = VW->numChunks ? VW->curChunk : VW->cz;
    if (VW->curWarpFile >= 0 && i < VW->warpNz && getNumWarpPoints(i, &nControl) &&
        nControl > 3)
      break;
    inv = tramat_create();
    inv[4] = -1.0;
    getChangeLimits(&ist, &ind);

    for (i = ist; i <= ind; i++) {
      tr = &(VW->tr[i]);
      tramat_multiply(tr->mat, inv, prod);
      tramat_copy(prod, tr->mat);
    }
    tramat_free(inv);

    synchronizeChunk(VW->cz);
    update_parameters();
    retransform_slice();
    VW->changed = 1;
    break;
	  
  case EDIT_MENU_DELETEPT:  /* Delete current control point */
    i = VW->numChunks ? VW->curChunk : VW->cz;
    if (VW->curWarpFile < 0 || i >= VW->warpNz || getNumWarpPoints(i, &nControl) || 
        !nControl || VW->curControl < 0 || VW->curControl >= nControl)
      break;
    removeWarpPoint(i, VW->curControl);
    VW->curControl = B3DMIN(nControl - 2, VW->curControl);
    reduceControlPoints(VW);
    VW->warpToggle->setEnabled(nControl < 5);  // 5 not 4 because it is 1 too high
    update_parameters();
    retransform_slice();
    VW->changed = 1;
    break;
  }
  return;
}

void MidasSlots::slotHelpmenu(int item)
{
  switch(item){
  case HELP_MENU_CONTROLS: 
     showHelpPage("midasHelp/controls.html#TOP");
    break;
  case HELP_MENU_HOTKEYS:
    showHelpPage("midasHelp/keyboard.html#TOP");
    break;
  case HELP_MENU_MOUSE:
     showHelpPage("midasHelp/mouse.html#TOP");
    break;
  case HELP_MENU_MANPAGE:
    showHelpPage("man/midas.html");
    break;
  case HELP_MENU_ABOUT: /* About */
    dia_vasmsg("Midas version",
	       MIDAS_VERSION_STRING,
	       "\n",
	       "Copyright (C)",COPYRIGHT_YEARS,"by",LAB_NAME1,"\n",
	       LAB_NAME2,"and Regents of the University of",
	       "Colorado\n",
	       "Written by James Kremer and David Mastronarde.\n",
	       NULL);
    break;
  }
}

/* Return starting and ending limits for transforms to change: either the
   current one, from the current one to the appropriate end in XG mode */
void MidasSlots::getChangeLimits (int *ist, int *ind)
{
  int cst, cnd, i, j;
  *ist = VW->cz;
  *ind = VW->cz;

  // Copy current transform to use for modifying warp points
  tramat_copy(VW->tr[VW->cz].mat, VW->oldMat);
  if (VW->xtype != XTYPE_XG)
    return;

  /* set other limit based on direction of comparisons */
  if (VW->refz < VW->cz) {
    *ind = VW->zsize - 1;
    cst = VW->cz + 1;
    cnd = VW->zsize - 1;
  } else {
    *ist = 0;
    cst = 0;
    cnd = VW->cz - 1;
  }
     
  /* clear the cache of transformed images for all but current section */
  for (i = cst; i <= cnd; i++)
    for (j = 0; j < VW->cachesize; j++)
      if (VW->cache[j].zval == i && VW->cache[j].xformed) {
	VW->cache[j].zval = -1;
	VW->cache[j].used = -1;
      }
}

void MidasSlots::rotate(float step)
{
  struct Midas_transform *tr;
  int i, ist, ind;
  float xofs = VW->xcenter - VW->xsize / 2.;
  float yofs = VW->ycenter - VW->ysize / 2.;
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -xofs, -yofs);
    tramat_rot(tr->mat, step); 
    tramat_translate(tr->mat, xofs, yofs);
  }
  adjustControlPoints(VW);
  synchronizeChunk(VW->cz);
  update_parameters();
  retransform_slice();
  VW->changed = 1;
}

void MidasSlots::translate(float xstep, float ystep)
{
  struct Midas_transform *tr;
  static int fastcount = 0;
  int ix = (int)xstep;
  int iy = (int)ystep;
  int i, ist, ind;
  float *xControl, *yControl, *xVector, *yVector;

  if (VW->editWarps) {

    // Translate a control point vector, use negative since it is an inverse
    i = VW->numChunks ? VW->curChunk : VW->cz;
    if (VW->curControl >= 0 && !getWarpPointArrays(i, &xControl, &yControl, &xVector,
                                                   &yVector)) {
      xVector[VW->curControl] -= xstep * VW->warpScale;
      yVector[VW->curControl] -= ystep * VW->warpScale;
      reduceControlPoints(VW);
    }
  } else {

    // Translate images
    getChangeLimits(&ist, &ind);
    for (i = ist; i <= ind; i++) {
      tr = &(VW->tr[i]);
      tramat_translate(tr->mat, xstep, ystep);
    }
    adjustControlPoints(VW);
  }

  /* If montage, maintain edge displacements too */
  if (VW->xtype == XTYPE_MONT) {
    VW->edgedx[VW->edgeind * 2 + VW->xory] += xstep;
    VW->edgedy[VW->edgeind * 2 + VW->xory] += ystep;
  }

  synchronizeChunk(VW->cz);
  update_parameters();
  if (ix == xstep && iy == ystep && ++fastcount % 10 && !VW->editWarps) {
    translate_slice(VW, ix, iy);
    VW->midasGL->update_slice_view();
  } else {
    fastcount = 0;
    retransform_slice();
  }
  VW->changed = 1;
}

void MidasSlots::scale(float step)
{
  struct Midas_transform *tr;
  int i, ist, ind;
  float xofs = VW->xcenter - VW->xsize / 2.;
  float yofs = VW->ycenter - VW->ysize / 2.;
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -xofs, -yofs);
    tramat_scale(tr->mat, step, step); 
    tramat_translate(tr->mat, xofs, yofs);
  }
  adjustControlPoints(VW);
  synchronizeChunk(VW->cz);
  update_parameters();
  retransform_slice();
  VW->changed = 1;
}

void MidasSlots::stretch(float step, float angle)
{
  struct Midas_transform *tr;
  int i, ist, ind;
  float xofs = VW->xcenter - VW->xsize / 2.;
  float yofs = VW->ycenter - VW->ysize / 2.;
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -xofs, -yofs);
    tramat_rot(tr->mat, -angle);
    tramat_scale(tr->mat, step, 1.0f); 
    tramat_rot(tr->mat, angle);
    tramat_translate(tr->mat, xofs, yofs);
  }
  adjustControlPoints(VW);
  synchronizeChunk(VW->cz);
  update_parameters();
  retransform_slice();
  VW->changed = 1;
}


void MidasSlots::slotParameter(int item)
{
  int which = item;
  float step, increment;
  float sign = 1.;
  if (which < 0) {
    which = -which;
    sign = -1;
  }
  increment = VW->increment[paramIncIndex[which - 1]];

  switch (which) {
  case 1:
    rotate(-sign * increment);
    break;
  case 2:
    step = increment;
    if (sign < 0)
      step = 1. / step;
    scale(step);
    break;
  case 3:
    step = increment;
    if (sign < 0)
      step = 1. / step;
    stretch(step, VW->sangle * 0.1f);
    break;
  case 4:
    translate(sign * increment, 0.);
    break;
  case 5:
    translate(0, sign * increment);
    break;
  }
}

	  

void MidasSlots::slotIncrement(int item)
{
  QString str;
  char string[32];
  int which = item;
  if (which > 0) {
    VW->incindex[--which]++;
    if (VW->incindex[which] >= MAX_INCREMENTS)
      VW->incindex[which] = MAX_INCREMENTS - 1;
  } else {
    which = -which - 1;
    VW->incindex[which]--;
    if (VW->incindex[which] < 0)
      VW->incindex[which] = 0;
  }
  VW->increment[which] = increments[VW->incindex[which]][which];
  sprintf_decimals(string, incDecimals[which], incDigits[which],
		   VW->increment[which]);
  str = string;
  VW->wIncrement[which]->setText(str);
}

void MidasSlots::slotAngle(int value)
{
  QString str;
  VW->sangle = value;
  str.sprintf("%.1f", value / 10.);
  VW->anglelabel->setText(str);
  VW->midasGL->draw();
  return;
}

/* section change functions */
int MidasSlots::get_bw_index()
{
  int ind = VW->cz;
  if (VW->showref) {
    ind = VW->refz;
    if (VW->xtype == XTYPE_XREF)
      ind = VW->zsize;
  }
  return ind;
}

void MidasSlots::try_section_change(int ds, int dsref)
{
  int ind, doBlackWhite = 0;
  int newcur = ds + VW->cz;
  int newref = dsref + VW->refz;
  if (newcur < 0 || newcur >= VW->zsize || newref < 0 || 
      (VW->xtype != XTYPE_XREF && newref >= VW->zsize) ||
      (VW->xtype == XTYPE_XREF && newref >= VW->refzsize) ||
      (VW->numChunks && (newcur < VW->chunk[VW->curChunk].start ||
                         newcur > VW->chunk[VW->curChunk].maxCurSec ||
                         newref < VW->chunk[VW->curChunk - 1].minRefSec ||
                         newref >= VW->chunk[VW->curChunk].start))) {
    update_sections();
    return;
  }

  if (VW->xtype == XTYPE_XREF && dsref) {
    VW->xsec = newref;    

    // Try to load; if it fails, reload original
    if (load_refimage(VW, VW->refname)) {
      VW->xsec = VW->refz;
      load_refimage(VW, VW->refname);
      update_sections();
      return;
    }

    // Do not fiddle with sliders if only the reference section changed
    doBlackWhite = ds;
  }

  VW->refz += dsref;
  VW->cz += ds;
  VW->xcenter = 0.5 * VW->xsize;
  VW->ycenter = 0.5 * VW->ysize;
  updateWarpEdit();
  update_parameters();
  if (doBlackWhite) {
    ind = get_bw_index();
    setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  } else {
    VW->midasGL->fill_viewdata(VW);
    VW->midasGL->draw();
  }
  update_sections();
  backup_current_mat();

  // Save the sections being displayed for a chunk
  if (VW->numChunks) {
    VW->chunk[VW->curChunk].curSec = newcur;
    VW->chunk[VW->curChunk - 1].refSec = newref;
  }
}

void MidasSlots::try_montage_section(int sec, int direction)
{
  int newcur;
  if (sec < VW->minzpiece)
    sec = VW->minzpiece;
  if (sec > VW->maxzpiece)
    sec = VW->maxzpiece;
  newcur = nearest_section(VW, sec, direction);
  if (newcur < VW->minzpiece || newcur == VW->montcz) {
    update_sections();
    return;
  }
  VW->montcz = newcur;
  manage_xory(VW);
  VW->curedge = nearest_edge(VW, VW->montcz, VW->xory, VW->curedge, 0, 
			     &VW->edgeind);
  finishNewEdge();
}

void MidasSlots::sectionInc(int ds)
{
  int dsref = 0;
  if (VW->keepsecdiff)
    dsref = ds;
  if (VW->xtype == XTYPE_MONT)
    try_montage_section(VW->montcz + ds, ds);
  else
    try_section_change(ds, dsref);
}

// The user enters a current section
// DNM 9/25/03: change to coordinates numbered from 1
void MidasSlots::slotCurValue(int sec)
{
  sec--;
  if (VW->xtype == XTYPE_MONT)
    try_montage_section(sec, 0);
  else {
    sec -= VW->cz;
    sectionInc(sec);
  }
  VW->midasWindow->setFocus();
}

// The user enters a reference section
void MidasSlots::slotRefValue(int sec)
{
  int ds;
  sec--;
  sec -= VW->refz;
  ds = 0;
  if (VW->keepsecdiff)
    ds = sec;
  try_section_change(ds, sec);
  VW->midasWindow->setFocus();
}

// A new chunk pair is selected
void MidasSlots::slotChunkValue(int sec)
{
  sec--;
  if (sec < 1 || sec >= VW->numChunks) {
    update_sections();
  } else {
  
    // Change the current chunk and go to the sections last viewed
    VW->curChunk = sec;
    try_section_change(VW->chunk[VW->curChunk].curSec - VW->cz,
                       VW->chunk[VW->curChunk - 1].refSec - VW->refz);
  }
  VW->midasWindow->setFocus();
}

/* Functions for changing the current edge */
void MidasSlots::try_montage_edge(int sec, int direction)
{
  int newcur;
  if (sec > VW->maxedge[VW->xory])
    sec = VW->maxedge[VW->xory];
  if (sec < 1)
    sec = 1;
  newcur = nearest_edge(VW, VW->montcz, VW->xory, sec, direction,
			&VW->edgeind);
  if (!newcur || newcur == VW->curedge) {
    update_sections();
    return;
  }
  VW->curedge = newcur;
  finishNewEdge();
}

void MidasSlots::slotEdge(int upDown)
{
  try_montage_edge(VW->curedge + upDown, upDown);
}

void MidasSlots::slotEdgeValue(int sec)
{
  int upDown = sec - VW->curedge;
  if (upDown != 1 && upDown != -1)
    upDown = 0;
  try_montage_edge(sec, upDown);
  VW->midasWindow->setFocus();
}

// Try to switch to a new lower piece; xory is -1 or 1 to move only along
// the X or Y axis, and direction is -1 or 1 to move in given direction
void MidasSlots::try_lower_piece(int pcx, int pcy, int xory, int direction)
{
  int base = (VW->montcz - VW->minzpiece) * VW->nxpieces * VW->nypieces;
  int limit = VW->nxpieces * VW->nypieces;
  int mindist, dist, xtry, ytry, target, ind, maxbelow, minabove, index;
  int found = 0;
  if (xory) {
    maxbelow = -1;
    minabove = limit + 1;

    // Move along the axis and find the last piece below and the first one 
    // above the target
    if (xory < 0) {
      target = pcx;
      for (xtry = 1; xtry <= VW->nxpieces - (VW->xory ? 0 : 1); xtry++) {
        ind = (xtry - 1) + (pcy - 1) * VW->nxpieces + base;
        if (includedEdge(ind, VW->xory)) {
          if (xtry < pcx)
            maxbelow = xtry;
          else if (xtry == pcx) {
            maxbelow = minabove = xtry;
            break;
          } else {
            minabove = xtry;
            break;
          }
        }
      }
    } else {
      target = pcy;
      for (ytry = 1; ytry <= VW->nypieces - (VW->xory ? 1 : 0); ytry++) {
        ind = (pcx - 1) + (ytry - 1) * VW->nxpieces + base;
        if (includedEdge(ind, VW->xory)) {
          if (ytry < pcy)
            maxbelow = ytry;
          else if (ytry == pcy) {
            maxbelow = minabove = ytry;
            break;
          } else {
            minabove = ytry;
            break;
          }
        }
      }
    }

    // Assign based on direction and what was found
    if (direction < 0) {
      if (maxbelow >= 0)
          found = maxbelow;
    } else if (direction > 0) {
      if (minabove <= limit)
        found = minabove;
    } else if (maxbelow > 0 || minabove <= limit) {
      if (maxbelow < 0)
        found = minabove;
      else if (minabove > limit)
        found = maxbelow;
      else {
        found = maxbelow;
        if (target - maxbelow > minabove - target)
          found = minabove;
      }
    }
    if (found && xory < 0) {
      index = (found - 1) + (pcy - 1) * VW->nxpieces + base;
      found = lower_piece_to_edgeno(found, pcy, VW->xory);
    } else if(found) {
      index = (pcx - 1) + (found - 1) * VW->nxpieces + base;
      found = lower_piece_to_edgeno(pcx, found, VW->xory);
    }
    
  } else {

    // Or, if no preferred axis, just find the nearest piece with an edge
    mindist = 1000000000;
    for (ytry = 1; ytry < VW->nypieces && mindist; ytry++) {
      for (xtry = 1; xtry < VW->nxpieces && mindist; xtry++) {
        ind = (xtry - 1) + (ytry - 1) * VW->nxpieces + base;
        if (includedEdge(ind, VW->xory)) {
          dist = (xtry - pcx) * (xtry - pcx) + (ytry - pcy) * (ytry - pcy);
          if (dist < mindist) {
            mindist = dist;
            found = lower_piece_to_edgeno(xtry, ytry, VW->xory);
            index = (xtry - 1) + (ytry - 1) * VW->nxpieces + base;
          }
        }  
      }
    }
  }          

  if (!found) {
    update_sections();
    return;
  }
  VW->curedge = found;
  VW->edgeind = VW->edgeupper[VW->xory + 2 * VW->montmap[index]];
  finishNewEdge();
}

// Common tasks after changing the edge
void MidasSlots::finishNewEdge()
{
  int ind;
  set_mont_pieces(VW);
  ind = get_bw_index();
  update_parameters();
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_sections();
  backup_current_mat();
}

// A new X value for lower piece
void MidasSlots::slotLowerXvalue(int sec)
{
  int pcx, pcy, upDown;
  edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
  upDown = sec - pcx;
  if (upDown != 1 && upDown != -1)
    upDown = 0;
  try_lower_piece(sec, pcy, -1, upDown);
  VW->midasWindow->setFocus();
}

// A new Y value for lower piece
void MidasSlots::slotLowerYvalue(int sec)
{
  int pcx, pcy, upDown;
  edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
  upDown = sec - pcy;
  if (upDown != 1 && upDown != -1)
    upDown = 0;
  try_lower_piece(pcx, sec, 1, upDown);
  VW->midasWindow->setFocus();
}

// Take one step of lower piece in X or Y
void MidasSlots::stepLowerXorY(int xory, int direction)
{
  int pcx, pcy;
  edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
  if (xory < 0)
    pcx += direction;
  else
    pcy += direction;
  if (pcx >= 1 && pcx <= VW->nxpieces && pcy >= 1 && pcy <= VW->nypieces)
    try_lower_piece(pcx, pcy, xory, direction);
}

// Step through warp points in order of vector size
void MidasSlots::slotSelectWarpPointBySize(int direction)
{
  float *lengths;
  int *indices;
  int i, nControl, nexti;
  float *xControl, *yControl, *xVector, *yVector;
  if (!VW->editWarps || VW->curControl < 0)
    return;
  i = VW->numChunks ? VW->curChunk : VW->cz;
  if (getNumWarpPoints(i, &nControl) || nControl < 2)
    return;
  
  if (getWarpPointArrays(i, &xControl, &yControl, &xVector, &yVector)) {
    midas_error("Error getting control points", "", 0);
    return;
  }
  indices = B3DMALLOC(int, nControl);
  lengths = B3DMALLOC(float, nControl);
  if (!indices || !lengths) {
    midas_error("Memory error", "", 0);
    B3DFREE(indices);
    B3DFREE(lengths);
    return;
  }
  
  for (i = 0; i < nControl; i++) {
    indices[i] = i;
    lengths[i] = (float)sqrt((double)xVector[i] * xVector[i] + yVector[i] * yVector[i]);
  }
  rsSortIndexedFloats(lengths, indices, nControl);
  if (!direction) {
      VW->midasGL->newCurrentControl(indices[nControl - 1], true);
  } else {
    for (i = 0; i < nControl; i++) {
      if (indices[i] == VW->curControl) {

        nexti = i + (direction > 0 ? 1 : -1);
        B3DCLAMP(nexti, 0, nControl - 1);
        VW->midasGL->newCurrentControl(indices[nexti], false);
        break;
      }
    }
  }
  VW->midasSlots->update_parameters();
  B3DFREE(indices);
  B3DFREE(lengths);
}

void MidasSlots::slotDrawVectors(bool state)
{
  VW->drawVectors = state;
  VW->midasGL->draw();
}

// Change the X or Y selection
void MidasSlots::slotXory(int which)
{
  int pcx, pcy;
  if (which == VW->xory)
    return;
  edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
  VW->xory = which;
  flush_xformed(VW);

  diaSetGroup(VW->edgeGroup, VW->xory);
  
  /* Keep the lower piece number the same if possible.  Set curedge to 0 to
     force it to recognize this as a new edge and do updates */
  VW->curedge = 0;
  manage_xory(VW);
  try_lower_piece(pcx, pcy, 0, 0);
}

void MidasSlots::manage_xory(MidasView *vw)
{
  int onlyone = -1;
  int ind;
  bool state;

  /* Check if there are edges in each direction */
  if (!nearest_edge(vw, vw->montcz, 0, 1, 0, &ind))
    onlyone = 1;
  if (!nearest_edge(vw, vw->montcz, 1, 1, 0, &ind))
    onlyone = 0;

  /* If there are edges in only one direction, set xory value properly
     and flush any transformed images if xory changed */
  if (onlyone >= 0 && vw->xory != onlyone) {
    vw->xory = onlyone;
    flush_xformed(vw);
  }

  if (!vw->wXedge)
    return;
  VW->edgeSpin->setRange(1, VW->maxedge[VW->xory]);
  diaSetGroup(VW->edgeGroup, VW->xory);
  VW->lowerXspin->setRange(1, VW->nxpieces - (vw->xory ? 0 : 1));
  VW->lowerYspin->setRange(1, VW->nypieces - (vw->xory ? 1 : 0));
  VW->lowerXspin->setEnabled(onlyone != 0);
  VW->lowerYspin->setEnabled(onlyone != 1);

  state = onlyone < 0;
  vw->wXedge->setEnabled(state);
  vw->wYedge->setEnabled(state);
  if (vw->xory != vw->centerXory) {
    vw->xcenter = vw->xsize - 
      (vw->xory ? vw->xsize : vw->nxoverlap / vw->binning) / 2;
    vw->ycenter = vw->ysize - 
      (vw->xory ? vw->nyoverlap / vw->binning : vw->ysize) / 2;
    vw->centerXory = vw->xory;
  }
}

void MidasSlots::slotLeave_out()
{
  translate(VW->curleavex, VW->curleavey);
}

void MidasSlots::slotTop_error(int item)
{
  int topxy, edge;
  edge = index_to_edgeno(VW->topind[item], topxy);
  if (!edge)
    return;
  if (VW->xory != topxy) {
    VW->xory = topxy;
    flush_xformed(VW);
    VW->curedge = 0;
  }
  manage_xory(VW);
  try_montage_edge(edge, 0);
}

void MidasSlots::slotSkipError(bool state)
{
  VW->skipErr = state ? 1 : 0;
  VW->wMeanerr->setEnabled(!state);
  VW->wCurerr->setEnabled(!state);
  VW->wLeaverr->setEnabled(!state);
  VW->wApplyLeave->setEnabled(!state);
  for (int i = 0; i < VW->numTopErr; i++)
    VW->wToperr[i]->setEnabled(!state);
  update_parameters();
}

void MidasSlots::slotSkipExcluded(bool state)
{
  int pcx, pcy;
  VW->excludeSkipped = state;
  edgeno_to_lower_piece(VW->curedge, VW->xory, pcx, pcy);
  if (state) {
    manage_xory(VW);
    try_lower_piece(pcx, pcy, 0, 0);
  }
  update_parameters();
}

void MidasSlots::slotExcludeEdge(bool state)
{
  VW->skippedEdge[VW->edgeind * 2 + VW->xory] = state ? 1 : 0;
  VW->wSkipExcluded->setEnabled(true);
  VW->anySkipped = 1;
  update_parameters();
}

void MidasSlots::slotRobustFit(bool state)
{
  VW->robustFit = state ? 1 : 0;
  update_parameters();
  VW->robustSpin->setEnabled(state);
}

void MidasSlots::slotRobustCrit(double value)
{
  VW->midasWindow->setFocus();
  VW->robustCrit = (float)value;
  update_parameters();
}

static float zooms[MAX_ZOOMIND] = {-8., -6., -4., -3., -2., 
				   1., 1.5, 2., 3., 4., 6., 8., 10.};
void MidasSlots::slotZoom(int upDown)
{
  QString str;
  int dz = upDown;

  VW->zoomind += dz;

  if (VW->zoomind < 0)
    VW->zoomind = 0;

  if (VW->zoomind >= MAX_ZOOMIND)
    VW->zoomind = MAX_ZOOMIND - 1;

  VW->zoom = zooms[VW->zoomind];
  VW->truezoom = VW->zoom;
  if (VW->zoom < 0.)
    VW->truezoom = -1. / VW->zoom;

  if (dz < 0)
    VW->midasGL->midas_clear();

  VW->midasGL->draw();

  if (VW->zoom > 0)
    str.sprintf("Zoom %.1f", VW->zoom);
  else
    str.sprintf("Zoom 1/%d", (int)-VW->zoom);
  VW->zoomlabel->setText( str);
}

void MidasSlots::slotInterpolate(bool state)
{
  VW->fastInterp = state ? 0 : 1;
  flush_xformed(VW);
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
}

void MidasSlots::display_bwslider_value(QLabel *w, int white)
{
  QString str;
  str.sprintf("%03d", white);

  // In RHEL6 and FC12, the white failed to update for several seconds or at all.
  // Printing any diagnostic made it work!  This one call seems to fix the problem
  qApp->processEvents();
  w->setText(str);
}

int MidasSlots::setbwlevels(int black, int white, int draw)
{
  int remute = FALSE;
  int ind, maxind, b0, w0, bn, wn, bs, ws, i;
  float bnf, dslope;

  if (VW->blackstate != black){
    VW->blackstate = black;
    diaSetSlider(VW->wBlacklevel, black);
    display_bwslider_value(VW->wBlackval, black);
    remute = TRUE;
  }

  if (VW->whitestate != white){
    VW->whitestate = white;
    diaSetSlider(VW->wWhitelevel, white);
    display_bwslider_value(VW->wWhiteval, white);
    remute = TRUE;
  }

  /* If nothing changed, just return */
  if (!remute) {
    if (draw) {
      VW->midasGL->fill_viewdata(VW);
      VW->midasGL->draw();
    }
    return 0;
  }

  /* Get index of transform being changed, and maximum index of others */
  ind = get_bw_index();
  maxind = VW->zsize;
  if (VW->xtype == XTYPE_XREF)
    maxind++;
     
  /* If applying to only one section, or there is no change in this 
     section's b & w, just change it, draw, and return */
  if (VW->applytoone || (VW->tr[ind].black == black && 
			 VW->tr[ind].white == white)) {
    VW->tr[ind].black = black;
    VW->tr[ind].white = white;
    VW->midasGL->fill_viewdata(VW);
    VW->midasGL->draw();
    return 1;
  }

  /* If applying to all, change each one by an amount that should map
     previously displayed intensities the same for all sections */
  b0 = VW->tr[ind].black;
  w0 = VW->tr[ind].white;
  for (i = 0; i < maxind; i++) {
    bs = VW->tr[i].black;
    ws = VW->tr[i].white;
    /* If an exact match to reference values, just take new ones */
    if (bs == b0 && ws == w0) {
      VW->tr[i].black = black;
      VW->tr[i].white = white;
    } else {
      dslope = (float)(ws - bs) / (float)(w0 - b0);
      bnf = bs + (black - b0) * dslope;
      /* compute nearest integer values to what equations give */
      wn = (int)floor(bnf + (white - black) * dslope + 0.5);
      bn = (int)floor(bnf + 0.5);

      /* enforce legality, differing by > 0 and within bounds */
      if (wn <= bn)
	wn = bn + 1;
      if (bn < 0) {
	bn = 0;
	if (wn < 1)
	  wn  = 1;
      }
      if (wn > 255) {
	wn = 255;
	if (bn > 254)
	  bn = 254;
      }

      /* save the new values */
      VW->tr[i].white = wn;
      VW->tr[i].black = bn;
    }
  }
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
  return 1;
}

void MidasSlots::slotBlacklevel(int value)
{
  int white, black;

  black = value;
  white = VW->whitestate;
  mBlackDisplayed = black;

  /* If slider is being dragged with Ctrl down, just display number */
   if (mBlackPressed && VW->ctrlPressed) {
     display_bwslider_value(VW->wBlackval, black);
     return;
   }

  if (black == 255)
    black = 254;
  if (black > white)
    white = black + 1;

  setbwlevels(black, white, 0);

}

void MidasSlots::slotWhitelevel(int value)
{
  int black, white;

  white = value;
  black = VW->blackstate;
  mWhiteDisplayed = white;

  /* If slider is being dragged with Ctrl down, just display number */
  if (mWhitePressed && VW->ctrlPressed) {
    display_bwslider_value(VW->wWhiteval, white);
    return;
  }

  if (!white)
    white = 1;
  if (white < black)
    black = white - 1;

  setbwlevels(black, white, 0);
}

// Routines to keep track of sliders being pressed and released
void MidasSlots::slotBlackPressed()
{
  mBlackPressed = true;
  mBlackDisplayed = VW->blackstate;
}

void MidasSlots::slotBlackReleased()
{
  mBlackPressed = false;
  slotBlacklevel(mBlackDisplayed);
}

void MidasSlots::slotWhitePressed()
{
  mWhitePressed = true;
  mWhiteDisplayed = VW->whitestate;
}

void MidasSlots::slotWhiteReleased()
{
  mWhitePressed = false;
  slotWhitelevel(mWhiteDisplayed);
}


/* Simple state changes */
void MidasSlots::slotApplyone(bool state)
{
    VW->applytoone = state ? 1 : 0;
}

void MidasSlots::slotKeepdiff(bool state)
{
    VW->keepsecdiff = state ? 1 : 0;
}

void MidasSlots::slotEditWarp(bool state)
{
  int i;
  float dx, dy, dz;
  // Open a new warping file if there is not one yet
  if (state && VW->curWarpFile < 0) {
    VW->warpNz = 0;
    mrc_get_scale(VW->hin, &dx, &dy, &dz);
    VW->warpScale = VW->binning;
    VW->curWarpFile = newWarpFile(VW->xsize * VW->binning, VW->ysize * VW->binning, 1,
                                  dx, WARP_INVERSE | WARP_CONTROL_PTS);
    if (VW->curWarpFile < 0) {
      midas_error("Error creating new warping storage", "", 0);
      diaSetChecked(VW->warpToggle, false);
      return;
    }
  }
  VW->editWarps = state;
  for (i = 0; i < 10; i++)
    VW->arrowsToGray[i]->setEnabled(!state);
  for (i = 0; i < 7; i++)
    VW->labelsToGray[i]->setEnabled(!state);
  for (i = 0; i < 3; i++)
    VW->wParameter[i]->setEnabled(!state);
  VW->wIncrement[0]->setEnabled(!state);
  VW->wIncrement[1]->setEnabled(!state);
  VW->anglescale->setEnabled(!state);
  diaShowWidget(VW->wLinearTrans, state);
  VW->curControl = -1;
  VW->midasGL->manageMouseLabel(" ");
  VW->midasGL->draw();
}


/* Auto contrast */
void MidasSlots::slotAutoContrast()
{
  float matt = 0.1;
  float sample, mean, sd;
  int i, ixStart, iyStart, nxUse, nyUse, black, white;
  int ind = get_bw_index();
  float targetMean = 150., targetSD = 40.;
  Islice *slice;
  unsigned char **lines;

  // Set area to use
  ixStart = (int)(VW->xsize * matt);
  iyStart = (int)(VW->ysize * matt);
  nxUse = VW->xsize - 2 * ixStart;
  nyUse = VW->ysize - 2 * iyStart;
  sample = 10000.0/(nxUse * nyUse);
  if (sample > 1.0)
    sample = 1.0;

  // Get slice and make line pointers
  slice = getRawSlice(VW, ind);
  lines = (unsigned char **)malloc(sizeof(unsigned char *) * VW->ysize);
  if (!lines) {
    midas_error("Error getting memory for doing auto-contrast", "", 0);
    return;
  }
  for (i = 0; i < VW->ysize; i++)
    lines[i] = slice->data.b + i * VW->xsize;

  // Get mean and Sd and convert to BW levels
  if (sampleMeanSD(lines, 0, VW->xsize, VW->ysize, sample,
                   ixStart, iyStart, nxUse, nyUse, &mean, &sd)) {
    midas_error("Error computing auto-contrast", "", 0);
    free(lines);
    return;
  }
  black = (int)(mean - sd * targetMean / targetSD + 0.5);
  white = (int)(mean + sd * (255 - targetMean) / targetSD + 0.5);
  if (black < 0)
    black = 0;
  if (white > 255)
    white = 255;
  setbwlevels(black, white, 1);
  free(lines);
}

/* Display change callbacks */
void MidasSlots::show_ref()
{
  int ind;
  VW->vmode = MIDAS_VIEW_SINGLE;
  VW->showref  = TRUE;
  ind = get_bw_index();
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_overlay();
}     

void MidasSlots::show_cur()
{
  int ind = VW->cz;
  VW->vmode = MIDAS_VIEW_SINGLE;
  VW->showref  = FALSE;
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_overlay();
}     

void MidasSlots::show_overlay()
{
  int ind = VW->cz;
  VW->vmode = MIDAS_VIEW_COLOR;
  VW->showref  = FALSE;
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_overlay();
}     


void MidasSlots::slotAlign_arm()
{
  show_ref();
}

void MidasSlots::slotAlign_disarm()
{
  show_cur();
}

void MidasSlots::slotReverse(bool state)
{
  VW->reversemap = state ? 1 : 0;
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
}

void MidasSlots::slotOverlay(bool state)
{
  if (state) {
    show_overlay();
  } else
    show_cur();
}

void MidasSlots::slotGlobRot(double value)
{
  if (VW->cosStretch)
    stretch_all_transforms(VW, 1);
  rotate_all_transforms(VW, value - VW->globalRot);
  if (VW->cosStretch)
    stretch_all_transforms(VW, 0);
  VW->globalRot = value;
  flush_xformed(VW);
  update_parameters();
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
  VW->midasWindow->setFocus();
}

void MidasSlots::slotConstrainMouse(bool state)
{
  VW->mouseXonly = state ? 1 : 0;
}

void MidasSlots::slotCosStretch(bool state)
{
  VW->cosStretch = state ? 1 : 0;
  VW->tiltOffSpin->setEnabled(state);
  stretch_all_transforms(VW, 1 - VW->cosStretch);
  flush_xformed(VW);
  update_parameters();
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
  VW->midasWindow->setFocus();
}

void MidasSlots::slotTiltOff(double value)
{
  stretch_all_transforms(VW, 1);
  VW->tiltOffset = value;
  stretch_all_transforms(VW, 0);
  flush_xformed(VW);
  update_parameters();
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
  VW->midasWindow->setFocus();
}

void MidasSlots::slotCorrelate()
{
  crossCorrelate(VW);
}

void MidasSlots::slotCorrBoxSize(int value)
{
  VW->corrBoxSize = value;
  VW->drawCorrBox = 2;
  VW->midasGL->draw();
  VW->midasWindow->setFocus();
}

void MidasSlots::slotCorrShiftLimit(int value)
{
  VW->corrShiftLimit = value;
  VW->drawCorrBox = 2;
  VW->midasGL->draw();
  VW->midasWindow->setFocus();
}

void MidasSlots::midas_keyinput(QKeyEvent *event)
{
  int transtep = 10;
  int bwinc = 3;
  int coninc = (bwinc + 1) / 2;
  int keysym = event->key();
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int shifted = event->modifiers() & Qt::ShiftModifier;
  float *xControl, *yControl, *xVector, *yVector;
  int cz;

  convertNumLock(keysym, keypad);

  switch(keysym) {

  case Qt::Key_Minus:
    slotZoom(-1);
    break;

  case Qt::Key_Equal:
    slotZoom(1);
    break;

    // No separate keypad keys in QT; they are a state flag
  case Qt::Key_Left:
    if (keypad) {
      VW->xtrans -= transtep;
      VW->midasGL->draw();
    } else
      slotParameter(-4);
    break;

  case Qt::Key_Right:
    if (keypad) {
      VW->xtrans += transtep;
      VW->midasGL->draw();
    } else
      slotParameter(4);
    break;

  case Qt::Key_Down:
    if (keypad) {
      VW->ytrans -= transtep;
      VW->midasGL->draw();
    } else
      slotParameter(-5);
    break;

  case Qt::Key_Up:
    if (keypad) {
      VW->ytrans += transtep;
      VW->midasGL->draw();
    } else
      slotParameter(5);
    break;

  case Qt::Key_O:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(-1);
    break;
	  
  case Qt::Key_L:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(1);
    break;
	  
  case Qt::Key_P:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(2);
    break;
	  
  case Qt::Key_Semicolon:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(-2);
    break;
	  
  case Qt::Key_BracketLeft:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(3);
    break;
	  
  case Qt::Key_Apostrophe:
    if (VW->xtype != XTYPE_MONT && !VW->editWarps)
      slotParameter(-3);
    break;
	  
  case Qt::Key_BracketRight:
  case Qt::Key_Backslash:
    if (VW->xtype == XTYPE_MONT || VW->editWarps)
      break;
    if (keysym ==  Qt::Key_BracketRight)
      VW->sangle += 50;
    else
      VW->sangle -= 50;
    if (VW->sangle < -900)
      VW->sangle = -900;
    if (VW->sangle > 900)
      VW->sangle = 900;
    VW->anglescale->setValue(VW->sangle);
    slotAngle(VW->sangle);
    break;

  case Qt::Key_Home:
  case Qt::Key_PageDown:
    show_ref();
    break;

  case Qt::Key_PageUp:
    show_cur();
    break;

#ifdef Q_OS_MACX
  case Qt::Key_Help:
#endif
  case Qt::Key_Delete:
  case Qt::Key_Insert:
    show_overlay();
    break;

  case Qt::Key_F1:
    if (VW->blackstate >= bwinc)
      setbwlevels(VW->blackstate - bwinc, VW->whitestate, 1);
    break;

  case Qt::Key_F2:
    if (VW->blackstate < VW->whitestate - bwinc)
      setbwlevels(VW->blackstate + bwinc, VW->whitestate, 1);
    break;

  case Qt::Key_F3:
    if (VW->whitestate > VW->blackstate + bwinc)
      setbwlevels(VW->blackstate, VW->whitestate - bwinc, 1);
    break;

  case Qt::Key_F4:
    if (VW->whitestate < 256 - bwinc)
      setbwlevels(VW->blackstate, VW->whitestate + bwinc, 1);
    break;

  case Qt::Key_F5:
    if (VW->whitestate < 256 - bwinc)
      setbwlevels(VW->blackstate + bwinc, VW->whitestate + bwinc, 1);
    break;

  case Qt::Key_F6:
    if (VW->blackstate >= bwinc)
      setbwlevels(VW->blackstate - bwinc, VW->whitestate - bwinc, 1);
    break;

  case Qt::Key_F7:
    if (VW->blackstate < VW->whitestate - 2 * coninc)
      setbwlevels(VW->blackstate - coninc, 
		  VW->whitestate + coninc, 1);
    break;

  case Qt::Key_F8:
    if (VW->whitestate < 256 - coninc && VW->blackstate >= coninc)
      setbwlevels(VW->blackstate + coninc,
		  VW->whitestate - coninc, 1);
    break;

  case Qt::Key_F11:
    if (VW->reversemap) {
      VW->reversemap = 0;
      VW->reversetoggle->setChecked(false);
    } else {
      VW->reversemap = 1;
      VW->reversetoggle->setChecked(true);
    }
    // The button change will cause a redraw
    break;
	  
  case Qt::Key_A:
    if (shifted && (VW->xtype == XTYPE_MONT))
      slotEdge(1);
    else if (shifted && VW->numChunks)
      slotChunkValue(VW->curChunk + 2);
    else if (event->modifiers() & Qt::ControlModifier)
      slotAutoContrast();
    else
      sectionInc(1);
    break;

  case Qt::Key_B:
    if (shifted && (VW->xtype == XTYPE_MONT))
      slotEdge(-1);
    else if (shifted && VW->numChunks)
      slotChunkValue(VW->curChunk);
    else
      sectionInc(-1);
    break;

  case Qt::Key_C:
    if (shifted)
      slotCorrelate();
    else if (VW->numChunks)
      try_section_change(0, 1);
    break;

  case Qt::Key_D:
    if (VW->numChunks)
      try_section_change(0, -1);
    break;

  case Qt::Key_X:
    if (VW->xtype == XTYPE_MONT && VW->nxpieces > 1)
      stepLowerXorY(-1, shifted ? 1 : -1);
    break;

  case Qt::Key_Y:
    if (VW->xtype == XTYPE_MONT && VW->nypieces > 1)
      stepLowerXorY(1, shifted ? 1 : -1);
    break;

  case Qt::Key_Z:
    if (shifted && VW->editWarps && VW->curControl >= 0) {
      cz = VW->numChunks ? VW->curChunk : VW->cz;
      if (getWarpPointArrays(cz, &xControl, &yControl, &xVector, &yVector)) {
        midas_error("Error getting control points", "", 0);
        return;
      }
      translate(xVector[VW->curControl], yVector[VW->curControl]);
    }
    break;

  case Qt::Key_Control:
    VW->ctrlPressed = 1;
    VW->midasGL->manageMouseLabel(" ");
    break;

  case Qt::Key_Shift:
    VW->shiftPressed = 1;
    VW->midasGL->manageMouseLabel(" ");
    break;

  }
}


/**************************************************************************/
/* Mouse functions                                                        */

void MidasSlots::mouse_shift_image()
{

  // As in 3dmod, move image with mouse for zoom < 1, move 1 image pixel per
  // mouse pixel above 1
  float factor = VW->truezoom < 1. ? 1. / VW->truezoom : 1.;
  if (VW->lastmx == VW->mx && VW->lastmy == VW->my)
    return;
  VW->xtrans += B3DNINT(factor * (VW->mx - VW->lastmx));
  VW->ytrans += B3DNINT(factor * (VW->my - VW->lastmy));
  VW->lastmx = VW->mx;
  VW->lastmy = VW->my;
  VW->midasGL->draw();
}

void MidasSlots::mouse_translate()
{
  float ddx, ddy = 0.;
  float zoom = VW->truezoom;
  float thresh = 0.2;

  ddx = (VW->mx - VW->lastmx) / zoom;
  if (!VW->mouseXonly)
    ddy = (VW->my - VW->lastmy) / zoom;
  if (ddx < thresh && ddx > -thresh && ddy < thresh && ddy > -thresh)
    return;
  translate(ddx, ddy);
  VW->lastmx = VW->mx;
  VW->lastmy = VW->my;
}

void MidasSlots::mouse_rotate()
{
  float drot;
  float delx, dely;
  float startang, endang;
  float thresh = 0.05;
  float xcen, ycen;
  float zoom = VW->truezoom;

  if (VW->lastmx == VW->mx && VW->lastmy == VW->my)
    return;

  xcen = zoom * VW->xcenter + VW->xoffset;
  ycen = zoom * VW->ycenter + VW->yoffset;
  delx = VW->lastmx - xcen;
  dely = VW->lastmy - ycen;
  if(delx > -20. && delx < 20. && dely > -20. && dely < 20.)
    return;

  startang = atan2((double)dely, (double)delx) / RADIANS_PER_DEGREE;
  delx = VW->mx - xcen;
  dely = VW->my - ycen;
  if(delx > -20. && delx < 20. && dely > -20. && dely < 20.)
    return;

  endang = atan2((double)dely, (double)delx) / RADIANS_PER_DEGREE;
  drot = endang - startang;
  if(drot < -360.)
    drot += 360.;
  if(drot > 360.)
    drot -= 360.;
  drot = thresh * floor((double)drot / thresh + 0.5);

  if(drot != 0.) {
    rotate(drot);
    VW->lastmx = VW->mx;
    VW->lastmy = VW->my;
  }
}

void MidasSlots::mouse_stretch(unsigned int state)
{
  float thresh = 0.001;
  float delx, dely, endang, radst, radnd, delrad;
  float xcen, ycen, xfix, yfix, distsq, tmin, disx, disy, p2lsq;
  float zoom = VW->truezoom;
  float a[3][3], b[9], tmat[9];
  struct Midas_transform *tr;
  int i, ist, ind;

  if (VW->lastmx == VW->mx && VW->lastmy == VW->my)
    return;
  
  xcen = zoom * VW->xcenter + VW->xoffset;
  ycen = zoom * VW->ycenter + VW->yoffset;
  if (!VW->useFixed || (state & Qt::ShiftModifier)) {
    delx = VW->lastmx - xcen;
    dely = VW->lastmy - ycen;
    if(delx > -20. && delx < 20. && dely > -20. && dely < 20.)
      return;
    radst = sqrt((double)(delx*delx + dely*dely));
    delx = VW->mx - xcen;
    dely = VW->my - ycen;
    if(delx > -20. && delx < 20. && dely > -20. && dely < 20.)
      return;
    endang = atan2((double)dely, (double)delx) / RADIANS_PER_DEGREE;
    radnd = sqrt((double)(delx*delx + dely*dely));
    delrad = (radnd - radst)/radst;
    delrad = thresh * floor((double)delrad / thresh + 0.5);
    if(delrad != 0.) {
      delrad += 1.;
      if (state & Qt::ShiftModifier)
        scale(delrad);
      else
        stretch(delrad, endang);
      VW->lastmx = VW->mx;
      VW->lastmy = VW->my;
    }
  } else {
    
    // Stretch with two fixed points
    // First check mouse coordinates for distance from the line between points
    xfix = zoom * VW->xfixed + VW->xoffset;
    yfix = zoom * VW->yfixed + VW->yoffset;
    delx = xfix - xcen;
    dely = yfix - ycen;
    distsq =  delx*delx+dely*dely;
    if (distsq < 2500.)
      return;
    
    // Compute point to line distance for both points
    tmin = ((VW->mx - xcen) * delx + (VW->my - ycen) * dely) / distsq;
    disx = xcen + tmin * delx - VW->mx;
    disy = ycen + tmin * dely - VW->my;
    p2lsq = disx * disx + disy * disy;
    if (p2lsq < 2500.)
      return;

    tmin = ((VW->lastmx - xcen) * delx + (VW->lastmy - ycen) * dely) / distsq;
    xfix = xcen + tmin * delx - VW->lastmx;
    yfix = ycen + tmin * dely - VW->lastmy;
    p2lsq= xfix * xfix + yfix * yfix;
    if (p2lsq < 2500.)
      return;
    
    // If both points are on same side of line, do the change, otherwise
    // just reset the mouse coordinates to avoid big jumps
    if (xfix * disx + yfix * disy > 0.) {
 
      // Now get centered image coordinates for previous and current mouse pos
      // and load the data matrix.  By chance the ordering of the b matrix is
      // the same as that of a transformation matrix
      tramat_idmat(b);
      a[0][0] = (VW->lastmx - VW->xoffset) / VW->truezoom - 0.5 * VW->xsize;
      a[0][1] = (VW->lastmy - VW->yoffset) / VW->truezoom - 0.5 * VW->ysize;
      a[0][2] = 1.;
      b[0] = (VW->mx - VW->xoffset) / VW->truezoom - 0.5 * VW->xsize;
      b[1] = (VW->my - VW->yoffset) / VW->truezoom - 0.5 * VW->ysize;
      a[1][0] = VW->xcenter - 0.5 * VW->xsize;
      a[1][1] = VW->ycenter - 0.5 * VW->ysize;
      a[1][2] = 1.;
      b[3] = a[1][0];
      b[4] = a[1][1];
      a[2][0] = VW->xfixed - 0.5 * VW->xsize;
      a[2][1] = VW->yfixed - 0.5 * VW->ysize;
      a[2][2] = 1.;
      b[6] = a[2][0];
      b[7] = a[2][1];
      if (gaussj(&a[0][0], 3, 3, b, 2, 3))
        return;

      getChangeLimits(&ist, &ind);

      for (i = ist; i <= ind; i++) {
        tr = &(VW->tr[i]);
        tramat_multiply(tr->mat, b, tmat);
        tramat_copy(tmat, tr->mat);
      }
      adjustControlPoints(VW);
      synchronizeChunk(VW->cz);
      update_parameters();
      retransform_slice();
      VW->changed = 1;
    }
    VW->lastmx = VW->mx;
    VW->lastmy = VW->my;
  }
}


int MidasSlots::getParamDecimals(int param)
{
  return paramDecimals[param];
}

int MidasSlots::getIncDecimals(int param)
{
  return incDecimals[param];
}

int MidasSlots::getParamDigits(int param)
{
  return paramDigits[param];
}

int MidasSlots::getIncDigits(int param)
{
  return incDigits[param];
}

float MidasSlots::getIncrement(int index, int type)
{
  return increments[index][type];
}

void MidasSlots::sprintf_decimals(char *string, int decimals, int digits, 
                                  float val)
{
  char format[10];
  sprintf(format, "%c%d.%df", '%', digits, decimals);
  sprintf(string, format, val);
}


void MidasSlots::backup_current_mat()
{
  int i, nControl;
  for (i = 0; i < 9; i++)
    VW->backup_mat[i] = VW->tr[VW->cz].mat[i];
  if (VW->xtype == XTYPE_MONT) {
    VW->backup_edgedx = VW->edgedx[VW->edgeind * 2 + VW->xory];
    VW->backup_edgedy = VW->edgedy[VW->edgeind * 2 + VW->xory];
  }
  if (VW->curWarpFile >= 0) {
    i = VW->numChunks ? VW->curChunk : VW->cz;
    getNumWarpPoints(i, &nControl);
    if (nControl > VW->maxWarpBackup) {
      B3DFREE(VW->backupXcontrol);
      B3DFREE(VW->backupYcontrol);
      B3DFREE(VW->backupXvector);
      B3DFREE(VW->backupYvector);
      VW->backupXcontrol = B3DMALLOC(float, nControl);
      VW->backupYcontrol = B3DMALLOC(float, nControl);
      VW->backupXvector = B3DMALLOC(float, nControl);
      VW->backupYvector = B3DMALLOC(float, nControl);
      if (!VW->backupXcontrol || !VW->backupYcontrol || !VW->backupXvector ||
          !VW->backupYvector) {
        VW->maxWarpBackup = 0;
        VW->numWarpBackup = 0;
        return;
      }
      VW->maxWarpBackup = nControl;
    }
    VW->numWarpBackup = nControl;
    if (!nControl)
      return;
    getWarpPoints(i, VW->backupXcontrol, VW->backupYcontrol, VW->backupXvector, 
                  VW->backupYvector);
  }
}

// Copy transform for given section to all the other sections in a chunk
void MidasSlots::synchronizeChunk(int sec)
{
  int i;
  if (!VW->numChunks)
    return;
  for (i = VW->chunk[VW->curChunk].start;
       i < VW->chunk[VW->curChunk + 1].start ; i++) {
    if (i != sec)
      tramat_copy(VW->tr[sec].mat, VW->tr[i].mat);
  }
}


/* Convert numeric keypad keys that come through as numbers because NumLock is
   on to the named keys */
/* But also turn off keypad on Mac if they are arrow keys */
static int keypadKeys[10] = {Qt::Key_Delete, Qt::Key_Insert, Qt::Key_End, 
                             Qt::Key_Down, Qt::Key_PageDown, Qt::Key_Left,
                             Qt::Key_Right, Qt::Key_Home, Qt::Key_Up,
                             Qt::Key_PageUp};
static int numLockKeys[10] = {Qt::Key_Period, Qt::Key_0, Qt::Key_1, Qt::Key_2,
                              Qt::Key_3, Qt::Key_4,
                              Qt::Key_6, Qt::Key_7, Qt::Key_8, Qt::Key_9};
void MidasSlots::convertNumLock(int &keysym, int &keypad)
{
  if (!keypad)
    return;
  for (int i = 0; i < 10; i++)
    if (keysym == numLockKeys[i]) {
      keysym = keypadKeys[i];
      return;
    }
#ifdef Q_OS_MACX
  if (keysym == Qt::Key_Left || keysym == Qt::Key_Right || 
      keysym == Qt::Key_Up || keysym == Qt::Key_Down)
    keypad = 0;
#endif
  return;
}

/* Show a help page in Qt Assistant; provide a full
   path if the path is not relative to IMOD_DIR/html
   Returns 1 for error, 0 for success */
int MidasSlots::showHelpPage(const char *page)
{
  if (mImodHelp)
    return (mImodHelp->showPage(page) > 0 ? 1 : 0);
  else
    return 1;
}
