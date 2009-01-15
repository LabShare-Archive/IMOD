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
 *  Log at end of file
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

int MidasSlots::index_to_edgeno(int index, int *xory)
{
  int  ipclo, pcx, pcy, edge;
  *xory = 0;
  if (index < 0)
    return 0;
  ipclo = VW->piecelower[index];
  *xory = index % 2;
  pcx = (VW->xpclist[ipclo] - VW->minxpiece) / (VW->xsize - VW->nxoverlap);
  pcy = (VW->ypclist[ipclo] - VW->minypiece) / (VW->ysize - VW->nyoverlap);
  if (*xory)
    edge = pcx * (VW->nypieces - 1) + pcy + 1;
  else
    edge = pcy * (VW->nxpieces - 1) + pcx + 1;
  return edge;
}

void MidasSlots::update_parameters()
{
  QString str;
  int i, first;
  char string[32];
  float param[5], xc, yc;
  float meanerr, toperr[8], meanleave, topleave, curerrx, curerry;
  int topleavind, topxy, edge;
  float *mat = VW->tr[VW->cz].mat;
  amatToRotmagstr(mat[0], mat[3], mat[1], mat[4], &param[0], &param[1],
                    &param[2], &VW->phi);

  if (VW->xtype != XTYPE_MONT) {
    xc = (float)VW->xsize * 0.5f;
    yc = (float)VW->ysize * 0.5f;
    param[3] = mat[6];
    param[4] = mat[7];
    first = 0;
  } else {
    param[3] = VW->edgedx[VW->edgeind * 2 + VW->xory];
    param[4] = VW->edgedy[VW->edgeind * 2 + VW->xory];
    first = 3;
    find_best_shifts(VW, 0, 4, &meanerr, toperr, VW->topind, &curerrx,
		     &curerry, VW->mousemoving);
    find_best_shifts(VW, 1, 1, &meanleave, &topleave, &topleavind,
		     &VW->curleavex, &VW->curleavey, VW->mousemoving);

    str.sprintf("Mean error: %.2f", meanerr);
    VW->wMeanerr->setText(str);
	  
    for (i = 0; i < 4; i++) {
      edge = index_to_edgeno(VW->topind[i], &topxy);
      str.sprintf("%s %d: %.2f  ", topxy ? "Y" : "X", edge, toperr[i]);
      VW->wToperr[i]->setText(str);
    }

    str.sprintf("This edge: %.2f, %.2f", curerrx, curerry);
    VW->wCurerr->setText(str);
	  
    str.sprintf("Leave-out: %.2f, %.2f", VW->curleavex, VW->curleavey);
    VW->wLeaverr->setText(str);
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
 
  if (VW->numChunks) {
    diaSetSpinBox(VW->chunkSpin, VW->curChunk + 1);
    VW->curSpin->blockSignals(true);
    VW->refSpin->blockSignals(true);
    VW->curSpin->setRange(VW->chunk[VW->curChunk].start + 1,
                          VW->chunk[VW->curChunk + 1].start);
    VW->refSpin->setRange(VW->chunk[VW->curChunk - 1].start + 1,
                          VW->chunk[VW->curChunk].start);
  }

  diaSetSpinBox(VW->curSpin, (VW->xtype == XTYPE_MONT ? VW->montcz : VW->cz)
                + 1);
  if (VW->xtype == XTYPE_MONT) {
    diaSetSpinBox(VW->edgeSpin, VW->curedge);
    return;
  }
  diaSetSpinBox(VW->refSpin, VW->refz + 1);
    
  if (!VW->difftoggle)
    return;
  str.sprintf("Keep Cur - Ref diff = %d", VW->cz - VW->refz);
  VW->difftoggle->setText( str);
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
  midasGetSlice(VW, MIDAS_SLICE_CURRENT);
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

  /* success: mark that save was done, no unsaved changes */
  dia_puts("Transforms saved to file.");
  VW->didsave = 1;
  VW->changed = 0;
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
  char *filters[] = {"Transform files (*.*xf *.*xg)"};
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
    qname = diaOpenFileName(NULL, "Select file to load transforms from", 1,
                            filters);
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
    if (VW->xtype == XTYPE_XF || VW->xtype == XTYPE_MONT) {
      dia_puts("Models may not be transformed in local "
	       "alignment or montage fixing mode");
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
  int i, ist, ind;
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

    synchronizeChunk(VW->cz);
    update_parameters();
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

    synchronizeChunk(VW->cz);
    update_parameters();
    retransform_slice();
    VW->changed = 1;
    break;

  case EDIT_MENU_MIRROR: /* Mirror around X axis  */
	  /* get unit transform and modify to mirror */
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
	  

  }
  return;
}

void MidasSlots::slotHelpmenu(int item)
{
  switch(item){
  case HELP_MENU_CONTROLS: 
     showHelpPage("midasHelp/controls.html");
    break;
  case HELP_MENU_HOTKEYS:
    showHelpPage("midasHelp/keyboard.html");
    break;
  case HELP_MENU_MOUSE:
     showHelpPage("midasHelp/mouse.html");
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
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, xstep, ystep);
  }

  /* If montage, maintain edge displacements too */
  if (VW->xtype == XTYPE_MONT) {
    VW->edgedx[VW->edgeind * 2 + VW->xory] += xstep;
    VW->edgedy[VW->edgeind * 2 + VW->xory] += ystep;
  }

  synchronizeChunk(VW->cz);
  update_parameters();
  if (ix == xstep && iy == ystep && ++fastcount % 10) {
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
  int ind;
  int newcur = ds + VW->cz;
  int newref = dsref + VW->refz;
  if ( (newcur < 0) || (newcur >= VW->zsize) ||
       ( (VW->xtype != XTYPE_XREF) &&
	 ( (newref < 0) || (newref >= VW->zsize) ) ) ||
       (VW->numChunks && ((newcur < VW->chunk[VW->curChunk].start) ||
                          (newcur >= VW->chunk[VW->curChunk + 1].start) ||
                          (newref < VW->chunk[VW->curChunk - 1].start) ||
                          (newref >= VW->chunk[VW->curChunk].start)))) {
    update_sections();
    return;
  }

  VW->cz += ds;
  VW->refz += dsref;
  ind = get_bw_index();
  VW->xcenter = 0.5 * VW->xsize;
  VW->ycenter = 0.5 * VW->ysize;
  update_parameters();
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
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
  int newcur, ind;
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
  set_mont_pieces(VW);
  ind = get_bw_index();
  update_parameters();
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_sections();
  backup_current_mat();
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
  if (VW->xtype != XTYPE_XREF) {
    sec -= VW->refz;
    ds = 0;
    if (VW->keepsecdiff)
      ds = sec;
    try_section_change(ds, sec);

    /* Changing the reference section from other file : check legality */
  } else if (sec < 0 || sec >= VW->refzsize) {
    update_sections();

  } else {

    /* Try to load; if it fails, reload original */
    VW->xsec = sec;
    if (load_refimage(VW, VW->refname)) {
      VW->xsec = VW->refz;
      load_refimage(VW, VW->refname);
      update_sections();
    } else {

      /* Finalize display, leave sliders alone */
      VW->refz = VW->xsec;
      update_sections();
      backup_current_mat();
      update_parameters();
      VW->midasGL->fill_viewdata(VW);
      VW->midasGL->draw();
    }
  }
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
  int newcur, ind;
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
  set_mont_pieces(VW);
  ind = get_bw_index();
  update_parameters();
  setbwlevels(VW->tr[ind].black, VW->tr[ind].white, 1);
  update_sections();
  backup_current_mat();
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


void MidasSlots::slotXory(int which)
{
  int newcur = VW->curedge;
  if (which == VW->xory)
    return;
  VW->xory = which;
  flush_xformed(VW);

  diaSetGroup(VW->edgeGroup, VW->xory);

  /* Adjust the edge number as necessary, and set current edge to zero to
     force it to recognize this as a new edge and do updates */
  if (newcur > VW->maxedge[VW->xory])
    newcur = VW->maxedge[VW->xory];
  VW->curedge = 0;
  VW->edgeSpin->setRange(1, VW->maxedge[VW->xory]);
  try_montage_edge(newcur, 0);
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

  state = onlyone < 0;
  vw->wXedge->setEnabled(state);
  vw->wYedge->setEnabled(state);
}

void MidasSlots::slotLeave_out()
{
  translate(VW->curleavex, VW->curleavey);
}

void MidasSlots::slotTop_error(int item)
{
  int topxy, edge;
  edge = index_to_edgeno(VW->topind[item], &topxy);
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

void MidasSlots::slotBlock(int upDown)
{
  QString str;
  int ds = upDown;

  VW->boxsize += ds;

  if (VW->boxsize < 1)
    VW->boxsize = 1;
  if (VW->boxsize > VW->xsize / 4)
    VW->boxsize = VW->xsize / 4;
  if (VW->boxsize > VW->ysize / 4)
    VW->boxsize = VW->ysize / 4;

  str.sprintf("Block size %2d", VW->boxsize);
  VW->blocklabel->setText(str);

  /* DNM 1/27/04: flush and retransform just as for interpolation change */
  if (VW->fastip) {
    flush_xformed(VW);
    VW->midasGL->fill_viewdata(VW);
    VW->midasGL->draw();
  }
}

void MidasSlots::slotInterpolate(bool state)
{
  VW->fastip = state ? 0 : 1;
  flush_xformed(VW);
  VW->midasGL->fill_viewdata(VW);
  VW->midasGL->draw();
}

void MidasSlots::display_bwslider_value(QLabel *w, int white)
{
  QString str;
  str.sprintf("%03d", white);
  w->setText(str);
}

int MidasSlots::setbwlevels(int black, int white, int draw)
{
  int remute = FALSE;
  int ind, maxind, b0, w0, bn, wn, bs, ws, i;
  float bnf, dslope;

  if (VW->blackstate != black){
    VW->blackstate = black;
    VW->wBlacklevel->setValue(black);
    display_bwslider_value(VW->wBlackval, black);
    remute = TRUE;
  }

  if (VW->whitestate != white){
    VW->whitestate = white;
    VW->wWhitelevel->setValue(white);
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

void MidasSlots::midas_keyinput(QKeyEvent *event)
{
  int transtep = 10;
  int bwinc = 3;
  int coninc = (bwinc + 1) / 2;
  int keysym = event->key();
  int keypad = event->modifiers() & Qt::KeypadModifier;

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
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-1);
    break;
	  
  case Qt::Key_L:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(1);
    break;
	  
  case Qt::Key_P:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(2);
    break;
	  
  case Qt::Key_Semicolon:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-2);
    break;
	  
  case Qt::Key_BracketLeft:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(3);
    break;
	  
  case Qt::Key_Apostrophe:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-3);
    break;
	  
  case Qt::Key_BracketRight:
  case Qt::Key_Backslash:
    if (VW->xtype == XTYPE_MONT)
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
    if ((event->modifiers() & Qt::ShiftModifier) && (VW->xtype == XTYPE_MONT))
      slotEdge(1);
    else if ((event->modifiers() & Qt::ShiftModifier) && VW->numChunks)
      slotChunkValue(VW->curChunk + 2);
    else if (event->modifiers() & Qt::ControlModifier)
      slotAutoContrast();
    else
      sectionInc(1);
    break;

  case Qt::Key_B:
    if ((event->modifiers() & Qt::ShiftModifier) && (VW->xtype == XTYPE_MONT))
      slotEdge(-1);
    else if ((event->modifiers() & Qt::ShiftModifier) && VW->numChunks)
      slotChunkValue(VW->curChunk);
    else
      sectionInc(-1);
    break;

  case Qt::Key_C:
    if (VW->numChunks)
      try_section_change(0, 1);
    break;

  case Qt::Key_D:
    if (VW->numChunks)
      try_section_change(0, -1);
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
  if (VW->lastmx == VW->mx && VW->lastmy == VW->my)
    return;
  VW->xtrans += VW->mx - VW->lastmx;
  VW->ytrans += VW->my - VW->lastmy;
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
  int i;
  for (i = 0; i < 9; i++)
    VW->backup_mat[i] = VW->tr[VW->cz].mat[i];
  if (VW->xtype != XTYPE_MONT)
    return;
  VW->backup_edgedx = VW->edgedx[VW->edgeind * 2 + VW->xory];
  VW->backup_edgedy = VW->edgedy[VW->edgeind * 2 + VW->xory];
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

/*
$Log$
Revision 3.20  2009/01/15 16:30:19  mast
Qt 4 port

Revision 3.19  2008/11/18 22:45:05  mast
Added Delete key for Mac, used new amatToRotmagstr

Revision 3.18  2008/10/13 04:36:23  mast
Added cosine stretching

Revision 3.17  2007/11/27 23:31:38  sueh
bug# 1038 Switching to html files for help.

Revision 3.16  2007/10/03 21:36:10  mast
Added ImodAssistant help object

Revision 3.15  2006/07/08 15:32:13  mast
Changes to implement second fixed point for stretching

Revision 3.14  2006/06/26 15:48:19  mast
Added autocontrast function

Revision 3.13  2006/05/20 16:07:56  mast
Changes to allow mirroring around X axis

Revision 3.12  2005/11/08 02:36:58  mast
Fixed X and Y labels on error buttons

Revision 3.11  2005/03/10 21:04:15  mast
Added -q option for use from etomo

Revision 3.10  2004/09/23 14:29:13  mast
Fixed bug from block mode that crashed reference mode

Revision 3.9  2004/09/13 18:43:49  mast
Made it skip across missing edges with spin button presses

Revision 3.8  2004/07/12 18:41:36  mast
Changes for chunk alignment, for switching to spin boxes, and for making
the sliders continuously active.

Revision 3.7  2004/02/27 21:37:46  mast
Fixed treatment of x/ycenter when transforming and rotating, etc.

Revision 3.6  2004/01/27 21:16:26  mast
Made it flush data and retransform when box size changes

Revision 3.5  2003/12/17 21:44:19  mast
Changes to implement global rotations

Revision 3.4  2003/10/24 03:55:35  mast
unknown change (untabified?)

Revision 3.3  2003/09/25 21:09:36  mast
Switched to sections numbered from 1 not 0

Revision 3.2  2003/04/17 20:56:55  mast
Changes for Mac key problems

Revision 3.1  2003/02/10 20:49:57  mast
Merge Qt source

Revision 1.1.2.2  2003/01/26 23:20:33  mast
using new library

Revision 1.1.2.1  2002/12/05 03:13:02  mast
New Qt version

Revision 3.2  2002/11/05 23:27:00  mast
Changed copyright notice to use lab name and years

Revision 3.1  2002/08/19 04:48:31  mast
In montage-fixing mode, made it suppress updates during mouse moves
when there are many pieces

*/
