/*  IMOD VERSION 2.50
 *
 *  slots.c -- Slot functions for Qt signals, and actions that they cause
 *
 *  Original author: James Kremer
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

#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include "midas.h"
#include <qdialog.h>
#include <qlayout.h>
#include <qtextedit.h>
#include <qmessagebox.h>
#include <qfiledialog.h>
#include "dia_qtutils.h"

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
  amat_to_rotmagstr(mat, &param[0], &param[1], &param[2], &VW->phi);

  if (VW->xtype != XTYPE_MONT) {
    xc = (float)VW->xsize * 0.5f;
    yc = (float)VW->ysize * 0.5f;
    param[3] = mat[6] + (xc * mat[0]) + (yc * mat[3]) - xc;
    param[4] = mat[7] + (xc * mat[1]) + (yc * mat[4]) - yc;
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
      str.sprintf("%s %d: %.2f  ", topxy ? "X" : "Y", edge, toperr[i]);
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

void MidasSlots::update_sections()
{
  QString str;
  str.sprintf("%d", VW->xtype == XTYPE_MONT ? VW->montcz : VW->cz);
  VW->curtext->setText(str);
  if (VW->xtype == XTYPE_MONT) {
    str.sprintf("%d", VW->curedge);
    VW->edgetext->setText(str);
    return;
  }
  str.sprintf("%d", VW->refz);
  VW->reftext->setText(str);
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
  int xformed;
  Islice *orgSlice = midasGetSlice(VW, MIDAS_SLICE_OCURRENT, &xformed);
  Islice *curSlice = midasGetSlice(VW, MIDAS_SLICE_CURRENT, &xformed);
  if (!xformed)
    midas_transform(orgSlice, curSlice, &VW->tr[VW->cz]);
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
      (QString::null, QString::null, 0, 0, 
       "Select name of file to save transforms in");
    if (qname.isEmpty())
      return 1;
    filename = strdup(qname.latin1());
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

  if (VW->xtype == XTYPE_MONT)
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
    filename = strdup(qname.latin1());
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
      (QString::null, QString::null, 0, 0, 
       "Enter file to save");
    if (qname.isEmpty())
      break;
    filename = strdup(qname.latin1());
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
    inName = QFileDialog::getOpenFileName
      (QString::null, QString::null, 0, 0, 
       "Select model file to transform");
    if (inName.isEmpty())
      break;
    qname = QFileDialog::getSaveFileName
      (QString::null, QString::null, 0, 0, 
       "Enter file to save transformed model into");
    if (qname.isEmpty())
      break;

    transform_model((char *)inName.latin1(), (char *)qname.latin1(), VW);
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

    update_parameters();
    retransform_slice();
    VW->changed = 1;
    break;


  }
  return;
}

void MidasSlots::control_help()
{
  dia_vasmsg
    ("Midas : Manual alignment program. Version ",
     MIDAS_VERSION_STRING,
     "\n\n",
     "Control panel help.\n",
     "\n\n",
     "Section Controls:\n",
     "\tThe two text boxes show the section numbers (numbered from 0) "
     "of the reference and current sections.  The section can be "
     "changed as desired by typing a number in one of these boxes "
     "followed by Enter.  If the \"Keep Cur - Ref diff\" box is "
     "selected, both reference and current sections will be changed "
     "when a new number is entered in one of the boxes.\n"
     "\tThe Up and Down Arrows for \"Section\" will increase or "
     "decrease the current section number.  If the \"Keep Cur - Ref "
     "diff\" box is selected, then the reference section will be "
     "changed in tandem.\n\n",

     "Contrast Controls:\n",
     "\tThe Black and White sliders control the intensity range that "
     "will be scaled to run from black to white on the screen.\n"
     "\tEach "
     "section has its own independent contrast setting.  If the \"Apply "
     "to only one section\" box is not selected, then changing the "
     "contrast (via the sliders or F1-F8 keys) will adjust the contrast "
     "for all sections in parallel.  If this box is selected, then "
     "changes will adjust the contrast for just one section, the one "
     "currently being displayed (or the current section, if the display "
     "shows both sections in overlay.)\n\n"

     "Display Controls:\n",
     "\tThe Up and Down Arrows for \"Zoom\" increase and decrease the "
     "zoom of the display.  Fractional zooms are possible, and needed "
     "for large images.\n"
     "\tThe Up and Down Arrows for \"Block size\" increase and "
     "decrease the block size for image transformation.  The larger the "
     "size, the faster images can be transformed, but the poorer the "
     "image quality, especially if there is substantial rotation of the "
     "transformed image.\n"
     "\tThe \"Interpolate\" box can be used to turn on bilinear "
     "interpolation in the image transformation, which will give the "
     "most accurate transformed image of the current section but will "
     "be slow for large images.\n"
     "\tThe \"Overlay view\" box can be used to toggle between showing "
     "one section and showing the two sections in overlay, with the "
     "current section in green and the previous in magenta.\n"
     "\tThe \"Toggle Ref/Cur\" button provides an easy way to toggle "
     "between current and reference sections with a mouse button.\n\n"

     "Transformation Controls:\n",
     "\tThe Arrow buttons allow each transformation parameter to be "
     "changed by a selected increment.  There is an additive "
     "increment for translation, an independent increment for rotation "
     "angle, and a multiplicative factor for magnification and "
     "stretching.\n"
     "\tThe stretch angle slider sets the axis along which the section "
     "will be stretched by either an Arrow button or a hotkey.  This "
     "axis is shown by the red dashed line.  If the section has already "
     "been stretched, the axis of that actual stretch is shown with a "
     "blue dashed line.\n"
     "\tRotation, magnification, and stretch will occur around the "
     "center of rotation, which is marked by the yellow star.  You can "
     "use Ctrl-Middle-Mouse-Button to move this center to a point that "
     "you want to keep fixed during further changes of the "
     "transformation.\n"
     "\nFile Menu Items:\n",
     "\tLoad Transforms: will load transforms from a file.  If the set "
     "of transforms currently in the program have not been saved since "
     "they were last changed, the program will first ask if you want to "
     "save those transforms.  After loading transforms from a file, "
     "that file becomes the file to which transforms will be saved.\n"
     "\tSave Transforms: will save the transforms to the current "
     "transform file, or to a new file that you specify, if no such "
     "file has been defined yet.\n"
     "\tSave Transforms As...:  will save the transforms to a new file "
     "that you specify.\n",
     "\tSave Contrast-scaled Image...: will make a copy of the image "
     "file, stretching the contrast for each section by its respective "
     "Black and White levels, as well as applying any global scaling "
     "that was specified with the -s option when starting the program.  "
     "Images will NOT be transformed; that should be done with "
     "Newstack.  A byte file will be created regardless of the mode of "
     "the input file.\n\n"

     "Edit Menu Items:\n",
     "\tStore Section Transform: will store the transform for the "
     "current section in the internal list of transformations.\n",
     "\tReset to Unit Transform: will set the transform for the current "
     "slice to unity (no translation, rotation, etc.).\n"
     "\tRevert to Stored Transform: will restore the transform for the "
     "current section from the transform stored in the internal list.\n"
     "\tTransform Model: will transform a model using the current set "
     "of transformations.\n\n",

     "Controls when Fixing Montages:\n",
     "\tX edges are between adjacent pieces in a row and are numbered "
     "from left to right in the bottom row, next row, etc.  Y edges are "
     "between adjacent pieces in a column and are numbered from bottom "
     "to top in the left-most column, next column, etc.\n"
     "\tIn the top panel, the X and Y radio buttons can be used to "
     "select which type of edge, if there is more than one type of edge "
     "in a section.  The edge number is displayed and can be changed by "
     "typing a new number into the text box, followed by Enter.  The Up "
     "and Down arrows can also be used to navigate between edges.\n"
     "\tOnly X and Y translations of one piece relative to another can "
     "be changed when fixing montages.\n"
     "\tThe bottom panel displays information about the errors in "
     "fitting pieces together with the current displacements and allows "
     "one to go to the edges with the highest error.  An error is the "
     "difference (or distance) between the displacement between pieces "
     "implied by the X and Y translation values at that edge, and the "
     "displacement achieved when all pieces are shifted into best "
     "alignment using the translation values for all of the edges.\n"
     "\tFor the edges with the four highest errors, the edge number and "
     "error are displayed in a button, which can be pressed to make "
     "that edge be the current edge.  The errors, and even the edges, "
     "displayed in these buttons will change whenever the displacement "
     "at the current edge is changed.\n"
     "\tBelow the buttons are the X and Y components of the error at "
     "the current edge.  On the line below that is the \"Leave-out\" "
     "error for the current edge, which is its error when its "
     "translation values are left out when solving for the best fit "
     "between pieces.  This error can be compared directly with any "
     "mismatch observed between the pieces.  Pressing the \"Apply "
     "Leave-out Error\" button will change the X and Y translations by "
     "these amounts.\n",
     NULL);
  return;
}
void MidasSlots::hotkey_help()
{
  dia_vasmsg
    ("Midas : Manual alignment program. Version ",
     MIDAS_VERSION_STRING,
     "\n\n",
     "Hotkey Help.\n\n",
     "\tFile and section keys\n"
     "\t----------------------------------------------------\n"
     "a\tAdvance current section to next section\n"
     "b\tBack up current section to previous section\n"
     "A\tAdvance to next edge on this section when fixing montages\n"
     "B\tBack up to previous edge when fixing montages\n"
     "s\tSave transforms to file\n"
     "\n",
     "\tDisplay control keys\n"
     "\t----------------------------------------------------\n"
     "-\tZoom image down\n"
     "=\tZoom image up\n"
     "F1\tDecrease Black level\n"
     "F2\tIncrease Black level\n"
     "F3\tDecrease White level\n"
     "F4\tIncrease White level\n"
     "F5\tDecrease brightness\n"
     "F6\tIncrease brightness\n"
     "F7\tDecrease contrast\n"
     "F8\tIncrease contrast\n"
     "F11\tReverse contrast\n"
     "PageUp\t\tShow current section\n"
     "PageDown \tShow reference section\n"
     "End\t\tShow reference section\n"
     "Insert\t\tShow overlay view of current and reference sections\n"
     "Keypad Up\tScroll image up\n"
     "Keypad Down\tScroll image down\n"
     "Keypad Left\tScroll image left\n"
     "Keypad Right\tScroll image right\n"
     "\n",
     "\tTransformation control keys \n"
     "\t(all by the increment or factor shown in control panel)\n"
     "\t-------------------------------------------------------\n"
     "Up\tTranslate current section up\n"
     "Down\tTranslate current section down\n"
     "Left\tTranslate current section left\n"
     "Right\tTranslate current section right\n"
     "o\tIncrease rotation angle\n"
     "l\tDecrease rotation angle\n"
     "p\tIncrease magnification of current section\n"
     ";\tDecrease magnification of current section\n"
     "[\tStretch current section along selected stretch axis\n"
     "'\tCompress current section along selected stretch axis\n"
     "]\tRotate selected stretch axis counterclockwise\n"
     "\\\tRotate selected stretch axis clockwise\n"
     "\n\n",
     NULL);
}
void MidasSlots::mouse_help()
{
  dia_vasmsg
    ("Midas : Manual alignment program. Version ",
     MIDAS_VERSION_STRING,
     "\n\n",
	   
     "Mouse Control Help.\n\n",
     "Left Button: Translation of current section.\n"
     "\tPress and hold "
     "the button and move the mouse to drag the section to the desired "
     "location relative to the reference section.\n\n"
     "Middle Button: Rotation of current section.\n\tPosition the mouse "
     "near the edge of the image, press and hold "
     "the button, and move the mouse tangentially around the center "
     "of rotation (yellow star).  The current section will rotate by a "
     "corresponding amount relative to the reference section.\n\n"
     "Right Button: Stretch of current section.\n\tPosition the mouse "
     "near the edge of the image along the desired axis of stretch.  "
     "Press and hold the button, and move the mouse radially away from "
     "or toward the center of rotation (yellow star).  The current "
     "section will "
     "stretch or compress by a corresponding amount along the radial "
     "axis, relative to the reference section.\n\n"
     "Shift - Right Button: Magnification of current section.\n"
     "\tPosition the mouse near the edge of the image, press and hold "
     "the Shift key, press and hold the mouse button, and move the "
     "mouse radially away from or toward the center of rotation.  "
     "The current section will expand or shrink by a corresponding "
     "amount relative to the reference section.\n\n"
     "Ctrl - Left Button: Shift zoomed-up image in window.\n"
     "\tWhen the whole image does not fit in the window, press and hold "
     "the Ctrl key, press and hold the mouse button, and move the "
     "mouse to drag the image to the desired position in the window.\n\n"
     "Ctrl - Middle Button: Move the center of rotation and stretch.\n"
     "\tPosition the mouse at the desired center of rotation, press and "
     "hold the Ctrl key, and click the mouse button to specify "
     "the new position."
     "\n\n",
     NULL);
}

void MidasSlots::slotHelpmenu(int item)
{

  switch(item){
  case HELP_MENU_CONTROLS: 
    control_help();
    break;
  case HELP_MENU_HOTKEYS:
    hotkey_help();
    break;
  case HELP_MENU_MOUSE:
    mouse_help();
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
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -VW->xcenter, -VW->ycenter);
    tramat_rot(tr->mat, step); 
    tramat_translate(tr->mat, VW->xcenter, VW->ycenter);
  }
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
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -VW->xcenter, -VW->ycenter);
    tramat_scale(tr->mat, step, step); 
    tramat_translate(tr->mat, VW->xcenter, VW->ycenter);
  }
  update_parameters();
  retransform_slice();
  VW->changed = 1;
}

void MidasSlots::stretch(float step, float angle)
{
  struct Midas_transform *tr;
  int i, ist, ind;
  getChangeLimits(&ist, &ind);

  for (i = ist; i <= ind; i++) {
    tr = &(VW->tr[i]);
    tramat_translate(tr->mat, -VW->xcenter, -VW->ycenter);
    tramat_rot(tr->mat, -angle);
    tramat_scale(tr->mat, step, 1.0f); 
    tramat_rot(tr->mat, angle);
    tramat_translate(tr->mat, VW->xcenter, VW->ycenter);
  }
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
	 ( (newref < 0) || (newref >= VW->zsize) 
	   /*|| (newref == newcur) */))) {
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

void MidasSlots::slotSection(int upDown)
{
  sectionInc(upDown);
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

void MidasSlots::slotCurtext()
{
  int sec;
  QString st = VW->curtext->text();
  sec = atoi(st.latin1());
  if (VW->xtype == XTYPE_MONT)
    try_montage_section(sec, 0);
  else {
    sec -= VW->cz;
    sectionInc(sec);
  }
  VW->midasWindow->setFocus();
}

void MidasSlots::slotReftext()
{
  int sec, ds;
  QString st = VW->reftext->text();
  sec = atoi(st.latin1());
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

void MidasSlots::slotEdgetext()
{
  int sec;
  QString st = VW->edgetext->text();
  sec = atoi(st.latin1());
  try_montage_edge(sec, 0);
  VW->midasWindow->setFocus();
}


void MidasSlots::slotXory(int which)
{
  int newcur = VW->curedge;
  if (which == VW->xory)
    return;
  VW->xory = which;
  flush_xformed(VW);

  VW->edgeGroup->blockSignals(true);
  VW->edgeGroup->setButton(VW->xory);
  VW->edgeGroup->blockSignals(false);

  /* Adjust the edge number as necessary, and set current edge to zero to
     force it to recognize this as a new edge and do updates */
  if (newcur > VW->maxedge[VW->xory])
    newcur = VW->maxedge[VW->xory];
  VW->curedge = 0;
  try_montage_edge(newcur, 0);
}

void MidasSlots::manage_xory(struct Midas_view *vw)
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
  vw->edgeGroup->blockSignals(true);
  vw->edgeGroup->setButton(vw->xory);
  vw->edgeGroup->blockSignals(false);

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

  if (VW->fastip)
    retransform_slice();
}

void MidasSlots::slotInterpolate(bool state)
{
  VW->fastip = state ? 0 : 1;
  retransform_slice();
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

  /* If slider is being dragged, just display number */
   if (mBlackPressed) {
     display_bwslider_value(VW->wBlackval, black);
     mBlackDisplayed = black;
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

  /* If slider is being dragged, just display number */
  if (mWhitePressed) {
    display_bwslider_value(VW->wWhiteval, white);
    mWhiteDisplayed = white;
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


void MidasSlots::midas_keyinput(QKeyEvent *event)
{
  int transtep = 10;
  int bwinc = 3;
  int coninc = (bwinc + 1) / 2;
  int keysym = event->key();
  switch(keysym) {

  case Key_Minus:
    slotZoom(-1);
    break;

  case Key_Equal:
    slotZoom(1);
    break;

    // No separate keypad keys in QT; they are a state flag
  case Key_Left:
    if (event->state() & Keypad) {
      VW->xtrans -= transtep;
      VW->midasGL->draw();
    } else
      slotParameter(-4);
    break;

  case Key_Right:
    if (event->state() & Keypad) {
      VW->xtrans += transtep;
      VW->midasGL->draw();
    } else
      slotParameter(4);
    break;

  case Key_Down:
    if (event->state() & Keypad) {
      VW->ytrans -= transtep;
      VW->midasGL->draw();
    } else
      slotParameter(-5);
    break;

  case Key_Up:
    if (event->state() & Keypad) {
      VW->ytrans += transtep;
      VW->midasGL->draw();
    } else
      slotParameter(5);
    break;

  case Key_O:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-1);
    break;
	  
  case Key_L:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(1);
    break;
	  
  case Key_P:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(2);
    break;
	  
  case Key_Semicolon:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-2);
    break;
	  
  case Key_BracketLeft:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(3);
    break;
	  
  case Key_Apostrophe:
    if (VW->xtype != XTYPE_MONT)
      slotParameter(-3);
    break;
	  
  case Key_BracketRight:
  case Key_Backslash:
    if (VW->xtype == XTYPE_MONT)
      break;
    if (keysym ==  Key_BracketRight)
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

  case Key_Home:
  case Key_Next:
    show_ref();
    break;

  case Key_Prior:
    show_cur();
    break;

  case Key_Insert:
    show_overlay();
    break;

  case Key_F1:
    if (VW->blackstate >= bwinc)
      setbwlevels(VW->blackstate - bwinc, VW->whitestate, 1);
    break;

  case Key_F2:
    if (VW->blackstate < VW->whitestate - bwinc)
      setbwlevels(VW->blackstate + bwinc, VW->whitestate, 1);
    break;

  case Key_F3:
    if (VW->whitestate > VW->blackstate + bwinc)
      setbwlevels(VW->blackstate, VW->whitestate - bwinc, 1);
    break;

  case Key_F4:
    if (VW->whitestate < 256 - bwinc)
      setbwlevels(VW->blackstate, VW->whitestate + bwinc, 1);
    break;

  case Key_F5:
    if (VW->whitestate < 256 - bwinc)
      setbwlevels(VW->blackstate + bwinc, VW->whitestate + bwinc, 1);
    break;

  case Key_F6:
    if (VW->blackstate >= bwinc)
      setbwlevels(VW->blackstate - bwinc, VW->whitestate - bwinc, 1);
    break;

  case Key_F7:
    if (VW->blackstate < VW->whitestate - 2 * coninc)
      setbwlevels(VW->blackstate - coninc, 
		  VW->whitestate + coninc, 1);
    break;

  case Key_F8:
    if (VW->whitestate < 256 - coninc && VW->blackstate >= coninc)
      setbwlevels(VW->blackstate + coninc,
		  VW->whitestate - coninc, 1);
    break;

  case Key_F11:
    if (VW->reversemap) {
      VW->reversemap = 0;
      VW->reversetoggle->setChecked(false);
    } else {
      VW->reversemap = 1;
      VW->reversetoggle->setChecked(true);
    }
    // The button change will cause a redraw
    break;
	  
  case Key_A:
    if ((event->state() & ShiftButton) && (VW->xtype == XTYPE_MONT))
      slotEdge(1);
    else
      slotSection(1);
    break;

  case Key_B:
    if ((event->state() & ShiftButton) && (VW->xtype == XTYPE_MONT))
      slotEdge(-1);
    else
      slotSection(-1);
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
  float ddx, ddy;
  float zoom = VW->truezoom;
  float thresh = 0.2;

  ddx = (VW->mx - VW->lastmx) / zoom;
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
    if (state & Qt::ShiftButton)
      scale(delrad);
    else
      stretch(delrad, endang);
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

void MidasSlots::sprintf_decimals(char *string, int decimals, int digits, float val)
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

