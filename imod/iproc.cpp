/*  IMOD VERSION 2.50
 *
 *  iproc.c -- image processing for imod.
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
Log at end of file
*/

#include <qlabel.h>
#include <qwidgetstack.h>
#include <qlayout.h>
#include <qtooltip.h>
#include <qcombobox.h>
#include <qlistbox.h>
#include <qvbox.h>
#include <qpushbutton.h>
#include "dia_qtutils.h"
#include "multislider.h"
#include "imod.h"
#include "imod_display.h"
#include "iproc.h"
#include "sliceproc.h"
#include "imod_info_cb.h"
#include "control.h"

#define MAX_LIST_TO_SHOW 6

/* internal functions. */
static void clearsec(ImodIProc *ip);
static void savesec(ImodIProc *ip);
static void cpdslice(Islice *sl, ImodIProc *ip);
static void copyAndDisplay();

static void edge_cb();
static void mkedge_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout);
static void thresh_cb();
static void mkthresh_cb(IProcWindow *win, QWidget *parent, 
                        QVBoxLayout *layout);
static void smooth_cb();
static void sharpen_cb();
static void grow_cb();
static void shrink_cb();

/* The table of entries and callbacks */
ImodIProcData proc_data[] = {
  {"edge", edge_cb, mkedge_cb, NULL},
  {"threshold", thresh_cb, mkthresh_cb, NULL},
  {"smooth", smooth_cb, NULL, "Smooth Image."},
  {"sharpen", sharpen_cb, NULL, "Sharpen Edges."},
  {"dilation", grow_cb, NULL, "Grow Threshold Area."},
  {"erosion", shrink_cb, NULL, "Shrink Threshold Area."},
  NULL,
};

/* Static variables for proc structure and a slice */
static ImodIProc proc = {0, 0};
static Islice s;

/*
 * CALLBACK FUNCTIONS FOR THE VARIOUS FILTERS
 */

// Edge enhancement
static void edge_cb()
{
  ImodIProc *ip = &proc;
  Islice *gs;

  switch (ip->edge){
  case 0:
    sliceByteEdgeSobel(&s);
    if(App->depth == 8)
      sliceByteAdd(&s, ip->vi->rampbase);
    break;

  case 1:
    sliceByteEdgePrewitt(&s);
    if(App->depth == 8)
      sliceByteAdd(&s, ip->vi->rampbase);
    break;

  case 2:
    sliceByteEdgeLaplacian(&s);
    break;
	  
  case 3:
    sliceByteGraham(&s);
    break;

  case 4:
    gs = sliceGradient(&s);
    if (!gs) return;
    cpdslice(gs, ip);
    break;

  default:
    break;
  }
}

// Threshold
static void thresh_cb()
{
  ImodIProc *ip = &proc;
  int xysize, thresh, minv, maxv;
  unsigned char *idat, *last;
     
  thresh = ip->threshold;
     
  if (App->depth == 8){
    thresh = (int)
      ((((float)ip->vi->rampsize/256.0f)*thresh) + ip->vi->rampbase);
    minv = ip->vi->rampbase;
    maxv = ip->vi->rampsize + minv;
  }else{
    minv = 0; maxv = 255;
  }

  xysize = ip->vi->xsize * ip->vi->ysize;
  idat = ip->iwork;
  for(last = idat + xysize; idat != last; idat++){
    if (*idat > thresh)
      *idat = maxv;
    else
      *idat = minv;
  }
}

// Smoothing
static void smooth_cb()
{
  ImodIProc *ip = &proc;
  sliceByteSmooth(&s);
}

// Sharpening
static void sharpen_cb()
{
  ImodIProc *ip = &proc;
  sliceByteSharpen(&s);
}

// Growing a thresholded area
static void grow_cb()
{
  ImodIProc *ip = &proc;

  if (App->depth == 8){
    s.min = ip->vi->rampbase;
    s.max = ip->vi->rampsize + s.min;
  }else{
    s.min = 0; s.max = 255;
  }

  // If the slice is not modified, run a threshold on it
  if (!ip->modified)
    thresh_cb();

  sliceByteGrow(&s,  (int)s.max);
}

// Shrinking a thresholded area
static void shrink_cb()
{
  ImodIProc *ip = &proc;

  if (App->depth == 8){
    s.min = ip->vi->rampbase;
    s.max = ip->vi->rampsize + s.min;
  }else{
    s.min = 0; s.max = 255;
  }

  // If the slice is not modified, run a threshold on it
  if (!ip->modified)
    thresh_cb();

  sliceByteShrink(&s,  (int)s.max);
}


/* Reset and get new data buffer */
int iprocRethink(struct ViewInfo *vi)
{
  if (proc.dia){
    if (proc.isaved) {
      clearsec(&proc);
      proc.idatasec = -1;
      free(proc.isaved);
      free(proc.iwork);
    }
    proc.isaved = (unsigned char *)malloc(vi->xsize * vi->ysize);
    if (!proc.isaved) {
      proc.iwork = NULL;
      proc.dia->close();
      return 1;
    }

    proc.iwork = (unsigned char *)malloc(vi->xsize * vi->ysize);
    if (!proc.iwork) {
      free(proc.isaved);
      proc.isaved = NULL;
      proc.dia->close();
      return 1;
    }
  }
  return 0;
}

/* Open the processing dialog box */
int inputIProcOpen(struct ViewInfo *vi)
{
  if (!proc.dia){
    if (!proc.vi) {
      proc.procnum = 0;
      proc.threshold = 128;
      proc.edge = 0;
    }
    proc.vi = vi;
    proc.idatasec = -1;
    proc.idatatime = 0;
    proc.modified = 0;
    proc.isaved = (unsigned char *)malloc(vi->xsize * vi->ysize);

    if (!proc.isaved)
      return(-1);

    proc.iwork = (unsigned char *)malloc(vi->xsize * vi->ysize);
    if (!proc.iwork) {
      free(proc.isaved);
      return(-1);
    }

    proc.dia = new IProcWindow(imodDialogManager.parent(IMOD_DIALOG), NULL);
    imodDialogManager.add((QWidget *)proc.dia, IMOD_DIALOG);

  }else{
    proc.dia->raise();
  }
  return(0);
}


/*
 * DATA COPYING FUNCTIONS
 */

/* Copy data from a generated slice into the working buffer and free slice */
static void cpdslice(Islice *sl, ImodIProc *ip)
{
  register unsigned char *from, *to, *last;
  int rampbase = ip->vi->rampbase;
  from = sl->data.b;
  to = ip->iwork;
  if (!to) return;

  last = to + (ip->vi->xsize * ip->vi->ysize);
  if (App->depth > 8){
    do{
      *to++ = *from++;
    }while (to !=  last);
  }else{
    do{
      *to++ = *from++ + rampbase;
    }while (to !=  last);
  }
  sliceFree(sl);
}

/* Copy the working buffer back to the display memory and draw */
static void copyAndDisplay()
{
  ImodIProc *ip = &proc;
  unsigned char **to = ivwGetCurrentZSection(ip->vi);
  unsigned char *from = ip->iwork;
  int i, j;
  int cz =  (int)(ip->vi->zmouse + 0.5f);
  
  for (j = 0; j < ip->vi->ysize; j++)
    for (i = 0; i < ip->vi->xsize; i++)
      to[j][i] = *from++;

  imod_info_float_clear(cz, ip->vi->ct);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
}


/* clear the section back to original data. */
static void clearsec(ImodIProc *ip)
{
  register unsigned char *from, *to;
  unsigned char **to2;
  int i, j;
     
  if (ip->idatasec < 0 || !ip->modified)
    return;

  from = ip->isaved;
  to = ip->iwork;
  to2 = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  if (!to2)
    return;
  for (j = 0; j < ip->vi->ysize; j++)
    for (i = 0; i < ip->vi->xsize; i++)
      *to++ = to2[j][i] = *from++;

  ip->modified = 0;
  imod_info_float_clear(ip->idatasec, ip->idatatime);
}

/* save the displayed image to saved and working buffers. */
static void savesec(ImodIProc *ip)
{
  register unsigned char *to, *to2;
  unsigned char **from;
  int i, j;
     
  if (ip->idatasec < 0)
    return;

  to   = ip->isaved;
  to2  = ip->iwork;
  from = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  if (!from) 
    return;
  for (j = 0; j < ip->vi->ysize; j++)
    for (i = 0; i < ip->vi->xsize; i++)
      *to++ = *to2++ = from[j][i];

  // Make sure there is floating info for this data so it can be saved when
  // it is cleared
  imod_info_bwfloat(ip->vi, ip->idatasec, ip->idatatime);
}


/* Functions to make the widgets for particular filters */
static void mkedge_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  diaLabel("Edge Enhancement Filter Type:", parent, layout);
  QComboBox *edgeBox = new QComboBox(parent);
  layout->addWidget(edgeBox);
  edgeBox->insertItem("Sobel");
  edgeBox->insertItem("Prewitt");
  edgeBox->insertItem("Laplacian");
  edgeBox->insertItem("Graham");
  edgeBox->insertItem("Gradient");
  edgeBox->setFocusPolicy(QComboBox::NoFocus);
  edgeBox->setCurrentItem(proc.edge);
  QObject::connect(edgeBox, SIGNAL(activated(int)), win, 
                   SLOT(edgeSelected(int)));
}

static void mkthresh_cb(IProcWindow *win, QWidget *parent, QVBoxLayout *layout)
{
  char *sliderLabel[] = {"Threshold filter value" };
  MultiSlider *slider = new MultiSlider(parent, 1, sliderLabel, 0, 254);
  slider->setValue(0, proc.threshold);
  QObject::connect(slider, SIGNAL(sliderChanged(int, int, bool)), win, 
          SLOT(threshChanged(int, int, bool)));
  layout->addLayout(slider->getLayout());
}



/* THE WINDOW CLASS CONSTRUCTOR */
static char *buttonLabels[] = {"Apply", "More", "Toggle", "Reset", "Save", 
                               "Done", "Help"};
static char *buttonTips[] = {"Operate on current section",
                             "Reiterate operation on current section",
                             "Toggle between processed and original image",
                             "Reset section to unprocessed image",
                             "Replace section in memory with processed image",
                             "Close dialog box", "Open help window"};

IProcWindow::IProcWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 7, buttonLabels, buttonTips, false, 
                " ", "", name)
{
  int i;
  QString str;
  QVBoxLayout *vLayout;
  QWidget *control;

  // Put an H layout inside the main layout, then fill that with the
  // List box and the widget stack
  QHBoxLayout *hLayout = new QHBoxLayout(mLayout);
  mListBox = new QListBox(this);
  hLayout->addWidget(mListBox);
  mListBox->setFocusPolicy(QListBox::NoFocus);

  mStack = new QWidgetStack(this);
  hLayout->addWidget(mStack);

  // Put a spacer on the right to keep the list box position from changing
  QHBox *hspace = new QHBox(this);
  hLayout->addWidget(hspace);
  hLayout->setStretchFactor(hspace, 5);
  

  for (i = 0; (proc_data[i].name); i++) {

    // For each item, add to list box, make a widget and give it a V layout
    mListBox->insertItem(proc_data[i].name);
    control = new QWidget(this);
    vLayout = new QVBoxLayout(control, 3, 6);

    // Call the make widget function or just add a label
    if (proc_data[i].mkwidget)
      proc_data[i].mkwidget (this, control, vLayout);
    else {
      diaLabel(proc_data[i].label, control, vLayout);
    }

    // Fill box with spacer
    QVBox *spacer = new QVBox(control);
    vLayout->addWidget(spacer);
    vLayout->setStretchFactor(spacer, 100);

    // Add widget to stack and set size policy to ignored
    mStack->addWidget(control, i);
    control->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
  }

  // Finalize list box setting and connections
  mListBox->setCurrentItem(proc.procnum);
  connect(mListBox, SIGNAL(highlighted(int)), this,
          SLOT(filterHighlighted(int)));
  connect(mListBox, SIGNAL(selected(int)), this, SLOT(filterSelected(int)));
  if (i > MAX_LIST_TO_SHOW)
    i = MAX_LIST_TO_SHOW;
  mListBox->setMinimumHeight(i * mListBox->itemHeight() + 4);
  QSize size = mListBox->sizeHint();
  //  mListBox->setFixedWidth(size.width());

  filterHighlighted(proc.procnum);

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonClicked(int)));
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setCaption(imodCaption("3dmod Image Processing"));
  show();
}

/* Action functions */
void IProcWindow::threshChanged(int which, int value, bool dragging)
{
  proc.threshold = value;
}

// To switch filters, set the size policy of the current widget back to ignored
// raise the new widget, set its size policy, make the stack process geometry
// again then adjust window size
void IProcWindow::filterHighlighted(int which)
{
  QWidget *control = mStack->visibleWidget();
  if (control)
    control->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
  proc.procnum = which;
  mStack->raiseWidget(which);
  control = mStack->visibleWidget();
  control->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  mStack->adjustSize();
  adjustSize();
}

void IProcWindow::filterSelected(int which)
{
  filterHighlighted(which);
  apply();
}

void IProcWindow::edgeSelected(int which)
{
  proc.edge = which;
}

// Respond to button click (release)
void IProcWindow::buttonClicked(int which)
{
  ImodIProc *ip = &proc;

  int cz =  (int)(ip->vi->zmouse + 0.5f);

  if (which < 5 && ip->vi->loadingImage)
    return;

  switch (which) {
  case 0:  // Apply
    apply();
    break;

  case 1:  // More
    /* If this is not the same section, treat it as an Apply */
    if (cz != ip->idatasec || ip->vi->ct != ip->idatatime) {
      apply();
      break;
    }

    /* Otherwise operate on the current data without restoring it */
    if ( proc_data[ip->procnum].cb) {
      proc_data[ip->procnum].cb();
      copyAndDisplay();
      ip->modified = 1;
    }
    break;

  case 2:  // Toggle
    if (ip->modified && cz == ip->idatasec && ip->vi->ct == ip->idatatime)
      copyAndDisplay();
    break;

  case 3: // reset
    clearsec(ip);
    imodDraw(ip->vi, IMOD_DRAW_IMAGE);
    break;

  case 4: // save
    ip->modified = 0;
    ip->idatasec = -1;
    break;

  case 5: // Done
    close();
    break;

  case 6: // Help
    dia_vasmsg
      ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
       "3dmod Image Processing \n"
       "~~~~~~~~~~~~~~~~~~~~~~~~"
       "\n\n",
       "Various kinds of simple filters can be applied with these "
       "controls.  The filter will always be applied to the current "
       "section.\n\n",
       "Single-click in the list of filters to select the current filter "
       "to be applied to the data; in some cases there will be further "
       "parameters to select.\n\n"
       "Selecting the [Apply] button will apply the current filter to the "
       "ORIGINAL image data.  Double-clicking in the filter list is the "
       "same as selecting the [Apply] button.\n\n"
       "Selecting the [More] button will apply the filter to the CURRENT "
       "image data, as modified by previous filter operations.\n\n"
       "Selecting the [Reset] button, applying a filter to a different "
       "section, closing the window with [Done], or flipping the data "
       "volume will all restore the original image data for a section, "
       "unless you select the [Save] button.  [Save] will permanently "
       "replace the image data in memory with the processed data.\n\n",
       NULL);
    break;
  }
}

// Respond to button press for toggle button only - redisplay original data
// but re-mark as modified
void IProcWindow::buttonPressed(int which)
{
  unsigned char *from;
  unsigned char **to2;
  int i, j;
  ImodIProc *ip = &proc;
  int cz =  (int)(ip->vi->zmouse + 0.5f);

  if (which != 2 || !ip->modified || cz != ip->idatasec || 
      ip->vi->ct != ip->idatatime)
    return;
     
  from = ip->isaved;
  to2 = ivwGetZSectionTime(ip->vi, ip->idatasec, ip->idatatime);
  if (!to2)
    return;
  for (j = 0; j < ip->vi->ysize; j++)
    for (i = 0; i < ip->vi->xsize; i++)
      to2[j][i] = *from++;

  imod_info_float_clear(ip->idatasec, ip->idatatime);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
}

// Apply the current filter
void IProcWindow::apply()
{
  ImodIProc *ip = &proc;
  sliceInit(&s, ip->vi->xsize, ip->vi->ysize, 0, ip->iwork);

  int cz =  (int)(ip->vi->zmouse + 0.5f);

  /* Unconditionally restore data if modified */
  clearsec(ip);

  /* If this is a new section, save the data */
  if (cz != ip->idatasec || ip->vi->ct != ip->idatatime) {
    ip->idatasec = cz;
    ip->idatatime = ip->vi->ct;
    savesec(ip);
  }
    
  /* Operate on the original data */
  if ( proc_data[ip->procnum].cb) {
    proc_data[ip->procnum].cb();
    ip->modified = 1;
    copyAndDisplay();
  }
}

// The window is closing, clean up and remove from manager
void IProcWindow::closeEvent ( QCloseEvent * e )
{
  ImodIProc *ip = &proc;
  if (!ip->dia)
    return;
  clearsec(ip);
  imodDialogManager.remove((QWidget *)ip->dia);
  imodDraw(ip->vi, IMOD_DRAW_IMAGE);
  if (ip->isaved)
    free(ip->isaved);
  if (ip->iwork)
    free(ip->iwork);
  ip->dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void IProcWindow::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void IProcWindow::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

/*

    $Log$
    Revision 4.5  2004/01/05 18:04:56  mast
    Prevented operating on images while data being loaded; renamed vw to vi

    Revision 4.4  2003/09/16 02:10:26  mast
    Changed to make a working copy of the image data using the new line
    pointers, operate on the working copy, and save back into the display
    data as needed.

    Revision 4.3  2003/04/25 03:28:32  mast
    Changes for name change to 3dmod

    Revision 4.2  2003/04/17 18:43:38  mast
    adding parent to window creation

    Revision 4.1  2003/02/10 20:29:02  mast
    autox.cpp

    Revision 1.1.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 1.1.2.1  2003/01/23 19:57:06  mast
    Qt version

    Revision 3.2.2.1  2003/01/13 01:15:43  mast
    changes for Qt version of info window

    Revision 3.2  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
