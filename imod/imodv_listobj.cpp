/*
 *  imodv_listobj.cpp - Object list dialog with controls for grouping objects
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

#include <qscrollview.h>
#include <qframe.h>
#include <qapplication.h>
#include <qcheckbox.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qhbox.h>
#include <qtooltip.h>
#include <qlayout.h>
#include "dia_qtutils.h"

#include "imodv.h"
#include "imod.h"
#include "control.h"
#include "preferences.h"
#include "imodv_objed.h"
#include "imodv_listobj.h"
#include "imodv_input.h"

// The pointer to the instance
static ImodvOlist *Oolist_dialog = NULL;

#define MAX_OOLIST_BUTTONS  5000
#define MAX_OOLIST_WIDTH 384
#define MAX_LIST_IN_COL 36
#define MAX_LIST_NAME 40

// The button arrays, and variable to keep track of grouping
static QCheckBox **OolistButtons;
static QCheckBox **groupButtons;
static int numOolistButtons = 0;
static bool grouping = false;

/*
 * Create the object list dialog
 */ 
void imodvObjectListDialog(ImodvApp *a, int state)
{
  int m;
  QString qstr;
  char *window_name;

  if (!state){
    if (Oolist_dialog)
      Oolist_dialog->close();
    return;
  }
  if (Oolist_dialog){
    Oolist_dialog->raise();
    return;
  }
  grouping = false;

  // Get number of buttons, number of columns and number per column
  // Make maximum number of buttons needed for all loaded models
  for (m = 0; m < a->nm; m++)
    if (numOolistButtons < a->mod[m]->objsize) 
      numOolistButtons = a->mod[m]->objsize; 
  if (numOolistButtons > MAX_OOLIST_BUTTONS)
    numOolistButtons = MAX_OOLIST_BUTTONS;

  OolistButtons = (QCheckBox **)malloc(numOolistButtons * sizeof(QCheckBox *));
  groupButtons = (QCheckBox **)malloc(numOolistButtons * sizeof(QCheckBox *));
  if (!OolistButtons || !groupButtons) {
    if (OolistButtons)
      free(OolistButtons);
    if (groupButtons)
      free(groupButtons);
    numOolistButtons = 0;
    wprint("\aMemory error getting array for checkboxes\n");
    return;
  }
       
  Oolist_dialog = new ImodvOlist(imodvDialogManager.parent(IMODV_DIALOG));

  imodvOlistUpdateOnOffs(a);

  // Get sizes to adjust window size with
  QSize svSize = Oolist_dialog->mScroll->sizeHint();
  QSize frameSize = Oolist_dialog->mFrame->sizeHint();
  Oolist_dialog->adjustSize();

  // 4 pixels added was enough to prevent scroll bars
  // If width is constrained, allow more height for horizontal scroll bar
  int newWidth = Oolist_dialog->width() + frameSize.width() - svSize.width() +
    8;
  int newHeight = Oolist_dialog->height() + frameSize.height() - 
    svSize.height() + 8;
  if (newWidth > MAX_OOLIST_WIDTH) {
    newWidth = MAX_OOLIST_WIDTH;
    newHeight += 20;
  }
  if (newHeight > QApplication::desktop()->height() - 100)
    newHeight = QApplication::desktop()->height() - 100;
  Oolist_dialog->resize(newWidth, newHeight);

  window_name = imodwEithername("3dmodv Object List: ", a->imod->fileName, 1);
  if (window_name) {
    qstr = window_name;
    free(window_name);
  }
  if (qstr.isEmpty())
    qstr = "3dmodv Object List";
  Oolist_dialog->setCaption(qstr);
  imodvDialogManager.add((QWidget *)Oolist_dialog, IMODV_DIALOG);

  // After getting size with group buttons present, hide them
  Oolist_dialog->groupToggled(false);
  Oolist_dialog->show();
}

// Set On/Off state for one object
void imodvOlistSetChecked(ImodvApp *a, int ob, bool state)
{
  if (!Oolist_dialog)
    return;
  if (ob < numOolistButtons && ob < a->imod->objsize)
    diaSetChecked(OolistButtons[ob], state);
}

// Set color for one object
void imodvOlistSetColor(ImodvApp *a, int ob)
{
  Iobj *obj;
  if (!Oolist_dialog || a->ob >= numOolistButtons)
    return;
  obj = &a->imod->obj[ob];
  OolistButtons[a->ob]->setPaletteBackgroundColor
    (QColor((int)(255 * obj->red), (int)(255 * obj->green),
            (int)(255 * obj->blue)));
}

// Update the group control buttons and buttons for objects
void imodvOlistUpdateGroups(ImodvApp *a)
{
  // Nothing to do yet
}

void imodvOlistUpdateOnOffs(ImodvApp *a)
{
  int ob;
  bool state;
  QString qstr;
  char obname[MAX_LIST_NAME];
  int len;
  QColor bkgColor;
  QColor gray;
  if (!Oolist_dialog || !numOolistButtons)
    return;

  gray = Oolist_dialog->paletteBackgroundColor();
  for (ob = 0; ob < numOolistButtons; ob++) {
    if (ob < a->imod->objsize) {
      // Get a truncated name
      // DMN 9/20/04: just truncate all columns a little bit now
      len = strlen(a->imod->obj[ob].name);
      if (len > MAX_LIST_NAME - 1)
        len = MAX_LIST_NAME - 1;
      strncpy(obname, a->imod->obj[ob].name, len);
      obname[len] = 0x00;
      qstr.sprintf("%d: %s",ob + 1, obname);
      OolistButtons[ob]->setText(qstr);
      state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
      bkgColor.setRgb((int)(255. * a->imod->obj[ob].red),
                      (int)(255. * a->imod->obj[ob].green),
                      (int)(255. * a->imod->obj[ob].blue));
    } else {
      state = false;
      bkgColor = gray;
    }
    OolistButtons[ob]->setEnabled(ob < a->imod->objsize);
    OolistButtons[ob]->setPaletteBackgroundColor(bkgColor);
    diaSetChecked(OolistButtons[ob], state);
  }
}

/*
 * Return true if an object should be edited as part of the group
 */
bool imodvOlistObjInGroup(ImodvApp *a, int ob)
{
  if (!Oolist_dialog || !numOolistButtons || !grouping || 
      ob >= numOolistButtons) 
    return false;
  return groupButtons[ob]->isOn();
}

// Simply return flag for whether grouping is on
bool imodvOlistGrouping(void)
{
  return grouping;
}

/*
 * Object list class constructor
 */
ImodvOlist::ImodvOlist(QWidget *parent, const char *name, WFlags fl)
  : QWidget(parent, name, fl)
{
  int nPerCol, olistNcol, ob;
  QString qstr;

  QVBoxLayout *layout = new QVBoxLayout(this, 11, 6, "list layout");
  QCheckBox *check = diaCheckBox("Select object group", this, layout);
  connect(check, SIGNAL(toggled(bool)), this, SLOT(groupToggled(bool)));
  QToolTip::add(check, "Turn on buttons for selecting objects to edit "
                "together");

  mScroll = new QScrollView(this);
  layout->addWidget(mScroll);
  mFrame = new QFrame(mScroll->viewport());
  mScroll->addChild(mFrame);
  mScroll->viewport()->setPaletteBackgroundColor
    (mFrame->paletteBackgroundColor());
  mGrid = new QGridLayout(mFrame, 1, 1, 0, 2, "list grid");

  olistNcol = (numOolistButtons + MAX_LIST_IN_COL - 1) / MAX_LIST_IN_COL;
  nPerCol = (numOolistButtons + olistNcol - 1) / olistNcol;

  // Get a signal mapper, connect to the slot for these buttons
  QSignalMapper *mapper = new QSignalMapper(this);
  connect(mapper, SIGNAL(mapped(int)), this, SLOT(toggleListSlot(int)));
  QSignalMapper *gmapper = new QSignalMapper(this);
  connect(gmapper, SIGNAL(mapped(int)), this, SLOT(toggleGroupSlot(int)));
  
  // Make the buttons, set properties and map them
  for (ob = 0; ob < numOolistButtons; ob++) {
    QHBoxLayout *hLayout = new QHBoxLayout();
    mGrid->addLayout(hLayout, ob % nPerCol, ob / nPerCol);
    groupButtons[ob] = diaCheckBox("", mFrame, hLayout);
    qstr.sprintf("%d: ",ob + 1);
    OolistButtons[ob] = diaCheckBox((char *)qstr.latin1(), mFrame, hLayout);
    mapper->setMapping(OolistButtons[ob], ob);
    connect(OolistButtons[ob], SIGNAL(toggled(bool)), mapper, SLOT(map()));
    gmapper->setMapping(groupButtons[ob], ob);
    connect(groupButtons[ob], SIGNAL(toggled(bool)), gmapper, SLOT(map()));
    hLayout->setStretchFactor(OolistButtons[ob], 100);

    // Hide the buttons later after window size is set
  }

  // Make a line
  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  layout->addWidget(line);

  QHBox *box = new QHBox(this);
  QPushButton *button = new QPushButton("Done", box);
  diaSetButtonWidth(button, ImodPrefs->getRoundedStyle(), 1.4, "Done");
  button->setFocusPolicy(QWidget::NoFocus);
  layout->addWidget(box);
  connect(button, SIGNAL(clicked()), this, SLOT(donePressed()));
}

void ImodvOlist::toggleGroupSlot(int ob)
{
  // Nothing happens here yet: buttons are consulted directly
}
 
void ImodvOlist::groupToggled(bool state)
{
  int ob;
  grouping = state;
  for (ob = 0; ob < numOolistButtons; ob++) {
    if (grouping)
      groupButtons[ob]->show();
    else {
      groupButtons[ob]->hide();
      diaSetChecked(groupButtons[ob], false);
    }
  }
}

void ImodvOlist::toggleListSlot(int ob)
{
  objedToggleObj(ob, OolistButtons[ob]->isOn());
}

void ImodvOlist::donePressed()
{
  close();
}

void ImodvOlist::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)Oolist_dialog);
  Oolist_dialog  = NULL;
  numOolistButtons = 0;
  free(OolistButtons);
  free(groupButtons);
  grouping = false;
  e->accept();
}

void ImodvOlist::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    imodvKeyPress(e);
}

void ImodvOlist::keyReleaseEvent ( QKeyEvent * e )
{
  imodvKeyRelease(e);
}

/*

$Log$

*/
