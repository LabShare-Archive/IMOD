/*
 *  mv_listobj.cpp - Object list dialog with controls for grouping objects
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qscrollarea.h>
#include <qframe.h>
#include <qapplication.h>
#include <qcheckbox.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qlabel.h>
#include <qtooltip.h>
#include <qlayout.h>
#include <qspinbox.h>
#include <qlineedit.h>
#include <qgroupbox.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QVBoxLayout>
#include <QDesktopWidget>
#include "dia_qtutils.h"
#include "objgroup.h"

#include "imodv.h"
#include "mv_gfx.h"
#include "imod.h"
#include "info_cb.h"
#include "control.h"
#include "preferences.h"
#include "mv_objed.h"
#include "mv_listobj.h"
#include "mv_input.h"

// The pointer to the instance
static ImodvOlist *Oolist_dialog = NULL;

#define MAX_OOLIST_BUTTONS  5000
#define MAX_OOLIST_WIDTH 384
#define MAX_LIST_IN_COL 36
#define MAX_LIST_NAME 40

enum {OBJGRP_NEW = 0, OBJGRP_DELETE, OBJGRP_CLEAR, OBJGRP_ADDALL, OBJGRP_SWAP,
      OBJGRP_TURNON, OBJGRP_TURNOFF, OBJGRP_OTHERSON, OBJGRP_OTHERSOFF};

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
  for (m = 0; m < a->numMods; m++)
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
  imod_info_input();
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

  setModvDialogTitle(Oolist_dialog, "3dmodv Object List: ");
  imodvDialogManager.add((QWidget *)Oolist_dialog, IMODV_DIALOG);

  // After getting size with group buttons present, maybe hide them
  Oolist_dialog->updateGroups(a);
  adjustGeometryAndShow((QWidget *)Oolist_dialog, IMODV_DIALOG);
  Oolist_dialog->adjustFrameSize();
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
  if (!Oolist_dialog || ob >= numOolistButtons || ob >= a->imod->objsize)
    return;
  obj = &a->imod->obj[ob];
  diaSetWidgetColor
    (OolistButtons[ob], QColor((int)(255 * obj->red), (int)(255 * obj->green),
            (int)(255 * obj->blue)));
}

// Update the group control buttons and buttons for objects
void imodvOlistUpdateGroups(ImodvApp *a)
{
  if (!Oolist_dialog || !numOolistButtons)
    return;
  Oolist_dialog->updateGroups(a);
}

void imodvOlistUpdateOnOffs(ImodvApp *a)
{
  int ob;
  bool state, nameChgd = false;
  QString qstr;
  char obname[MAX_LIST_NAME];
  int len;
  QColor bkgColor;
  QColor gray;
  if (!Oolist_dialog || !numOolistButtons)
    return;
  QPalette palette = Oolist_dialog->palette();
  gray = palette.color(Oolist_dialog->backgroundRole());

  // Hiding seems to help the updates some, especially if there are text 
  // changes or massive turning off of buttons, but processing events after
  // hiding doesn't help the time on Linux and leaves it gray (3/26/09)
  if (numOolistButtons > 256)
    Oolist_dialog->mFrame->hide();
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
      state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
      bkgColor.setRgb((int)(255. * a->imod->obj[ob].red),
                      (int)(255. * a->imod->obj[ob].green),
                      (int)(255. * a->imod->obj[ob].blue));
      if (OolistButtons[ob]->isHidden()) {
        OolistButtons[ob]->show();
        nameChgd = true;
      }
      if (qstr != OolistButtons[ob]->text()) {
        OolistButtons[ob]->setText(qstr);
        nameChgd = true;
      }
      diaSetWidgetColor(OolistButtons[ob], bkgColor);
      diaSetChecked(OolistButtons[ob], state);
    } else if (!OolistButtons[ob]->isHidden()) {
      OolistButtons[ob]->hide();
      nameChgd = true;
    }
  }
  if (numOolistButtons > 256)
    Oolist_dialog->mFrame->show();
  if (nameChgd) 
    Oolist_dialog->adjustFrameSize();
}

/*
 * Return true if an object should be edited as part of the group
 */
bool imodvOlistObjInGroup(ImodvApp *a, int ob)
{
  if (!Oolist_dialog || !numOolistButtons || !grouping || 
      ob >= numOolistButtons) 
    return false;
  return groupButtons[ob]->isChecked();
}

// Simply return flag for whether grouping is on
bool imodvOlistGrouping(void)
{
  return grouping;
}

/*
 * Object list class constructor
 */
ImodvOlist::ImodvOlist(QWidget *parent, Qt::WFlags fl)
  : QWidget(parent, fl)
{
  int nPerCol, olistNcol, ob, i;
  QString qstr;
  QLabel *label;
  const char *labels[] = {"New", "Delete", "Clear", "Add All", "Swap", "ON", "OFF",
                    "On", "Off"};
  const char *tips[] = {"Start a new object group, copied from current group",
                  "Remove the current object group from list of groups",
                  "Remove all objects from the current group",
                  "Add all objects to the current group",
                  "Remove current members and add all non-members",
                  "Turn ON all objects in the current group",
                  "Turn OFF all objects in the current group",
                  "Turn ON objects NOT in the current group",
                  "Turn OFF objects NOT in the current group"};
  
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  QVBoxLayout *layout = new QVBoxLayout(this);
  layout->setContentsMargins(5, 5, 5, 5);
  layout->setSpacing(6);

  QGroupBox *grpbox = new QGroupBox("Object Group Selection");
  layout->addWidget(grpbox);
  QVBoxLayout *gblay = new QVBoxLayout(grpbox);
  gblay->setContentsMargins(6, 3, 6, 6);
  gblay->setSpacing(6);
  QHBoxLayout *hbox = diaHBoxLayout(gblay); 
  
  mGroupSpin = (QSpinBox *)diaLabeledSpin(0, 0., 0., 1., "Group", grpbox,hbox);
  mGroupSpin->setSpecialValueText("None");
  connect(mGroupSpin, SIGNAL(valueChanged(int)), this, 
          SLOT(curGroupChanged(int)));
  mGroupSpin->setToolTip("Select the current object group or turn off "
                "selection");

  mNumberLabel = diaLabel("/99", grpbox, hbox);
  mNameEdit = new QLineEdit(grpbox);
  hbox->addWidget(mNameEdit);
  mNameEdit->setMaxLength(OBJGRP_STRSIZE - 1);
  mNameEdit->setFocusPolicy(Qt::ClickFocus);
  connect(mNameEdit, SIGNAL(returnPressed()), this,
          SLOT(returnPressed()));
  connect(mNameEdit, SIGNAL(textChanged(const QString&)), this,
          SLOT(nameChanged(const QString&)));
  mNameEdit->setToolTip("Enter a name for the current group");

  QSignalMapper *clickMapper = new QSignalMapper(this);
  connect(clickMapper, SIGNAL(mapped(int)), this,
          SLOT(actionButtonClicked(int)));

  // Make the buttons and put in hbox
  for (i = 0; i < OBJLIST_NUMBUTTONS; i++) {

    if (i == 0 || i == 5)
      hbox = diaHBoxLayout(gblay);
    if (i == 7) {
      label = diaLabel("Others:", grpbox, hbox);
      label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    }

    qstr = labels[i];
    mButtons[i] = diaPushButton(LATIN1(qstr), grpbox, hbox);
    clickMapper->setMapping(mButtons[i], i);
    connect(mButtons[i], SIGNAL(clicked()), clickMapper, SLOT(map()));
    mButtons[i]->setToolTip(tips[i]);
  }

  mScroll = new QScrollArea(this);
  layout->addWidget(mScroll);
  mFrame = new QFrame();
  mScroll->setAutoFillBackground(true);
  QPalette palette = mFrame->palette();
  QColor bkg = palette.color(mFrame->backgroundRole());
  diaSetWidgetColor(mScroll->viewport(), bkg);
  mGrid = new QGridLayout(mFrame);
  mGrid->setSpacing(2);
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
    OolistButtons[ob] = diaCheckBox(LATIN1(qstr), mFrame, hLayout);
    OolistButtons[ob]->setAutoFillBackground(true);
    mapper->setMapping(OolistButtons[ob], ob);
    connect(OolistButtons[ob], SIGNAL(toggled(bool)), mapper, SLOT(map()));
    gmapper->setMapping(groupButtons[ob], ob);
    connect(groupButtons[ob], SIGNAL(toggled(bool)), gmapper, SLOT(map()));
    hLayout->setStretchFactor(OolistButtons[ob], 100);

    // Hide the buttons later after window size is set
    grouping = true;
  }
  mScroll->setWidget(mFrame);

  // Make a line
  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  layout->addWidget(line);

  QHBoxLayout *box = diaHBoxLayout(layout);
  mDoneButton = diaPushButton("Done", this, box);
  connect(mDoneButton, SIGNAL(clicked()), this, SLOT(donePressed()));
  mHelpButton = diaPushButton("Help", this, box);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(helpPressed()));
  setFontDependentWidths();
}

void ImodvOlist::toggleGroupSlot(int ob)
{
  int index;
  bool state;
  IobjGroup *group = (IobjGroup *)ilistItem(Imodv->imod->groupList, 
                                            Imodv->imod->curObjGroup);
  if (!group)
    return;
  index = objGroupLookup(group, ob);
  state = groupButtons[ob]->isChecked();
  if (state && index < 0)
    objGroupAppend(group, ob);
  else if (!state && index >= 0)
    ilistRemove(group->objList, index);
}
 
void ImodvOlist::toggleListSlot(int ob)
{
  objedToggleObj(ob, OolistButtons[ob]->isChecked());
}

void ImodvOlist::donePressed()
{
  close();
}

void ImodvOlist::helpPressed()
{
  imodShowHelpPage("objectList.html#TOP");
}

void ImodvOlist::actionButtonClicked(int which)
{
  Imod *imod = Imodv->imod;
  IobjGroup *group, *ogroup;
  int ob, index, changed = 1;

  if (which != OBJGRP_NEW && which != OBJGRP_DELETE) {
    group = (IobjGroup *)ilistItem(imod->groupList, imod->curObjGroup);
    if (!group)
      return;
  }

  switch (which) {
  case OBJGRP_NEW:
    imodvRegisterModelChg();
    group = objGroupListExpand(&imod->groupList);
    if (!group)
      break;
    ogroup = (IobjGroup *)ilistItem(imod->groupList, imod->curObjGroup);
    if (ogroup)
      group->objList = ilistDup(ogroup->objList);
    imod->curObjGroup = ilistSize(imod->groupList) - 1;
    updateGroups(Imodv);
    break;

  case OBJGRP_DELETE:
    if (imod->curObjGroup < 0)
      return;
    imodvRegisterModelChg();
    if (objGroupListRemove(imod->groupList, imod->curObjGroup))
      break;
    imod->curObjGroup = B3DMIN(B3DMAX(0, imod->curObjGroup - 1),
                               ilistSize(imod->groupList) - 1);
    updateGroups(Imodv);
    break;

  case OBJGRP_CLEAR:
    imodvRegisterModelChg();
    ilistTruncate(group->objList, 0);
    updateGroups(Imodv);
    break;

  case OBJGRP_ADDALL:
    imodvRegisterModelChg();
    if (group->objList)
      ilistTruncate(group->objList, 0);
    for (ob = 0; ob < imod->objsize; ob++)
      if (objGroupAppend(group, ob))
        return;
    updateGroups(Imodv);
    break;

  case OBJGRP_SWAP:
    imodvRegisterModelChg();
    for (ob = 0; ob < imod->objsize; ob++) {
      index = objGroupLookup(group, ob);
      if (index >= 0)
        ilistRemove(group->objList, index);
      else
        if (objGroupAppend(group, ob))
          return;
    }
    updateGroups(Imodv);
    break;

  case OBJGRP_TURNON:
  case OBJGRP_TURNOFF:
  case OBJGRP_OTHERSON:
  case OBJGRP_OTHERSOFF:
    changed = 0;
    for (ob = 0; ob < imod->objsize; ob++) {
      index = objGroupLookup(group, ob);
      if (((index >= 0 && which == OBJGRP_TURNON) || 
           (index < 0 && which == OBJGRP_OTHERSON)) && 
          iobjOff(imod->obj[ob].flags)) {
        imodvRegisterObjectChg(ob);
        imod->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
        changed = 1;
      } else if (((index >= 0 && which == OBJGRP_TURNOFF) ||
                  (index < 0 && which == OBJGRP_OTHERSOFF)) &&
                 !iobjOff(imod->obj[ob].flags)) {
        imodvRegisterObjectChg(ob);
        imod->obj[ob].flags |= IMOD_OBJFLAG_OFF;
        changed = 1;
      }
    }
    imodvDraw(Imodv);
    imodvDrawImodImages();
    imodvObjedNewView();
    break;

  }
  if (changed)
    imodvFinishChgUnit();
}

void ImodvOlist::nameChanged(const QString &str)
{
  IobjGroup *group = (IobjGroup *)ilistItem(Imodv->imod->groupList, 
                                            Imodv->imod->curObjGroup);
  if (!group)
    return;
  strncpy(&group->name[0], LATIN1(str), OBJGRP_STRSIZE);
  group->name[OBJGRP_STRSIZE - 1] = 0x00;
}

void ImodvOlist::curGroupChanged(int value)
{
  setFocus();
  Imodv->imod->curObjGroup = value - 1;
  updateGroups(Imodv);
}

void ImodvOlist::returnPressed()
{
  setFocus();
}

void ImodvOlist::updateGroups(ImodvApp *a)
{
  IobjGroup *group;
  int i, ob, numGroups = ilistSize(a->imod->groupList);
  int curGrp = a->imod->curObjGroup;
  int *objs;
  bool *states;
  bool showChgd = false;
  QString str;
  diaSetSpinMMVal(mGroupSpin, 0, numGroups, curGrp + 1);
  str.sprintf("/%d", numGroups);
  mNumberLabel->setText(str);
  mNameEdit->setEnabled(curGrp >= 0);
  for (i = 1; i < OBJLIST_NUMBUTTONS; i++)
    mButtons[i]->setEnabled(curGrp >= 0);
  if (curGrp < 0) {
    if (grouping) {
      mFrame->hide();
      for (ob = 0; ob < numOolistButtons; ob++)
        groupButtons[ob]->hide();
      mFrame->show();
      adjustFrameSize();
    }
    grouping = false;
    return;
  }
  if (!grouping) {
    mFrame->hide();
    for (ob = 0; ob < numOolistButtons; ob++)
      groupButtons[ob]->show();
    mFrame->show();
    adjustFrameSize();
  }
  grouping = true;

  group = (IobjGroup *)ilistItem(a->imod->groupList, curGrp);
  if (!group)
    return;
  mNameEdit->setText(group->name);
  states = (bool *)malloc(sizeof(bool) * numOolistButtons);
  if (!states)
    return;
  for (ob = 0; ob < numOolistButtons; ob++)
    states[ob] = false;
  objs = (int *)ilistFirst(group->objList);
  if (objs) {
    for (i = 0; i < ilistSize(group->objList); i++)
      if (objs[i] >= 0 && objs[i] < numOolistButtons)
        states[objs[i]] = true;
  }
  for (ob = 0; ob < numOolistButtons; ob++) {
    if (ob < a->imod->objsize) {
      diaSetChecked(groupButtons[ob], states[ob]);
      if (groupButtons[ob]->isHidden()) {
        groupButtons[ob]->show();
        showChgd = true;
      }
    } else if (!groupButtons[ob]->isHidden()) {
      groupButtons[ob]->hide();
      showChgd = true;
    }
  }
  free(states);
  if (showChgd)
    adjustFrameSize();
}

void ImodvOlist::adjustFrameSize()
{
  imod_info_input();
  mFrame->adjustSize();
}

void ImodvOlist::setFontDependentWidths()
{
  int i, width, minWidth;
  bool rounded = ImodPrefs->getRoundedStyle();
  minWidth = diaGetButtonWidth(this, rounded, 1.3, mButtons[0]->text());
  for (i = 0; i < OBJLIST_NUMBUTTONS; i++) {
    width = diaGetButtonWidth(this, rounded, 1.3, mButtons[i]->text());
    width = B3DMAX(minWidth, width);
    mButtons[i]->setFixedWidth(width);
  }
  width = diaGetButtonWidth(this, rounded, 1.8, "Help");
  mHelpButton->setFixedWidth(width);
  mDoneButton->setFixedWidth(width);
}

void ImodvOlist::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
  setFontDependentWidths();
  adjustFrameSize();
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
  if (utilCloseKey(e))
    close();
  else
    imodvKeyPress(e);
}

void ImodvOlist::keyReleaseEvent ( QKeyEvent * e )
{
  imodvKeyRelease(e);
}
