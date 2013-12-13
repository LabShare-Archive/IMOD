/* 
 *  dialog_frame.cpp       Implementation of a base class for non-modal dialogs
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qlayout.h>
#include <qframe.h>
#include <qevent.h>
#include <qtooltip.h>
#include <qpushbutton.h>
#include <qsignalmapper.h>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include "dialog_frame.h"
#include "dia_qtutils.h"

/*!
 * DialogFrame provides a widget whose default style is to be a dialog box that
 * destroys itself on closing.  Its main area is a QVBoxLayout, protected
 * member {mLayout}, that can be populated with widgets by the inheriting 
 * class.  The bottom row(s) will have [numButton] buttons, with text given in
 * [labels].  Tooltips for each button can be provided in [tips] if it is
 * non-NULL.  The buttons will be equally sized if [equalSized] is true; 
 * otherwise they will all be just big enough for their respective text.  The
 * window title will be set to [caption], or to [fallback] if [caption] is 
 * NULL.  [name] defaults to 0, [fl] defaults to Qt::Window. 
 * ^ The class emits two signals: actionPressed(int which) and 
 * actionClicked(int which)
 * with the argument providing the number of the button pressed or clicked.
 */
DialogFrame::DialogFrame(QWidget *parent, int numButtons, const char *labels[], 
			 const char *tips[],
			 bool equalSized, const char *caption, const char *fallback,
			 const char *name, Qt::WFlags fl)
  : QWidget(parent, fl)
{
  makeDialogFrame(parent, numButtons, 1, labels, tips, equalSized, false,
                  caption, fallback, fl);
}

/*!
 * This alternate constructor includes [numRows] to specify the number of rows,
 * and [rounded] to indicate that the style has rounded buttons.  Other
 * items are as above.
 */
DialogFrame::DialogFrame(QWidget *parent, int numButtons, int numRows,
                         const char *labels[], const char *tips[], bool equalSized,
                         bool rounded, const char *caption, const char *fallback,
			 const char *name, Qt::WFlags fl)
  : QWidget(parent, fl)
{
  makeDialogFrame(parent, numButtons, numRows, labels, tips, equalSized, 
                  rounded, caption, fallback, fl);
}

void DialogFrame::makeDialogFrame(QWidget *parent, int numButtons, int numRows,
                                  const char *labels[], const char *tips[],
                                  bool equalSized, bool rounded, const char *caption,
                                  const char *fallback, Qt::WFlags fl)
{
  int i, row = 0, rowStart = 0;
  QString str;
  QPushButton *button;
  QHBoxLayout *layout2;  
  mEqualSized = equalSized;
  mNumButtons = numButtons;
  mRoundedStyle = rounded;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);

  // Force it to a small size so it will end up at minumum size
  resize(50, 50);

  // Get outer layout then the layout that derived class will build into
  QVBoxLayout *outerLayout = new QVBoxLayout(this);
  outerLayout->setSpacing(6);
  outerLayout->setContentsMargins(8, 8, 8, 8);
  mLayout = new QVBoxLayout();
  outerLayout->addLayout(mLayout);

  // Make the line
  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  outerLayout->addWidget(line);

  // set up signal mappers for the buttons
  QSignalMapper *pressMapper = new QSignalMapper(this);
  connect(pressMapper, SIGNAL(mapped(int)), this, 
          SLOT(actionButtonPressed(int)));
  QSignalMapper *clickMapper = new QSignalMapper(this);
  connect(clickMapper, SIGNAL(mapped(int)), this, 
          SLOT(actionButtonClicked(int)));
  
  // Make the buttons and put in layout(s)
  for (i = 0; i < numButtons; i++) {

    // If time to start a new row, get a new layout and compute the index
    // Of the next row start
    if (i == rowStart) {
      layout2 = new QHBoxLayout();
      layout2->setContentsMargins(0, 0, 0, 0);
      layout2->setSpacing(6);
      outerLayout->addLayout(layout2);
      rowStart += numButtons / numRows;
      if (row < numButtons % numRows)
        rowStart++;
      row++;
    }

    str = labels[i];
    button = new QPushButton(str, this);
    if (i < BUTTON_ARRAY_MAX)
      mButtons[i] = button;
    button->setFocusPolicy(Qt::NoFocus);
    layout2->addWidget(button);
    pressMapper->setMapping(button, i);
    connect(button, SIGNAL(pressed()), pressMapper, SLOT(map()));
    clickMapper->setMapping(button, i);
    connect(button, SIGNAL(clicked()), clickMapper, SLOT(map()));
    if (tips != NULL)
      button->setToolTip(tips[i]);
  }
  dfSetFontDependentWidths();

  setFocusPolicy(Qt::StrongFocus);

  str = caption;
  if (str.isEmpty())
      str = fallback;
  setWindowTitle(str);
}

void DialogFrame::dfSetFontDependentWidths()
{
  int numButtons = mNumButtons < BUTTON_ARRAY_MAX 
    ? mNumButtons : BUTTON_ARRAY_MAX;
  int width = 0;
  int twidth, i;

  // If equalsized buttons, find maximum width
  if (mEqualSized) {
    for (i = 0; i < numButtons; i++) {
      twidth = diaGetButtonWidth(this, mRoundedStyle, 1.25, 
                                 mButtons[i]->text());
      if (width < twidth)
	width = twidth;
    }
  }

  // Set width of each button based on its own width or maximum width
  for (i = 0; i < numButtons; i++) {
    if (!mEqualSized)
      width = diaGetButtonWidth(this, mRoundedStyle, 1.25, 
                                mButtons[i]->text());
    mButtons[i]->setFixedWidth(width);
  }
}

/*!
 * A virtual protected function that maintains the size of the buttons upon
 * font change.  If the inheriting class reimplements this, it should first update
 * the mRoundedStyle variable, then call DialogFrame::changeEvent(e), then set its 
 * font-dependent sizes
 */
void DialogFrame::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    dfSetFontDependentWidths();
}

void DialogFrame::actionButtonPressed(int which)
{
  emit actionPressed(which);
}

void DialogFrame::actionButtonClicked(int which)
{
  emit actionClicked(which);
}
