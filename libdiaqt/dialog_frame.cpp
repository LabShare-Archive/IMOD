/*  IMOD VERSION 2.7.9
 *
 *  dialog_frame.cpp       Implementation of a base class for non-modal dialogs
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional         *
 *   Electron Microscopy of Cells ("BL3DEMC") and the Regents of the         *
 *   University of Colorado.                                                 *
 *                                                                           *
 *   BL3DEMC reserves the exclusive rights of preparing derivative works,    *
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
 *   for the Boulder Laboratory for 3-Dimensional EM of Cells.               *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

/* DialogFrame provides a widget whose default style is to be a dialog box that
   destroys itself on closing.  Its main area is a QVBoxLayout, mLayout,
   that can be populated with widgets by the inheriting class.  The bottom
   row(s) will have numButton buttons, with text given in "labels".  The
   buttons will be equally sized if equalSized is true; otherwise they will
   all be just big enough for their respective text.  The window title will
   be set to "caption", or to "fallback" if "caption" is null.
   The alternate construction with "numRows" and "rounded" allows the buttons 
   to be on more than one row and allows specification of whether the style
   has rounded buttons.
 */

#include <qlayout.h>
#include <qframe.h>
#include <qtooltip.h>
#include <qpushbutton.h>
#include <qsignalmapper.h>
#include "dialog_frame.h"
#include "dia_qtutils.h"

DialogFrame::DialogFrame(QWidget *parent, int numButtons, char *labels[], 
			 char *tips[],
			 bool equalSized, char *caption, char *fallback,
			 const char *name, WFlags fl)
  : QWidget(parent, name, fl)
{
  makeDialogFrame(parent, numButtons, 1, labels, tips, equalSized, false,
                  caption, fallback, name, fl);
}

DialogFrame::DialogFrame(QWidget *parent, int numButtons, int numRows,
                         char *labels[], char *tips[], bool equalSized,
                         bool rounded, char *caption, char *fallback,
			 const char *name, WFlags fl)
  : QWidget(parent, name, fl)
{
  makeDialogFrame(parent, numButtons, numRows, labels, tips, equalSized, 
                  rounded, caption, fallback, name, fl);
}

void DialogFrame::makeDialogFrame(QWidget *parent, int numButtons, int numRows,
                                  char *labels[], char *tips[],
                                  bool equalSized, bool rounded, char *caption,
                                  char *fallback, const char *name, WFlags fl)
{
  int width = 0;
  int i, twidth, row = 0, rowStart = 0;
  QString str;
  QPushButton *button;
  QHBoxLayout *layout2;  
  mEqualSized = equalSized;
  mNumButtons = numButtons;
  mRoundedStyle = rounded;

  // Force it to a small size so it will end up at minumum size
  resize(50, 50);

  // Get outer layout then the layout that derived class will build into
  QVBoxLayout *outerLayout = new QVBoxLayout(this, 8, 6, "outer layout");
  mLayout = new QVBoxLayout(outerLayout);

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
      layout2 = new QHBoxLayout(0, 0, 6);
      outerLayout->addLayout(layout2);
      rowStart += numButtons / numRows;
      if (row < numButtons % numRows)
        rowStart++;
      row++;
    }

    str = labels[i];
    button = new QPushButton(str, this, labels[i]);
    if (i < BUTTON_ARRAY_MAX)
      mButtons[i] = button;
    button->setFocusPolicy(NoFocus);
    layout2->addWidget(button);
    pressMapper->setMapping(button, i);
    connect(button, SIGNAL(pressed()), pressMapper, SLOT(map()));
    clickMapper->setMapping(button, i);
    connect(button, SIGNAL(clicked()), clickMapper, SLOT(map()));
    if (tips != NULL)
      QToolTip::add(button, tips[i]);
  }
  setFontDependentWidths();

  setFocusPolicy(StrongFocus);

  str = caption;
  if (str.isEmpty())
      str = fallback;
  setCaption(str);
}

void DialogFrame::setFontDependentWidths()
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

void DialogFrame::fontChange(const QFont &oldFont)
{
  setFontDependentWidths();
  QWidget::fontChange(oldFont);
}

void DialogFrame::actionButtonPressed(int which)
{
  emit actionPressed(which);
}

void DialogFrame::actionButtonClicked(int which)
{
  emit actionClicked(which);
}

/*
$Log$
Revision 1.5  2004/06/23 03:35:41  mast
Added ability to put buttons on multiple lines

Revision 1.4  2004/01/22 19:06:18  mast
Changed actionPressed to actionClicked and added real actionPressed

Revision 1.3  2003/03/24 17:41:47  mast
Set up to resize buttons on font change

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:28  mast
adding as library file

Revision 1.1.2.4  2003/01/23 19:55:42  mast
switch from button pressed to clicked

Revision 1.1.2.3  2003/01/18 01:08:09  mast
add tooltips

Revision 1.1.2.2  2002/12/30 06:37:46  mast
set the size small so it will show up at minimum size

Revision 1.1.2.1  2002/12/29 04:20:38  mast
Initial creation

*/
