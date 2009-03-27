/*  
 *  dia_qutils.cpp       Utility calls for using Qt classes
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qslider.h>
#include <qabstractbutton.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qlayout.h>
#include <qapplication.h>
#include <qspinbox.h>
#include <qabstractspinbox.h>
#include <QDoubleSpinBox>
#include <qdialog.h>
#include <qfiledialog.h>
#include <qinputdialog.h>
#include <qmessagebox.h>
#include <qtextedit.h>
#include <qbuttongroup.h>
//Added by qt3to4:
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QDesktopWidget>

#include "dia_qtutils.h"
#include "b3dutil.h"

char *Dia_title = NULL;

/*! Makes a new push button with the given [text], adds it to [layout] if it
  is non-NULL, and sets it for no focus.  [layout] can be a QHBoxLayout or a
  QVBoxLayout. */
QPushButton *diaPushButton(const char *text, QWidget *parent, 
                           QBoxLayout *layout)
{
  QPushButton *button = new QPushButton(text, parent);
  button->setFocusPolicy(Qt::NoFocus);
  if (layout)
    layout->addWidget(button);
  return button;
}

/*! Makes a new check box with the given [text], adds it to [layout] if it is 
  non-NULL, and sets it for no focus.  [layout] can be a QHBoxLayout or a
  QVBoxLayout. */
QCheckBox *diaCheckBox(const char *text, QWidget *parent, QBoxLayout *layout)
{
  QCheckBox *button = new QCheckBox(text, parent);
  button->setFocusPolicy(Qt::NoFocus);
  if (layout)
    layout->addWidget(button);
  return button;
}

/*!
 * Makes a new radio button with the give [text] and with the given [parent]
 * (which can be NULL), adds it to the button group [group] with button ID [id]
 * if [group] is non-NULL, adds it to [layout] if it is non-NULL, sets it for
 * no focus, and sets [tooltip] as the tooltip if it is non-NULL.  [layout] 
 * can be a QHBoxLayout or a QVBoxLayout.
 */
QRadioButton *diaRadioButton(char *label, QWidget *parent, QButtonGroup *group,
                             QBoxLayout *layout, int id, char *tooltip)
{
  QRadioButton *radio = new QRadioButton(QString(label), parent);
  if (group)
    group->addButton(radio, id);
  if (layout)
    layout->addWidget(radio);
  radio->setFocusPolicy(Qt::NoFocus);
  if (tooltip)
    radio->setToolTip(QString(tooltip));
  return radio;
}

/*! Makes a new label with the given [text] and adds it to [layout] if it
 is non-NULL. [layout] can be a QHBoxLayout or a QVBoxLayout.*/
QLabel *diaLabel(const char *text, QWidget *parent, QBoxLayout *layout)
{
  QLabel *label = new QLabel(text, parent);
  if (layout)
    layout->addWidget(label);
  return label;
}

/*! 
 * Makes a labeled spin box, with the label given by [text] to the left of the
 * box and right aligned to it, provided that [layout] is a horizontal layout 
 * box in which to place them.  (In a toolbar, [layout] can be NULL.)  
 * [minValue], [maxValue], and [step] are the  minimum, maximum, and step 
 * sizes for the spin box.  If [nDecimal] is non-zero, it creates and returns 
 * a QDoubleSpinBox with that number of decimal places.  It skips the label
 * if [text] is NULL.  The focus policy is set to ClickFocus.  Keyboard 
 * tracking is turned off.
 */
QAbstractSpinBox *diaLabeledSpin(int nDecimal, float minValue, float maxValue,
                                 float step, const char *text, QWidget *parent,
                                 QBoxLayout *layout)
{
  QSpinBox *spin;
  QDoubleSpinBox *fspin;
  if (text) {
    QLabel *label = diaLabel(text, parent, layout);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  }
  if (nDecimal) {
    fspin = new QDoubleSpinBox(parent);
    fspin->setDecimals(nDecimal);
    fspin->setRange((double)minValue, (double)maxValue);
    fspin->setSingleStep((double)step);
    spin = (QSpinBox *)fspin;
  } else {
    spin = new QSpinBox(parent);
    spin->setRange(B3DNINT(minValue), B3DNINT(maxValue));
    spin->setSingleStep(B3DNINT(step));
  }
  if (layout)
    layout->addWidget(spin);
  spin->setFocusPolicy(Qt::ClickFocus);
  spin->setKeyboardTracking(false);
  return (QAbstractSpinBox *)spin;
}

/*! Creates a QVBoxLayout and adds it to [layout], replacing the very useful
  Qt 3 constructor. */
QVBoxLayout *diaVBoxLayout(QBoxLayout *layout)
{
  QVBoxLayout *lay = new QVBoxLayout();
  layout->addLayout(lay);
  return lay;
}

/*! Creates a QHBoxLayout and adds it to [layout] */
QHBoxLayout *diaHBoxLayout(QBoxLayout *layout)
{
  QHBoxLayout *lay = new QHBoxLayout();
  layout->addLayout(lay);
  return lay;
}

/*! 
 * Creates and returns a new horizontal slider with range from [min] to [max],
 * value [value] and single step size [step], adds it to [layout] if it is not
 * NULL, and sets it for no focus.  The page step and single step are both set
 * to 1.
 */
QSlider *diaSlider(int min, int max, int step, int value,
                   QWidget *parent, QBoxLayout *layout)
{
  QSlider *slider = new QSlider(parent);
  if (layout)
    layout->addWidget(slider);
  slider->setOrientation(Qt::Horizontal);
  slider->setRange(min, max);
  slider->setSingleStep(step);
  slider->setPageStep(step);
  slider->setValue(value);
  slider->setFocusPolicy(Qt::NoFocus);
  return slider;
}

/*! Sets a checkbox or checkable toolbutton [button] to [state] with signals 
  blocked */
void diaSetChecked(QAbstractButton *button, bool state)
{
  button->blockSignals(true);
  button->setChecked(state);
  button->blockSignals(false);
}

/*! Sets [slider] to [value] with signals blocked */
void diaSetSlider(QSlider *slider, int value)
{
  slider->blockSignals(true);
  slider->setValue(value);
  slider->blockSignals(false);
}

/*! Sets a spin box [box] to [value] with signals blocked */
void diaSetSpinBox(QSpinBox *box, int value)
{
  box->blockSignals(true);
  box->setValue(value);
  box->blockSignals(false);
}

/*! Sets a double spin box [box] to [value] with signals blocked */
void diaSetDoubleSpinBox(QDoubleSpinBox *box, double value)
{
  box->blockSignals(true);
  box->setValue(value);
  box->blockSignals(false);
}

/*! Sets a spin box [box] to [value] and sets its minimum and maximum to [min]
 * and [max], with signals blocked */
void diaSetSpinMMVal(QSpinBox *box, int min, int max, int value)
{
  box->blockSignals(true);
  box->setRange(min, max);
  box->setValue(value);
  box->blockSignals(false);
}

/*! Turns on button with ID [id] in button group [group], with signals blocked.
 * The ID must be explicitly assigned such as when adding the button to the 
 * group. */
void diaSetGroup(QButtonGroup *group, int id)
{
  QAbstractButton *button = group->button(id);
  if (!button)
    return;
  button->blockSignals(true);
  button->setChecked(true);
  button->blockSignals(false);
}

/*! Sets a text edit [edit] to the given [tex] with signals blocked */
void diaSetEditText(QLineEdit *edit, const QString &text)
{
  edit->blockSignals(true);
  edit->setText(text);
  edit->blockSignals(false);
}

/*! Shows or hides [widget] depending on value of [state]. */
void diaShowWidget(QWidget *widget, bool state)
{
  if (state)
    widget->show();
  else
    widget->hide();
}

/*!
 * Determines a button width appropriate for the given [text], multiplying by
 * [factor] and adding height if [rounded] is true.
 */
int diaGetButtonWidth(QWidget *widget, bool rounded, float factor, 
                      const QString &text)
{
  int width = (int)(factor * widget->fontMetrics().width(text) + 0.5);
  if (rounded)
    width += (int)(1.5 * widget->fontMetrics().height());
  return width;
}

/*!
 * Sets [button] to a fixed width of appropriate for the given [text], 
 * multiplying by [factor] and adding height if [rounded] is true.  The width 
 * is returned to use in setting other buttons to the same width.
 */
int diaSetButtonWidth(QPushButton *button, bool rounded,
                                float factor, const QString &text)
{
  int width = diaGetButtonWidth(button, rounded, factor, text);
  button->setFixedWidth(width);
  return width;
}

/* !
 * Sets the background or other color of [widget] to [color].  The role of the
 * color in the palette is specified by [role], which has a default value of
 * QPalette::NoRole, in which case {widget->backgroundRole()} will be used for
 * the role.  In order for background color changes to work, it is necessary to
 * call {widget->setAutoFillBackground(true)} when the widget is created.
 */
void diaSetWidgetColor(QWidget *widget, QColor color, QPalette::ColorRole role)
{
  QPalette palette = widget->palette();
  if (role == QPalette::NoRole)
    role = widget->backgroundRole();
  palette.setColor(role, color);
  widget->setPalette(palette);
}

// Some routines for controlling window size and keeping the window on the
// screen.  The BORDERS are the total borders outside the window 
// excluding frame.  The TITLE_SPACE is the amount to allow for title bar
#define X_BORDERS 24
#define Y_BORDERS 60
#define TITLE_SPACE 28

/*! Gets the maximum window size in [width] and [height], i.e., the desktop 
  size minus assumed borders */
void diaMaximumWindowSize(int &width, int &height)
{
  width = QApplication::desktop()->width() - X_BORDERS;
  height = QApplication::desktop()->height() - Y_BORDERS;
}

/*! Limits [width] and [height] to the maximum window size */
void diaLimitWindowSize(int &width, int &height)
{
  int limh, limw;
  diaMaximumWindowSize(limw, limh);
  if (width > limw)
    width = limw;
  if (height > limh)
    height = limh;
}

/*!
 * Limits the position of a window with width [neww], height [newh], and 
 * desired position [newdx], [newdy], adjusting [newdx] and [newdy] so that
 * the window should be fullyon the screen.  This is done differently for
 * X11, Mac, and Windows.
 */
void diaLimitWindowPos(int neww, int newh, int &newdx, int &newdy)
{
  int limw = QApplication::desktop()->width();
  int limh = QApplication::desktop()->height();

  // X11: spread margin equally top and bottom
  // Windows: put extra at bottom for task bar
  // Mac: put extra at top for menu
  int mintop = (Y_BORDERS - TITLE_SPACE) / 2;
#ifdef _WIN32
  mintop = 0;
#endif
#ifdef Q_OS_MACX
  mintop = Y_BORDERS - TITLE_SPACE - 10;
#endif
#ifdef SGI_GEOMETRY_HACK
  mintop = (Y_BORDERS + TITLE_SPACE) / 2;
#endif

  if (newdx < X_BORDERS / 2)
    newdx = X_BORDERS / 2;
  if (newdx + neww > limw - X_BORDERS / 2)
    newdx = limw - X_BORDERS / 2 - neww;

  if (newdy < mintop)
    newdy = mintop;
  if (newdy + newh > limh - (Y_BORDERS - mintop))
    newdy = limh - (Y_BORDERS - mintop) - newh;
}



/*! Sets the application title into the static variable {Dia_title} */
void diaSetTitle(const char *title)
{
  if (Dia_title)
    free(Dia_title);
  Dia_title = strdup(title);
}

/*! Puts up an application-model message box with the information string in
  [message] and an OK button */
int dia_puts(const char *message)
{
  QString str = message;
  QString title = Dia_title;
  title += " Message";
  QMessageBox::information(0, title, str, 
			   QMessageBox::Ok, QMessageBox::NoButton,
			   QMessageBox::NoButton);
  return 0;
}

/*! Puts up an application-modal message box with an error string in [message]
  [message] and an OK button */
int dia_err(const char *message)
{
  QString str = message;
  QString title = Dia_title;
  title += " Error";
  QMessageBox::warning(0, title, str, 
			   QMessageBox::Ok, QMessageBox::NoButton,
			   QMessageBox::NoButton);
  return 0;
}

/*! Puts up an application-modal message box with the text in [question] and
  Yes and No buttons.  Returns 0 for no, 1 for yes. */
int dia_ask(const char *question)
{
  QString str = question;
  QString title = Dia_title;
  title += " Query";
  int retval =   QMessageBox::information(0, title, str, 
			   QMessageBox::Yes, QMessageBox::No,
			   QMessageBox::NoButton);
  return retval == QMessageBox::Yes ? 1 : 0;
}

/*!
 * Puts up an application-modal message box with the text in [question] and
 * Yes, Yes Always, and No buttons.  Returns 0 for No, 1 for Yes, 2 for Yes 
 * Always.
 */
int dia_ask_forever(const char *question)
{
  QString str = question;
  QString title = Dia_title;
  title += " Query";
  str += "\n\nPress Yes Always to stop seeing this question.";
  int retval = QMessageBox::information(0, title, str, QString("Yes"),
                                        QString("Yes Always"), QString("No"),
                                        0, 2);
  return ((retval + 1) % 3);
}

/*!
 * Puts up an application-modal message box with the text in [question] and
 * with up to three buttons, whose labels are in [lab1], [lab2], and [lab3].
 * Supply a NULL to omit a button.  Returns the number of the button pressed,
 * numbered from 1.
 */
int dia_choice(const char *question, const char *lab1, const char *lab2,
               const char *lab3)
{
  QString str = question;
  QString title = Dia_title;
  QString but1 = lab1;
  QString but2, but3;
  if (lab2)
    but2 = lab2;
  if (lab3)
    but3 = lab3;
  title += " Query";
  return  QMessageBox::information(0, title, str, but1, but2, but3) + 1;
}

/*!
 * Uses QInputDialog to get an integer or float value from the user.  The text
 * should be in [prompt]; [value] provides a default or initial value and the
 * new value is returned into this variable unless the user cancels.  The
 * If [decimal] is 0 it sets up a spin box with [low] and [high] as its limits;
 * otherwise it gets a float with the given number of decimal places, with 
 * [low] and [high] specifying scaled lower an upper limits. 
 * Returns 0 if the user cancels.
 */
int diaQInput(int *value, int low, int high, int decimal, const char *prompt)
{
  bool ok = false;
  QString str = prompt;
  QString title = Dia_title;
  int result, i;
  double from, to, dresult, dvalue, factor;
  

  if (!decimal) {
    result = QInputDialog::getInteger
      (NULL, title, str, *value, low, high, 1, &ok);
    if (ok)
      *value = result;
    return ok ? 1 : 0;
  } else {

    factor = 1.;
    for (i = 0; i < decimal; i++)
      factor *= 10.;
    from = low / factor;
    to = high / factor;
    dvalue = *value / factor;
    dresult = QInputDialog::getDouble
      (NULL, title, str, dvalue, from, to, decimal, &ok);
    if (ok)
      *value = (int)floor(dresult * factor + 0.5);
  }

  return ok ? 1 : 0;
}

/*!
 * Gets the name of a single existing file with a file chooser that will show
 * [caption] in its title bar.  A set of [numFilters] filters can be given in
 * [filters]; the first will be the default filter.  Returns an empty string
 * if the user cancels.
 */
QString diaOpenFileName(QWidget *parent, const char *caption, int numFilters,
                        char *filters[])
{
  QString qname = QString(Dia_title) + QString(": ") + QString(caption);
  QString filter;
  for (int i = 0; i < numFilters; i++)
    filter += QString(filters[i]) + QString(";;");
  filter += QString("All Files (*)");

  // Qt 4.4 on Mac required explicit directory entry or it went to last dir
  qname = QFileDialog::getOpenFileName(parent, qname, QDir::currentPath(), 
                                       filter);
  return qname;
}

/*! Makes a scrolled text window with the text taken a set of character strings
  passed as variable arguments */
void dia_vasmsg(char *msg, ...)
{
  // Turn it into an array of strings
  char **argv;
  char *emsg;
  char *tmsg;
  int argc = 0;
  va_list ap;

  tmsg = msg;
  va_start(ap, msg);
  while ((emsg = va_arg(ap, char *))) {
    argc++;
  }
  va_end(ap);

  argv = (char **)malloc((argc + 2) * sizeof(char *));

  argc = 1;
  va_start(ap, msg);
  argv[0] = tmsg;
  while ((emsg = va_arg(ap, char *))) {
    argv[argc] = emsg;
    argc++;
  }
  argv[argc] = NULL;
  va_end(ap);
  dia_smsg(argv);
  free(argv);
}

/*! Makes a scrolled text window with the text taken from the array of 
  character strings in [msg] */
void dia_smsg( char **msg)
{
  char *p;
  char *buf;
  char *lineStart;
  char *temp;
  int maxline, maxrow, linesize;
  long bufsize;
  int i, twidth, doline;
  int lastspace, curpos;
  int maxWidth = (int)(0.8 * QApplication::desktop()->width());
  int maxHeight = (int)(0.8 * QApplication::desktop()->height());
  int height, width = 0;
  QString test;

  QDialog *dlg = new QDialog();
  dlg->setAttribute(Qt::WA_DeleteOnClose);

  for (i = 0, bufsize = 0; msg[i]; i++){
    linesize = strlen(msg[i]);
    bufsize += linesize;
  }

  buf = (char *)malloc(bufsize + i + 1);
  p = buf;
  for (p = buf, i = 0; msg[i]; i++) {
    p += strlen (strcpy (p, msg[i]));
    /* DNM: this macro call caused program built on Irix 6.5 to not run
       on earlier Irix's.  Casting as (int) didn't help - just do 
       explicit tests */
    /*if (!isspace (p[-1]))  spaces, tabs and newlines are spaces.. */
    if (p[-1] != ' ' && p[-1] != '\t' && p[-1] != '\n')
      *p++ = ' '; /* lines are concatenated, insert a space */
  }
  *--p = 0; /* get rid of trailing space... */

  // DNM: count the actual lines and their lengths to get the right size window

  maxline = 0;
  maxrow = 1;
  curpos = 0;
  lastspace = 40;
  lineStart = buf;

  for (p = buf; *p; p++) {
    doline = 0;
    if (*p == '\t')
      curpos = 8 * (curpos/ 8 + 1);
    else if (*p == ' ') {
      lastspace = curpos;
      curpos++;
    } else if (*p == '\n') {
      if (curpos >= maxline)
        maxline = curpos + 1;
      curpos = 0;
      doline = p + 1 - lineStart;
    } else if (curpos > 78 ) {
      if (lastspace >= maxline)
        maxline = lastspace + 1;
      curpos -= lastspace;
      doline = lastspace;
    } else
      curpos++;
    
    if (doline) {
      temp = (char *)malloc(doline + 1);
      if (temp) {
	strncpy(temp, lineStart, doline);
	temp[doline] = 0x00;
	test = temp;
	twidth = dlg->fontMetrics().width(test);
	if (width < twidth)
	  width = twidth;
	free(temp);
      }
      lineStart = p + 1;
      lastspace = 40;
      maxrow++;
    }
  }

  if (!maxline & !width) {
    maxline = curpos + 1;
    test = "";
    for (i = 0; i < maxline + 2; i++)
      test += "8";
    width = dlg->fontMetrics().width(test);
  }

  if (maxrow > 50)
    maxrow = 40;

  QString qmsg = buf;

  // Make a vertical layout with the text edit and a close button
  QVBoxLayout *vbox = new QVBoxLayout(dlg);
  QTextEdit *edit = new QTextEdit(dlg);
  edit->setText(qmsg);
  edit->setReadOnly(true);
  vbox->addWidget(edit);
  QHBoxLayout *hbox = diaHBoxLayout(vbox);
  QPushButton *button = diaPushButton("Close", dlg, hbox);
  diaSetButtonWidth(button, true, 1.4, "Close");
  QObject::connect(button, SIGNAL(clicked()), dlg, SLOT(close()));

  // Figure out width and height of text and height of button, and set size
  if (width > maxWidth)
    width = maxWidth;
  height = (maxrow + 5) * edit->fontMetrics().height();
  if (height > maxHeight)
    height = maxHeight;
  QSize hint = hbox->sizeHint();

  // This was width + 20 when the width was based on character count alone
  dlg->resize(width + 60, height + hint.height());

  // Set title
  test = Dia_title;
  test += " Help";
  dlg->setWindowTitle(test);
  dlg->show();
}

/*
$Log$
Revision 1.14  2009/01/26 04:39:25  mast
Set page step of slider to 1: that is the left click step

Revision 1.13  2009/01/15 16:30:26  mast
Qt 4 port

Revision 1.12  2008/05/25 05:35:52  mast
Added function to show/hide a widget

Revision 1.11  2008/01/13 22:22:25  mast
Made layout optional in diaWidget functions so they can be used in toolbars

Revision 1.10  2007/07/08 16:54:32  mast
Added dia_ask_forever, documented

Revision 1.9  2006/09/05 14:24:40  mast
Added labeled spin box creator

Revision 1.8  2006/03/01 19:13:23  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 1.7  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 1.6  2004/11/21 05:53:03  mast
Added routine to set text with blocked signals

Revision 1.5  2004/11/20 05:07:23  mast
Add spin box min/max/val function, allow H or V layouts in dia functions

Revision 1.4  2004/11/04 23:32:44  mast
Changes for rounded button style

Revision 1.3  2003/11/01 18:14:29  mast
Allow repeated setting of title without leaking

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:36  mast
adding as library file

Revision 1.1.2.9  2003/01/18 01:11:46  mast
add call to make radio button

Revision 1.1.2.8  2003/01/13 01:08:43  mast
Implemented dia_ask, dia_choice, and a replacement for dia_int

Revision 1.1.2.7  2003/01/06 19:01:47  mast
adding log line

*/
