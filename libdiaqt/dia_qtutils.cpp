/*  IMOD VERSION 2.7.9
 *
 *  dia_qutils.cpp       Utility calls for using Qt classes
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
$Log$
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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qslider.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qlayout.h>
#include <qapplication.h>
#include <qhbox.h>
#include <qspinbox.h>
#include <qdialog.h>
#include <qfiledialog.h>
#include <qinputdialog.h>
#include <qmessagebox.h>
#include <qtextedit.h>
#include <qbuttongroup.h>

#include "dia_qtutils.h"

char *Dia_title = NULL;

// Make a new push button, add it to the vertical box layout, set for no focus
QPushButton *diaPushButton(char *text, QWidget *parent, 
                                  QBoxLayout *layout)
{
  QPushButton *button = new QPushButton(text, parent);
  button->setFocusPolicy(QWidget::NoFocus);
  layout->addWidget(button);
  return button;
}

// Make a new check box, add it to the vertical box layout, set for no focus
QCheckBox *diaCheckBox(char *text, QWidget *parent, QBoxLayout *layout)
{
  QCheckBox *button = new QCheckBox(text, parent);
  button->setFocusPolicy(QWidget::NoFocus);
  layout->addWidget(button);
  return button;
}

// Make a new radio button, set for no focus
QRadioButton *diaRadioButton(char *text, QWidget *parent)
{
  QRadioButton *button = new QRadioButton(text, parent);
  button->setFocusPolicy(QWidget::NoFocus);
  return button;
}

// Make a new label and add it to the vertical box layout
QLabel *diaLabel(char *text, QWidget *parent, QBoxLayout *layout)
{
  QLabel *label = new QLabel(text, parent);
  layout->addWidget(label);
  return label;
}

// Set a checkbox and block the signals
void diaSetChecked(QCheckBox *button, bool state)
{
  button->blockSignals(true);
  button->setChecked(state);
  button->blockSignals(false);
}

// Set a slider and block signals
void diaSetSlider(QSlider *slider, int value)
{
  slider->blockSignals(true);
  slider->setValue(value);
  slider->blockSignals(false);
}

// Set a spin box and block signals
void diaSetSpinBox(QSpinBox *box, int value)
{
  box->blockSignals(true);
  box->setValue(value);
  box->blockSignals(false);
}

// Set a spin box including min and max and block signals
void diaSetSpinMMVal(QSpinBox *box, int min, int max, int value)
{
  box->blockSignals(true);
  box->setMinValue(min);
  box->setMaxValue(max);
  box->setValue(value);
  box->blockSignals(false);
}

// Set a button group and block signals
void diaSetGroup(QButtonGroup *group, int value)
{
  group->blockSignals(true);
  group->setButton(value);
  group->blockSignals(false);
}

// Set a text edit and block signals
void diaSetEditText(QLineEdit *edit, const QString &text)
{
  edit->blockSignals(true);
  edit->setText(text);
  edit->blockSignals(false);
}

// Determine a button width appropriate for the given text, multiplying by
// the given factor and adding height if rounded is true
int diaGetButtonWidth(QWidget *widget, bool rounded, float factor, 
                      const QString &text)
{
  int width = (int)(factor * widget->fontMetrics().width(text) + 0.5);
  if (rounded)
    width += (int)(1.5 * widget->fontMetrics().height());
  return width;
}

// Set the fixed width of a button, returning the width for reuse
int diaSetButtonWidth(QPushButton *button, bool rounded,
                                float factor, const QString &text)
{
  int width = diaGetButtonWidth(button, rounded, factor, text);
  button->setFixedWidth(width);
  return width;
}


// Set a title into Dia_title
void diaSetTitle(char *title)
{
  if (Dia_title)
    free(Dia_title);
  Dia_title = strdup(title);
}

// An application-model box with an information string
int dia_puts(char *message)
{
  QString str = message;
  QString title = Dia_title;
  title += " Message";
  QMessageBox::information(0, title, str, 
			   QMessageBox::Ok, QMessageBox::NoButton,
			   QMessageBox::NoButton);
  return 0;
}

// An application-model box with an error string
int dia_err(char *message)
{
  QString str = message;
  QString title = Dia_title;
  title += " Error";
  QMessageBox::warning(0, title, str, 
			   QMessageBox::Ok, QMessageBox::NoButton,
			   QMessageBox::NoButton);
  return 0;
}

// An application modal box to ask a yes-no question
int dia_ask(char *question)
{
  QString str = question;
  QString title = Dia_title;
  title += " Query";
  int retval =   QMessageBox::information(0, title, str, 
			   QMessageBox::Yes, QMessageBox::No,
			   QMessageBox::NoButton);
  return retval == QMessageBox::Yes ? 1 : 0;
}

// An application modal box to give up to three arbitrary responses
int dia_choice(char *question, char *lab1, char *lab2, char *lab3)
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

// A function to use QInputDialogs to get an integer or float
// It returns 0 if the user cancels and leave value unchanged
int diaQInput(int *value, int low, int high, int decimal, char *prompt)
{
  bool ok = false;
  QString str = prompt;
  QString title = Dia_title;
  int result, i;
  double from, to, dresult, dvalue, factor;
  

  if (!decimal) {
    result = QInputDialog::getInteger(title, str, *value, low, high, 1, &ok);
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
    dresult = QInputDialog::getDouble(title, str, dvalue, from, to, decimal,
				      &ok);
    if (ok)
      *value = (int)floor(dresult * factor + 0.5);
  }

  return ok ? 1 : 0;
}

// Get a single existing file name with a set of filters; the first will
// be the default filter
QString diaOpenFileName(QWidget *parent, char *caption, int numFilters,
                        char *filters[])
{
  QString qname = Dia_title;
  QFileDialog fileDialog(parent, NULL, true);
  fileDialog.setMode(QFileDialog::ExistingFile);
  fileDialog.setCaption(qname + ": " + caption);
  for (int i = numFilters - 1; i >= 0; i--)
    fileDialog.addFilter(QString(filters[i]));
  qname = "";
  if (fileDialog.exec() == QDialog::Accepted)
    qname = fileDialog.selectedFile();
  return qname;
}

// Get a scrolled message window from a variable set of character strings
// Turn it into an array of strings
void dia_vasmsg(char *msg, ...)
{
  char **argv;
  char *emsg;
  char *tmsg;
  int argc = 0;
  va_list ap;

  tmsg = msg;
  va_start(ap, msg);
  while( emsg = va_arg(ap, char *)){
    argc++;
  }
  va_end(ap);

  argv = (char **)malloc((argc + 2) * sizeof(char *));

  argc = 1;
  va_start(ap, msg);
  argv[0] = tmsg;
  while( emsg = va_arg(ap, char *)){
    argv[argc] = emsg;
    argc++;
  }
  argv[argc] = NULL;
  va_end(ap);
  dia_smsg(argv);
  free(argv);
}

// Get a scrolled message window from an array of character strings
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

  QDialog *dlg = new QDialog(0, 0, false, 
                             Qt::WDestructiveClose);

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
  QHBox *hbox = new QHBox(dlg);
  vbox->addWidget(hbox);
  QPushButton *button = new QPushButton("Close", hbox);
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
  dlg->setCaption(test);
  dlg->show();
}
