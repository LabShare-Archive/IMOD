/*   dia_qutils.h  -  declarations for dia_qutils.cpp
 *
 *   Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef DIA_QTUTILS_H
#define DIA_QTUTILS_H

#include "dllexport.h"

class QCheckBox;
class QLabel;
class QPushButton;
class QRadioButton;
class QBoxLayout;
class QWidget;
class QSlider;
class QSpinBox;
class FloatSpinBox;
class QButtonGroup;
class QString;
class QLineEdit;

extern DLL_IM_EX char *Dia_title;

void DLL_IM_EX diaSetSpinBox(QSpinBox *box, int value);
void DLL_IM_EX diaSetSpinMMVal(QSpinBox *box, int min, int max, int value);
void DLL_IM_EX diaSetGroup(QButtonGroup *group, int value);
void DLL_IM_EX diaSetSlider(QSlider *slider, int value);
void DLL_IM_EX diaSetChecked(QCheckBox *button, bool state);
void DLL_IM_EX diaSetEditText(QLineEdit *edit, const QString &text);
QLabel DLL_IM_EX *diaLabel(char *text, QWidget *parent, QBoxLayout *layout);
QRadioButton DLL_IM_EX *diaRadioButton(char *text, QWidget *parent);
QPushButton DLL_IM_EX *diaPushButton(char *text, QWidget *parent, 
			   QBoxLayout *layout);
QCheckBox DLL_IM_EX *diaCheckBox(char *text, QWidget *parent, 
                                 QBoxLayout *layout);
QSpinBox DLL_IM_EX *diaLabeledSpin(int nDecimal, int minValue, int maxValue,
                                   int step, char *text, QWidget *parent,
                                   QBoxLayout *layout);
int DLL_IM_EX diaGetButtonWidth(QWidget *widget, bool rounded, float factor, 
                      const QString &text);
int DLL_IM_EX diaSetButtonWidth(QPushButton *button, bool rounded,
                                float factor, const QString &text);
void DLL_IM_EX diaMaximumWindowSize(int &width, int &height);
void DLL_IM_EX diaLimitWindowSize(int &width, int &height);
void DLL_IM_EX diaLimitWindowPos(int neww, int newh, int &newdx, int &newdy);
void DLL_IM_EX diaSetTitle(char *title);

// Get a single existing file name with a set of filters
QString DLL_IM_EX diaOpenFileName(QWidget *parent, char *caption, 
                                  int numFilters, char *filters[]);

int DLL_IM_EX dia_err(char *message);
int DLL_IM_EX dia_puts(char *message);
int DLL_IM_EX dia_ask(char *question);
int DLL_IM_EX dia_ask_forever(char *question);
int DLL_IM_EX dia_choice(char *question, char *lab1, char *lab2, char *lab3);
int DLL_IM_EX diaQInput(int *value, int low, int high, int decimal, 
                        char *prompt);
void DLL_IM_EX dia_vasmsg(char *msg, ...);
void DLL_IM_EX dia_smsg(char **msg);

#endif

/*
$Log$
Revision 3.7  2006/09/05 14:24:23  mast
Added labeled spin box creator

Revision 3.6  2006/03/01 19:13:18  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 3.5  2004/11/21 05:53:11  mast
Added routine to set text with blocked signals

Revision 3.4  2004/11/20 03:29:35  mast
Make calls take Q or H box layout, add call to set spin box all at once

Revision 3.3  2004/11/04 23:31:07  mast
Changes for rounded button style

Revision 3.2  2004/06/04 02:57:28  mast
Implement export/import macro for making libdiaqt be a DLL

Revision 3.1  2003/02/10 20:57:02  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:37:00  mast
includes for library

Revision 1.1.2.5  2003/01/18 01:11:48  mast
add call to make radio button

Revision 1.1.2.4  2003/01/13 01:09:10  mast
added dia_ask, dia_choice, and diaQInput

Revision 1.1.2.3  2003/01/06 15:37:40  mast
new functions for setting spin box and button group

Revision 1.1.2.2  2003/01/01 05:44:56  mast
adding message functions

Revision 1.1.2.1  2002/12/30 06:32:59  mast
Initial creation

*/
