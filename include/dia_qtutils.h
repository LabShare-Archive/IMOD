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
//Added by qt3to4:
#include <QLabel>
#include <QPalette>

class QCheckBox;
class QLabel;
class QPushButton;
class QRadioButton;
class QBoxLayout;
class QHBoxLayout;
class QVBoxLayout;
class QWidget;
class QSlider;
class QSpinBox;
class QAbstractSpinBox;
class QAbstractButton;
class QDoubleSpinBox;
class QButtonGroup;
class QString;
class QLineEdit;

extern DLL_IM_EX char *Dia_title;

void DLL_IM_EX diaSetSpinBox(QSpinBox *box, int value);
void DLL_IM_EX diaSetDoubleSpinBox(QDoubleSpinBox *box, double value);
void DLL_IM_EX diaSetSpinMMVal(QSpinBox *box, int min, int max, int value);
void DLL_IM_EX diaSetGroup(QButtonGroup *group, int id);
void DLL_IM_EX diaSetSlider(QSlider *slider, int value);
void DLL_IM_EX diaSetChecked(QAbstractButton *button, bool state);
void DLL_IM_EX diaSetEditText(QLineEdit *edit, const QString &text);
void DLL_IM_EX diaShowWidget(QWidget *widget, bool state);
QLabel DLL_IM_EX *diaLabel(const char *text, QWidget *parent,
                           QBoxLayout *layout);
QRadioButton DLL_IM_EX *diaRadioButton(char *label, QWidget *parent,
                                       QButtonGroup *group, QBoxLayout *layout,
                                       int id, char *tooltip);
QPushButton DLL_IM_EX *diaPushButton(const char *text, QWidget *parent, 
			   QBoxLayout *layout);
QCheckBox DLL_IM_EX *diaCheckBox(const char *text, QWidget *parent, 
                                 QBoxLayout *layout);
QSlider DLL_IM_EX *diaSlider(int min, int max, int step, int value,
                             QWidget *parent, QBoxLayout *layout);
QAbstractSpinBox DLL_IM_EX *diaLabeledSpin
(int nDecimal, float minValue, float maxValue, float step, const char *text,
 QWidget *parent, QBoxLayout *layout);
QVBoxLayout DLL_IM_EX *diaVBoxLayout(QBoxLayout *layout);
QHBoxLayout DLL_IM_EX *diaHBoxLayout(QBoxLayout *layout);
int DLL_IM_EX diaGetButtonWidth(QWidget *widget, bool rounded, float factor, 
                      const QString &text);
int DLL_IM_EX diaSetButtonWidth(QPushButton *button, bool rounded,
                                float factor, const QString &text);
void DLL_IM_EX diaSetWidgetColor(QWidget *widget, QColor color,
                                 QPalette::ColorRole role = QPalette::NoRole);
void DLL_IM_EX diaMaximumWindowSize(int &width, int &height);
void DLL_IM_EX diaLimitWindowSize(int &width, int &height);
void DLL_IM_EX diaLimitWindowPos(int neww, int newh, int &newdx, int &newdy);
void DLL_IM_EX diaSetTitle(const char *title);

// Get a single existing file name with a set of filters
QString DLL_IM_EX diaOpenFileName(QWidget *parent, const char *caption, 
                                  int numFilters, char *filters[]);

int DLL_IM_EX dia_err(const char *message);
int DLL_IM_EX dia_puts(const char *message);
int DLL_IM_EX dia_ask(const char *question);
int DLL_IM_EX dia_ask_forever(const char *question);
int DLL_IM_EX dia_choice(const char *question, const char *lab1, 
                         const char *lab2, const char *lab3);
int DLL_IM_EX diaQInput(int *value, int low, int high, int decimal, 
                        const char *prompt);
void DLL_IM_EX dia_vasmsg(char *msg, ...);
void DLL_IM_EX dia_smsg(char **msg);

#endif

/*

$Log$
Revision 3.9  2008/05/27 05:56:33  mast
Added show/hide function

Revision 3.8  2007/07/08 16:53:51  mast
Added dia_ask_forever

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
