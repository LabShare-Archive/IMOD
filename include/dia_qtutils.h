/*   dia_qutils.h  -  declarations for dia_qutils.cpp
 *
 *   Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT file for full copyright notice.
 *
 *  $Id$
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
QRadioButton DLL_IM_EX *diaRadioButton(const char *label, QWidget *parent,
                                       QButtonGroup *group, QBoxLayout *layout,
                                       int id, const char *tooltip);
QPushButton DLL_IM_EX *diaPushButton(const char *text, QWidget *parent, 
			   QBoxLayout *layout);
QCheckBox DLL_IM_EX *diaCheckBox(const char *text, QWidget *parent, 
                                 QBoxLayout *layout);
QSlider DLL_IM_EX *diaSlider(int min, int max, int step, int value,
                             QWidget *parent, QBoxLayout *layout);
QAbstractSpinBox DLL_IM_EX *diaLabeledSpin
(int nDecimal, float minValue, float maxValue, float step, const char *text,
 QWidget *parent, QBoxLayout *layout, QLabel **labelPtr = NULL);
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
                                  int numFilters, const char *filters[]);

int DLL_IM_EX dia_err(const char *message);
int DLL_IM_EX dia_puts(const char *message);
int DLL_IM_EX dia_ask(const char *question);
int DLL_IM_EX dia_ask_forever(const char *question);
int DLL_IM_EX dia_choice(const char *question, const char *lab1, 
                         const char *lab2, const char *lab3);
int DLL_IM_EX diaQInput(int *value, int low, int high, int decimal, 
                        const char *prompt);
void DLL_IM_EX dia_vasmsg(const char *msg, ...);
void DLL_IM_EX dia_smsg(const char **msg);

#endif
