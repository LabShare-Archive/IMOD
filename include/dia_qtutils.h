/*   dia_qutils.h  -  declarations for dia_qutils.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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

#ifndef DIA_QTUTILS_H
#define DIA_QTUTILS_H
class QCheckBox;
class QLabel;
class QPushButton;
class QRadioButton;
class QVBoxLayout;
class QWidget;
class QSlider;
class QSpinBox;
class QButtonGroup;
class QString;

extern char *Dia_title;

void diaSetSpinBox(QSpinBox *box, int value);
void diaSetGroup(QButtonGroup *group, int value);
void diaSetSlider(QSlider *slider, int value);
void diaSetChecked(QCheckBox *button, bool state);
QLabel *diaLabel(char *text, QWidget *parent, QVBoxLayout *layout);
QRadioButton *diaRadioButton(char *text, QWidget *parent);
QPushButton *diaPushButton(char *text, QWidget *parent, 
			   QVBoxLayout *layout);
QCheckBox *diaCheckBox(char *text, QWidget *parent, QVBoxLayout *layout);
void diaSetTitle(char *title);

// Get a single existing file name with a set of filters
QString diaOpenFileName(QWidget *parent, char *caption, int numFilters, 
                        char *filters[]);

int dia_err(char *message);
int dia_puts(char *message);
int dia_ask(char *question);
int dia_choice(char *question, char *lab1, char *lab2, char *lab3);
int diaQInput(int *value, int low, int high, int decimal, char *prompt);
void dia_vasmsg(char *msg, ...);
void dia_smsg(char **msg);

#endif
