/*   floatspinbox.h  -  declarations for floatspinbox.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
*/

#ifndef FLOATSPINBOX_H
#define FLOATSPINBOX_H
#include <qspinbox.h>

// A floating spin button class
class FloatSpinBox : public QSpinBox
{
  Q_OBJECT
    public:
  FloatSpinBox( int nDecimal, int minValue, int maxValue, int step = 10, 
		QWidget * parent = 0, const char * name = 0);
  ~FloatSpinBox() {};

  QString mapValueToText( int value );
  int mapTextToValue( bool *ok );

 private:
  int mScale;
  int mDecimal;
};
  
#endif
