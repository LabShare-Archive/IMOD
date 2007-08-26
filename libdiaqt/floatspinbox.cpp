/*   floatspinbox.cpp  -  A floating point spin box class
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 *  $Id$
 */                                                                           

/*  $Log$
Revision 1.2  2003/12/27 20:37:42  mast
needed to include math.h for floor

Revision 1.1  2003/12/16 23:50:10  mast
Initial creation

*/

#include "floatspinbox.h"
#include <math.h>
#include <qvalidator.h>

/*!
 * A float spin box class: specify number of decimal places in [nDecimal],
 * and [minValue], [maxValue], and [step] in the integer units of the
 * underlying spin box.  [step] defaults to 10, [parent] and [name] to NULL.
 */
FloatSpinBox::FloatSpinBox( int nDecimal, int minValue, int maxValue, 
                            int step, QWidget * parent, const char * name)
  : QSpinBox(minValue, maxValue, step, parent, name)
{
  mScale = 1;
  mDecimal = nDecimal;
  for (int i = 0; i < nDecimal; i++)
    mScale *= 10;
  QDoubleValidator *validator = new QDoubleValidator
    ((double)minValue / mScale, (double)maxValue / mScale, 1, this);
  setValidator(validator);
}

QString FloatSpinBox::mapValueToText( int value )
{
  char format[10];
  QString result;
  sprintf(format, "%%.%df", mDecimal);
  result.sprintf(format, (double)value / mScale);
  return result;
}

int FloatSpinBox::mapTextToValue( bool *ok )
{
  *ok = true;
  return (int) floor(mScale * text().toFloat() + 0.5);
}
