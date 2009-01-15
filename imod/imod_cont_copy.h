/*   imod_cont_copy.h  -  declarations for imod_cont_copy.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMOD_CONT_COPY_H
#define IMOD_CONT_COPY_H

#include "dialog_frame.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
class QButtonGroup;
class QSpinBox;
class QComboBox;
class QRadioButton;

typedef struct ViewInfo ImodView;

class ContourCopy : public DialogFrame
{
  Q_OBJECT

 public:
  ContourCopy(QWidget *parent, const char *name = NULL);
  ~ContourCopy() {};
  void apply();

  public slots:
  void buttonPressed(int which);
  void placeSelected(int which);
  void toValueChanged(int value);
  void rangeSelected(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  void update();
  QComboBox *mToCombo;
  QButtonGroup *mRadioGroup;
  QSpinBox *mToSpinBox;
  QRadioButton *mTimeRadio;
};

int openContourCopyDialog(ImodView *vw);
void iccCopyContour(void);

#endif
/*  

$Log$
Revision 4.3  2008/12/10 01:05:15  mast
Added hot key for contour copy

Revision 4.2  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 19:59:28  mast
Initial creation

*/
