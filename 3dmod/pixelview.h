//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>
#include <QCloseEvent>
/*   pixelview.h  -  declarations for pixelview.cpp and PixelView class
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef PIXELVIEW_H
#define PIXELVIEW_H

int  open_pixelview(struct ViewInfo *vi);
void pvNewMousePosition(struct ViewInfo *vi, float x, float y, int iz);

#define PV_ROWS 7
#define PV_COLS 7

#include <qwidget.h>
class QLabel;
class QPushButton;
class QCheckBox;

class PixelView: public QWidget
{
  Q_OBJECT

 public:
  PixelView(QWidget *parent, const char *name = 0, 
	      Qt::WFlags fl = Qt::Window);
  ~PixelView() {};
  void update();
  void setButtonWidths();
  QLabel *mMouseLabel;
  QCheckBox *mFileValBox;

  public slots:
    void buttonPressed(int pos);
  void fromFileToggled(bool state);
  void showButsToggled(bool state);
  void gridFileToggled(bool state);
  void convertToggled(bool state);
  void helpClicked(void);
  void adjustDialogSize();

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void changeEvent(QEvent *e);

 private:
    QLabel *mBotLabels[PV_COLS];
    QLabel *mLeftLabels[PV_ROWS];
    QLabel *mLabXY;
    QPushButton *mButtons[PV_ROWS][PV_COLS];
    QColor mGrayColor;       // Original color
    int mMinRow, mMinCol, mMaxRow, mMaxCol;   // Row, column of last min/max
    QCheckBox *mGridValBox;
    QCheckBox *mConvertBox;
    QPushButton *mHelpButton;
};

#endif

/*

$Log$
Revision 4.5  2008/05/27 05:33:15  mast
Changes for rgb and memory displays

Revision 4.4  2006/09/18 15:46:46  mast
Moved mouse line to top

Revision 4.3  2006/09/17 18:15:34  mast
Added mouse position/value report line

Revision 4.2  2003/03/26 06:30:56  mast
adjusting to font changes

Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.2  2003/01/10 23:49:19  mast
clean up unused call

Revision 1.1.2.1  2003/01/04 03:49:53  mast
Initial creation

*/

