/*   pixelview.h  -  declarations for pixelview.cpp and PixelView class
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.2  2003/01/10 23:49:19  mast
clean up unused call

Revision 1.1.2.1  2003/01/04 03:49:53  mast
Initial creation

*/

#ifndef PIXELVIEW_H
#define PIXELVIEW_H

int  open_pixelview(struct ViewInfo *vi);

#define PV_ROWS 7
#define PV_COLS 7

#include <qwidget.h>
class QLabel;
class QPushButton;

class PixelView: public QWidget
{
  Q_OBJECT

 public:
  PixelView(QWidget *parent, const char *name = 0, 
	      WFlags fl = Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~PixelView() {};
  void update();
  void setButtonWidths();

  public slots:
    void buttonPressed(int pos);

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void fontChange(const QFont &oldFont) {setButtonWidths();};

 private:
    QLabel *mBotLabels[PV_COLS];
    QLabel *mLeftLabels[PV_ROWS];
    QPushButton *mButtons[PV_ROWS][PV_COLS];
    QColor mGrayColor;       // Original color
    int mMinRow, mMinCol, mMaxRow, mMaxCol;   // Row, column of last min/max
};

#endif
