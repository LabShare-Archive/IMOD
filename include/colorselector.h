/*   colorselector.h  -  declarations for colorselector.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.2  2003/03/19 19:38:20  mast
Change the color panel to a GL widget

Revision 3.1  2003/02/10 20:57:02  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:37:08  mast
includes for library

Revision 1.1.2.5  2003/01/23 19:55:42  mast
switch from button pressed to clicked

Revision 1.1.2.4  2003/01/01 05:43:44  mast
rationalizing toplevel versus dialog style

Revision 1.1.2.3  2002/12/30 06:36:10  mast
parameterizing the hot slider

Revision 1.1.2.2  2002/12/29 04:13:15  mast
inherit dialog_frame

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation

*/

#include "dialog_frame.h"
#include <qgl.h>

class MultiSlider;
class QFrame;
class ColorSelectorGL;

class ColorSelector : public DialogFrame
{
  Q_OBJECT

 public:
  ColorSelector(QWidget *parent, QString label, int red, int green, int blue, 
                int hotFlag, int hotKey, const char *name = NULL, 
                WFlags fl =  Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~ColorSelector();

 signals:
  void newColor(int r, int g, int b);
  void done();
  void closing();
  void keyPress( QKeyEvent * e );
  void keyRelease( QKeyEvent * e );

  public slots:
    void buttonPressed(int which);
    void sliderChanged(int which, int value, bool dragging);

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );

 private:
    void donePressed();
    void restorePressed();
    void qtSelectorPressed();
    void imposeColor(bool setSliders, bool emitSignal);
    bool mCtrlPressed;
    int mHotKey;
    int mHotFlag;
    int mOriginalRGB[3];
    int mCurrentRGB[3];
    MultiSlider *mSliders;
    QFrame *mColorBox;
    ColorSelectorGL *mGLw;
};

class ColorSelectorGL : public QGLWidget
{
 public:
  ColorSelectorGL(int *currentRGB, QWidget * parent = 0, 
		  const char * name = 0);
  ~ColorSelectorGL() {};
 protected:
  void paintGL();
  void timerEvent(QTimerEvent *e);

 private:
  int *mRGB;
  bool mFirstDraw;
};
