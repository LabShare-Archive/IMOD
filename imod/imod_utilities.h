/*   imod_utilities.h  -  private declarations for imod_utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 */

#ifndef UTILITIES_H
#define  UTILITIES_H
#include <qstring.h>

#define TB_AUTO_RAISE true
class ToolEdit;
class QToolBar;
class QWidget;
class ArrowButton;
class QPushButton;
class QToolButton;
class QToolEdit;
class QBoxLayout;
class QIcon;
class QSignalMapper;
class QMouseEvent;
class QKeyEvent;

typedef struct ViewInfo ImodView;
typedef struct scale_bar ScaleBar;

void utilDrawSymbol(int mx, int my, int sym, int size, int flags);
void utilCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                          int *imPtSize);
void utilGetLongestTimeString(ImodView *vi, QString *str);
int utilContInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax);
bool utilEnableStipple(ImodView *vi, Icont *cont);
void utilDisableStipple(ImodView *vi, Icont *cont);
void utilClearWindow(int index);
float utilMouseZaxisRotation(int winx, int mx, int lastmx, int winy, int my,
                             int lastmy);
void utilSetObjFlag(Iobj *obj, int flagType, bool state, b3dUInt32 flag);
int utilNextSecWithCont(ImodView *vi, Iobj *obj, int curz, int dir);
ToolEdit *utilTBZoomTools(QWidget *parent, QToolBar *toolBar, 
                          ArrowButton **upArrow, ArrowButton **downArrow);
QAction *utilTBArrowButton(Qt::ArrowType type, QWidget *parent, 
                           QToolBar *toolBar, ArrowButton **arrow,
                           const char *toolTip);
QAction *utilTBToolEdit(int width, QWidget *parent, QToolBar *toolBar,
                        ToolEdit **edit, const char *toolTip);
QAction *utilTBToolButton(QWidget *parent, QToolBar *toolBar,
                          QToolButton **button, const char *toolTip);
QAction *utilTBPushButton(const char *text, QWidget *parent, QToolBar *toolBar,
                          QPushButton **button, const char *toolTip);
void utilBitListsToIcons(unsigned char *bitList[][2], QIcon *icons[], int num);
QAction *utilSetupToggleButton(QWidget *parent, QToolBar *toolBar, 
                               QBoxLayout *layout, QSignalMapper *mapper,
                               QIcon *icons[], char *tips[], 
                               QToolButton *buts[], int states[], int ind);
void utilRaiseIfNeeded(QWidget *window, QMouseEvent *event);
bool utilNeedToSetCursor();
bool utilCloseKey(QKeyEvent *e);
bool utilStartMontSnap(int winx, int winy, int xFullSize, int yFullSize,
                       float factor, ScaleBar &barSaved, int &numChunks,
                       unsigned char **framePix, unsigned char ***fullPix,
                       unsigned char ***linePtrs);
void utilMontSnapScaleBar(int ix, int iy, int frames, int winx, int winy, 
                          float scale, bool savedDraw);
void utilFinishMontSnap(unsigned char **linePtrs,
                        int xFullSize, int yFullSize, int format, int &fileno,
                        int digits, float zoom, char *prefix, char *message);
void utilFreeMontSnapArrays(unsigned char **fullPix, int numChunks, 
                            unsigned char *framePix, unsigned char **linePtrs);
float utilWheelToPointSizeScaling(float zoom);
char *imodwfname(const char *intro);
char *imodwEithername(const char *intro, const char *filein, int modelFirst);
char *imodwGivenName(const char *intro, const char *filein);
QString imodCaption(const char *intro);
#endif

