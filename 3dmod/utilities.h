/*   utilities.h  -  private declarations for utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 */

#ifndef UTILITIES_H
#define  UTILITIES_H
#include <qstring.h>

#define TB_AUTO_RAISE true
#define FLIP_TO_ROTATION 0
#define ROTATION_TO_FLIP 1
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
class QGLWidget;
class QSignalMapper;
class QMenu;
class QAction;
class HotToolBar;
class QMainWindow;

typedef struct {
  const char *text;
  int key;
  unsigned char ctrl;
  unsigned char shift;
  short mainIndex;
} PopupEntry;

typedef struct ViewInfo ImodView;
typedef struct scale_bar ScaleBar;

void utilDrawSymbol(int mx, int my, int sym, int size, int flags);
void utilCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                          int *imPtSize);
void utilGetLongestTimeString(ImodView *vi, QString *str);
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
void utilFileListsToIcons(const char *fileList[][2], QIcon *icons[], int num);
QAction *utilSetupToggleButton(QWidget *parent, QToolBar *toolBar, 
                               QBoxLayout *layout, QSignalMapper *mapper,
                               QIcon *icons[], const char *tips[], 
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
                        int digits, float zoom, const char *prefix, const char *message);
void utilFreeMontSnapArrays(unsigned char **fullPix, int numChunks, 
                            unsigned char *framePix, unsigned char **linePtrs);
float utilWheelToPointSizeScaling(float zoom);
void utilWheelChangePointSize(ImodView *vi, float zoom, int delta);
int utilIsBandCommitted(int x, int y, int winX, int winY, int bandmin, int &rbMouseX0,
                        int &rbMouseX1, int &rbMouseY0, int &rbMouseY1, int *dragging);
void utilAnalyzeBandEdge(int ix, int iy, int rbMouseX0, int rbMouseX1, int rbMouseY0,
                         int rbMouseY1, int &dragBand, int *dragging);
void utilSetCursor(int mode, bool setAnyway, bool needSpecial, bool needSizeAll,
                   int *dragging, bool needModel, int &mouseMode, int &lastShape, 
                   QGLWidget *GLw);
int utilTestBandMove(int x, int y, int rbMouseX0, int rbMouseX1, int rbMouseY0,
                     int rbMouseY1);
void utilExchangeFlipRotation(Imod *imod, int direction);
Icont *utilAutoNewContour(ImodView *vi, Icont *cont, bool notPlanar, bool timeMismatch,
                          int timeLock, int setSurface, const char *planeText,
                          const char *surfText);
void utilAssignSurfToCont(ImodView *vi, Iobj *obj, Icont *cont, int newSurf);
QAction **utilBuildPopupMenu(PopupEntry *specTable, bool addDefault,
                             QSignalMapper *mapper, QMenu *menu, int &numSpecific,
                             QAction **mainActions);
void utilBuildExecPopupMenu(QWidget *parent, PopupEntry *specTable, bool addDefault, 
                            QSignalMapper *mapper, QContextMenuEvent *event);
int utilLookupPopupHit(int index, PopupEntry *specificTable, int numSpecific,
                       Qt::KeyboardModifiers &modifiers);
HotToolBar *utilMakeToolBar(QMainWindow *parent, bool addBreak, int spacing,
                            const char *caption);
void setupFilledContTesselator();
void drawFilledPolygon(Icont *cont);
void utilWprintMeasure(QString &baseMess, Imod *imod, float measure, bool area = false);
char *imodwfname(const char *intro);
char *imodwEithername(const char *intro, const char *filein, int modelFirst);
void setModvDialogTitle(QWidget *dia, const char *intro);
char *imodwGivenName(const char *intro, const char *filein);
QString imodCaption(const char *intro);
#endif

