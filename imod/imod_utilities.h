/*   imod_utilities.h  -  private declarations for imod_utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 *  Log at end
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
void utilDrawSymbol(int mx, int my, int sym, int size, int flags);
void utilCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                          int *imPtSize);
void utilGetLongestTimeString(ImodView *vi, QString *str);
void utilEnableStipple(ImodView *vi, Icont *cont);
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
char *imodwfname(const char *intro);
char *imodwEithername(const char *intro, const char *filein, int modelFirst);
char *imodwGivenName(const char *intro, const char *filein);
QString imodCaption(const char *intro);
#endif

/*
 *
 *  $Log$
 *  Revision 1.9  2009/04/06 19:36:52  mast
 *  Added function to give flag for needing  to fix cursor
 *
 *  Revision 1.8  2009/03/30 18:25:44  mast
 *  Added function to handle raising on mouse event, workaround Mac Qt 4.5.0
 *
 *  Revision 1.7  2009/03/26 05:41:01  mast
 *  Change nearest section function to work for an object passed as argument
 *
 *  Revision 1.6  2009/02/25 05:35:53  mast
 *  Add function for getting next/prev Z with contours
 *
 *  Revision 1.5  2009/01/15 16:33:17  mast
 *  Qt 4 port
 *
 *  Revision 1.4  2008/07/16 04:30:50  mast
 *  Add function to set object flag
 *
 *  Revision 1.3  2008/02/03 18:36:14  mast
 *  Added function for converting mouse movement to in-plane rotation
 *
 *  Revision 1.2  2008/01/13 22:26:13  mast
 *  Added clearing function
 *
 *  Revision 1.1  2007/12/04 18:42:02  mast
 *  Added to get common functions out of xzap.cpp and imod.
 *
 */                                                                           
