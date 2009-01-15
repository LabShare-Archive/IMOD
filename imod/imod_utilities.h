/*   imod_utilities.h  -  private declarations for imod_utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 *  $Log$
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
 *
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
char *imodwfname(const char *intro);
char *imodwEithername(const char *intro, const char *filein, int modelFirst);
char *imodwGivenName(const char *intro, const char *filein);
QString imodCaption(const char *intro);
#endif
