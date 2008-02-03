/*   imod_utilities.h  -  private declarations for imod_utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 *  $Log$
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
char *imodwfname(char *intro);
char *imodwEithername(char *intro, char *filein, int modelFirst);
char *imodwGivenName(char *intro, char *filein);
QString imodCaption(char *intro);
#endif
