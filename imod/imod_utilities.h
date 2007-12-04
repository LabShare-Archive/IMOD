/*   imod_utilities.h  -  private declarations for imod_utilities.cpp
 *       Publicly exposed utilities are in imod.h
 *
 *  $Id$
 *  $Log$
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
char *imodwfname(char *intro);
char *imodwEithername(char *intro, char *filein, int modelFirst);
char *imodwGivenName(char *intro, char *filein);
QString imodCaption(char *intro);
#endif
