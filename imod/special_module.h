/*   special_module.h  -  simple class to have an internal module treated like
 *                          a plugin and appear in Special menu
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1  2003/10/01 05:08:32  mast
Initial creation

*/

#ifndef SPECIAL_CLASS_H
#define SPECIAL_CLASS_H

#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif
class QStringList;

class QKeyEvent;
typedef char *(*SpecialInfo)(int *);
typedef void (*SpecialExecuteType)(ImodView *, int, int);
typedef int (*SpecialExecuteMessage)(ImodView *, QStringList *, int *);
typedef void (*SpecialExecute)(ImodView *);
typedef int (*SpecialKeys)(ImodView *, QKeyEvent *);

class SpecialModule
{
 public:
  SpecialInfo mInfo;
  SpecialExecuteType mExecuteType;
  SpecialExecuteMessage mExecuteMessage;
  SpecialExecute mExecute;
  SpecialKeys mKeys;
};

#endif
