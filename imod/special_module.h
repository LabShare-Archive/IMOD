//Added by qt3to4:
#include <QKeyEvent>
#include <QEvent>
#include <QMouseEvent>
/*   special_module.h  -  simple class to have an internal module treated like
 *                          a plugin and appear in Special menu
 *
 *  $Id$
 */

#ifndef SPECIAL_CLASS_H
#define SPECIAL_CLASS_H

#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif
class QStringList;

class QKeyEvent;
class QMouseEvent;
typedef char *(*SpecialInfo)(int *);
typedef void (*SpecialExecuteType)(ImodView *, int, int);
typedef int (*SpecialExecuteMessage)(ImodView *, QStringList *, int *);
typedef void (*SpecialExecute)(ImodView *);
typedef int (*SpecialKeys)(ImodView *, QKeyEvent *);
typedef int (*SpecialMouse)(ImodView *, QMouseEvent *, float, float,
                        int, int, int);
typedef int (*SpecialEvent)(ImodView *, QEvent *, float, float);

class SpecialModule
{
 public:
  SpecialInfo mInfo;
  SpecialExecuteType mExecuteType;
  SpecialExecuteMessage mExecuteMessage;
  SpecialExecute mExecute;
  SpecialKeys mKeys;
  SpecialMouse mMouse;
  SpecialEvent mEvent;
};

#endif

/*

$Log$
Revision 1.4  2007/12/04 18:48:45  mast
Added event function

Revision 1.3  2006/02/13 05:11:03  mast
Added mouse function

Revision 1.2  2004/09/24 18:08:34  mast
Added message execution function

Revision 1.1  2003/10/01 05:08:32  mast
Initial creation

*/

