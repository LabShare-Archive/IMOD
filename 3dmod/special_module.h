/*   special_module.h  -  simple class to have an internal module treated like
 *                          a plugin and appear in Special menu
 *
 *  $Id$
 */

#ifndef SPECIAL_CLASS_H
#define SPECIAL_CLASS_H

#include <qstring.h>
#include <qstringlist.h>

#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif
class QKeyEvent;
class QMouseEvent;
typedef const char *(*SpecialInfo)(int *);
typedef void (*SpecialExecuteType)(ImodView *, int, int);
typedef int (*SpecialExecuteMessage)(ImodView *, QStringList *, int *);
typedef void (*SpecialExecute)(ImodView *);
typedef int (*SpecialKeys)(ImodView *, QKeyEvent *);
typedef int (*SpecialMouse)(ImodView *, QMouseEvent *, float, float, int, int, int);
typedef int (*SpecialEvent)(ImodView *, QEvent *, float, float);
typedef QString (*SpecialOpenFileName)(QWidget *, const QString &, const QString &, 
                                      const QString &);
typedef QStringList (*SpecialOpenFileNames)(QWidget *, const QString &, const QString &, 
                                           const QString &);
typedef QString (*SpecialSaveFileName)(QWidget *, const QString &);

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
  SpecialOpenFileName mOpenFileName;
  SpecialOpenFileNames mOpenFileNames;
  SpecialSaveFileName mSaveFileName;
};

#endif
