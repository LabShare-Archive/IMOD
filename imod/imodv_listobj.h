/*   imodv_listobj.h  -  declarations for list_obj.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODV_LISTOBJ_H
#define IMODV_LISTOBJ_H

typedef struct __imodv_struct ImodvApp;
#include <qwidget.h>
class QGridLayout;
class QFrame;
class QScrollView;

void imodvObjectListDialog(ImodvApp *a, int state);
void imodvOlistSetChecked(ImodvApp *a, int ob, bool state);
void imodvOlistSetColor(ImodvApp *a, int ob);
void imodvOlistUpdateOnOffs(ImodvApp *a);
void imodvOlistUpdateGroups(ImodvApp *a);
bool imodvOlistObjInGroup(ImodvApp *a, int ob);
bool imodvOlistGrouping(void);

class ImodvOlist : public QWidget
{
  Q_OBJECT

 public:
  ImodvOlist(QWidget *parent, const char *name = NULL, 
                WFlags fl =  Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~ImodvOlist() {};

  QGridLayout *mGrid;
  QFrame *mFrame;
  QScrollView *mScroll;

 public slots:
  void toggleListSlot(int ob);
  void toggleGroupSlot(int ob);
  void groupToggled(bool state);
  void donePressed();

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
};

#endif
/*

$Log$

*/
