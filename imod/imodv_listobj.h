/*   imodv_listobj.h  -  declarations for list_obj.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODV_LISTOBJ_H
#define IMODV_LISTOBJ_H

//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>
#include <QCloseEvent>

#define OBJLIST_NUMBUTTONS 9

typedef struct __imodv_struct ImodvApp;
#include <qwidget.h>
class QGridLayout;
class QFrame;
class QScrollArea;
class QPushButton;
class QSpinBox;
class QLineEdit;
class QLabel;

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
  ImodvOlist(QWidget *parent, Qt::WFlags fl = Qt::Window);
  ~ImodvOlist() {};
  QFrame *mFrame;
  QScrollArea *mScroll;
  void updateGroups(ImodvApp *a);
  void adjustFrameSize();

 public slots:
  void toggleListSlot(int ob);
  void toggleGroupSlot(int ob);
  void donePressed();
  void helpPressed();
  void actionButtonClicked(int which);
  void nameChanged ( const QString & );
  void curGroupChanged(int value);
  void returnPressed();

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void fontChange(const QFont &oldFont);

 private:
    void setFontDependentWidths();
  QGridLayout *mGrid;
  QSpinBox *mGroupSpin;
  QLineEdit *mNameEdit;
  QPushButton *mHelpButton, *mDoneButton;
  QPushButton *mButtons[OBJLIST_NUMBUTTONS];
  QLabel *mNumberLabel;
};

#endif
/*

$Log$
Revision 4.3  2008/05/22 15:41:59  mast
Added Others On button

Revision 4.2  2008/01/27 06:22:28  mast
Added multiple object group controls

Revision 4.1  2008/01/21 17:48:30  mast
New module


*/
