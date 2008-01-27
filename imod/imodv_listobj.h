/*   imodv_listobj.h  -  declarations for list_obj.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODV_LISTOBJ_H
#define IMODV_LISTOBJ_H


#define OBJLIST_NUMBUTTONS 8

typedef struct __imodv_struct ImodvApp;
#include <qwidget.h>
class QGridLayout;
class QFrame;
class QScrollView;
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
  ImodvOlist(QWidget *parent, const char *name = NULL, 
                WFlags fl =  Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~ImodvOlist() {};
  QFrame *mFrame;
  QScrollView *mScroll;
  void updateGroups(ImodvApp *a);

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
Revision 4.1  2008/01/21 17:48:30  mast
New module


*/
