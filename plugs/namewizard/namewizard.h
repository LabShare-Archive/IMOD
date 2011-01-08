#include "dialog_frame.h"
#include <QKeyEvent>
#include <QLabel>
#include <QEvent>
#include <QCloseEvent>
class QPushButton;
class QCheckBox;

class QLabel;
class QComboBox;
class QScrollArea;
class QGridLayout;
class QVBoxLayout;
class QHBoxLayout;
class QSpacerItem;
class QEvent;
class QColorDialog;
class QColor;
class QCompleter;
class QListView;
class QMenu;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"
#include "customdialog.h"

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################


struct ObjectLineItem
{
  QString      prevName;
  QString      numContsStr;
  QColor       prevColor;
  QColor       matchColor;
  bool         hasMatch;
  bool         matchHasColor;
  bool         colorsMatch;
  bool         setup;
  
  QCheckBox    *chkObj;
  QWidget      *widLine;
  QHBoxLayout  *layLine;
  QLabel       *lblObjNum;
  QLineEdit    *txtObjName;
  ColorButton  *btnColor;
  QLabel       *lblLink;
  QLineEdit    *txtIdentifier;
  
  
  ObjectLineItem()
  {
    setup = false;
    numContsStr = "";
    reset();
  }
  
  void reset()
  {
    hasMatch = false;
    colorsMatch = false;
    matchHasColor = false;
    prevName = "";
  }
};

//-------------------------------
//## INTERPOLATOR WINDOW:

class NameWizard : public DialogFrame
{
  Q_OBJECT

public:
  NameWizard(QWidget *parent, const char *name = NULL);
  ~NameWizard() {};
  QAction *addAction( QMenu *menu, const char *member, QString text, QString tip );
  int countSelectedObjs( string *objList );
  QString getFirstValidFilePath();
  void resizeEvent ( QResizeEvent * event );
  
public slots:
  void buttonPressed(int);
  void initValues();
  void loadSettings();
  void saveSettings();
  
  void nameModified();
  void changeCols( int i );
  void changeColsIdx0() { changeCols(0); }
  void changeColsIdx1() { changeCols(1); }
  
  void refresh();
  void refreshObjList();
  void refreshObjItem( int itemIdx );
  
  void loadTable();
  void loadNames();
  int  loadNamesFromFile( QString );
  
  int  updateColors();
  void matchColors();
  
  void deleteSelected();
  void mergeSelected();
  void duplicateSelected();
  void moveSelected();
  
  void batchRenameSelected();
  void deselectAll();
  void selectRange();
  void selectMatching();
  
  void helpPluginHelp();
  void helpNamingHelp();
  
  
protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  int  promptRenameObject( int objIdx );
  
private:
  
  QLabel       *lblIdentifier;
  QPushButton  *btnChangeCol;
  
  QWidget      *widList;
  QVBoxLayout  *layList;
  QScrollArea  *scrollArea;
  
  vector<ObjectLineItem> lineItem;
  
  QCompleter *completer;
  
  QHBoxLayout  *layButtons;
  QCheckBox    *useSuggestedColors;
  QPushButton  *refreshListButton;
  QPushButton  *selectionButton;
  QPushButton  *editNameTableButton;
  QPushButton  *matchColorsButton;
};


//-------------------------------
//## CONSTANTS:

const int NUM_SAVED_VALS = 1;

//-------------------------------


struct NameEntry
{
  QString name;
  QString red;
  QString green;
  QString blue;
  QString hyperlink;
  QString identifier;
  QString description;
	QString superCat;
	QString synonyms;
};


//-------------------------------
//## NAMEWIZARD DATA STRUCTURE:

struct NameWizardData   // contains all local plugin data
{
  ImodView    *view;
  NameWizard *window;
  
  //## VARIABLES:
  
  QString defaultFilePath;
  QString secondaryFilePath;
  
  QStringList wordList;
  vector<NameEntry> nameList;
  
  
  //## SETTINGS:
  
  int rightColIdx;
  //bool copySuggestedColors;
  
  bool initialized;
};



//############################################################


//-------------------------------
//## SMALL FUNCTIONS:

Iobj *getCurrObj();
Icont *getCurrCont();
Ipoint *getCurrPt();
bool isCurrObjValidAndShown();
bool isCurrContValid();
bool isCurrPtValid();

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj, int objIdx );


//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_changeSelectedSlice( int changeZ, bool redraw, bool snapToEnds=true );

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );
int edit_removeAllFlaggedContoursFromObj( Iobj *obj );

//############################################################
