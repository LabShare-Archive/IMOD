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
//## CONSTANTS:

const int NUM_SAVED_VALS = 4;

//############################################################


//-------------------------------
//## NAME ENTRY STRUCTURE:

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
//## OBJECT LINE ITEM STRUCTURE:

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
  bool         checked;
	NameEntry    nameEntry;
	
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
//## NAME WIZARD WINDOW:

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
	
  void initValues();
  void loadSettings();
  void saveSettings();
  void showNagScreenPersistentCsv();
  
  bool ifRefreshNeeded();
  void alertIfRefreshNeeded();
	void checkClicked();
  void nameModified();
  void changeCols( int i );
  void changeColsIdx0() { changeCols(0); }
  void changeColsIdx1() { changeCols(1); }
  
  void refreshObjList();
  void refreshObjItem( int itemIdx );
  void updateStatusLabel();
  
  void editStandardNamesFile();
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
	QColor makeQColor( float r, float g, float b );
  void batchRecolorSelected();
	void randomlyRecolor();
	
	void batchLabelChange();
	bool setObjLabel( Iobj *obj, QString newLabelStr );
	QString getObjLabel( Iobj *obj );
	
  void moreSettings();
  void buttonPressed(int);
	
	void helpPluginHelp();
  void helpNamingHelp();
	
protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  int  promptRenameObject( int objIdx );
  
private:
  
	QLabel      *lblStatusLabel;			// changes color and shows # of bad matches etc
	
  QLabel       *lblIdentifier;			// set to "UniqueID" or "Contours"
  QPushButton  *btnChangeCol;				// tiny drop down button to change above value
  
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
  
  
  //## SAVED SETTINGS:
  
  int rightColIdx;            // used to change the right-most column to show
                              //  either "UniqueId" or "Contours"
  bool showNagPersitentCsv;   // if true, will show nag screen if the file path 
                              //  "secondaryFilePath" has no file
  bool showStatusLabel;       // if true, shows "lblStatusLabel".
  
	bool addIdToObjLabels;			// if true: will change the label of objects to match the
															//  the uniqueID of the matchin name entry, unless the
															//  word "STEREOLOGY" appears.
	
	//## OTHER SETTINGS:
	
  bool initialized;						// set to true once plugin is initialized
	bool shiftDown;							// set to true when the shift key is down
};



//############################################################

