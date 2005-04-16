package etomo.ui;

import java.awt.AWTEvent;
import java.awt.BorderLayout;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import etomo.BaseManager;
import etomo.Controller;
import etomo.EtomoDirector;
import etomo.storage.DataFileFilter;
import etomo.type.AxisType;
import etomo.util.UniqueKey;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004, 2005</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.24  2005/04/12 19:38:39  sueh
 * <p> bug# 615 Do not disable fit window menu option.
 * <p>
 * <p> Revision 3.23  2005/04/01 02:53:07  sueh
 * <p> bug# 622 newstuff: turning off fit after fit is excuted.  Added setEnabledFit().
 * <p>
 * <p> Revision 3.22  2005/04/01 00:11:45  sueh
 * <p> bug# 622 Disable A, B, and Both menu items when doing a single axis
 * <p> tomogram.
 * <p>
 * <p> Revision 3.21  2005/03/30 23:44:14  sueh
 * <p> bug# 622 Removed the divider in Axis A only and B only.
 * <p>
 * <p> Revision 3.20  2005/02/17 20:24:47  sueh
 * <p> Removed unused code.
 * <p>
 * <p> Revision 3.19  2005/02/11 23:14:14  sueh
 * <p> bug# 594 Removing the Window menu, since it is redundant.
 * <p>
 * <p> Revision 3.18  2005/02/09 22:29:53  sueh
 * <p> bug# 594 Calling pack() for a new window and MainPanel.fitWindow() for
 * <p> an existing window.  MainPanel.fitWindow() functionality doesn't work
 * <p> when opening Etomo.
 * <p>
 * <p> Revision 3.17  2005/02/09 20:51:36  sueh
 * <p> bug# 594 Moved maximumSize from MainPanel to MainFrame so that it
 * <p> will work with the tabbedPane.  Changing rootPanel to Border layout.
 * <p>
 * <p> Revision 3.16  2005/02/07 22:42:52  sueh
 * <p> bug# 594 Removed setWindowMenuLabels(ConstHashedArray).  Adding
 * <p> functions to call WindowSwitch.add, remove, and rename().  In
 * <p> setCurrentManager(), removing everything from rootPanel before adding
 * <p> result of WindowSwitch.getPanel().
 * <p>
 * <p> Revision 3.15  2005/01/27 00:11:49  sueh
 * <p> bug# 543 For Ctrl-F (fit window) call MainPanel.fitWindow() with force set to
 * <p> true, to cause a fit window with autofit off.
 * <p>
 * <p> Revision 3.14  2004/12/02 20:41:30  sueh
 * <p> bug# 566 Added Join Guide menu item.
 * <p>
 * <p> Revision 3.13  2004/11/19 23:58:35  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.12.2.16  2004/11/19 00:23:00  sueh
 * <p> bug# 520 Enabling or disabling Save As according to whether the current
 * <p> manager will allow its param file to be changed.
 * <p>
 * <p> Revision 3.12.2.15  2004/10/28 22:16:20  sueh
 * <p> bug# 520 Changes HashedArray function to a set a value to an existing
 * <p> key from add() to set().
 * <p>
 * <p> Revision 3.12.2.14  2004/10/18 19:12:24  sueh
 * <p> bug# 520 In menuFileMRUListAction changed call to openManager instead
 * <p> of openTomogram, so Joins can be opened this way.
 * <p>
 * <p> Revision 3.12.2.13  2004/10/15 00:48:52  sueh
 * <p> bug# 520 Removed getTestParamFilename() because it is used to get
 * <p> either .edf or .ejf, but not both.  Changed openEtomoDataFileDialog() to
 * <p> openDataFileDialog() and changed the function to get either .ejf or .edf
 * <p> files.
 * <p>
 * <p> Revision 3.12.2.12  2004/10/11 02:14:34  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.12.2.11  2004/10/08 16:33:08  sueh
 * <p> bug# 520 Moved SettingsDialog to EtomoDirector.  Since EtomoDirector
 * <p> is a singleton, made all functions and member variables non-static.
 * <p> Made openMessageDialog() available to EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.10  2004/10/07 16:38:40  sueh
 * <p> bug# 520 formatted
 * <p>
 * <p> Revision 3.12.2.9  2004/10/06 02:24:30  sueh
 * <p> bug# 520 Created functions to enable and disable New Tomogram and
 * <p> New Join menu items.
 * <p>
 * <p> Revision 3.12.2.8  2004/10/01 19:59:57  sueh
 * <p> bug# 520 Standardized getting the metadata file name.
 * <p>
 * <p> Revision 3.12.2.7  2004/09/15 22:40:52  sueh
 * <p> bug# 520 making openMessageDialog public
 * <p>
 * <p> Revision 3.12.2.6  2004/09/13 20:24:20  sueh
 * <p> bug# 520 Change MRUlist action to open a tomogram in a new window
 * <p> using EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.5  2004/09/13 17:19:12  sueh
 * <p> bug# 520 Changed file menu:  changed New to New Tomogram, added
 * <p> New Join, added Close to close individual managers.  Changed Window
 * <p> menu storeage to a HashedArray with the same keys as
 * <p> EtomoDirectory.managerList.  This makes it easier to build the menu and
 * <p> to access it when a menu item is selected.  Added JPanel.repaint() call
 * <p> to fix a problem where the Setup dialog is not repainting which it is
 * <p> selected in the Window menu.  Renamed setCurrentWindowLabels() to
 * <p> setWindowMenuLabels().  Added numbering to tomogram datasets and
 * <p> joins listed under the window menu, because they may not have unique
 * <p> names.  Changed the menu items under window to JCheckBoxMenuItem
 * <p> and created a function to check one menu item.  Handling most of the
 * <p> file menu items with EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.4  2004/09/09 22:12:55  sueh
 * <p> bug# 520 make etomo switch between datasets by removing and adding
 * <p> MainPanel
 * <p>
 * <p> Revision 3.12.2.3  2004/09/09 17:39:03  sueh
 * <p> bug# 520 Add the Window menu.  Add current window list menu item.
 * <p> Move call to createMenus out of MainFrame() because the list of managers
 * <p> hasn't been built when MainFrame is created.  Create
 * <p> setCurrentWindowLabels() which created a new current window menu list.
 * <p>
 * <p> Revision 3.12.2.2  2004/09/08 22:37:34  sueh
 * <p> bug# 520 removed fields that only belong in MainPanel
 * <p>
 * <p> Revision 3.12.2.1  2004/09/07 17:59:47  sueh
 * <p> bug# 520 encapsulated mainPanel
 * <p>
 * <p> Revision 3.12  2004/07/24 01:53:52  sueh
 * <p> bug# 513 make sure that packAxis() won't fail if it is called
 * <p> when the Setup dialog is running.
 * <p>
 * <p> Revision 3.11  2004/07/24 01:48:04  sueh
 * <p> bug# 513 making fitWindow() public so that app manager can
 * <p> call it
 * <p>
 * <p> Revision 3.10  2004/07/23 23:00:00  sueh
 * <p> bug# 517, bug# 513 comments, renamed functions for clarity,
 * <p> removed code in setDividerPostion() which was making
 * <p> fitWindow() fail when the frame was taller then the screen,
 * <p> moved tooSmall() from AxisPanelPRocess and renamed it
 * <p> isFitScreenError.
 * <p>
 * <p> Revision 3.9  2004/07/23 00:08:16  sueh
 * <p> bug# 517 Don't use setSize() anymore because the layout
 * <p> manager doesn't reliably fix the layout after this call.
 * <p> To get the layout manager to limit the size of the dialogs
 * <p> to the viewable area, and a new rootPanel with a BoxLayout,
 * <p> make it the root of the mainPanel.
 * <p>
 * <p> Revision 3.8  2004/07/21 21:08:33  sueh
 * <p> bug# 512 added options menu items Axis A, Axis B, and Both
 * <p> Axes.  Removed options menu item Advanced
 * <p>
 * <p> Revision 3.7  2004/07/16 23:01:03  sueh
 * <p> bug# 501 sending System.out prints only when debug is set
 * <p>
 * <p> Revision 3.6  2004/07/16 22:05:13  sueh
 * <p> bug# 501 adjusting divider to fix problem with
 * <p> JsplitPane.resetToPreferedSizes() that happens when
 * <p> etomo is too wide for the screen
 * <p>
 * <p> Revision 3.5  2004/05/19 23:17:14  sueh
 * <p> bug# 425 fixing single axis bug
 * <p>
 * <p> Revision 3.4  2004/05/19 23:11:47  sueh
 * <p> bug# 425 if the window is too tall for the screen, resize it
 * <p>
 * <p> Revision 3.3  2004/05/15 01:42:12  sueh
 * <p> bug# 415 MainFrame is already calling System.exit(), so don't
 * <p> do EXIT_ON_CLOSE when X button is pressed.
 * <p>
 * <p> Revision 3.2  2004/04/28 22:38:14  sueh
 * <p> bug# 268 if a panel was hidden, set the divider location to
 * <p> continue to hide it
 * <p>
 * <p> Revision 3.1  2004/02/13 17:39:03  sueh
 * <p> bug# 268 during fit window, if the split bar is all the way to
 * <p> one side, hide the hidden side, resize as usual, then show it
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.21  2003/11/04 22:02:17  sueh
 * <p> bug329 MainFrame: setMRUFileLabels(): Don't display blank
 * <p> MRU entries
 * <p>
 * <p> Revision 2.20  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.19  2003/11/03 19:36:09  sueh
 * <p> bug266 EtomoFileFilter:  added an implements clause to the class def to allow the use
 * <p> of this file with File.listFiles(FileFilter)
 * <p>
 * <p> Revision 2.18  2003/10/30 20:29:55  rickg
 * <p> Bug# 341 3dmod and eTomo users guide
 * <p>
 * <p> Revision 2.17  2003/10/15 17:27:05  sueh
 * <p> Bug266 added a file filter to getTestParamFilename()
 * <p>
 * <p> Revision 2.16  2003/09/30 03:13:38  rickg
 * <p> bug248 File / open now calls the correct function in the
 * <p> AppManager (as does the MRU list opens)
 * <p>
 * <p> Revision 2.15  2003/09/30 02:15:09  rickg
 * <p> Added message dialogs that were originally in the
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.14  2003/06/10 05:14:53  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.13  2003/05/27 08:54:18  rickg
 * <p> Determinant progress bar now takes a string
 * <p>
 * <p> Revision 2.12  2003/05/23 14:22:38  rickg
 * <p> Progress bar determinant delegate methods
 * <p> axisPanel mapping method
 * <p>
 * <p> Revision 2.11  2003/05/19 22:10:03  rickg
 * <p> Added new to file menu
 * <p> Added tomography guide and imod guide to help menu
 * <p> Restructured action handlers
 * <p>
 * <p> Revision 2.10  2003/05/19 04:54:18  rickg
 * <p> Added mnemonics for menus
 * <p>
 * <p> Revision 2.9  2003/05/15 22:25:14  rickg
 * <p> created separate setDividerLocation method to correctly update
 * <p> panel
 * <p>
 * <p> Revision 2.8  2003/05/15 20:21:22  rickg
 * <p> Added extra validation call hopefully to make sure divider gets rendered
 * <p> correctly
 * <p>
 * <p> Revision 2.7  2003/05/08 19:58:25  rickg
 * <p> Addd post processing state update on an updateAll... call
 * <p>
 * <p> Revision 2.6  2003/05/07 17:49:12  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Updated status bar
 * <p>
 * <p> Revision 2.5  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.4  2003/03/20 17:42:18  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.3  2003/01/29 15:18:19  rickg
 * <p> Added combine state setter
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:43  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 23:51:23  rickg
 * <p> Added a split pane manager to the mane window for dual
 * <p> axis layout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.9.2.2  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.9.2.1  2003/01/11 00:45:27  rickg
 * <p> Added app icon
 * <p>
 * <p> Revision 1.9  2002/12/18 20:45:32  rickg
 * <p> Advanced menu implemented
 * <p>
 * <p> Revision 1.8  2002/12/11 21:28:29  rickg
 * <p> Implemented repaint method, doesn't work well
 * <p>
 * <p> Revision 1.7  2002/12/11 00:37:26  rickg
 * <p> Added handler for options/settings menu
 * <p>
 * <p> Revision 1.6  2002/12/09 04:42:29  rickg
 * <p> Automatically add .edf extenstion when doing save as or
 * <p> save with not existing filename
 * <p>
 * <p> Revision 1.5  2002/12/09 04:16:11  rickg
 * <p> Added EDF file filter to open dialog
 * <p>
 * <p> Revision 1.4  2002/11/19 05:32:55  rickg
 * <p> Label spelling correction
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class MainFrame extends JFrame implements ContextMenu {
  public static final String rcsid = "$Id$";
  
  private static final int estimatedMenuHeight = 60;
  private static final int extraScreenWidthMultiplier = 2;
  private static final Dimension frameBorder = new Dimension(10, 48);

  //private JPanel contentPane;
  private JPanel rootPanel;
  private MainPanel mainPanel = null;

  //  Menu bar
  private final int nMRUFileMax = 10;
  private JMenuBar menuBar = new JMenuBar();

  private JMenu menuFile = new JMenu("File");
  private JMenuItem menuFileNewTomogram = new JMenuItem("New Tomogram",
      KeyEvent.VK_N);
  private JMenuItem menuFileNewJoin = new JMenuItem("New Join", KeyEvent.VK_J);
  private JMenuItem menuFileOpen = new JMenuItem("Open...", KeyEvent.VK_O);
  private JMenuItem menuFileSave = new JMenuItem("Save", KeyEvent.VK_S);
  private JMenuItem menuFileSaveAs = new JMenuItem("Save As", KeyEvent.VK_A);
  private JMenuItem menuFileClose = new JMenuItem("Close", KeyEvent.VK_C);
  private JMenuItem menuFileExit = new JMenuItem("Exit", KeyEvent.VK_X);
  private JMenuItem[] menuMRUList = new JMenuItem[nMRUFileMax];

  private JMenu menuOptions = new JMenu("Options");
  private JMenuItem menuAxisA = new JMenuItem("Axis A", KeyEvent.VK_A);
  private JMenuItem menuAxisB = new JMenuItem("Axis B", KeyEvent.VK_B);
  private JMenuItem menuAxisBoth = new JMenuItem("Both Axes", KeyEvent.VK_2);
  private JMenuItem menuSettings = new JMenuItem("Settings", KeyEvent.VK_S);
  private JMenuItem menuFitWindow = new JMenuItem("Fit Window", KeyEvent.VK_F);

  private JMenu menuHelp = new JMenu("Help");
  private JMenuItem menuTomoGuide = new JMenuItem("Tomography Guide",
      KeyEvent.VK_T);
  private JMenuItem menuImodGuide = new JMenuItem("Imod Users Guide",
      KeyEvent.VK_I);
  private JMenuItem menu3dmodGuide = new JMenuItem("3dmod Users Guide",
      KeyEvent.VK_3);
  private JMenuItem menuEtomoGuide = new JMenuItem("Etomo Users Guide",
      KeyEvent.VK_E);
  private JMenuItem menuJoinGuide = new JMenuItem("Join Users Guide",
      KeyEvent.VK_J);
  private JMenuItem menuHelpAbout = new JMenuItem("About", KeyEvent.VK_A);

  //manager object
  private BaseManager currentManager;
  GenericMouseAdapter mouseAdapter = null;
  WindowSwitch windowSwitch = new WindowSwitch();
  private SubFrame subFrame = null;

  /**
   * Main window constructor.  This sets up the menus and status line.
   */
  public MainFrame() {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);

    ImageIcon iconEtomo = new ImageIcon(ClassLoader
        .getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());

    setTitle("eTomo");

    Toolkit toolkit = Toolkit.getDefaultToolkit();
    Dimension screenSize = toolkit.getScreenSize();
    screenSize.height -= estimatedMenuHeight;
    screenSize.width *= extraScreenWidthMultiplier;
    Dimension rootPanelSize = new Dimension(screenSize);
    rootPanelSize.height -= frameBorder.height;
    rootPanelSize.width -= frameBorder.width;

    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    rootPanel.setMaximumSize(rootPanelSize);

    //rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.PAGE_AXIS));

    //  add the context menu to all of the main window objects
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
  }

  public void setCurrentManager(BaseManager currentManager, UniqueKey managerKey, boolean newWindow) {
    this.currentManager = currentManager;
    if (mainPanel != null) {
      rootPanel.removeAll();
    }
    if (currentManager != null) {
      mainPanel = currentManager.getMainPanel();
      rootPanel.add(windowSwitch.getPanel(managerKey));
      mainPanel.addMouseListener(mouseAdapter);
      enableMenu(currentManager);
      mainPanel.repaint();
    }
    if (newWindow) {
      pack();
    }
    else {
      mainPanel.fitWindow();
    }
  }
  
  MainPanel getMainPanel() {
    return mainPanel;
  }
  
  private void enableMenu(BaseManager currentManager) {
    menuFileSaveAs.setEnabled(currentManager.canChangeParamFileName());
    boolean dualAxis = currentManager.getBaseMetaData().getAxisType() == AxisType.DUAL_AXIS;
    menuAxisA.setEnabled(dualAxis);
    menuAxisB.setEnabled(dualAxis);
    menuAxisBoth.setEnabled(dualAxis);
  }
  
  public void setCurrentManager(BaseManager currentManager, UniqueKey managerKey) {
    setCurrentManager(currentManager, managerKey, false);
  }

  //  Right mouse button context menu
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(mainPanel, mouseEvent, "");
  }

  public void setEnabledNewTomogramMenuItem(boolean enable) {
    menuFileNewTomogram.setEnabled(enable);
  }

  public void setEnabledNewJoinMenuItem(boolean enable) {
    menuFileNewJoin.setEnabled(enable);
  }

  /**
   * Set the MRU etomo data file list.  This fills in the MRU menu items
   * on the File menu
   */
  public void setMRUFileLabels(String[] mRUList) {
    for (int i = 0; i < mRUList.length; i++) {
      if (i == nMRUFileMax) {
        return;
      }
      if (mRUList[i].equals("")) {
        menuMRUList[i].setVisible(false);
      }
      else {
        menuMRUList[i].setText(mRUList[i]);
        menuMRUList[i].setVisible(true);
      }
    }
    for (int i = mRUList.length; i < nMRUFileMax; i++) {
      menuMRUList[i].setVisible(false);
    }
  }
  
  public void addWindow(Controller controller, UniqueKey controllerKey) {
    windowSwitch.add(controller, controllerKey);
  }
  
  public void removeWindow(UniqueKey controllerKey) {
    windowSwitch.remove(controllerKey);
  }
  
  public void renameWindow(UniqueKey oldKey, UniqueKey newKey) {
    windowSwitch.rename(oldKey, newKey);
  }

  public void selectWindowMenuItem(UniqueKey currentManagerKey) {
    windowSwitch.selectWindow(currentManagerKey);
  }

  /**
   * Handle File menu actions
   * @param event
   */
  private void menuFileAction(ActionEvent event) {
    if (event.getActionCommand().equals(menuFileNewTomogram.getActionCommand())) {
      EtomoDirector.getInstance().openTomogram(true);
    }

    if (event.getActionCommand().equals(menuFileNewJoin.getActionCommand())) {
      EtomoDirector.getInstance().openJoin(true);
    }

    if (event.getActionCommand().equals(menuFileOpen.getActionCommand())) {
      File dataFile = openDataFileDialog();
      if (dataFile != null) {
        EtomoDirector.getInstance().openManager(dataFile, true);
      }
    }

    if (event.getActionCommand().equals(menuFileSave.getActionCommand())) {
      //  Check to see if there is a current parameter file chosen
      //  if not open a dialog box to select the name
      boolean haveTestParamFilename = true;
      if (currentManager.getTestParamFile() == null) {
        haveTestParamFilename = mainPanel.getTestParamFilename();
      }
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile();
      }
    }

    if (event.getActionCommand().equals(menuFileSaveAs.getActionCommand())) {
      boolean haveTestParamFilename = mainPanel.getTestParamFilename();
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile();
      }
    }

    if (event.getActionCommand().equals(menuFileClose.getActionCommand())) {
      EtomoDirector.getInstance().closeCurrentManager();
    }

    if (event.getActionCommand().equals(menuFileExit.getActionCommand())) {
      //  Check to see if we need to save any data
      if (EtomoDirector.getInstance().exitProgram()) {
        System.exit(0);
      }
    }
  }
/*
  private void menuWindowAction(ActionEvent event) {
  }
*/
  /**
   * Open the specified MRU EDF file
   * @param event
   */
  private void menuFileMRUListAction(ActionEvent event) {
    EtomoDirector.getInstance().openManager(new File(event.getActionCommand()),
        true);
  }

  /**
   * Handle the options menu events
   * @param event
   */
  private void menuOptionsAction(ActionEvent event) {
    String command = event.getActionCommand();
    boolean newStuff = EtomoDirector.getInstance().isNewstuff();
    if (command.equals(menuSettings.getActionCommand())) {
      EtomoDirector.getInstance().openSettingsDialog();
    }
    else if (command.equals(menuAxisA.getActionCommand())) {
      if (newStuff) {
        showAxisA();
      }
      else {
        mainPanel.setDividerLocation(1);
      }
    }
    else if (command.equals(menuAxisB.getActionCommand())) {
      if (newStuff) {
        showAxisB();
      }
      else {
        mainPanel.setDividerLocation(0);
      }
    }
    else if (command.equals(menuAxisBoth.getActionCommand())) {
      if (newStuff) {
        showBothAxis();
      }
      else {
        mainPanel.setDividerLocation(.5);
      }
    }
    else if (command.equals(menuFitWindow.getActionCommand())) {
      mainPanel.fitWindow(true);
    }
  }
  
  public void pack() {
    super.pack();
    if (subFrame != null) {
      subFrame.pack();
    }
  }
  
  private void packMainFrame() {
    super.pack();
  }
  
  private void packSubFrame() {
    if (subFrame != null) {
      subFrame.pack();
    }
  }
  
  public void showAxisA() {
    if (subFrame != null) {
      subFrame.setVisible(false);
    }
    mainPanel.showAxisA();
  }
  
  public void showAxisB() {
    if (subFrame != null) {
      subFrame.setVisible(false);
    }
    mainPanel.showAxisB();
  }
  
  public void showBothAxis() {
    mainPanel.showAxisA();
    if (subFrame == null || !subFrame.isDisplayable()) {
      subFrame = new SubFrame(this);
    }
    else {
      subFrame.updateAxis();
    }
    pack();
  }

  private void menuHelpAction(ActionEvent event) {

    // Get the URL to the IMOD html directory
    String imodURL = "";
    try {
      imodURL = EtomoDirector.getInstance().getIMODDirectory().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.getInstance().getIMODDirectory().toString());
      return;
    }

    if (event.getActionCommand().equals(menuTomoGuide.getActionCommand())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomoguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menuImodGuide.getActionCommand())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu3dmodGuide.getActionCommand())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "3dmodguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menuEtomoGuide.getActionCommand())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "UsingEtomo.html");
      manpage.setVisible(true);
    }
    
    if (event.getActionCommand().equals(menuJoinGuide.getActionCommand())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomojoin.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menuHelpAbout.getActionCommand())) {
      MainFrame_AboutBox dlg = new MainFrame_AboutBox(this);
      Dimension dlgSize = dlg.getPreferredSize();
      Dimension frmSize = getSize();
      Point loc = getLocation();
      dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x,
          (frmSize.height - dlgSize.height) / 2 + loc.y);
      dlg.setModal(true);
      dlg.show();
    }
  }

  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING) {
      menuFileExit.doClick();
    }
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return boolean True if the Yes option was selected
   */
  public boolean openYesNoDialog(String[] message) {
    try {
      int answer = JOptionPane.showConfirmDialog(this, message,
          "Etomo question", JOptionPane.YES_NO_OPTION);

      if (answer == JOptionPane.YES_OPTION) {
        return true;
      }
      else {
        return false;
      }
    }
    catch (HeadlessException except) {
      except.printStackTrace();
      return false;
    }
  }

  /**
   * Open a Yes, No or Cancel question dialog
   * @param message
   * @return int state of the users select
   */
  public int openYesNoCancelDialog(String[] message) {
    return JOptionPane.showConfirmDialog(this, message, "Etomo question",
        JOptionPane.YES_NO_CANCEL_OPTION);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public void openMessageDialog(Object message, String title) {
    JOptionPane.showMessageDialog(this, message, title,
        JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Open a File Chooser dialog with an data file filter, if the user selects
   * or names a file return a File object wiht that slected, otherwise
   * return null.
   * @return A File object specifiying the selected file or null if none
   * was selected. 
   */
  public File openDataFileDialog() {
    //  Open up the file chooser in current working directory
    JFileChooser chooser = new JFileChooser(new File(System
        .getProperty("user.dir")));
    DataFileFilter fileFilter = new DataFileFilter();
    chooser.setFileFilter(fileFilter);

    chooser.setDialogTitle("Open " + fileFilter.getDescription());
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return chooser.getSelectedFile();
    }
    return null;
  }

  //  TODO Need a way to repaint the existing font
  public void repaintWindow() {
    repaintContainer(this);
    this.repaint();
  }

  private void repaintContainer(Container container) {
    Component[] comps = container.getComponents();
    for (int i = 0; i < comps.length; i++) {
      if (comps[i] instanceof Container) {
        Container cont = (Container) comps[i];
        repaintContainer(cont);
      }
      comps[i].repaint();
    }
  }

  /**
   * Create the menus for the main window
   */
  public void createMenus() {
    //  Mnemonics for the main menu bar
    menuFile.setMnemonic(KeyEvent.VK_F);
    menuOptions.setMnemonic(KeyEvent.VK_O);
    menuHelp.setMnemonic(KeyEvent.VK_H);

    //  Accelerators
    menuSettings.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
        ActionEvent.CTRL_MASK));

    menuAxisA.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
        ActionEvent.CTRL_MASK));
    menuAxisB.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
        ActionEvent.CTRL_MASK));
    menuAxisBoth.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2,
        ActionEvent.CTRL_MASK));

    menuFitWindow.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
        ActionEvent.CTRL_MASK));

    //  Bind the menu items to their listeners
    FileActionListener fileActionListener = new FileActionListener(this);
    menuFileNewTomogram.addActionListener(fileActionListener);
    menuFileNewJoin.addActionListener(fileActionListener);
    menuFileOpen.addActionListener(fileActionListener);
    menuFileSave.addActionListener(fileActionListener);
    menuFileSaveAs.addActionListener(fileActionListener);
    menuFileClose.addActionListener(fileActionListener);
    menuFileExit.addActionListener(fileActionListener);

    OptionsActionListener optionsActionListener = new OptionsActionListener(
        this);
    menuSettings.addActionListener(optionsActionListener);
    menuFitWindow.addActionListener(optionsActionListener);
    menuAxisA.addActionListener(optionsActionListener);
    menuAxisB.addActionListener(optionsActionListener);
    menuAxisBoth.addActionListener(optionsActionListener);

    //WindowActionListener windowActionListener = new WindowActionListener(this);

    HelpActionListener helpActionListener = new HelpActionListener(this);
    menuTomoGuide.addActionListener(helpActionListener);
    menuImodGuide.addActionListener(helpActionListener);
    menu3dmodGuide.addActionListener(helpActionListener);
    menuEtomoGuide.addActionListener(helpActionListener);
    menuJoinGuide.addActionListener(helpActionListener);
    menuHelpAbout.addActionListener(helpActionListener);

    //  File menu
    menuFile.add(menuFileNewTomogram);
    menuFile.add(menuFileNewJoin);
    menuFile.add(menuFileOpen);
    menuFile.add(menuFileSave);
    menuFile.add(menuFileSaveAs);
    menuFile.add(menuFileClose);
    menuFile.add(menuFileExit);
    menuFile.addSeparator();

    //  Initialize all of the MRU file menu items
    FileMRUListActionListener fileMRUListActionListener = new FileMRUListActionListener(
        this);
    for (int i = 0; i < nMRUFileMax; i++) {
      menuMRUList[i] = new JMenuItem();
      menuMRUList[i].addActionListener(fileMRUListActionListener);
      menuMRUList[i].setVisible(false);
      menuFile.add(menuMRUList[i]);
    }

    // Options menu
    menuOptions.add(menuSettings);
    menuOptions.add(menuAxisA);
    menuOptions.add(menuAxisB);
    menuOptions.add(menuAxisBoth);
    menuOptions.add(menuFitWindow);

    // Help menu
    menuHelp.add(menuTomoGuide);
    menuHelp.add(menuImodGuide);
    menuHelp.add(menu3dmodGuide);
    menuHelp.add(menuEtomoGuide);
    menuHelp.add(menuJoinGuide);
    menuHelp.add(menuHelpAbout);

    //  Construct menu bar
    menuBar.add(menuFile);
    menuBar.add(menuOptions);
    menuBar.add(menuHelp);
    setJMenuBar(menuBar);
  }

  //  File menu action listener
  class FileActionListener implements ActionListener {
    MainFrame adaptee;

    FileActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.menuFileAction(event);
    }
  }

  //  MRU file list action listener
  class FileMRUListActionListener implements ActionListener {
    MainFrame adaptee;

    FileMRUListActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuFileMRUListAction(e);
    }
  }

  //  MRU file list action listener
  /*  class WindowListActionListener implements ActionListener {
    MainFrame adaptee;

    WindowListActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuWindowListAction(e);
    }
  }
*/
  // Options file action listener
  class OptionsActionListener implements ActionListener {
    MainFrame adaptee;

    OptionsActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuOptionsAction(e);
    }
  }
/*
  // Options file action listener
  class WindowActionListener implements ActionListener {
    MainFrame adaptee;

    WindowActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuWindowAction(e);
    }
  }*/

  // Help file action listener
  class HelpActionListener implements ActionListener {
    MainFrame adaptee;

    HelpActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuHelpAction(e);
    }
  }

}