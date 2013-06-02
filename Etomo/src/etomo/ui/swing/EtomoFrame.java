package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JMenuBar;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.PeetManager;
import etomo.process.ProcessMessages;
import etomo.storage.DataFileFilter;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DirectiveFileType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
abstract class EtomoFrame extends AbstractFrame {
  public static final String rcsid = "$Id$";

  boolean main;
  EtomoMenu menu;
  JMenuBar menuBar;
  MainPanel mainPanel;
  BaseManager currentManager;

  private final boolean singleFrame;

  static EtomoFrame mainFrame = null;
  static EtomoFrame subFrame = null;

  EtomoFrame() {
    singleFrame = false;
  }

  EtomoFrame(boolean singleFrame) {
    this.singleFrame = singleFrame;
  }

  abstract void register();

  void initialize() {
    menu = EtomoMenu.getInstance(this);
    ImageIcon iconEtomo = new ImageIcon(ClassLoader.getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());
    getMenus();
  }

  /**
   * Saves the current location of the frame to UserConfiguration.
   */
  void saveLocation() {
    if (!EtomoDirector.INSTANCE.getArguments().isIgnoreLoc()) {
      EtomoDirector.INSTANCE.getUserConfiguration().setLastLocation(getFrameType(),
          getLocation());
    }
  }

  void moveSubFrame() {
    if (singleFrame) {
      return;
    }
    if (subFrame != null) {
      subFrame.moveSubFrame();
    }
  }

  void toFront(AxisID axisID) {
    getFrame(axisID).toFront();
  }

  final boolean isMenu3dmodStartupWindow() {
    return menu.isMenu3dmodStartupWindow();
  }

  final boolean isMenuSaveEnabled() {
    return menu.isMenuSaveEnabled();
  }

  final boolean isMenu3dmodBinBy2() {
    return menu.isMenu3dmodBinBy2();
  }

  final void setMenu3dmodStartupWindow(boolean menu3dmodStartupWindow) {
    menu.setMenu3dmodStartupWindow(menu3dmodStartupWindow);
  }

  final void setMenu3dmodBinBy2(boolean menu3dmodBinBy2) {
    menu.setMenu3dmodBinBy2(menu3dmodBinBy2);
  }

  public void menuToolsAction(ActionEvent event) {
    menu.menuToolsAction(getAxisID(), event);
  }

  /**
   * Handle File menu actions
   * @param event
   */
  public void menuFileAction(ActionEvent event) {
    if (!menu.menuFileAction(event)) {
      AxisID axisID = getAxisID();
      if (menu.equalsNewTomogram(event)) {
        EtomoDirector.INSTANCE.openTomogram(true, axisID);
      }
      else if (menu.equalsNewJoin(event)) {
        EtomoDirector.INSTANCE.openJoin(true, axisID);
      }
      else if (menu.equalsNewGenericParallel(event)) {
        EtomoDirector.INSTANCE.openGenericParallel(true, axisID);
      }
      else if (menu.equalsNewAnisotropicDiffusion(event)) {
        EtomoDirector.INSTANCE.openAnisotropicDiffusion(true, axisID);
      }
      else if (menu.equalsNewPeet(event)) {
        if (PeetManager.isInterfaceAvailable()) {
          EtomoDirector.INSTANCE.openPeet(true, axisID);
        }
      }
      else if (menu.equalsNewSerialSections(event)) {
        EtomoDirector.INSTANCE.openSerialSections(true, axisID);
      }
      else if (menu.equalsOpen(event)) {
        File dataFile = openDataFileDialog();
        if (dataFile != null) {
          EtomoDirector.INSTANCE.openManager(dataFile, true, axisID);
        }
      }
      else if (menu.equalsExit(event)) {
        UIHarness.INSTANCE.exit(axisID, 0);
      }
      else if (menu.equalsTomosnapshot(event)) {
        if (currentManager != null) {
          currentManager.tomosnapshot(axisID);
        }
      }
      else if (menu.equalsDirectiveFileEditor(event)) {
        DirectiveFileType type = menu.getDirectiveFileType(event);
        if (type != null) {
          StringBuffer errmsg = new StringBuffer();
          String timestamp = currentManager.saveAll(errmsg);
          if (timestamp != null) {
            EtomoDirector.INSTANCE.openDirectiveEditor(type, currentManager, timestamp,
                errmsg);
          }
        }
      }
    }
  }
  
  void close() {
    EtomoDirector.INSTANCE.closeCurrentManager(getAxisID(), false);
  }

  void cancel() {
  }

  void save(AxisID axisID) {
    try {
      if (currentManager.saveParamFile()) {
        return;
      }
      // Don't allow the user to do the equivalent of a Save As if Save As isn't
      // available.
      if (!currentManager.canChangeParamFileName()) {
        UIHarness.INSTANCE.openMessageDialog(currentManager,
            "Please set the name of dataset or the join before saving", "Cannot Save");
        return;
      }
      // Do a Save As
      if (getParamFilename()) {
        currentManager.saveParamFile();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(currentManager,
          "Unable to write parameters.\n" + e.getMessage(), "Etomo Error", axisID);
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(currentManager,
          "Unable to write parameters.\n" + e.getMessage(), "Etomo Error", axisID);
    }
  }

  void saveAs() {
    try {
      if (getParamFilename()) {
        currentManager.saveParamFile();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(currentManager, "Unable to save parameters.\n"
          + e.getMessage(), "Etomo Error", getAxisID());
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(currentManager, "Unable to save parameters.\n"
          + e.getMessage(), "Etomo Error", getAxisID());
    }
  }

  /**
   * Open the specified MRU EDF file
   * @param event
   */
  void menuFileMRUListAction(ActionEvent event) {
    EtomoDirector.INSTANCE.openManager(new File(event.getActionCommand()), true,
        getAxisID());
  }

  /**
   * Handle help menu actions
   * @param event
   */
  public void menuHelpAction(ActionEvent event) {
    menu.menuHelpAction(currentManager, getAxisID(), this, event);
  }

  /**
   * Handle some of the view menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  public void menuViewAction(ActionEvent event) {
    // Run fitWindow on both frames.
    if (menu.equalsFitWindow(event)) {
      UIHarness.INSTANCE.pack(true, currentManager);
      if (getOtherFrame() != null) {
        UIHarness.INSTANCE.pack(AxisID.SECOND, true, currentManager);
      }
    }
    else {
      throw new IllegalStateException(
          "Cannot handled menu command in this class.  command="
              + event.getActionCommand());
    }
  }

  /**
   * Handle some of the options menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  public void menuOptionsAction(ActionEvent event) {
    if (menu.equalsSettings(event)) {
      EtomoDirector.INSTANCE.openSettingsDialog();
    }
    else if (menu.equals3dmodStartUpWindow(event)) {
      EtomoFrame frame = getOtherFrame();
      if (frame != null) {
        frame.setMenu3dmodStartupWindow(isMenu3dmodStartupWindow());
      }
    }
    else if (menu.equals3dmodBinBy2(event)) {
      EtomoFrame frame = getOtherFrame();
      if (frame != null) {
        frame.setMenu3dmodBinBy2(isMenu3dmodBinBy2());
      }
    }
    else {
      throw new IllegalStateException(
          "Cannot handled menu command in this class.  command="
              + event.getActionCommand());
    }
  }

  /**
   * Enable/disable menu items.  Also run this function on the other frame since
   * it changes the menu's appearance.
   * @param currentManager
   */
  void setEnabled(BaseManager currentManager) {
    menu.setEnabled(currentManager);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      otherFrame.menu.setEnabled(currentManager);
    }
  }

  /**
   * Set the MRU etomo data file list.  This fills in the MRU menu items
   * on the File menu.
   * Also run this function on the other frame since it changes the menu's
   * appearance.
   */
  void setMRUFileLabels(String[] mRUList) {
    menu.setMRUFileLabels(mRUList);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setMRUFileLabels(mRUList);
    }
  }

  void setEnabledNewTomogramMenuItem(boolean enable) {
    menu.setEnabledNewTomogram(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewTomogram(enable);
    }
  }

  void setEnabledLogWindowMenuItem(boolean enable) {
    menu.setEnabledLogWindow(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledLogWindow(enable);
    }
  }

  void setEnabledNewJoinMenuItem(boolean enable) {
    menu.setEnabledNewJoin(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewJoin(enable);
    }
  }

  void setEnabledNewGenericParallelMenuItem(boolean enable) {
    menu.setEnabledNewGenericParallel(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewGenericParallel(enable);
    }
  }

  void setEnabledNewAnisotropicDiffusionMenuItem(boolean enable) {
    menu.setEnabledNewAnisotropicDiffusion(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewAnisotropicDiffusion(enable);
    }
  }

  void setEnabledNewPeetMenuItem(boolean enable) {
    menu.setEnabledNewPeet(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewPeet(enable);
    }
  }

  void setEnabledNewSerialSectionsMenuItem(boolean enable) {
    menu.setEnabledNewSerialSections(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledNewSerialSections(enable);
    }
  }

  void repaint(AxisID axisID) {
    getFrame(axisID).repaint();
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String message, String title, AxisID axisID) {
    getFrame(axisID).openMessageDialog(manager, axisID, message, title);
  }

  void displayInfoMessage(BaseManager manager, String message, String title, AxisID axisID) {
    getFrame(axisID).openInfoMessageDialog(manager, axisID, message, title);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String message, String title) {
    getFrame(AxisID.ONLY).openMessageDialog(manager, AxisID.ONLY, message, title);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String[] message, String title, AxisID axisID) {
    getFrame(axisID).openMessageDialog(manager, axisID, message, title);
  }

  void displayErrorMessage(BaseManager manager, ProcessMessages processMessages,
      String title, AxisID axisID) {
    getFrame(axisID).openErrorMessageDialog(manager, axisID, processMessages, title);
  }

  void displayWarningMessage(BaseManager manager, ProcessMessages processMessages,
      String title, AxisID axisID) {
    getFrame(axisID).openWarningMessageDialog(manager, axisID, processMessages, title);
  }

  int displayYesNoCancelMessage(BaseManager manager, String message, AxisID axisID) {
    return getFrame(axisID).openYesNoCancelDialog(manager, axisID, message);
  }

  boolean displayYesNoMessage(BaseManager manager, String message, AxisID axisID) {
    return getFrame(axisID).openYesNoDialog(manager, axisID, message);
  }

  boolean displayDeleteMessage(BaseManager manager, String message[], AxisID axisID) {
    return getFrame(axisID).openDeleteDialog(manager, axisID, message);
  }

  boolean displayYesNoWarningDialog(BaseManager manager, String message, AxisID axisID) {
    return getFrame(axisID).openYesNoWarningDialog(manager, axisID, message);
  }

  boolean displayYesNoMessage(BaseManager manager, String[] message, AxisID axisID) {
    return getFrame(axisID).openYesNoDialog(manager, axisID, message);
  }

  /**
   * Open a file chooser to get an .edf, .ejf, .epp, or .epe file.
   * @return true if succeeded
   */
  private boolean getParamFilename() {
    // Open up the file chooser in current working directory
    File workingDir = new File(currentManager.getPropertyUserDir());
    JFileChooser chooser = new FileChooser(workingDir);
    DataFileFilter fileFilter = mainPanel.getDataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Save " + fileFilter.getDescription());
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    File[] edfFiles = workingDir.listFiles(fileFilter);
    if (edfFiles.length == 0) {
      String metaDataFileName = currentManager.getBaseMetaData().getMetaDataFileName();
      if (metaDataFileName != null) {
        chooser.setSelectedFile(new File(workingDir, metaDataFileName));
      }
    }
    int returnVal = chooser.showSaveDialog(this);

    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return false;
    }
    // If the file does not already have an extension appended then add the
    // extension
    File dataFile = chooser.getSelectedFile();
    String fileName = chooser.getSelectedFile().getName();
    if (fileName.indexOf(".") == -1) {
      dataFile = new File(chooser.getSelectedFile().getAbsolutePath()
          + currentManager.getBaseMetaData().getFileExtension());

    }
    return currentManager.setParamFile(dataFile);
  }

  void pack(AxisID axisID) {
    getFrame(axisID).pack();
  }

  void pack(AxisID axisID, boolean force) {
    getFrame(axisID).pack(force);
  }

  /**
   * Increase the bounds by one pixel before packing.  This preserves the
   * scrollbar when the window size doesn't change.
   */
  public void pack() {
    pack(false);
  }

  /**
   * Get the Etomo menus
   */
  private void getMenus() {
    menuBar = menu.getMenuBar();
    setJMenuBar(menuBar);
  }

  /**
   * Open a File Chooser dialog with an data file filter, if the user selects
   * or names a file return a File object wiht that slected, otherwise
   * return null.
   * @return A File object specifiying the selected file or null if none
   * was selected. 
   */
  private File openDataFileDialog() {
    // Open up the file chooser in current working directory
    JFileChooser chooser = new FileChooser(new File(System.getProperty("user.dir")));
    DataFileFilter fileFilter = new DataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Open " + fileFilter.getDescription());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return chooser.getSelectedFile();
    }
    return null;
  }

  AxisID getAxisID() {
    if (!main) {
      return AxisID.SECOND;
    }
    if (mainPanel == null) {
      return null;
    }
    if (mainPanel.getAxisType() == AxisType.SINGLE_AXIS || mainPanel.isShowingSetup()) {
      return AxisID.ONLY;
    }
    if (!mainPanel.isShowingAxisA()) {
      return AxisID.SECOND;
    }
    return AxisID.FIRST;
  }

  EtomoFrame getOtherFrame() {
    if (singleFrame) {
      return this;
    }
    if (main) {
      return subFrame;
    }
    return mainFrame;
  }

  /**
   * Used by single axis functions.  Gets the frame containing the axis.
   * @param axisID
   * @return
   */
  private EtomoFrame getFrame(AxisID axisID) {
    if (singleFrame) {
      return this;
    }
    if (mainFrame == null) {
      throw new NullPointerException("MainFrame instance was not registered.");
    }
    if (axisID != AxisID.SECOND) {
      return mainFrame;
    }
    if (mainPanel != null && mainPanel.isShowingBothAxis() && subFrame != null) {
      return subFrame;
    }
    return mainFrame;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 18:07:55  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.48  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.47  2010/02/05 00:47:31  sueh
 * <p> bug# 1309 Fixed an occasional null pointer exception in getFrame.
 * <p>
 * <p> Revision 1.46  2009/11/24 00:44:00  sueh
 * <p> bug# 1289 On New PEET, calling PeetManager.isInterfaceAvaiable before
 * <p> opening PEET interface.
 * <p>
 * <p> Revision 1.45  2009/11/20 17:04:12  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.  Added isMenuSaveEnabled to
 * <p> allow a save function to have the same limits as the save menu option.
 * <p>
 * <p> Revision 1.44  2009/10/23 19:45:53  sueh
 * <p> bug# 1275 Make separate menu items for generic parallel process and
 * <p> NAD.
 * <p>
 * <p> Revision 1.43  2009/04/02 19:16:58  sueh
 * <p> bug# 1206 Tomosnapshot no longer uses a process display.
 * <p>
 * <p> Revision 1.42  2009/03/16 23:51:36  sueh
 * <p> bug# 1186 In showOptionDialog pass the managerKey parameter to
 * <p> BaseManager.logMessage.  Add managerKey to every function which calls
 * <p> showOptionDialog.
 * <p>
 * <p> Revision 1.41  2009/02/04 23:31:59  sueh
 * <p> bug# 1158 Add a View pull down menu.
 * <p>
 * <p> Revision 1.40  2009/01/20 19:57:42  sueh
 * <p> bug# 1102 Changed  UITestAction to UITestActionType.
 * <p>
 * <p> Revision 1.39  2008/05/30 22:31:42  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.38  2008/05/30 21:28:16  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.37  2008/05/28 17:30:01  sueh
 * <p> bug# 1110 Handle a null pointer when calling super.pack().  Not much
 * <p> can be done about it and the pack isn't critical.  But it may show that the
 * <p> display is corrupt.  I don't have enough information to try to pop up a
 * <p> helpful message at this point.
 * <p>
 * <p> Revision 1.36  2008/05/03 00:49:39  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.35  2007/12/26 22:23:28  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.34  2007/09/07 00:26:41  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.33  2007/05/02 21:05:48  sueh
 * <p> bug# 964 Removed Import PRM and Duplicate PEET menu items.
 * <p>
 * <p> Revision 1.32  2007/03/31 03:00:24  sueh
 * <p> bug# 964 Added Duplicate Peet and Import .prm File menu items.
 * <p>
 * <p> Revision 1.31  2007/03/01 01:31:12  sueh
 * <p> bug# 964 Fixed bug in getParamFilename (Save As).  Tried to fill in a default
 * <p> selected file that might be null.
 * <p>
 * <p> Revision 1.30  2007/02/21 04:21:53  sueh
 * <p> bug# 964 Using UIHarness to pop up dialog.
 * <p>
 * <p> Revision 1.29  2007/02/19 22:01:10  sueh
 * <p> bug# 964 Handling new PEET event.
 * <p>
 * <p> Revision 1.28  2006/11/15 20:53:19  sueh
 * <p> bug# 872 In menuFileAction():  call currentManager.saveParamFile() instead of
 * <p> save().  SaveParamFile now calls save().
 * <p>
 * <p> Revision 1.27  2006/07/21 19:02:15  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 1.26  2006/06/21 15:52:31  sueh
 * <p> bug# 581 Using Imodqtassist instead of HTMLPageWindow() to pop up help.
 * <p>
 * <p> Revision 1.25  2006/04/25 19:15:04  sueh
 * <p> bug# 787 Implemented the OptionPane function showOptionDialog() so
 * <p> that the popups could be named and found by JfcUnit.
 * <p> NamedComponentFinder.  This replaces the currentPopupName
 * <p> functionality.
 * <p>
 * <p> Revision 1.24  2006/04/06 20:16:02  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.23  2006/03/20 18:02:15  sueh
 * <p> bug# 835 Added menu option to create a new ParallelManager.
 * <p>
 * <p> Revision 1.22  2006/01/20 21:09:53  sueh
 * <p> bug# 401 Added the ability for wrap(String, ArrayList) to wrap on commas.
 * <p>
 * <p> Revision 1.21  2006/01/12 17:09:54  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.20  2006/01/11 22:10:50  sueh
 * <p> bug# 675 Removed popupCounter and printTitle().  Using a name
 * <p> created from the title to refer to the popup.  Don't want to stop using
 * <p> static JOptionPane calls, so that name can't be attached to the popup
 * <p> dialog.  Instead I'm saving the name in currentPopupName while the
 * <p> popup dialog is active.  When --names is used, calling printName().
 * <p> Moved all JOptionPane calls to one function -
 * <p> showOptionPane(String[],String,int,int,Object[],Object,String[]).  This
 * <p> simplifies setting currentPopupName and printing the name.  Added two
 * <p> functions to help translate from the open...Dialog functions -
 * <p> showOptionConfirm() and showOptionPanel(String[],String,int)
 * <p>
 * <p> Revision 1.19  2006/01/04 20:25:33  sueh
 * <p> bug# 675 For printing the title when print name is on:  manipulating a popup
 * <p> requires two name/value pairs.  The two have to be linked by a number.
 * <p> Adding popupCounter, which counts the number of popups and give each a
 * <p> unique number.
 * <p>
 * <p> Revision 1.18  2006/01/03 23:37:37  sueh
 * <p> bug# 675 If print names is set in EtomoDirector, print the information
 * <p> needed to get and manipulate the dialog.
 * <p>
 * <p> Revision 1.17  2005/12/09 20:29:25  sueh
 * <p> bug# Added an info message popup
 * <p>
 * <p> Revision 1.16  2005/12/08 00:58:58  sueh
 * <p> bug# 504 Added displayYesNoWarningDialog() which displays a yes/no
 * <p> popup with No selected and a warning icon.
 * <p>
 * <p> Revision 1.15  2005/11/02 22:14:43  sueh
 * <p> bug# 754 Integrating ProcessMessages.  Added functions
 * <p> displayErrorMessage, displayWarningMessage, openErrorMessageDialog,
 * <p> openWarningMessageDialog, setupMessageArray, toStringArray,
 * <p> wrapError, and wrapWarning.
 * <p>
 * <p> Revision 1.14  2005/10/31 17:54:32  sueh
 * <p> bug# 730 Pop up error message if Save fails and cannot do Save As.
 * <p> Prevent Save As functionality in Save when Save As is disabled.
 * <p>
 * <p> Revision 1.13  2005/09/29 18:48:00  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 1.12  2005/08/11 23:48:28  sueh
 * <p> bug# 711  Add is and set functions from menu3dmodStartupWindow and
 * <p> menu3dmodBinBy2.  In menuOptionsAction() handle the coordination of
 * <p> the two frame menus.
 * <p>
 * <p> Revision 1.11  2005/08/04 20:09:36  sueh
 * <p> bug# 532 renaming fitWindow to pack.  Getting dialogs to size by calling
 * <p> UIHarness.pack instead of EtomoFrame.pack
 * <p>
 * <p> Revision 1.10  2005/07/29 00:54:00  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.9  2005/07/18 17:54:48  sueh
 * <p> bug# 697 In getAxisID() handle null mainPanel.
 * <p>
 * <p> Revision 1.8  2005/07/01 21:13:17  sueh
 * <p> bug# 619 temporary changes to pass the frame to the demo JDialog.
 * <p>
 * <p> Revision 1.7  2005/05/18 22:46:55  sueh
 * <p> bug# 662 Changed high-level open message dialog function names from
 * <p> open...Dialog (the same as the low level functions()) to
 * <p> display...Message.  This made it easier to distiguish between them and
 * <p> create more options.  Added a displayMessage() function which defaulted
 * <p> to AxisID.ONLY.  Added a delete dialog with two options, Delete and No.
 * <p> The delete dialog is safer then the Yes/No dialog because it doesn't return
 * <p> yes as its default option.  To do this, I added displayDeleteMessage() and
 * <p> optionDeleteDialog().  Changed wrap(String, ArrayList) to handle lines
 * <p> that are empty except for a "\n" character.
 * <p>
 * <p> Revision 1.6  2005/05/10 17:01:41  sueh
 * <p> bug# 660 Fixed wrap(String, ArrayList).  It now breaks up message
 * <p> strings by "\n" before breaking them up because they are too long.  This
 * <p> prevents unnecessary wrapping.
 * <p>
 * <p> Revision 1.5  2005/04/27 02:14:42  sueh
 * <p> bug# 615 Drop createMenus() to private, since it is no longer used by
 * <p> EtomoDirector.  Increase the level of getOtherFrame() to protected, since
 * <p> it is being used by SubFrame.
 * <p>
 * <p> Revision 1.4  2005/04/26 17:38:25  sueh
 * <p> bug# 615 Made MainFrame, SubFrame, and EtomoFrame package-level
 * <p> classes.  All MainFrame functionality is handled through UIHarness to
 * <p> make Etomo more compatible with JUnit.  Fixed functions access levels.
 * <p>
 * <p> Revision 1.3  2005/04/25 21:04:10  sueh
 * <p> bug# 615 Moving message dialog functions, menu appearance functions,
 * <p> and fitting and repainting functions from mainPanel to this class.  Moved
 * <p> menu management and the coordination of the two frames from
 * <p> EtomoMenu to this class.  Added EtomoFrame static instance variables
 * <p> for an instance of the MainFrame and an instance of the SubFrame.
 * <p> Added an abstract register() function to initialize the static instance
 * <p> variables.
 * <p>
 * <p> Revision 1.2  2005/04/21 20:33:22  sueh
 * <p> bug# 615 Make EtomoFrame is a class.  It now handles the menu action
 * <p> functions common to MainFrame and SubFrame.
 * <p>
 * <p> Revision 1.1  2005/04/20 01:37:15  sueh
 * <p> bug# 615 Added a interface for MainFrame and SubFrame to they can
 * <p> work with the same menu class.
 * <p> </p>
 */
