package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.ManagerKey;
import etomo.process.ImodqtassistProcess;
import etomo.process.ProcessMessages;
import etomo.storage.DataFileFilter;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.UITestActionType;
import etomo.type.UITestSubjectType;
import etomo.util.Utilities;

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
abstract class EtomoFrame extends JFrame {
  public static final String rcsid = "$Id$";

  private static final int messageWidth = 60;
  private static final int maxMessageLines = 20;
  private static final boolean printNames = EtomoDirector.INSTANCE
      .getArguments().isPrintNames();
  private static final String ETOMO_QUESTION = "Etomo question";
  private static final String YES = "Yes";
  private static final String NO = "No";
  private static final String CANCEL = "Cancel";
  private static final String OK = "OK";

  private boolean verbose = false;
  boolean main;
  EtomoMenu menu;
  JMenuBar menuBar;
  MainPanel mainPanel;
  BaseManager currentManager;

  static EtomoFrame mainFrame = null;
  static EtomoFrame subFrame = null;

  abstract void register();

  void initialize() {
    menu = new EtomoMenu();
    ImageIcon iconEtomo = new ImageIcon(ClassLoader
        .getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());
    createMenus();
  }

  void moveSubFrame() {
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

  final void setVerbose(boolean verbose) {
    this.verbose = verbose;
  }

  final void setMenu3dmodStartupWindow(boolean menu3dmodStartupWindow) {
    menu.setMenu3dmodStartupWindow(menu3dmodStartupWindow);
  }

  final void setMenu3dmodBinBy2(boolean menu3dmodBinBy2) {
    menu.setMenu3dmodBinBy2(menu3dmodBinBy2);
  }

  /**
   * Handle File menu actions
   * @param event
   */
  void menuFileAction(ActionEvent event) {
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
      EtomoDirector.INSTANCE.openPeet(true, axisID);
    }
    else if (menu.equalsOpen(event)) {
      File dataFile = openDataFileDialog();
      if (dataFile != null) {
        EtomoDirector.INSTANCE.openManager(dataFile, true, axisID);
      }
    }
    else if (menu.equalsSave(event)) {
      save(axisID);
    }
    else if (menu.equalsSaveAs(event)) {
      saveAs(axisID);
    }
    else if (menu.equalsClose(event)) {
      EtomoDirector.INSTANCE.closeCurrentManager(axisID, false);
    }
    else if (menu.equalsExit(event)) {
      UIHarness.INSTANCE.exit(axisID);
    }
    else if (menu.equalsTomosnapshot(event)) {
      currentManager.tomosnapshot(axisID);
    }
  }

  void save(AxisID axisID) {
    ManagerKey managerKey = currentManager.getManagerKey();
    try {
      if (currentManager.saveParamFile()) {
        return;
      }
      //Don't allow the user to do the equivalent of a Save As if Save As isn't available.
      if (!currentManager.canChangeParamFileName()) {
        UIHarness.INSTANCE.openMessageDialog(
            "Please set the name of dataset or the join before saving",
            "Cannot Save", managerKey);
        return;
      }
      //Do a Save As
      if (getParamFilename()) {
        currentManager.saveParamFile();
      }
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to write parameters.\n"
          + e.getMessage(), "Etomo Error", axisID, managerKey);
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to write parameters.\n"
          + e.getMessage(), "Etomo Error", axisID, managerKey);
    }
  }

  private void saveAs(AxisID axisID) {
    ManagerKey managerKey = currentManager.getManagerKey();
    try {
      if (getParamFilename()) {
        currentManager.saveParamFile();
      }
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to save parameters.\n"
          + e.getMessage(), "Etomo Error", axisID, managerKey);
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to save parameters.\n"
          + e.getMessage(), "Etomo Error", axisID, managerKey);
    }
  }

  /**
   * Open the specified MRU EDF file
   * @param event
   */
  void menuFileMRUListAction(ActionEvent event) {
    EtomoDirector.INSTANCE.openManager(new File(event.getActionCommand()),
        true, getAxisID());
  }

  /**
   * Handle help menu actions
   * @param event
   */
  void menuHelpAction(ActionEvent event) {
    AxisID axisID = getAxisID();
    // Get the URL to the IMOD html directory
    String imodURL = "";
    try {
      imodURL = EtomoDirector.INSTANCE.getIMODDirectory().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.INSTANCE.getIMODDirectory().toString());
      return;
    }

    if (menu.equalsTomoGuide(event)) {
      //TODO
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       manpage.openURL(imodURL + "tomoguide.html");
       manpage.setVisible(true);*/
      ImodqtassistProcess.INSTANCE.open(currentManager, "tomoguide.html",
          getAxisID());
    }

    if (menu.equalsImodGuide(event)) {
      ImodqtassistProcess.INSTANCE.open(currentManager, "guide.html",
          getAxisID());
    }

    if (menu.equals3dmodGuide(event)) {
      ImodqtassistProcess.INSTANCE.open(currentManager, "3dmodguide.html",
          getAxisID());
    }

    if (menu.equalsEtomoGuide(event)) {
      ImodqtassistProcess.INSTANCE.open(currentManager, "UsingEtomo.html",
          getAxisID());
    }

    if (menu.equalsJoinGuide(event)) {
      ImodqtassistProcess.INSTANCE.open(currentManager, "tomojoin.html",
          getAxisID());
    }

    if (menu.equalsHelpAbout(event)) {
      MainFrame_AboutBox dlg = new MainFrame_AboutBox(currentManager, this,
          axisID);
      Dimension dlgSize = dlg.getPreferredSize();
      Dimension frmSize = getSize();
      Point loc = getLocation();
      dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x,
          (frmSize.height - dlgSize.height) / 2 + loc.y);
      dlg.setModal(true);
      dlg.setVisible(true);
    }
  }

  /**
   * Handle some of the view menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  void menuViewAction(ActionEvent event) {
    //Run fitWindow on both frames.
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
  void menuOptionsAction(ActionEvent event) {
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

  void repaint(AxisID axisID) {
    getFrame(axisID).repaint();
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String message, String title, AxisID axisID,
      ManagerKey managerKey) {
    getFrame(axisID).openMessageDialog(message, title, managerKey);
  }

  void displayInfoMessage(String message, String title, AxisID axisID,
      ManagerKey managerKey) {
    getFrame(axisID).openInfoMessageDialog(message, title, managerKey);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String message, String title, ManagerKey managerKey) {
    getFrame(AxisID.ONLY).openMessageDialog(message, title, managerKey);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String[] message, String title, AxisID axisID,
      ManagerKey managerKey) {
    getFrame(axisID).openMessageDialog(message, title, managerKey);
  }

  void displayErrorMessage(ProcessMessages processMessages, String title,
      AxisID axisID, ManagerKey managerKey) {
    getFrame(axisID).openErrorMessageDialog(processMessages, title, managerKey);
  }

  void displayWarningMessage(ProcessMessages processMessages, String title,
      AxisID axisID, ManagerKey managerKey) {
    getFrame(axisID).openWarningMessageDialog(processMessages, title,
        managerKey);
  }

  int displayYesNoCancelMessage(String[] message, AxisID axisID,
      ManagerKey managerKey) {
    return getFrame(axisID).openYesNoCancelDialog(message, managerKey);
  }

  boolean displayYesNoMessage(String message, AxisID axisID,
      ManagerKey managerKey) {
    return getFrame(axisID).openYesNoDialog(message, managerKey);
  }

  boolean displayDeleteMessage(String message[], AxisID axisID,
      ManagerKey managerKey) {
    return getFrame(axisID).openDeleteDialog(message, managerKey);
  }

  boolean displayYesNoWarningDialog(String message, AxisID axisID,
      ManagerKey managerKey) {
    return getFrame(axisID).openYesNoWarningDialog(message, managerKey);
  }

  boolean displayYesNoMessage(String[] message, AxisID axisID,
      ManagerKey managerKey) {
    return getFrame(axisID).openYesNoDialog(message, managerKey);
  }

  /**
   * Open a message dialog with a wrapped message with the dataset appended.
   * @param message
   * @param title
   */
  private void openMessageDialog(String message, String title,
      ManagerKey managerKey) {
    showOptionPane(wrap(message), title, JOptionPane.ERROR_MESSAGE, managerKey);
  }

  private void openInfoMessageDialog(String message, String title,
      ManagerKey managerKey) {
    showOptionPane(wrap(message), title, JOptionPane.INFORMATION_MESSAGE,
        managerKey);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  private void openMessageDialog(String[] message, String title,
      ManagerKey managerKey) {
    showOptionPane(wrap(message), title, JOptionPane.ERROR_MESSAGE, managerKey);
  }

  private void openErrorMessageDialog(ProcessMessages processMessages,
      String title, ManagerKey managerKey) {
    showOptionPane(wrapError(processMessages), title,
        JOptionPane.ERROR_MESSAGE, managerKey);
  }

  private void openWarningMessageDialog(ProcessMessages processMessages,
      String title, ManagerKey managerKey) {
    showOptionPane(wrapWarning(processMessages), title,
        JOptionPane.ERROR_MESSAGE, managerKey);
  }

  /**
   * Open a Yes, No or Cancel question dialog
   * @param message
   * @return int state of the users select
   */
  private int openYesNoCancelDialog(String[] message, ManagerKey managerKey) {
    return showOptionConfirmPane(wrap(message), ETOMO_QUESTION,
        JOptionPane.YES_NO_CANCEL_OPTION, new String[] { YES, NO, CANCEL },
        managerKey);
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String message, ManagerKey managerKey) {
    int result = showOptionConfirmPane(wrap(message), ETOMO_QUESTION,
        JOptionPane.YES_NO_OPTION, new String[] { YES, NO }, managerKey);
    return result == JOptionPane.YES_OPTION;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openDeleteDialog(String[] message, ManagerKey managerKey) {
    String[] results = new String[] { "Delete", "No" };
    int result = showOptionPane(wrap(message), "Delete File?",
        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, results,
        null, results, managerKey);
    return result == 0;
  }

  private boolean openYesNoWarningDialog(String message, ManagerKey managerKey) {
    String[] results = new String[] { "Yes", "No" };
    int result = showOptionPane(wrap(message), "Etomo Warning",
        JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, results,
        results[1], results, managerKey);
    return result == 0;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String[] message, ManagerKey managerKey) {
    int result = showOptionConfirmPane(wrap(message), ETOMO_QUESTION,
        JOptionPane.YES_NO_OPTION, new String[] { YES, NO }, managerKey);
    return result == JOptionPane.YES_OPTION;
  }

  private int showOptionConfirmPane(String[] message, String title,
      int optionType, String[] optionStrings, ManagerKey managerKey) {
    return showOptionPane(message, title, optionType,
        JOptionPane.QUESTION_MESSAGE, null, null, optionStrings, managerKey);
  }

  private void showOptionPane(String[] message, String title, int messageType,
      ManagerKey managerKey) {
    showOptionPane(message, title, JOptionPane.DEFAULT_OPTION, messageType,
        null, null, new String[] { OK }, managerKey);
  }

  private int showOptionPane(String[] message, String title, int optionType,
      int messageType, Object[] options, Object initialValue,
      String[] optionStrings, ManagerKey managerKey) {
    int result = showOptionDialog(this, message, title, optionType,
        messageType, null, options, initialValue, optionStrings, managerKey);
    return result;
  }

  /**
   * Shows all pop up message dialogs.  Pass in in BaseManager so that the
   * @param parentComponent
   * @param message
   * @param title
   * @param optionType
   * @param messageType
   * @param icon
   * @param options
   * @param initialValue
   * @param optionStrings
   * @param manager
   * @return
   * @throws HeadlessException
   */
  private int showOptionDialog(Component parentComponent, String[] message,
      String title, int optionType, int messageType, Icon icon,
      Object[] options, Object initialValue, String[] optionStrings,
      ManagerKey managerKey) throws HeadlessException {
    if (currentManager != null) {
      currentManager.logMessage(message, title, getAxisID(), managerKey);
    }
    JOptionPane pane = new JOptionPane(message, messageType, optionType, icon,
        options, initialValue);

    pane.setInitialValue(initialValue);
    pane.setComponentOrientation(((parentComponent == null) ? JOptionPane
        .getRootFrame() : parentComponent).getComponentOrientation());

    JDialog dialog = pane.createDialog(parentComponent, title);

    pane.selectInitialValue();
    String name = Utilities.convertLabelToName(title);
    pane.setName(name);
    printName(name, optionStrings, title, message);
    dialog.setVisible(true);
    dialog.dispose();

    Object selectedValue = pane.getValue();

    if (selectedValue == null)
      return JOptionPane.CLOSED_OPTION;
    if (options == null) {
      if (selectedValue instanceof Integer)
        return ((Integer) selectedValue).intValue();
      return JOptionPane.CLOSED_OPTION;
    }
    for (int counter = 0, maxCounter = options.length; counter < maxCounter; counter++) {
      if (options[counter].equals(selectedValue))
        return counter;
    }
    return JOptionPane.CLOSED_OPTION;
  }

  private synchronized final void printName(String name,
      String[] optionStrings, String title, String[] message) {
    if (printNames) {
      //print waitfor popup name/value pair
      StringBuffer buffer = new StringBuffer(UITestActionType.WAIT.toString()
          + AutodocTokenizer.SEPARATOR_CHAR
          + UITestSubjectType.POPUP.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
      //if there are options, then print a popup name/value pair
      if (optionStrings != null && optionStrings.length > 0) {
        buffer.append(optionStrings[0]);
        for (int i = 1; i < optionStrings.length; i++) {
          buffer.append(',' + optionStrings[i]);
        }
        System.out.println(buffer);
      }
    }
    if (verbose) {
      //if verbose then print the popup title and message
      System.err.println("Popup:");
      System.err.println(title);
      if (message != null) {
        for (int i = 0; i < message.length; i++) {
          System.err.println(message[i]);
        }
      }
    }
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(String message) {
    ArrayList messageArray = setupMessageArray();
    messageArray = wrap(message, messageArray);
    return toStringArray(messageArray);
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(String[] message) {
    ArrayList messageArray = setupMessageArray();
    for (int i = 0; i < message.length; i++) {
      messageArray = wrap(message[i], messageArray);
    }
    return toStringArray(messageArray);
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private final String[] wrapError(ProcessMessages processMessages) {
    ArrayList messageArray = setupMessageArray();
    for (int i = 0; i < processMessages.errorListSize(); i++) {
      messageArray = wrap(processMessages.getError(i), messageArray);
    }
    return toStringArray(messageArray);
  }

  private final String[] wrapWarning(ProcessMessages processMessages) {
    ArrayList messageArray = setupMessageArray();
    for (int i = 0; i < processMessages.warningListSize(); i++) {
      messageArray = wrap(processMessages.getWarning(i), messageArray);
    }
    return toStringArray(messageArray);
  }

  private final ArrayList setupMessageArray() {
    ArrayList messageArray = new ArrayList();
    if (currentManager != null) {
      BaseMetaData metaData = currentManager.getBaseMetaData();
      if (metaData != null) {
        messageArray.add(currentManager.getName() + ":");
      }
    }
    return messageArray;
  }

  private final String[] toStringArray(ArrayList arrayList) {
    if (arrayList.size() == 1) {
      String[] returnArray = { (String) arrayList.get(0) };
      return returnArray;
    }
    return (String[]) arrayList.toArray(new String[arrayList.size()]);
  }

  /**
   * wrap the message and place it in messageArray
   * @param messagePiece
   * @param messageArray
   * @return messageArray
   */
  private ArrayList wrap(String messagePiece, ArrayList messageArray) {
    if (messagePiece == null) {
      if (messageArray.size() == 0) {
        messageArray.add(" ");
      }
      return messageArray;
    }
    if (messagePiece.equals("\n")) {
      messageArray.add(" ");
      return messageArray;
    }
    //first - break up the message piece by line
    String[] messagePieceArray = messagePiece.split("\n");
    //second - break up each line by maximum length
    for (int i = 0; i < messagePieceArray.length; i++) {
      //handle empty lines
      if (messagePieceArray[i] == null || messagePieceArray[i].length() == 0) {
        messageArray.add(" ");
      }
      else {
        int messageLength = messagePieceArray[i].length();
        int messageIndex = 0;
        while (messageIndex < messageLength
            && messageArray.size() < maxMessageLines) {
          int endIndex = Math.min(messageLength, messageIndex + messageWidth);
          StringBuffer newLine = new StringBuffer(messagePieceArray[i]
              .substring(messageIndex, endIndex));
          //overflowing line - look for whitespace or a comma
          messageIndex = endIndex;
          char lastChar = ' ';
          while (messageIndex < messageLength
              && messagePieceArray[i].substring(messageIndex, messageIndex + 1)
                  .matches("\\S+") && lastChar != ',') {
            lastChar = messagePieceArray[i].charAt(messageIndex++);
            newLine.append(lastChar);
          }
          messageArray.add(newLine.toString());
        }
      }
    }
    return messageArray;
  }

  /**
   * Open a file chooser to get an .edf, .ejf, .epp, or .epe file.
   * @return true if succeeded
   */
  private boolean getParamFilename() {
    //  Open up the file chooser in current working directory
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
      String metaDataFileName = currentManager.getBaseMetaData()
          .getMetaDataFileName();
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
    currentManager.setParamFile(dataFile);
    return true;
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

  void pack(boolean force) {
    if (!force && !EtomoDirector.INSTANCE.getUserConfiguration().isAutoFit()) {
      setVisible(true);
    }
    else {
      Rectangle bounds = getBounds();
      bounds.height++;
      bounds.width++;
      setBounds(bounds);
      try {
        super.pack();
      }
      catch (NullPointerException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * Create the Etomo menus
   */
  private void createMenus() {
    menu.createMenus(this);
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
    //  Open up the file chooser in current working directory
    JFileChooser chooser = new FileChooser(new File(System
        .getProperty("user.dir")));
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
    if (mainPanel.getAxisType() == AxisType.SINGLE_AXIS
        || mainPanel.isShowingSetup()) {
      return AxisID.ONLY;
    }
    if (!mainPanel.isShowingAxisA()) {
      return AxisID.SECOND;
    }
    return AxisID.FIRST;
  }

  EtomoFrame getOtherFrame() {
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
    if (mainFrame == null) {
      throw new NullPointerException("MainFrame instance was not registered.");
    }
    if (axisID != AxisID.SECOND) {
      return mainFrame;
    }
    if (mainPanel.isShowingBothAxis() && subFrame != null) {
      return subFrame;
    }
    return mainFrame;
  }

}
/**
 * <p> $Log$
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
