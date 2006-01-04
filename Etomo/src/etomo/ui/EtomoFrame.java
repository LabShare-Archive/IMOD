package etomo.ui;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;

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
//TEMP should be package level
abstract class EtomoFrame extends JFrame {
  public static final String rcsid = "$Id$";

  private static final int messageWidth = 60;
  private static final int maxMessageLines = 20;
  private static final boolean printNames = EtomoDirector.getInstance()
      .isPrintNames();
  private static final String ETOMO_QUESTION = "Etomo question";
  private static final String YES = "Yes";
  private static final String NO = "No";
  private static final String OK = "OK";

  private long popupCounter = 0;
  protected boolean main;
  protected EtomoMenu menu;
  protected JMenuBar menuBar;
  protected MainPanel mainPanel;
  protected BaseManager currentManager;

  protected static EtomoFrame mainFrame = null;
  protected static EtomoFrame subFrame = null;

  protected abstract void register();

  protected void initialize() {
    menu = new EtomoMenu();
    ImageIcon iconEtomo = new ImageIcon(ClassLoader
        .getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());
    createMenus();
  }

  final boolean isMenu3dmodStartupWindow() {
    return menu.isMenu3dmodStartupWindow();
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

  /**
   * Handle File menu actions
   * @param event
   */
  void menuFileAction(ActionEvent event) {
    AxisID axisID = getAxisID();
    if (menu.equalsFileNewTomogram(event)) {
      EtomoDirector.getInstance().openTomogram(true, axisID);
    }

    if (menu.equalsFileNewJoin(event)) {
      EtomoDirector.getInstance().openJoin(true, axisID);
    }

    if (menu.equalsFileOpen(event)) {
      File dataFile = openDataFileDialog();
      if (dataFile != null) {
        EtomoDirector.getInstance().openManager(dataFile, true, axisID);
      }
    }

    if (menu.equalsFileSave(event)) {
      if (currentManager.save(axisID)) {
        return;
      }
      //Don't allow the user to do the equivalent of a Save As without checking.
      if (!currentManager.canChangeParamFileName()) {
        openMessageDialog(
            "Please set the name of dataset or the join before saving",
            "Cannot Save");
        return;
      }
      //Do a Save As
      if (getTestParamFilename()) {
        currentManager.save(axisID);
      }
    }

    if (menu.equalsFileSaveAs(event)) {
      if (getTestParamFilename()) {
        currentManager.save(axisID);
      }
    }

    if (menu.equalsFileClose(event)) {
      EtomoDirector.getInstance().closeCurrentManager(axisID);
    }

    if (menu.equalsFileExit(event)) {
      //  Check to see if we need to save any data
      if (EtomoDirector.getInstance().exitProgram(axisID)) {
        System.exit(0);
      }
    }

    if (menu.equalsFileTomosnapshot(event)) {
      currentManager.tomosnapshot(axisID);
    }
  }

  /**
   * Open the specified MRU EDF file
   * @param event
   */
  void menuFileMRUListAction(ActionEvent event) {
    EtomoDirector.getInstance().openManager(new File(event.getActionCommand()),
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
      imodURL = EtomoDirector.getInstance().getIMODDirectory().toURL()
          .toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.getInstance().getIMODDirectory()
          .toString());
      return;
    }

    if (menu.equalsTomoGuide(event)) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomoguide.html");
      manpage.setVisible(true);
    }

    if (menu.equalsImodGuide(event)) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);
    }

    if (menu.equals3dmodGuide(event)) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "3dmodguide.html");
      manpage.setVisible(true);
    }

    if (menu.equalsEtomoGuide(event)) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "UsingEtomo.html");
      manpage.setVisible(true);
    }

    if (menu.equalsJoinGuide(event)) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomojoin.html");
      manpage.setVisible(true);
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
   * Handle some of the options menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  protected void menuOptionsAction(ActionEvent event) {
    if (menu.equalsSettings(event)) {
      EtomoDirector.getInstance().openSettingsDialog();
    }
    //Run fitWindow on both frames.
    else if (menu.equalsFitWindow(event)) {
      UIHarness.INSTANCE.pack(true, currentManager);
      if (getOtherFrame() != null) {
        UIHarness.INSTANCE.pack(AxisID.SECOND, true, currentManager);
      }
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
  protected void setEnabled(BaseManager currentManager) {
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
  protected void setMRUFileLabels(String[] mRUList) {
    menu.setMRUFileLabels(mRUList);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setMRUFileLabels(mRUList);
    }
  }

  void setEnabledNewTomogramMenuItem(boolean enable) {
    menu.setEnabledFileNewTomogram(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledFileNewTomogram(enable);
    }
  }

  void setEnabledNewJoinMenuItem(boolean enable) {
    menu.setEnabledFileNewJoin(enable);
    EtomoFrame otherFrame = getOtherFrame();
    if (otherFrame != null) {
      getOtherFrame().menu.setEnabledFileNewJoin(enable);
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
  void displayMessage(String message, String title, AxisID axisID) {
    getFrame(axisID).openMessageDialog(message, title);
  }

  void displayInfoMessage(String message, String title, AxisID axisID) {
    getFrame(axisID).openInfoMessageDialog(message, title);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String message, String title) {
    getFrame(AxisID.ONLY).openMessageDialog(message, title);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String[] message, String title, AxisID axisID) {
    getFrame(axisID).openMessageDialog(message, title);
  }

  void displayErrorMessage(ProcessMessages processMessages, String title,
      AxisID axisID) {
    getFrame(axisID).openErrorMessageDialog(processMessages, title);
  }

  void displayWarningMessage(ProcessMessages processMessages, String title,
      AxisID axisID) {
    getFrame(axisID).openWarningMessageDialog(processMessages, title);
  }

  int displayYesNoCancelMessage(String[] message, AxisID axisID) {
    return getFrame(axisID).openYesNoCancelDialog(message);
  }

  boolean displayYesNoMessage(String message, AxisID axisID) {
    return getFrame(axisID).openYesNoDialog(message);
  }

  boolean displayDeleteMessage(String message[], AxisID axisID) {
    return getFrame(axisID).openDeleteDialog(message);
  }

  boolean displayYesNoWarningDialog(String message, AxisID axisID) {
    return getFrame(axisID).openYesNoWarningDialog(message);
  }

  boolean displayYesNoMessage(String[] message, AxisID axisID) {
    return getFrame(axisID).openYesNoDialog(message);
  }

  /**
   * Open a message dialog with a wrapped message with the dataset appended.
   * @param message
   * @param title
   */
  private void openMessageDialog(String message, String title) {
    if (printNames) {
      printTitle(title, OK, null);
    }
    JOptionPane.showMessageDialog(this, wrap(message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  private void openInfoMessageDialog(String message, String title) {
    if (printNames) {
      printTitle(title, OK, null);
    }
    JOptionPane.showMessageDialog(this, wrap(message), title,
        JOptionPane.INFORMATION_MESSAGE);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  private void openMessageDialog(String[] message, String title) {
    if (printNames) {
      printTitle(title, OK, null);
    }
    JOptionPane.showMessageDialog(this, wrap(message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  private void openErrorMessageDialog(ProcessMessages processMessages,
      String title) {
    if (printNames) {
      printTitle(title, OK, null);
    }
    JOptionPane.showMessageDialog(this, wrapError(processMessages), title,
        JOptionPane.ERROR_MESSAGE);
  }

  private void openWarningMessageDialog(ProcessMessages processMessages,
      String title) {
    if (printNames) {
      printTitle(title, OK, null);
    }
    JOptionPane.showMessageDialog(this, wrapWarning(processMessages), title,
        JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Open a Yes, No or Cancel question dialog
   * @param message
   * @return int state of the users select
   */
  private int openYesNoCancelDialog(String[] message) {
    if (printNames) {
      printTitle(ETOMO_QUESTION, YES, NO);
    }
    return JOptionPane.showConfirmDialog(this, wrap(message), ETOMO_QUESTION,
        JOptionPane.YES_NO_CANCEL_OPTION);
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String message) {
    if (printNames) {
      printTitle(ETOMO_QUESTION, YES, NO);
    }
    int result = JOptionPane.showConfirmDialog(this, wrap(message),
        ETOMO_QUESTION, JOptionPane.YES_NO_OPTION);
    return result == JOptionPane.YES_OPTION;
  }

  private final void printTitle(String title, String option1, String option2) {
    System.out.println("popup" + AutodocTokenizer.SEPARATOR_CHAR
        + String.valueOf(popupCounter) + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
        + ' ' + title);
    StringBuffer buffer = new StringBuffer("popup"
        + AutodocTokenizer.SEPARATOR_CHAR + String.valueOf(popupCounter++) + ' '
        + AutodocTokenizer.DEFAULT_DELIMITER + ' ' + option1);
    if (option2 != null) {
      buffer.append(',' + option2);
    }
    System.out.println(buffer);
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openDeleteDialog(String[] message) {
    String[] results = new String[] { "Delete", "No" };
    String title = "Delete File?";
    if (printNames) {
      printTitle(title, results[0], results[1]);
    }
    int result = JOptionPane.showOptionDialog(this, wrap(message), title,
        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null,
        results, null);
    return result == 0;
  }

  private boolean openYesNoWarningDialog(String message) {
    String[] results = new String[] { "Yes", "No" };
    String title = "Etomo Warning";
    if (printNames) {
      printTitle(title, results[0], results[1]);
    }
    int result = JOptionPane.showOptionDialog(this, wrap(message), title,
        JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null, results,
        "No");
    return result == 0;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String[] message) {
    if (printNames) {
      printTitle(ETOMO_QUESTION, YES, NO);
    }
    int result = JOptionPane.showConfirmDialog(this, wrap(message),
        ETOMO_QUESTION, JOptionPane.YES_NO_OPTION);
    return result == JOptionPane.YES_OPTION;
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
      messageArray.add(currentManager.getBaseMetaData().getName() + ":");
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
          messageIndex = endIndex;
          while (messageIndex < messageLength
              && messagePieceArray[i].substring(messageIndex, messageIndex + 1)
                  .matches("\\S+")) {
            newLine.append(messagePieceArray[i].charAt(messageIndex++));
          }
          messageArray.add(newLine.toString());
        }
      }
    }
    return messageArray;
  }

  /**
   * Open a file chooser to get an .edf or .ejf file.
   * @return
   */
  private boolean getTestParamFilename() {
    //  Open up the file chooser in current working directory
    File workingDir = new File(currentManager.getPropertyUserDir());
    JFileChooser chooser = new JFileChooser(workingDir);
    DataFileFilter fileFilter = mainPanel.getDataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Save " + fileFilter.getDescription());
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    File[] edfFiles = workingDir.listFiles(fileFilter);
    if (edfFiles.length == 0) {
      File defaultFile = new File(workingDir, currentManager.getBaseMetaData()
          .getMetaDataFileName());
      chooser.setSelectedFile(defaultFile);
    }
    int returnVal = chooser.showSaveDialog(this);

    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return false;
    }
    // If the file does not already have an extension appended then add an edf
    // extension
    File dataFile = chooser.getSelectedFile();
    String fileName = chooser.getSelectedFile().getName();
    if (fileName.indexOf(".") == -1) {
      dataFile = new File(chooser.getSelectedFile().getAbsolutePath()
          + currentManager.getBaseMetaData().getFileExtension());

    }
    currentManager.setTestParamFile(dataFile);
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
    if (!force
        && !EtomoDirector.getInstance().getUserConfiguration().isAutoFit()) {
      setVisible(true);
    }
    else {
      Rectangle bounds = getBounds();
      bounds.height++;
      bounds.width++;
      setBounds(bounds);
      super.pack();
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

  private AxisID getAxisID() {
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

  protected EtomoFrame getOtherFrame() {
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