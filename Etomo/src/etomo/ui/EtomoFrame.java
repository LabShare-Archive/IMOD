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
abstract class EtomoFrame extends JFrame {
  public static  final String  rcsid =  "$Id$";
  
  private static final int messageWidth = 60;
  private static final int maxMessageLines = 20;

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
  
  /**
   * Handle File menu actions
   * @param event
   */
  void menuFileAction(ActionEvent event) {
    AxisID axisID = getAxisID();
    if (event.getActionCommand().equals(menu.getActionCommandFileNewTomogram())) {
      EtomoDirector.getInstance().openTomogram(true, axisID);
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileNewJoin())) {
      EtomoDirector.getInstance().openJoin(true, axisID);
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileOpen())) {
      File dataFile = openDataFileDialog();
      if (dataFile != null) {
        EtomoDirector.getInstance().openManager(dataFile, true, axisID);
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileSave())) {
      //  Check to see if there is a current parameter file chosen
      //  if not open a dialog box to select the name
      boolean haveTestParamFilename = true;
      if (currentManager.getTestParamFile() == null) {
        haveTestParamFilename = getTestParamFilename();
      }
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile(axisID);
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileSaveAs())) {
      boolean haveTestParamFilename = getTestParamFilename();
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile(axisID);
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileClose())) {
      EtomoDirector.getInstance().closeCurrentManager(axisID);
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileExit())) {
      //  Check to see if we need to save any data
      if (EtomoDirector.getInstance().exitProgram(axisID)) {
        System.exit(0);
      }
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
      imodURL = EtomoDirector.getInstance().getIMODDirectory().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.getInstance().getIMODDirectory().toString());
      return;
    }

    if (event.getActionCommand().equals(menu.getActionCommandTomoGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomoguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandImodGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommand3dmodGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "3dmodguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandEtomoGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "UsingEtomo.html");
      manpage.setVisible(true);
    }
    
    if (event.getActionCommand().equals(menu.getActionCommandJoinGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomojoin.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandHelpAbout())) {
      MainFrame_AboutBox dlg = new MainFrame_AboutBox(this, axisID);
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
    String command = event.getActionCommand();
    if (command.equals(menu.getActionCommandSettings())) {
      EtomoDirector.getInstance().openSettingsDialog();
    }
    //Run fitWindow on both frames.
    else if (command.equals(menu.getActionCommandFitWindow())) {
      fitWindow(true);
      EtomoFrame frame = getOtherFrame();
      if (frame != null) {
        frame.fitWindow();
      }
    }
    else {
      throw new IllegalStateException("Cannot handled menu command in this class.  command="+command);
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
  
  void fitWindow(AxisID axisID) {
    getFrame(axisID).fitWindow();
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(String message, String title, AxisID axisID) {
    getFrame(axisID).openMessageDialog(message, title);
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
  
  int displayYesNoCancelMessage(String[] message, AxisID axisID) {
    return getFrame(axisID).openYesNoCancelDialog(message);
  }
  
  boolean displayYesNoMessage(String message, AxisID axisID) {
    return getFrame(axisID).openYesNoDialog(message);
  }
  
  boolean displayDeleteMessage(String message[], AxisID axisID) {
    return getFrame(axisID).openDeleteDialog(message);
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
    JOptionPane.showMessageDialog(this, wrap(message), title,
        JOptionPane.ERROR_MESSAGE);
  }
  
  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  private void openMessageDialog(String[] message, String title) {
    JOptionPane.showMessageDialog(this, wrap(message), title,
        JOptionPane.ERROR_MESSAGE);
  }
  
  /**
   * Open a Yes, No or Cancel question dialog
   * @param message
   * @return int state of the users select
   */
  private int openYesNoCancelDialog(String[] message) {
    return JOptionPane.showConfirmDialog(this, wrap(message), "Etomo question",
        JOptionPane.YES_NO_CANCEL_OPTION);
  }
  
  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String message) {
    int result = JOptionPane.showConfirmDialog(this, wrap(message), "Etomo question",
        JOptionPane.YES_NO_OPTION);
    return result == JOptionPane.YES_OPTION;
  }
  
  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openDeleteDialog(String[] message) {
    String[] results = new String[] { "Delete", "No" };
    int result = JOptionPane.showOptionDialog(this, wrap(message),
        "Delete File?", JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE,
        null, results, null);
    return result == 0;
  }
  
  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  private boolean openYesNoDialog(String[] message) {
    int result = JOptionPane.showConfirmDialog(this, wrap(message), "Etomo question",
        JOptionPane.YES_NO_OPTION);
    return result == JOptionPane.YES_OPTION;
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(String message) {
    ArrayList messageArray = new ArrayList();
    if (currentManager != null) {
      messageArray.add(currentManager.getBaseMetaData().getName() + ":");
    }
    messageArray = wrap(message, messageArray);
    if (messageArray.size() == 1) {
      String[] returnArray = {message};
      return returnArray;
    }
    return (String[]) messageArray.toArray(new String[messageArray.size()]);
  }
  
  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(String[] message) {
    ArrayList messageArray = new ArrayList();
    if (currentManager != null) {
      messageArray.add(currentManager.getBaseMetaData().getName() + ":");
    }
    for (int i = 0; i < message.length; i++) {
      messageArray = wrap(message[i], messageArray);
    }
    if (messageArray.size() == 1) {
      String[] returnArray = {(String) messageArray.get(0)};
      return returnArray;
    }
    return (String[]) messageArray.toArray(new String[messageArray.size()]);
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
          StringBuffer newLine = new StringBuffer(messagePieceArray[i].substring(
              messageIndex, endIndex));
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

    
  void fitWindow() {
    fitWindow(false);
  }
  
  /**
   * fit window to its components and to the screen
   *
   */
  void fitWindow(boolean force) {
    if (!force && !EtomoDirector.getInstance().getUserConfiguration().isAutoFit()) {
      setVisible(true);
    }
    else {
      pack();
    }
  }
    
  /**
   * Open a file chooser to get an .edf or .ejf file.
   * @return
   */
  private boolean getTestParamFilename() {
    //  Open up the file chooser in current working directory
    File workingDir = new File(currentManager.getPropertyUserDir());
    JFileChooser chooser =
      new JFileChooser(workingDir);
    DataFileFilter fileFilter = mainPanel.getDataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Save " + fileFilter.getDescription());
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    File[] edfFiles = workingDir.listFiles(fileFilter);
    if (edfFiles.length == 0) {
      File defaultFile = new File(workingDir, currentManager.getBaseMetaData().getMetaDataFileName());
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
      dataFile = new File(chooser.getSelectedFile().getAbsolutePath() + currentManager.getBaseMetaData().getFileExtension());

    }
    currentManager.setTestParamFile(dataFile);
    return true;
  }

  

  /**
   * Increase the bounds by one pixel before packing.  This preserves the
   * scrollbar when the window size doesn't change.
   */
  public void pack() {
    Rectangle bounds = getBounds();
    bounds.height++;
    bounds.width++;
    setBounds(bounds);
    super.pack();
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