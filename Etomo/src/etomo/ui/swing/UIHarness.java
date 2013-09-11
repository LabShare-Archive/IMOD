package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.io.File;
import java.util.Hashtable;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.ui.UIComponent;
import etomo.util.UniqueKey;

/**
 * <p>Description: Class to provide a public interface to the application
 * frames.  Must allow objects in other classes to call any public or package
 * level function in MainFrame.  If necessary, it can be modified to handle
 * functions from SubFrame.  Must not generate any headless exceptions when JUnit
 * is running.  Logs the text of all dialog messages to etomo_test.log when
 * --test is used on the command line.</p>
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
public final class UIHarness {
  public static final String rcsid = "$Id$";

  private static final String LOG_TAG = "LOG";

  public static final UIHarness INSTANCE = new UIHarness();

  private final Hashtable managerFrameTable = new Hashtable();

  private boolean initialized = false;
  private boolean headless = false;
  private MainFrame mainFrame = null;
  private boolean verbose = false;
  private JFileChooser fileChooser = null;

  private UIHarness() {
  }

  public void setTitle(BaseManager manager, String title) {
    if (isHead()) {
      getFrame(manager).setTitle(title);
    }
  }

  public void moveSubFrame() {
    if (isHead()) {
      mainFrame.moveSubFrame();
    }
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(BaseManager manager, String message,
      String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayMessage(manager, message, title, axisID);
    }
    else {
      log(message, title, axisID);
    }
  }

  private Component getComponent(final UIComponent uiComponent) {
    if (uiComponent != null) {
      SwingComponent swingComponent = uiComponent.getUIComponent();
      if (swingComponent != null) {
        return swingComponent.getComponent();
      }
    }
    return null;
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(final BaseManager manager,
      final UIComponent uiComponent, final String message, final String title,
      final AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayMessage(manager, getComponent(uiComponent), message,
          title, axisID);
    }
    else {
      log(message, title, axisID);
    }
  }

  public synchronized void openMessageDialog(final UIComponent uiComponent,
      final String message, final String title) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(null)
          .displayMessage(null, getComponent(uiComponent), message, title, null);
    }
    else {
      log(message, title, null);
    }
  }

  public void openProblemValueMessageDialog(final UIComponent uiComponent,
      final String problem, final String paramName, final String paramDescr,
      final String fieldLabel, final String problemValue, final String replacementValue,
      final String replacementValueDescr) {
    openWarningMessageDialog(uiComponent, problem
        + " '"
        + paramName
        + "' parameter "
        + (paramDescr != null ? paramDescr : "")
        + " value '"
        + problemValue
        + "'."
        + (replacementValue != null ? "  The " + problem.toLowerCase()
            + " value will be replaced with "
            + (replacementValueDescr != null ? "'" + replacementValueDescr + "': " : "")
            + " '" + replacementValue + "'." : "")
        + (fieldLabel != null ? "  See the '" + fieldLabel + "' field." : ""), problem
        + " Value");
  }

  private synchronized void openWarningMessageDialog(final UIComponent uiComponent,
      final String message, final String title) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(null).displayWarningMessage(null, getComponent(uiComponent), message,
          title, null);
    }
    else {
      log(message, title, null);
    }
  }

  public synchronized void openMessageDialog(final UIComponent uiComponent,
      final String[] message, final String title) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(null)
          .displayMessage(null, getComponent(uiComponent), message, title, null);
    }
    else {
      log(message, title, null);
    }
  }

  public synchronized void openInfoMessageDialog(BaseManager manager, String message,
      String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayInfoMessage(manager, message, title, axisID);
    }
    else {
      log(message, title, axisID);
    }
  }

  /**
   * open one dialog and display all error messages in messages.
   * @param messages
   * @param title
   */
  public synchronized void openErrorMessageDialog(BaseManager manager,
      ProcessMessages message, String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayErrorMessage(manager, message, title, axisID);
    }
    else {
      logError(message, title, axisID);
    }
  }

  /**
   * open one dialog and display all warning messages in messages.
   * @param messages
   * @param title
   */
  public synchronized void openWarningMessageDialog(BaseManager manager,
      ProcessMessages messages, String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayWarningMessage(manager, messages, title, axisID);
    }
    else {
      logWarning(messages, title, axisID);
    }
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(BaseManager manager, String message,
      String title) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayMessage(manager, message, title);
    }
    else {
      log(message, title);
    }
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(BaseManager manager, String[] message,
      String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      getFrame(manager).displayMessage(manager, message, title, axisID);
    }
    else {
      log(message, title, axisID);
    }
  }

  public synchronized EtomoBoolean2 openYesNoCancelDialog(BaseManager manager,
      String message, AxisID axisID) {
    EtomoBoolean2 retval = null;
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      int dialogRetValue = getFrame(manager).displayYesNoCancelMessage(manager, message,
          axisID);
      if (dialogRetValue == JOptionPane.CANCEL_OPTION) {
        return null;
      }
      retval = new EtomoBoolean2();
      if (dialogRetValue == JOptionPane.YES_OPTION) {
        retval.set(true);
      }
      else if (dialogRetValue == JOptionPane.YES_OPTION) {
        retval.set(false);
      }
      return retval;
    }
    log(message, axisID);
    retval = new EtomoBoolean2();
    retval.set(true);
    return retval;
  }

  public synchronized boolean openYesNoDialog(BaseManager manager, String message,
      AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      return getFrame(manager).displayYesNoMessage(manager, message, axisID);
    }
    log(message, axisID);
    return true;
  }

  public synchronized boolean openYesNoDialogWithDefaultNo(BaseManager manager, String message,
      String title, AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      return getFrame(manager).openYesNoDialogWithDefaultNo(manager, message, title, axisID);
    }
    log(message, axisID);
    return true;
  }

  public synchronized boolean openDeleteDialog(BaseManager manager, String[] message,
      AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      return getFrame(manager).displayDeleteMessage(manager, message, axisID);
    }
    log(message, axisID);
    return true;
  }

  public synchronized boolean openYesNoWarningDialog(BaseManager manager, String message,
      AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      return getFrame(manager).displayYesNoWarningDialog(manager, message, axisID);
    }
    log(message, axisID);
    return true;
  }

  public synchronized boolean openYesNoDialog(BaseManager manager, String[] message,
      AxisID axisID) {
    if (isHead() && !EtomoDirector.INSTANCE.isTestFailed()) {
      return getFrame(manager).displayYesNoMessage(manager, message, axisID);
    }
    log(message, axisID);
    return true;
  }

  public void showAxisA() {
    if (isHead()) {
      mainFrame.showAxisA();
    }
  }

  public void showAxisB() {
    if (isHead()) {
      mainFrame.showAxisB();
    }
  }

  public void showBothAxis() {
    if (isHead()) {
      mainFrame.showBothAxis();
    }
  }

  public void toFront(BaseManager manager) {
    if (isHead()) {
      getFrame(manager).toFront();
    }
  }

  public void pack(BaseManager manager) {
    if (isHead()) {
      if (manager != null) {
        manager.pack();
      }
      AbstractFrame abstractFrame = getFrame(manager);
      abstractFrame.repaint();
      abstractFrame.pack();
      if (manager != null) {
        Component focusComponent = manager.getFocusComponent();
        if (focusComponent != null) {
          focusComponent.requestFocus();
        }
      }
    }
  }

  public void cancel(final BaseManager manager) {
    if (isHead()) {
      AbstractFrame frame = getFrame(manager);
      if (frame != null) {
        frame.cancel();
      }
    }
  }

  public void save(final BaseManager manager, final AxisID axisID) {
    if (isHead()) {
      AbstractFrame frame = getFrame(manager);
      if (frame != null) {
        frame.save(axisID);
      }
    }
  }

  public AbstractFrame getFrame(BaseManager manager) {
    if (manager == null || !manager.isInManagerFrame()) {
      return mainFrame;
    }
    return (ManagerFrame) managerFrameTable.get(manager);
  }

  public AbstractFrame getMainFrame() {
    return mainFrame;
  }

  public void pack(boolean force, BaseManager manager) {
    if (isHead()) {
      manager.pack();
      AbstractFrame abstractFrame = getFrame(manager);
      abstractFrame.repaint();
      abstractFrame.pack(force);
      if (manager != null) {
        Component focusComponent = manager.getFocusComponent();
        if (focusComponent != null) {
          focusComponent.requestFocus();
        }
      }
    }
  }

  public void pack(AxisID axisID, BaseManager manager) {
    if (isHead()) {
      manager.pack();
      AbstractFrame abstractFrame = getFrame(manager);
      abstractFrame.repaint(axisID);
      abstractFrame.pack(axisID);
      Component focusComponent = manager.getFocusComponent();
      // System.out.println("focusComponent=" + focusComponent);
      if (focusComponent != null) {
        focusComponent.requestFocus();
      }
      // else {
      // new Exception().printStackTrace();
      // }
    }
  }

  public void pack(AxisID axisID, boolean force, BaseManager manager) {
    if (isHead()) {
      manager.pack();
      AbstractFrame abstractFrame = getFrame(manager);
      abstractFrame.repaint(axisID);
      abstractFrame.pack(axisID, force);
      Component focusComponent = manager.getFocusComponent();
      // System.out.println("focusComponent=" + focusComponent);
      if (focusComponent != null) {
        focusComponent.requestFocus();
      }
      // else {
      // new Exception().printStackTrace();
      // }
    }
  }

  public void setEnabledNewTomogramMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewTomogramMenuItem(enable);
    }
  }

  public void setMRUFileLabels(String[] mRUList) {
    if (isHead()) {
      mainFrame.setMRUFileLabels(mRUList);
    }
  }

  public boolean is3dmodStartupWindow() {
    if (!isHead()) {
      return false;
    }
    return mainFrame.isMenu3dmodStartupWindow();
  }

  public boolean is3dmodBinBy2() {
    if (!isHead()) {
      return false;
    }
    return mainFrame.isMenu3dmodBinBy2();
  }

  public void doLayout(BaseManager manager) {
    if (isHead()) {
      getFrame(manager).doLayout();
    }
  }

  public void validate(BaseManager manager) {
    if (isHead()) {
      getFrame(manager).validate();
    }
  }

  public void setVisible(BaseManager manager, boolean b) {
    if (isHead()) {
      getFrame(manager).setVisible(b);
    }
  }

  public Dimension getSize(BaseManager manager) {
    if (isHead()) {
      return getFrame(manager).getSize();
    }
    return FixedDim.x0_y0;
  }

  public Point getLocation(BaseManager manager) {
    if (isHead()) {
      return getFrame(manager).getLocation();
    }
    return new Point(0, 0);
  }

  /**
   * Returns the existing file chooser.  Everything in the file chooser is reset except
   * current directory.
   * @return a file chooser if the application has a GUI, otherwise null
   */
  public JFileChooser getFileChooser() {
    if (isHead()) {
      if (fileChooser == null) {
        fileChooser = new FileChooser(null);
        fileChooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
      }
      else {
        // restore to defaults
        fileChooser.resetChoosableFileFilters();
        fileChooser.setDialogTitle(FileChooser.DEFAULT_TITLE);
        fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
        fileChooser.setFileFilter(null);
        fileChooser.setFileHidingEnabled(true);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setMultiSelectionEnabled(false);
        fileChooser.setSelectedFile(new File(""));
      }
      return fileChooser;
    }
    return null;
  }

  public void setCurrentManager(BaseManager currentManager, UniqueKey managerKey,
      boolean newWindow) {
    if (isHead()) {
      mainFrame.setCurrentManager(currentManager, managerKey, newWindow);
    }
  }

  public void setCurrentManager(BaseManager currentManager, UniqueKey managerKey) {
    if (isHead()) {
      mainFrame.setCurrentManager(currentManager, managerKey);
    }
  }

  public void save(AxisID axisID) {
    if (isHead()) {
      if (mainFrame.isMenuSaveEnabled()) {
        mainFrame.save(axisID);
      }
    }
  }

  public void selectWindowMenuItem(UniqueKey currentKey) {
    if (isHead()) {
      mainFrame.selectWindowMenuItem(currentKey);
    }
  }

  /**
   * If there is a head, tells mainFrame to select a window menu item base on
   * currentManagerKey.
   * @param currentManagerKey
   * @param newWindow
   */
  public void selectWindowMenuItem(UniqueKey currentManagerKey, boolean newWindow) {
    if (isHead()) {
      mainFrame.selectWindowMenuItem(currentManagerKey, newWindow);
    }
  }

  public void setEnabledLogWindowMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledLogWindowMenuItem(enable);
    }
  }

  public void setEnabledNewJoinMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewJoinMenuItem(enable);
    }
  }

  public void setEnabledNewGenericParallelMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewGenericParallelMenuItem(enable);
    }
  }

  public void setEnabledNewAnisotropicDiffusionMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewAnisotropicDiffusionMenuItem(enable);
    }
  }

  public void setEnabledNewPeetMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewPeetMenuItem(enable);
    }
  }

  public void setEnabledNewSerialSectionsMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewSerialSectionsMenuItem(enable);
    }
  }

  public void addFrame(final BaseManager manager, final boolean savable) {
    if (isHead()) {
      ManagerFrame managerFrame = ManagerFrame.getInstance(manager, savable);
      managerFrameTable.put(manager, managerFrame);
    }
  }

  public void addWindow(BaseManager manager, UniqueKey managerKey) {
    if (isHead()) {
      mainFrame.addWindow(manager, managerKey);
    }
  }

  public void removeWindow(UniqueKey managerKey) {
    if (isHead()) {
      mainFrame.removeWindow(managerKey);
    }
  }

  public void renameWindow(UniqueKey oldKey, UniqueKey newManagerKey) {
    if (isHead()) {
      mainFrame.renameWindow(oldKey, newManagerKey);
    }
  }

  public void repaintWindow(BaseManager manager) {
    if (isHead()) {
      getFrame(manager).repaintWindow();
    }
  }

  /**
   * Initialize if necessary.  Instantiate mainFrame if headless is false.
   *
   */
  public void createMainFrame() {
    if (!initialized) {
      initialize();
    }
    if (!headless && mainFrame == null) {
      mainFrame = new MainFrame();
      mainFrame.setVerbose(verbose);
    }
  }

  public void exit(final AxisID axisID, final int exitValue) {
    // Store the current location of the frame in case etomo exits.
    if (isHead()) {
      mainFrame.saveLocation();
      EtomoFrame subFrame = mainFrame.getOtherFrame();
      if (subFrame != null) {
        subFrame.saveLocation();
      }
    }
    // Check to see if etomo an exit, save data, and then exit.
    if (EtomoDirector.INSTANCE.exitProgram(axisID)) {
      System.err.println("exitValue:" + exitValue);
      System.exit(exitValue);
    }
  }

  /**
   * Initialize if necessary.
   * @return True, if mainFrame is not equal to null.
   */
  private boolean isHead() {
    if (!initialized) {
      initialize();
    }
    return mainFrame != null;
  }

  /**
   * Initialize headless, testLog, and logWriter.
   *
   */
  private void initialize() {
    initialized = true;
    EtomoDirector etomo = EtomoDirector.INSTANCE;
    headless = etomo.getArguments().isHeadless();
  }

  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param axisID
   */
  private void log(String message, AxisID axisID) {
    log(message, null, axisID);
  }

  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param title
   * @param axisID
   */
  private void log(String message, String title) {
    log(message, title, AxisID.ONLY);
  }

  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param title
   * @param axisID
   */
  private void log(String message, String title, AxisID axisID) {
    logHeader(title, axisID);
    System.err.println(message);
    System.err.println();
    System.err.flush();
  }

  private void logError(ProcessMessages processMessages, String title, AxisID axisID) {
    logHeader(title, axisID);
    processMessages.printError();
    System.err.println();
    System.err.flush();
  }

  private void logWarning(ProcessMessages processMessages, String title, AxisID axisID) {
    logHeader(title, axisID);
    processMessages.printWarning();
    System.err.println();
    System.err.flush();
  }

  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param axisID
   */
  private void log(String[] message, AxisID axisID) {
    log(message, null, axisID);
  }

  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param title
   * @param axisID
   */
  private void log(String[] message, String title, AxisID axisID) {
    logHeader(title, axisID);
    if (message != null) {
      for (int i = 0; i < message.length; i++) {
        System.err.println(message[i]);
      }
    }
    System.err.println();
    System.err.flush();
  }

  private void logHeader(final String title, final AxisID axisID) {
    System.err.println(LOG_TAG + ": " + (title == null ? "" : title)
        + (axisID == null || axisID == AxisID.ONLY ? "" : "(" + axisID + ")") + ":");
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 21:42:34  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.40  2010/06/02 21:47:48  sueh
 * <p> bug# 1380 In exit saving the frame location.  Changed
 * <p> EtomoDirector.testDone to testFailed for clarity.
 * <p>
 * <p> Revision 1.39  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.38  2009/11/20 17:38:54  sueh
 * <p> bug# 1282 Added save.  Has the same function as the Save menu option.
 * <p>
 * <p> Revision 1.37  2009/10/27 20:43:15  sueh
 * <p> bug# 1275 Moved FrontPageDialog to FrontPageManager.
 * <p>
 * <p> Revision 1.36  2009/10/23 19:47:59  sueh
 * <p> bug# 1275 Added setTitle and displayFromPage.
 * <p>
 * <p> Revision 1.35  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.34  2009/03/05 23:31:41  sueh
 * <p> bug# 1194 Added msgUpdateLogProperties.
 * <p>
 * <p> Revision 1.33  2009/02/04 23:36:09  sueh
 * <p> bug# 1158 Added msgChanged(LogPanel) and
 * <p> setEnabledLogWindowMenuItem.
 * <p>
 * <p> Revision 1.32  2008/10/06 22:47:26  sueh
 * <p> bug# 1113 Removed packPanel, which is unecessary since scrolling was
 * <p> removed.
 * <p>
 * <p> Revision 1.31  2008/09/30 22:52:57  sueh
 * <p> bug# 1113 Modify pack functions so that they can request focus after
 * <p> repaint and pack are done.  The focus Component comes from the
 * <p> manager.
 * <p>
 * <p> Revision 1.30  2008/05/30 21:35:13  sueh
 * <p> bug# 1102 Made some functions public for ui test.
 * <p>
 * <p> Revision 1.29  2007/12/26 22:39:02  sueh
 * <p> bug# 1052 Added exit - general way to exit from Etomo.
 * <p>
 * <p> Revision 1.28  2007/09/07 00:29:43  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.27  2007/05/02 21:07:45  sueh
 * <p> bug# 964 Removed Import PRM and Duplicate PEET menu items.
 * <p>
 * <p> Revision 1.26  2007/04/19 22:07:03  sueh
 * <p> bug# 964 Preventing a ull point buger.
 * <p>
 * <p> Revision 1.25  2007/03/31 03:03:10  sueh
 * <p> bug# 964 Enabling/disabling Duplicate Peet menu item.
 * <p>
 * <p> Revision 1.24  2007/02/19 22:04:03  sueh
 * <p> bug# 964 Added setEnabledNewPeetMenuItem.
 * <p>
 * <p> Revision 1.23  2006/06/06 18:14:59  sueh
 * <p> bug# 766 Add a logging option, which always writes popup messages to the
 * <p> error, and also pops them up.
 * <p>
 * <p> Revision 1.22  2006/04/25 19:37:07  sueh
 * <p> bug# 787 Added testDone to EtomoDirector so that Etomo can exit
 * <p> without popups when the UITest package fails or asks Etomo to exit.
 * <p>
 * <p> Revision 1.21  2006/03/20 18:08:05  sueh
 * <p> bug# 835 Added menu option to create a new ParallelManager.
 * <p>
 * <p> Revision 1.20  2006/01/11 23:16:43  sueh
 * <p> bug# 675 added setVerbose() and getCurrentPopupName().
 * <p>
 * <p> Revision 1.19  2005/12/23 02:24:20  sueh
 * <p> bug# 675 Split the test option functionality into headless and test.
 * <p>
 * <p> Revision 1.18  2005/12/09 20:37:08  sueh
 * <p> bug# Added an info message popup
 * <p>
 * <p> Revision 1.17  2005/12/08 00:59:12  sueh
 * <p> bug# 504 Added openYesNoWarningDialog() which displays a yes/no
 * <p> popup with No selected and a warning icon.
 * <p>
 * <p> Revision 1.16  2005/11/02 22:15:15  sueh
 * <p> bug# 754 Integrating ProcessMessages.  Added functions logError,
 * <p> logWarning, openErrorMessageDialog, openWarningMessageDialog.
 * <p>
 * <p> Revision 1.15  2005/09/22 21:34:42  sueh
 * <p> bug# 532 Moved ApplicationManager.packDialogs functions to
 * <p> BaseManager and renamed them packPanel.
 * <p>
 * <p> Revision 1.14  2005/08/12 00:21:26  sueh
 * <p> bug# 711 changed StartUpWindow to StartupWindow.
 * <p>
 * <p> Revision 1.13  2005/08/12 00:02:43  sueh
 * <p> bug# 711  Add is3dmodStartUpWindow() and 3dmodBinBy2() to get the
 * <p> menu settings.
 * <p>
 * <p> Revision 1.12  2005/08/04 20:18:10  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.11  2005/07/01 21:26:47  sueh
 * <p> bug# 619 Temporality getting the frame to use with the demo
 * <p>
 * <p> Revision 1.10  2005/06/21 00:49:13  sueh
 * <p> bug# 522 Added comment
 * <p>
 * <p> Revision 1.9  2005/06/17 00:35:32  sueh
 * <p> Removed unnecessary imports.
 * <p>
 * <p> Revision 1.8  2005/06/16 20:10:13  sueh
 * <p> bug# 692 Log messages to the err log instead of a separate file.
 * <p>
 * <p> Revision 1.7  2005/06/01 21:29:17  sueh
 * <p> bug# 667 Removing the Controller classes.  Trying make meta data and
 * <p> app manager equals didn't work very well.  Meta data is created by and
 * <p> managed by app mgr and the class structure should reflect that.
 * <p>
 * <p> Revision 1.6  2005/05/18 22:48:53  sueh
 * <p> bug# 662 Added an openMessageDialog function which doesn't require
 * <p> specifying the axisID (defaults to AxisID.ONLY).  Added
 * <p> openDeleteDialog().
 * <p>
 * <p> Revision 1.5  2005/05/10 03:28:38  sueh
 * <p> bug# 615 Do not create etomo_test.log unless --test is set.
 * <p>
 * <p> Revision 1.4  2005/04/27 02:20:10  sueh
 * <p> bug# 615 Removed createMenus(), since it does not have to be visible.
 * <p> In createMainFrame() make sure that mainFrame cannot be instantiated
 * <p> more then once.
 * <p>
 * <p> Revision 1.3  2005/04/26 18:35:33  sueh
 * <p> bug# 615 Fixed a bug in log().  LogWriter was not flushing to the file.
 * <p>
 * <p> Revision 1.2  2005/04/26 17:44:30  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  Added all functions
 * <p> necessary to handle all MainFrame functionality through UIHarness.  This
 * <p> makes Etomo more compatible with JUnit.
 * <p>
 * <p> Revision 1.1  2005/04/25 21:42:16  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p> </p>
 */
