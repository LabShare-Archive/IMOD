package etomo.ui;

import java.awt.Dimension;
import java.awt.Point;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.Controller;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.util.UniqueKey;
import etomo.util.Utilities;

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
public class UIHarness {
  public static  final String  rcsid =  "$Id$";
  
  public static final UIHarness INSTANCE = new UIHarness();
  
  private boolean initialized = false;
  private boolean test = false;
  private MainFrame mainFrame = null;
  private File testLog = null;
  private BufferedWriter logWriter = null;
  
  private UIHarness() {
  }
  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(String message, String title, AxisID axisID) {
    if (isHead()) {
      mainFrame.openMessageDialog(message, title, axisID);
    }
    else {
      log("openMessageDialog", message, title, axisID);
    }
  }
  
  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public synchronized void openMessageDialog(String[] message, String title, AxisID axisID) {
    if (isHead()) {
      mainFrame.openMessageDialog(message, title, axisID);
    }
    else {
      log("openMessageDialog", message, title, axisID);
    }
  }
  
  public synchronized int openYesNoCancelDialog(String[] message, AxisID axisID) {
    if (isHead()) {
      return mainFrame.openYesNoCancelDialog(message, axisID);
    }
    log("openYesNoCancelDialog", message, axisID);
    return JOptionPane.YES_OPTION;
  }
  
  public synchronized boolean openYesNoDialog(String message, AxisID axisID) {
    if (isHead()) {
      return mainFrame.openYesNoDialog(message, axisID);
    }
    log("openYesNoDialog", message, axisID);
    return true;
  }
  
  public synchronized boolean openYesNoDialog(String[] message, AxisID axisID) {
    if (isHead()) {
      return mainFrame.openYesNoDialog(message, axisID);
    }
    log("openYesNoDialog", message, axisID);
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
  
  public void fitWindow() {
    if (isHead()) {
      mainFrame.fitWindow();
    }
  }
  
  public void fitWindow(boolean force) {
    if (isHead()) {
      mainFrame.fitWindow(force);
    }
  }
  
  public void fitWindow(AxisID axisID) {
    if (isHead()) {
      mainFrame.fitWindow(axisID);
    }
  }
  
  public void repaint(AxisID axisID) {
    if (isHead()) {
      mainFrame.repaint(axisID);
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
  
  public void pack() {
    if (isHead()) {
      mainFrame.pack();
    }
  }
  
  public void doLayout() {
    if (isHead()) {
      mainFrame.doLayout();
    }
  }
  
  public void validate() {
    if (isHead()) {
      mainFrame.validate();
    }
  }
  
  public void setVisible(boolean b) {
    if (isHead()) {
      mainFrame.setVisible(b);
    }
  }
  
  public Dimension getSize() {
    if (isHead()) {
      return mainFrame.getSize();
    }
    return new Dimension(0,0);
  }
  
  public Point getLocation() {
    if (isHead()) {
      return mainFrame.getLocation();
    }
    return new Point(0,0);
  }
  
  public void setCurrentManager(BaseManager currentManager,
      UniqueKey managerKey, boolean newWindow) {
    if (isHead()) {
      mainFrame.setCurrentManager(currentManager, managerKey, newWindow);
    }
  }
  
  public void setCurrentManager(BaseManager currentManager, UniqueKey managerKey) {
    if (isHead()) {
      mainFrame.setCurrentManager(currentManager, managerKey);
    }
  }
  
  public void selectWindowMenuItem(UniqueKey currentManagerKey) {
    if (isHead()) {
      mainFrame.selectWindowMenuItem(currentManagerKey);
    }
  }
  
  public void selectWindowMenuItem(UniqueKey currentManagerKey, boolean newWindow) {
    if (isHead()) {
      mainFrame.selectWindowMenuItem(currentManagerKey, newWindow);
    }
  }
  
  public void setEnabledNewJoinMenuItem(boolean enable) {
    if (isHead()) {
      mainFrame.setEnabledNewJoinMenuItem(enable);
    }
  }
  
  public void addWindow(Controller controller, UniqueKey controllerKey) {
    if (isHead()) {
      mainFrame.addWindow(controller, controllerKey);
    }
  }
  
  public void removeWindow(UniqueKey controllerKey) {
    if (isHead()) {
      mainFrame.removeWindow(controllerKey);
    }
  }
  
  public void renameWindow(UniqueKey oldKey, UniqueKey newKey) {
    if (isHead()) {
      mainFrame.renameWindow(oldKey, newKey);
    }
  }
  
  public void repaintWindow() {
    if (isHead()) {
      mainFrame.repaintWindow();
    }
  }
  
  /**
   * Initialize if necessary.  Instantiate mainFrame if test is false.
   *
   */
  public void createMainFrame() {
    if (!initialized) {
      initialize();
    }
    if (!test && mainFrame == null) {
      mainFrame = new MainFrame();
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
   * Initialize test, testLog, and logWriter.
   *
   */
  private void initialize() {
    initialized = true;
    EtomoDirector etomo = EtomoDirector.getInstance();
    test = etomo.isTest();
    if (test) {
      testLog = new File(etomo.getCurrentPropertyUserDir(), "etomo_test.log");
      if (testLog.exists()) {
        try {
          Utilities.renameFile(testLog, new File(testLog.getAbsolutePath() + "~"));
        }
        catch (IOException e) {
          e.printStackTrace();
          testLog.delete();
        }
      }
      try {
        logWriter = new BufferedWriter(new FileWriter(testLog));
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
  
  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param axisID
   */
  private void log(String function, String message, AxisID axisID) {
    log(function, message, null, axisID);
  }
  
  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param title
   * @param axisID
   */
  private void log(String function, String message, String title, AxisID axisID) {
    if (logWriter != null) {
      try {
        logWriter.newLine();
        logWriter.write(function + ", " + axisID + ", " + title + ":");
        logWriter.newLine();
        logWriter.write(message);
        logWriter.newLine();
        logWriter.flush();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
  
  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param axisID
   */
  private void log(String function, String[] message, AxisID axisID) {
    log(function, message, null, axisID);
  }
  
  /**
   * Log the parameters in testLog with logWriter.
   * @param function
   * @param message
   * @param title
   * @param axisID
   */
  private void log(String function, String[] message, String title, AxisID axisID) {
    if (logWriter != null) {
      try {
        logWriter.newLine();
        if (title == null) {
          logWriter.write(function + ", " + axisID + ":");
        }
        else {
          logWriter.write(function + ", " + axisID + ", " + title + ":");
        }
        logWriter.newLine();
        if (message != null) {
          for (int i = 0; i < message.length; i++) {
            logWriter.write(message[i]);
            logWriter.newLine();
          }
        }
        logWriter.flush();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}
/**
* <p> $Log$
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