package etomo.ui;

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
  
  public void createMenus() {
    if (isHead()) {
      mainFrame.createMenus();
    }
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

  private boolean isHead() {
    if (!initialized && !test && mainFrame == null) {
      initialize();
    }
    return !test && mainFrame != null;
  }
  
  private void initialize() {
    EtomoDirector etomo = EtomoDirector.getInstance();
    test = etomo.isTest();
    mainFrame = etomo.getMainFrame();
    if (testLog == null) {
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
    }
    try {
      logWriter = new BufferedWriter(new FileWriter(testLog));
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    initialized = true;
  }
  
  private void log(String function, String message, AxisID axisID) {
    log(function, message, null, axisID);
  }
  
  private void log(String function, String message, String title, AxisID axisID) {
    if (logWriter != null) {
      try {
        logWriter.newLine();
        logWriter.write(function + ", " + axisID + ", " + title + ":");
        logWriter.newLine();
        logWriter.write(message);
        logWriter.newLine();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
  
  private void log(String function, String[] message, AxisID axisID) {
    log(function, message, null, axisID);
  }
  
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
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }


}
/**
* <p> $Log$ </p>
*/