package etomo.ui;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.io.File;
import java.util.ArrayList;

import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.4  2005/01/27 20:22:18  sueh
 * <p> bug# 513 Synchronizing fit window code.
 * <p>
 * <p> Revision 1.3  2005/01/27 00:13:03  sueh
 * <p> bug# 543 Checking autofit before fitting window.  Added
 * <p> fitWindow(boolean force) to force a fit window for Ctrl-F.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:58:52  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.13  2004/11/19 19:38:56  sueh
 * <p> bug# 520 Added wrap functions to wrap message dialog messages.
 * <p>
 * <p> Revision 1.1.2.12  2004/11/19 00:23:45  sueh
 * <p> bug# 520 Changed the file extension in ConstJoinMetaData to contain the
 * <p> period.
 * <p>
 * <p> Revision 1.1.2.11  2004/10/15 00:50:18  sueh
 * <p> bug# 520 Made getTestParamFilename() generic.  Removed
 * <p> openEtomoDataFileDialog().
 * <p>
 * <p> Revision 1.1.2.10  2004/10/11 02:15:57  sueh
 * <p> bug# 520 Moved responsibility for axisPanelA and axisPanelB member
 * <p> variables to the child classes.  Used abstract functions to use these
 * <p> variables in the base class.  This is more reliable and doesn't require
 * <p> casting.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/08 16:34:05  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/01 20:00:21  sueh
 * <p> bug# 520 Standardized getting the metadata file name.
 * <p>
 * <p> Revision 1.1.2.7  2004/09/29 19:37:04  sueh
 * <p> bug# 520 Moved status bar initialization to child classes.
 * <p>
 * <p> Revision 1.1.2.6  2004/09/21 18:02:52  sueh
 * <p> bug# 520 Added openYesNoDialog(String, String).  For
 * <p> openMessageDialog(), handling the situation where no dataset name is
 * <p> set by opening the message without adding anything to it.
 * <p>
 * <p> Revision 1.1.2.5  2004/09/15 22:45:34  sueh
 * <p> bug# 520 Moved openSetupPanel to MainTomogramPanel.  Moved
 * <p> showProcessingPanel() to this class.  Moved AxisProcessPanel creation
 * <p> to abstract functions.  Added the dataset name before the message in
 * <p> openMessageDialog.
 * <p>
 * <p> Revision 1.1.2.4  2004/09/09 17:34:41  sueh
 * <p> bug# 520 remove unnecessary functions that are duplicated in MainFrame:
 * <p> menuFileMRUListAction and popUpContextMenu
 * <p>
 * <p> Revision 1.1.2.3  2004/09/08 22:39:54  sueh
 * <p> bug# 520 class doesn't need to be abstract, fixed problem with packing
 * <p> setup dialog by calling revalidate()
 * <p>
 * <p> Revision 1.1.2.2  2004/09/08 20:11:31  sueh
 * <p> bug# 520 make this class into a base class.  Move all tomogram specific
 * <p> functionality to MainTomogramPanel.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/07 18:01:08  sueh
 * <p> bug# 520 moved all variables and functions associated with mainPAnel
 * <p> to MainPanel.
 * <p> </p>
 */
public abstract class MainPanel extends JPanel {
  public static final String rcsid =
    "$Id$";

  protected JLabel statusBar = new JLabel("No data set loaded");

  protected JPanel panelCenter = new JPanel();

  //  These panels get instantiated as needed
  protected ScrollPanel scrollA;
  protected JScrollPane scrollPaneA;
  protected ScrollPanel scrollB;
  protected JScrollPane scrollPaneB;
  protected JSplitPane splitPane;
  protected BaseManager manager = null;
  
  private static final int estimatedMenuHeight = 60;
  private static final int extraScreenWidthMultiplier = 2;
  private static final Dimension frameBorder = new Dimension(10, 48);
  private static final int messageWidth = 60;
  private static final int maxMessageLines = 20;
  
  protected abstract void createAxisPanelA(AxisID axisID);
  protected abstract void createAxisPanelB();
  protected abstract void resetAxisPanels();
  protected abstract void addAxisPanelA();
  protected abstract void addAxisPanelB();
  protected abstract boolean AxisPanelAIsNull();
  protected abstract boolean AxisPanelBIsNull();
  protected abstract boolean hideAxisPanelA();
  protected abstract boolean hideAxisPanelB();
  protected abstract void showAxisPanelA();  
  protected abstract void showAxisPanelB();
  protected abstract boolean isAxisPanelAFitScreenError();
  protected abstract AxisProcessPanel mapBaseAxis(AxisID axisID);
  protected abstract DataFileFilter getDataFileFilter();

  /**
   * Main window constructor.  This sets up the menus and status line.
   */
  public MainPanel(BaseManager manager) {
    this.manager = manager;
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
/*
    Toolkit toolkit = Toolkit.getDefaultToolkit();
    Dimension screenSize = toolkit.getScreenSize();
    screenSize.height -= estimatedMenuHeight;
    screenSize.width *= extraScreenWidthMultiplier;
    Dimension mainPanelSize = new Dimension(screenSize);
    mainPanelSize.height -= frameBorder.height;
    mainPanelSize.width -= frameBorder.width;
*/
    setLayout(new BorderLayout());
/*    setMaximumSize(mainPanelSize);
*/
    //  Construct the main frame panel layout
    panelCenter.setLayout(new BoxLayout(panelCenter, BoxLayout.X_AXIS));
    add(panelCenter, BorderLayout.CENTER);
    add(statusBar, BorderLayout.SOUTH);
  }

  /**
   * set divider location
   * @param value
   */
  public void setDividerLocation(double value) {
    if (splitPane != null) {
      //removing commands that cause the divider location to change incorrectly
      //when the window is taller then the screen
      //scrollPaneA.doLayout();
      //scrollPaneB.doLayout();
      //splitPane.doLayout();
      //splitPane.revalidate();
      //splitPane.validate();
      splitPane.setDividerLocation(value);
    }
  }

  /**
   * Show a blank processing panel
   */
  public void showBlankProcess(AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.eraseDialogPanel();
  }

  /**
   * Show the specified processing panel
   */
  public void showProcess(Container processPanel, AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.replaceDialogPanel(processPanel);
    fitWindow();
  }

  /**
   * Set the progress bar to the beginning of determinant sequence
   * @param label
   * @param nSteps
   */
  public void setProgressBar(String label, int nSteps, AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.setProgressBar(label, nSteps);
    axisPanel.setProgressBarValue(0);
  }

  /**
   * Set the progress bar to the specified value
   * @param value
   * @param axisID
   */
  public void setProgressBarValue(int value, AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.setProgressBarValue(value);
  }

  /**
   * Set the progress bar to the speficied value and update the string
   * @param value
   * @param string
   * @param axisID
   */
  public void setProgressBarValue(int value, String string, AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.setProgressBarValue(value, string);
  }

  /**
   *  Start the indeterminate progress bar on the specified axis 
   */
  public void startProgressBar(String name, AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.startProgressBar(name);
  }

  /**
   * Stop the specified progress bar
   * @param axisID
   */
  public void stopProgressBar(AxisID axisID) {
    AxisProcessPanel axisPanel = mapBaseAxis(axisID);
    axisPanel.stopProgressBar();
  }

  public boolean getTestParamFilename() {
    //  Open up the file chooser in current working directory
    File workingDir = new File(manager.getPropertyUserDir());
    JFileChooser chooser =
      new JFileChooser(workingDir);
    DataFileFilter fileFilter = getDataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Save " + fileFilter.getDescription());
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    File[] edfFiles = workingDir.listFiles(fileFilter);
    if (edfFiles.length == 0) {
      File defaultFile = new File(workingDir, manager.getBaseMetaData().getMetaDataFileName());
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
      dataFile = new File(chooser.getSelectedFile().getAbsolutePath() + manager.getBaseMetaData().getFileExtension());

    }
    manager.setTestParamFile(dataFile);
    return true;
  }

  /**
   * Show the processing panel for the requested AxisType
   */
  public void showProcessingPanel(AxisType axisType) {
    //  Delete any existing panels
    resetAxisPanels();

    panelCenter.removeAll();
    if (axisType == AxisType.SINGLE_AXIS) {
      createAxisPanelA(AxisID.ONLY);
      scrollA = new ScrollPanel();
      addAxisPanelA();
      scrollPaneA = new JScrollPane(scrollA);
      panelCenter.add(scrollPaneA);
    }
    else {
      createAxisPanelA(AxisID.FIRST);
      scrollA = new ScrollPanel();
      addAxisPanelA();
      scrollPaneA = new JScrollPane(scrollA);

      createAxisPanelB();
      scrollB = new ScrollPanel();
      addAxisPanelB();
      scrollPaneB = new JScrollPane(scrollB);
      splitPane =
        new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPaneA, scrollPaneB);
      splitPane.setDividerLocation(0.5);
      splitPane.setOneTouchExpandable(true);
      panelCenter.add(splitPane);
    }
  }


  /**
   * if A or B is hidden, hide the panel which the user has hidden before
   * calling pack().
   *
   */
  protected void packAxis() {
    if (manager.isDualAxis()
      && !AxisPanelAIsNull()
      && !AxisPanelBIsNull()) {
      boolean hideA = hideAxisPanelA();
      boolean hideB = hideAxisPanelB();
      EtomoDirector.getInstance().getMainFrame().pack();
      splitPane.resetToPreferredSizes();
      
      //handle bug in Windows where divider goes all the way to the left
      //when the frame is wider then the screen
      if (!hideA && !hideB && isAxisPanelAFitScreenError()) {
        setDividerLocation(.8); //.8 currently works.  Adjust as needed.
        splitPane.resetToPreferredSizes();
      }
      
      showAxisPanelA();
      showAxisPanelB();
      if (hideA) {
        setDividerLocation(0);
      }
      else if (hideB) {
        setDividerLocation(1);
      }
    }
    else {
      EtomoDirector.getInstance().getMainFrame().pack();
    }
  }
  
  /**
   * checks for a bug in windows that causes MainFrame.fitScreen() to move the
   * divider almost all the way to the left
   * @return
   */
  protected boolean isFitScreenError(AxisProcessPanel axisPanel) {
    if (axisPanel.getWidth() <= 16) {
      return true;
    }
    return false;
  }

  /**
   * set vertical scrollbar policy
   * @param always
   */
  protected void setVerticalScrollBarPolicy(boolean always) {
    int policy =
      always
        ? JScrollPane.VERTICAL_SCROLLBAR_ALWAYS
        : JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED;
    if (scrollPaneA != null) {
      scrollPaneA.setVerticalScrollBarPolicy(policy);
    }
    if (scrollPaneB != null) {
      scrollPaneB.setVerticalScrollBarPolicy(policy);
    }
  }
  
  public void fitWindow() {
    fitWindow(false);
  }
  
  /**
   * fit window to its components and to the screen
   *
   */
  public void fitWindow(boolean force) {
    if (!force && !EtomoDirector.getInstance().getUserConfiguration().isAutoFit()) {
      return;
    }
    synchronized (MainFrame.class) {
      packAxis();
      //the mainPanel has a limited size, but the frame does not
      //if the frame has a greater height then the mainPanel + the frame's border
      //height, then a scroll bar will be used.
      //Make room for the scroll bar when calling pack()
      if (getSize().height - getSize().height > frameBorder.height) {
        setVerticalScrollBarPolicy(true);
        packAxis();
        setVerticalScrollBarPolicy(false);
      }
    }
  }


  /**
   * Open a Yes or No question dialog
   * @param message
   * @return boolean True if the Yes option was selected
   */
  public boolean openYesNoDialog(String[] message) {
    try {
      int answer =
        JOptionPane.showConfirmDialog(
          this,
          message,
          "Etomo question",
          JOptionPane.YES_NO_OPTION);

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
  
  public boolean openYesNoDialog(String message) {
    try {
      int answer =
        JOptionPane.showConfirmDialog(
          this,
          message,
          "Etomo question",
          JOptionPane.YES_NO_OPTION);

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
    return JOptionPane.showConfirmDialog(
      this,
      message,
      "Etomo question",
      JOptionPane.YES_NO_CANCEL_OPTION);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public void openMessageDialog(String[] message, String title) {
    if (message == null) {
      EtomoDirector.getInstance().getMainFrame().openMessageDialog(
          manager.getBaseMetaData().getName() + ":", title);
      return;
    }
    String[] wrappedMessage = wrap(message);
    EtomoDirector.getInstance().getMainFrame().openMessageDialog(wrappedMessage, title);
  }
  
  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  public void openMessageDialog(String message, String title) {
    if (message == null) {
      EtomoDirector.getInstance().getMainFrame().openMessageDialog(
          manager.getBaseMetaData().getName() + ":", title);
      return;
    }
    else {
      String[] wrappedMessage = wrap(message);
      EtomoDirector.getInstance().getMainFrame().openMessageDialog(wrappedMessage, title);
    }
  }
  
  private String[] wrap(String message) {
    ArrayList messageArray = new ArrayList();
    messageArray.add(manager.getBaseMetaData().getName() + ":");
    messageArray = wrap(message, messageArray);
    if (messageArray.size() == 1) {
      String[] returnArray = {message};
      return returnArray;
    }
    return (String[]) messageArray.toArray(new String[messageArray.size()]);
  }
  
  private String[] wrap(String[] message) {
    ArrayList messageArray = new ArrayList();
    messageArray.add(manager.getBaseMetaData().getName() + ":");
    for (int i = 0; i < message.length; i++) {
      messageArray = wrap(message[i], messageArray);
    }
    if (messageArray.size() == 1) {
      String[] returnArray = {(String) messageArray.get(0)};
      return returnArray;
    }
    return (String[]) messageArray.toArray(new String[messageArray.size()]);
  }
  
  private ArrayList wrap(String message, ArrayList messageArray) {
    int messageLength = message.length();
    int messageIndex = 0;
    while (messageIndex < messageLength && messageArray.size() < maxMessageLines) {
      int endIndex = Math.min(messageLength, messageIndex + messageWidth);
      StringBuffer line = new StringBuffer(message.substring(messageIndex, endIndex));
      messageIndex = endIndex;
      while (messageIndex < messageLength
          && message.substring(messageIndex, messageIndex + 1).matches("\\S+")) {
        line.append(message.charAt(messageIndex++));
      }
      messageArray.add(line.toString());
    }
    return messageArray;
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

}
