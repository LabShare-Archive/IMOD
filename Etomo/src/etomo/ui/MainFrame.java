package etomo.ui;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.*;
import etomo.type.*;
import etomo.process.ProcessState;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class MainFrame extends JFrame implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel mainPanel;
  private BorderLayout borderMain = new BorderLayout();

  //
  //  Menu bar
  private final int nMRUFileMax = 10;
  private JMenuBar menuBar = new JMenuBar();

  private JMenu menuFile = new JMenu("File");
  private JMenuItem menuFileOpen = new JMenuItem("Open...");
  private JMenuItem menuFileSave = new JMenuItem("Save");
  private JMenuItem menuFileSaveAs = new JMenuItem("Save As");
  private JMenuItem menuFileExit = new JMenuItem("Exit");
  private JMenuItem[] menuMRUList = new JMenuItem[nMRUFileMax];

  private JMenu menuSettings = new JMenu("Settings");
  private JMenuItem menuAdvanced = new JMenuItem("Advanced");
  private JMenuItem menuOptions = new JMenuItem("Options");

  private JMenu menuHelp = new JMenu("Help");
  private JMenuItem menuHelpAbout = new JMenuItem("About");

  private JLabel statusBar = new JLabel("No data set loaded");

  private JPanel panelCenter = new JPanel();

  //
  //  Data Set ID panel
  //
  private DataSetIDPanel dataSetIDPanel = new DataSetIDPanel();

  //
  //  Data Set type panel objects
  //
  private DataSetTypePanel dataSetTypePanel = new DataSetTypePanel();

  //
  //  Process control panels
  //
  private JPanel panelProcess = new JPanel();
  private BeveledBorder borderProcesses = new BeveledBorder("Processes");

  private ProcessControlPanel procCtlPanelSetup =
    new ProcessControlPanel("Setup\n");
  private ProcessControlPanel procCtlPanelPreProc =
    new ProcessControlPanel("Pre-\nProcessing");
  private ProcessControlPanel procCtlPanelCoarseAlign =
    new ProcessControlPanel("Coarse\nAlignment");
  private ProcessControlPanel procCtlPanelFiducialModel =
    new ProcessControlPanel("Fiducial\nModel Gen.");
  private ProcessControlPanel procCtlPanelAlignmentEst =
    new ProcessControlPanel("Fine\nAlignment");
  private ProcessControlPanel procCtlPanelTomogramPositioning =
    new ProcessControlPanel("Tomogram\nPositioning");
  private ProcessControlPanel procCtlPanelTomogramGeneration =
    new ProcessControlPanel("Tomogram\nGeneration");
  private ProcessControlPanel procCtlPanelTomogramCombination =
    new ProcessControlPanel("Tomogram\nCombination");
  private ProcessControlPanel procCtlPanelPostProcessing =
    new ProcessControlPanel("Post\nProcessing");

  //
  //  Application manager object
  //
  private ApplicationManager applicationManager;

  /**Construct the frame*/
  public MainFrame(ApplicationManager appManager) {
    applicationManager = appManager;

    enableEvents(AWTEvent.WINDOW_EVENT_MASK);

    //setIconImage(Toolkit.getDefaultToolkit().createImage(
    //MainFrame.class.getResource("[Your Icon]")));

    setTitle("eTomo");

    mainPanel = (JPanel) getContentPane();
    mainPanel.setLayout(borderMain);

    //
    //  Menu bar text and adapters
    //
    menuFileOpen.addActionListener(
      new MainFrame_menuFileOpenActionAdapter(this));
    menuFileSave.addActionListener(
      new MainFrame_menuFileSaveActionAdapter(this));
    menuFileSaveAs.addActionListener(
      new MainFrame_menuFileSaveAsActionAdapter(this));
    menuFileExit.addActionListener(
      new MainFrame_menuFileExitActionAdapter(this));

    menuOptions.addActionListener(new MainFrame_menuOptionsActionAdapter(this));

    menuHelpAbout.addActionListener(
      new MainFrame_menuHelpAboutActionAdapter(this));

    menuFile.add(menuFileOpen);
    menuFile.add(menuFileSave);
    menuFile.add(menuFileSaveAs);
    menuFile.add(menuFileExit);
    menuFile.addSeparator();

    //
    //  Initialize all of the MRU file menu items
    //

    MainFrame_menuFileMRUListActionAdapter mRUListActionAdapter =
      new MainFrame_menuFileMRUListActionAdapter(this);
    for (int i = 0; i < nMRUFileMax; i++) {
      menuMRUList[i] = new JMenuItem();
      menuMRUList[i].addActionListener(mRUListActionAdapter);
      menuMRUList[i].setVisible(false);
      menuFile.add(menuMRUList[i]);
    }

    menuHelp.add(menuHelpAbout);
    menuBar.add(menuFile);
    menuSettings.add(menuAdvanced);
    menuSettings.add(menuOptions);
    menuBar.add(menuSettings);
    menuBar.add(menuHelp);
    this.setJMenuBar(menuBar);

    createProcessControlPanel();

    //
    //  Construct the main frame panel layout
    //
    panelCenter.setLayout(new BoxLayout(panelCenter, BoxLayout.Y_AXIS));
    panelCenter.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCenter.add(dataSetIDPanel.getPanel());
    panelCenter.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCenter.add(dataSetTypePanel.getPanel());
    panelCenter.add(Box.createRigidArea(FixedDim.x0_y10));
    panelCenter.add(Box.createVerticalGlue());
    panelCenter.add(panelProcess);
    panelCenter.add(Box.createVerticalGlue());
    panelCenter.add(Box.createRigidArea(FixedDim.x0_y10));

    mainPanel.add(panelCenter, BorderLayout.CENTER);
    mainPanel.add(statusBar, BorderLayout.SOUTH);

    //  add the context menu to all of the main window objects
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    mainPanel.addMouseListener(mouseAdapter);
    dataSetIDPanel.addMouseListener(mouseAdapter);
    dataSetTypePanel.addMouseListener(mouseAdapter);
    procCtlPanelSetup.addMouseListener(mouseAdapter);
    procCtlPanelPreProc.addMouseListener(mouseAdapter);
    procCtlPanelCoarseAlign.addMouseListener(mouseAdapter);
    procCtlPanelFiducialModel.addMouseListener(mouseAdapter);
    procCtlPanelAlignmentEst.addMouseListener(mouseAdapter);
    procCtlPanelTomogramPositioning.addMouseListener(mouseAdapter);
    procCtlPanelTomogramGeneration.addMouseListener(mouseAdapter);
    procCtlPanelTomogramCombination.addMouseListener(mouseAdapter);
    procCtlPanelPostProcessing.addMouseListener(mouseAdapter);
  }

  /**
   * Update the window with the MetaData parameters
   */
  public void updateDataParameters(ConstMetaData metaData) {
    dataSetIDPanel.setWorkingDirectory(metaData.getWorkingDirectory());
    dataSetIDPanel.setDataSetName(metaData.getFilesetName());
    dataSetIDPanel.setBackupDirectory(metaData.getBackupDirectory());
    dataSetTypePanel.setAxisType(metaData.getAxisType());
    dataSetTypePanel.setViewType(metaData.getViewType());
    dataSetTypePanel.setSectionType(metaData.getSectionType());

  }

  /**
   * Set the status bar with the file name of the data parameter file
   */
  public void setStatusBar(String text) {
    statusBar.setText("Parameter file: " + text);
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
      menuMRUList[i].setText(mRUList[i]);
      menuMRUList[i].setVisible(true);
    }
    for (int i = mRUList.length; i < nMRUFileMax; i++) {
      menuMRUList[i].setVisible(false);
    }
  }

  /**
   * Setup panel state control
   */
  public void setSetupState(ProcessState setupState) {
    procCtlPanelSetup.setState(setupState);
  }

  /**
   * Pre-processing panel state control
   */
  public void setPreProcState(ProcessState setupState) {
    procCtlPanelPreProc.setState(setupState);
  }

  /**
   * Coarse alignment panel state control
   */
  public void setCoarseAlignState(ProcessState setupState) {
    procCtlPanelCoarseAlign.setState(setupState);
  }

  /**
   * Fiducial model panel state control
   */
  public void setFiducialModelState(ProcessState setupState) {
    procCtlPanelFiducialModel.setState(setupState);
  }

  public void setAlignmentEstState(ProcessState setupState) {
    procCtlPanelAlignmentEst.setState(setupState);
  }

  public void setTomogramPositioningState(ProcessState setupState) {
    procCtlPanelTomogramPositioning.setState(setupState);
  }

  public void setTomogramGenerationState(ProcessState setupState) {
    procCtlPanelTomogramGeneration.setState(setupState);
  }

  public void setTomogramCombinationState(ProcessState setupState) {
    procCtlPanelTomogramCombination.setState(setupState);
  }

  public void setPostProcessingState(ProcessState setupState) {
    procCtlPanelPostProcessing.setState(setupState);
  }

  void menuFileOpen_actionPerformed(ActionEvent e) {
    //
    //  Open a file dialog box to get a
    //
    String cwd = System.getProperty("user.dir");

    //
    //  Open up the file chooser in current working directory
    //
    JFileChooser chooser = new JFileChooser(new File(cwd));
    chooser.setDialogTitle("Open etomo data file");
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);

    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File paramFile = chooser.getSelectedFile();
      applicationManager.openTestParamFile(paramFile);
    }
  }

  void menuFileSave_actionPerformed(ActionEvent e) {

    //  Check to see if there is a current parameter file chosen
    //  if not open a dialog box to select the name

    boolean haveTestParamFilename = true;
    if (applicationManager.getTestParamFile() == null) {
      haveTestParamFilename = getTestParamFilename();
    }
    if (haveTestParamFilename) {
      applicationManager.saveTestParamFile();
    }
  }

  public boolean getTestParamFilename() {
    String cwd = System.getProperty("user.dir");

    //  Open up the file chooser in current working directory
    JFileChooser chooser = new JFileChooser(new File(cwd));
    chooser.setDialogTitle("Save etomo data file");
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showSaveDialog(this);

    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return false;
    }
    applicationManager.setTestParamFile(chooser.getSelectedFile());
    return true;
  }

  void menuFileSaveAs_actionPerformed(ActionEvent e) {
    boolean haveTestParamFilename = getTestParamFilename();
    if (haveTestParamFilename) {
      applicationManager.saveTestParamFile();
    }
  }

  /**File | Exit action performed*/
  void menuFileExit_actionPerformed(ActionEvent e) {

    //  Check to see if we need to save any data
    if (applicationManager.exitProgram()) {
      System.exit(0);
    }
  }

  void menuFileMRUList_actionPerformed(ActionEvent e) {
    applicationManager.openTestParamFile(new File(e.getActionCommand()));
  }
  void menuOptions_actionPerformed(ActionEvent e) {
    //
    //  Open the options dialog box
    //
    System.out.println(e.paramString());
  }

  /**Help | About action performed*/
  void menuHelpAbout_actionPerformed(ActionEvent e) {
    MainFrame_AboutBox dlg = new MainFrame_AboutBox(this);
    Dimension dlgSize = dlg.getPreferredSize();
    Dimension frmSize = getSize();
    Point loc = getLocation();
    dlg.setLocation(
      (frmSize.width - dlgSize.width) / 2 + loc.x,
      (frmSize.height - dlgSize.height) / 2 + loc.y);
    dlg.setModal(true);
    dlg.show();
  }

  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent e) {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING) {
      menuFileExit_actionPerformed(null);
    }
  }

  //
  //  Process button actions
  //
  void buttonSetup_actionPerformed(ActionEvent e) {
    applicationManager.openSetupDialog();
  }
  void buttonPreProc_actionPerformed(ActionEvent e) {
    applicationManager.openPreProcDialog();
  }
  void buttonCoarseAlign_actionPerformed(ActionEvent e) {
    applicationManager.openCoarseAlignDialog();
  }
  void buttonFiducialModel_actionPerformed(ActionEvent e) {
    applicationManager.openFiducialModelDialog();
  }
  void buttonAlignmentEstimation_actionPerformed(ActionEvent e) {
    applicationManager.openAlignmentEstimationDialog();
  }
  void buttonTomogramPositioning_actionPerformed(ActionEvent e) {
    applicationManager.openTomogramPositioningDialog();
  }
  void buttonTomogramGeneration_actionPerformed(ActionEvent e) {
    applicationManager.openTomogramGenerationDialog();
  }
  void buttonTomogramCombination_actionPerformed(ActionEvent e) {
    applicationManager.openTomogramCombinationDialog();
  }
  void buttonPostProcessing_actionPerformed(ActionEvent e) {
    applicationManager.openPostProcessingDialog();
  }

  //  Right mouse button context menu
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(mainPanel, mouseEvent, "");
  }

  /**
   *  Build the process control panels
   */
  private void createProcessControlPanel() {
    procCtlPanelSetup.setButtonActionListener(
      new MainFrame_buttonSetupActionAdapter(this));
    procCtlPanelPreProc.setButtonActionListener(
      new MainFrame_buttonPreProcActionAdapter(this));
    procCtlPanelCoarseAlign.setButtonActionListener(
      new MainFrame_buttonCoarseAlignActionAdapter(this));
    procCtlPanelFiducialModel.setButtonActionListener(
      new MainFrame_buttonFiducialModelActionAdapter(this));
    procCtlPanelAlignmentEst.setButtonActionListener(
      new MainFrame_buttonAlignmentEstimationActionAdapter(this));
    procCtlPanelTomogramPositioning.setButtonActionListener(
      new MainFrame_buttonTomogramPositioningActionAdapter(this));
    procCtlPanelTomogramGeneration.setButtonActionListener(
      new MainFrame_buttonTomogramGenerationActionAdapter(this));
    procCtlPanelTomogramCombination.setButtonActionListener(
      new MainFrame_buttonTomogramCombinationActionAdapter(this));
    procCtlPanelPostProcessing.setButtonActionListener(
      new MainFrame_buttonPostProcessingActionAdapter(this));

    setToolTipText();

    panelProcess.setLayout(new BoxLayout(panelProcess, BoxLayout.X_AXIS));

    panelProcess.add(procCtlPanelSetup.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelPreProc.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelCoarseAlign.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelFiducialModel.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelAlignmentEst.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelTomogramPositioning.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelTomogramGeneration.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelTomogramCombination.getPanel());
    panelProcess.add(Box.createRigidArea(FixedDim.x20_y0));
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.add(procCtlPanelPostProcessing.getPanel());
    panelProcess.add(Box.createHorizontalGlue());

    panelProcess.setBorder(borderProcesses.getBorder());

  }

  /**
   * Initialize the tooltip text for the main window objects
   */
  private void setToolTipText() {
    procCtlPanelSetup.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the entry of the data location, name, tilt angle<br>specification and other dataset parameters.");
    procCtlPanelPreProc.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the conversion of Digital Micrograph files, specifying<br>the CCD eraser parameters and performing the corr-<br>correlation required for coarse alignment.");
    procCtlPanelCoarseAlign.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>the generation and examination of a coarse aligned<br>stack and the ability fix alignment problems using Midas.");
    procCtlPanelFiducialModel.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the construction of the fiducial model used to<br>develop the fine alignment of the projection images");
    procCtlPanelAlignmentEst.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the generation and examination of a finely aligned stack.");
    procCtlPanelTomogramPositioning.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the bounding and positioning of the tomogram<br>volume and creating the final alignment parameters.");
    procCtlPanelTomogramGeneration.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the generation of the final aligned stack and generation<br>and examination of the tomogram.");
    procCtlPanelTomogramCombination.setToolTipText(
      "<html>This process control panel is not yet complete<br>");
    procCtlPanelPostProcessing.setToolTipText(
      "<html>This process control panel is not yet complete<br>");

  }
}

//
//  Action adapters to handle file events
//
class MainFrame_menuFileOpenActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuFileOpenActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileOpen_actionPerformed(e);
  }
}

class MainFrame_menuFileSaveActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuFileSaveActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileSave_actionPerformed(e);
  }
}

class MainFrame_menuFileSaveAsActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuFileSaveAsActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileSaveAs_actionPerformed(e);
  }
}

class MainFrame_menuFileExitActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuFileExitActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileExit_actionPerformed(e);
  }
}
class MainFrame_menuFileMRUListActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuFileMRUListActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileMRUList_actionPerformed(e);
  }
}

class MainFrame_menuOptionsActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuOptionsActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuOptions_actionPerformed(e);
  }
}

class MainFrame_menuHelpAboutActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_menuHelpAboutActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuHelpAbout_actionPerformed(e);
  }
}

//
//  Action adapters to handle process panel events
//
class MainFrame_buttonSetupActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonSetupActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonSetup_actionPerformed(e);
  }
}

class MainFrame_buttonPreProcActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonPreProcActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonPreProc_actionPerformed(e);
  }
}

class MainFrame_buttonCoarseAlignActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonCoarseAlignActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonCoarseAlign_actionPerformed(e);
  }
}

class MainFrame_buttonFiducialModelActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonFiducialModelActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonFiducialModel_actionPerformed(e);
  }
}

class MainFrame_buttonAlignmentEstimationActionAdapter
  implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonAlignmentEstimationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonAlignmentEstimation_actionPerformed(e);
  }
}

class MainFrame_buttonTomogramPositioningActionAdapter
  implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonTomogramPositioningActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramPositioning_actionPerformed(e);
  }
}

class MainFrame_buttonTomogramGenerationActionAdapter
  implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonTomogramGenerationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramGeneration_actionPerformed(e);
  }
}

class MainFrame_buttonTomogramCombinationActionAdapter
  implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonTomogramCombinationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramCombination_actionPerformed(e);
  }
}

class MainFrame_buttonPostProcessingActionAdapter implements ActionListener {
  MainFrame adaptee;

  MainFrame_buttonPostProcessingActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonPostProcessing_actionPerformed(e);
  }
}
