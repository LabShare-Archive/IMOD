package etomo.ui;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.*;
import etomo.type.*;
import etomo.process.ProcessState;
import etomo.storage.EtomoFileFilter;

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

  private JMenu menuOptions = new JMenu("Options");
  private JMenuItem menuAdvanced = new JMenuItem("Advanced");
  private JMenuItem menuSettings = new JMenuItem("Settings");

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

    //  Menu bar text and adapters
    menuFileOpen.addActionListener(new menuFileOpenActionAdapter(this));
    menuFileSave.addActionListener(new menuFileSaveActionAdapter(this));
    menuFileSaveAs.addActionListener(new menuFileSaveAsActionAdapter(this));
    menuFileExit.addActionListener(new menuFileExitActionAdapter(this));

    menuSettings.addActionListener(new menuOptionsSettingsActionAdapter(this));

    menuHelpAbout.addActionListener(new menuHelpAboutActionAdapter(this));

    //  File menu
    menuFile.add(menuFileOpen);
    menuFile.add(menuFileSave);
    menuFile.add(menuFileSaveAs);
    menuFile.add(menuFileExit);
    menuFile.addSeparator();

    //  Initialize all of the MRU file menu items
    menuFileMRUListActionAdapter mRUListActionAdapter =
      new menuFileMRUListActionAdapter(this);
    for (int i = 0; i < nMRUFileMax; i++) {
      menuMRUList[i] = new JMenuItem();
      menuMRUList[i].addActionListener(mRUListActionAdapter);
      menuMRUList[i].setVisible(false);
      menuFile.add(menuMRUList[i]);
    }

    menuHelp.add(menuHelpAbout);
    menuBar.add(menuFile);
    menuOptions.add(menuSettings);
    menuOptions.add(menuAdvanced);
    menuBar.add(menuOptions);
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

  void menuFileOpenAction(ActionEvent e) {
    //
    //  Open up the file chooser in current working directory
    //
    JFileChooser chooser =
      new JFileChooser(new File(applicationManager.getWorkingDirectory()));
    EtomoFileFilter edfFilter = new EtomoFileFilter();
    chooser.setFileFilter(edfFilter);

    chooser.setDialogTitle("Open etomo data file");
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);

    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File paramFile = chooser.getSelectedFile();
      applicationManager.openTestParamFile(paramFile);
    }
  }

  void menuFileSaveAction(ActionEvent e) {

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
    String cwd = applicationManager.getWorkingDirectory();

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
    // If the file does not already have an extension appended then add an edf
    // extension
    File edfFile = chooser.getSelectedFile();
    String fileName = chooser.getSelectedFile().getName();
    if (fileName.indexOf(".") == -1) {
      edfFile = new File(chooser.getSelectedFile().getAbsolutePath() + ".edf");

    }
    applicationManager.setTestParamFile(edfFile);
    return true;
  }

  void menuFileSaveAsAction(ActionEvent e) {
    boolean haveTestParamFilename = getTestParamFilename();
    if (haveTestParamFilename) {
      applicationManager.saveTestParamFile();
    }
  }

  /**File | Exit action performed*/
  void menuFileExitAction(ActionEvent e) {

    //  Check to see if we need to save any data
    if (applicationManager.exitProgram()) {
      System.exit(0);
    }
  }

  void menuFileMRUListAction(ActionEvent e) {
    applicationManager.openTestParamFile(new File(e.getActionCommand()));
  }

  /**
   * Options/Settings action
   */
  void menuOptionsSettingsAction(ActionEvent e) {
    applicationManager.openSettingsDialog();
  }

  /**Help | About action performed*/
  void menuHelpAboutAction(ActionEvent e) {
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
      menuFileExitAction(null);
    }
  }

  //
  //  Process button actions
  //
  void buttonSetupAction(ActionEvent e) {
    applicationManager.openSetupDialog();
  }
  void buttonPreProcAction(ActionEvent e) {
    applicationManager.openPreProcDialog();
  }
  void buttonCoarseAlignAction(ActionEvent e) {
    applicationManager.openCoarseAlignDialog();
  }
  void buttonFiducialModelAction(ActionEvent e) {
    applicationManager.openFiducialModelDialog();
  }
  void buttonAlignmentEstimationAction(ActionEvent e) {
    applicationManager.openAlignmentEstimationDialog();
  }
  void buttonTomogramPositioningAction(ActionEvent e) {
    applicationManager.openTomogramPositioningDialog();
  }
  void buttonTomogramGenerationAction(ActionEvent e) {
    applicationManager.openTomogramGenerationDialog();
  }
  void buttonTomogramCombinationAction(ActionEvent e) {
    applicationManager.openTomogramCombinationDialog();
  }
  void buttonPostProcessingAction(ActionEvent e) {
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
      new buttonSetupActionAdapter(this));
    procCtlPanelPreProc.setButtonActionListener(
      new buttonPreProcActionAdapter(this));
    procCtlPanelCoarseAlign.setButtonActionListener(
      new buttonCoarseAlignActionAdapter(this));
    procCtlPanelFiducialModel.setButtonActionListener(
      new buttonFiducialModelActionAdapter(this));
    procCtlPanelAlignmentEst.setButtonActionListener(
      new buttonAlignmentEstimationActionAdapter(this));
    procCtlPanelTomogramPositioning.setButtonActionListener(
      new buttonTomogramPositioningActionAdapter(this));
    procCtlPanelTomogramGeneration.setButtonActionListener(
      new buttonTomogramGenerationActionAdapter(this));
    procCtlPanelTomogramCombination.setButtonActionListener(
      new buttonTomogramCombinationActionAdapter(this));
    procCtlPanelPostProcessing.setButtonActionListener(
      new buttonPostProcessingActionAdapter(this));

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
      "<html>This process control panel opens a dialog box allowing<br>the generation and examination of a coarse aligned<br>stack and the ability to fix alignment problems using Midas.");
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
class menuFileOpenActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuFileOpenActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileOpenAction(e);
  }
}

class menuFileSaveActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuFileSaveActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileSaveAction(e);
  }
}

class menuFileSaveAsActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuFileSaveAsActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileSaveAsAction(e);
  }
}

class menuFileExitActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuFileExitActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileExitAction(e);
  }
}

class menuOptionsSettingsActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuOptionsSettingsActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuOptionsSettingsAction(e);
  }
}

class menuFileMRUListActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuFileMRUListActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuFileMRUListAction(e);
  }
}

class menuHelpAboutActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuHelpAboutActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuHelpAboutAction(e);
  }
}

//
//  Action adapters to handle process panel events
//
class buttonSetupActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonSetupActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonSetupAction(e);
  }
}

class buttonPreProcActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonPreProcActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonPreProcAction(e);
  }
}

class buttonCoarseAlignActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonCoarseAlignActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonCoarseAlignAction(e);
  }
}

class buttonFiducialModelActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonFiducialModelActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonFiducialModelAction(e);
  }
}

class buttonAlignmentEstimationActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonAlignmentEstimationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonAlignmentEstimationAction(e);
  }
}

class buttonTomogramPositioningActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonTomogramPositioningActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramPositioningAction(e);
  }
}

class buttonTomogramGenerationActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonTomogramGenerationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramGenerationAction(e);
  }
}

class buttonTomogramCombinationActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonTomogramCombinationActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonTomogramCombinationAction(e);
  }
}

class buttonPostProcessingActionAdapter implements ActionListener {
  MainFrame adaptee;

  buttonPostProcessingActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.buttonPostProcessingAction(e);
  }
}
