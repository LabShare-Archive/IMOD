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
 * <p> Revision 2.4  2003/03/20 17:42:18  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.3  2003/01/29 15:18:19  rickg
 * <p> Added combine state setter
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:43  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 23:51:23  rickg
 * <p> Added a split pane manager to the mane window for dual
 * <p> axis layout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.9.2.2  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.9.2.1  2003/01/11 00:45:27  rickg
 * <p> Added app icon
 * <p>
 * <p> Revision 1.9  2002/12/18 20:45:32  rickg
 * <p> Advanced menu implemented
 * <p>
 * <p> Revision 1.8  2002/12/11 21:28:29  rickg
 * <p> Implemented repaint method, doesn't work well
 * <p>
 * <p> Revision 1.7  2002/12/11 00:37:26  rickg
 * <p> Added handler for options/settings menu
 * <p>
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
  private JMenuItem menuFitWindow = new JMenuItem("Fit Window");

  private JMenu menuHelp = new JMenu("Help");
  private JMenuItem menuHelpAbout = new JMenuItem("About");

  private JLabel statusBar = new JLabel("No data set loaded");

  private JPanel panelCenter = new JPanel();

  //  These panels get instantiated as needed
  private AxisProcessPanel axisPanelA;
  private AxisProcessPanel axisPanelB;
  private JSplitPane splitPane;

  //  Application manager object
  private ApplicationManager applicationManager;

  /**
   * Main window constructor.  This sets up the menus and status line.
   */
  public MainFrame(ApplicationManager appManager) {
    applicationManager = appManager;

    enableEvents(AWTEvent.WINDOW_EVENT_MASK);

    ImageIcon iconEtomo =
      new ImageIcon(ClassLoader.getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());

    setTitle("eTomo");

    mainPanel = (JPanel) getContentPane();
    mainPanel.setLayout(new BorderLayout());

    createMenus();

    //  Construct the main frame panel layout
    panelCenter.setLayout(new BoxLayout(panelCenter, BoxLayout.X_AXIS));
    mainPanel.add(panelCenter, BorderLayout.CENTER);
    mainPanel.add(statusBar, BorderLayout.SOUTH);

    //  add the context menu to all of the main window objects
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    mainPanel.addMouseListener(mouseAdapter);
  }

  /**
   * Open the setup panel
   */
  public void openSetupPanel(SetupDialog setupDialog) {
    panelCenter.removeAll();
    panelCenter.add(setupDialog.getContainer());
    pack();
  }

  /**
   * Show the processing panel for the requested AxisType
   */
  public void showProcessingPanel(AxisType axisType) {

    //  Delete any existing panels
    axisPanelA = null;
    axisPanelB = null;

    panelCenter.removeAll();
    if (axisType == AxisType.SINGLE_AXIS) {
      axisPanelA = new AxisProcessPanel(applicationManager, AxisID.ONLY);
      ScrollPanel scrollA = new ScrollPanel();
      scrollA.add(axisPanelA.getContainer());
      JScrollPane scrollPaneA = new JScrollPane(scrollA);
      panelCenter.add(scrollPaneA);
    }
    else {
      axisPanelA = new AxisProcessPanel(applicationManager, AxisID.FIRST);
      ScrollPanel scrollA = new ScrollPanel();
      scrollA.add(axisPanelA.getContainer());
      JScrollPane scrollPaneA = new JScrollPane(scrollA);

      axisPanelB = new AxisProcessPanel(applicationManager, AxisID.SECOND);
      ScrollPanel scrollB = new ScrollPanel();
      scrollB.add(axisPanelB.getContainer());
      JScrollPane scrollPaneB = new JScrollPane(scrollB);

      splitPane =
        new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPaneA, scrollPaneB);
      //      panelCenter.add(scrollPaneA);
      //      panelCenter.add(scrollPaneB);
      splitPane.setDividerLocation(0.5);
      panelCenter.add(splitPane);
    }
  }

  /**
   * Show a blank processing panel
   */
  public void showBlankProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.eraseDialogPanel();
    }
    else {
      axisPanelA.eraseDialogPanel();
    }
  }

  /**
   * Show the specified processing panel
   */
  public void showProcess(Container processPanel, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.replaceDialogPanel(processPanel);
    }
    else {
      axisPanelA.replaceDialogPanel(processPanel);
    }
  }

  /**
   * 
   */
  public void startProgressBar(String name, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.startProgressBar(name);
    }
    else {
      axisPanelA.startProgressBar(name);
    }
  }

  public void stopProgressBar(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.stopProgressBar();
    }
    else {
      axisPanelA.stopProgressBar();
    }
  }

  /**
   * Set the status bar with the file name of the data parameter file
   */
  public void updateDataParameters(MetaData metaData) {
    StringBuffer buffer = new StringBuffer();
    if (metaData == null) {
      buffer.append("No data set loaded");
    }
    else {
      buffer.append("Data file: ");
      buffer.append(metaData.getWorkingDirectory());
      buffer.append("/");
      buffer.append(metaData.getDatasetName());
      buffer.append("   Source: ");
      buffer.append(metaData.getDataSource().toString());
      buffer.append("   Axis type: ");
      buffer.append(metaData.getAxisType().toString());
      buffer.append("   Tomograms: ");
      buffer.append(metaData.getSectionType().toString());
    }
    statusBar.setText(buffer.toString());
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
      if (applicationManager.openTestParamFile(paramFile)) {
        applicationManager.openProcessingPanel();
      }
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
    if (applicationManager.openTestParamFile(new File(e.getActionCommand()))) {
      applicationManager.openProcessingPanel();
    }
  }

  /**
   * Options/Settings action
   */
  void menuOptionsAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(menuSettings.getActionCommand())) {
      applicationManager.openSettingsDialog();
    }
    else if (command.equals(menuAdvanced.getActionCommand())) {
      applicationManager.setAdvanced(!applicationManager.getAdvanced());
      setAdvancedLabel();
    }
    else if (command.equals(menuFitWindow.getActionCommand())) {

      pack();
      if (applicationManager.isDualAxis()) {
        splitPane.resetToPreferredSizes();
      }
    }
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

  //  Right mouse button context menu
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(mainPanel, mouseEvent, "");
  }

  /**
   * Update the state of all the process control panels
   * @param processTrack the process track object containing the state to be
   * displayed
   */
  public void updateAllProcessingStates(ProcessTrack processTrack) {
    if (axisPanelA == null) {
      return;
    }

    axisPanelA.setPreProcState(processTrack.getPreProcessingState(AxisID.ONLY));
    axisPanelA.setCoarseAlignState(
      processTrack.getCoarseAlignmentState(AxisID.ONLY));
    axisPanelA.setFiducialModelState(
      processTrack.getFiducialModelState(AxisID.ONLY));
    axisPanelA.setFineAlignmentState(
      processTrack.getFineAlignmentState(AxisID.ONLY));
    axisPanelA.setTomogramPositioningState(
      processTrack.getTomogramPositioningState(AxisID.ONLY));
    axisPanelA.setTomogramGenerationState(
      processTrack.getTomogramGenerationState(AxisID.ONLY));
    axisPanelA.setTomogramCombinationState(
      processTrack.getTomogramCombinationState());
    if (applicationManager.isDualAxis()) {
      axisPanelB.setPreProcState(
        processTrack.getPreProcessingState(AxisID.SECOND));
      axisPanelB.setCoarseAlignState(
        processTrack.getCoarseAlignmentState(AxisID.SECOND));
      axisPanelB.setFiducialModelState(
        processTrack.getFiducialModelState(AxisID.SECOND));
      axisPanelB.setFineAlignmentState(
        processTrack.getFineAlignmentState(AxisID.SECOND));
      axisPanelB.setTomogramPositioningState(
        processTrack.getTomogramPositioningState(AxisID.SECOND));
      axisPanelB.setTomogramGenerationState(
        processTrack.getTomogramGenerationState(AxisID.SECOND));
    }
  }

  public void setPreProcessingState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setPreProcState(state);
    }
    else {
      axisPanelA.setPreProcState(state);
    }
  }

  public void setCoarseAlignState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setCoarseAlignState(state);
    }
    else {
      axisPanelA.setCoarseAlignState(state);
    }
  }

  public void setFiducialModelState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setFiducialModelState(state);
    }
    else {
      axisPanelA.setFiducialModelState(state);
    }
  }

  public void setFineAlignmentState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setFineAlignmentState(state);
    }
    else {
      axisPanelA.setFineAlignmentState(state);
    }
  }

  public void setTomogramPositioningState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setTomogramPositioningState(state);
    }
    else {
      axisPanelA.setTomogramPositioningState(state);
    }
  }

  public void setTomogramGenerationState(ProcessState state, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      axisPanelB.setTomogramGenerationState(state);
    }
    else {
      axisPanelA.setTomogramGenerationState(state);
    }
  }

  public void setTomogramCombinationState(ProcessState state) {
    axisPanelA.setTomogramCombinationState(state);
  }

  public void setPostProcessingState(ProcessState state) {
    axisPanelA.setPostProcessingState(state);
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

  private void setAdvancedLabel() {
    if (applicationManager.getAdvanced()) {
      menuAdvanced.setText("Advanced X");
    }
    else {
      menuAdvanced.setText("Advanced");
    }
  }

  /**
   * Create the menus for the main window
   */
  private void createMenus() {
    //  Menu bar text and adapters
    menuFileOpen.addActionListener(new menuFileOpenActionAdapter(this));
    menuFileSave.addActionListener(new menuFileSaveActionAdapter(this));
    menuFileSaveAs.addActionListener(new menuFileSaveAsActionAdapter(this));
    menuFileExit.addActionListener(new menuFileExitActionAdapter(this));

    menuSettings.addActionListener(new menuOptionsActionAdapter(this));
    menuFitWindow.addActionListener(new menuOptionsActionAdapter(this));
    menuAdvanced.addActionListener(new menuOptionsActionAdapter(this));
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
    setAdvancedLabel();
    menuOptions.add(menuAdvanced);
    menuOptions.add(menuFitWindow);
    menuBar.add(menuOptions);
    menuBar.add(menuHelp);
    setJMenuBar(menuBar);
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

class menuOptionsActionAdapter implements ActionListener {
  MainFrame adaptee;

  menuOptionsActionAdapter(MainFrame adaptee) {
    this.adaptee = adaptee;
  }
  public void actionPerformed(ActionEvent e) {
    adaptee.menuOptionsAction(e);
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
