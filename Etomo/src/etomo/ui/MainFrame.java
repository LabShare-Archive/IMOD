package etomo.ui;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;

import etomo.ApplicationManager;
import etomo.process.ProcessState;
import etomo.storage.EtomoFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.MetaData;
import etomo.type.ProcessTrack;

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
 * <p> Revision 2.11  2003/05/19 22:10:03  rickg
 * <p> Added new to file menu
 * <p> Added tomography guide and imod guide to help menu
 * <p> Restructured action handlers
 * <p>
 * <p> Revision 2.10  2003/05/19 04:54:18  rickg
 * <p> Added mnemonics for menus
 * <p>
 * <p> Revision 2.9  2003/05/15 22:25:14  rickg
 * <p> created separate setDividerLocation method to correctly update
 * <p> panel
 * <p>
 * <p> Revision 2.8  2003/05/15 20:21:22  rickg
 * <p> Added extra validation call hopefully to make sure divider gets rendered
 * <p> correctly
 * <p>
 * <p> Revision 2.7  2003/05/08 19:58:25  rickg
 * <p> Addd post processing state update on an updateAll... call
 * <p>
 * <p> Revision 2.6  2003/05/07 17:49:12  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Updated status bar
 * <p>
 * <p> Revision 2.5  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
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
  private JMenuItem menuFileNew = new JMenuItem("New", KeyEvent.VK_N);
  private JMenuItem menuFileOpen = new JMenuItem("Open...", KeyEvent.VK_O);
  private JMenuItem menuFileSave = new JMenuItem("Save", KeyEvent.VK_S);
  private JMenuItem menuFileSaveAs = new JMenuItem("Save As", KeyEvent.VK_A);
  private JMenuItem menuFileExit = new JMenuItem("Exit", KeyEvent.VK_X);
  private JMenuItem[] menuMRUList = new JMenuItem[nMRUFileMax];

  private JMenu menuOptions = new JMenu("Options");
  private JMenuItem menuAdvanced = new JMenuItem("Advanced", KeyEvent.VK_A);
  private JMenuItem menuSettings = new JMenuItem("Settings", KeyEvent.VK_S);
  private JMenuItem menuFitWindow = new JMenuItem("Fit Window", KeyEvent.VK_F);

  private JMenu menuHelp = new JMenu("Help");
  private JMenuItem menuTomoGuide =
    new JMenuItem("Tomography Guide", KeyEvent.VK_T);
  private JMenuItem menuImodGuide = new JMenuItem("Imod Guide", KeyEvent.VK_I);
  private JMenuItem menuHelpAbout = new JMenuItem("About", KeyEvent.VK_A);

  private JLabel statusBar = new JLabel("No data set loaded");

  private JPanel panelCenter = new JPanel();

  //  These panels get instantiated as needed
  private AxisProcessPanel axisPanelA;
  private ScrollPanel scrollA;
  private JScrollPane scrollPaneA;
  private AxisProcessPanel axisPanelB;
  private ScrollPanel scrollB;
  private JScrollPane scrollPaneB;

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
      scrollA = new ScrollPanel();
      scrollA.add(axisPanelA.getContainer());
      scrollPaneA = new JScrollPane(scrollA);
      panelCenter.add(scrollPaneA);
    }
    else {
      axisPanelA = new AxisProcessPanel(applicationManager, AxisID.FIRST);
      scrollA = new ScrollPanel();
      scrollA.add(axisPanelA.getContainer());
      scrollPaneA = new JScrollPane(scrollA);

      axisPanelB = new AxisProcessPanel(applicationManager, AxisID.SECOND);
      scrollB = new ScrollPanel();
      scrollB.add(axisPanelB.getContainer());
      scrollPaneB = new JScrollPane(scrollB);
      splitPane =
        new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPaneA, scrollPaneB);
      splitPane.setDividerLocation(0.5);
      splitPane.setOneTouchExpandable(true);
      panelCenter.add(splitPane);
    }
  }

  public void setDividerLocation(double value) {
    if (splitPane != null) {
      scrollPaneA.doLayout();
      scrollPaneB.doLayout();
      splitPane.doLayout();
      splitPane.revalidate();
      splitPane.validate();
      splitPane.setDividerLocation(value);
    }
  }

  /**
   * Show a blank processing panel
   */
  public void showBlankProcess(AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.eraseDialogPanel();
  }
  
  /**
   * Show the specified processing panel
   */
  public void showProcess(Container processPanel, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.replaceDialogPanel(processPanel);
  }

  /**
   * Set the progress bar to the beginning of determinant sequence
   * @param label
   * @param nSteps
   */
  public void setProgressBar(String label, int nSteps, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.startProgressBar(label);
    axisPanel.setProgressBarValue(0);
  }

  /**
   * Set the progress bar to the specified value
   * @param value
   * @param axisID
   */
  public void setProgressBarValue(int value, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setProgressBarValue(value);
  }

  /**
   *  Start the indeterminate progress bar on the specified axis 
   */
  public void startProgressBar(String name, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.startProgressBar(name);
  }

  /**
   * Stop the specified progress bar
   * @param axisID
   */
  public void stopProgressBar(AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.stopProgressBar();
  }

  /**
   * Set the status bar with the file name of the data parameter file
   */
  public void updateDataParameters(File paramFile, MetaData metaData) {
    StringBuffer buffer = new StringBuffer();
    if (metaData == null) {
      buffer.append("No data set loaded");
    }
    else {
      if (paramFile == null) {
        buffer.append("Data file: NOT SAVED");
      }
      else {
        buffer.append("Data file: " + paramFile.getAbsolutePath());
      }

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

  public boolean getTestParamFilename() {
    //  Open up the file chooser in current working directory
    JFileChooser chooser =
      new JFileChooser(new File(System.getProperty("user.dir")));
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

  void menuFileMRUListAction(ActionEvent event) {
    if (applicationManager
      .openTestParamFile(new File(event.getActionCommand()))) {
      applicationManager.openProcessingPanel();
    }
  }

  /**
   * Handle the options menu events
   * @param event
   */
  private void menuOptionsAction(ActionEvent event) {
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

  private void menuHelpAction(ActionEvent event) {
    if (event.getActionCommand().equals(menuTomoGuide.getActionCommand())) {
      String imodURL = "";
      try {
        imodURL =
          applicationManager.getIMODDirectory().toURL().toString() + "/html/";
      }
      catch (MalformedURLException except) {
        except.printStackTrace();
        System.err.println("Malformed URL:");
        System.err.println(applicationManager.getIMODDirectory().toString());
        return;
      }

      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomoguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menuImodGuide.getActionCommand())) {
      String imodURL = "";
      try {
        imodURL =
          applicationManager.getIMODDirectory().toURL().toString() + "/html/";
      }
      catch (MalformedURLException except) {
        except.printStackTrace();
        System.err.println("Malformed URL:");
        System.err.println(applicationManager.getIMODDirectory().toString());
        return;
      }

      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);

    }

    if (event.getActionCommand().equals(menuHelpAbout.getActionCommand())) {
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
  }

  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING) {
      menuFileExit.doClick();
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
    axisPanelA.setPostProcessingState(processTrack.getPostProcessingState());

  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setPreProcessingState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setPreProcState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setCoarseAlignState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setCoarseAlignState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setFiducialModelState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setFiducialModelState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setFineAlignmentState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setFineAlignmentState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setTomogramPositioningState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setTomogramPositioningState(state);
  }

  /**
   * 
   * @param state
   * @param axisID
   */
  public void setTomogramGenerationState(ProcessState state, AxisID axisID) {
    AxisProcessPanel axisPanel = mapAxis(axisID);
    axisPanel.setTomogramGenerationState(state);
  }

  /**
   * 
   * @param state
   */
  public void setTomogramCombinationState(ProcessState state) {
    axisPanelA.setTomogramCombinationState(state);
  }

  /**
   * 
   * @param state
   */
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

  /**
   * Convienence function to return a reference to the correct AxisProcessPanel
   * @param axisID
   * @return
   */
  private AxisProcessPanel mapAxis(AxisID axisID) {
    if(axisID == AxisID.SECOND) {
      return axisPanelB;
    }
    return axisPanelA;
  }
  
  /**
   * Set the advanced label to to the opposite state
   */
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
    //  Mnemonics for the main menu bar
    menuFile.setMnemonic(KeyEvent.VK_F);
    menuOptions.setMnemonic(KeyEvent.VK_O);
    menuHelp.setMnemonic(KeyEvent.VK_H);

    //  Accelerators
    menuSettings.setAccelerator(
      KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));

    menuAdvanced.setAccelerator(
      KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));

    menuFitWindow.setAccelerator(
      KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));

    //  Bind the menu items to their listeners
    FileActionListener fileActionListener = new FileActionListener(this);
    menuFileNew.addActionListener(fileActionListener);
    menuFileOpen.addActionListener(fileActionListener);
    menuFileSave.addActionListener(fileActionListener);
    menuFileSaveAs.addActionListener(fileActionListener);
    menuFileExit.addActionListener(fileActionListener);

    OptionsActionListener optionsActionListener =
      new OptionsActionListener(this);
    menuSettings.addActionListener(optionsActionListener);
    menuFitWindow.addActionListener(optionsActionListener);
    menuAdvanced.addActionListener(optionsActionListener);

    HelpActionListener helpActionListener = new HelpActionListener(this);
    menuTomoGuide.addActionListener(helpActionListener);
    menuImodGuide.addActionListener(helpActionListener);
    menuHelpAbout.addActionListener(helpActionListener);

    //  File menu
    menuFile.add(menuFileNew);
    menuFile.add(menuFileOpen);
    menuFile.add(menuFileSave);
    menuFile.add(menuFileSaveAs);
    menuFile.add(menuFileExit);
    menuFile.addSeparator();

    //  Initialize all of the MRU file menu items
    FileMRUListActionListener fileMRUListActionListener =
      new FileMRUListActionListener(this);
    for (int i = 0; i < nMRUFileMax; i++) {
      menuMRUList[i] = new JMenuItem();
      menuMRUList[i].addActionListener(fileMRUListActionListener);
      menuMRUList[i].setVisible(false);
      menuFile.add(menuMRUList[i]);
    }

    // Options menu
    menuOptions.add(menuSettings);
    setAdvancedLabel();
    menuOptions.add(menuAdvanced);
    menuOptions.add(menuFitWindow);

    // Help menu
    menuHelp.add(menuTomoGuide);
    menuHelp.add(menuImodGuide);
    menuHelp.add(menuHelpAbout);

    //  Construct menu bar
    menuBar.add(menuFile);
    menuBar.add(menuOptions);
    menuBar.add(menuHelp);
    setJMenuBar(menuBar);
  }

  /**
   * Handle File menu actions
   * @param event
   */
  private void menuFileAction(ActionEvent event) {
    if (event.getActionCommand().equals(menuFileNew.getActionCommand())) {
      applicationManager.openNewDataset();
    }

    if (event.getActionCommand().equals(menuFileOpen.getActionCommand())) {
      //  Open up the file chooser in current working directory
      JFileChooser chooser =
        new JFileChooser(new File(System.getProperty("user.dir")));
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

    if (event.getActionCommand().equals(menuFileSave.getActionCommand())) {
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

    if (event.getActionCommand().equals(menuFileSaveAs.getActionCommand())) {
      boolean haveTestParamFilename = getTestParamFilename();
      if (haveTestParamFilename) {
        applicationManager.saveTestParamFile();
      }
    }

    if (event.getActionCommand().equals(menuFileExit.getActionCommand())) {
      //  Check to see if we need to save any data
      if (applicationManager.exitProgram()) {
        System.exit(0);
      }
    }
  }

  //  File menu action listener
  class FileActionListener implements ActionListener {
    MainFrame adaptee;

    FileActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      adaptee.menuFileAction(event);
    }
  }

  //  MRU file list action listener
  class FileMRUListActionListener implements ActionListener {
    MainFrame adaptee;

    FileMRUListActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent e) {
      adaptee.menuFileMRUListAction(e);
    }
  }

  // Options file action listener
  class OptionsActionListener implements ActionListener {
    MainFrame adaptee;

    OptionsActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent e) {
      adaptee.menuOptionsAction(e);
    }
  }

  // Help file action listener
  class HelpActionListener implements ActionListener {
    MainFrame adaptee;

    HelpActionListener(MainFrame adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent e) {
      adaptee.menuHelpAction(e);
    }
  }

}
