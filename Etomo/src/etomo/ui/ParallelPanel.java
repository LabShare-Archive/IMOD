package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.CpuAdoc;
import etomo.storage.Network;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.type.PanelHeaderState;
import etomo.type.ProcessResultDisplay;
import etomo.util.HashedArray;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ParallelPanel implements Expandable {
  public static final String rcsid = "$Id$";

  private static final String TITLE = "Parallel Processing";
  public static final String RESUME_LABEL = "Resume";
  static final String FIELD_LABEL = "Parallel processing";
  static final String MAX_CPUS_STRING = ":  Maximum number of CPUs recommended is ";

  private static HashedArray maxCPUList = null;
  private static EtomoBoolean2 validAutodoc = null;

  private final EtomoPanel tablePanel = new EtomoPanel();
  private final EtomoPanel computerTablePanel = new EtomoPanel();
  private final EtomoPanel queueTablePanel = new EtomoPanel();
  private final LabeledTextField ltfCPUsSelected = new LabeledTextField(
      "CPUs: ");
  private final LabeledTextField ltfChunksFinished = new LabeledTextField(
      "Chunks finished: ");
  private final MultiLineButton btnResume = new MultiLineButton(RESUME_LABEL);
  private final MultiLineButton btnPause = new MultiLineButton("Pause");
  private final MultiLineButton btnSaveDefaults = new MultiLineButton(
      "Save As Defaults");
  private final SpacedPanel bodyPanel = SpacedPanel.getInstance();
  private final EtomoPanel rootPanel = new EtomoPanel();
  private final MultiLineButton btnRestartLoad = new MultiLineButton(
      "Restart Load");
  private final CheckBox cbQueues = new CheckBox("Use a cluster");

  private final BaseManager manager;
  private final AxisID axisID;
  private final ProcessorTable computerTable;
  private final ProcessorTable queueTable;
  private final Spinner sNice;
  private final PanelHeader header;
  private final AxisProcessPanel parent;
  private final int niceFloor;

  //private ParallelProcessMonitor parallelProcessMonitor = null;
  private boolean visible = true;
  private boolean open = true;
  private boolean pauseEnabled = false;
  private ProcesschunksParam processchunksParam = null;
  private ProcessResultDisplay processResultDisplay = null;

  private final boolean popupChunkWarnings;

  public static ParallelPanel getInstance(final BaseManager manager,
      final AxisID axisID, final PanelHeaderState state,
      final AxisProcessPanel parent, boolean popupChunkWarnings) {
    ParallelPanel instance = new ParallelPanel(manager, axisID, state, parent,
        popupChunkWarnings);
    instance.addListeners();
    return instance;
  }

  private ParallelPanel(final BaseManager manager, final AxisID axisID,
      final PanelHeaderState state, final AxisProcessPanel parent,
      boolean popupChunkWarnings) {
    this.manager = manager;
    this.axisID = axisID;
    this.parent = parent;
    this.popupChunkWarnings = popupChunkWarnings;
    //initialize table
    computerTable = new ProcessorTable(manager, this, axisID, false);
    queueTable = new ProcessorTable(manager, this, axisID, true);
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.X_AXIS));
    bodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel southPanel = SpacedPanel.getInstance();
    southPanel.setBoxLayout(BoxLayout.X_AXIS);
    //southPanel;
    southPanel.add(ltfCPUsSelected);
    southPanel.add(btnRestartLoad);
    //sNice
    niceFloor = CpuAdoc.INSTANCE.getMinNice(manager, axisID, manager
        .getPropertyUserDir());
    sNice = Spinner.getLabeledInstance("Nice: ", manager
        .getParallelProcessingDefaultNice(), niceFloor,
        ProcesschunksParam.NICE_CEILING);
    southPanel.add(sNice.getContainer());
    southPanel.add(btnPause);
    southPanel.add(btnResume);
    southPanel.add(btnSaveDefaults);
    //tablePanel
    buildTablePanel();
    //bodyPanel
    bodyPanel.addRigidArea();
    bodyPanel.add(tablePanel);
    bodyPanel.add(southPanel);
    if (Network.hasQueues(manager, axisID, manager.getPropertyUserDir())) {
      if (Network.hasComputers(manager, axisID, manager.getPropertyUserDir())) {
        JPanel clusterPanel = new JPanel();
        clusterPanel
            .setLayout(new BoxLayout(clusterPanel, BoxLayout.LINE_AXIS));
        clusterPanel.setAlignmentX(Component.RIGHT_ALIGNMENT);
        clusterPanel.add(cbQueues);
        bodyPanel.add(clusterPanel);
      }
      else {
        cbQueues.setSelected(true);
      }
    }
    //header
    header = PanelHeader.getMoreLessInstance(TITLE, this, null);
    //rootPanel
    rootPanel.add(header);
    rootPanel.add(bodyPanel.getContainer());
    ltfChunksFinished.setTextPreferredWidth(UIParameters.INSTANCE
        .getFourDigitWidth());
    ltfChunksFinished.setEditable(false);
    ltfCPUsSelected.setTextPreferredWidth(UIParameters.INSTANCE
        .getFourDigitWidth());
    ltfCPUsSelected.setEditable(false);
    btnPause.setEnabled(false);
    header.setState(state);
    selectTable();
    setToolTipText();
  }

  private void addListeners() {
    ParallelPanelActionListener actionListener = new ParallelPanelActionListener(
        this);
    btnResume.addActionListener(actionListener);
    btnPause.addActionListener(actionListener);
    btnSaveDefaults.addActionListener(actionListener);
    btnRestartLoad.addActionListener(actionListener);
    cbQueues.addActionListener(actionListener);
  }

  private void buildTablePanel() {
    tablePanel.removeAll();
    tablePanel.add(Box.createHorizontalGlue());
    tablePanel.add(computerTable.getContainer());
    tablePanel.add(queueTable.getContainer());
    tablePanel.add(Box.createHorizontalGlue());
  }

  public ParallelProgressDisplay getParallelProgressDisplay() {
    if (cbQueues.isSelected()) {
      return queueTable;
    }
    return computerTable;
  }

  public LoadDisplay getLoadDisplay() {
    if (cbQueues.isSelected()) {
      return queueTable;
    }
    return computerTable;
  }

  void setPauseEnabled(final boolean pauseEnabled) {
    this.pauseEnabled = pauseEnabled;
    if (btnPause == null) {
      return;
    }
    btnPause.setEnabled(pauseEnabled);
    parent.setParallelInUse(pauseEnabled);
  }

  void setCPUsSelected(final int cpusSelected) {
    ltfCPUsSelected.setText(cpusSelected);
  }

  String getCPUsSelected() {
    return ltfCPUsSelected.getText();
  }

  String getCPUsSelectedLabel() {
    return ltfCPUsSelected.getLabel();
  }

  Container getContainer() {
    return rootPanel;
  }

  public void setProcessInfo(final ProcesschunksParam processchunksParam,
      final ProcessResultDisplay processResultDisplay) {
    this.processchunksParam = processchunksParam;
    this.processResultDisplay = processResultDisplay;
    if (processchunksParam != null) {
      header.setText(TITLE + " - " + processchunksParam.getRootName());
    }
  }

  private void action(final ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnResume.getActionCommand()) {
      manager.resume(axisID, processchunksParam, processResultDisplay, null,
          rootPanel, null, popupChunkWarnings);
    }
    else if (command == btnPause.getActionCommand()) {
      manager.pause(axisID);
    }
    else if (command == btnSaveDefaults.getActionCommand()) {
      if (cbQueues.isSelected()) {
        manager.savePreferences(axisID, queueTable);
      }
      else {
        manager.savePreferences(axisID, computerTable);
      }
    }
    else if (command == btnRestartLoad.getActionCommand()) {
      if (cbQueues.isSelected()) {
        queueTable.restartLoadMonitor();
      }
      else {
        computerTable.restartLoadMonitor();
      }
    }
    else if (command == cbQueues.getActionCommand()) {
      selectTable();
    }
  }

  private void selectTable() {
    if (cbQueues.isSelected()) {
      computerTable.stopLoad();
      queueTable.startLoad();
      computerTable.setVisible(false);
      queueTable.setVisible(true);
      queueTable.msgCPUsSelectedChanged();
      sNice.setEnabled(false);
    }
    else {
      queueTable.stopLoad();
      computerTable.startLoad();
      queueTable.setVisible(false);
      computerTable.setVisible(true);
      computerTable.msgCPUsSelectedChanged();
      sNice.setEnabled(true);
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  void msgEndingProcess() {
    cbQueues.setEnabled(true);
  }

  void msgKillingProcess() {
    btnPause.setEnabled(false);
    btnResume.setEnabled(true);
  }

  void msgPausingProcess() {
    btnResume.setEnabled(true);
  }

  /**
   * If getting parameters, must not allow the user to change the current table
   * until that parameters have been used.
   * @param param
   */
  public void getResumeParameters(final ProcesschunksParam param) {
    cbQueues.setEnabled(false);
    param.setResume(true);
    param.setNice(sNice.getValue());
    param.setCPUNumber(ltfCPUsSelected.getText());
    param.resetMachineName();
    if (cbQueues.isSelected()) {
      queueTable.getParameters(param);
    }
    else {
      computerTable.getParameters(param);
    }
  }

  /**
   * If getting parameters, must not allow the user to change the current table
   * until that parameters have been used.
   * @param param
   */
  public boolean getParameters(final ProcesschunksParam param) {
    cbQueues.setEnabled(false);
    param.setNice(sNice.getValue());
    param.setCPUNumber(ltfCPUsSelected.getText());
    if (cbQueues.isSelected()) {
      queueTable.getParameters(param);
    }
    else {
      computerTable.getParameters(param);
    }
    String error = param.validate();
    if (error == null) {
      return true;
    }
    UIHarness.INSTANCE.openMessageDialog(manager,
        error
            + "  "
            + (cbQueues.isSelected() ? queueTable : computerTable)
                .getHelpMessage(), "Table Error", axisID);
    return false;
  }

  public void expand(final GlobalExpandButton button) {
  }

  /**
   * set the visible boolean based on whether the panel is visible
   * the body panel setVisible function was called by the header panel
   */
  public void expand(final ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      open = button.isExpanded();
      bodyPanel.setVisible(open);
      UIHarness.INSTANCE.pack(axisID, manager);
    }
    else if (header.equalsMoreLess(button)) {
      boolean expanded = button.isExpanded();
      btnSaveDefaults.setVisible(expanded);
      queueTable.setExpanded(expanded);
      computerTable.setExpanded(expanded);
      buildTablePanel();
    }
    else {
      return;
    }
  }

  void getHeaderState(final PanelHeaderState headerState) {
    header.getState(headerState);
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    ltfCPUsSelected.setToolTipText("Must be at least 1.");
    sNice
        .setToolTipText("Lower the value to run the processes at a higher priority.  Raise the value to run at a lower priority.");
    btnPause
        .setToolTipText("Finishes the processes that are currently running and then stops.");
    btnPause
        .setToolTipText("Finishes the processes that are currently running and then stops.");
    btnResume
        .setToolTipText("Starts the process but does not redo the chunks that are already completed.");
    btnSaveDefaults
        .setToolTipText("Saves the computers and number of CPUs selected.");
  }

  private static final class ParallelPanelActionListener implements
      ActionListener {
    private final ParallelPanel adaptee;

    private ParallelPanelActionListener(final ParallelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.71  2010/02/26 20:38:28  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.70  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.69  2010/01/11 23:59:00  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.68  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.67  2009/04/14 20:43:26  sueh
 * <p> bug# 1212 In getResumeParameters set cpuNumber.
 * <p>
 * <p> Revision 1.66  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.65  2009/01/20 20:18:46  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 1.64  2008/10/27 20:42:03  sueh
 * <p> bug# 1141 Added getCPUsSelected.  Removed
 * <p> getParameters(SplittiltParam) because it is not generic.
 * <p>
 * <p> Revision 1.63  2008/10/06 22:40:17  sueh
 * <p> bug# 1113 Removed pack, which is unecessary since table scrolling was
 * <p> removed.
 * <p>
 * <p> Revision 1.62  2008/09/30 22:01:39  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.61  2008/07/19 01:06:07  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.60  2008/05/03 00:51:39  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.59  2007/12/10 22:44:26  sueh
 * <p> bug# 1041 Put the label for the Resume button into a constant and made is
 * <p> public because it is being used in another package.
 * <p>
 * <p> Revision 1.58  2007/11/06 20:14:02  sueh
 * <p> bug# 1047
 * <p>
 * <p> Revision 1.57  2007/09/27 20:54:34  sueh
 * <p> bug# 1044 Added Queue processor table and the "Use a cluster" checkbox.
 * <p> Moved the implementation of ParallelPRogressDisplay and LoadAverageDisplay
 * <p> to ProcessorTable.
 * <p>
 * <p> Revision 1.56  2007/08/29 21:50:04  sueh
 * <p> bug# 1041 In action, passing rootPanel to manager.resume.
 * <p>
 * <p> Revision 1.55  2007/08/08 14:53:30  sueh
 * <p> bug# 834 Fixed the parallel processing check box label.
 * <p>
 * <p> Revision 1.54  2007/07/17 21:36:35  sueh
 * <p> bug# 1018 Moved getAutodoc, getMaxCPUs, and isValidAutodoc
 * <p> functionality to CpuAdoc.
 * <p>
 * <p> Revision 1.53  2007/05/26 00:32:56  sueh
 * <p> bug# 994 Using getInstance in ParallelPanel.  Added btnRestartLoad.
 * <p>
 * <p> Revision 1.52  2007/05/25 00:26:59  sueh
 * <p> bug# 994 Passing tooltip to msgLoadAverageFailed and
 * <p> msgStartingProcess.
 * <p>
 * <p> Revision 1.51  2007/05/22 21:20:58  sueh
 * <p> bug# 999 Removing "final" from functions because the whole class is
 * <p> already final.
 * <p>
 * <p> Revision 1.50  2007/05/21 22:30:25  sueh
 * <p> bug# 1000 Passing the manager to ParallelPanel.
 * <p>
 * <p> Revision 1.49  2007/05/21 18:11:14  sueh
 * <p> bug# 992 Added usersColumn.  Do not display Users column when
 * <p> usersColumn is false.
 * <p>
 * <p> Revision 1.48  2007/05/18 23:53:15  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.47  2007/05/03 00:46:52  sueh
 * <p> bug# 964 Placing the nice default in the manager so it can be changed.
 * <p>
 * <p> Revision 1.46  2007/05/01 22:29:22  sueh
 * <p> bug# 964 In LabeledSpinner, saving SpinnerNumberModel so that the
 * <p> maximum can be changed.
 * <p>
 * <p> Revision 1.45  2007/03/21 19:46:05  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.44  2007/03/15 21:47:29  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.43  2007/03/01 01:40:02  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.42  2007/02/09 00:51:04  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.41  2006/11/29 00:20:25  sueh
 * <p> bug# 934 Added end() to notify the load average threads when the manager exits.
 * <p>
 * <p> Revision 1.40  2006/11/18 00:49:24  sueh
 * <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
 * <p>
 * <p> Revision 1.39  2006/11/08 21:07:14  sueh
 * <p> bug# 936  SetLoadAverage:  Remove load15 and add users.
 * <p>
 * <p> Revision 1.38  2006/07/21 22:12:03  sueh
 * <p> bug# 901 Getting the calibration directory environment variable name from
 * <p> EnvironmentVariable.
 * <p>
 * <p> Revision 1.37  2006/07/21 19:12:29  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 1.36  2006/04/28 21:02:42  sueh
 * <p> bug# 787 PanelHeader:  Removed the member variable title, which was
 * <p> not used.
 * <p>
 * <p> Revision 1.35  2006/03/27 21:05:56  sueh
 * <p> bug# 836 Added DialogType to PanelHeader get instances functions so
 * <p> that the buttons in PanelHeader could save themselves.
 * <p>
 * <p> Revision 1.34  2006/02/15 18:53:05  sueh
 * <p> bug# 796 remove unnecessary import
 * <p>
 * <p> Revision 1.33  2006/02/09 23:06:02  sueh
 * <p> bug# 796 isValidAutodoc(): allowing parallel processing in windows
 * <p>
 * <p> Revision 1.32  2006/02/08 03:37:40  sueh
 * <p> bug# 796 added setCPUUsage().
 * <p>
 * <p> Revision 1.31  2006/02/06 23:00:34  sueh
 * <p> bug# 806 Autodoc is not validate if this is a Windows OS
 * <p>
 * <p> Revision 1.30  2006/01/26 22:05:38  sueh
 * <p> bug# 401 Added processResultDisplay parameters to all the functions associated
 * <p> with toggle buttons.
 * <p>
 * <p> Revision 1.29  2006/01/12 17:16:20  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.28  2006/01/11 22:19:07  sueh
 * <p> bug# 675 Replaced getUnformattedValue with getValue
 * <p>
 * <p> Revision 1.27  2005/12/14 20:57:40  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.26  2005/11/21 22:01:46  sueh
 * <p> bug# 761 In getParameters(ProcesschunksParam) pop up an error
 * <p> message and return false if ProcesschunksParam.validate() does not
 * <p> return null.
 * <p>
 * <p> Revision 1.25  2005/11/19 02:44:08  sueh
 * <p> bug# 744 Changed parallel processing display clearFailureReason
 * <p> function to msgStartingProcess.  This hides the implementation.  Turned
 * <p> msgInterruptingProcess into msgKillingProcess and msgPausingProcess,
 * <p> so that the kill could turn off the pause button.  Added
 * <p> msgStartingProcessOnSelectedComputers so that the failure reason
 * <p> could be erased when running processchunks.
 * <p>
 * <p> Revision 1.24  2005/11/14 22:14:08  sueh
 * <p> bug# 762 Made performAction() protected.
 * <p>
 * <p> Revision 1.23  2005/10/15 00:35:45  sueh
 * <p> bug# 532 Added isInUse().  In setPauseEnabled() calling
 * <p> AxisProcessPanel.setParallelInUse().
 * <p>
 * <p> Revision 1.22  2005/10/13 22:35:59  sueh
 * <p> bug# 532 Fixed parallel processing title.
 * <p>
 * <p> Revision 1.21  2005/10/12 22:45:24  sueh
 * <p> bug# Added validAutodoc and isValidAutodoc().
 * <p>
 * <p> Revision 1.20  2005/09/27 23:39:52  sueh
 * <p> bug# 532 Added boolean open to distinguish between making the panel
 * <p> visible and opening and closing it.  Added PanelHeaderState to the
 * <p> constructor.
 * <p>
 * <p> Revision 1.19  2005/09/22 21:25:11  sueh
 * <p> bug# 532 Moved the parallel process panel to AxisProcessPanel.
 * <p>
 * <p> Revision 1.18  2005/09/21 16:59:11  sueh
 * <p> bug# 532 Added member variable ProcesschunksParam to store the most
 * <p> recently used ProcesschunksParam.  Change the header title to show the
 * <p> root name of the ProcesschunksParam being run.  Remove the
 * <p> ParallelDialog dialog member variable.  Call manager.resume() instead of
 * <p> dialog.resume().  Add static function getAutodoc() to get the cpu autodoc.
 * <p> Add static function getMaxCPUs() to get and store the maximum
 * <p> recommend CPU for a process.  Make resetResults() public, so it can be
 * <p> called by ApplicationManager.  Added
 * <p> getResumeParameters(ProcesschunksParam) to update a used
 * <p> ProcesschunksParam from the parallel panel and set its resume option to
 * <p> true.
 * <p>
 * <p> Revision 1.17  2005/09/20 19:03:11  sueh
 * <p> bug# 532 Added code to expand() to handle the more/less button.  Moved
 * <p> the code to put padding around the processor table and add it to
 * <p> tablePanel into buildTablePanel() so it could be call from two places.  It is
 * <p> necessary to call buildTablePanel() when the more/less button is pressed.
 * <p>
 * <p> Revision 1.16  2005/09/19 16:40:21  sueh
 * <p> bug# 532 Changed member variables ParallelDialog parent to container.
 * <p> Added setContainer() to change the container each time the panel is
 * <p> placed on a dialog.
 * <p>
 * <p> Revision 1.15  2005/09/13 00:00:38  sueh
 * <p> bug# 532 Added a call to BaseManager.loadPreferences() to load defaults
 * <p> into the processor table.  Implemented Save Defaults button by adding a
 * <p> call to BaseManager.savePreferences() in performAction().
 * <p>
 * <p> Revision 1.14  2005/09/10 01:54:49  sueh
 * <p> bug# 532 Added clearFailureReason() so that the failure reason can be
 * <p> cleared when a new connection to the computer is attempted.
 * <p>
 * <p> Revision 1.13  2005/09/09 21:47:17  sueh
 * <p> bug# 532 moved call to clearLoadAverage() to msgLoadAverageFailed().
 * <p>
 * <p> Revision 1.12  2005/09/01 18:35:20  sueh
 * <p> bug# 532 ParallelPanel is no longer a doubleton.  It needs to be manager
 * <p> level.
 * <p>
 * <p> Revision 1.11  2005/09/01 18:02:10  sueh
 * <p> bug# 532 Added clearLoadAverage() to clear the load averages when the
 * <p> load average command fails.  Added a drop reason.
 * <p>
 * <p> Revision 1.10  2005/08/30 19:21:23  sueh
 * <p> bug# 532 Added parallel process monitor.  Added loadAverageFailed() to
 * <p> handle the failure of a w command.
 * <p>
 * <p> Revision 1.9  2005/08/22 18:08:08  sueh
 * <p> bug# 532 Made ParallelPane a doubleton.  Added start and
 * <p> stopLoadAverages
 * <p>
 * <p> Revision 1.8  2005/08/04 20:13:54  sueh
 * <p> bug# 532  Removed demo functions.  Added pack().
 * <p>
 * <p> Revision 1.7  2005/08/01 18:11:31  sueh
 * <p> bug# 532 Changed ProcessorTableRow.signalRestart() to addRestart.
 * <p> Added nice spinner.  Added getParameters(ProcesschunksParam).
 * <p>
 * <p> Revision 1.6  2005/07/29 00:54:29  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.5  2005/07/21 22:21:07  sueh
 * <p> bug# 532 Implementing setup and teardownParallelProgressDisplay() for
 * <p> ParallelProgressDisplay so that the pause button can function like a kill
 * <p> button.
 * <p>
 * <p> Revision 1.4  2005/07/11 23:12:20  sueh
 * <p> bug# 619 Removed the split, start, and kill buttons.  Split and start are
 * <p> now handled by the parent dialog.  Kill is handled by progress panel.
 * <p> Added randomization to be used by the demo monitor.  Added functions:
 * <p> buildRandomizerLists, getCpusSelected, resetResults,
 * <p> setEnabledResume, setVisible, signalRandomRestart,
 * <p> signalRandomSuccess, signalStartProgress.  Removed totalResults().
 * <p>
 * <p> Revision 1.3  2005/07/06 23:45:38  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 1.2  2005/07/01 23:04:26  sueh
 * <p> bug# 619 removed parent dialog from constructor
 * <p>
 * <p> Revision 1.1  2005/07/01 21:21:23  sueh
 * <p> bug# 619 Panel containing parallel processing
 * <p> </p>
 */
