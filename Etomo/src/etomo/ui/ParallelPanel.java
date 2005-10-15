package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import etomo.BaseManager;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.SplittiltParam;
import etomo.process.LoadAverageMonitor;
import etomo.process.ParallelProcessMonitor;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.PanelHeaderState;
import etomo.type.ProcessName;
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
public final class ParallelPanel implements ParallelProgressDisplay, Expandable, LoadAverageDisplay {
  public static final String rcsid = "$Id$";

  static final String TITLE = "Parallel Processing";
  static final String MAX_CPUS_STRING = ":  Maximum number of CPUs recommended is ";
  
  private static HashedArray maxCPUList = null;
  private static EtomoBoolean2 validAutodoc = null;
  
  private final JPanel tablePanel = new JPanel();
  private final ParallelPanelActionListener actionListener = new ParallelPanelActionListener(
      this);
  private final LabeledTextField ltfCPUsSelected = new LabeledTextField(
  "CPUs selected: ");
  private final LabeledTextField ltfChunksFinished = new LabeledTextField(
  "Chunks finished: ");
  private final MultiLineButton btnResume = new MultiLineButton("Resume");
  private final MultiLineButton btnPause = new MultiLineButton("Pause");
  private final MultiLineButton btnSaveDefaults = new MultiLineButton("Save As Defaults");
  private final SpacedPanel bodyPanel = new SpacedPanel();
  private final JPanel rootPanel = new JPanel();
  
  private final BaseManager manager;
  private final AxisID axisID;
  private final ProcessorTable processorTable;
  private final LabeledSpinner nice;
  private final PanelHeader header;
  private final AxisProcessPanel parent;
  
  private LoadAverageMonitor loadAverageMonitor = null;
  private ParallelProcessMonitor parallelProcessMonitor = null;
  private boolean visible = true;
  private boolean open = true;
  private boolean pauseEnabled = false;
  private ProcesschunksParam processchunksParam = null;

  public ParallelPanel(BaseManager manager, AxisID axisID, PanelHeaderState state, AxisProcessPanel parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.parent = parent;
    //initialize table
    //boolean expanded = PanelHeader.isMoreLessExpanded(state);
    processorTable = new ProcessorTable(this, axisID);//, expanded);
    //set listeners
    btnResume.addActionListener(actionListener);
    btnPause.addActionListener(actionListener);
    btnSaveDefaults.addActionListener(actionListener);
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.X_AXIS));
    bodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel southPanel = new SpacedPanel();
    southPanel.setBoxLayout(BoxLayout.X_AXIS);
    //southPanel;
    southPanel.add(ltfCPUsSelected);
    SpinnerModel model = new SpinnerNumberModel(
        ProcesschunksParam.NICE_DEFAULT, ProcesschunksParam.NICE_FLOOR,
        ProcesschunksParam.NICE_CEILING, 1);
    nice = new LabeledSpinner("Nice: ", model);
    southPanel.add(nice);
    southPanel.add(btnPause);
    southPanel.add(btnResume);
    southPanel.add(btnSaveDefaults);
    //tablePanel
    buildTablePanel();
    //bodyPanel
    bodyPanel.addRigidArea();
    bodyPanel.add(tablePanel);
    bodyPanel.add(southPanel);
    //header
    header = PanelHeader.getMoreLessInstance(
        BaseScreenState.PARALLEL_HEADER_GROUP, TITLE, this);
    //rootPanel
    rootPanel.add(header.getContainer());
    rootPanel.add(bodyPanel.getContainer());
    ltfChunksFinished.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfChunksFinished.setEditable(false);
    ltfCPUsSelected.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfCPUsSelected.setEditable(false);
    processorTable.msgCPUsSelectedChanged();
    btnPause.setEnabled(false);
    header.setState(state);
  }
  
  private final void buildTablePanel() {
    tablePanel.removeAll();
    tablePanel.add(Box.createHorizontalGlue());
    tablePanel.add(processorTable.getContainer());
    tablePanel.add(Box.createHorizontalGlue());
  }
  
  public final void start() {
    processorTable.startGetLoadAverage(this);
  }
  
  public final void stop() {
    processorTable.stopGetLoadAverage(this);
  }
  
  final void startGetLoadAverage(String computer) {
    manager.startGetLoadAverage(this, computer);
  }
  
  final void stopGetLoadAverage(String computer) {
    if (loadAverageMonitor != null) {
      manager.stopGetLoadAverage(this, computer);
    }
  }
  
  public final LoadAverageMonitor getLoadAverageMonitor() {
    if (loadAverageMonitor == null) {
      loadAverageMonitor = new LoadAverageMonitor(this);
    }
    return loadAverageMonitor;
  }
  
  public final void setLoadAverage(String computer, double load1, double load5, double load15) {
    processorTable.setLoadAverage(computer, load1, load5, load15);
  }
  
  public final void setPauseEnabled(boolean pauseEnabled) {
    this.pauseEnabled = pauseEnabled;
    if (btnPause == null) {
      return;
    }
    btnPause.setEnabled(pauseEnabled);
    parent.setParallelInUse(pauseEnabled);
  }
  
  public final boolean isInUse() {
    System.out.println(btnPause.isEnabled());
    return btnPause.isEnabled();
  }
  
  public final void msgCPUsSelectedChanged(int cpusSelected) {
    ltfCPUsSelected.setText(cpusSelected);
  }

  public final Container getContainer() {
    return rootPanel;
  }
  
  public final void setProcesschunksParam(ProcesschunksParam processchunksParam) {
    this.processchunksParam = processchunksParam;
    if (processchunksParam != null) {
      header.setText(TITLE + " - " + processchunksParam.getRootName());
    }
  }

  private final void performAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnResume.getText()) {
      manager.resume(axisID, processchunksParam);
    }
    else if (command == btnPause.getText()) {
      manager.pause(axisID);
    }
    else if (command == btnSaveDefaults.getText()) {
      manager.savePreferences(axisID, processorTable);
    }
  }

  final void setVisible(boolean visible) {
    if (this.visible == visible) {
      return;
    }
    this.visible = visible;
    rootPanel.setVisible(visible);
  }
  
  static final boolean isValidAutodoc(AxisID axisID) {
    if (validAutodoc != null) {
      return validAutodoc.is();
    }
    validAutodoc = new EtomoBoolean2();
    Autodoc autodoc = getAutodoc(axisID);
    if (autodoc != null && autodoc.isSectionExists(ProcessorTable.SECTION_TYPE)) {
      validAutodoc.set(true);
    }
    return validAutodoc.is();
  }
  
  /**
   * get cpu autodoc
   * @param axisID
   * @return
   */
  static final Autodoc getAutodoc(AxisID axisID) {
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.getInstance(Autodoc.CPU, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    if (autodoc == null) {
      System.err
          .println("Unable to display the rows of the processor table./nMissing $IMOD_CALIB_DIR/cpu.adoc file./nSee $IMOD_DIR/autodoc/cpu.adoc.");
    }
    return autodoc;
  }
  
  /**
   * get the maximum recommended CPUs for the process
   * 
   * saves the value in a hashed array if it is found
   * @param axisID
   * @param process
   * @return
   */
  static final ConstEtomoNumber getMaxCPUs(AxisID axisID, ProcessName processName) {
    String process = processName.toString();
    EtomoNumber maxCPUs = null;
    //look for maxCPUs in maxCPUList
    if (maxCPUList != null) {
      maxCPUs = (EtomoNumber) maxCPUList.get(process);
    }
    if (maxCPUs != null) {
      return maxCPUs;
    }
    //look for maxCPUs in cpu.adoc
    maxCPUs = new EtomoNumber();
    Autodoc autodoc = getAutodoc(axisID);
    if (autodoc == null) {
      return maxCPUs;
    }
    Attribute maxAttribute = autodoc.getAttribute("max");
    try {
      maxAttribute.getAttribute(process).getUnformattedValue(maxCPUs);
    }
    catch (NullPointerException e) {
    }
    //add maxCPUs to maxCPUList
    if (maxCPUs != null && !maxCPUs.isNull()) {
      if (maxCPUList != null) {
        maxCPUList.add(process, maxCPUs);
      }
    }
    return maxCPUs;
  }

  public final int getCPUsSelected() {
    return processorTable.getCPUsSelected();
  }

  public final void addRestart(String computer) {
    processorTable.addRestart(computer);
  }
  
  public final void msgDropped(String computer, String reason) {
    processorTable.msgDropped(computer, reason);
  }

  public final void addSuccess(String computer) {
    processorTable.addSuccess(computer);
  }
  
  public final void resetResults() {
    processorTable.resetResults();
  }
  
  public final void msgInterruptingProcess() {
    btnResume.setEnabled(true);
  }
  
  public final boolean getParameters(SplittiltParam param) {
    ConstEtomoNumber numMachines = param.setNumMachines(ltfCPUsSelected
        .getText());
    if (!numMachines.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(ltfCPUsSelected.getLabel() + " "
          + numMachines.getInvalidReason() + "\n"
          + processorTable.getHelpMessage(), "Table Error", axisID);
      return false;
    }
    return true;
  }
  
  public final void getResumeParameters(ProcesschunksParam param) {
    param.setResume(true);
    param.setNice(nice.getValue());
    param.resetMachineName();
    processorTable.getParameters(param);
 }
  
  public final void getParameters(ProcesschunksParam param) {
     param.setNice(nice.getValue());
     processorTable.getParameters(param);
  }
  
  public final void pack() {
    if (!visible || !open) {
      return;
    }
    processorTable.pack();
  }
  
  /**
   * set the visible boolean based on whether the panel is visible
   * the body panel setVisible function was called by the header panel
   */
  public final void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      open = button.isExpanded();
      bodyPanel.setVisible(open);
    }
    else if (header.equalsMoreLess(button)) {
      if (processorTable == null) {
        return;
      }
      boolean expanded = button.isExpanded();
      btnSaveDefaults.setVisible(expanded);
      processorTable.setExpanded(expanded);
      buildTablePanel();
    }
    else {
      return;
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }
  
  public final void msgLoadAverageFailed(String computer, String reason) {
    if (parallelProcessMonitor != null) {
      parallelProcessMonitor.drop(computer);
    }
    processorTable.clearLoadAverage(computer, reason);
  }
  
  public void clearFailureReason(String computer) {
    processorTable.clearFailureReason(computer);
  }
  
  /**
   * sets parallelProcessMonitor with a monitor which is monitoring a parallel
   * process associated with this ParallelProgressDisplay
   * @param ParallelProcessMonitor
   */
  public final void setParallelProcessMonitor(ParallelProcessMonitor parallelProcessMonitor) {
    this.parallelProcessMonitor = parallelProcessMonitor;
  }
  
  public final void getHeaderState(PanelHeaderState headerState) {
    header.getState(headerState);
  }
  
  public final void setHeaderState(PanelHeaderState headerState) {
    header.setState(headerState);
  }
  

  private final class ParallelPanelActionListener implements ActionListener {
    ParallelPanel adaptee;

    ParallelPanelActionListener(ParallelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.performAction(event);
    }
  }
}
/**
 * <p> $Log$
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