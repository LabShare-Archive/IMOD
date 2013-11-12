package etomo.ui.swing;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.IntermittentCommand;
import etomo.comscript.ProcesschunksParam;
import etomo.process.LoadAverageMonitor;
import etomo.process.LoadMonitor;
import etomo.process.QueuechunkLoadMonitor;
import etomo.storage.CpuAdoc;
import etomo.storage.Node;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstEtomoVersion;

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
abstract class ProcessorTable implements Storable, ParallelProgressDisplay, LoadDisplay,
    Viewable {
  public static final String rcsid = "$Id$";

  private final JPanel rootPanel = new JPanel();
  private JPanel tablePanel;
  private GridBagLayout layout = null;
  private GridBagConstraints constraints = null;
  private final HeaderCell header1Computer = new HeaderCell();
  private final HeaderCell header1NumberCPUs;
  private final HeaderCell header1CPUType = new HeaderCell("CPU Type");
  private final HeaderCell header1Speed = new HeaderCell("Speed");
  private final HeaderCell header1RAM = new HeaderCell("RAM");
  private final HeaderCell header1OS = new HeaderCell("OS");
  private final HeaderCell header1Restarts = new HeaderCell("Restarts");
  private final HeaderCell header1Finished = new HeaderCell("Finished");
  private final HeaderCell header1Failure = new HeaderCell("Failure");
  private final HeaderCell header2Computer = new HeaderCell();
  private final HeaderCell header2NumberCPUsUsed = new HeaderCell("Used");
  private final HeaderCell header2NumberCPUsMax = new HeaderCell("Max.");
  private final HeaderCell header2CPUType = new HeaderCell();
  private final HeaderCell header2Speed = new HeaderCell();
  private final HeaderCell header2RAM = new HeaderCell();
  private final HeaderCell header2OS = new HeaderCell();
  private final HeaderCell header2Restarts = new HeaderCell();
  private final HeaderCell header2Finished = new HeaderCell("Chunks");
  private final HeaderCell header2Failure = new HeaderCell("Reason");

  private final RowList rowList = new RowList();
  private final Viewport viewport = new Viewport(this, EtomoDirector.INSTANCE
      .getUserConfiguration().getParallelTableSize().getInt(), null, null, null,
      "Processor");
  private final ParallelPanel parent;
  final AxisID axisID;
  final BaseManager manager;
  private final LoadMonitor loadMonitor;

  private boolean numberColumn = false;
  private boolean typeColumn = false;
  private boolean speedColumn = false;
  private boolean memoryColumn = false;
  private boolean osColumn = false;
  private String speedUnits = null;
  private String memoryUnits = null;
  private boolean scrolling = false;
  private boolean expanded = false;
  private boolean stopped = true;

  abstract int getSize();

  abstract Node getNode(int index);

  abstract ProcessorTableRow createProcessorTableRow(ProcessorTable processorTable,
      Node node, int numRowsInTable);

  abstract String getHeader1ComputerText();

  abstract void addHeader1Load(JPanel tablePanel, GridBagLayout layout,
      GridBagConstraints constraints);

  abstract void addHeader1Users(JPanel tablePanel, GridBagLayout layout,
      GridBagConstraints constraints);

  abstract void addHeader2Load(JPanel tablePanel, GridBagLayout layout,
      GridBagConstraints constraints);

  abstract void addHeader2Users(JPanel tablePanel, GridBagLayout layout,
      GridBagConstraints constraints);

  abstract boolean useUsersColumn();

  abstract IntermittentCommand getIntermittentCommand(String computer);

  abstract void setHeaderLoadToolTipText();

  abstract void setHeaderUsersToolTipText();

  abstract boolean isExcludeNode(Node node);

  abstract boolean isNiceable();

  abstract String getStorePrepend();

  abstract String getLoadPrepend(final ConstEtomoVersion version);

  abstract void initRow(ProcessorTableRow row);

  abstract String getNoCpusSelectedErrorMessage();

  ProcessorTable(final BaseManager manager, final ParallelPanel parent,
      final AxisID axisID, final boolean displayQueues) {
    this.manager = manager;
    this.parent = parent;
    this.axisID = axisID;
    header1NumberCPUs = new HeaderCell(getheader1NumberCPUsTitle());
    if (displayQueues) {
      loadMonitor = new QueuechunkLoadMonitor(this, axisID, manager);
    }
    else {
      loadMonitor = new LoadAverageMonitor(this, axisID, manager);
    }
  }

  String getheader1NumberCPUsTitle() {
    return "# CPUs";
  }

  void createTable() {
    expanded = true;
    initTable();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.X_AXIS));
    rootPanel.setBorder(LineBorder.createBlackLineBorder());
    build();
  }

  void setExpanded(final boolean expanded) {
    if (this.expanded == expanded) {
      return;
    }
    this.expanded = expanded;
    rowList.setContractedIndex(expanded);
    build();
  }

  void setVisible(final boolean visible) {
    rootPanel.setVisible(visible);
  }

  void build() {
    rootPanel.removeAll();
    buildTable();
    rootPanel.add(tablePanel);
    rootPanel.add(viewport.getPagingPanel());
    // configure
    UIHarness.INSTANCE.repaintWindow(manager);
  }

  private void initTable() {
    speedUnits = CpuAdoc.INSTANCE.getSpeedUnits(manager, axisID,
        manager.getPropertyUserDir());
    memoryUnits = CpuAdoc.INSTANCE.getMemoryUnits(manager, axisID,
        manager.getPropertyUserDir());
    // loop through the nodes
    // loop on nodes
    int size = getSize();
    for (int i = 0; i < size; i++) {
      // get the node
      Node node = getNode(i);
      // exclude any node with the "exclude-interface" attribute set to the
      // current interface
      if (node != null && !node.isExcludedInterface(manager.getInterfaceType())
          && (!node.isExcludedUser(System.getProperty("user.name")))
          && !isExcludeNode(node)) {
        if (enableNumberColumn(node)) {
          numberColumn = true;
        }
        // get the type attribute
        // set typeColumn to true if an type attribute is returned
        typeColumn = !node.isTypeEmpty();
        // get the speed attribute
        // set speedColumn to true if an speed attribute is returned
        speedColumn = !node.isSpeedEmpty();
        // get the memory attribute
        // set memoryColumn to true if an memory attribute is returned
        memoryColumn = !node.isMemoryEmpty();
        // get the os attribute
        // set osColumn to true if an os attribute is returned
        osColumn = !node.isOsEmpty();
        // create the row
        ProcessorTableRow row = createProcessorTableRow(this, node, size);
        initRow(row);
        // add the row to the rows HashedArray
        rowList.add(row);
      }
    }
    // set custom header text
    header1Computer.setText(getHeader1ComputerText());
    if (speedColumn) {
      header2Speed.setText(speedUnits);
    }
    if (memoryColumn) {
      header2RAM.setText(memoryUnits);
    }
    // try {
    ParameterStore parameterStore = EtomoDirector.INSTANCE.getParameterStore();
    parameterStore.load(this);
    setToolTipText();
    if (rowList.size() == 1) {
      rowList.setSelected(0, true);
      rowList.enableSelectionField(0, false);
    }
  }

  boolean enableNumberColumn(final Node node) {
    return node.getNumber() > 1;
  }

  public void msgViewportPaged() {
    build();
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void buildTable() {
    tablePanel = new JPanel();
    layout = new GridBagLayout();
    constraints = new GridBagConstraints();
    // build table
    tablePanel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    // header row 1
    constraints.anchor = GridBagConstraints.CENTER;
    // constraints.weightx = 1.0;
    constraints.weightx = 0.0;
    // constraints.weighty = 1.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1Computer.add(tablePanel, layout, constraints);
    if (numberColumn && expanded) {
      constraints.gridwidth = 2;
    }
    header1NumberCPUs.add(tablePanel, layout, constraints);
    addHeader1Load(tablePanel, layout, constraints);
    addHeader1Users(tablePanel, layout, constraints);
    if (useTypeColumn()) {
      header1CPUType.add(tablePanel, layout, constraints);
    }
    if (useSpeedColumn()) {
      header1Speed.add(tablePanel, layout, constraints);
    }
    if (useMemoryColumn()) {
      header1RAM.add(tablePanel, layout, constraints);
    }
    if (useOSColumn()) {
      header1OS.add(tablePanel, layout, constraints);
    }
    header1Restarts.add(tablePanel, layout, constraints);
    header1Finished.add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1Failure.add(tablePanel, layout, constraints);
    // header row 2
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header2Computer.add(tablePanel, layout, constraints);
    header2NumberCPUsUsed.add(tablePanel, layout, constraints);
    if (useNumberColumn()) {
      header2NumberCPUsMax.add(tablePanel, layout, constraints);
    }
    addHeader2Load(tablePanel, layout, constraints);
    addHeader2Users(tablePanel, layout, constraints);
    if (useTypeColumn()) {
      header2CPUType.add(tablePanel, layout, constraints);
    }
    if (useSpeedColumn()) {
      header2Speed.add(tablePanel, layout, constraints);
    }
    if (useMemoryColumn()) {
      header2RAM.add(tablePanel, layout, constraints);
    }
    if (useOSColumn()) {
      header2OS.add(tablePanel, layout, constraints);
    }
    header2Restarts.add(tablePanel, layout, constraints);
    header2Finished.add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2Failure.add(tablePanel, layout, constraints);
    // add rows to the table
    viewport.msgViewableChanged();
    rowList.display(expanded, viewport);
  }

  Container getContainer() {
    return rootPanel;
  }

  JPanel getTablePanel() {
    return tablePanel;
  }

  GridBagLayout getTableLayout() {
    return layout;
  }

  GridBagConstraints getTableConstraints() {
    return constraints;
  }

  public void resetResults() {
    rowList.resetResults();
  }

  int getTotalSuccesses() {
    return rowList.getTotalSuccesses();
  }

  void msgCPUsSelectedChanged() {
    parent.setCPUsSelected(getCPUsSelected());
  }

  public void msgEndingProcess() {
    parent.msgEndingProcess();
  }

  public void msgKillingProcess() {
    parent.msgKillingProcess();
  }

  public void msgProcessStarted() {
    parent.msgProcessStarted();
  }

  public void msgPausingProcess() {
    parent.msgPausingProcess();
  }

  int getCPUsSelected() {
    return rowList.getCPUsSelected();
  }

  void restartLoadMonitor() {
    loadMonitor.restart();
  }

  boolean useNumberColumn() {
    return numberColumn && expanded;
  }

  boolean useTypeColumn() {
    return typeColumn && expanded;
  }

  boolean useSpeedColumn() {
    return speedColumn && expanded;
  }

  boolean useMemoryColumn() {
    return memoryColumn && expanded;
  }

  boolean useOSColumn() {
    return osColumn && expanded;
  }

  int getFirstSelectedIndex() {
    return rowList.getFirstSelectedIndex();
  }

  int getNextSelectedIndex(final int lastIndex) {
    return rowList.getNextSelectedIndex(lastIndex);
  }

  void getParameters(final ProcesschunksParam param) {
    rowList.getParameters(param);
  }

  String getFirstSelectedComputer() {
    return rowList.getComputer(rowList.getFirstSelectedIndex());
  }

  public int size() {
    return rowList.size(expanded);
  }

  private ProcessorTableRow getRow(final String computer) {
    return rowList.get(computer);
  }

  public void addRestart(final String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addRestart();
  }

  public void addSuccess(final String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addSuccess();
  }

  public void setComputerMap(final Map<String, String> computerMap) {
    rowList.setComputerMap(computerMap);
    parent.msgComputerMapSet();
  }

  public void msgDropped(final String computer, final String reason) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.msgDropped(reason);
  }

  String getHelpMessage() {
    return "Click on check boxes in the " + header1Computer.getText()
        + " column and use the spinner in the " + header1NumberCPUs.getText() + " "
        + header2NumberCPUsUsed.getText() + " column where available.";
  }

  public void startLoad() {
    stopped = false;
    for (int i = 0; i < rowList.size(); i++) {
      manager.startLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  private IntermittentCommand getIntermittentCommand(final int index) {
    ProcessorTableRow row = (ProcessorTableRow) rowList.get(index);
    String computer = row.getComputer();
    return getIntermittentCommand(computer);
  }

  /**
   * sets parallelProcessMonitor with a monitor which is monitoring a parallel
   * process associated with this ParallelProgressDisplay
   * @param ParallelProcessMonitor
   */
  /* public void setParallelProcessMonitor( final ParallelProcessMonitor
   * parallelProcessMonitor) { parent.setParallelProcessMonitor(parallelProcessMonitor); } */

  public void endLoad() {
    stopped = true;
    for (int i = 0; i < rowList.size(); i++) {
      manager.endLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  public void stopLoad() {
    stopped = true;
    for (int i = 0; i < rowList.size(); i++) {
      manager.stopLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  public boolean isStopped() {
    return stopped;
  }

  public void setLoad(final String computer, final double load1, final double load5,
      final int users, final String usersTooltip) {
    ((ProcessorTableRow) rowList.get(computer))
        .setLoad(load1, load5, users, usersTooltip);
  }

  public void setLoad(final String computer, final String[] loadArray) {
    ((ProcessorTableRow) rowList.get(computer)).setLoad(loadArray);
  }

  public void setCPUUsage(final String computer, final double cpuUsage,
      final ConstEtomoNumber numberOfProcessors) {
    ((ProcessorTableRow) rowList.get(computer)).setCPUUsage(cpuUsage, numberOfProcessors);
  }

  /**
   * Clears the load from the display.  Does not ask the monitor to
   * drop the computer because processchunks handles this very well, and it is
   * possible that the computer may still be available.
   */
  public void msgLoadFailed(final String computer, final String reason,
      final String tooltip) {
    ((ProcessorTableRow) rowList.get(computer)).clearLoad(reason, tooltip);
  }

  public void msgStartingProcessOnSelectedComputers() {
    clearFailureReason(true);
  }

  /**
   * Clear failure reason, if failure reason equals failureReason1 or 2.  This means
   * that intermittent processes only clear their own messages.  This is useful
   * for restarting an intermittent process without losing the processchunks
   * failure reason.
   */
  public void msgStartingProcess(final String computer, final String failureReason1,
      String failureReason2) {
    ((ProcessorTableRow) rowList.get(computer)).clearFailureReason(failureReason1,
        failureReason2);
  }

  void clearFailureReason(final boolean selectedComputers) {
    rowList.clearFailureReason(selectedComputers);
  }

  public void store(final Properties props) {
    store(props, "");
  }

  public void store(final Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = getStorePrepend();
    }
    else {
      prepend += "." + getStorePrepend();
    }
    rowList.store(props, prepend);
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public final void load(final Properties props) {
    load(props, "");
  }

  void load(final Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = getLoadPrepend(parent.getVersion());
    }
    else {
      prepend += "." + getLoadPrepend(parent.getVersion());
    }
    rowList.load(props, prepend);
  }

  final boolean isScrolling() {
    return scrolling;
  }

  final private void setToolTipText() {
    String text;
    text = "Select computers to use for parallel processing.";
    header1Computer.setToolTipText(text);
    header2Computer.setToolTipText(text);
    text = "Select the number of CPUs to use for each computer.";
    header1NumberCPUs.setToolTipText(text);
    header2NumberCPUsUsed.setToolTipText(text);
    if (numberColumn) {
      header2NumberCPUsMax
          .setToolTipText("The maximum number of CPUs available on each computer.");
    }
    setHeaderLoadToolTipText();
    setHeaderUsersToolTipText();
    text = "The number of times processes failed on each computer.";
    header1Restarts.setToolTipText(text);
    header2Restarts.setToolTipText(text);
    text = "The number of processes each computer completed for a distributed process.";
    header1Finished.setToolTipText(text);
    header2Finished.setToolTipText(text);
    if (header1CPUType != null) {
      text = "The CPU type of each computer.";
      header1CPUType.setToolTipText(text);
      header2CPUType.setToolTipText(text);
    }
    if (header1Speed != null) {
      text = "The speed of each computer.";
      header1Speed.setToolTipText(text);
      header2Speed.setToolTipText(text);
    }
    if (header1RAM != null) {
      text = "The amount of RAM in each computer.";
      header1RAM.setToolTipText(text);
      header2RAM.setToolTipText(text);
    }
    if (header1OS != null) {
      text = "The operating system of each computer.";
      header1OS.setToolTipText(text);
      header2OS.setToolTipText(text);
    }
    text = "Reason for a failure by the load average or a process";
    header1Failure.setToolTipText(text);
    header2Failure.setToolTipText(text);
  }

  private static final class RowList {
    private final List list = new ArrayList();
    // Contracted index for use when the table is not expanded..
    private final List contractedIndex = new ArrayList();

    private RowList() {
    }

    /**
     * Changes the selected computers and CPUs to match computerMap.
     * @param computerMap
     */
    private void setComputerMap(final Map<String, String> computerMap) {
      if (computerMap == null || computerMap.isEmpty()) {
        return;
      }
      for (int i = 0; i < list.size(); i++) {
        // First unselect a computer. Then select the computer if it is in
        // computerMap.
        ProcessorTableRow row = get(i);
        row.setSelected(false);
        String key = row.getComputer();
        if (computerMap.containsKey(key)) {
          row.setSelected(true);
          row.setCPUsSelected(computerMap.get(key));
        }
      }
    }

    private void add(final ProcessorTableRow row) {
      list.add(row);
    }

    private void display(final boolean expanded, final Viewport viewport) {
      for (int i = 0; i < size(expanded); i++) {
        ProcessorTableRow row;
        if (expanded) {
          row = get(i);
        }
        else {
          row = (ProcessorTableRow) contractedIndex.get(i);
        }
        row.deleteRow();
        row.setColumns();
        row.display(i, viewport);
      }
    }

    private int size(final boolean expanded) {
      if (expanded) {
        return size();
      }
      return contractedIndex.size();
    }

    private int size() {
      return list.size();
    }

    private ProcessorTableRow get(final int index) {
      if (index == -1) {
        return null;
      }
      return (ProcessorTableRow) list.get(index);
    }

    private ProcessorTableRow get(final String computer) {
      for (int i = 0; i < size(); i++) {
        ProcessorTableRow row = get(i);
        if (row.equals(computer)) {
          return row;
        }
      }
      return null;
    }

    private void setContractedIndex(final boolean expanded) {
      contractedIndex.clear();
      if (!expanded) {
        for (int i = 0; i < size(); i++) {
          ProcessorTableRow row = get(i);
          if (row.isSelected()) {
            contractedIndex.add(row);
          }
        }
      }
    }

    private void getParameters(final ProcesschunksParam param) {
      for (int i = 0; i < size(); i++) {
        get(i).getParameters(param);
      }
    }

    private void setSelected(final int index, final boolean selected) {
      get(index).setSelected(selected);
    }

    private void enableSelectionField(final int index, final boolean enabled) {
      get(index).enableSelectionField(enabled);
    }

    private void resetResults() {
      for (int i = 0; i < size(); i++) {
        get(i).resetResults();
      }
    }

    private int getTotalSuccesses() {
      int successes = 0;
      for (int i = 0; i < size(); i++) {
        successes += get(i).getSuccesses();
      }
      return successes;
    }

    private int getCPUsSelected() {
      int cpusSelected = 0;
      for (int i = 0; i < size(); i++) {
        cpusSelected += get(i).getCPUsSelected();
      }
      return cpusSelected;
    }

    private int getFirstSelectedIndex() {
      for (int i = 0; i < size(); i++) {
        if (get(i).isSelected()) {
          return i;
        }
      }
      return -1;
    }

    private int getNextSelectedIndex(final int lastIndex) {
      for (int i = lastIndex + 1; i < size(); i++) {
        if (get(i).isSelected()) {
          return i;
        }
      }
      return -1;
    }

    private String getComputer(final int index) {
      ProcessorTableRow row = get(index);
      if (row == null) {
        return null;
      }
      return row.getComputer();
    }

    private void clearFailureReason(final boolean selectedComputers) {
      for (int i = 0; i < size(); i++) {
        ProcessorTableRow row = get(i);
        if (!selectedComputers || row.isSelected()) {
          row.clearFailureReason();
        }
      }
    }

    private void store(final Properties props, final String prepend) {
      for (int i = 0; i < size(); i++) {
        get(i).store(props, prepend);
      }
    }

    private void load(final Properties props, final String prepend) {
      for (int i = 0; i < size(); i++) {
        get(i).load(props, prepend);
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2011/07/22 19:55:51  sueh
 * <p> Bug# 1515 In initTable, replaced to select a single row.
 * <p>
 * <p> Revision 1.7  2011/07/18 23:13:31  sueh
 * <p> Bug# 1515 Added getNoCpusSelectedErrorMessage
 * <p>
 * <p> Revision 1.6  2011/07/18 22:45:52  sueh
 * <p> Bug# 1515 In initTable always selecting a single row.  Removed isSelectOnlyRow, which is no longer
 * <p> being called.
 * <p>
 * <p> Revision 1.5  2011/06/25 03:11:41  sueh
 * <p> Bug# 1499 In RowList.get(int) and getComputer(int) handling index == -1.
 * <p>
 * <p> Revision 1.4  2011/05/19 16:33:13  sueh
 * <p> bug# 1473 In ParameterStore.load, removed unused throw.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:20:14  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:15:08  sueh
 * <p> bug# 1422 Handling the three types of tables (queue, CPU, and GPU) by
 * <p> putting the differences into child classes.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.58  2010/10/12 02:44:42  sueh
 * <p> bug# 1391 Added setComputerMap.
 * <p>
 * <p> Revision 1.57  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.56  2010/01/11 23:59:01  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.55  2009/04/20 20:07:51  sueh
 * <p> bug# 1192 Added setComputerMap and RowList.setComputerMap, which
 * <p> changes the row to match computerMap.
 * <p>
 * <p> Revision 1.54  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.53  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.52  2008/10/07 16:43:35  sueh
 * <p> bug# 1113 Improved names:  changed Viewport.msgViewportMoved to
 * <p> msgViewportPaged.
 * <p>
 * <p> Revision 1.51  2008/10/06 22:43:25  sueh
 * <p> bug# 1113 Removed pack, which is unecessary since table scrolling was
 * <p> removed.  Moved rows into RowList.  Used a regular array instead of HashedArray, since the hash was never used and the table is not very big.  Implemented Viewable.  Added a Viewport.  Got rid of scrolling and
 * <p> all functions associated with scrolling.
 * <p>
 * <p> Revision 1.50  2008/07/19 01:07:35  sueh
 * <p> bug# 1125 sharing # CPUs header.
 * <p>
 * <p> Revision 1.49  2008/01/31 20:30:37  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.48  2007/09/27 21:02:47  sueh
 * <p> bug# 1044 Added a displayQueues mode.  Implementing
 * <p> ParallelProgressDisplay and LoadDisplay.
 * <p>
 * <p> Revision 1.47  2007/09/07 00:28:13  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.46  2007/07/30 18:54:16  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.45  2007/07/17 21:44:05  sueh
 * <p> bug# 1018 Adding all cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 1.44  2007/05/25 00:28:19  sueh
 * <p> bug# 964 Added tooltip to clearLoadAverage.  Added a second reason to
 * <p> clearFailureReason.
 * <p>
 * <p> Revision 1.43  2007/05/22 21:21:40  sueh
 * <p> bug# 999 Checking CpuAdoc.users before adding a row.
 * <p>
 * <p> Revision 1.42  2007/05/21 22:31:28  sueh
 * <p> bug# 1000 In initTable(), excluding sections based on exclude-interface.
 * <p>
 * <p> Revision 1.41  2007/05/21 18:11:22  sueh
 * <p> bug# 992 Added usersColumn.  Do not display Users column when
 * <p> usersColumn is false.
 * <p>
 * <p> Revision 1.40  2007/03/21 19:46:38  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.39  2007/03/15 21:48:22  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.38  2007/02/09 00:51:54  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.37  2007/02/05 23:40:45  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.36  2006/11/29 00:20:49  sueh
 * <p> bug# 934 Added endGetLoadAverage().  Does the same thing as
 * <p> stopGetLoadAverage(), but also removes the monitor.  Uses when a manager
 * <p> exits.
 * <p>
 * <p> Revision 1.35  2006/11/18 00:49:50  sueh
 * <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
 * <p>
 * <p> Revision 1.34  2006/11/15 21:24:34  sueh
 * <p> bug# 872 Saving to .etomo with ParameterStore.
 * <p>
 * <p> Revision 1.33  2006/11/08 21:08:01  sueh
 * <p> bug# 936:  Remove the 15 Min. column and add the Users column.
 * <p>
 * <p> Revision 1.32  2006/11/07 22:53:36  sueh
 * <p> bug# 954 Added tooltips
 * <p>
 * <p> Revision 1.31  2006/02/09 23:40:36  sueh
 * <p> bug# 796 In Windows an exception was caused by tool tips being set
 * <p>
 * <p> Revision 1.30  2006/02/08 03:38:11  sueh
 * <p> bug# 796 Use cpu usage instead of load average for windows.
 * <p>
 * <p> Revision 1.29  2006/01/12 17:37:12  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.28  2006/01/11 22:32:32  sueh
 * <p> bug# 675 replaced Attribute.getUnformattedValue with getValue
 * <p>
 * <p> Revision 1.27  2005/12/23 02:18:44  sueh
 * <p> bug# 675 Renamed Section.getFirstSectionLocation to getSectionLocation.
 * <p> Removed getSection(sectionLocation).  Changed nextSection so it gets the
 * <p> current section and increments.
 * <p>
 * <p> Revision 1.26  2005/12/16 01:46:01  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.25  2005/12/14 20:58:10  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.24  2005/11/19 02:45:10  sueh
 * <p> bug# 744 Added clearFailureReason(boolean selectedComputers).
 * <p>
 * <p> Revision 1.23  2005/11/14 22:16:36  sueh
 * <p> bug# 762 The internal class is now accessing protected functions instead
 * <p> of private variables.
 * <p>
 * <p> Revision 1.22  2005/11/10 18:16:57  sueh
 * <p> bug# 733 Made the class public so that its constance SECTION_TYPE
 * <p> could be used.
 * <p>
 * <p> Revision 1.21  2005/11/04 01:03:46  sueh
 * <p> bug# 732 Cleaned up RunPack.run().  Moved the code from
 * <p> setMaximumSize() and setPreferredSize() into RunPack.run().  Made
 * <p> getWidth() look at the second header instead of a row which might not be
 * <p> displayed.  Made getHeight() look at each header and row to get an
 * <p> accurate size.  Removed calcMaximumHeight() because it was
 * <p> unecessary.  Correct the scrollbar width to get the width right and remove
 * <p> the horizontal scrollbar.  Recalculating size and resizing each time
 * <p> RunPack.run() is run, so that fit works on the table.  Made
 * <p> ProcessorTableRow keep track of whether it was displayed to that a fit of
 * <p> a contracted table will show all displayed rows.  Took the maximum
 * <p> number of rows into account in getHeight().  Only setting preferred size
 * <p> when the number of row is greater then MAXIMUM_ROWS.
 * <p>
 * <p> Revision 1.20  2005/10/12 22:46:17  sueh
 * <p> bug# 532 Moved the section type string to a public static final, so it can
 * <p> be used by ParallelPanel.
 * <p>
 * <p> Revision 1.19  2005/09/27 23:44:29  sueh
 * <p> bug# 532 Moved loading prefererences to the end of initTable() from
 * <p> ParallelPanel.
 * <p>
 * <p> Revision 1.18  2005/09/22 21:29:36  sueh
 * <p> bug# 532 Removed restartsError.  Taking error level from ParallelPanel in
 * <p> ProcessorTableRow.
 * <p>
 * <p> Revision 1.17  2005/09/21 17:04:33  sueh
 * <p> bug# 532 getting autodoc from ParallelPanel.getAutodoc().  Fix getWidth()
 * <p> so that it gets the width from a row which is currently displayed (rows that
 * <p> are not selected are not displayed when the table is contracted).
 * <p>
 * <p> Revision 1.16  2005/09/20 19:12:40  sueh
 * <p> bug# 532  Implementing setExpanded(boolean).  Add boolean expanded.
 * <p> When expanded is false, only display fields and rows which the user
 * <p> would look at while running a process.  Divide createTable() into initTable()
 * <p> and buildTable().  Create fields and get audodoc data in initTable().  Then
 * <p> use buildTable() to build the table according to the autodoc data and
 * <p> expanded.  Change getMinimumHeight() to use expanded.  SetExpanded()
 * <p> runs buildTable().  All table fields are member variables.
 * <p>
 * <p> Revision 1.15  2005/09/13 00:01:26  sueh
 * <p> bug# 532 Implemented Storable.
 * <p>
 * <p> Revision 1.14  2005/09/10 01:54:59  sueh
 * <p> bug# 532 Added clearFailureReason() so that the failure reason can be
 * <p> cleared when a new connection to the computer is attempted.
 * <p>
 * <p> Revision 1.13  2005/09/09 21:47:45  sueh
 * <p> bug# 532 Passed reason string to clearLoadAverage().
 * <p>
 * <p> Revision 1.12  2005/09/01 18:03:19  sueh
 * <p> bug# 532 Added clearLoadAverage() to clear the load averages when the
 * <p> load average command fails.  Added a drop reason.  Added a error level
 * <p> for the restarts column.
 * <p>
 * <p> Revision 1.11  2005/08/27 22:38:40  sueh
 * <p> bug# 532 Populating the table from cpu.adoc:  getting the rows of the
 * <p> table, the units of speed and memory, and whether a column needs to be
 * <p> displayed.
 * <p>
 * <p> Revision 1.10  2005/08/24 00:25:25  sueh
 * <p> bug# 532 Added ashtray.  Made tubule a 2 cpu system
 * <p>
 * <p> Revision 1.9  2005/08/22 18:14:27  sueh
 * <p> bug# 532 Removed dummy load averages.  Added a key to each row,
 * <p>
 * <p> Revision 1.8  2005/08/04 20:16:24  sueh
 * <p> bug# 532  Fixed table resizing problems.  Added RunPack() to use with
 * <p> invoke later.  Added functions to calculate height and width of the table.
 * <p>
 * <p> Revision 1.7  2005/08/01 18:13:12  sueh
 * <p> bug# 532 Changed ProcessorTableRow.signalRestart() to addRestart.
 * <p> Added Failure Reason column.
 * <p>
 * <p> Revision 1.6  2005/07/21 22:22:30  sueh
 * <p> bug# 532 Added getHelpMessage so that the parallel panel can complain
 * <p> when no CPUs are selected.
 * <p>
 * <p> Revision 1.5  2005/07/15 16:31:49  sueh
 * <p> bug# 532 Removed experiment about not scrolling headers
 * <p>
 * <p> Revision 1.4  2005/07/14 22:14:40  sueh
 * <p> bug# 532 Experimenting with extending GridBagLayout to make a header
 * <p> in the scroll pane.
 * <p>
 * <p> Revision 1.3  2005/07/11 23:19:33  sueh
 * <p> bug# 619 Added scrolling and sized table.  Added functions:
 * <p> getContainer, getCpusSelected, getFirstSelectedIndex,
 * <p> getNextSelectedIndex, getRestartFactor, getSuccessFactor,
 * <p> getTablePanel, setMaximumSize, sizeSize, signalRestart,
 * <p> signalSuccess.
 * <p>
 * <p> Revision 1.2  2005/07/01 23:05:20  sueh
 * <p> bug# 619 added getTotalSUccesses(), signalCpusSelectedChanged()
 * <p>
 * <p> Revision 1.1  2005/07/01 21:21:47  sueh
 * <p> bug# 619 Table containing a list of computers and CPUs
 * <p> </p>
 */
