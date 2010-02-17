package etomo.ui;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.IntermittentCommand;
import etomo.comscript.LoadAverageParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.QueuechunkParam;
import etomo.process.LoadAverageMonitor;
import etomo.process.LoadMonitor;
import etomo.process.QueuechunkLoadMonitor;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.Network;
import etomo.storage.Node;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.Utilities;

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
final class ProcessorTable implements Storable, ParallelProgressDisplay,
    LoadDisplay, Viewable {
  public static final String rcsid = "$Id$";

  static final String NUMBER_CPUS_HEADER = "# CPUs";
  private static final String STORE_PREPEND = "ProcessorTable";

  private final JPanel rootPanel = new JPanel();
  private JPanel tablePanel;
  private GridBagLayout layout = null;
  private GridBagConstraints constraints = null;
  private final HeaderCell header1Computer = new HeaderCell();
  private final HeaderCell header1NumberCPUs = new HeaderCell(
      NUMBER_CPUS_HEADER);
  private final HeaderCell header1Load = new HeaderCell("Load Average");
  private final HeaderCell header1Users = new HeaderCell("Users");
  private final HeaderCell header1CPUUsage = new HeaderCell("CPU Usage");
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
  private final HeaderCell header2Load1 = new HeaderCell("1 Min.");
  private final HeaderCell header2Load5 = new HeaderCell("5 Min.");
  private final HeaderCell header2Users = new HeaderCell();
  private final HeaderCell header2CPUUsage = new HeaderCell();
  private final HeaderCell header2CPUType = new HeaderCell();
  private final HeaderCell header2Speed = new HeaderCell();
  private final HeaderCell header2RAM = new HeaderCell();
  private final HeaderCell header2OS = new HeaderCell();
  private final HeaderCell header2Restarts = new HeaderCell();
  private final HeaderCell header2Finished = new HeaderCell("Chunks");
  private final HeaderCell header2Failure = new HeaderCell("Reason");

  private final RowList rowList = new RowList();
  private final Viewport viewport = new Viewport(this, EtomoDirector.INSTANCE
      .getUserConfiguration().getParallelTableSize().getInt(), null, null,
      null, "Processor");
  private final ParallelPanel parent;
  private final AxisID axisID;
  private final BaseManager manager;
  private final boolean displayQueues;
  private final LoadMonitor loadMonitor;

  private boolean numberColumn = false;
  private boolean typeColumn = false;
  private boolean speedColumn = false;
  private boolean memoryColumn = false;
  private boolean usersColumn = false;
  private boolean osColumn = false;
  private String speedUnits = null;
  private String memoryUnits = null;
  private boolean scrolling = false;

  private HeaderCell[] header1LoadArray;
  private HeaderCell[] header2LoadArray;
  private boolean expanded = false;

  ProcessorTable(final BaseManager manager, final ParallelPanel parent,
      final AxisID axisID, final boolean displayQueues) {
    this.manager = manager;
    this.parent = parent;
    this.axisID = axisID;
    this.displayQueues = displayQueues;
    if (displayQueues) {
      loadMonitor = new QueuechunkLoadMonitor(this, axisID, manager);
    }
    else {
      loadMonitor = new LoadAverageMonitor(this, axisID, manager);
    }
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
    //configure
    UIHarness.INSTANCE.repaintWindow(manager);
  }

  private void initTable() {
    usersColumn = CpuAdoc.INSTANCE.isUsersColumn(manager, axisID, manager
        .getPropertyUserDir())
        && !displayQueues;
    speedUnits = CpuAdoc.INSTANCE.getSpeedUnits(manager, axisID, manager
        .getPropertyUserDir());
    memoryUnits = CpuAdoc.INSTANCE.getMemoryUnits(manager, axisID, manager
        .getPropertyUserDir());
    String[] loadUnitsArray = null;
    if (displayQueues) {
      loadUnitsArray = CpuAdoc.INSTANCE.getLoadUnits(manager, axisID, manager
          .getPropertyUserDir());
      if (loadUnitsArray.length == 0) {
        header1LoadArray = new HeaderCell[1];
        header1LoadArray[0] = new HeaderCell("Load");
      }
      else {
        header1LoadArray = new HeaderCell[loadUnitsArray.length];
        for (int i = 0; i < loadUnitsArray.length; i++) {
          header1LoadArray[i] = new HeaderCell(loadUnitsArray[i]);
        }
      }
      header2LoadArray = new HeaderCell[header1LoadArray.length];
      for (int i = 0; i < header2LoadArray.length; i++) {
        header2LoadArray[i] = new HeaderCell();
      }
    }
    //loop through the nodes
    EtomoNumber number = new EtomoNumber();
    //loop on nodes
    int size;
    if (displayQueues) {
      size = Network
          .getNumQueues(manager, axisID, manager.getPropertyUserDir());
    }
    else {
      size = Network.getNumComputers(manager, axisID, manager
          .getPropertyUserDir());
    }
    ButtonGroup buttonGroup = null;
    if (displayQueues) {
      buttonGroup = new ButtonGroup();
    }
    for (int i = 0; i < size; i++) {
      //get the node
      Node node;
      if (displayQueues) {
        node = Network.getQueue(manager, i, axisID, manager
            .getPropertyUserDir());
      }
      else {
        node = Network.getComputer(manager, i, axisID, manager
            .getPropertyUserDir());
      }
      //exclude any node with the "exclude-interface" attribute set to the
      //current interface
      if (node != null && !node.isExcludedInterface(manager.getInterfaceType())
          && (!node.isExcludedUser(System.getProperty("user.name")))) {
        String name = node.getName();
        //get the number attribute
        //set numberColumn to true if an number attribute is returned
        number.set(node.getNumber());
        if (!number.isDefault()) {
          numberColumn = true;
        }
        //get the type attribute
        //set typeColumn to true if an type attribute is returned
        String type = node.getType();
        if (!type.matches("\\s*")) {
          typeColumn = true;
        }
        //get the speed attribute
        //set speedColumn to true if an speed attribute is returned
        String speed = node.getSpeed();
        if (!speed.matches("\\s*")) {
          speedColumn = true;
        }
        //get the memory attribute
        //set memoryColumn to true if an memory attribute is returned
        String memory = node.getMemory();
        if (!memory.matches("\\s*")) {
          memoryColumn = true;
        }
        //get the os attribute
        //set osColumn to true if an os attribute is returned
        String os = node.getOs();
        if (!os.matches("\\s*")) {
          osColumn = true;
        }
        //create the row
        ProcessorTableRow row;
        if (displayQueues) {
          row = ProcessorTableRow.getQueueInstance(this, name, number.getInt(),
              type, speed, memory, os, buttonGroup, header2LoadArray.length);
        }
        else {
          row = ProcessorTableRow.getComputerInstance(this, name, number
              .getInt(), type, speed, memory, os);
        }
        //add the row to the rows HashedArray
        rowList.add(row);
      }
    }
    //set custom header text
    if (displayQueues) {
      header1Computer.setText("Queue");
    }
    else {
      header1Computer.setText("Computer");
    }
    if (speedColumn) {
      header2Speed.setText(speedUnits);
    }
    if (memoryColumn) {
      header2RAM.setText(memoryUnits);
    }
    if (displayQueues) {
      if (loadUnitsArray == null || loadUnitsArray.length == 0) {

      }
    }
    try {
      ParameterStore parameterStore = EtomoDirector.INSTANCE
          .getParameterStore();
      parameterStore.load(this);
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to load parameters.\n" + e.getMessage(), "Etomo Error",
          axisID);
    }
    setToolTipText();
    if (displayQueues && rowList.size() == 1) {
      rowList.setSelected(0, true);
    }
  }

  public void msgViewportPaged() {
    build();
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void buildTable() {
    tablePanel = new JPanel();
    layout = new GridBagLayout();
    constraints = new GridBagConstraints();
    //build table
    tablePanel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    //header row 1
    constraints.anchor = GridBagConstraints.CENTER;
    //constraints.weightx = 1.0;
    constraints.weightx = 0.0;
    //constraints.weighty = 1.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1Computer.add(tablePanel, layout, constraints);
    if (numberColumn && expanded) {
      constraints.gridwidth = 2;
    }
    header1NumberCPUs.add(tablePanel, layout, constraints);
    if (Utilities.isWindowsOS()) {
      constraints.gridwidth = 1;
      header1CPUUsage.add(tablePanel, layout, constraints);
    }
    else {
      if (displayQueues) {
        constraints.gridwidth = 1;
        for (int i = 0; i < header1LoadArray.length; i++) {
          header1LoadArray[i].add(tablePanel, layout, constraints);
        }
      }
      else {
        constraints.gridwidth = 2;
        header1Load.add(tablePanel, layout, constraints);
        constraints.gridwidth = 1;
        //The users column contains a load value, so it can't be used when
        //displaying queues.
        if (usersColumn) {
          header1Users.add(tablePanel, layout, constraints);
        }
      }
    }
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
    //header row 2
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
    if (Utilities.isWindowsOS()) {
      header2CPUUsage.add(tablePanel, layout, constraints);
    }
    else {
      if (displayQueues) {
        for (int i = 0; i < header2LoadArray.length; i++) {
          header2LoadArray[i].add(tablePanel, layout, constraints);
        }
      }
      else {
        header2Load1.add(tablePanel, layout, constraints);
        header2Load5.add(tablePanel, layout, constraints);
        if (usersColumn) {
          header2Users.add(tablePanel, layout, constraints);
        }
      }
    }
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
    //add rows to the table
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

  long getTotalSuccesses() {
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

  public void msgPausingProcess() {
    parent.msgPausingProcess();
  }

  int getCPUsSelected() {
    return rowList.getCPUsSelected();
  }

  void restartLoadMonitor() {
    loadMonitor.restart();
  }

  boolean useUsersColumn() {
    return usersColumn;
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
    if (displayQueues) {
      String queue = rowList.getComputer(rowList.getFirstSelectedIndex());
      Node node = Network.getQueue(manager, queue, axisID, manager
          .getPropertyUserDir());
      if (node != null) {
        param.setQueueCommand(node.getCommand());
      }
      param.setQueue(queue);
    }
    rowList.getParameters(param);
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

  public void setComputerMap(final Map computerMap) {
    rowList.setComputerMap(computerMap);
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
        + " column and use the spinner in the " + header1NumberCPUs.getText()
        + " " + header2NumberCPUsUsed.getText() + " column where available.";
  }

  public void startLoad() {
    for (int i = 0; i < rowList.size(); i++) {
      manager.startLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  private IntermittentCommand getIntermittentCommand(final int index) {
    ProcessorTableRow row = (ProcessorTableRow) rowList.get(index);
    String computer = row.getComputer();
    if (displayQueues) {
      return QueuechunkParam.getLoadInstance(computer, axisID, manager);
    }
    return LoadAverageParam.getInstance(computer, manager);
  }

  /**
   * sets parallelProcessMonitor with a monitor which is monitoring a parallel
   * process associated with this ParallelProgressDisplay
   * @param ParallelProcessMonitor
   */
  /*  public void setParallelProcessMonitor(
   final ParallelProcessMonitor parallelProcessMonitor) {
   parent.setParallelProcessMonitor(parallelProcessMonitor);
   }*/

  public void endLoad() {
    for (int i = 0; i < rowList.size(); i++) {
      manager.endLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  public void stopLoad() {
    for (int i = 0; i < rowList.size(); i++) {
      manager.stopLoad(getIntermittentCommand(i), loadMonitor);
    }
  }

  public void setLoad(final String computer, final double load1,
      final double load5, final int users, String usersTooltip) {
    ((ProcessorTableRow) rowList.get(computer)).setLoad(load1, load5, users,
        usersTooltip);
  }

  public void setLoad(final String computer, final String[] loadArray) {
    ((ProcessorTableRow) rowList.get(computer)).setLoad(loadArray);
  }

  public void setCPUUsage(final String computer, final double cpuUsage) {
    ((ProcessorTableRow) rowList.get(computer)).setCPUUsage(cpuUsage);
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
  public void msgStartingProcess(final String computer,
      final String failureReason1, String failureReason2) {
    ((ProcessorTableRow) rowList.get(computer)).clearFailureReason(
        failureReason1, failureReason2);
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
      prepend = STORE_PREPEND;
    }
    else {
      prepend += "." + STORE_PREPEND;
    }
    rowList.store(props, prepend);
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(final Properties props) {
    load(props, "");
  }

  public void load(final Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = STORE_PREPEND;
    }
    else {
      prepend += "." + STORE_PREPEND;
    }
    rowList.load(props, prepend);
  }

  boolean isScrolling() {
    return scrolling;
  }

  private void setToolTipText() {
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
    if (Utilities.isWindowsOS()) {
      header1CPUUsage
          .setToolTipText("The CPU usage (0 to number of CPUs) averaged over one second.");
    }
    else {
      header1Load.setToolTipText("Represents how busy each computer is.");
      header2Load1.setToolTipText("The load averaged over one minute.");
      header2Load5.setToolTipText("The load averaged over five minutes.");
      text = "The number of users logged into the computer.";
      header1Users.setToolTipText(text);
      header2Users.setToolTipText(text);
    }
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
    //Contracted index for use when the table is not expanded..
    private final List contractedIndex = new ArrayList();

    private RowList() {
    }

    /**
     * Changes the selected computers and CPUs to match computerMap.
     * @param computerMap
     */
    private void setComputerMap(final Map computerMap) {
      if (computerMap == null || computerMap.isEmpty()) {
        return;
      }
      for (int i = 0; i < list.size(); i++) {
        //First unselect a computer.  Then select the computer if it is in
        //computerMap.
        ProcessorTableRow row = get(i);
        row.setSelected(false);
        String key = row.getComputer();
        if (computerMap.containsKey(key)) {
          row.setSelected(true);
          row.setCPUsSelected((String) computerMap.get(key));
        }
      }
    }

    private void add(final ProcessorTableRow row) {
      list.add(row);
    }

    private void display(boolean expanded, Viewport viewport) {
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

    private int size(boolean expanded) {
      if (expanded) {
        return size();
      }
      return contractedIndex.size();
    }

    private int size() {
      return list.size();
    }

    private ProcessorTableRow get(final int index) {
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

    private void resetResults() {
      for (int i = 0; i < size(); i++) {
        get(i).resetResults();
      }
    }

    private long getTotalSuccesses() {
      long successes = 0;
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
      return get(index).getComputer();
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
