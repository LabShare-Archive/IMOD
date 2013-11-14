package etomo.ui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.comscript.ProcesschunksParam;
import etomo.storage.Node;
import etomo.storage.Storable;
import etomo.type.ConstEtomoNumber;
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
final class ProcessorTableRow implements Storable {
  public static final String rcsid = "$Id$";

  private static final String STORE_SELECTED = "Selected";
  private static final String STORE_CPUS_SELECTED = "CPUsSelected";
  private static final int DEFAULT_CPUS_SELECTED = 1;

  private final FieldCell cellNumberCpus = FieldCell.getIneditableInstance();
  private final FieldCell cellLoad1 = FieldCell.getIneditableInstance();
  private final FieldCell cellLoad5 = FieldCell.getIneditableInstance();
  private final FieldCell cellUsers = FieldCell.getIneditableInstance();
  private final FieldCell cellCPUUsage = FieldCell.getIneditableInstance();
  private final FieldCell cellRestarts = FieldCell.getIneditableInstance();
  private final FieldCell cellSuccesses = FieldCell.getIneditableInstance();
  private final FieldCell cellFailureReason = FieldCell.getIneditableInstance();
  private final FieldCell cellSpeed = FieldCell.getIneditableInstance();
  private final FieldCell cellMemory = FieldCell.getIneditableInstance();
  private final FieldCell cellOS = FieldCell.getIneditableInstance();
  private final FieldCell cellCPUType = FieldCell.getIneditableInstance();

  private final boolean displayQueues;
  private final int numRowsInTable;
  /**
   * A computer or queue.
   */
  private final ToggleCell cellComputer;
  private final FieldCell[] cellLoadArray;
  private final String[] deviceArray;

  private ProcessorTable table = null;
  private InputCell cellCPUsSelected = null;
  private String computerName;
  private String cpuType = null;
  private int numCpus = 1;
  private String speed = null;
  private String memory = null;
  private String os = null;
  private boolean rowInitialized = false;
  private boolean numberColumn = false;
  private boolean typeColumn = false;
  private boolean speedColumn = false;
  private boolean memoryColumn = false;
  private boolean osColumn = false;
  private boolean usersColumn = false;
  private boolean displayed = false;
  private boolean loadWarning = true;

  private ProcessorTableRow(final ProcessorTable table, final Node node,
      final int numCpus, final boolean displayQueues, final ButtonGroup queueButtonGroup,
      final int queueLoadArraySize, final int numRowsInTable) {
    this.table = table;
    this.numRowsInTable = numRowsInTable;
    computerName = node.getName();
    cpuType = node.getType();
    this.numCpus = numCpus;
    speed = node.getSpeed();
    memory = node.getMemory();
    deviceArray= node.getGpuDeviceArray();
    os = node.getOs();
    this.displayQueues = displayQueues;
    if (displayQueues) {
      cellComputer = new RadioButtonCell(queueButtonGroup);
      cellLoadArray = new FieldCell[queueLoadArraySize];
      for (int i = 0; i < queueLoadArraySize; i++) {
        cellLoadArray[i] = FieldCell.getIneditableInstance();
      }
    }
    else {
      cellComputer = new CheckBoxCell();
      cellLoadArray = null;
    }
  }

  static ProcessorTableRow getComputerInstance(final ProcessorTable table,
      final Node node, final int numCpus, final int numRowsInTable) {
    ProcessorTableRow instance = new ProcessorTableRow(table, node, numCpus, false, null,
        0, numRowsInTable);
    instance.initRow();
    return instance;
  }

  static ProcessorTableRow getQueueInstance(final ProcessorTable table, final Node node,
      final int numCpus, final ButtonGroup buttonGroup, final int loadArraySize,
      final int numRowsInTable) {
    ProcessorTableRow instance = new ProcessorTableRow(table, node, numCpus, true,
        buttonGroup, loadArraySize, numRowsInTable);
    instance.initRow();
    return instance;
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = cellComputer.getLabel();
    }
    else {
      prepend += "." + cellComputer.getLabel();
    }
    group = prepend + ".";
    props.setProperty(group + STORE_SELECTED, String.valueOf(isSelected()));
    if (cellCPUsSelected == null) {
      return;
    }
    if (numCpus == 1) {
      props.setProperty(group + STORE_CPUS_SELECTED,
          String.valueOf(((FieldCell) cellCPUsSelected).getValue()));
    }
    else if (cellCPUsSelected != null) {
      props.setProperty(group + STORE_CPUS_SELECTED,
          String.valueOf(((SpinnerCell) cellCPUsSelected).getIntValue()));
    }
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  /**
   * load the computers and number of CPUs selected
   */
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = cellComputer.getLabel();
    }
    else {
      prepend += "." + cellComputer.getLabel();
    }
    group = prepend + ".";
    boolean selected = Boolean
        .valueOf(props.getProperty(group + STORE_SELECTED, "false")).booleanValue();
    setSelected(selected);
    if (numCpus > 1 && isSelected() && cellCPUsSelected != null) {
      ((SpinnerCell) cellCPUsSelected).setValue(Integer.parseInt(props.getProperty(group
          + STORE_CPUS_SELECTED, Integer.toString(DEFAULT_CPUS_SELECTED))));
    }
  }

  private void initRow() {
    rowInitialized = true;
    cellComputer.setLabel(computerName);
    cellComputer.addActionListener(new ProcessorTableRowActionListener(this));
    if (displayQueues) {
      cellComputer.addChangeListener(new PTRComputerChangeListener(this));
    }
    if (cpuType != null) {
      cellCPUType.setValue(cpuType);
    }
    if (numCpus > 1) {
      cellCPUsSelected = SpinnerCell.getIntInstance(0, numCpus);
      SpinnerCell spinnerCell = (SpinnerCell) cellCPUsSelected;
      spinnerCell.addChangeListener(new PTRCPUChangeListener(this));
      spinnerCell.setValue(DEFAULT_CPUS_SELECTED);
      spinnerCell.setDisabledValue(0);
    }
    else {
      cellCPUsSelected = FieldCell.getIneditableInstance();
      ((FieldCell) cellCPUsSelected).setValue(1);
    }
    cellNumberCpus.setValue(numCpus);
    cellSpeed.setValue(speed);
    cellMemory.setEditable(false);
    cellMemory.setValue(memory);
    cellOS.setValue(os);
    updateSelected(false);
  }

  void turnOffLoadWarning() {
    loadWarning = false;
    cellCPUUsage.setWarning(false);
    cellLoad1.setWarning(false);
    cellLoad5.setWarning(false);
  }

  void setNumberColumn(boolean numberColumn) {
    this.numberColumn = numberColumn;
  }

  void setTypeColumn(boolean typeColumn) {
    this.typeColumn = typeColumn;
  }

  void setSpeedColumn(boolean speedColumn) {
    this.speedColumn = speedColumn;
  }

  void setMemoryColumn(boolean memoryColumn) {
    this.memoryColumn = memoryColumn;
  }

  void setOSColumn(boolean osColumn) {
    this.osColumn = osColumn;
  }

  void setUsersColumn(boolean usersColumn) {
    this.usersColumn = usersColumn;
  }

  boolean isDisplayed() {
    return displayed;
  }

  void deleteRow() {
    displayed = false;
  }

  void setColumns() {
    setNumberColumn(table.useNumberColumn());
    setTypeColumn(table.useTypeColumn());
    setSpeedColumn(table.useSpeedColumn());
    setMemoryColumn(table.useMemoryColumn());
    setOSColumn(table.useOSColumn());
    setUsersColumn(table.useUsersColumn());
  }

  void display(int index, Viewport viewport) {
    displayed = true;
    if (!viewport.inViewport(index)) {
      return;
    }
    // create row
    JPanel panel = table.getTablePanel();
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weighty = 0.0;
    // constraints.weightx = 1.0;
    constraints.weightx = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    cellComputer.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    cellCPUsSelected.add(panel, layout, constraints);
    if (numberColumn) {
      cellNumberCpus.add(panel, layout, constraints);
    }
    if (Utilities.isWindowsOS()) {
      cellCPUUsage.add(panel, layout, constraints);
    }
    else {
      if (displayQueues) {
        for (int i = 0; i < cellLoadArray.length; i++) {
          cellLoadArray[i].add(panel, layout, constraints);
        }
      }
      else {
        cellLoad1.add(panel, layout, constraints);
        cellLoad5.add(panel, layout, constraints);
        if (usersColumn) {
          cellUsers.add(panel, layout, constraints);
        }
      }
    }
    if (typeColumn) {
      cellCPUType.add(panel, layout, constraints);
    }
    if (speedColumn) {
      cellSpeed.add(panel, layout, constraints);
    }
    if (memoryColumn) {
      cellMemory.add(panel, layout, constraints);
    }
    if (osColumn) {
      cellOS.add(panel, layout, constraints);
    }
    cellRestarts.add(panel, layout, constraints);
    cellSuccesses.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    cellFailureReason.add(panel, layout, constraints);
  }

  void performAction() {
    updateSelected(cellComputer.isSelected());
  }

  void stateChangedCPU() {
    table.msgCPUsSelectedChanged();
  }

  void stateChangedComputer() {
    if (!displayQueues) {
      return;
    }
    // handle radio button changes
    updateSelected(cellComputer.isSelected());
  }

  final void msgDropped(String reason) {
    setSelected(false);
    cellFailureReason.setValue(reason);
    cellFailureReason
        .setToolTipText("This computer was dropped from the current distributed process.");
  }

  public void setSelected(boolean selected) {
    // Do not allow the row to be unselected if it is disabled and the only row.
    if (!selected && numRowsInTable == 1 && !cellComputer.isEnabled()) {
      return;
    }
    cellComputer.setSelected(selected);
    updateSelected(selected);
  }

  public void enableSelectionField(final boolean enabled) {
    cellComputer.setEnabled(enabled);
  }

  public void setCPUsSelected(String cpusSelected) {
    if (numCpus == 1) {
      ((FieldCell) cellCPUsSelected).setValue(Integer.parseInt(cpusSelected));
    }
    else {
      ((SpinnerCell) cellCPUsSelected).setValue(Integer.parseInt(cpusSelected));
    }
  }

  private void updateSelected(boolean selected) {
    cellCPUsSelected.setEnabled(selected);
    setSelectedError();
    table.msgCPUsSelectedChanged();
  }

  private void setSelectedError() {
    boolean noloadAverage;
    if (displayQueues) {
      noloadAverage = cellLoadArray[0].isEmpty() || cellLoadArray[0].equals("NA");
    }
    else if (Utilities.isWindowsOS()) {
      noloadAverage = cellCPUUsage.isEmpty();
    }
    else {
      noloadAverage = cellLoad1.isEmpty();
    }
    cellComputer.setWarning(cellComputer.isSelected() && noloadAverage);
  }

  final boolean isSelected() {
    return cellComputer.isSelected();
  }

  final void getParameters(ProcesschunksParam param) {
    int numCpus = getCPUsSelected();
    if (!displayQueues && numCpus > 0) {
      param.addMachineName(cellComputer.getLabel(), numCpus, deviceArray);
    }
  }

  int getSuccesses() {
    return cellSuccesses.getIntValue();
  }

  int getCPUsSelected() {
    if (!isSelected()) {
      return 0;
    }
    if (cellCPUsSelected instanceof SpinnerCell) {
      return ((SpinnerCell) cellCPUsSelected).getIntValue();
    }
    int cpusSelected = ((FieldCell) cellCPUsSelected).getIntValue();
    if (cpusSelected == EtomoNumber.INTEGER_NULL_VALUE) {
      cpusSelected = 0;
    }
    return cpusSelected;
  }

  boolean equals(String computer) {
    if (cellComputer.getLabel().equals(computer)) {
      return true;
    }
    return false;
  }

  void addSuccess() {
    int successes = cellSuccesses.getIntValue();
    if (successes == EtomoNumber.INTEGER_NULL_VALUE) {
      successes = 1;
    }
    else {
      successes++;
    }
    cellSuccesses.setValue(successes);
  }

  void resetResults() {
    cellSuccesses.setValue();
    cellRestarts.setValue();
    cellRestarts.setError(false);
    cellRestarts.setWarning(false);
  }

  void addRestart() {
    int restarts = cellRestarts.getIntValue();
    if (restarts == EtomoNumber.INTEGER_NULL_VALUE) {
      restarts = 1;
    }
    else {
      restarts++;
    }
    cellRestarts.setValue(restarts);
    if (restarts >= ProcesschunksParam.DROP_VALUE) {
      cellRestarts.setError(true);
    }
    else if (restarts > 0) {
      cellRestarts.setWarning(true);
    }
  }

  void setLoad(double load1, double load5, int users, String usersTooltip) {
    int numberCpus = cellNumberCpus.getIntValue();
    setLoad(cellLoad1, load1, numberCpus);
    setLoad(cellLoad5, load5, numberCpus);
    cellComputer.setWarning(false);
    if (usersColumn) {
      cellUsers.setValue(users);
      cellUsers.setToolTipText(usersTooltip);
    }
  }

  void setLoad(String[] loadArray) {
    for (int i = 0; i < loadArray.length; i++) {
      if (i < cellLoadArray.length) {
        cellLoadArray[i].setValue(loadArray[i]);
      }
    }
    cellComputer.setWarning(false);
  }

  void setCPUUsage(double cpuUsage, final ConstEtomoNumber numberOfProcessors) {
    double usage;
    if (numberOfProcessors == null || numberOfProcessors.isNull()) {
      usage = cpuUsage / 100.0;
    }
    else {
      usage = cpuUsage * numberOfProcessors.getInt() / 100.0;
    }
    if (loadWarning) {
      cellCPUUsage.setWarning(cpuUsage > 75);
    }
    cellCPUUsage.setValue(usage);
    cellComputer.setWarning(false);
  }

  final void clearLoad(String reason, String tooltip) {
    String loadName;
    if (Utilities.isWindowsOS()) {
      loadName = "CPU usage";
      cellCPUUsage.setValue();
      cellCPUUsage.setWarning(false);
    }
    else {
      loadName = "load averages";
      cellLoad1.setValue();
      cellLoad1.setWarning(false);
      cellLoad5.setValue();
      cellLoad5.setWarning(false);
      cellUsers.setValue();
    }
    setSelectedError();
    cellFailureReason.setValue(reason);
    cellFailureReason.setToolTipText(tooltip);
    // cellFailureReason.setToolTipText("Unable to get the " + loadName
    // + " for this computer.");
  }

  /**
   * Clear failure reason, if failure reason equals failureReason1 or 2.  This means
   * that processes can only clear their own messages.  This is useful
   * for restarting an intermittent process without losing the processchunks
   * failure reason.
   */
  final void clearFailureReason(String failureReason1, String failureReason2) {
    String value = cellFailureReason.getValue();
    if (value == null || (!value.equals(failureReason1) && !value.equals(failureReason2))) {
      return;
    }
    clearFailureReason();
  }

  final void clearFailureReason() {
    cellFailureReason.setValue();
    cellFailureReason.setToolTipText(null);
  }

  private final void setLoad(FieldCell cellLoad, double load, int numberCpus) {
    if (loadWarning) {
      cellLoad.setWarning(load >= numberCpus);
    }
    cellLoad.setValue(load);
  }

  final int getHeight() {
    return cellComputer.getHeight();
  }

  final String getComputer() {
    return cellComputer.getLabel();
  }

  /* final int getWidth() { int width = 0; width += cellComputer.getWidth(); width +=
   * cellCPUsSelected.getWidth(); if (numberColumn) { width += cellNumberCpus.getWidth();
   * } if (Utilities.isWindowsOS()) { width += cellCPUUsage.getWidth(); } else { width +=
   * cellLoad1.getWidth(); width += cellLoad5.getWidth(); if (usersColumn) { width +=
   * cellUsers.getWidth(); } } if (typeColumn) { width += cellCPUType.getWidth(); } if
   * (speedColumn) { width += cellSpeed.getWidth(); } if (memoryColumn) { width +=
   * cellMemory.getWidth(); } if (osColumn) { width += cellOS.getWidth(); } width +=
   * cellRestarts.getWidth(); width += cellSuccesses.getWidth(); width +=
   * cellFailureReason.getWidth(); return width + 3; } */

  private class ProcessorTableRowActionListener implements ActionListener {
    ProcessorTableRow adaptee;

    ProcessorTableRowActionListener(ProcessorTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.performAction();
    }
  }

  private class PTRCPUChangeListener implements ChangeListener {
    ProcessorTableRow adaptee;

    PTRCPUChangeListener(ProcessorTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.stateChangedCPU();
    }
  }

  private class PTRComputerChangeListener implements ChangeListener {
    ProcessorTableRow adaptee;

    PTRComputerChangeListener(ProcessorTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.stateChangedComputer();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.5  2011/07/23 03:05:33  sueh
 * <p> Bug# 1517 In setCPUUsage, incorporate the numberOfProcessors into the CPU usage, and set the
 * <p> warning and 75%.
 * <p>
 * <p> Revision 1.4  2011/04/04 17:24:16  sueh
 * <p> bug# 1416 Fixed a bug in setCPUsSelected - class wasn't handling a computer with a single CPU or GPU.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:20:22  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:16:40  sueh
 * <p> bug# 1422 Passing a Node instead of values that came from a Node.
 * <p> Added the ability to turn off the load warning.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.35  2009/04/20 20:13:50  sueh
 * <p> bug# 1192 Added setCPUsSelected.  Setting the computer and CPUs by
 * <p> calling ProcesschunksParam.addMachineName once.
 * <p>
 * <p> Revision 1.34  2009/04/13 22:57:49  sueh
 * <p> Removed unnecessary print.
 * <p>
 * <p> Revision 1.33  2008/10/06 22:44:46  sueh
 * <p> bug# 1113 Changed addRow to display.  Display now takes index and viewport so it can check if it should display itself.
 * <p>
 * <p> Revision 1.32  2007/09/27 21:03:23  sueh
 * <p> bug# 1044 Added a displayQueues mode.
 * <p>
 * <p> Revision 1.31  2007/07/30 19:21:13  sueh
 * <p> bug# 1029 Resetting restarts in resetResults().
 * <p>
 * <p> Revision 1.30  2007/05/25 00:28:35  sueh
 * <p> bug# 964 Added tooltip to clearLoadAverage.  Added a second reason to
 * <p> clearFailureReason.
 * <p>
 * <p> Revision 1.29  2007/05/21 18:11:30  sueh
 * <p> bug# 992 Added usersColumn.  Do not display Users column when
 * <p> usersColumn is false.
 * <p>
 * <p> Revision 1.28  2007/04/02 21:52:50  sueh
 * <p> bug# 964 Added FieldCell.editable to make instances of FieldCell that can't be
 * <p> edited.  This allows FieldCell.setEditable and setEnabled to be called without
 * <p> checking whether a field should be editable.
 * <p>
 * <p> Revision 1.27  2007/03/27 19:31:56  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.26  2007/02/09 00:52:03  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.25  2007/02/05 23:42:05  sueh
 * <p> bug# 962 Added SpinnerCell.getInstance.
 * <p>
 * <p> Revision 1.24  2006/11/29 00:22:13  sueh
 * <p> bug# 934 Added the parameter String failureReason to clearFailureReason(), so
 * <p> that it won't delete a failure reason by another processes.  Changed
 * <p> setSelectedError() to set a warning instead of an error.
 * <p>
 * <p> Revision 1.23  2006/11/18 00:50:01  sueh
 * <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
 * <p>
 * <p> Revision 1.22  2006/11/08 21:10:13  sueh
 * <p> bug# 936:  Remove cellLoad15 and add cellUsers.  Clear cellUsers with the load
 * <p> averages, but never set its warning.  Check that the load average has been set
 * <p> by just looking at cellLoad1 (for Linux/Mac) since they are all set together.
 * <p>
 * <p> Revision 1.21  2006/02/08 03:46:14  sueh
 * <p> bug# 796 added cellCPUUsage to use instead of load averages in windows
 * <p>
 * <p> Revision 1.20  2005/12/16 01:46:10  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.19  2005/12/14 20:58:29  sueh
 * <p> bug# 784 Added context sensitive tool tips to failure reasons.
 * <p>
 * <p> Revision 1.18  2005/11/14 22:17:35  sueh
 * <p> bug# 762 Made performAction() and stateChanged() protected.
 * <p>
 * <p> Revision 1.17  2005/11/04 00:55:08  sueh
 * <p> bug# 732 Added isDisplayed() and deleteRow().  The resize functionality
 * <p> needs to know what rows are displayed.
 * <p>
 * <p> Revision 1.16  2005/09/27 23:46:56  sueh
 * <p> bug# 532 Moved call to initRow() to the constructor so that the cells will
 * <p> all be constructed before that perferences are read.
 * <p>
 * <p> Revision 1.15  2005/09/22 21:32:55  sueh
 * <p> bug# 532 Removed restartsError.  Taking error level from ParallelPanel in
 * <p> ProcessorTableRow.
 * <p>
 * <p> Revision 1.14  2005/09/13 00:02:07  sueh
 * <p> bug# 532 Implemented storable to store whether cellComputer is selected
 * <p> and how many CPUs are selected.
 * <p>
 * <p> Revision 1.13  2005/09/10 01:55:15  sueh
 * <p> bug# 532 Added clearFailureReason() so that the failure reason can be
 * <p> cleared when a new connection to the computer is attempted.
 * <p>
 * <p> Revision 1.12  2005/09/09 21:47:55  sueh
 * <p> bug# 532 Passed reason string to clearLoadAverage().
 * <p>
 * <p> Revision 1.11  2005/09/01 18:03:33  sueh
 * <p> bug# 532 Added clearLoadAverage() to clear the load averages when the
 * <p> load average command fails.  Added a drop reason.  Added a error level
 * <p> for the restarts column.
 * <p>
 * <p> Revision 1.10  2005/08/27 22:42:07  sueh
 * <p> bug# 532 Added cells for speed and memory.  Displaying columns based
 * <p> on booleans:  memoryColumn, numberColumn, etc.  Changed cellOs to
 * <p> cellOS.  In setSelected() set cellComputer.error to true if the load fields
 * <p> are empty.  Turn off cellComputer.error when load fields are set.
 * <p>
 * <p> Revision 1.9  2005/08/22 18:15:05  sueh
 * <p> bug# 532 Removed dummy load averages.
 * <p>
 * <p> Revision 1.8  2005/08/04 20:19:33  sueh
 * <p> bug# 532  Removed demo fields and functions.  Added functions:
 * <p> getWidth, addSuccess, and drop.
 * <p>
 * <p> Revision 1.7  2005/08/01 18:15:40  sueh
 * <p> bug# 532 Changed ProcessorTableRow.signalRestart() to addRestart.
 * <p> Added Failure Reason Column.  Added
 * <p> getParameters(ProcesschunksParam).  Changed getCpusSelected() to
 * <p> take the selection state into account.  Changed resetResults() to reset
 * <p> error and warning states.  Added setLoad().
 * <p>
 * <p> Revision 1.6  2005/07/19 22:35:28  sueh
 * <p> bug# 532 Since the error setting affects the background, don't set error
 * <p> == true for CellRestarts automatically.
 * <p>
 * <p> Revision 1.5  2005/07/15 16:32:05  sueh
 * <p> bug# 532 Removed experiment about not scrolling headers
 * <p>
 * <p> Revision 1.4  2005/07/14 22:15:01  sueh
 * <p> bug# 532 Experimenting with extending GridBagLayout to make a header
 * <p> in the scroll pane.
 * <p>
 * <p> Revision 1.3  2005/07/11 23:22:56  sueh
 * <p> bug# 619 Showing results when signals rather then all at once.  Add
 * <p> functions:  getBorderHeight, getHeight, getSuccessFactor, isSelected,
 * <p> signalRestart, and signalSuccess.
 * <p>
 * <p> Revision 1.2  2005/07/01 23:06:06  sueh
 * <p> bug# 619 Added getCpusSelected, setSuccesses, stateChanged
 * <p>
 * <p> Revision 1.1  2005/07/01 21:22:02  sueh
 * <p> bug# 619 A row in a table containing a list of computers and CPUs
 * <p> </p>
 */
