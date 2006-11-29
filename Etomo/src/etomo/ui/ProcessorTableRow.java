package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.comscript.ProcesschunksParam;
import etomo.storage.Storable;
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

  private final FieldCell cellNumberCpus = new FieldCell();
  private final FieldCell cellLoad1 = new FieldCell();
  private final FieldCell cellLoad5 = new FieldCell();
  private final FieldCell cellUsers = new FieldCell();
  private final FieldCell cellCPUUsage = new FieldCell();
  private final FieldCell cellRestarts = new FieldCell();
  private final FieldCell cellSuccesses = new FieldCell();
  private final FieldCell cellFailureReason = new FieldCell();
  private final FieldCell cellSpeed = new FieldCell();
  private final FieldCell cellMemory = new FieldCell();
  private final FieldCell cellOS = new FieldCell();
  private final CheckBoxCell cellComputer = new CheckBoxCell();
  private final FieldCell cellCPUType = new FieldCell();
  private final TooltipFormatter tooltipFormatter = new TooltipFormatter();

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
  private boolean displayed = false;

  ProcessorTableRow(ProcessorTable table, String computerName, int numCpus,
      String cpuType, String speed, String memory, String os) {
    this.table = table;
    this.computerName = computerName;
    this.cpuType = cpuType;
    this.numCpus = numCpus;
    this.speed = speed;
    this.memory = memory;
    this.os = os;
    initRow();
  }

  ProcessorTableRow(ProcessorTable table, String computerName) {
    this(table, computerName, 1, null, null, null, null);
  }

  ProcessorTableRow(ProcessorTable table, String computerName, int numCpus) {
    this(table, computerName, numCpus, null, null, null, null);
  }

  ProcessorTableRow(ProcessorTable table, String computerName, String cpuType) {
    this(table, computerName, 1, cpuType, null, null, null);
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
      props.setProperty(group + STORE_CPUS_SELECTED, String
          .valueOf(((FieldCell) cellCPUsSelected).getValue()));
    }
    else if (cellCPUsSelected != null) {
      props.setProperty(group + STORE_CPUS_SELECTED, String
          .valueOf(((SpinnerCell) cellCPUsSelected).getValue()));
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
    boolean selected = Boolean.valueOf(
        props.getProperty(group + STORE_SELECTED, "false")).booleanValue();
    cellComputer.setSelected(selected);
    setSelected(selected);
    if (numCpus > 1 && isSelected() && cellCPUsSelected != null) {
      ((SpinnerCell) cellCPUsSelected).setValue(Integer.parseInt(props
          .getProperty(group + STORE_CPUS_SELECTED, Integer
              .toString(DEFAULT_CPUS_SELECTED))));
    }
  }

  private void initRow() {
    rowInitialized = true;
    cellComputer.setLabel(computerName);
    cellComputer.addActionListener(new ProcessorTableRowActionListener(this));
    if (cpuType != null) {
      cellCPUType.setValue(cpuType);
    }
    cellCPUType.setEnabled(false);
    if (numCpus > 1) {
      cellCPUsSelected = new SpinnerCell(0, numCpus);
      SpinnerCell spinnerCell = (SpinnerCell) cellCPUsSelected;
      spinnerCell.addChangeListener(new ProcessorTableRowChangeListener(this));
      spinnerCell.setValue(DEFAULT_CPUS_SELECTED);
      spinnerCell.setDisabledValue(0);
    }
    else {
      cellCPUsSelected = new FieldCell();
      ((FieldCell) cellCPUsSelected).setValue(1);
      cellCPUsSelected.setEnabled(false);
    }
    cellNumberCpus.setValue(numCpus);
    cellNumberCpus.setEnabled(false);
    cellLoad1.setEnabled(false);
    cellLoad5.setEnabled(false);
    cellUsers.setEnabled(false);
    cellCPUUsage.setEnabled(false);
    cellRestarts.setEnabled(false);
    cellSuccesses.setEnabled(false);
    cellFailureReason.setEnabled(false);
    cellSpeed.setEnabled(false);
    cellSpeed.setValue(speed);
    cellMemory.setEnabled(false);
    cellMemory.setValue(memory);
    cellOS.setEnabled(false);
    cellOS.setValue(os);
    setSelected(false);
  }

  final void setNumberColumn(boolean numberColumn) {
    this.numberColumn = numberColumn;
  }

  final void setTypeColumn(boolean typeColumn) {
    this.typeColumn = typeColumn;
  }

  final void setSpeedColumn(boolean speedColumn) {
    this.speedColumn = speedColumn;
  }

  final void setMemoryColumn(boolean memoryColumn) {
    this.memoryColumn = memoryColumn;
  }

  final void setOSColumn(boolean osColumn) {
    this.osColumn = osColumn;
  }

  final boolean isDisplayed() {
    return displayed;
  }

  final void deleteRow() {
    displayed = false;
  }

  void addRow() {
    displayed = true;
    //create row
    JPanel panel = table.getTablePanel();
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weighty = 0.0;
    //constraints.weightx = 1.0;
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
      cellLoad1.add(panel, layout, constraints);
      cellLoad5.add(panel, layout, constraints);
      cellUsers.add(panel, layout, constraints);
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

  protected void performAction(ActionEvent event) {
    setSelected(cellComputer.isSelected());
  }

  protected void stateChanged(ChangeEvent event) {
    table.msgCPUsSelectedChanged();
  }

  final void msgDropped(String reason) {
    cellComputer.setSelected(false);
    setSelected(false);
    cellFailureReason.setValue(reason);
    cellFailureReason.setToolTipText(tooltipFormatter.setText(
        "This computer was dropped from the current distributed process.")
        .format());
  }

  private void setSelected(boolean selected) {
    if (cellCPUsSelected instanceof SpinnerCell) {
      SpinnerCell cell = (SpinnerCell) cellCPUsSelected;
      cell.setEnabled(selected);
    }
    else {
      FieldCell cell = (FieldCell) cellCPUsSelected;
      cell.setHideValue(!selected);
    }
    setSelectedError();
    table.msgCPUsSelectedChanged();
  }

  private void setSelectedError() {
    boolean noloadAverage;
    if (Utilities.isWindowsOS()) {
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
    for (int i = 0; i < numCpus; i++) {
      param.addMachineName(cellComputer.getLabel());
    }
  }

  final void resetResults() {
    cellSuccesses.setValue();
  }

  final long getSuccesses() {
    return cellSuccesses.getLongValue();
  }

  final int getCPUsSelected() {
    if (!isSelected()) {
      return 0;
    }
    if (cellCPUsSelected instanceof SpinnerCell) {
      return ((SpinnerCell) cellCPUsSelected).getValue();
    }
    int cpusSelected = ((FieldCell) cellCPUsSelected).getIntValue();
    if (cpusSelected == EtomoNumber.INTEGER_NULL_VALUE) {
      cpusSelected = 0;
    }
    return cpusSelected;
  }

  final boolean equals(String computer) {
    if (cellComputer.getLabel().equals(computer)) {
      return true;
    }
    return false;
  }

  final void addSuccess() {
    int successes = cellSuccesses.getIntValue();
    if (successes == EtomoNumber.INTEGER_NULL_VALUE) {
      successes = 1;
    }
    else {
      successes++;
    }
    cellSuccesses.setValue(successes);
  }

  final void addRestart() {
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

  final void setLoadAverage(double load1, double load5, int users,
      String usersTooltip) {
    int numberCpus = cellNumberCpus.getIntValue();
    setLoad(cellLoad1, load1, numberCpus);
    setLoad(cellLoad5, load5, numberCpus);
    cellUsers.setValue(users);
    cellUsers.setToolTipText(usersTooltip);
  }

  final void setCPUUsage(double cpuUsage) {
    int numberCPUs = cellNumberCpus.getIntValue();
    double usage = cpuUsage * numberCPUs / 100.0;
    cellCPUUsage.setWarning(numberCPUs - usage <= .25);
    cellCPUUsage.setValue(usage);
    cellComputer.setWarning(false);
  }

  final void clearLoadAverage(String reason) {
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
    cellFailureReason.setToolTipText(tooltipFormatter.setText(
        "Unable to get the " + loadName + " for this computer.").format());
  }

  final void clearFailureReason(String failureReason) {
    String value = cellFailureReason.getValue();
    if (value == null || !value.equals(failureReason)) {
      return;
    }
    clearFailureReason();
  }

  final void clearFailureReason() {
    cellFailureReason.setValue();
    cellFailureReason.setToolTipText(null);
  }

  private final void setLoad(FieldCell cellLoad, double load, int numberCpus) {
    cellLoad.setWarning(load >= numberCpus);
    cellLoad.setValue(load);
    cellComputer.setWarning(false);
  }

  final int getHeight() {
    return cellComputer.getHeight();
  }

  final String getComputer() {
    return cellComputer.getLabel();
  }

  final int getWidth() {
    int width = 0;
    width += cellComputer.getWidth();
    width += cellCPUsSelected.getWidth();
    if (numberColumn) {
      width += cellNumberCpus.getWidth();
    }
    if (Utilities.isWindowsOS()) {
      width += cellCPUUsage.getWidth();
    }
    else {
      width += cellLoad1.getWidth();
      width += cellLoad5.getWidth();
      width += cellUsers.getWidth();
    }
    if (typeColumn) {
      width += cellCPUType.getWidth();
    }
    if (speedColumn) {
      width += cellSpeed.getWidth();
    }
    if (memoryColumn) {
      width += cellMemory.getWidth();
    }
    if (osColumn) {
      width += cellOS.getWidth();
    }
    width += cellRestarts.getWidth();
    width += cellSuccesses.getWidth();
    width += cellFailureReason.getWidth();
    return width + 3;
  }

  private class ProcessorTableRowActionListener implements ActionListener {
    ProcessorTableRow adaptee;

    ProcessorTableRowActionListener(ProcessorTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.performAction(event);
    }
  }

  private class ProcessorTableRowChangeListener implements ChangeListener {
    ProcessorTableRow adaptee;

    ProcessorTableRowChangeListener(ProcessorTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.stateChanged(event);
    }
  }
}
/**
 * <p> $Log$
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
