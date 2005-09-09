package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.comscript.ProcesschunksParam;
import etomo.type.EtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class ProcessorTableRow {
  public static final String rcsid = "$Id$";

  private ProcessorTable table = null;
  private CheckBoxCell cellComputer = new CheckBoxCell();
  private FieldCell cellCPUType = new FieldCell();
  private InputCell cellCPUsSelected = null;
  private FieldCell cellNumberCpus = new FieldCell();
  private FieldCell cellLoad1 = new FieldCell();
  private FieldCell cellLoad5 = new FieldCell();
  private FieldCell cellLoad15 = new FieldCell();
  private FieldCell cellRestarts = new FieldCell();
  private FieldCell cellSuccesses = new FieldCell();
  private FieldCell cellFailureReason = new FieldCell();
  private FieldCell cellSpeed = new FieldCell();
  private FieldCell cellMemory = new FieldCell();
  private FieldCell cellOS = new FieldCell();
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
  private int restartsError = 10000;
  
  ProcessorTableRow(ProcessorTable table, String computerName, int numCpus,
      String cpuType, String speed, String memory, String os) {
    this.table = table;
    this.computerName = computerName;
    this.cpuType = cpuType;
    this.numCpus = numCpus;
    this.speed = speed;
    this.memory = memory;
    this.os = os;
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
      spinnerCell.setValue(1);
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
    cellLoad15.setEnabled(false);
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
  
  final void setRestartsError(int restartsError) {
    this.restartsError= restartsError;
  }

  void addRow() {
    if (!rowInitialized) {
      initRow();
    }
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
    cellLoad1.add(panel, layout, constraints);
    cellLoad5.add(panel, layout, constraints);
    cellLoad15.add(panel, layout, constraints);
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

  private void performAction(ActionEvent event) {
    setSelected(cellComputer.isSelected());
  }
  
  private void stateChanged(ChangeEvent event) {
    table.msgCPUsSelectedChanged();
  }
  
  final void msgDropped(String reason) {
    cellComputer.setSelected(false);
    setSelected(false);
    setFailureReason(reason);
  }
  
  private final void setFailureReason(String reason) {
    cellFailureReason.setValue(reason);
    cellFailureReason.setError(reason != null && !reason.matches("\\s+"));
  }

  private void setSelected(boolean selected) {
    if (selected) {
      cellRestarts.setError(false);
    }
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
    cellComputer.setError(cellComputer.isSelected() && cellLoad1.isEmpty()
        && cellLoad5.isEmpty() && cellLoad15.isEmpty());
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
    if (restarts >= restartsError) {
      cellRestarts.setError(true);
    }
    else if (restarts > 0) {
      cellRestarts.setWarning(true);
    }
  }
  
  final void setLoadAverage(double load1, double load5, double load15) {
    int numberCpus = cellNumberCpus.getIntValue();
    setLoad(cellLoad1, load1, numberCpus);
    setLoad(cellLoad5, load5, numberCpus);
    setLoad(cellLoad15, load15, numberCpus);
  }
  
  final void clearLoadAverage(String reason) {
    int numberCpus = cellNumberCpus.getIntValue();
    cellLoad1.setValue();
    cellLoad1.setWarning(false);
    cellLoad5.setValue();
    cellLoad5.setWarning(false);
    cellLoad15.setValue();
    cellLoad15.setWarning(false);
    setSelectedError();
    setFailureReason(reason);
  }
  
  private final void setLoad(FieldCell cellLoad, double load, int numberCpus) {
    cellLoad.setWarning(load >= numberCpus);
    cellLoad.setValue(load);
    cellComputer.setError(false);
  }
  
  final int getHeight() {
    return cellComputer.getHeight();
  }
  
  final String getComputer() {
    return cellComputer.getLabel();
  }
  
  final int getWidth() {
    int width = 0;
    if (cellComputer != null) {
      width += cellComputer.getWidth();
    }
    if (cellCPUsSelected != null) {
      width += cellCPUsSelected.getWidth();
    }
    if (cellNumberCpus != null && numberColumn) {
      width += cellNumberCpus.getWidth();
    }
    if (cellLoad1 != null) {
      width += cellLoad1.getWidth();
    }
    if (cellLoad5 != null) {
      width += cellLoad5.getWidth();
    }
    if (cellLoad15 != null) {
      width += cellLoad15.getWidth();
    }
    if (cellCPUType != null && typeColumn) {
      width += cellCPUType.getWidth();
    }
    if (cellSpeed != null && speedColumn) {
      width += cellSpeed.getWidth();
    }
    if (cellMemory != null && memoryColumn) {
      width += cellMemory.getWidth();
    }
    if (cellOS != null && osColumn) {
      width += cellOS.getWidth();
    }
    if (cellRestarts != null) {
      width += cellRestarts.getWidth();
    }
    if (cellSuccesses != null) {
      width += cellSuccesses.getWidth();
    }
    if (cellFailureReason != null) {
      width += cellFailureReason.getWidth();
    }
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