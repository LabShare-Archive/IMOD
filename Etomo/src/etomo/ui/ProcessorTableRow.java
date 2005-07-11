package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

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
  private FieldCell cellOs = new FieldCell();
  private FieldCell cellCpuType = new FieldCell();
  private InputCell cellCpusSelected = null;
  private FieldCell cellNumberCpus = new FieldCell();
  private FieldCell cellLoad1 = new FieldCell();
  private FieldCell cellLoad5 = new FieldCell();
  private FieldCell cellLoad15 = new FieldCell();
  private FieldCell cellRestarts = new FieldCell();
  private FieldCell cellSuccesses = new FieldCell();
  private String computerName;
  private String os = "Linux";
  private String cpuType = "";
  private int numCpus = 1;
  private double load1;
  private double load5;
  private double load15;
  private long restarts = 0;
  private long successes = 0;
  private boolean rowInitialized = false;

  ProcessorTableRow(ProcessorTable table, String computerName, String cpuType,
      int numCpus, double load1, double load5, double load15) {
    this.table = table;
    this.computerName = computerName;
    this.cpuType = cpuType;
    this.numCpus = numCpus;
    this.load1 = load1;
    this.load5 = load5;
    this.load15 = load15;
    if (computerName.equals("monalisa")) {
      restarts = 80;
      successes = 0;
      return;
    }
    double load = (load1 + load5 + load15) / 3;
    restarts = Math.round(load);
    if (cpuType.equals("")) {
      successes = Math.round(3 - load);
    }
    else {
      successes = Math.round((3 - load) * 2);
    }
  }

  ProcessorTableRow(ProcessorTable table, String computerName, double load1,
      double load5, double load15) {
    this(table, computerName, "", 1, load1, load5, load15);
  }

  ProcessorTableRow(ProcessorTable table, String computerName, int numCpus,
      double load1, double load5, double load15) {
    this(table, computerName, "", numCpus, load1, load5, load15);
  }

  ProcessorTableRow(ProcessorTable table, String computerName, String cpuType,
      double load1, double load5, double load15) {
    this(table, computerName, cpuType, 1, load1, load5, load15);
  }
  
  private void initRow() {
    rowInitialized = true;
    cellComputer.setLabel(computerName);
    cellComputer.addActionListener(new ProcessorTableRowActionListener(this));
    cellOs.setValue(os);
    cellOs.setEnabled(false);
    cellCpuType.setValue(cpuType);
    cellCpuType.setEnabled(false);
    if (numCpus > 1) {
      cellCpusSelected = new SpinnerCell(0, numCpus);
      SpinnerCell spinnerCell = (SpinnerCell) cellCpusSelected;
      spinnerCell.addChangeListener(new ProcessorTableRowChangeListener(this));
      spinnerCell.setValue(1);
      spinnerCell.setDisabledValue(0);
    }
    else {
      cellCpusSelected = new FieldCell();
      ((FieldCell) cellCpusSelected).setValue(1);
      cellCpusSelected.setEnabled(false);
    }
    cellNumberCpus.setValue(numCpus);
    cellNumberCpus.setEnabled(false);
    cellLoad1.setEnabled(false);
    cellLoad1.setValue(load1);
    cellLoad5.setEnabled(false);
    cellLoad5.setValue(load5);
    cellLoad15.setEnabled(false);
    cellLoad15.setValue(load15);
    cellRestarts.setEnabled(false);
    cellRestarts.setError(true);
    cellSuccesses.setEnabled(false);
    setSelected(false);
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
    constraints.weightx = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    cellComputer.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    cellCpusSelected.add(panel, layout, constraints);
    cellNumberCpus.add(panel, layout, constraints);
    cellLoad1.add(panel, layout, constraints);
    cellLoad5.add(panel, layout, constraints);
    cellLoad15.add(panel, layout, constraints);
    cellCpuType.add(panel, layout, constraints);
    cellOs.add(panel, layout, constraints);
    cellRestarts.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    cellSuccesses.add(panel, layout, constraints);
  }

  private void performAction(ActionEvent event) {
    setSelected(cellComputer.isSelected());
  }
  
  private void stateChanged(ChangeEvent event) {
    table.signalCpusSelectedChanged();
  }

  private void setSelected(boolean selected) {
    if (cellCpusSelected instanceof SpinnerCell) {
      SpinnerCell cell = (SpinnerCell) cellCpusSelected;
      cell.setEnabled(selected);
    }
    else {
      FieldCell cell = (FieldCell) cellCpusSelected;
      cell.setHideValue(!selected);
    }
    table.signalCpusSelectedChanged();
  }
  
  final boolean isSelected() {
    return cellComputer.isSelected();
  }

  final void resetResults() {
    cellRestarts.setValueEmpty();
    cellSuccesses.setValueEmpty();
  }
  
  final long getRestartFactor() {
    if (!isSelected()) {
      return 0;
    }
    if (cellCpusSelected instanceof SpinnerCell) {
      return restarts * ((SpinnerCell) cellCpusSelected).getValue();
    }
    else {
      return restarts;
    }
  }
  
  final long getSuccessFactor() {
    if (!isSelected()) {
      return 0;
    }
    if (cellCpusSelected instanceof SpinnerCell) {
      return successes * ((SpinnerCell) cellCpusSelected).getValue();
    }
    else {
      return successes;
    }
  }
  
  final long getSuccesses() {
    return cellSuccesses.getLongValue();
  }
  
  final int getCpusSelected() {
    if (cellCpusSelected instanceof SpinnerCell) {
      return ((SpinnerCell) cellCpusSelected).getValue();
    }
    int cpusSelected = ((FieldCell) cellCpusSelected).getIntValue();
    if (cpusSelected == EtomoNumber.INTEGER_NULL_VALUE) {
      cpusSelected = 0;
    }
    return cpusSelected;
  }
  
  final void signalRestart() {
    int restarts = cellRestarts.getIntValue();
    if (restarts == EtomoNumber.INTEGER_NULL_VALUE) {
      restarts = 0;
    }
    cellRestarts.setValue(restarts + 1);
  }
  
  final void signalSuccess() {
    int successes = cellSuccesses.getIntValue();
    if (successes == EtomoNumber.INTEGER_NULL_VALUE) {
      successes = 0;
    }
    cellSuccesses.setValue(successes + 1);
  }
  
  final int getHeight() {
    return cellComputer.getHeight();
  }
  
  final int getBorderHeight() {
    return cellComputer.getBorderHeight();
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
 * <p> Revision 1.2  2005/07/01 23:06:06  sueh
 * <p> bug# 619 Added getCpusSelected, setSuccesses, stateChanged
 * <p>
 * <p> Revision 1.1  2005/07/01 21:22:02  sueh
 * <p> bug# 619 A row in a table containing a list of computers and CPUs
 * <p> </p>
 */