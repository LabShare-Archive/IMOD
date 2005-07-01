package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;

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
  private InputCell cellNumberCpusUsed = null;
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
      restarts = 7;
      successes = 1;
      return;
    }
    double load = (load1 + load5 + load15) / 3;
    restarts = Math.round(load);
    if (cpuType.equals("")) {
      successes = Math.round(3 - load);
    }
    else {
      successes = Math.round((3 - load) * 3);
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
      cellNumberCpusUsed = new SpinnerCell(0, numCpus);
      ((SpinnerCell) cellNumberCpusUsed).setValue(1);
      ((SpinnerCell) cellNumberCpusUsed).setDisabledValue(0);
    }
    else {
      cellNumberCpusUsed = new FieldCell();
      ((FieldCell) cellNumberCpusUsed).setValue(1);
      cellNumberCpusUsed.setEnabled(false);
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
    JPanel panel = table.getRootPanel();
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weighty = 0.0;
    constraints.weightx = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    cellComputer.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    cellNumberCpusUsed.add(panel, layout, constraints);
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
    if (event.getActionCommand().equals(cellComputer.getLabel())) {
      setSelected(cellComputer.isSelected());
    }
  }

  private void setSelected(boolean selected) {
    if (cellNumberCpusUsed instanceof SpinnerCell) {
      SpinnerCell cell = (SpinnerCell) cellNumberCpusUsed;
      cell.setEnabled(selected);
    }
    else {
      FieldCell cell = (FieldCell) cellNumberCpusUsed;
      cell.setHideValue(!selected);
    }
  }

  void resetResults() {
    cellRestarts.setValueEmpty();
    cellSuccesses.setValueEmpty();
  }

  void showResults() {
    if (cellComputer.isSelected()) {
      //set cellRestarts
      long currentValue = cellRestarts.getLongValue();
      cellRestarts.setValue(Math.max(currentValue, restarts));
      //set cellSuccesses
      currentValue = cellSuccesses.getLongValue();
      long newValue;
      if (cellNumberCpusUsed instanceof SpinnerCell) {
        newValue = successes
            * ((SpinnerCell) cellNumberCpusUsed).getValue();
      }
      else {
        newValue = successes;
      }
      cellSuccesses.setValue(Math.max(currentValue, newValue));
    }
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
}
/**
 * <p> $Log$ </p>
 */