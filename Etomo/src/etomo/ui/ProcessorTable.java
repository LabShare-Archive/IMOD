package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Vector;


import javax.swing.JPanel;
import javax.swing.border.LineBorder;

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
final class ProcessorTable {
  public static  final String  rcsid =  "$Id$";
  
  private JPanel rootPanel = new JPanel();
  
  private GridBagLayout layout = new GridBagLayout();
  private GridBagConstraints constraints = new GridBagConstraints();
  
  private Vector rows = new Vector();
  
  ProcessorTable() {
    createTable(rootPanel);
  }
  
  private void createTable(JPanel panel) {
    panel.setBorder(LineBorder.createBlackLineBorder());
    panel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    //header row 1
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    new HeaderCell("Computer").add(panel, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    new HeaderCell("Number CPUs").add(panel, layout, constraints);
    constraints.gridwidth = 3;
    new HeaderCell("Load Average").add(panel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("CPU Type").add(panel, layout, constraints);
    new HeaderCell("OS").add(panel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("Restarts").add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Successes").add(panel, layout, constraints);
    //header row 2
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    new HeaderCell().add(panel, layout, constraints);
    new HeaderCell("Used").add(panel, layout, constraints);
    new HeaderCell("Max.").add(panel, layout, constraints);
    new HeaderCell("1 Min.").add(panel, layout, constraints);
    new HeaderCell("5 Min.").add(panel, layout, constraints);
    new HeaderCell("15 Min.").add(panel, layout, constraints);
    new HeaderCell().add(panel, layout, constraints);
    new HeaderCell().add(panel, layout, constraints);
    new HeaderCell().add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell().add(panel, layout, constraints);
    //rows
    ProcessorTableRow row = new ProcessorTableRow(this, "bebop", 0.20, 0.19, 0.18);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "mustang", 2, 0.67, 0.61, 0.70);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "monalisa", 2, 1.22, 1.27, 1.08);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "wanderer", 0.00, 0.00, 0.00);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "shrek", "Opteron", 2, 1.00, 1.02, 1.34);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "druid", 0.24, 0.05, 0.02);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "tubule", "Opteron", 2.00, 2.00, 1.91);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "bigfoot", 2, 0.00, 0.00, 0.00);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "blkbox2", 0.00, 0.00, 0.09);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "sanguine", 0.60, 0.29, 0.19);
    row.addRow();
    rows.add(row);
    row = new ProcessorTableRow(this, "thot", 0.00, 0.00, 0.00);
    row.addRow();
    rows.add(row);
  }
  
  JPanel getRootPanel() {
    return rootPanel;
  }
  
  GridBagLayout getTableLayout() {
    return layout;
  }
  
  GridBagConstraints getTableConstraints() {
    return constraints;
  }
  
  void resetResults() {
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).resetResults();
    }
  }
  
  void showResults() {
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).showResults();
    }
  }

}
/**
* <p> $Log$ </p>
*/