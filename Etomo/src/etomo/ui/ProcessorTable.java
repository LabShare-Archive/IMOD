package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
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
  public static final String rcsid = "$Id$";

  private int tableHeight = 300;
  private JScrollPane scrollPane;
  private JPanel tablePanel;

  private TableLayout layout = new TableLayout();
  private GridBagConstraints constraints = new GridBagConstraints();
  private boolean tableCreated = false;
  private int width = 0;
  private HeaderCell unweightedHeaderCell = null;

  private Vector rows = new Vector();
  private ParallelPanel parent = null;

  ProcessorTable(ParallelPanel parent) {
    this.parent = parent;
    //panels
    tablePanel = new JPanel();
    //tablePanel
    createTable();
    //scrollPane
    scrollPane = new JScrollPane(tablePanel);
    //scrollPane.setColumnHeaderView(tablePanel);
    //configure
    setMaximumSize(tableHeight);
  }

  private final void createTable() {
    tablePanel.setBorder(LineBorder.createBlackLineBorder());
    tablePanel.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    //header row 1
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    new HeaderCell("Computer").add(tablePanel, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    new HeaderCell("Number CPUs").add(tablePanel, layout, constraints);
    constraints.gridwidth = 3;
    new HeaderCell("Load Average").add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("CPU Type").add(tablePanel, layout, constraints);
    new HeaderCell("OS").add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("Restarts").add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Finished").add(tablePanel, layout, constraints);
    //header row 2
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    new HeaderCell().add(tablePanel, layout, constraints);
    unweightedHeaderCell = new HeaderCell("Used");
    unweightedHeaderCell.add(tablePanel, layout, constraints);
    new HeaderCell("Max.").add(tablePanel, layout, constraints);
    new HeaderCell("1 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("5 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("15 Min.").add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Chunks").add(tablePanel, layout, constraints);
    //rows
    ProcessorTableRow row = new ProcessorTableRow(this, "bebop", 0.20, 0.19,
        0.18);
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
    tableCreated = true;
  }

  final Container getContainer() {
    return scrollPane;
  }

  final JPanel getTablePanel() {
    return tablePanel;
  }

  final TableLayout getTableLayout() {
    return layout;
  }

  final GridBagConstraints getTableConstraints() {
    return constraints;
  }

  final void resetResults() {
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).resetResults();
    }
  }

  final long getTotalSuccesses() {
    long successes = 0;
    for (int i = 0; i < rows.size(); i++) {
      successes += ((ProcessorTableRow) rows.get(i)).getSuccesses();
    }
    return successes;
  }

  final void signalCpusSelectedChanged() {
    if (!tableCreated) {
      return;
    }
    parent.signalCpusSelectedChanged(getCpusSelected());
  }

  final int getCpusSelected() {
    if (!tableCreated) {
      return 0;
    }
    int cpusSelected = 0;
    for (int i = 0; i < rows.size(); i++) {
      cpusSelected += ((ProcessorTableRow) rows.get(i)).getCpusSelected();
    }
    return cpusSelected;
  }

  private final void setMaximumSize(int height) {
    Dimension screenSize = UIUtilities.getScreenSize();
    Dimension paneSize = new Dimension();
    paneSize.setSize(screenSize.getWidth(), new Integer(height).doubleValue());
    scrollPane.setMaximumSize(paneSize);
  }

  final void setSize() {
    if (scrollPane == null) {
      return;
    }
    //correct maximum height if table is too small for the the current tableHeight
    if (rows != null) {
      int rowsSize = rows.size();
      if (rowsSize > 0) {
        ProcessorTableRow row = (ProcessorTableRow) rows.get(0);
        int cellHeight = row.getHeight();
        int headerCellHeight = unweightedHeaderCell.getHeight();
        //table height is the sum of the height of the cells plus the bottom
        //borders.  Subtract 1 from each of the borders because the cells are
        //next to each other
        if (cellHeight > 0 && headerCellHeight > 0) {
          int calcTableHeight = (cellHeight + row.getBorderHeight() - 1)
              * rowsSize
              + (unweightedHeaderCell.getHeight() + unweightedHeaderCell
                  .getBorderHeight() - 1) * 2;
          if (calcTableHeight < tableHeight) {
            tableHeight = calcTableHeight;
            setMaximumSize(calcTableHeight);
          }
        }
      }
    }
    //set preferred size when we know a good width
    int currentWidth = scrollPane.getWidth();
    if (currentWidth > width) {
      width = currentWidth;
      scrollPane.setPreferredSize(new Dimension(width, tableHeight));
    }
  }

  final int getFirstSelectedIndex() {
    if (rows == null) {
      return -1;
    }
    for (int i = 0; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      if (row.isSelected()) {
        return i;
      }
    }
    return -1;
  }

  final int getNextSelectedIndex(int lastIndex) {
    if (rows == null) {
      return -1;
    }
    for (int i = lastIndex + 1; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      if (row.isSelected()) {
        return i;
      }
    }
    return -1;
  }

  final long getRestartFactor(int index) {
    if (rows == null) {
      return 0;
    }
    return ((ProcessorTableRow) rows.get(index)).getRestartFactor();
  }

  final long getSuccessFactor(int index) {
    if (rows == null) {
      return 0;
    }
    return ((ProcessorTableRow) rows.get(index)).getSuccessFactor();
  }

  final void signalRestart(int index) {
    if (!((ProcessorTableRow) rows.get(2)).isSelected()) {
      return;
    }
    ((ProcessorTableRow) rows.get(index)).signalRestart();
  }

  final void signalSuccess(int index) {
    ((ProcessorTableRow) rows.get(index)).signalSuccess();
  }
}
/**
 * <p> $Log$
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