package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;

import etomo.comscript.ProcesschunksParam;
import etomo.type.AxisID;

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

  private static final int maxRows = 15;
  private static final double initialHeight = (maxRows + 2) * 20;//initially estimate the row height to be 20 pixels
  private static final double maxWidth = UIUtilities.getScreenSize().getWidth();
  private double maxHeight = 0;
  private double lastWidth = 0;
  private int scrollBarWidth = 0;
  private JScrollPane scrollPane;
  private JPanel tablePanel;

  private GridBagLayout layout = new GridBagLayout();
  private GridBagConstraints constraints = new GridBagConstraints();
  private boolean tableCreated = false;
  private HeaderCell hdrComputer = null;
  private HeaderCell hdrNumberCpus = null;
  private HeaderCell hdrNumberCpusUsed = null;

  private Vector rows = new Vector();
  private ParallelPanel parent = null;
  private AxisID axisID;

  ProcessorTable(ParallelPanel parent, AxisID axisID) {
    this.parent = parent;
    this.axisID = axisID;
    //panels
    tablePanel = new JPanel();
    //tablePanel
    createTable();
    //scrollPane
    scrollPane = new JScrollPane(tablePanel);
    //configure
    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    setMaximumSize(initialHeight);
  }

  private final void createTable() {
    tablePanel.setBorder(LineBorder.createBlackLineBorder());
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
    hdrComputer = new HeaderCell("Computer");
    hdrComputer.add(tablePanel, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    hdrNumberCpus = new HeaderCell("Number CPUs");
    hdrNumberCpus.add(tablePanel, layout, constraints);
    constraints.gridwidth = 3;
    new HeaderCell("Load Average").add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("CPU Type").add(tablePanel, layout, constraints);
    new HeaderCell("OS").add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
    new HeaderCell("Restarts").add(tablePanel, layout, constraints);
    new HeaderCell("Finished").add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Failure").add(tablePanel, layout, constraints);
    //header row 2
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    new HeaderCell().add(tablePanel, layout, constraints);
    hdrNumberCpusUsed = new HeaderCell("Used");
    hdrNumberCpusUsed.add(tablePanel, layout, constraints);
    new HeaderCell("Max.").add(tablePanel, layout, constraints);
    new HeaderCell("1 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("5 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("15 Min.").add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell("Chunks").add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Reason").add(tablePanel, layout, constraints);
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

  final GridBagLayout getTableLayout() {
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

  private final void setMaximumSize(double height) {
    if (height == 0) {
      return;
    }
    Dimension maxSize = new Dimension();
    maxSize.setSize(maxWidth, height);
    scrollPane.setMaximumSize(maxSize);
  }
  
  private final void setPreferredSize(double width, double height) {
    if (width == 0 || height == 0) {
      return;
    }
    Dimension maxSize = new Dimension();
    maxSize.setSize(width, height);
    scrollPane.setPreferredSize(maxSize);
    lastWidth = width;
  }
  
  private class RunPack implements Runnable {
    public void run() {
      if (scrollPane == null) {
        return;
      }
      //set maximum height
      if (maxHeight == 0) {
        calcMaximumHeight();
      }
      //reset maximum height if table is too small for the the current tableHeight
      int height = getMinimumHeight();
      if (height < maxHeight) {
        setMaximumSize(height);
      }
      //to prevent an empty space at the bottom of the parallel panel when the
      //vertical scroll bar is used, set the preferred size
      double width = getWidth();
      if (width > 0) {
        //add scroll bar width to width
        if (scrollBarWidth == 0) {
          scrollBarWidth = scrollBarWidth = scrollPane.getVerticalScrollBar().getWidth() + 1;
        }
        width += scrollBarWidth;
        setPreferredSize(width, Math.min(height, maxHeight));
      }
    }
  }

  final void pack() {
    SwingUtilities.invokeLater(new RunPack());
  }
  
  private final int getMinimumHeight() {
    if (hdrNumberCpusUsed == null) {
      return 0;
    }
    int height = 0;
    int fieldHeight = hdrNumberCpusUsed.getHeight();
    height = fieldHeight * 2;
    if (rows != null) {
      height += fieldHeight * rows.size();
    }
    return height;
  }
  
  private final void calcMaximumHeight() {
    if (hdrNumberCpusUsed == null) {
      maxHeight = 0;
    }
    maxHeight = hdrNumberCpusUsed.getHeight() * (2 + maxRows);
  }
  
  private final double getWidth() {
    if (rows == null || rows.size() == 0) {
      return 0;
    }
    return ((ProcessorTableRow) rows.get(0)).getWidth();
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
  
  final void getParameters(ProcesschunksParam param) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).getParameters(param);
    }
  }
  
  private final ProcessorTableRow getRow(String computer) {
    if (rows == null) {
      return null;
    }
    for (int i = 0; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      if (row.equals(computer)) {
        return row;
      }
    }
    return null;
  }

  final void addRestart(String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addRestart();
  }
  
  final void addSuccess(String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addSuccess();
  }
  
  final void drop(String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.drop();
  }
  
  final String getHelpMessage() {
    return "Click on check boxes in the " + hdrComputer.getText()
        + " column and use the spinner in the " + hdrNumberCpus.getText() + " "
        + hdrNumberCpusUsed.getText() + " column where available.";
  }
}
/**
 * <p> $Log$
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