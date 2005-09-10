package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;

import etomo.comscript.ProcesschunksParam;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.HashedArray;

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

  private static final String SPEED_ADOC_KEY = "speed";
  private static final String MEMORY_ADOC_KEY = "memory";
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
  private HeaderCell hdrComputer = null;
  private HeaderCell hdrNumberCpus = null;
  private HeaderCell hdrNumberCpusUsed = null;

  private HashedArray rows = new HashedArray();
  private ParallelPanel parent = null;
  private AxisID axisID;
  private int restartsError = -1;

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
    boolean numberColumn = false;
    boolean typeColumn = false;
    boolean speedColumn = false;
    boolean memoryColumn = false;
    boolean osColumn = false;
    String speedUnits = null;
    String memoryUnits = null;
    //build rows
    //get autodoc
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.getInstance(Autodoc.CPU, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    if (autodoc == null) {
      System.out.println("autodoc null");
      return;
    }
    //get units
    Attribute unitsAttribute = autodoc.getAttribute("units");
    //get speed units
    try {
      speedUnits = unitsAttribute.getAttribute(SPEED_ADOC_KEY).getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    //get memory units
    try {
      memoryUnits = unitsAttribute.getAttribute(MEMORY_ADOC_KEY).getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    //get first section
    SectionLocation sectionLocation = autodoc
        .getFirstSectionLocation("Computer");
    Section computer = autodoc.getSection(sectionLocation);
    EtomoNumber number = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    //loop on sections
    while (computer != null) {
      //get name of the section
      String computerName = computer.getName();
      //get the number attribute
      //set numberColumn to true if an number attribute is returned
      number.set(1);
      try {
        number.set(computer.getAttribute("number").getUnformattedValue());
        numberColumn = true;
      }
      catch (NullPointerException e) {
      }
      //get the type attribute
      //set typeColumn to true if an type attribute is returned
      String type = null;
      try {
        type = computer.getAttribute("type").getUnformattedValue();
        typeColumn = true;
      }
      catch (NullPointerException e) {
      }
      //get the speed attribute
      //set speedColumn to true if an speed attribute is returned
      String speed = null;
      try {
        speed = computer.getAttribute(SPEED_ADOC_KEY).getUnformattedValue();
        speedColumn = true;
      }
      catch (NullPointerException e) {
      }
      //get the memory attribute
      //set memoryColumn to true if an memory attribute is returned
      String memory = null;
      try {
        memory = computer.getAttribute(MEMORY_ADOC_KEY).getUnformattedValue();
        memoryColumn = true;
      }
      catch (NullPointerException e) {
      }
      //get the os attribute
      //set osColumn to true if an os attribute is returned
      String os = null;
      try {
        os = computer.getAttribute("os").getUnformattedValue();
        osColumn = true;
      }
      catch (NullPointerException e) {
      }
      //create the row
      ProcessorTableRow row = new ProcessorTableRow(this, computerName, number
          .getInt(), type, speed, memory, os);
      if (restartsError != -1) {
        row.setRestartsError(restartsError);
      }
      //add the row to the rows HashedArray
      rows.add(computerName, row);
      //get the next section
      computer = autodoc.nextSection(sectionLocation);
    }
    //build table
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
    if (numberColumn) {
      constraints.gridwidth = 2;
    }
    hdrNumberCpus = new HeaderCell("# CPUs");
    hdrNumberCpus.add(tablePanel, layout, constraints);
    constraints.gridwidth = 3;
    new HeaderCell("Load Average").add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
    if (typeColumn) {
      new HeaderCell("CPU Type").add(tablePanel, layout, constraints);
    }
    if (speedColumn) {
      new HeaderCell("Speed").add(tablePanel, layout, constraints);
    }
    if (memoryColumn) {
      new HeaderCell("RAM").add(tablePanel, layout, constraints);
    }
    if (osColumn) {
      new HeaderCell("OS").add(tablePanel, layout, constraints);
    }
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
    if (numberColumn) {
      new HeaderCell("Max.").add(tablePanel, layout, constraints);
    }
    new HeaderCell("1 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("5 Min.").add(tablePanel, layout, constraints);
    new HeaderCell("15 Min.").add(tablePanel, layout, constraints);
    if (typeColumn) {
      new HeaderCell().add(tablePanel, layout, constraints);
    }
    if (speedColumn) {
      new HeaderCell(speedUnits).add(tablePanel, layout, constraints);
    }
    if (memoryColumn) {
      new HeaderCell(memoryUnits).add(tablePanel, layout, constraints);
    }
    if (osColumn) {
      new HeaderCell().add(tablePanel, layout, constraints);
    }
    new HeaderCell().add(tablePanel, layout, constraints);
    new HeaderCell("Chunks").add(tablePanel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    new HeaderCell("Reason").add(tablePanel, layout, constraints);
    //add rows to the table
    //loop on rows
    for (int i = 0; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);  
      row.setNumberColumn(numberColumn);
      row.setTypeColumn(typeColumn);
      row.setSpeedColumn(speedColumn);
      row.setMemoryColumn(memoryColumn);
      row.setOSColumn(osColumn);
      row.addRow();
    }
    /*
    ProcessorTableRow row = new ProcessorTableRow(this, "bebop");
    row.addRow();
    rows.add("bebop", row);
    row = new ProcessorTableRow(this, "mustang", 2);
    row.addRow();
    rows.add("mustang", row);
    row = new ProcessorTableRow(this, "monalisa", 2);
    row.addRow();
    rows.add("monalisa", row);
    row = new ProcessorTableRow(this, "wanderer");
    row.addRow();
    rows.add("wanderer", row);
    row = new ProcessorTableRow(this, "shrek", 2, "Opteron");
    row.addRow();
    rows.add("shrek", row);
    row = new ProcessorTableRow(this, "druid");
    row.addRow();
    rows.add("druid", row);
    row = new ProcessorTableRow(this, "tubule", 2, "Opteron");
    row.addRow();
    rows.add("tubule", row);
    row = new ProcessorTableRow(this, "bigfoot", 2);
    row.addRow();
    rows.add("bigfoot", row);
    row = new ProcessorTableRow(this, "blkbox2");
    row.addRow();
    rows.add("blkbox2", row);
    row = new ProcessorTableRow(this, "sanguine");
    row.addRow();
    rows.add("sanguine", row);
    row = new ProcessorTableRow(this, "thot");
    row.addRow();
    rows.add("thot", row);
    row = new ProcessorTableRow(this, "ashtray", "G5");
    row.addRow();
    rows.add("ashtray", row);
    */
  }
  
  final void setRestartsError(int restartsError) {
    this.restartsError = restartsError;
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

  final void msgCPUsSelectedChanged() {
    if (rows == null) {
      return;
    }
    parent.msgCPUsSelectedChanged(getCPUsSelected());
  }

  final int getCPUsSelected() {
    if (rows == null) {
      return 0;
    }
    int cpusSelected = 0;
    for (int i = 0; i < rows.size(); i++) {
      cpusSelected += ((ProcessorTableRow) rows.get(i)).getCPUsSelected();
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
  
  final void msgDropped(String computer, String reason) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.msgDropped(reason);
  }
  
  final String getHelpMessage() {
    return "Click on check boxes in the " + hdrComputer.getText()
        + " column and use the spinner in the " + hdrNumberCpus.getText() + " "
        + hdrNumberCpusUsed.getText() + " column where available.";
  }
  
  final void startGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.startGetLoadAverage(((ProcessorTableRow) rows.get(i)).getComputer());
    }
  }
  
  final void stopGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.stopGetLoadAverage(((ProcessorTableRow) rows.get(i)).getComputer());
    }
  }
  
  final void setLoadAverage(String computer, double load1, double load5, double load15) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).setLoadAverage(load1, load5, load15);
  }
  
  final void clearLoadAverage(String computer, String reason) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).clearLoadAverage(reason);
  }
  
  final void clearFailureReason(String computer) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).clearFailureReason();
  }
}
/**
 * <p> $Log$
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