package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.Properties;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;

import etomo.EtomoDirector;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.HashedArray;

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
public final class ProcessorTable implements Storable {
  public static final String rcsid = "$Id$";

  public static final String SECTION_TYPE = "Computer";

  private static final String SPEED_ADOC_KEY = "speed";
  private static final String MEMORY_ADOC_KEY = "memory";
  private static final String STORE_PREPEND = "ProcessorTable";

  private static final int MAXIMUM_ROWS = 15;
  private JScrollPane scrollPane;
  private JPanel tablePanel;
  private GridBagLayout layout = null;
  private GridBagConstraints constraints = null;
  private final HeaderCell header1Computer = new HeaderCell("Computer");
  private final HeaderCell header1NumberCPUs = new HeaderCell("# CPUs");
  private final HeaderCell header1Load = new HeaderCell("Load Average");
  private HeaderCell header1CPUType = null;
  private HeaderCell header1Speed = null;
  private HeaderCell header1RAM = null;
  private HeaderCell header1OS = null;
  private final HeaderCell header1Restarts = new HeaderCell("Restarts");
  private final HeaderCell header1Finished = new HeaderCell("Finished");
  private final HeaderCell header1Failure = new HeaderCell("Failure");
  private final HeaderCell header2Computer = new HeaderCell();
  private final HeaderCell header2NumberCPUsUsed = new HeaderCell("Used");
  private HeaderCell header2NumberCPUsMax = null;
  private final HeaderCell header2Load1 = new HeaderCell("1 Min.");
  private final HeaderCell header2Load5 = new HeaderCell("5 Min.");
  private final HeaderCell header2Load15 = new HeaderCell("15 Min.");
  private HeaderCell header2CPUType = null;
  private HeaderCell header2Speed = null;
  private HeaderCell header2RAM = null;
  private HeaderCell header2OS = null;
  private final HeaderCell header2Restarts = new HeaderCell();
  private final HeaderCell header2Finished = new HeaderCell("Chunks");
  private final HeaderCell header2Failure = new HeaderCell("Reason");

  private final HashedArray rows = new HashedArray();
  private final ParallelPanel parent;
  private final AxisID axisID;
  private boolean expanded;

  private boolean numberColumn = false;
  private boolean typeColumn = false;
  private boolean speedColumn = false;
  private boolean memoryColumn = false;
  private boolean osColumn = false;
  private String speedUnits = null;
  private String memoryUnits = null;
  private boolean scrolling = false;

  ProcessorTable(ParallelPanel parent, AxisID axisID) {
    this.parent = parent;
    this.axisID = axisID;
    expanded = true;
    initTable();
    buildScrollPane();
  }

  final void setExpanded(boolean expanded) {
    if (this.expanded == expanded) {
      return;
    }
    this.expanded = expanded;
    buildScrollPane();
  }

  final void buildScrollPane() {
    buildTable();
    //scrollPane
    scrollPane = new JScrollPane(tablePanel);
    scrollPane
        .setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    //configure
    UIHarness.INSTANCE.repaintWindow();
  }

  private final void initTable() {
    //build rows
    //get autodoc
    Autodoc autodoc = ParallelPanel.getAutodoc(axisID);
    if (autodoc == null) {
      return;
    }
    //get units
    Attribute unitsAttribute = autodoc.getAttribute("units");
    //get speed units
    try {
      speedUnits = unitsAttribute.getAttribute(SPEED_ADOC_KEY)
          .getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    //get memory units
    try {
      memoryUnits = unitsAttribute.getAttribute(MEMORY_ADOC_KEY)
          .getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    //get first section
    SectionLocation sectionLocation = autodoc.getSectionLocation(SECTION_TYPE);
    Section computer = autodoc.nextSection(sectionLocation);
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
      //add the row to the rows HashedArray
      rows.add(computerName, row);
      //get the next section
      computer = autodoc.nextSection(sectionLocation);
    }
    //create headers
    //header 1
    if (typeColumn) {
      header1CPUType = new HeaderCell("CPU Type");
    }
    if (speedColumn) {
      header1Speed = new HeaderCell("Speed");
    }
    if (memoryColumn) {
      header1RAM = new HeaderCell("RAM");
    }
    if (osColumn) {
      header1OS = new HeaderCell("OS");
    }
    //header row 2
    if (numberColumn) {
      header2NumberCPUsMax = new HeaderCell("Max.");
    }
    if (typeColumn) {
      header2CPUType = new HeaderCell();
    }
    if (speedColumn) {
      header2Speed = new HeaderCell(speedUnits);
    }
    if (memoryColumn) {
      header2RAM = new HeaderCell(memoryUnits);
    }
    if (osColumn) {
      header2OS = new HeaderCell();
    }
    EtomoDirector.getInstance().loadPreferences(this, axisID);
    setToolTipText();
  }

  private final void buildTable() {
    tablePanel = new JPanel();
    layout = new GridBagLayout();
    constraints = new GridBagConstraints();
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
    header1Computer.add(tablePanel, layout, constraints);
    if (numberColumn && expanded) {
      constraints.gridwidth = 2;
    }
    header1NumberCPUs.add(tablePanel, layout, constraints);
    constraints.gridwidth = 3;
    header1Load.add(tablePanel, layout, constraints);
    constraints.gridwidth = 1;
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
    header2Load1.add(tablePanel, layout, constraints);
    header2Load5.add(tablePanel, layout, constraints);
    header2Load15.add(tablePanel, layout, constraints);
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
    //loop on rows
    for (int i = 0; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      row.deleteRow();
      if (expanded || row.isSelected()) {
        row.setNumberColumn(numberColumn && expanded);
        row.setTypeColumn(typeColumn && expanded);
        row.setSpeedColumn(speedColumn && expanded);
        row.setMemoryColumn(memoryColumn && expanded);
        row.setOSColumn(osColumn && expanded);
        row.addRow();
      }
    }
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

  private class RunPack implements Runnable {
    public void run() {
      JScrollPane scrollPane = getScrollPane();
      if (scrollPane == null) {
        return;
      }
      //get width
      double width = getWidth();
      if (width == 0) {
        width = 250;
      }
      double height = getHeight();
      //adjust width by scroll bar width
      width += scrollPane.getVerticalScrollBar().getWidth() + 5;
      //set current size
      Dimension size = new Dimension();
      size.setSize(width, height);
      scrollPane.setMaximumSize(size);
      if (isScrolling()) {
        //to prevent an empty space at the bottom of the parallel panel when
        //scrolling, set the preferred size
        scrollPane.setPreferredSize(size);
      }
    }
  }

  final void pack() {
    SwingUtilities.invokeLater(new RunPack());
  }

  /**
   * Get the total height of the table
   * Get the expanded height if getExpandedHeight is true
   * @param getExpandedHeight
   * @return
   */
  protected final double getHeight() {
    int height = header1Computer.getHeight();
    if (height == 1) {
      height = 21;//guess the height if the top header cell is not displayed
    }
    int approxCellheight = height;//save an approximate cell height
    //add the height of the second header
    int cellHeight = header2Computer.getHeight();
    if (cellHeight == 1) {
      height += approxCellheight;
    }
    else {
      height += header2Computer.getHeight();
    }
    if (rows == null || rows.size() == 0) {
      return height;
    }
    //add the height of each row
    int totalRows = 0;
    scrolling = false;
    for (int i = 0; i < rows.size(); i++) {
      //limit the size of the table
      if (totalRows >= MAXIMUM_ROWS) {
        scrolling = true;
        break;
      }
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      //check if row is currently displayed
      if (row.isDisplayed()) {
        totalRows++;
        cellHeight = row.getHeight();
        if (cellHeight == 1) {
          height += approxCellheight;
        }
        else {
          height += row.getHeight();
        }
      }
    }
    return height;
  }

  private final void calcMaximumHeight() {

  }

  protected final double getWidth() {
    int width = header2Computer.getWidth();
    width += header2NumberCPUsUsed.getWidth();
    if (useNumberColumn()) {
      width += header2NumberCPUsMax.getWidth();
    }
    width += header2Load1.getWidth();
    width += header2Load5.getWidth();
    width += header2Load15.getWidth();
    if (useTypeColumn()) {
      width += header2CPUType.getWidth();
    }
    if (useSpeedColumn()) {
      width += header2Speed.getWidth();
    }
    if (useMemoryColumn()) {
      width += header2RAM.getWidth();
    }
    if (useOSColumn()) {
      width += header2OS.getWidth();
    }
    width += header2Restarts.getWidth();
    width += header2Finished.getWidth();
    width += header2Failure.getWidth();
    return width;
  }

  private final boolean useNumberColumn() {
    return numberColumn && expanded;
  }

  private final boolean useTypeColumn() {
    return typeColumn && expanded;
  }

  private final boolean useSpeedColumn() {
    return speedColumn && expanded;
  }

  private final boolean useMemoryColumn() {
    return memoryColumn && expanded;
  }

  private final boolean useOSColumn() {
    return osColumn && expanded;
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
    return "Click on check boxes in the " + header1Computer.getText()
        + " column and use the spinner in the " + header1NumberCPUs.getText()
        + " " + header2NumberCPUsUsed.getText() + " column where available.";
  }

  final void startGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.startGetLoadAverage(((ProcessorTableRow) rows.get(i))
          .getComputer());
    }
  }

  final void stopGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.stopGetLoadAverage(((ProcessorTableRow) rows.get(i))
          .getComputer());
    }
  }

  final void setLoadAverage(String computer, double load1, double load5,
      double load15) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).setLoadAverage(load1, load5,
        load15);
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

  final void clearFailureReason(boolean selectedComputers) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      ProcessorTableRow row = (ProcessorTableRow) rows.get(i);
      if (!selectedComputers || row.isSelected()) {
        row.clearFailureReason();
      }
    }
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    if (rows == null) {
      return;
    }
    String group;
    if (prepend == "") {
      prepend = STORE_PREPEND;
    }
    else {
      prepend += "." + STORE_PREPEND;
    }
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).store(props, prepend);
    }
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public final void load(Properties props) {
    load(props, "");
  }

  public final void load(Properties props, String prepend) {
    if (rows == null) {
      return;
    }
    String group;
    if (prepend == "") {
      prepend = STORE_PREPEND;
    }
    else {
      prepend += "." + STORE_PREPEND;
    }
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).load(props, prepend);
    }
  }

  protected final JScrollPane getScrollPane() {
    return scrollPane;
  }

  protected final boolean isScrolling() {
    return scrolling;
  }

  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = tooltipFormatter.setText(
        "Select computers to use for parallel processing.").format();
    header1Computer.setToolTipText(text);
    header2Computer.setToolTipText(text);
    text = tooltipFormatter.setText(
        "Select the number of CPUs to use for each computer.").format();
    header1NumberCPUs.setToolTipText(text);
    header2NumberCPUsUsed.setToolTipText(text);
    header2NumberCPUsMax.setToolTipText(tooltipFormatter.setText(
        "The maximum number of CPUs available on each computer.").format());
    header1Load.setToolTipText(tooltipFormatter.setText(
        "Represents how busy each computer is.").format());
    header2Load1.setToolTipText(tooltipFormatter.setText(
        "The load averaged over one minute.").format());
    header2Load5.setToolTipText(tooltipFormatter.setText(
        "The load averaged over five minutes.").format());
    header2Load15.setToolTipText(tooltipFormatter.setText(
        "The load averaged over fifteen minutes.").format());
    text = tooltipFormatter.setText(
        "The number of times processes failed on each computer.").format();
    header1Restarts.setToolTipText(text);
    header2Restarts.setToolTipText(text);
    text = tooltipFormatter
        .setText(
            "The number of processes each computer completed for a distributed process.")
        .format();
    header1Finished.setToolTipText(text);
    header2Finished.setToolTipText(text);
  }

}
/**
 * <p> $Log$
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