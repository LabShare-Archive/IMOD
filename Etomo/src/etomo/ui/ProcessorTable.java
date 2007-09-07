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

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.HashedArray;
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
public final class ProcessorTable implements Storable {
  public static final String rcsid = "$Id$";

  private static final String STORE_PREPEND = "ProcessorTable";

  private static final int MAXIMUM_ROWS = 15;
  private JScrollPane scrollPane;
  private JPanel tablePanel;
  private GridBagLayout layout = null;
  private GridBagConstraints constraints = null;
  private final HeaderCell header1Computer = new HeaderCell("Computer");
  private final HeaderCell header1NumberCPUs = new HeaderCell("# CPUs");
  private final HeaderCell header1Load = new HeaderCell("Load Average");
  private final HeaderCell header1Users = new HeaderCell("Users");
  private final HeaderCell header1CPUUsage = new HeaderCell("CPU Usage");
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
  private final HeaderCell header2Users = new HeaderCell();
  private final HeaderCell header2CPUUsage = new HeaderCell();
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
  private final BaseManager manager;

  private boolean numberColumn = false;
  private boolean typeColumn = false;
  private boolean speedColumn = false;
  private boolean memoryColumn = false;
  private boolean usersColumn = false;
  private boolean osColumn = false;
  private String speedUnits = null;
  private String memoryUnits = null;
  private boolean scrolling = false;

  private boolean expanded;

  ProcessorTable(BaseManager manager, ParallelPanel parent, AxisID axisID) {
    this.manager = manager;
    this.parent = parent;
    this.axisID = axisID;
    expanded = true;
    initTable();
    buildScrollPane();
  }

  void setExpanded(boolean expanded) {
    if (this.expanded == expanded) {
      return;
    }
    this.expanded = expanded;
    buildScrollPane();
  }

  void buildScrollPane() {
    buildTable();
    //scrollPane
    scrollPane = new JScrollPane(tablePanel);
    scrollPane
        .setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    //configure
    UIHarness.INSTANCE.repaintWindow();
  }

  private void initTable() {
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(axisID, manager);
    usersColumn = cpuAdoc.isUsersColumn();
    //build rows
    //get speed units
    try {
      speedUnits = cpuAdoc.getUnitsSpeed();
    }
    catch (NullPointerException e) {
    }
    //get memory units
    try {
      memoryUnits = cpuAdoc.getUnitsMemory();
    }
    catch (NullPointerException e) {
    }
    //loop through the computers
    EtomoNumber number = new EtomoNumber();
    String name = null;
    //loop on sections
    for (int i = 0; i < cpuAdoc.getNumComputers(); i++) {
      //get name of the section
      String computerName = cpuAdoc.getName(i);
      CpuAdoc.Computer computer = cpuAdoc.getComputer(i);
      //exclude any computer with the "exclude-interface" attribute set to the
      //current interface
      if (!computer.isExcludedInterface(manager.getInterfaceType())
          && (!computer.isExcludedUser(System.getProperty("user.name")))) {
        //get the number attribute
        //set numberColumn to true if an number attribute is returned
        number.set(computer.getNumber());
        if (!number.isDefault()) {
          numberColumn = true;
        }
        //get the type attribute
        //set typeColumn to true if an type attribute is returned
        String type = computer.getType();
        if (!type.matches("\\s*")) {
          typeColumn = true;
        }
        //get the speed attribute
        //set speedColumn to true if an speed attribute is returned
        String speed = computer.getSpeed();
        if (!speed.matches("\\s*")) {
          speedColumn = true;
        }
        //get the memory attribute
        //set memoryColumn to true if an memory attribute is returned
        String memory = computer.getMemory();
        if (!memory.matches("\\s*")) {
          memoryColumn = true;
        }
        //get the os attribute
        //set osColumn to true if an os attribute is returned
        String os = computer.getOs();
        if (!os.matches("\\s*")) {
          osColumn = true;
        }
        //create the row
        ProcessorTableRow row = new ProcessorTableRow(this, computerName,
            number.getInt(), type, speed, memory, os);
        //add the row to the rows HashedArray
        rows.add(computerName, row);
      }
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
    try {
      ParameterStore parameterStore = EtomoDirector.INSTANCE
          .getParameterStore();
      if (parameterStore != null) {
        parameterStore.load(this);
      }
    }
    catch (LogFile.WriteException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load parameters.\n"
          + e.getMessage(), "Etomo Error", axisID);
    }
    setToolTipText();
  }

  private void buildTable() {
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
    if (Utilities.isWindowsOS()) {
      constraints.gridwidth = 1;
      header1CPUUsage.add(tablePanel, layout, constraints);
    }
    else {
      constraints.gridwidth = 2;
      header1Load.add(tablePanel, layout, constraints);
      constraints.gridwidth = 1;
      if (usersColumn) {
        header1Users.add(tablePanel, layout, constraints);
      }
    }
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
    if (Utilities.isWindowsOS()) {
      header2CPUUsage.add(tablePanel, layout, constraints);
    }
    else {
      header2Load1.add(tablePanel, layout, constraints);
      header2Load5.add(tablePanel, layout, constraints);
      if (usersColumn) {
        header2Users.add(tablePanel, layout, constraints);
      }
    }
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
        row.setNumberColumn(useNumberColumn());
        row.setTypeColumn(useTypeColumn());
        row.setSpeedColumn(useSpeedColumn());
        row.setMemoryColumn(useMemoryColumn());
        row.setOSColumn(useOSColumn());
        row.setUsersColumn(usersColumn);
        row.addRow();
      }
    }
  }

  Container getContainer() {
    return scrollPane;
  }

  JPanel getTablePanel() {
    return tablePanel;
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

  long getTotalSuccesses() {
    long successes = 0;
    for (int i = 0; i < rows.size(); i++) {
      successes += ((ProcessorTableRow) rows.get(i)).getSuccesses();
    }
    return successes;
  }

  void msgCPUsSelectedChanged() {
    if (rows == null) {
      return;
    }
    parent.msgCPUsSelectedChanged(getCPUsSelected());
  }

  int getCPUsSelected() {
    if (rows == null) {
      return 0;
    }
    int cpusSelected = 0;
    for (int i = 0; i < rows.size(); i++) {
      cpusSelected += ((ProcessorTableRow) rows.get(i)).getCPUsSelected();
    }
    return cpusSelected;
  }

  private final class RunPack implements Runnable {
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

  void pack() {
    SwingUtilities.invokeLater(new RunPack());
  }

  /**
   * Get the total height of the table
   * Get the expanded height if getExpandedHeight is true
   * @param getExpandedHeight
   * @return
   */
  double getHeight() {
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

  private void calcMaximumHeight() {

  }

  double getWidth() {
    int width = header2Computer.getWidth();
    width += header2NumberCPUsUsed.getWidth();
    if (useNumberColumn()) {
      width += header2NumberCPUsMax.getWidth();
    }
    width += header2Load1.getWidth();
    width += header2Load5.getWidth();
    if (usersColumn) {
      width += header2Users.getWidth();
    }
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

  private boolean useNumberColumn() {
    return numberColumn && expanded;
  }

  private boolean useTypeColumn() {
    return typeColumn && expanded;
  }

  private boolean useSpeedColumn() {
    return speedColumn && expanded;
  }

  private boolean useMemoryColumn() {
    return memoryColumn && expanded;
  }

  private boolean useOSColumn() {
    return osColumn && expanded;
  }

  int getFirstSelectedIndex() {
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

  int getNextSelectedIndex(int lastIndex) {
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

  void getParameters(ProcesschunksParam param) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      ((ProcessorTableRow) rows.get(i)).getParameters(param);
    }
  }

  private ProcessorTableRow getRow(String computer) {
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

  void addRestart(String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addRestart();
  }

  void addSuccess(String computer) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.addSuccess();
  }

  void msgDropped(String computer, String reason) {
    ProcessorTableRow row = getRow(computer);
    if (row == null) {
      return;
    }
    row.msgDropped(reason);
  }

  String getHelpMessage() {
    return "Click on check boxes in the " + header1Computer.getText()
        + " column and use the spinner in the " + header1NumberCPUs.getText()
        + " " + header2NumberCPUsUsed.getText() + " column where available.";
  }

  void startGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.startGetLoadAverage(((ProcessorTableRow) rows.get(i))
          .getComputer());
    }
  }

  void endGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display
          .endGetLoadAverage(((ProcessorTableRow) rows.get(i)).getComputer());
    }
  }

  void stopGetLoadAverage(ParallelPanel display) {
    if (rows == null) {
      return;
    }
    for (int i = 0; i < rows.size(); i++) {
      display.stopGetLoadAverage(((ProcessorTableRow) rows.get(i))
          .getComputer());
    }
  }

  void setLoadAverage(String computer, double load1, double load5, int users,
      String usersTooltip) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).setLoadAverage(load1, load5,
        users, usersTooltip);
  }

  void setCPUUsage(String computer, double cpuUsage) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).setCPUUsage(cpuUsage);
  }

  void clearLoadAverage(String computer, String reason, String tooltip) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).clearLoadAverage(reason, tooltip);
  }

  void clearFailureReason(String computer, String failureReason1,
      String failureReason2) {
    if (rows == null) {
      return;
    }
    ((ProcessorTableRow) rows.get(computer)).clearFailureReason(failureReason1,
        failureReason2);
  }

  void clearFailureReason(boolean selectedComputers) {
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
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
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

  JScrollPane getScrollPane() {
    return scrollPane;
  }

  boolean isScrolling() {
    return scrolling;
  }

  private void setToolTipText() {
    String text;
    text = "Select computers to use for parallel processing.";
    header1Computer.setToolTipText(text);
    header2Computer.setToolTipText(text);
    text = "Select the number of CPUs to use for each computer.";
    header1NumberCPUs.setToolTipText(text);
    header2NumberCPUsUsed.setToolTipText(text);
    if (numberColumn) {
      header2NumberCPUsMax
          .setToolTipText("The maximum number of CPUs available on each computer.");
    }
    if (Utilities.isWindowsOS()) {
      header1CPUUsage
          .setToolTipText("The CPU usage (0 to number of CPUs) averaged over one second.");
    }
    else {
      header1Load.setToolTipText("Represents how busy each computer is.");
      header2Load1.setToolTipText("The load averaged over one minute.");
      header2Load5.setToolTipText("The load averaged over five minutes.");
      text = "The number of users logged into the computer.";
      header1Users.setToolTipText(text);
      header2Users.setToolTipText(text);
    }
    text = "The number of times processes failed on each computer.";
    header1Restarts.setToolTipText(text);
    header2Restarts.setToolTipText(text);
    text = "The number of processes each computer completed for a distributed process.";
    header1Finished.setToolTipText(text);
    header2Finished.setToolTipText(text);
    if (header1CPUType != null) {
      text = "The CPU type of each computer.";
      header1CPUType.setToolTipText(text);
      header2CPUType.setToolTipText(text);
    }
    if (header1Speed != null) {
      text = "The speed of each computer.";
      header1Speed.setToolTipText(text);
      header2Speed.setToolTipText(text);
    }
    if (header1RAM != null) {
      text = "The amount of RAM in each computer.";
      header1RAM.setToolTipText(text);
      header2RAM.setToolTipText(text);
    }
    if (header1OS != null) {
      text = "The operating system of each computer.";
      header1OS.setToolTipText(text);
      header2OS.setToolTipText(text);
    }
    text = "Reason for a failure by the load average or a process";
    header1Failure.setToolTipText(text);
    header2Failure.setToolTipText(text);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.46  2007/07/30 18:54:16  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.45  2007/07/17 21:44:05  sueh
 * <p> bug# 1018 Adding all cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 1.44  2007/05/25 00:28:19  sueh
 * <p> bug# 964 Added tooltip to clearLoadAverage.  Added a second reason to
 * <p> clearFailureReason.
 * <p>
 * <p> Revision 1.43  2007/05/22 21:21:40  sueh
 * <p> bug# 999 Checking CpuAdoc.users before adding a row.
 * <p>
 * <p> Revision 1.42  2007/05/21 22:31:28  sueh
 * <p> bug# 1000 In initTable(), excluding sections based on exclude-interface.
 * <p>
 * <p> Revision 1.41  2007/05/21 18:11:22  sueh
 * <p> bug# 992 Added usersColumn.  Do not display Users column when
 * <p> usersColumn is false.
 * <p>
 * <p> Revision 1.40  2007/03/21 19:46:38  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.39  2007/03/15 21:48:22  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.38  2007/02/09 00:51:54  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.37  2007/02/05 23:40:45  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.36  2006/11/29 00:20:49  sueh
 * <p> bug# 934 Added endGetLoadAverage().  Does the same thing as
 * <p> stopGetLoadAverage(), but also removes the monitor.  Uses when a manager
 * <p> exits.
 * <p>
 * <p> Revision 1.35  2006/11/18 00:49:50  sueh
 * <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
 * <p>
 * <p> Revision 1.34  2006/11/15 21:24:34  sueh
 * <p> bug# 872 Saving to .etomo with ParameterStore.
 * <p>
 * <p> Revision 1.33  2006/11/08 21:08:01  sueh
 * <p> bug# 936:  Remove the 15 Min. column and add the Users column.
 * <p>
 * <p> Revision 1.32  2006/11/07 22:53:36  sueh
 * <p> bug# 954 Added tooltips
 * <p>
 * <p> Revision 1.31  2006/02/09 23:40:36  sueh
 * <p> bug# 796 In Windows an exception was caused by tool tips being set
 * <p>
 * <p> Revision 1.30  2006/02/08 03:38:11  sueh
 * <p> bug# 796 Use cpu usage instead of load average for windows.
 * <p>
 * <p> Revision 1.29  2006/01/12 17:37:12  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.28  2006/01/11 22:32:32  sueh
 * <p> bug# 675 replaced Attribute.getUnformattedValue with getValue
 * <p>
 * <p> Revision 1.27  2005/12/23 02:18:44  sueh
 * <p> bug# 675 Renamed Section.getFirstSectionLocation to getSectionLocation.
 * <p> Removed getSection(sectionLocation).  Changed nextSection so it gets the
 * <p> current section and increments.
 * <p>
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
