package etomo.ui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JPanel;

import etomo.JoinManager;
import etomo.process.ImodManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstSectionTableRowData;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SectionTableRowData;
import etomo.type.SlicerAngles;
import etomo.util.DatasetFiles;

/**
 * <p>Description: Manages the fields, buttons, state, and data of one row of
 * SectionTablePanel.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 19:28:59  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.35  2010/02/26 20:38:28  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.34  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.33  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.32  2009/01/20 20:25:09  sueh
 * <p> bug# 1102 Added setNames.
 * <p>
 * <p> Revision 1.31  2008/09/30 22:42:26  sueh
 * <p> bug# 1113 Change add() to display.  Checking Viewport before
 * <p> displaying.
 * <p>
 * <p> Revision 1.30  2007/04/02 21:52:59  sueh
 * <p> bug# 964 Added FieldCell.editable to make instances of FieldCell that can't be
 * <p> edited.  This allows FieldCell.setEditable and setEnabled to be called without
 * <p> checking whether a field should be editable.
 * <p>
 * <p> Revision 1.29  2007/03/27 19:32:07  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.28  2007/03/27 00:06:38  sueh
 * <p> bug# 964 Made HighlighterButton more thread-safe.  Removed unnecessary
 * <p> functions.
 * <p>
 * <p> Revision 1.27  2007/03/01 01:43:02  sueh
 * <p> bug# 964 Implementing Highlighable.
 * <p>
 * <p> Revision 1.26  2007/02/09 00:52:45  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.25  2007/02/05 23:43:42  sueh
 * <p> bug# 962 Added rejoin display.
 * <p>
 * <p> Revision 1.24  2006/10/17 20:21:16  sueh
 * <p> bug# 919  setInverted():  Removing setWarning call from referenceSection and
 * <p> currentSection.  Adding setWarning call to rowNumber.
 * <p>
 * <p> Revision 1.23  2006/10/16 22:53:01  sueh
 * <p> bug# 919  Added setInverted().
 * <p>
 * <p> Revision 1.22  2006/10/13 22:30:37  sueh
 * <p> bug# 919 Added inverted boolean.
 * <p>
 * <p> Revision 1.21  2006/07/21 19:14:14  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 1.20  2006/07/10 21:28:56  sueh
 * <p> Removed order cut added Z order.
 * <p>
 * <p> Revision 1.19  2006/06/29 22:02:28  sueh
 * <p> bug# 880 Removed orderCut from data because it doesn't need to be stored.
 * <p>
 * <p> Revision 1.18  2006/06/29 20:10:23  sueh
 * <p> bug# 880 Adding orderCut.  Displaying orderCut instead of rowNumber.
 * <p> RowNumber is still used as the key in the .ejf file.
 * <p>
 * <p> Revision 1.17  2006/04/06 20:32:45  sueh
 * <p> bug# 808 In SlicerAngles, turned x, y, and z into EtomoNumbers.
 * <p>
 * <p> Revision 1.16  2006/03/21 19:42:53  sueh
 * <p> bug# 807 Changed displayCurTab() so that the chunk display on the align
 * <p> tab matches the row number.
 * <p>
 * <p> Revision 1.15  2006/01/27 18:43:00  sueh
 * <p> bug# 801 Added validation for makejoin and finishjoin
 * <p>
 * <p> Revision 1.14  2005/12/16 01:47:03  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.13  2005/12/14 01:33:36  sueh
 * <p> bug# 783 The only instance of curTab is now in JoinDialog.  Removed
 * <p> removeSetup, etc.  Remove() removes all fields.
 * <p>
 * <p> Revision 1.12  2005/11/30 21:19:01  sueh
 * <p> bug# 757 Removed getJoinXMax, YMax, and ZMax().  Added getXMax,
 * <p> YMax, and ZMax().  Get join max when the current tab is join, otherwise
 * <p> get setup max.
 * <p>
 * <p> Revision 1.11  2005/11/29 22:53:23  sueh
 * <p> bug# 757 Split final start and end into setup and join.  Split section into
 * <p> setup and join.  Display setup fields in setup and align tabs.  Display join
 * <p> fields in join tab.  Changed configure to setInUse().
 * <p>
 * <p> Revision 1.10  2005/11/14 22:19:39  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 1.9  2005/08/10 20:46:49  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 1.8  2005/07/29 19:47:55  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.7  2005/07/01 21:22:48  sueh
 * <p> bug# 619 Changed FieldCell.getText() and setText() to getValue() and
 * <p> setValue().
 * <p>
 * <p> Revision 1.6  2005/04/25 21:38:33  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.5  2005/01/26 00:06:02  sueh
 * <p> Removing ConstEtomoNumber.displayDefault.  To get the default to
 * <p> display, set displayValue and default the same.
 * <p>
 * <p> Revision 1.4  2004/11/24 01:05:05  sueh
 * <p> bug# 520 removed invalidReason.  Simplified retrieveData().
 * <p>
 * <p> Revision 1.3  2004/11/23 22:37:21  sueh
 * <p> bug# 520 retrieveData(): Display error message if a numeric field is
 * <p> incorrect.   Retrieve all correct fields.  Since retrieveData() is called by
 * <p> different functions, pass flag deciding whether to display an error
 * <p> message.
 * <p>
 * <p> Revision 1.2  2004/11/20 00:03:43  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.20  2004/11/19 00:28:13  sueh
 * <p> bug# 520 Added equals function to check whether the screen fields have
 * <p> changed since meta data was updated.  Added equalsSample to check
 * <p> whether the fields used to create the sample have changed.
 * <p>
 * <p> Revision 1.1.2.19  2004/11/16 02:29:23  sueh
 * <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
 * <p> EtomoFloat, and EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.18  2004/11/15 22:26:27  sueh
 * <p> bug# 520 Added setMode().  Moved enabling and disabling to setMode().
 * <p>
 * <p> Revision 1.1.2.17  2004/11/09 16:20:41  sueh
 * <p> bug# 520 Correcting weight distribution for join tab.  Top header is 2, other
 * <p> lines are 1.
 * <p>
 * <p> Revision 1.1.2.16  2004/11/08 22:30:12  sueh
 * <p> bug# 520 Tried setting constraints.weighty to zero to prevent the table
 * <p> from expanding.
 * <p>
 * <p> Revision 1.1.2.15  2004/10/30 02:38:38  sueh
 * <p> bug# 520 Converted rotation angles to EtomoSimpleType.
 * <p>
 * <p> Revision 1.1.2.14  2004/10/29 22:18:33  sueh
 * <p> bug# 520 Added imodRotIndex to manage the 3dmod associated with the
 * <p> .rot file created from section when rotation angles are specified.
 * <p>
 * <p> Revision 1.1.2.13  2004/10/25 23:16:49  sueh
 * <p> bug# 520 Changed table in Align tab:  Removed Sample Slices.  Added
 * <p> Slices in Sample.  Added Chunk table.  Also add xMax and yMax.
 * <p>
 * <p> Revision 1.1.2.12  2004/10/22 21:12:58  sueh
 * <p> bug# 520 Changed SectionTableRow.sampleSampleTop() to
 * <p> setSampleTopNumberSlices().  Removed getSampleBottom().  Fixed
 * <p> displayCurTab().
 * <p>
 * <p> Revision 1.1.2.11  2004/10/22 16:42:41  sueh
 * <p> bug# 520 Fixed displayCurTab: getting sample bottom size from current
 * <p> row.  Changed Chunk to a header.
 * <p>
 * <p> Revision 1.1.2.10  2004/10/22 03:28:33  sueh
 * <p> bug# 520 Added Chunk, Reference section, and Current section to Align
 * <p> tab.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/15 00:52:26  sueh
 * <p> bug# 520 Added toString().
 * <p>
 * <p> Revision 1.1.2.8  2004/10/13 23:15:36  sueh
 * <p> bug# 520 Allowed the ui components of the row to be removed and re-
 * <p> added.  This way the table can look different on different tabs.  Set the
 * <p> state of fields based on the tab.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/08 16:40:22  sueh
 * <p> bug# Using setRowNumber() to change the status of sample slice
 * <p> numbers.  Fixed retrieveData().  Changed getData() to return false when
 * <p> retrieveData() fails.  Added getInvalidReason().
 * <p>
 * <p> Revision 1.1.2.6  2004/10/06 02:31:17  sueh
 * <p> bug# 520 Added Z max
 * <p>
 * <p> Revision 1.1.2.5  2004/10/01 20:07:11  sueh
 * <p> bug# 520 Converted text fields to FieldCells.  Removed color control
 * <p> (done in FieldCell).
 * <p>
 * <p> Revision 1.1.2.4  2004/09/29 19:45:01  sueh
 * <p> bug# 520 View part of the section table row.  Contains section table row
 * <p> screen fields.  Added SectionTableRowData member variable so hold,
 * <p> store, and compare data from the screen.  Added displayData() to display
 * <p> data on the screen.  Added retrieveData() to retrieve data from the
 * <p> screen.  Added an equals function.  Disabled the sectionFile field.
 * <p>
 * <p> Revision 1.1.2.3  2004/09/22 22:17:30  sueh
 * <p> bug# 520 Added set rotation angle functions.  When highlighting, tell the
 * <p> panel when the highlight is being turned off as well as when its being
 * <p> turned on (for button enable/disable).
 * <p>
 * <p> Revision 1.1.2.2  2004/09/21 18:12:04  sueh
 * <p> bug# 520 Added remove(), to remove the row from the table.  Added
 * <p> imodIndex - the vector index of the 3dmod in ImodManager.  Added
 * <p> create(), to create the row in the table for the first time.  Added add(), to
 * <p> added the rows back into the table.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/17 21:48:41  sueh
 * <p> bug# 520 Handles row display, state, and data.  Can highlight all of its
 * <p> fields.  Can expand the section field
 * <p> </p>
 */
public final class SectionTableRow implements Highlightable {
  public static final String rcsid = "$Id$";

  static final String INVERTED_WARNING = "The handedness of structures will change in inverted sections.";

  private final FieldCell setupSection = FieldCell.getIneditableInstance();
  private final FieldCell joinSection = FieldCell.getIneditableInstance();
  private final FieldCell sampleBottomStart = FieldCell.getEditableInstance();
  private final FieldCell sampleBottomEnd = FieldCell.getEditableInstance();
  private final FieldCell sampleTopStart = FieldCell.getEditableInstance();
  private final FieldCell sampleTopEnd = FieldCell.getEditableInstance();
  private final FieldCell slicesInSample = FieldCell.getIneditableInstance();
  private final HeaderCell currentChunk = new HeaderCell();
  private final FieldCell referenceSection = FieldCell.getIneditableInstance();
  private final FieldCell currentSection = FieldCell.getIneditableInstance();
  private final FieldCell setupFinalStart = FieldCell.getEditableInstance();
  private final FieldCell setupFinalEnd = FieldCell.getEditableInstance();
  private final FieldCell joinFinalStart = FieldCell.getEditableInstance();
  private final FieldCell joinFinalEnd = FieldCell.getEditableInstance();
  private final FieldCell rotationAngleX = FieldCell.getEditableInstance();
  private final FieldCell rotationAngleY = FieldCell.getEditableInstance();
  private final FieldCell rotationAngleZ = FieldCell.getEditableInstance();

  private final JoinManager manager;
  private final SectionTablePanel table;
  private final HighlighterButton highlighterButton;

  private SectionTableRowData data;
  private final HeaderCell rowNumber = new HeaderCell(
      (int) (30 * UIParameters.INSTANCE.getFontSizeAdjustment()));
  private int imodIndex = -1;
  private int imodRotIndex = -1;
  private boolean sectionExpanded = false;
  private boolean valid = true;
  private boolean inverted = false;

  /**
   * Create colors, fields, and buttons.  Add the row to the table
   * @param table
   * @param rowNumber
   */
  public SectionTableRow(JoinManager manager, SectionTablePanel table, int rowNumber,
      File tomogram, boolean sectionExpanded) {
    this(manager, table, sectionExpanded);
    data = new SectionTableRowData(manager, rowNumber);
    data.setSetupSection(tomogram);
    displayData(rowNumber);
    setToolTipText();
  }

  public SectionTableRow(JoinManager manager, SectionTablePanel table,
      SectionTableRowData data, boolean sectionExpanded) {
    this(manager, table, sectionExpanded);
    this.data = new SectionTableRowData(manager, data);
    this.sectionExpanded = sectionExpanded;
    displayData(data.getRowNumber().getInt());
    setToolTipText();
  }

  private SectionTableRow(JoinManager manager, SectionTablePanel table,
      boolean sectionExpanded) {
    this.manager = manager;
    this.table = table;
    this.sectionExpanded = sectionExpanded;
    highlighterButton = HighlighterButton.getInstance(this, table);
  }

  public String toString() {
    return "[" + setupSection + "]";
  }

  void setNames() {
    sampleBottomStart.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getSampleHeaderCell());
    sampleBottomEnd.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getSampleHeaderCell());
    sampleTopStart.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getSampleHeaderCell());
    sampleTopEnd.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getSampleHeaderCell());
    rotationAngleX.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getRotationHeaderCell());
    rotationAngleY.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getRotationHeaderCell());
    rotationAngleZ.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getRotationHeaderCell());
    joinFinalStart.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getJoinFinalHeaderCell());
    joinFinalEnd.setHeaders(SectionTablePanel.LABEL, rowNumber,
        table.getJoinFinalHeaderCell());
  }

  void setInUse() {
    if (table.isSetupTab()) {
      int rowNumber = data.getRowNumber().getInt();
      boolean bottomInUse = rowNumber > 1;
      boolean topInUse = rowNumber < table.size();
      boolean finalInuse = table.isJoinTab();
      sampleBottomStart.setInUse(bottomInUse);
      sampleBottomEnd.setInUse(bottomInUse);
      sampleTopStart.setInUse(topInUse);
      sampleTopEnd.setInUse(topInUse);
      setupFinalStart.setInUse(finalInuse);
      setupFinalEnd.setInUse(finalInuse);
    }
    else if (table.isJoinTab()) {
      joinFinalStart.setInUse(true);
      joinFinalEnd.setInUse(true);
    }
  }

  final boolean isRotated() {
    return DatasetFiles.isRotatedTomogram(data.getJoinSection());
  }

  void remove() {
    rowNumber.remove();
    highlighterButton.remove();
    setupSection.remove();
    sampleBottomStart.remove();
    sampleBottomEnd.remove();
    sampleTopStart.remove();
    sampleTopEnd.remove();
    setupFinalStart.remove();
    setupFinalEnd.remove();
    joinFinalStart.remove();
    joinFinalEnd.remove();
    rotationAngleX.remove();
    rotationAngleY.remove();
    rotationAngleZ.remove();
    // align
    slicesInSample.remove();
    currentChunk.remove();
    referenceSection.remove();
    currentSection.remove();
    // join
    joinSection.remove();
    joinFinalStart.remove();
    joinFinalEnd.remove();
  }

  final void removeImod() {
    manager.imodRemove(ImodManager.TOMOGRAM_KEY, imodIndex);
    manager.imodRemove(ImodManager.ROT_TOMOGRAM_KEY, imodRotIndex);
  }

  void setJoinFinalStartHighlight(boolean highlight) {
    setCellHighlight(highlight, joinFinalStart);
  }

  void setJoinFinalEndHighlight(boolean highlight) {
    setCellHighlight(highlight, joinFinalEnd);
  }

  private void setCellHighlight(boolean highlight, InputCell cell) {
    // avoid turning off highlighting in a highlighted row
    if (!highlight && highlighterButton.isHighlighted()) {
      return;
    }
    cell.setHighlight(highlight);
  }

  void setMode(int mode) {
    switch (mode) {
    case JoinDialog.SAMPLE_PRODUCED_MODE:
      sampleBottomStart.setEditable(false);
      sampleBottomEnd.setEditable(false);
      sampleTopStart.setEditable(false);
      sampleTopEnd.setEditable(false);
      rotationAngleX.setEditable(false);
      rotationAngleY.setEditable(false);
      rotationAngleZ.setEditable(false);
      return;
    case JoinDialog.SETUP_MODE:
    case JoinDialog.SAMPLE_NOT_PRODUCED_MODE:
    case JoinDialog.CHANGING_SAMPLE_MODE:
      sampleBottomStart.setEditable(true);
      sampleBottomEnd.setEditable(true);
      sampleTopStart.setEditable(true);
      sampleTopEnd.setEditable(true);
      rotationAngleX.setEditable(true);
      rotationAngleY.setEditable(true);
      rotationAngleZ.setEditable(true);
      return;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
  }

  void setInverted(ConstEtomoNumber inverted) {
    if (inverted == null) {
      return;
    }
    this.inverted = inverted.is();
    String tooltip = null;
    if (this.inverted) {
      tooltip = "This section is inverted.  " + INVERTED_WARNING;
    }
    rowNumber.setWarning(this.inverted, tooltip);
    setupSection.setWarning(this.inverted, tooltip);
    joinSection.setWarning(this.inverted, tooltip);
    sampleBottomStart.setWarning(this.inverted, tooltip);
    sampleBottomEnd.setWarning(this.inverted, tooltip);
    sampleTopStart.setWarning(this.inverted, tooltip);
    sampleTopEnd.setWarning(this.inverted, tooltip);
    slicesInSample.setWarning(this.inverted, tooltip);
    setupFinalStart.setWarning(this.inverted, tooltip);
    setupFinalEnd.setWarning(this.inverted, tooltip);
    joinFinalStart.setWarning(this.inverted, tooltip);
    joinFinalEnd.setWarning(this.inverted, tooltip);
    rotationAngleX.setWarning(this.inverted, tooltip);
    rotationAngleY.setWarning(this.inverted, tooltip);
    rotationAngleZ.setWarning(this.inverted, tooltip);
  }

  private int totalInRange(ConstEtomoNumber start, ConstEtomoNumber end) {
    if (start.isNull() || end.isNull()) {
      return 0;
    }
    return end.getInt() - start.getInt() + 1;
  }

  private int getPrevSampleEnd(SectionTableRow prevRow) {
    // first row
    if (prevRow == null) {
      return 0;
    }
    return prevRow.slicesInSample.getEndValue();
  }

  private int getBottomSampleSlices(SectionTableRow prevRow) {
    // first row
    if (prevRow == null) {
      return 0;
    }
    return totalInRange(data.getSampleBottomStart(), data.getSampleBottomEnd());
  }

  private int getTopSampleSlices(int totalRows, ConstEtomoNumber rowNum) {
    // last row
    if (rowNum.equals(totalRows)) {
      return 0;
    }
    return totalInRange(data.getSampleTopStart(), data.getSampleTopEnd());
  }

  private int getPrevTopSampleSlices(SectionTableRow prevRow) {
    // first row
    if (prevRow == null) {
      return 0;
    }
    return totalInRange(prevRow.data.getSampleTopStart(), prevRow.data.getSampleTopEnd());
  }

  void setupCurTab(SectionTableRow prevRow, int totalRows) {
    // Set align display only fields
    if (table.isAlignTab()) {
      ConstEtomoNumber rowNum = data.getRowNumber();
      int prevSampleEnd = getPrevSampleEnd(prevRow);
      int bottomSampleSlices = getBottomSampleSlices(prevRow);
      int topSampleSlices = getTopSampleSlices(totalRows, rowNum);
      int prevTopSampleSlices = getPrevTopSampleSlices(prevRow);

      slicesInSample.setRangeValue(prevSampleEnd + 1, prevSampleEnd + bottomSampleSlices
          + topSampleSlices);
      if (prevRow == null) {
        currentChunk.setText("");
        currentSection.setValue();
        referenceSection.setValue();
      }
      else {
        currentChunk.setText(rowNum.toString());
        currentSection.setRangeValue(prevSampleEnd + 1, prevSampleEnd
            + bottomSampleSlices);
        referenceSection.setRangeValue(prevSampleEnd - prevTopSampleSlices + 1,
            prevSampleEnd);
      }
    }
  }

  /**
   * Remove row from display.  Display the row if it is inside the viewer.
   * @param index
   * @param viewer
   * @param panel
   */
  void display(int index, JPanel panel, Viewport viewport) {
    if (!viewport.inViewport(index)) {
      return;
    }
    if (table.isSetupTab()) {
      addSetup(panel);
    }
    else if (table.isAlignTab()) {
      addAlign(panel);
    }
    else if (table.isJoinTab()) {
      addJoin(panel);
    }
    else if (table.isRejoinTab()) {
      addRejoin(panel);
    }
  }

  private void addSetup(JPanel panel) {
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    highlighterButton.add(panel, layout, constraints);
    constraints.weightx = 0.2;
    constraints.gridwidth = 2;
    setupSection.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    sampleBottomStart.add(panel, layout, constraints);
    sampleBottomEnd.add(panel, layout, constraints);
    sampleTopStart.add(panel, layout, constraints);
    sampleTopEnd.add(panel, layout, constraints);
    setupFinalStart.add(panel, layout, constraints);
    setupFinalEnd.add(panel, layout, constraints);
    rotationAngleX.add(panel, layout, constraints);
    rotationAngleY.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    rotationAngleZ.add(panel, layout, constraints);
  }

  private void addAlign(JPanel panel) {
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    constraints.weightx = 0.2;
    constraints.gridwidth = 2;
    setupSection.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    slicesInSample.add(panel, layout, constraints);
    currentChunk.add(panel, layout, constraints);
    referenceSection.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    currentSection.add(panel, layout, constraints);
  }

  private void addJoin(JPanel panel) {
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    highlighterButton.add(panel, layout, constraints);
    constraints.weightx = 0.2;
    constraints.gridwidth = 2;
    joinSection.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    joinFinalStart.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    joinFinalEnd.add(panel, layout, constraints);
    joinFinalStart.setEditable(true);
    joinFinalEnd.setEditable(true);
  }

  private void addRejoin(JPanel panel) {
    addJoin(panel);
    joinFinalStart.setEditable(false);
    joinFinalEnd.setEditable(false);
  }

  private void displayData(int rowNumber) {
    this.rowNumber.setText(String.valueOf(rowNumber));
    displayData();
  }

  /**
   * Copy field from data to the screen.
   * Copy all fields stored in data that can be displayed on the screen
   *
   */
  private void displayData() {
    setSectionText();
    sampleBottomStart.setValue(data.getSampleBottomStart().toString());
    sampleBottomEnd.setValue(data.getSampleBottomEnd().toString());
    sampleTopStart.setValue(data.getSampleTopStart().toString());
    sampleTopEnd.setValue(data.getSampleTopEnd().toString());
    setupFinalStart.setValue(data.getSetupFinalStart().toString());
    setupFinalEnd.setValue(data.getSetupFinalEnd().toString());
    joinFinalStart.setValue(data.getJoinFinalStart().toString());
    joinFinalEnd.setValue(data.getJoinFinalEnd().toString());
    rotationAngleX.setValue(data.getRotationAngleX().toString());
    rotationAngleY.setValue(data.getRotationAngleY().toString());
    rotationAngleZ.setValue(data.getRotationAngleZ().toString());
    setInverted(data.getInverted());
  }

  /**
   * Copy data from screen to data.
   * Copies all fields that can be modified on the screen and are stored in data
   * Checks for errors.  Prints a error message based on the first error found
   * and returns false.
   * Tries to retrieve all values, regardless of errors.
   * @return
   */
  private boolean retrieveData(boolean displayErrorMessage) {
    data.setInverted(inverted);
    valid = true;
    String errorInfo = "\nInvalid number in section " + rowNumber.getText();
    String errorTitle = "Invalid Number";
    String errorMessage = data.setSampleBottomStart(sampleBottomStart.getValue())
        .validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setSampleBottomEnd(sampleBottomEnd.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setSampleTopStart(sampleTopStart.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setSampleTopEnd(sampleTopEnd.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setSetupFinalStart(setupFinalStart.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setSetupFinalEnd(setupFinalEnd.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setJoinFinalStart(joinFinalStart.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setJoinFinalEnd(joinFinalEnd.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setRotationAngleX(rotationAngleX.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setRotationAngleY(rotationAngleY.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    errorMessage = data.setRotationAngleZ(rotationAngleZ.getValue()).validate(null);
    if (errorMessage != null && valid) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage + errorInfo, errorTitle,
          AxisID.ONLY);
      valid = false;
    }
    return valid;
  }

  boolean validateMakejoincom(String maxRow) {
    retrieveData(false);
    String rowNumberText = rowNumber.getText();
    if (!validate(data.getSampleBottomStart(), data.getSampleBottomEnd(), true,
        rowNumberText.equals("1"))) {
      return false;
    }
    return validate(data.getSampleTopStart(), data.getSampleTopEnd(), true,
        rowNumberText.equals(maxRow));
  }

  boolean validateFinishjoin() {
    retrieveData(false);
    return validate(data.getJoinFinalStart(), data.getJoinFinalEnd(), false, true);

  }

  private boolean validate(ConstEtomoNumber start, ConstEtomoNumber end,
      boolean validateValues, final boolean optional) {
    if (start.isNull() && !end.isNull()) {
      UIHarness.INSTANCE.openMessageDialog(manager, start.getDescription()
          + " cannot be empty when " + end.getDescription()
          + " has been entered.  Invalid numbers in section " + rowNumber.getText(),
          "Entry Error", AxisID.ONLY);
      valid = false;
      return valid;
    }
    if (!start.isNull() && end.isNull()) {
      UIHarness.INSTANCE.openMessageDialog(manager, end.getDescription()
          + " cannot be empty when " + start.getDescription()
          + " has been entered.  Invalid numbers in section " + rowNumber.getText(),
          "Entry Error", AxisID.ONLY);
      valid = false;
      return valid;
    }
    if (validateValues) {
      if (start.isInt() && start.getInt() > end.getInt()) {
        UIHarness.INSTANCE.openMessageDialog(manager, start.getDescription()
            + " must be less then or equal to " + start.getDescription() + ".",
            "Entry Error", AxisID.ONLY);
        valid = false;
        return valid;
      }
      if (start.getInt() > end.getInt()) {
        UIHarness.INSTANCE.openMessageDialog(manager, start.getDescription()
            + " must be less then or equal to " + start.getDescription()
            + ".  Invalid numbers in section " + rowNumber.getText(), "Entry Error",
            AxisID.ONLY);
        valid = false;
        return valid;
      }
    }
    if (!optional) {
      if (start.isNull()) {
        UIHarness.INSTANCE.openMessageDialog(manager, start.getDescription()
            + " is required in section " + rowNumber.getText(), "Entry Error",
            AxisID.ONLY);
        valid = false;
        return valid;
      }
      if (end.isNull()) {
        UIHarness.INSTANCE.openMessageDialog(manager, end.getDescription()
            + " is required in section " + rowNumber.getText(), "Entry Error",
            AxisID.ONLY);
        valid = false;
        return valid;
      }
    }
    valid = true;
    return valid;
  }

  boolean isValid() {
    return valid;
  }

  /**
   * Toggle the setup section between absolute path when expand is true, and
   * name when expand is false.
   * @param expand
   */
  void expandSection(boolean expand) {
    sectionExpanded = expand;
    setSectionText();
  }

  private void setSectionText() {
    File section = data.getSetupSection();
    if (section != null) {
      if (sectionExpanded) {
        setupSection.setValue(section.getAbsolutePath());
      }
      else {
        setupSection.setValue(section.getName());
      }
    }
    section = data.getJoinSection();
    if (section == null) {
      return;
    }
    if (sectionExpanded) {
      joinSection.setValue(section.getAbsolutePath());
    }
    else {
      joinSection.setValue(section.getName());
    }
  }

  void swapBottomTop() {
    String bottom = sampleBottomStart.getValue();
    String top = sampleTopStart.getValue();
    sampleBottomStart.setValue(top);
    sampleTopStart.setValue(bottom);
    bottom = sampleBottomEnd.getValue();
    top = sampleTopEnd.getValue();
    sampleBottomEnd.setValue(top);
    sampleTopEnd.setValue(bottom);
  }

  void setRowNumber(int rowNumber) {
    data.setRowNumber(rowNumber);
    this.rowNumber.setText(String.valueOf(rowNumber));
  }

  /* void setImodIndex(int imodIndex) { this.imodIndex = imodIndex; } void
   * setImodRotIndex(int imodRotIndex) { this.imodRotIndex = imodRotIndex; } */
  void setRotationAngles(SlicerAngles slicerAngles) {
    rotationAngleX.setValue(slicerAngles.getX().toString());
    rotationAngleY.setValue(slicerAngles.getY().toString());
    rotationAngleZ.setValue(slicerAngles.getZ().toString());
  }

  /**
   * Change the foreground and background for all the fields in the row based
   * on whether the highlighter button is selected.
   *
   */
  public void highlight(boolean highlight) {
    setupSection.setHighlight(highlight);
    joinSection.setHighlight(highlight);
    sampleBottomStart.setHighlight(highlight);
    sampleBottomEnd.setHighlight(highlight);
    sampleTopStart.setHighlight(highlight);
    sampleTopEnd.setHighlight(highlight);
    slicesInSample.setHighlight(highlight);
    setupFinalStart.setHighlight(highlight);
    setupFinalEnd.setHighlight(highlight);
    joinFinalStart.setHighlight(highlight);
    joinFinalEnd.setHighlight(highlight);
    rotationAngleX.setHighlight(highlight);
    rotationAngleY.setHighlight(highlight);
    rotationAngleZ.setHighlight(highlight);
  }

  boolean isHighlighted() {
    return highlighterButton.isHighlighted();
  }

  File getSetupSectionFile() {
    return data.getSetupSection();
  }

  File getJoinSectionFile() {
    return data.getJoinSection();
  }

  String getSetupSectionText() {
    return setupSection.getValue();
  }

  int getXMax() {
    if (table.isJoinTab()) {
      return data.getJoinXMax();
    }
    return data.getSetupXMax();
  }

  int getYMax() {
    if (table.isJoinTab()) {
      return data.getJoinYMax();
    }
    return data.getSetupYMax();
  }

  int getZMax() {
    if (table.isJoinTab()) {
      return data.getJoinZMax();
    }
    return data.getSetupZMax();
  }

  ConstSectionTableRowData getData() {
    retrieveData(true);
    return data;
  }

  String getInvalidReason() {
    return data.getInvalidReason();
  }

  public boolean equalsSetupSection(File section) {
    if (data.getSetupSection().getAbsolutePath().equals(section.getAbsolutePath())) {
      return true;
    }
    return false;
  }

  public boolean equalsJoinSection(File section) {
    if (data.getJoinSection().getAbsolutePath().equals(section.getAbsolutePath())) {
      return true;
    }
    return false;
  }

  public boolean equals(SectionTableRow that) {
    retrieveData(false);
    return data.equals(that.data);
  }

  public boolean equals(ConstSectionTableRowData thatData) {
    retrieveData(false);
    return data.equals(thatData);
  }

  public boolean equalsSample(ConstSectionTableRowData thatData) {
    retrieveData(false);
    return data.equalsSample(thatData);
  }

  final void imodOpenSetupSectionFile(int binning, Run3dmodMenuOptions menuOptions) {
    imodIndex = manager.imodOpen(ImodManager.TOMOGRAM_KEY, imodIndex,
        data.getSetupSection(), binning, menuOptions);
  }

  final void imodOpenJoinSectionFile(int binning, Run3dmodMenuOptions menuOptions) {
    File joinSection = data.getJoinSection();
    if (DatasetFiles.isRotatedTomogram(joinSection)) {
      imodRotIndex = manager.imodOpen(ImodManager.ROT_TOMOGRAM_KEY, imodRotIndex,
          joinSection, binning, menuOptions);
    }
    else {
      imodIndex = manager.imodOpen(ImodManager.TOMOGRAM_KEY, imodIndex, joinSection,
          binning, menuOptions);
    }
  }

  final boolean imodGetAngles() {
    if (imodIndex == -1) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Open in 3dmod and use the Slicer to change the angles.", "Open 3dmod",
          AxisID.ONLY);
      return false;
    }
    SlicerAngles slicerAngles = manager.imodGetSlicerAngles(ImodManager.TOMOGRAM_KEY,
        imodIndex);
    if (slicerAngles == null || !slicerAngles.isComplete()) {
      return false;
    }
    setRotationAngles(slicerAngles);
    return true;
  }

  final void synchronizeSetupToJoin() {
    retrieveData(true);
    data.synchronizeSetupToJoin();
    displayData();
  }

  final void synchronizeJoinToSetup() {
    retrieveData(true);
    data.synchronizeJoinToSetup();
    displayData();
  }

  private void setToolTipText() {
    highlighterButton.setToolTipText("Press to select the section.");
    currentChunk.setToolTipText("The number of the chunk in Midas.");
  }
}