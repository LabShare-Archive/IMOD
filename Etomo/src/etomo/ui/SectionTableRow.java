package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JPanel;

import etomo.type.ConstEtomoNumber;
import etomo.type.ConstSectionTableRowData;
import etomo.type.SectionTableRowData;
import etomo.type.SlicerAngles;
import etomo.util.MRCHeader;

/**
* <p>Description: Manages the fields, buttons, state, and data of one row of
* SectionTablePanel.</p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
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
public class SectionTableRow {
  public static final String rcsid = "$Id$";
  
  //data
  SectionTableRowData data = null;
  
  //state
  private int imodIndex = -1;
  private int imodRotIndex = -1;
  private boolean sectionExpanded = false;
  private int curTab = JoinDialog.SETUP_TAB;
  
  //ui
  SectionTablePanel table = null;
  private HeaderCell rowNumber = null;
  private MultiLineToggleButton highlighterButton = null;
  private FieldCell section = null;
  private FieldCell sampleBottomStart = null;
  private FieldCell sampleBottomEnd = null;
  private FieldCell sampleTopStart = null;
  private FieldCell sampleTopEnd = null;
  private FieldCell slicesInSample = null;
  private HeaderCell currentChunk = null;
  private FieldCell referenceSection = null;
  private FieldCell currentSection = null;
  private FieldCell finalStart = null;
  private FieldCell finalEnd = null;
  private FieldCell rotationAngleX = null;
  private FieldCell rotationAngleY = null;
  private FieldCell rotationAngleZ = null;
  private SectionTableRowActionListener actionListener = new SectionTableRowActionListener(
      this);
  
  /**
   * Create colors, fields, and buttons.  Add the row to the table
   * @param table
   * @param rowNumber
   */
  public SectionTableRow(SectionTablePanel table, int rowNumber, File tomogram,
      boolean sectionExpanded, MRCHeader header, int curTab) {
    this.table = table;
    data = new SectionTableRowData(rowNumber);
    data.setSection(tomogram);
    data.setXMax(header.getNColumns());
    data.setYMax(header.getNRows());
    data.setZMax(header.getNSections());
    this.sectionExpanded = sectionExpanded;
    this.curTab = curTab;
  }

  public SectionTableRow(SectionTablePanel table, SectionTableRowData data,
      boolean sectionExpanded, int curTab) {
    this.table = table;
    this.data = new SectionTableRowData(data);
    this.sectionExpanded = sectionExpanded;
    this.curTab = curTab;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",\ntable=" + table + ",\rowNumber=" + rowNumber.getText()
        + ",\nsection=" + section.getText() + ",\nsampleBottomStart="
        + sampleBottomStart.getText() + ",\nsampleBottomEnd="
        + sampleBottomEnd.getText() + ",\nsampleTopStart="
        + sampleTopStart.getText() + ",\nsampleTopEnd="
        + sampleTopEnd.getText() + ",\nfinalStart=" + finalStart.getText()
        + ",\nfinalEnd=" + finalEnd.getText() + ",\nrotationAngleX="
        + rotationAngleX.getText() + ",\nrotationAngleY="
        + rotationAngleY.getText() + ",\nrotationAngleZ="
        + rotationAngleZ.getText() + ",\nimodIndex=" + imodIndex
        + ",\nsectionExpanded=" + sectionExpanded + ",\ncurTab=" + curTab
        + ",\ncurrentChunk=" + currentChunk.getText() + ",\nslicesInSample="
        + slicesInSample.getText() + ",\nreferenceSection="
        + referenceSection.getText() + ",\ncurrentSection="
        + currentSection.getText() + ",\ncurrentSection="
        + currentSection.getText() + ",\ndata=" + data;
  } 

  void create(int mode) {
    rowNumber = new HeaderCell(data.getRowNumber().toString(true),
        FixedDim.rowNumberWidth);
    highlighterButton = table.createToggleButton("=>", FixedDim.highlighterWidth);
    highlighterButton.addActionListener(actionListener);
    section = new FieldCell();
    section.setEnabled(false);
    setSectionText();
    sampleBottomStart = new FieldCell();
    sampleBottomEnd = new FieldCell();
    sampleTopStart = new FieldCell();
    sampleTopEnd = new FieldCell();
    slicesInSample = new FieldCell();
    slicesInSample.setEnabled(false);
    currentChunk = new HeaderCell();
    referenceSection = new FieldCell();
    referenceSection.setEnabled(false);
    currentSection = new FieldCell();
    currentSection.setEnabled(false);
    finalStart = new FieldCell();
    finalEnd = new FieldCell();
    rotationAngleX = new FieldCell();
    rotationAngleY = new FieldCell();
    rotationAngleZ = new FieldCell();
    configureFields();
    setMode(mode);
    displayData();
  }
  
  void configureFields() {
    int rowNumber = data.getRowNumber().getInteger();
    boolean bottomInUse = rowNumber > 1;
    boolean topInUse = rowNumber < table.getTableSize();
    boolean finalInuse = curTab == JoinDialog.JOIN_TAB;

    
    sampleBottomStart.setInUse(bottomInUse);
    sampleBottomEnd.setInUse(bottomInUse);
    
    sampleTopStart.setInUse(topInUse);
    sampleTopEnd.setInUse(topInUse);
    
    finalStart.setInUse(finalInuse);
    finalEnd.setInUse(finalInuse);
  }
  
  void remove() {
    if (curTab == JoinDialog.SETUP_TAB) {
      removeSetup();
    }
    else if (curTab == JoinDialog.ALIGN_TAB) {
      removeAlign();
    }
    else if (curTab == JoinDialog.JOIN_TAB) {
      removeJoin();
    }
  }
  
  private void removeSetup() {
    rowNumber.remove();
    table.removeCell(highlighterButton);
    section.remove();
    sampleBottomStart.remove();
    sampleBottomEnd.remove();
    sampleTopStart.remove();
    sampleTopEnd.remove();
    finalStart.remove();
    finalEnd.remove();
    rotationAngleX.remove();
    rotationAngleY.remove();
    rotationAngleZ.remove();
  }
  
  private void removeAlign() {
    rowNumber.remove();
    section.remove();
    slicesInSample.remove();
    currentChunk.remove();
    referenceSection.remove();
    currentSection.remove();
  }
  
  private void removeJoin() {
    rowNumber.remove();
    table.removeCell(highlighterButton);
    section.remove();
    finalStart.remove();
    finalEnd.remove();
  }
  
  void setMode(int mode) {
    switch (mode) {
    case JoinDialog.SAMPLE_PRODUCED_MODE:
      sampleBottomStart.setEnabled(false);
      sampleBottomEnd.setEnabled(false);
      sampleTopStart.setEnabled(false);
      sampleTopEnd.setEnabled(false);
      rotationAngleX.setEnabled(false);
      rotationAngleY.setEnabled(false);
      rotationAngleZ.setEnabled(false);
      return;
    case JoinDialog.SETUP_MODE:
    case JoinDialog.SAMPLE_NOT_PRODUCED_MODE:
    case JoinDialog.CHANGING_SAMPLE_MODE:
      sampleBottomStart.setEnabled(true);
      sampleBottomEnd.setEnabled(true);
      sampleTopStart.setEnabled(true);
      sampleTopEnd.setEnabled(true);
      rotationAngleX.setEnabled(true);
      rotationAngleY.setEnabled(true);
      rotationAngleZ.setEnabled(true);
      return;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
  }
  
  void setCurTab(int curTab) {
    this.curTab = curTab;
  }
  
  int displayCurTab(JPanel panel, int prevSlice) {
    remove();
    add(panel);
    configureFields();
    //Set align display only fields
    if (curTab == JoinDialog.ALIGN_TAB) {
      int start;
      int chunkSize = data.getChunkSize(table.getTableSize()).getInteger();
      if (chunkSize > 0) {
        start = prevSlice + 1;
        prevSlice += chunkSize;
        slicesInSample.setText(Integer.toString(start) + " - " + Integer.toString(prevSlice));
      }
      else {
        referenceSection.setText("");
      }
    }
    return prevSlice;
  }
  
  int displayCurTabChunkTable(JPanel panel, int prevSlice,
      int nextSampleBottomNumberSlices) {
    if (curTab == JoinDialog.ALIGN_TAB) {
      if (nextSampleBottomNumberSlices == -1) {
        currentChunk.setText("");
        referenceSection.setText("");
        currentSection.setText("");
      }
      else {
        ConstEtomoNumber rowNumber = data.getRowNumber();
        currentChunk.setText(Integer.toString(rowNumber.getInteger() + 1));
        int start;
        int sampleTopNumberSlices = data.getSampleTopNumberSlices();
        if (sampleTopNumberSlices > 0) {
          start = prevSlice + 1;
          prevSlice += sampleTopNumberSlices;
          referenceSection.setText(Integer.toString(start) + " - "
              + Integer.toString(prevSlice));
        }
        else {
          referenceSection.setText("");
        }

        if (nextSampleBottomNumberSlices > 0) {
          start = prevSlice + 1;
          prevSlice += nextSampleBottomNumberSlices;
          currentSection.setText(Integer.toString(start) + " - "
              + Integer.toString(prevSlice));
        }
        else {
          currentSection.setText("");
        }
      }
    }
    return prevSlice;
  }


  
  void add(JPanel panel) {
    if (curTab == JoinDialog.SETUP_TAB) {
      addSetup(panel);
    }
    else if (curTab == JoinDialog.ALIGN_TAB) {
      addAlign(panel);
    }
    else if (curTab == JoinDialog.JOIN_TAB) {
      addJoin(panel);
    }
  }
  
  private void addSetup(JPanel panel) {
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weighty = 1.0;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    table.addCell(highlighterButton);
    constraints.gridwidth = 2;
    section.add(panel, layout, constraints);
    constraints.gridwidth = 1;
    sampleBottomStart.add(panel, layout, constraints);
    sampleBottomEnd.add(panel, layout, constraints);
    sampleTopStart.add(panel, layout, constraints);
    sampleTopEnd.add(panel, layout, constraints);
    finalStart.add(panel, layout, constraints);
    finalEnd.add(panel, layout, constraints);
    rotationAngleX.add(panel, layout, constraints);
    rotationAngleY.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    rotationAngleZ.add(panel, layout, constraints);
  }
  
  private void addAlign(JPanel panel) {
    GridBagLayout layout = table.getTableLayout();
    GridBagConstraints constraints = table.getTableConstraints();
    constraints.weighty = 1.0;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    section.add(panel, layout, constraints);
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
    constraints.weighty = 1.0;
    constraints.gridwidth = 1;
    rowNumber.add(panel, layout, constraints);
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    table.addCell(highlighterButton);
    constraints.gridwidth = 2;
    section.add(panel, layout, constraints);
    constraints.gridwidth = 1;
    finalStart.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    finalEnd.add(panel, layout, constraints);
  }
  
  /**
   * Copy field from data to the screen.
   * Copy all fields stored in data that can be displayed on the screen
   *
   */
  private void displayData() {
    rowNumber.setText(data.getRowNumber().toString(true));
    setSectionText();
    sampleBottomStart.setText(data.getSampleBottomStart().toString(true));
    sampleBottomEnd.setText(data.getSampleBottomEnd().toString(true));
    sampleTopStart.setText(data.getSampleTopStart().toString(true));
    sampleTopEnd.setText(data.getSampleTopEnd().toString(true));
    finalStart.setText(data.getFinalStartString());
    finalEnd.setText(data.getFinalEndString());
    rotationAngleX.setText(data.getRotationAngleX().toString());
    rotationAngleY.setText(data.getRotationAngleY().toString());
    rotationAngleZ.setText(data.getRotationAngleZ().toString());
  }
  
  /**
   * Copy data from screen to data.
   * Copies all fields that can be modified on the screen and are stored in data
   * 
   * @return
   */
  private boolean retrieveData() {
    if (!data.setSampleBottomStart(sampleBottomStart.getText()).isValid()
        || !data.setSampleBottomEnd(sampleBottomEnd.getText()).isValid()
        || !data.setSampleTopStart(sampleTopStart.getText()).isValid()
        || !data.setSampleTopEnd(sampleTopEnd.getText()).isValid()
        || !data.setFinalStart(finalStart.getText())
        || !data.setFinalEnd(finalEnd.getText())
        || !data.setRotationAngleX(rotationAngleX.getText())
        || !data.setRotationAngleY(rotationAngleY.getText())
        || !data.setRotationAngleZ(rotationAngleZ.getText())) {
      return false;
    }
    return true;
  }

  /**
   * Toggle section between absolute path when expand is true, and name when
   * expand is false.
   * @param expand
   */
  void expandSection(boolean expand) {
    sectionExpanded = expand;
    if (data.getSection() == null) {
      return;
    }
    setSectionText();
  }
  
  private void setSectionText() {
    if (sectionExpanded) {
      section.setText(data.getSection().getAbsolutePath());
    }
    else {
      section.setText(data.getSection().getName());
    }
  }
  
  void setRowNumber(int rowNumber, boolean maxRow) {
    data.setRowNumber(rowNumber);
    this.rowNumber.setText("<html><b>" + Integer.toString(rowNumber) + "</b>");
    configureFields();
  }
  
  void setImodIndex(int imodIndex) {
    this.imodIndex = imodIndex;
  }
  
  void setImodRotIndex(int imodRotIndex) {
    this.imodRotIndex = imodRotIndex;
  }
  
  void setRotationAngles(SlicerAngles slicerAngles) {
    rotationAngleX.setText(slicerAngles.getXText());
    rotationAngleY.setText(slicerAngles.getYText());
    rotationAngleZ.setText(slicerAngles.getZText());
  }

  /**
   * Toggle the highlighter button based on the highlighted parameter.
   * Change the foreground and background for all the fields in the row.  Do
   * nothing of the highlighter button matches the highlight parameter.
   * @param highlighted
   */
  void setHighlight(boolean highlight) {
    if (highlight == highlighterButton.isSelected()) {
      return;
    }
    highlighterButton.setSelected(highlight);
    highlight();
  }
  
  /**
   * Change the foreground and background for all the fields in the row based
   * on whether the highlighter button is selected.
   *
   */
  private void highlight() {
    boolean highlight = highlighterButton.isSelected();
    section.setHighlighted(highlight);
    sampleBottomStart.setHighlighted(highlight);
    sampleBottomEnd.setHighlighted(highlight);
    sampleTopStart.setHighlighted(highlight);
    sampleTopEnd.setHighlighted(highlight);
    slicesInSample.setHighlighted(highlight);
    finalStart.setHighlighted(highlight);
    finalEnd.setHighlighted(highlight);
    rotationAngleX.setHighlighted(highlight);
    rotationAngleY.setHighlighted(highlight);
    rotationAngleZ.setHighlighted(highlight);
  }
  
  private void highlighterButtonAction() {
    table.msgHighlighting(data.getRowIndex(), highlighterButton.isSelected());
    highlight();
  }
  
  boolean isHighlighted() {
    return highlighterButton.isSelected();
  }
  
  File getSectionFile() {
    return data.getSection();
  }
  
  String getSectionText() {
    return section.getText();
  }
  
  int getXMax() {
    return data.getXMax().getInteger();
  }
  
  int getYMax() {
    return data.getYMax().getInteger();
  }
  
  int getZMax() {
    return data.getZMax();
  }
  
  int getImodIndex() {
    return imodIndex;
  }
  
  int getImodRotIndex() {
    return imodRotIndex;
  }
  
  int getSampleBottomNumberSlices() {
    return data.getSampleBottomNumberSlices();
  }
  
  ConstSectionTableRowData getData() {
    if (!retrieveData()) {
      return null;
    }
    return data;
  }
  
  String getInvalidReason() {
    return data.getInvalidReason();
  }
    
  public boolean equalsSection(File section) {
    if (data.getSection().getAbsolutePath().equals(section.getAbsolutePath())) {
      return true;
    }
    return false;
  }
  
  public boolean equals(SectionTableRow that) {
    retrieveData();
    return data.equals(that.data);
  }
  
  public boolean equals(ConstSectionTableRowData thatData) {
    retrieveData();
    return data.equals(thatData);
  }

  public boolean equalsSample(ConstSectionTableRowData thatData) {
    retrieveData();
    return data.equalsSample(thatData);
  }
 
  /**
   * Handle button actions
   * @param event
   */
  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(highlighterButton.getActionCommand())) {
      highlighterButtonAction();
    }
  }

  
  /**
   *  Action listener for SectionTableRow
   */
  class SectionTableRowActionListener implements ActionListener {

    SectionTableRow adaptee;

    SectionTableRowActionListener(SectionTableRow adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

}
