package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.BevelBorder;
import javax.swing.border.LineBorder;

import etomo.JoinManager;
import etomo.process.ImodManager;
import etomo.storage.TomogramFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.JoinMetaData;
import etomo.type.SectionTableRowData;
import etomo.type.SlicerAngles;
import etomo.util.InvalidParameterException;
import etomo.util.JoinInfoFile;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
* <p>Description: A panel containing the section table.  Implements Expandable
* so it can use ExpandButtons. </p>
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
* <p> Revision 1.11  2005/07/20 17:54:05  sueh
* <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
* <p> MRCHeader, because its filling up the error log with exceptions that are
* <p> related to real problems.
* <p>
* <p> Revision 1.10  2005/07/06 23:46:35  sueh
* <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
* <p> their functionality in SpacedPanel.  Simplified the construction of
* <p> SpacedPanel.
* <p>
* <p> Revision 1.9  2005/06/20 16:55:55  sueh
* <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
* <p> constructing in addSection().
* <p>
* <p> Revision 1.8  2005/06/16 21:20:56  sueh
* <p> bug# 614 Fixed 3dmod button name.
* <p>
* <p> Revision 1.7  2005/04/26 17:41:15  sueh
* <p> bug# 615 Change the name of the UIHarness member variable to
* <p> uiHarness.
* <p>
* <p> Revision 1.6  2005/04/25 21:12:09  sueh
* <p> bug# 615 Passing the axis where a command originates to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.  Move the interface for
* <p> popping up message dialogs to UIHarness.  It prevents headless
* <p> exceptions during a test execution.  It also allows logging of dialog
* <p> messages during a test.  It also centralizes the dialog interface and
* <p> allows the dialog functions to be synchronized to prevent dialogs popping
* <p> up in both windows at once.  All Frame functions will use UIHarness as a
* <p> public interface.
* <p>
* <p> Revision 1.5  2005/04/21 20:46:39  sueh
* <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
* <p> that requires it.
* <p>
* <p> Revision 1.4  2004/11/23 22:34:32  sueh
* <p> bug# 520 getMetaData() returning a success boolean.
* <p>
* <p> Revision 1.3  2004/11/23 00:29:55  sueh
* <p> bug# 520 Prevented Add Section from coming on  during a flip.
* <p>
* <p> Revision 1.2  2004/11/20 00:03:36  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.28  2004/11/19 00:25:55  sueh
* <p> bug# 520 Added equals function to check whether the screen fields have
* <p> changed since meta data was updated.  Added equalsSample to check
* <p> whether the fields used to create the sample have changed.  Removed
* <p> open3dmod and expand sections from setMode so they can be on all the
* <p> time.
* <p>
* <p> Revision 1.1.2.27  2004/11/15 22:26:07  sueh
* <p> bug# 520 Added setMode().  Moved enabling and disabling to setMode().
* <p>
* <p> Revision 1.1.2.26  2004/11/12 23:01:12  sueh
* <p> bug# 520 Fixed bug where deleting all rows did not work.
* <p>
* <p> Revision 1.1.2.25  2004/11/11 01:43:00  sueh
* <p> bug# 520 Adding binning to open 3dmod functions.
* <p>
* <p> Revision 1.1.2.24  2004/11/09 16:19:55  sueh
* <p> bug# 520 Removed small functions that where only being called once from
* <p> one member function: createRootPanel, displayCurTabInRows,
* <p> enableRowButtons(), flipSection, setCurTabInRows.  Using function in
* <p> JoinDialog to create open in 3dmod panel, removing createImodPanel
* <p>
* <p> Revision 1.1.2.23  2004/11/08 22:29:27  sueh
* <p> bug# 520 Removed excess spacing around table or excess spacing in
* <p> table by wrapping the whole tabel in a panel with a grid layout with a 2,1
* <p> layout.  The entire table takes up the top element.  The bottom element is
* <p> not filled in.  This keeps the table from expanding when the other item
* <p> on the join panel is taller.  Also tried setting constraints.weighty to zero
* <p> to prevent the table from expanding.
* <p>
* <p> Revision 1.1.2.22  2004/10/30 02:38:24  sueh
* <p> bug# 520 Stopped deleting .rot file when getting section.  Checking
* <p> rootname.info file for .rot file when opening the section in the join tab.
* <p> Will use the .rot name instead of the section, if it is listed in the .info file.
* <p>
* <p> Revision 1.1.2.21  2004/10/29 22:17:23  sueh
* <p> bug# 520 Added .rot file handling.  When a .rot file exists:  Rename the corresponding .rot file when
* <p> a tomogram file is added with addSection().  Remove the imodRotIndex
* <p> when a tomogram file is deleted with deleteSection.  Open the .rot file
* <p> instead of the tomogram file when the current tab is Join.
* <p>
* <p> Revision 1.1.2.20  2004/10/28 22:17:45  sueh
* <p> bug# 520 In addSection(File), used a variable instead of calling
* <p> rows.size() multiple times.
* <p>
* <p> Revision 1.1.2.19  2004/10/25 23:16:11  sueh
* <p> bug# 520 Changed table in Align tab:  Removed Sample Slices.  Added
* <p> Slices in Sample.  Added Chunk table.  Also add xMax and yMax.
* <p>
* <p> Revision 1.1.2.18  2004/10/22 21:12:04  sueh
* <p> bug# 520 Changed SectionTableRow.sampleSampleTop() to
* <p> setSampleTopNumberSlices().
* <p>
* <p> Revision 1.1.2.17  2004/10/22 16:40:05  sueh
* <p> bug# 520 Don't need prevSampleBottom.  This value should come from
* <p> the current sample.
* <p>
* <p> Revision 1.1.2.16  2004/10/22 03:28:05  sueh
* <p> bug# 520 Added Open 3dmod button to Join tab.  Added Chunk,
* <p> reference section, and current section to Align tab.
* <p>
* <p> Revision 1.1.2.15  2004/10/18 18:11:44  sueh
* <p> bug# 520 Removed print statement.
* <p>
* <p> Revision 1.1.2.14  2004/10/15 00:52:03  sueh
* <p> bug# 520 Initialized rows to null so that it matches the output of
* <p> ConstJoinMetaData.getSectionTableData().  Added setMetaData().
* <p>
* <p> Revision 1.1.2.13  2004/10/14 03:32:56  sueh
* <p> bug# 520 Renamed JoinManager.imodOpen() to imodOpenFile.
* <p>
* <p> Revision 1.1.2.12  2004/10/13 23:14:13  sueh
* <p> bug# 520 Allowed the components of the rootPanel and the table panel to
* <p> be removed and re-added.  This way the table can look different on
* <p> different tabs.
* <p>
* <p> Revision 1.1.2.11  2004/10/11 02:17:09  sueh
* <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
* <p> property.  This property would need a different value for each manager.
* <p> This variable can be retrieved from the manager if the object knows its
* <p> manager.  Otherwise it can retrieve it from the current manager using the
* <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
* <p> gets the value from the "user.dir" property.
* <p>
* <p> Revision 1.1.2.10  2004/10/08 16:36:00  sueh
* <p> bug# Using SectionTableRow.setRowNumber() to change the status of
* <p> sample slice numbers.
* <p>
* <p> Revision 1.1.2.9  2004/10/06 02:29:41  sueh
* <p> bug# 520 Fixed flip tomogram functionality.  If the user wants to flip the
* <p> tomogram, call JoinManager flip and exit.  Also disable the Add Section
* <p> button while the tomogram is being flipped.  When the flip process is
* <p> done, the process manager will call the function to add the section and
* <p> enable the Add Section button.
* <p>
* <p> Revision 1.1.2.8  2004/10/01 20:04:22  sueh
* <p> bug# 520 Moved fuctionality to create table headers and fields to
* <p> HeaderCell and FieldCell.  Fixed enable/disable of buttons.  Added
* <p> enable/disable of Add Section button.  Added functionality to Add
* <p> Section: checking for duplicate file paths and flipping tomogram if
* <p> necessary.  To do: add the flip command.
* <p>
* <p> Revision 1.1.2.7  2004/09/29 19:38:29  sueh
* <p> bug# 520 Added retrieveData() to retrieve data from the screen.
* <p>
* <p> Revision 1.1.2.6  2004/09/23 23:39:51  sueh
* <p> bug# 520 Converted to DoubleSpacedPanel and SpacedPanel.  Sized the
* <p> spinner.  Calling JoinDialog.setNumSections() when adding or deleting a
* <p> section.
* <p>
* <p> Revision 1.1.2.5  2004/09/22 22:14:43  sueh
* <p> bug# 520 Enabling and disabling buttons (enableRowButtons() and
* <p> enableTableButtons()).  Modified calls to work with the more genral
* <p> JoinManager.imod... functions.  Added get rotation angle functionality.
* <p>
* <p> Revision 1.1.2.4  2004/09/21 18:08:40  sueh
* <p> bug# 520 Moved buttons that affect the section table from JoinDialog to
* <p> this class.  Added move up, move down, add section, and delete section
* <p> buttons.  Added a binning spinnner and an open 3dmod button.  Added
* <p> functions to add existing fields to the table (such as
* <p> addHeader(JButton, text, width)).  Added removeFromTable and repaint.
* <p> To add, delete or move rows the grid bag layout, all effected rows and all
* <p> those below them must be removed and readded.
* <p>
* <p> Revision 1.1.2.3  2004/09/17 21:47:20  sueh
* <p> bug# 520 Added an array of rows.  Encapsulated each row into
* <p> SectionTableRow.  Encapsulated the expand button into ExpandButton.
* <p> Implemented row highlighting with highlighting(int), which is called by a
* <p> row that is turning on its highlighting.  Implemented expanding sections
* <p> by implementing Expandable with expand(ExpandButton) which tells
* <p> each row to expand its section display.  Factored cell creation code and
* <p> changed some cell creation fuctions to package level so they can be
* <p> called by SectionTableRow.
* <p>
* <p> Revision 1.1.2.2  2004/09/16 18:31:26  sueh
* <p> bug# 520 sized the fields, added a row number, reorganized
* <p> functions
* <p>
* <p> Revision 1.1.2.1  2004/09/15 22:47:26  sueh
* <p> bug# 520 creates the Sections table for JoinDialog.
* <p> </p>
*/
public class SectionTablePanel implements ContextMenu, Expandable {
  public static final String rcsid = "$Id$";

  private static final Dimension buttonDimension = UIParameters
      .getButtonDimension();
  private static final String flipWarning[] = { "Tomograms have to be flipped after generation",
  "in order to be in the right orientation for joining serial sections." };

  private JPanel rootPanel;
  private SpacedPanel pnlBorder;
  private JPanel pnlTable;
  private SpacedPanel pnlButtons;
  private SpacedPanel pnlImod;
  private SpacedPanel pnlButtonsComponent1 = null;
  private SpacedPanel pnlButtonsComponent2 = null;
  private UIHarness uiHarness = UIHarness.INSTANCE;

  private ExpandButton btnExpandSections;
  private MultiLineButton btnMoveSectionUp;
  private MultiLineButton btnMoveSectionDown;
  private MultiLineButton btnAddSection;
  private MultiLineButton btnDeleteSection;
  private LabeledSpinner spinBinning;
  private MultiLineButton btnOpen3dmod;
  private MultiLineButton btnGetAngles;
  
  private HeaderCell hdrOrder;
  private HeaderCell hdrSections;
  private HeaderCell hdrSampleSlices;
  private HeaderCell hdrSlicesInSample1;
  private HeaderCell hdrCurrentChunk1;
  private HeaderCell hdrReferenceSection1;
  private HeaderCell hdrCurrentSection1;
  private HeaderCell hdrFinal;
  private HeaderCell hdrRotationAngles;
  private HeaderCell hdr1Row2;
  private HeaderCell hdr2Row2;
  private HeaderCell hdrBottom;
  private HeaderCell hdrTop;
  private HeaderCell hdrSlicesInSample2;
  private HeaderCell hdrCurrentChunk2;
  private HeaderCell hdrReferenceSection2;
  private HeaderCell hdrCurrentSection2;
  private HeaderCell hdr3Row2;
  private HeaderCell hdr4Row2;
  private HeaderCell hdr1Row3;
  private HeaderCell hdr2Row3;
  private HeaderCell hdrSampleSlicesBottomStart;
  private HeaderCell hdrSampleSlicesBottomEnd;
  private HeaderCell hdrSampleSlicesTopStart;
  private HeaderCell hdrSampleSlicesTopEnd;
  private HeaderCell hdrFinalStart;
  private HeaderCell hdrFinalEnd;
  private HeaderCell hdrRotationAnglesX;
  private HeaderCell hdrRotationAnglesY;
  private HeaderCell hdrRotationAnglesZ;

  private ArrayList rows = null;

  private GridBagLayout layout = new GridBagLayout();
  private GridBagConstraints constraints = new GridBagConstraints();
  private SectionTableActionListener sectionTableActionListener = new SectionTableActionListener(
      this);

  private final JoinManager joinManager;
  private final JoinDialog joinDialog;
  
  private int curTab;
  private int mode = JoinDialog.SETUP_MODE;
  private boolean flipping = false;

  /**
   * Creates the panel and table.
   *
   */
  SectionTablePanel(JoinDialog joinDialog, JoinManager joinManager, int curTab) {
    this.joinDialog = joinDialog;
    this.joinManager = joinManager;
    this.curTab = curTab;
    //create root panel
    rootPanel = new JPanel();
    pnlBorder = new SpacedPanel();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(BorderFactory.createEtchedBorder());
    createTablePanel();
    addTablePanelComponents();
    createButtonsPanel();
    addButtonsPanelComponents();
    addRootPanelComponents();
  }
  
  private void addRootPanelComponents() {
    if (curTab == JoinDialog.JOIN_TAB) {
      rootPanel.setLayout(new GridLayout(2,1));
    }
    else {
      rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    }
    rootPanel.add(pnlBorder.getContainer());
    pnlBorder.add(pnlTable);
    if (curTab != JoinDialog.ALIGN_TAB) {
      addButtonsPanelComponents();
      pnlBorder.add(pnlButtons);
    }
  }

  /**
   * Creates the panel and table.  Adds the header rows.  Adds SectionTableRows
   * to rows to create each row.
   *
   */
  private void createTablePanel() {
    pnlTable = new JPanel();
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    pnlTable.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    //Header
    //First row
    hdrOrder = new HeaderCell("Order");
    hdrSections = new HeaderCell("Sections", FixedDim.sectionsWidth);
    btnExpandSections = new ExpandButton(this);
    hdrSampleSlices = new HeaderCell("Sample Slices");
    hdrSlicesInSample1 = new HeaderCell("Slices in");
    hdrCurrentChunk1 = new HeaderCell("Current");
    hdrReferenceSection1 = new HeaderCell("Reference");
    hdrCurrentSection1 = new HeaderCell("Current");
    hdrFinal = new HeaderCell("Final");
    hdrRotationAngles = new HeaderCell("Rotation Angles");
    //second row
    hdr1Row2 = new HeaderCell();
    hdr2Row2 = new HeaderCell();
    hdrBottom = new HeaderCell("Bottom");
    hdrTop = new HeaderCell("Top");
    hdrSlicesInSample2 = new HeaderCell("Sample");
    hdrCurrentChunk2 = new HeaderCell("Chunk");
    hdrReferenceSection2 = new HeaderCell("Section");
    hdrCurrentSection2 = new HeaderCell("Section");
    hdr3Row2 = new HeaderCell();
    hdr4Row2 = new HeaderCell();
    //Third row
    hdr1Row3 = new HeaderCell();
    hdr2Row3 = new HeaderCell();
    hdrSampleSlicesBottomStart = new HeaderCell("Start", FixedDim.numericWidth);
    hdrSampleSlicesBottomEnd = new HeaderCell("End", FixedDim.numericWidth);
    hdrSampleSlicesTopStart = new HeaderCell("Start", FixedDim.numericWidth);
    hdrSampleSlicesTopEnd = new HeaderCell("End", FixedDim.numericWidth);
    hdrFinalStart = new HeaderCell("Start", FixedDim.numericWidth);
    hdrFinalEnd = new HeaderCell("End", FixedDim.numericWidth);
    hdrRotationAnglesX = new HeaderCell("X", FixedDim.numericWidth);
    hdrRotationAnglesY = new HeaderCell("Y", FixedDim.numericWidth);
    hdrRotationAnglesZ = new HeaderCell("Z", FixedDim.numericWidth);
  }

  private void addTablePanelComponents() {
    //Table constraints
    if (curTab == JoinDialog.SETUP_TAB) {
      addSetupTablePanelComponents();
    }
    else if (curTab == JoinDialog.ALIGN_TAB) {
      addAlignTablePanelComponents();
    }
    else if (curTab == JoinDialog.JOIN_TAB) {
      addJoinTablePanelComponents();
    }
  }
  /**
   * Creates the panel and table.  Adds the header rows.  Adds SectionTableRows
   * to rows to create each row.
   *
   */
  private void addSetupTablePanelComponents() {
    //Header
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 2;
    hdrOrder.add(pnlTable, layout, constraints);
    constraints.weighty = 0.0;
    constraints.gridwidth = 1;
    hdrSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 1.0;
    constraints.gridwidth = 4;
    hdrSampleSlices.add(pnlTable, layout, constraints);
    constraints.gridwidth = 2;
    hdrFinal.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrRotationAngles.add(pnlTable, layout, constraints);
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    hdr1Row2.add(pnlTable, layout, constraints);
    hdr2Row2.add(pnlTable, layout, constraints);
    hdrBottom.add(pnlTable, layout, constraints);
    hdrTop.add(pnlTable, layout, constraints);
    hdr3Row2.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdr4Row2.add(pnlTable, layout, constraints);
    //Third row
    constraints.gridwidth = 2;
    hdr1Row3.add(pnlTable, layout, constraints);
    hdr2Row3.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    hdrSampleSlicesBottomStart.add(pnlTable, layout, constraints);
    hdrSampleSlicesBottomEnd.add(pnlTable, layout, constraints);
    hdrSampleSlicesTopStart.add(pnlTable, layout, constraints);
    hdrSampleSlicesTopEnd.add(pnlTable, layout, constraints);
    hdrFinalStart.add(pnlTable, layout, constraints);
    hdrFinalEnd.add(pnlTable, layout, constraints);
    hdrRotationAnglesX.add(pnlTable, layout, constraints);
    hdrRotationAnglesY.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrRotationAnglesZ.add(pnlTable, layout, constraints);
  }
  
  private void addAlignTablePanelComponents() {
    //Header
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    hdrOrder.add(pnlTable, layout, constraints);
    constraints.weighty = 0.0;
    hdrSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    btnExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 1.0;
    constraints.gridwidth = 1;
    hdrSlicesInSample1.add(pnlTable, layout, constraints);
    hdrCurrentChunk1.add(pnlTable, layout, constraints);
    hdrReferenceSection1.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrCurrentSection1.add(pnlTable, layout, constraints);
    
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    hdr1Row2.add(pnlTable, layout, constraints);
    constraints.gridwidth = 2;
    hdr2Row2.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    hdrSlicesInSample2.add(pnlTable, layout, constraints);
    hdrCurrentChunk2.add(pnlTable, layout, constraints);
    hdrReferenceSection2.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrCurrentSection2.add(pnlTable, layout, constraints);
  }
  
  private void addJoinTablePanelComponents() {
    //Header
    //First row
    constraints.weightx = 1.0;
    constraints.weighty = 2.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 2;
    hdrOrder.add(pnlTable, layout, constraints);
    constraints.weighty = 0.0;
    constraints.gridwidth = 1;
    hdrSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 1.0;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrFinal.add(pnlTable, layout, constraints);
    //Second row
    constraints.weighty = 1.0;
    constraints.gridwidth = 2;
    hdr1Row3.add(pnlTable, layout, constraints);
    constraints.weighty = 0.0;
    hdr2Row3.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    hdrFinalStart.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    hdrFinalEnd.add(pnlTable, layout, constraints);
  }



  private void createButtonsPanel() {
    pnlButtons = new SpacedPanel();
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    //first component
    pnlButtonsComponent1 = new SpacedPanel();
    pnlButtonsComponent1.setBoxLayout(BoxLayout.Y_AXIS);
    btnMoveSectionUp = new MultiLineButton("Move Section Up");
    UIUtilities.setButtonSize(btnMoveSectionUp, buttonDimension, true);
    btnMoveSectionUp.addActionListener(sectionTableActionListener);
    pnlButtonsComponent1.add(btnMoveSectionUp);
    btnAddSection = new MultiLineButton("Add Section");
    UIUtilities.setButtonSize(btnAddSection, buttonDimension, true);
    btnAddSection.addActionListener(sectionTableActionListener);
    pnlButtonsComponent1.add(btnAddSection);
    UIUtilities.setButtonSizeAll(pnlButtonsComponent1.getContainer(), buttonDimension);
    //second component
    pnlButtonsComponent2 = new SpacedPanel();
    pnlButtonsComponent2.setBoxLayout(BoxLayout.Y_AXIS);
    btnMoveSectionDown = new MultiLineButton("Move Section Down");
    UIUtilities.setButtonSize(btnMoveSectionDown, buttonDimension, true);
    btnMoveSectionDown.addActionListener(sectionTableActionListener);
    pnlButtonsComponent2.add(btnMoveSectionDown);
    btnDeleteSection = new MultiLineButton("Delete Section");
    UIUtilities.setButtonSize(btnDeleteSection, buttonDimension, true);
    btnDeleteSection.addActionListener(sectionTableActionListener);
    pnlButtonsComponent2.add(btnDeleteSection);
    //third component
    btnOpen3dmod = new MultiLineButton("Open in 3dmod");
    btnOpen3dmod.addActionListener(sectionTableActionListener);
    SpinnerModel spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning = new LabeledSpinner(JoinDialog.OPEN_BINNED_BY, spinnerModel);
    pnlImod = joinDialog.createOpen3dmodPanel(spinBinning, btnOpen3dmod);
    //createImodPanel();
    //fourth component
    btnGetAngles = new MultiLineButton("Get Angles from Slicer");
    UIUtilities.setButtonSize(btnGetAngles, buttonDimension, true);
    btnGetAngles.addActionListener(sectionTableActionListener);
  }
  
  private void addButtonsPanelComponents() {
    if (curTab == JoinDialog.SETUP_TAB) {
      pnlButtons.add(pnlButtonsComponent1);
      pnlButtons.add(pnlButtonsComponent2);
    }
    if (curTab != JoinDialog.ALIGN_TAB) {
      pnlButtons.add(pnlImod);
    }
    if (curTab == JoinDialog.SETUP_TAB) {
      pnlButtons.add(btnGetAngles);
    }
  }
  
  void setCurTab(int curTab) {
    this.curTab = curTab;
  }
  
  void displayCurTab() {
    rootPanel.removeAll();
    pnlButtons.removeAll();
    pnlBorder.removeAll();
    addRootPanelComponents();
    addButtonsPanelComponents();
    pnlTable.removeAll();
    addTablePanelComponents();
    if (rows == null) {
      return;
    }
    int rowsSize = rows.size();
    int prevSampleSlice = 0;
    int prevChunkTableSlice = 0;
    int nextSampleBottomNumberSlices;
    //Set curTab
    for (int i = 0; i < rowsSize; i++) {
      ((SectionTableRow) rows.get(i)).setCurTab(curTab);
    }
    //redisplay rows and calculate chunks
    for (int i = 0; i < rowsSize; i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      prevSampleSlice = row.displayCurTab(pnlTable, prevSampleSlice);
      if (i < rowsSize - 1) {
        SectionTableRow nextRow = (SectionTableRow) rows.get(i + 1);
        nextSampleBottomNumberSlices = nextRow.getSampleBottomNumberSlices();

      }
      else {
        nextSampleBottomNumberSlices = -1;
      }
      prevChunkTableSlice = row.displayCurTabChunkTable(pnlTable,
          prevChunkTableSlice, nextSampleBottomNumberSlices);
    }
  }
  
  int getTableSize() {
    return rows.size();
  }

  /**
   * Informs this panel that a row is highlighting.  Only one row may be
   * highlighted at once, so it turns off highlighting on all the other rows.
   * @param rowNumber
   */
  void msgHighlighting(int rowIndex, boolean highlightTurnedOn) {
    int highlightedRowIndex;
    if (highlightTurnedOn) {
      highlightedRowIndex = rowIndex;
      for (int i = 0; i < rows.size(); i++) {
        if (i != rowIndex) {
          ((SectionTableRow) rows.get(i)).setHighlight(false);
        }
      }
    }
    else {
      highlightedRowIndex = -1;
    }
    setMode();
  }

  private int getHighlightedRowIndex() {
    if (rows == null) {
      return -1;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (((SectionTableRow) rows.get(i)).isHighlighted()) {
        return i;
      }
    }
    return -1;
  }
  
  /**
   * enable buttons made on the current mode parameter
   *
   */
  void setMode() {
    setMode(mode);
  }
  
  /**
   * Enable buttons based on the mode parameter
   * @param mode
   */
  void setMode(int mode) {
    this.mode = mode;
    //enable buttons that are not effected by highlighting
    switch (mode) {
    case JoinDialog.SAMPLE_PRODUCED_MODE:
      btnAddSection.setEnabled(false);
      btnMoveSectionUp.setEnabled(false);
      btnMoveSectionDown.setEnabled(false);
      btnDeleteSection.setEnabled(false);
      btnGetAngles.setEnabled(false);
      break;
    case JoinDialog.SETUP_MODE:
    case JoinDialog.SAMPLE_NOT_PRODUCED_MODE:
    case JoinDialog.CHANGING_SAMPLE_MODE:
      if (!flipping) {
        btnAddSection.setEnabled(true);
      }
      break;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
    enableRowButtons(getHighlightedRowIndex());
    if (rows == null) {
      return;
    }
    for (int i= 0; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).setMode(mode);
    }
  }

  /**
   * Enable row level buttons based on the current highlight
   * @param highlightedRowIndex
   */
  private void enableRowButtons(int highlightedRowIndex) {
    int rowsSize = 0;
    if (rows != null) {
      rowsSize = rows.size();
    }
    if (rows == null || rowsSize == 0) {
      btnOpen3dmod.setEnabled(false);
      btnExpandSections.setEnabled(false);
      if (mode != JoinDialog.SAMPLE_PRODUCED_MODE) {
        btnMoveSectionUp.setEnabled(false);
        btnMoveSectionDown.setEnabled(false);
        btnDeleteSection.setEnabled(false);
        btnGetAngles.setEnabled(false);
      }
      return;
    }
    btnOpen3dmod.setEnabled(highlightedRowIndex > -1);
    btnExpandSections.setEnabled(true);
    if (mode != JoinDialog.SAMPLE_PRODUCED_MODE) {
      btnMoveSectionUp.setEnabled(highlightedRowIndex > 0);
      btnMoveSectionDown.setEnabled(highlightedRowIndex > -1
          && highlightedRowIndex < rowsSize - 1);
      btnDeleteSection.setEnabled(highlightedRowIndex > -1);
      btnGetAngles.setEnabled(highlightedRowIndex > -1);
    }
  }

  /**
   * Implements the Expandable interface.  Matches the expand button parameter
   * and performs the expand/contract operation.  Expands the section in each
   * row.
   * @param expandButton
   */
  public void expand(ExpandButton expandButton) {
    if (expandButton.equals(btnExpandSections)) {
      boolean expand = btnExpandSections.isExpanded();
      for (int i = 0; i < rows.size(); i++) {
        ((SectionTableRow) rows.get(i)).expandSection(expand);
      }
    }
    else {
      throw new IllegalStateException("Unknown expand button," + expandButton);
    }
  }

  public GridBagLayout getTableLayout() {
    return layout;
  }

  public void enableAddSection() {
    flipping = false;
    setMode();
  }
  
  public boolean equals(ConstJoinMetaData metaData) {
    ArrayList array = metaData.getSectionTableData();
    if (rows == null) {
      if (array == null) {
        return true;
      }
      return false;
    }
    if (array == null) {
      return false;
    }
    if (rows.size() != array.size()) {
      return false;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i)).equals((ConstSectionTableRowData) array.get(i))) {
        return false;
      }
    }
    return true;
  }
  
  public boolean equalsSample(ConstJoinMetaData metaData) {
    ArrayList array = metaData.getSectionTableData();
    if (rows == null) {
      if (array == null) {
        return true;
      }
      return false;
    }
    if (array == null) {
      return false;
    }
    if (rows.size() != array.size()) {
      return false;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i)).equalsSample((ConstSectionTableRowData) array.get(i))) {
        return false;
      }
    }
    return true;
  }


  
  public GridBagConstraints getTableConstraints() {
    return constraints;
  }

  /**
   * Swap the highlighted row with the one above it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveSectionUp() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    if (rowIndex == 0) {
      uiHarness.openMessageDialog("Can't move the row up.  Its at the top.",
          "Wrong Row", AxisID.ONLY);
      return;
    }
    removeRowsFromTable(rowIndex - 1);
    Object rowMoveUp = rows.remove(rowIndex);
    Object rowMoveDown = rows.remove(rowIndex - 1);
    rows.add(rowIndex - 1, rowMoveUp);
    rows.add(rowIndex, rowMoveDown);
    addRowsToTable(rowIndex - 1);
    renumberTable(rowIndex - 1);
    enableRowButtons(rowIndex - 1);
    repaint();
  }

  /**
   * Swap the highlighted row with the one below it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveSectionDown() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    if (rowIndex == rows.size() - 1) {
      uiHarness.openMessageDialog(
          "Can't move the row down.  Its at the bottom.", "Wrong Row", AxisID.ONLY);
      return;
    }
    removeRowsFromTable(rowIndex);
    Object rowMoveUp = rows.remove(rowIndex + 1);
    Object rowMoveDown = rows.remove(rowIndex);
    rows.add(rowIndex, rowMoveUp);
    rows.add(rowIndex + 1, rowMoveDown);
    addRowsToTable(rowIndex);
    renumberTable(rowIndex);
    enableRowButtons(rowIndex + 1);
    repaint();
  }

  private void addSection() {
    StringBuffer invalidBuffer = new StringBuffer();
    if (!Utilities.isValidFile(joinDialog.getWorkingDir(),
        JoinDialog.WORKING_DIRECTORY_TEXT, invalidBuffer, true,
        true, true, true)) {
      uiHarness.openMessageDialog(invalidBuffer.toString(), "Unable to Add Section", AxisID.ONLY);
      return;
    }
    //  Open up the file chooser in the working directory
    JFileChooser chooser = new JFileChooser(new File(joinManager
        .getPropertyUserDir()));
    TomogramFileFilter tomogramFilter = new TomogramFileFilter();
    chooser.setFileFilter(tomogramFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlBorder.getContainer());
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File tomogram = chooser.getSelectedFile();
      if (isDuplicate(tomogram)) {
        return;
      }
      MRCHeader header = MRCHeader.getInstance(
          joinManager.getPropertyUserDir(), tomogram.getAbsolutePath(),
          AxisID.ONLY);
      if (!readHeader(header)) {
        return;
      }
      flipping = true;
      btnAddSection.setEnabled(false);
      if (header.getNRows() < header.getNSections()) {
        //The tomogram may not be flipped
        //Ask use if can flip the tomogram
        String msgFlipped[] = {
            "It looks like you didn't flip the tomogram in Post Processing",
            "bacause the tomogram is thicker in Z then it is long in Y.",
            flipWarning[0], flipWarning[1],
            "Shall I use the clip flipyz command to flip Y and Z?" };
        if (uiHarness.openYesNoDialog(msgFlipped, AxisID.ONLY)) {
          joinManager.flip(tomogram, joinDialog.getWorkingDir());
          return;
        }
      }
      addSection(tomogram);
      joinManager.packMainWindow(AxisID.ONLY);
    }
  }
  
  private boolean readHeader(MRCHeader header) {
    try {
      header.read();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      String msgInvalidParameterException[] = {
          "The header command returned an error (InvalidParameterException).",
          "This file may not contain a tomogram.",
          "Are you sure you want to open this file?" };
      if (!uiHarness.openYesNoDialog(
          msgInvalidParameterException, AxisID.ONLY)) {
        return false;
      }
    }
    catch (IOException e) {
      if (header.getNRows() == -1 || header.getNSections() == -1) {
        String msgIOException[] = {
            "The header command returned an error (IOException).",
            "Unable to tell if the tomogram is flipped.", flipWarning[0],
            flipWarning[1], "Are you sure you want to open this file?" };
        if (!uiHarness.openYesNoDialog(msgIOException, AxisID.ONLY)) {
          return false;
        }
      }
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
      if (header.getNRows() == -1 || header.getNSections() == -1) {
        String msgNumberFormatException[] = {
            "The header command returned an error (NumberFormatException).",
            "Unable to tell if the tomogram is flipped.", flipWarning[0],
            flipWarning[1], "Are you sure you want to open this file?" };
        if (!uiHarness.openYesNoDialog(
            msgNumberFormatException, AxisID.ONLY)) {
          return false;
        }
      }
    }
    return true;
  }

  private boolean isDuplicate(File section) {
    if (rows == null) {
      return false;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (((SectionTableRow) rows.get(i)).equalsSection(section)) {
        String msgDuplicate = "The file, " + section.getAbsolutePath()
            + ", is already in the table.";
        uiHarness.openMessageDialog(msgDuplicate,
            "Add Section Failed", AxisID.ONLY);
        return true;
      }
    }
    return false;
  }

  void addSection(File tomogram) {
    flipping = false;
    setMode();
    if (!tomogram.exists()) {
      uiHarness.openMessageDialog(
          tomogram.getAbsolutePath() + " does not exist.", "File Error", AxisID.ONLY);
      return;
    }
    if (!tomogram.isFile()) {
      uiHarness.openMessageDialog(
          tomogram.getAbsolutePath() + " is not a file.", "File Error", AxisID.ONLY);
      return;
    }
    MRCHeader header = MRCHeader.getInstance(joinManager.getPropertyUserDir(),
        tomogram.getAbsolutePath(), AxisID.ONLY);
    try {
      header.read();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
    }
    if (rows == null) {
      rows = new ArrayList();
    }
    SectionTableRow row = new SectionTableRow(this, rows.size() + 1, tomogram,
        btnExpandSections.isExpanded(), header, curTab);
    row.create(mode);
    row.add(pnlTable);
    rows.add(row);
    int tableSize = rows.size();
    if (tableSize > 1) {
      ((SectionTableRow) rows.get(tableSize - 2)).configureFields();
    }
    joinDialog.setNumSections(tableSize);
    repaint();
  }
  
  /**
   * Delete the highlighted row.  Remove it in the rows ArrayList.
   * Remove it from the table.  Renumber the row numbers in the table.
   */
  private void deleteSection() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    SectionTableRow row = (SectionTableRow) rows.get(rowIndex);
    if (!uiHarness.openYesNoDialog(
        "Really remove " + row.getSectionText() + "?", AxisID.ONLY)) {
      return;
    }
    rows.remove(rowIndex);
    joinManager.imodRemove(ImodManager.TOMOGRAM_KEY, row.getImodIndex());
    joinManager.imodRemove(ImodManager.ROT_TOMOGRAM_KEY, row.getImodRotIndex());
    row.remove();
    renumberTable(rowIndex);
    if (rows.size() > 0) {
      if (rowIndex == 0) {
        ((SectionTableRow) rows.get(0)).configureFields();
      }
      else if (rowIndex == rows.size()) {
        ((SectionTableRow) rows.get(rows.size() - 1)).configureFields();
      }
    }
    joinDialog.setNumSections(rows.size());
    enableRowButtons(-1);
    repaint();
  }
  
  void deleteSections() {
    while (rows.size() > 0) {
      SectionTableRow row = (SectionTableRow) rows.remove(0);
      row.remove();
    }
    repaint();
  }
  
  /**
   * Opens a section in 3dmod
   * May open a .rot file instead of the original section in the join tab.
   * Keeps track of the index of the 3dmod so it can close it and retrieve
   * rotation angles.
   * 
   */
  private void imodSection() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    int binning = ((Integer) this.spinBinning.getValue()).intValue();
    SectionTableRow row = (SectionTableRow) rows.get(rowIndex);
    File sectionFile = row.getSectionFile();
    //if join tab, open .rot file, if it exists and is listed in .info file
    if (curTab == JoinDialog.JOIN_TAB) {
      JoinInfoFile infoFile = new JoinInfoFile(
          joinManager.getPropertyUserDir(), joinDialog.getRootName());
      if (infoFile.read(rows.size())) {
        String infoFileSectionName = infoFile.getFileName(rowIndex);
        if (infoFileSectionName.substring(infoFileSectionName.lastIndexOf("."))
            .equals(".rot")) {
          File rotSectionFile = new File(joinManager.getPropertyUserDir(),
              infoFileSectionName);
          if (rotSectionFile.exists()) {
            //open rotTomogram 3dmod and keep track of it
            row.setImodRotIndex(joinManager.imodOpen(
                ImodManager.ROT_TOMOGRAM_KEY, row.getImodRotIndex(),
                rotSectionFile, binning));
            return;
          }
        }
      }
    }
    //open tomogram 3dmod and keep track of it
    row.setImodIndex(joinManager.imodOpen(ImodManager.TOMOGRAM_KEY, row
        .getImodIndex(), sectionFile, binning));
  }

  private void imodGetAngles() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    SectionTableRow row = (SectionTableRow) rows.get(rowIndex);
    int imodIndex = row.getImodIndex();
    if (imodIndex == -1) {
      uiHarness.openMessageDialog(
          "Open in 3dmod and use the Slicer to change the angles.",
          "Open 3dmod", AxisID.ONLY);
      return;
    }
    SlicerAngles slicerAngles = joinManager.imodGetSlicerAngles(
        ImodManager.TOMOGRAM_KEY, imodIndex);
    if (slicerAngles == null || !slicerAngles.isComplete()) {
      return;
    }
    row.setRotationAngles(slicerAngles);
    repaint();
  }

  /**
   * Renumber the table starting from the row in the ArrayList at startIndex.
   * @param startIndex
   */
  private void renumberTable(int startIndex) {
    int rowsSize = rows.size();
    for (int i = startIndex; i < rowsSize; i++) {
      ((SectionTableRow) rows.get(i)).setRowNumber(i + 1, i + 1 == rowsSize);
    }
  }

  /**
   * Remove the rows from the table starting from the row in the ArrayList at
   * startIndex.
   * @param startIndex
   */
  private void removeRowsFromTable(int startIndex) {
    for (int i = startIndex; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).remove();
    }
  }

  /**
   * Add rows in the ArrayList to the table starting from the row in the 
   * ArrayList at startIndex.
   * @param startIndex
   */
  private void addRowsToTable(int startIndex) {
    for (int i = startIndex; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).add(pnlTable);
    }
  }

  public boolean getMetaData(JoinMetaData metaData) {
    boolean success = true;
    metaData.resetSectionTableData();
    if (rows == null) {
      return success;
    }
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      ConstSectionTableRowData rowData = row.getData();
      if (!row.isValid()) {
        success =  false; //getData() failed
      }
      metaData.setSectionTableData(new SectionTableRowData(rowData));
    }
    return success;
  }
  
  public void setMetaData(ConstJoinMetaData metaData) {
    ArrayList rowData = metaData.getSectionTableData();
    if (rowData == null) {
      return;
    }
    rows = new ArrayList(rowData.size());
    for (int i = 0; i < rowData.size(); i++) {
      SectionTableRowData data = (SectionTableRowData) rowData.get(i);
      SectionTableRow row = new SectionTableRow(this, data, false, JoinDialog.SETUP_TAB);
      int rowIndex = data.getRowIndex();
      rows.add(rowIndex, row);
    }
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      row.create(mode);
      row.add(pnlTable);
    }
    joinDialog.setNumSections(rows.size());
    repaint();
  }
  
  public String getInvalidReason() {
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      String invalidReason = row.getInvalidReason();
      if (invalidReason != null) {
        return invalidReason;
      }
    }
    return null;
  }
  
  int getXMax() {
    int xMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      xMax = Math.max(xMax, row.getXMax());
    }
    return xMax;
  }
  
  int getYMax() {
    int yMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      yMax = Math.max(yMax, row.getYMax());
    }
    return yMax;
  }
  
  int getZMax() {
    if (rows == null) {
      return 0;
    }
    int zMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      zMax = Math.max(zMax, row.getZMax());
    }
    return zMax;
  }

  /**
   * Add a JComponent to the table.
   * @param cell
   */
  public void addCell(JComponent cell) {
    layout.setConstraints(cell, constraints);
    pnlTable.add(cell);
  }

  public void removeCell(JComponent cell) {
    pnlTable.remove(cell);
  }

  /**
   * Call mainPanel repaint.
   *
   */
  private void repaint() {
    joinManager.getMainPanel().repaint();
  }

  /**
   * Create a multi line toggle button.  Set the border to raised bevel to make
   * it 3D.  Set its preferred width.
   * @param value
   * @param width
   * @return button created
   */
  MultiLineToggleButton createToggleButton(String text, int width) {
    MultiLineToggleButton button = new MultiLineToggleButton(text);
    button.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    Dimension size = button.getPreferredSize();
    size.width = width;
    button.setPreferredSize(size);
    return button;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
  }

  Container getContainer() {
    return rootPanel;
  }
  
  JPanel getRootPanel() {
    return rootPanel;
  }

  /**
   * Handle actions
   * @param event
   */
  private void action(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnMoveSectionUp.getActionCommand())) {
      moveSectionUp();
    }
    else if (command.equals(btnMoveSectionDown.getActionCommand())) {
      moveSectionDown();
    }
    else if (command.equals(btnAddSection.getActionCommand())) {
      addSection();
    }
    else if (command.equals(btnDeleteSection.getActionCommand())) {
      deleteSection();
    }
    else if (command.equals(btnOpen3dmod.getActionCommand())) {
      imodSection();
    }
    else if (command.equals(btnGetAngles.getActionCommand())) {
      imodGetAngles();
    }
  }

  //
  //  Action listener adapters
  //
  class SectionTableActionListener implements ActionListener {

    SectionTablePanel adaptee;

    SectionTableActionListener(SectionTablePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      action(event);
    }
  }
}