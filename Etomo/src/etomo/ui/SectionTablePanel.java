package etomo.ui;

import java.awt.Component;
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
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.JoinManager;
import etomo.storage.JoinInfoFile;
import etomo.storage.LogFile;
import etomo.storage.TomogramFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.JoinMetaData;
import etomo.type.JoinState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SectionTableRowData;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
 * <p>Description: A panel containing the section table.  Implements Expandable
 * so it can use ExpandButtons. </p>
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
 * <p> Revision 1.39  2007/03/27 00:05:45  sueh
 * <p> bug# 964 Removed print statement.
 * <p>
 * <p> Revision 1.38  2007/03/01 01:42:49  sueh
 * <p> bug# 964 Implementing Highlighable.
 * <p>
 * <p> Revision 1.37  2007/02/09 00:52:35  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.36  2007/02/05 23:43:18  sueh
 * <p> bug# 962 Switched to BinnedXY3dmodButton instead of a spinner and a button.
 * <p>
 * <p> Revision 1.35  2006/11/15 21:32:09  sueh
 * <p> bug# 950 Don't call defaultSizeInXY during synchronize.
 * <p>
 * <p> Revision 1.34  2006/11/07 23:01:24  sueh
 * <p> bug# 954 Adding tooltip to the second label in the spinners.  Adding Z order and
 * <p> invert table tooltips.
 * <p>
 * <p> Revision 1.33  2006/10/16 22:52:47  sueh
 * <p> bug# 919  Added setInverted().
 * <p>
 * <p> Revision 1.32  2006/07/21 19:13:56  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 1.31  2006/07/20 17:21:17  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 1.30  2006/07/10 21:28:19  sueh
 * <p> Removed order cut added Z order.
 * <p>
 * <p> Revision 1.29  2006/06/29 22:02:12  sueh
 * <p> bug# 880 Making sure that the order cut label appears correctly on each tab.
 * <p> Setting order cut after loading from metadata.
 * <p>
 * <p> Revision 1.28  2006/06/29 20:09:32  sueh
 * <p> bug# 880 Added renumberOrderCut().  Renumbering orderCut each time a sample
 * <p> is added or deleted.
 * <p>
 * <p> Revision 1.27  2006/04/28 21:04:30  sueh
 * <p> bug# 787 Named the expander button.
 * <p>
 * <p> Revision 1.26  2006/04/06 20:18:51  sueh
 * <p> bug# 808 Calling functions in JoinState when deleting or moving a row.
 * <p>
 * <p> Revision 1.25  2006/03/21 19:40:11  sueh
 * <p> bug# 807 In displayCurTab():  moved all calculations and saved integers,
 * <p> used for displaying the align tab, to SectionTableRow.  Passing the previous
 * <p> row to SectionTableRow.displayCurTab().
 * <p>
 * <p> Revision 1.24  2006/01/27 18:42:49  sueh
 * <p> bug# 801 Added validation for makejoin and finishjoin
 * <p>
 * <p> Revision 1.23  2005/12/16 18:27:21  sueh
 * <p> bug# 785 Added getMode().
 * <p>
 * <p> Revision 1.22  2005/12/16 01:46:53  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.21  2005/12/14 01:32:47  sueh
 * <p> bug# 783 Added isSetupTab(), etc, so that the only instance of curTab is
 * <p> in JoinDialog.
 * <p>
 * <p> Revision 1.20  2005/11/30 21:18:28  sueh
 * <p> bug# 757 Removed getJoinXMax, YMax, and ZMax().  Added getXMax,
 * <p> YMax, and ZMax().
 * <p>
 * <p> Revision 1.19  2005/11/29 22:50:56  sueh
 * <p> bug# 757 Added another header row to the join tab, which displays when
 * <p> one of the sections is a .rot file.
 * <p>
 * <p> Revision 1.18  2005/11/14 22:19:15  sueh
 * <p> bug# 762 Made action() protected.
 * <p>
 * <p> Revision 1.17  2005/09/20 19:13:14  sueh
 * <p> bug# 532 Using static get instance functions instead of the constructor
 * <p> because the options that must be passed to the constructor are getting
 * <p> too complicated.  Made the join expanding button the same size as the
 * <p> other expander buttons.
 * <p>
 * <p> Revision 1.16  2005/08/11 23:58:38  sueh
 * <p> bug# 711  Change 3dmod buttons to Run3dmodButton.  Implement
 * <p> Run3dmodButtonContainer.  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 1.15  2005/08/10 20:46:30  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 1.14  2005/08/09 20:36:07  sueh
 * <p> bug# 711 Moving button sizing from UIUtilities to the multi line button
 * <p> classes.  default setSize() sets the standard button dimension.
 * <p>
 * <p> Revision 1.13  2005/08/04 20:16:50  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.12  2005/07/29 00:54:35  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
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
public class SectionTablePanel implements ContextMenu, Expandable,
    Run3dmodButtonContainer,Highlightable {
  public static final String rcsid = "$Id$";

  private static final Dimension buttonDimension = UIParameters.INSTANCE
      .getButtonDimension();
  private static final String flipWarning[] = {
      "Tomograms have to be flipped after generation",
      "in order to be in the right orientation for joining serial sections." };
  private static final String HEADER1_SECTIONS_LABEL = "Sections";

  private final JPanel rootPanel = new JPanel();
  private final SpacedPanel pnlBorder = new SpacedPanel();
  private final JPanel pnlTable = new JPanel();
  private final SpacedPanel pnlButtons = new SpacedPanel();
  private final SpacedPanel pnlButtonsComponent1 = new SpacedPanel();
  private final SpacedPanel pnlButtonsComponent2 = new SpacedPanel();
  private final SpacedPanel pnlButtonsComponent4 = new SpacedPanel();
  private final UIHarness uiHarness = UIHarness.INSTANCE;
  private final MultiLineButton btnMoveSectionUp = new MultiLineButton(
      "Move Section Up");
  private final MultiLineButton btnMoveSectionDown = new MultiLineButton(
      "Move Section Down");
  private final MultiLineButton btnAddSection = new MultiLineButton(
      "Add Section");
  private final MultiLineButton btnDeleteSection = new MultiLineButton(
      "Delete Section");
  private final MultiLineButton btnGetAngles = new MultiLineButton(
      "Get Angles from Slicer");
  private final MultiLineButton btnInvertTable = new MultiLineButton(
      "Invert Table");
  //first header row
  private final HeaderCell header1ZOrder = new HeaderCell("Z Order");
  private final HeaderCell header1SetupSections = new HeaderCell(
      HEADER1_SECTIONS_LABEL, UIParameters.INSTANCE.getSectionsWidth());
  private ExpandButton button1ExpandSections = null;
  private final HeaderCell header1JoinSections = new HeaderCell(
      HEADER1_SECTIONS_LABEL, UIParameters.INSTANCE.getSectionsWidth());
  private final HeaderCell header1Sample = new HeaderCell("Sample Slices");
  private final HeaderCell header1SlicesInSample = new HeaderCell("Slices in");
  private final HeaderCell header1CurrentChunk = new HeaderCell("Current");
  private final HeaderCell header1ReferenceSection = new HeaderCell("Reference");
  private final HeaderCell header1CurrentSection = new HeaderCell("Current");
  private final HeaderCell header1SetupFinal = new HeaderCell("Final");
  private final HeaderCell header1JoinFinal = new HeaderCell("Final");
  private final HeaderCell header1Rotation = new HeaderCell("Rotation Angles");
  //second header row
  private final HeaderCell header2ZOrder = new HeaderCell();
  private final HeaderCell header2SetupSections = new HeaderCell();
  private final HeaderCell header2JoinSections = new HeaderCell("In Final");
  private final HeaderCell header2SampleBottom = new HeaderCell("Bottom");
  private final HeaderCell header2SampleTop = new HeaderCell("Top");
  private final HeaderCell header2SlicesInSample = new HeaderCell("Sample");
  private final HeaderCell header2CurrentChunk = new HeaderCell("Chunk");
  private final HeaderCell header2ReferenceSection = new HeaderCell("Section");
  private final HeaderCell header2CurrentSection = new HeaderCell("Section");
  private final HeaderCell header2SetupFinal = new HeaderCell();
  private final HeaderCell header2JoinFinal = new HeaderCell();
  private final HeaderCell header2Rotation = new HeaderCell();
  //third header row
  private final HeaderCell header3ZOrder = new HeaderCell();
  private final HeaderCell header3SetupSections = new HeaderCell();
  private final HeaderCell header3JoinSections = new HeaderCell();
  private final HeaderCell header3SampleBottomStart = new HeaderCell("Start",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3SampleBottomEnd = new HeaderCell("End",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3SampleTopStart = new HeaderCell("Start",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3SampleTopEnd = new HeaderCell("End",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3SetupFinalStart = new HeaderCell("Start",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3SetupFinalEnd = new HeaderCell("End",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3JoinFinalStart = new HeaderCell("Start",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3JoinFinalEnd = new HeaderCell("End",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3RotationX = new HeaderCell("X",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3RotationY = new HeaderCell("Y",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3RotationZ = new HeaderCell("Z",
      UIParameters.INSTANCE.getNumericWidth());
  private ArrayList rows = new ArrayList();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final SectionTableActionListener sectionTableActionListener = new SectionTableActionListener(
      this);

  private final JoinManager manager;
  private final JoinDialog joinDialog;

  private BinnedXY3dmodButton b3bOpen3dmod;

  private int mode = JoinDialog.SETUP_MODE;
  private boolean flipping = false;
  private final JoinState state;

  /**
   * Creates the panel and table.
   *
   */
  SectionTablePanel(JoinDialog joinDialog, JoinManager manager, JoinState state) {
    this.joinDialog = joinDialog;
    this.manager = manager;
    this.state = state;
    //create root panel
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(BorderFactory.createEtchedBorder());
    //table
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    pnlTable.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    button1ExpandSections = ExpandButton.getInstance(this,
        ExpandButton.Type.MORE);
    button1ExpandSections.setName(HEADER1_SECTIONS_LABEL);
    addTablePanelComponents();
    //buttons
    createButtonsPanel();
    addButtonsPanelComponents();
    addRootPanelComponents();
    setToolTipText();
  }

  final boolean isSetupTab() {
    return joinDialog.isSetupTab();
  }

  final boolean isAlignTab() {
    return joinDialog.isAlignTab();
  }

  final boolean isJoinTab() {
    return joinDialog.isJoinTab();
  }

  final boolean isRejoinTab() {
    return joinDialog.isRejoinTab();
  }

  private void addRootPanelComponents() {
    if (isJoinTab()) {
      rootPanel.setLayout(new GridLayout(2, 1));
    }
    else {
      rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    }
    rootPanel.add(pnlBorder.getContainer());
    pnlBorder.add(pnlTable);
    if (!isAlignTab()) {
      addButtonsPanelComponents();
      pnlBorder.add(pnlButtons);
    }
  }

  private void addTablePanelComponents() {
    //Table constraints
    if (isSetupTab()) {
      addSetupTablePanelComponents();
    }
    else if (isAlignTab()) {
      addAlignTablePanelComponents();
    }
    else if (isJoinTab() || isRejoinTab()) {
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
    constraints.weightx = 0.0;
    constraints.weighty = 0.2;
    constraints.gridheight = 1;
    constraints.gridwidth = 2;
    header1ZOrder.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    constraints.weightx = 0.2;
    header1SetupSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    button1ExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 4;
    header1Sample.add(pnlTable, layout, constraints);
    constraints.gridwidth = 2;
    header1SetupFinal.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1Rotation.add(pnlTable, layout, constraints);
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header2ZOrder.add(pnlTable, layout, constraints);
    constraints.weightx = 0.2;
    header2SetupSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header2SampleBottom.add(pnlTable, layout, constraints);
    header2SampleTop.add(pnlTable, layout, constraints);
    header2SetupFinal.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2Rotation.add(pnlTable, layout, constraints);
    //Third row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header3ZOrder.add(pnlTable, layout, constraints);
    constraints.weightx = 0.2;
    header3SetupSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    header3SampleBottomStart.add(pnlTable, layout, constraints);
    header3SampleBottomEnd.add(pnlTable, layout, constraints);
    header3SampleTopStart.add(pnlTable, layout, constraints);
    header3SampleTopEnd.add(pnlTable, layout, constraints);
    header3SetupFinalStart.add(pnlTable, layout, constraints);
    header3SetupFinalEnd.add(pnlTable, layout, constraints);
    header3RotationX.add(pnlTable, layout, constraints);
    header3RotationY.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header3RotationZ.add(pnlTable, layout, constraints);
  }

  final int getMode() {
    return mode;
  }

  private void addAlignTablePanelComponents() {
    //Header
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.2;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1ZOrder.add(pnlTable, layout, constraints);
    constraints.weightx = 0.2;
    header1SetupSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    button1ExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    header1SlicesInSample.add(pnlTable, layout, constraints);
    header1CurrentChunk.add(pnlTable, layout, constraints);
    header1ReferenceSection.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1CurrentSection.add(pnlTable, layout, constraints);
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    header2ZOrder.add(pnlTable, layout, constraints);
    constraints.weightx = 0.2;
    constraints.gridwidth = 2;
    header2SetupSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    header2SlicesInSample.add(pnlTable, layout, constraints);
    header2CurrentChunk.add(pnlTable, layout, constraints);
    header2ReferenceSection.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2CurrentSection.add(pnlTable, layout, constraints);
  }

  private void addJoinTablePanelComponents() {
    //Header
    //First row
    constraints.weightx = 0.0;
    constraints.weighty = 0.2;
    constraints.gridheight = 1;
    constraints.gridwidth = 2;
    header1ZOrder.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    constraints.weightx = 0.2;
    header1JoinSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    button1ExpandSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1JoinFinal.add(pnlTable, layout, constraints);
    if (hasRotatedSection()) {
      //Second row
      constraints.weightx = 0.0;
      constraints.gridwidth = 2;
      header2ZOrder.add(pnlTable, layout, constraints);
      constraints.weightx = 0.2;
      constraints.gridwidth = 2;
      header2JoinSections.add(pnlTable, layout, constraints);
      constraints.weightx = 0.1;
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      header2JoinFinal.add(pnlTable, layout, constraints);
      header3JoinSections.setText("Orientation");
    }
    //Third row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header3ZOrder.add(pnlTable, layout, constraints);
    constraints.weightx = 0.2;
    header3JoinSections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 1;
    header3JoinFinalStart.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header3JoinFinalEnd.add(pnlTable, layout, constraints);
  }

  /**
   * @return true if at least one row is rotated
   */
  private final boolean hasRotatedSection() {
    for (int i = 0; i < rows.size(); i++) {
      if (((SectionTableRow) rows.get(i)).isRotated()) {
        return true;
      }
    }
    return false;
  }

  private void createButtonsPanel() {
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    //first component
    pnlButtonsComponent1.setBoxLayout(BoxLayout.Y_AXIS);
    btnMoveSectionUp.setSize(true);
    btnMoveSectionUp.addActionListener(sectionTableActionListener);
    pnlButtonsComponent1.add(btnMoveSectionUp);
    btnAddSection.setSize(true);
    btnAddSection.addActionListener(sectionTableActionListener);
    pnlButtonsComponent1.add(btnAddSection);
    UIUtilities.setButtonSizeAll(pnlButtonsComponent1.getContainer(),
        buttonDimension);
    //second component
    pnlButtonsComponent2.setBoxLayout(BoxLayout.Y_AXIS);
    btnMoveSectionDown.setSize(true);
    btnMoveSectionDown.addActionListener(sectionTableActionListener);
    pnlButtonsComponent2.add(btnMoveSectionDown);
    btnDeleteSection.setSize(true);
    btnDeleteSection.addActionListener(sectionTableActionListener);
    pnlButtonsComponent2.add(btnDeleteSection);
    //third component
    b3bOpen3dmod = new BinnedXY3dmodButton("Open in 3dmod", this);
    b3bOpen3dmod.addActionListener(sectionTableActionListener);
    b3bOpen3dmod
        .setSpinnerToolTipText("The binning to use when opening a section in 3dmod.");
    //fourth component
    pnlButtonsComponent4.setBoxLayout(BoxLayout.Y_AXIS);
    btnGetAngles.setSize(true);
    btnGetAngles.addActionListener(sectionTableActionListener);
    pnlButtonsComponent4.add(btnGetAngles);
    btnInvertTable.setSize(true);
    btnInvertTable.addActionListener(sectionTableActionListener);
    pnlButtonsComponent4.add(btnInvertTable);
  }

  private void addButtonsPanelComponents() {
    if (isSetupTab()) {
      pnlButtons.add(pnlButtonsComponent1);
      pnlButtons.add(pnlButtonsComponent2);
    }
    if (!isAlignTab()) {
      pnlButtons.add(b3bOpen3dmod.getContainer());
    }
    if (isSetupTab()) {
      pnlButtons.add(pnlButtonsComponent4);
    }
  }

  void displayCurTab() {
    rootPanel.removeAll();
    pnlButtons.removeAll();
    pnlBorder.removeAll();
    addRootPanelComponents();
    addButtonsPanelComponents();
    pnlTable.removeAll();
    addTablePanelComponents();
    SectionTableRow prevRow = null;
    //redisplay rows and calculate chunks
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      row.displayCurTab(pnlTable, prevRow, rows.size());
      prevRow = row;
    }
  }

  final int getTableSize() {
    return rows.size();
  }

  private int getHighlightedRowIndex() {
    for (int i = 0; i < rows.size(); i++) {
      if (((SectionTableRow) rows.get(i)).isHighlighted()) {
        return i;
      }
    }
    return -1;
  }

  void setInverted() throws LogFile.FileException{
    JoinInfoFile joinInfoFile =  JoinInfoFile.getInstance(manager);
    int invertedCount = 0;
    int size = rows.size();
    for (int i = 0; i < size; i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      ConstEtomoNumber inverted = joinInfoFile.getInverted(i);
      if (inverted == null) {
        continue;
      }
      if (inverted.is()) {
        invertedCount++;
      }
      row.setInverted(inverted);
    }
    if (invertedCount > size / 2) {
      uiHarness
          .openMessageDialog(
              "Most of the sections in this join will be inverted.  "
                  + SectionTableRow.INVERTED_WARNING
                  + "  If you don't want these inversions, "
                  + "push the \"Change Setup\" button and then push the \"Invert Table\" button.",
              "Join Warning");
    }
  }
  
  /**
   * Respond to highlight request
   */
  public void highlight(boolean highlight) {
    setMode();
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
      btnInvertTable.setEnabled(false);
      break;
    case JoinDialog.SETUP_MODE:
    case JoinDialog.SAMPLE_NOT_PRODUCED_MODE:
    case JoinDialog.CHANGING_SAMPLE_MODE:
      if (!flipping) {
        btnAddSection.setEnabled(true);
        btnInvertTable.setEnabled(true);
      }
      break;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
    enableRowButtons(getHighlightedRowIndex());
    for (int i = 0; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).setMode(mode);
    }
  }

  void setJoinFinalStartHighlight(boolean highlight) {
    for (int i = 0; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).setJoinFinalStartHighlight(highlight);
    }
  }

  void setJoinFinalEndHighlight(boolean highlight) {
    for (int i = 0; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).setJoinFinalEndHighlight(highlight);
    }
  }

  /**
   * Enable row level buttons based on the current highlight
   * @param highlightedRowIndex
   */
  private void enableRowButtons(int highlightedRowIndex) {
    int rowsSize = 0;
    rowsSize = rows.size();
    if (rowsSize == 0) {
      b3bOpen3dmod.setEnabled(false);
      button1ExpandSections.setEnabled(false);
      if (mode != JoinDialog.SAMPLE_PRODUCED_MODE) {
        btnMoveSectionUp.setEnabled(false);
        btnMoveSectionDown.setEnabled(false);
        btnDeleteSection.setEnabled(false);
        btnGetAngles.setEnabled(false);
      }
      return;
    }
    b3bOpen3dmod.setEnabled(highlightedRowIndex > -1);
    button1ExpandSections.setEnabled(true);
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
    if (expandButton.equals(button1ExpandSections)) {
      boolean expand = button1ExpandSections.isExpanded();
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
    if (array == null) {
      return false;
    }
    if (rows.size() != array.size()) {
      return false;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i))
          .equals((ConstSectionTableRowData) array.get(i))) {
        return false;
      }
    }
    return true;
  }

  public boolean equalsSample(ConstJoinMetaData metaData) {
    ArrayList array = metaData.getSectionTableData();
    if (array == null) {
      return false;
    }
    if (rows.size() != array.size()) {
      return false;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i))
          .equalsSample((ConstSectionTableRowData) array.get(i))) {
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
    state.moveRowUp(rowIndex);
    configureRows();
    enableRowButtons(rowIndex - 1);
    joinDialog.msgRowChange();
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
          "Can't move the row down.  Its at the bottom.", "Wrong Row",
          AxisID.ONLY);
      return;
    }
    removeRowsFromTable(rowIndex);
    Object rowMoveUp = rows.remove(rowIndex + 1);
    Object rowMoveDown = rows.remove(rowIndex);
    rows.add(rowIndex, rowMoveUp);
    rows.add(rowIndex + 1, rowMoveDown);
    addRowsToTable(rowIndex);
    renumberTable(rowIndex);
    state.moveRowDown(rowIndex);
    configureRows();
    enableRowButtons(rowIndex + 1);
    joinDialog.msgRowChange();
    repaint();
  }

  private void addSection() {
    StringBuffer invalidBuffer = new StringBuffer();
    if (!Utilities.isValidFile(joinDialog.getWorkingDir(),
        JoinDialog.WORKING_DIRECTORY_TEXT, invalidBuffer, true, true, true,
        true)) {
      uiHarness.openMessageDialog(invalidBuffer.toString(),
          "Unable to Add Section", AxisID.ONLY);
      return;
    }
    //  Open up the file chooser in the working directory
    JFileChooser chooser = new JFileChooser(new File(manager
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
      MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
          tomogram.getAbsolutePath(), AxisID.ONLY);
      if (!readHeader(header)) {
        return;
      }
      flipping = true;
      btnAddSection.setEnabled(false);
      btnInvertTable.setEnabled(false);
      if (header.getNRows() < header.getNSections()) {
        //The tomogram may not be flipped
        //Ask use if can flip the tomogram
        String msgFlipped[] = {
            "It looks like you didn't flip the tomogram in Post Processing",
            "bacause the tomogram is thicker in Z then it is long in Y.",
            flipWarning[0], flipWarning[1],
            "Shall I use the clip flipyz command to flip Y and Z?" };
        if (uiHarness.openYesNoDialog(msgFlipped, AxisID.ONLY)) {
          manager.flip(tomogram, joinDialog.getWorkingDir());
          return;
        }
      }
      addSection(tomogram);
      uiHarness.pack(AxisID.ONLY, manager);
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
      if (!uiHarness.openYesNoDialog(msgInvalidParameterException, AxisID.ONLY)) {
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
        if (!uiHarness.openYesNoDialog(msgNumberFormatException, AxisID.ONLY)) {
          return false;
        }
      }
    }
    return true;
  }

  private boolean isDuplicate(File section) {
    for (int i = 0; i < rows.size(); i++) {
      if (((SectionTableRow) rows.get(i)).equalsSetupSection(section)) {
        String msgDuplicate = "The file, " + section.getAbsolutePath()
            + ", is already in the table.";
        uiHarness.openMessageDialog(msgDuplicate, "Add Section Failed",
            AxisID.ONLY);
        return true;
      }
    }
    return false;
  }

  void addSection(File tomogram) {
    flipping = false;
    setMode();
    if (!tomogram.exists()) {
      uiHarness.openMessageDialog(tomogram.getAbsolutePath()
          + " does not exist.", "File Error", AxisID.ONLY);
      return;
    }
    if (!tomogram.isFile()) {
      uiHarness.openMessageDialog(tomogram.getAbsolutePath()
          + " is not a file.", "File Error", AxisID.ONLY);
      return;
    }
    //Sections are only added in the Setup tab, so assume that the join
    //expand button is contracted.
    SectionTableRow row = new SectionTableRow(manager, this, rows.size() + 1,
        tomogram, button1ExpandSections.isExpanded());
    row.setMode(mode);
    row.add(pnlTable);
    rows.add(row);
    int tableSize = rows.size();
    configureRows();
    joinDialog.setNumSections(tableSize);
    joinDialog.msgRowChange();
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
    if (!uiHarness.openYesNoDialog("Really remove " + row.getSetupSectionText()
        + "?", AxisID.ONLY)) {
      return;
    }
    rows.remove(rowIndex);
    row.remove();
    row.removeImod();
    renumberTable(rowIndex);
    state.deleteRow(rowIndex);
    configureRows();
    joinDialog.setNumSections(rows.size());
    enableRowButtons(-1);
    joinDialog.msgRowChange();
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
  private void imodSection(Run3dmodMenuOptions menuOptions) {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    int binning = b3bOpen3dmod.getInt();
    SectionTableRow row = (SectionTableRow) rows.get(rowIndex);
    if (isSetupTab()) {
      row.imodOpenSetupSectionFile(binning, menuOptions);
    }
    else {
      row.imodOpenJoinSectionFile(binning, menuOptions);
    }
  }

  private void imodGetAngles() {
    int rowIndex = getHighlightedRowIndex();
    if (rowIndex == -1) {
      return;
    }
    if (((SectionTableRow) rows.get(rowIndex)).imodGetAngles()) {
      repaint();
    }
  }

  /**
   * Renumber the table starting from the row in the ArrayList at startIndex.
   * @param startIndex
   */
  private void renumberTable(int startIndex) {
    int rowsSize = rows.size();
    SectionTableRow row;
    for (int i = startIndex; i < rowsSize; i++) {
      ((SectionTableRow) rows.get(i)).setRowNumber(i + 1);
    }
  }

  private void invertTable() {
    int size = rows.size();
    ArrayList newRows = new ArrayList();
    int rowNumber = 0;
    for (int i = size - 1; i >= 0; i--) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      //remove the row from the display
      row.remove();
      //place the row in its new position in the array and configure it
      row.setRowNumber(++rowNumber);
      row.swapBottomTop();
      newRows.add(row);
    }
    //add the rows to the display
    rows = newRows;
    addRowsToTable(0);
    configureRows();
    enableRowButtons(getHighlightedRowIndex());
    joinDialog.msgRowChange();
    repaint();
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
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      ConstSectionTableRowData rowData = row.getData();
      if (!row.isValid()) {
        success = false; //getData() failed
      }
      metaData.setSectionTableData(new SectionTableRowData(manager, rowData));
    }
    return success;
  }

  boolean validateMakejoincom() {
    if (rows == null) {
      return true;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i)).validateMakejoincom()) {
        return false;
      }
    }
    return true;
  }

  boolean validateFinishjoin() {
    if (rows == null) {
      return true;
    }
    for (int i = 0; i < rows.size(); i++) {
      if (!((SectionTableRow) rows.get(i)).validateFinishjoin()) {
        return false;
      }
    }
    return true;
  }

  public void setMetaData(ConstJoinMetaData metaData) {
    ArrayList rowData = metaData.getSectionTableData();
    if (rowData == null) {
      return;
    }
    for (int i = 0; i < rowData.size(); i++) {
      SectionTableRowData data = (SectionTableRowData) rowData.get(i);
      SectionTableRow row = new SectionTableRow(manager, this, data, false);
      int rowIndex = data.getRowIndex();
      rows.add(rowIndex, row);
    }
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      row.setMode(mode);
      row.add(pnlTable);
    }
    configureRows();
    joinDialog.setNumSections(rows.size());
    repaint();
  }

  private final void configureRows() {
    for (int i = 0; i < rows.size(); i++) {
      ((SectionTableRow) rows.get(i)).setInUse();
    }
  }

  final public String getInvalidReason() {
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      String invalidReason = row.getInvalidReason();
      if (invalidReason != null) {
        return invalidReason;
      }
    }
    return null;
  }

  final int getXMax() {
    int xMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      xMax = Math.max(xMax, row.getXMax());
    }
    return xMax;
  }

  final int getYMax() {
    int yMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      yMax = Math.max(yMax, row.getYMax());
    }
    return yMax;
  }

  final int getZMax() {
    int zMax = 0;
    for (int i = 0; i < rows.size(); i++) {
      SectionTableRow row = (SectionTableRow) rows.get(i);
      zMax = Math.max(zMax, row.getZMax());
    }
    return zMax;
  }

  int getRowsSize() {
    return rows.size();
  }

  public final void removeCell(Component cell) {
    pnlTable.remove(cell);
  }

  /**
   * Call mainPanel repaint.
   *
   */
  private final void repaint() {
    manager.getMainPanel().repaint();
  }

  /**
   * Right mouse button context menu
   */
  public final void popUpContextMenu(MouseEvent mouseEvent) {
  }

  final Container getContainer() {
    return rootPanel;
  }

  final JPanel getRootPanel() {
    return rootPanel;
  }

  public final void run3dmod(Run3dmodButton button,
      Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  private final void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(b3bOpen3dmod.getActionCommand())) {
      imodSection(menuOptions);
    }
  }

  /**
   * Handle actions
   * @param event
   */
  protected final void action(ActionEvent event) {
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
    else if (command.equals(btnGetAngles.getActionCommand())) {
      imodGetAngles();
    }
    else if (command.equals(btnInvertTable.getActionCommand())) {
      invertTable();
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  /**
   * Synchronizes when entering or leaving the join tab.  This function should
   * be called when switching tabs and before any save to the .ejf file.
   * @param prevTab
   * @param curTab
   */
  final void synchronize(JoinDialog.Tab prevTab, JoinDialog.Tab curTab) {
    if (rows.size() == 0) {
      return;
    }
    //synchronize setup columns to join columns when the user gets to the join
    //tab or the model tab
    if (curTab == JoinDialog.Tab.JOIN || curTab == JoinDialog.Tab.REJOIN
        || curTab == JoinDialog.Tab.MODEL) {
      for (int i = 0; i < rows.size(); i++) {
        ((SectionTableRow) rows.get(i)).synchronizeSetupToJoin();
      }
      //joinDialog.defaultSizeInXY();
    }
    //synchronize join columns to setup columns when the users leaves the join
    //tab
    else if (prevTab == JoinDialog.Tab.JOIN || prevTab == JoinDialog.Tab.REJOIN) {
      for (int i = 0; i < rows.size(); i++) {
        ((SectionTableRow) rows.get(i)).synchronizeJoinToSetup();
      }
    }
  }

  private void setToolTipText() {
    btnMoveSectionUp.setToolTipText("Press to move the selected section up.");
    btnMoveSectionDown
        .setToolTipText("Press to move the selected section down.");
    btnAddSection
        .setToolTipText("Press to add a section to the joined tomogram.");
    btnDeleteSection
        .setToolTipText("Press to delete the selected section from the joined tomogram.");
    btnGetAngles
        .setToolTipText("Press to get the X, Y, and Z rotation from the slicer in 3dmod for the selected section.");
    btnDeleteSection
        .setToolTipText("The order of the sections in the joined tomogram.");

    String text = "The sections used in the joined tomogram.";
    header1SetupSections.setToolTipText(text);
    header2SetupSections.setToolTipText(text);
    header3SetupSections.setToolTipText(text);

    text = "The sections, including rotated sections, used in the joined tomogram.";
    header1JoinSections.setToolTipText(text);
    header2JoinSections.setToolTipText(text);
    header3JoinSections.setToolTipText(text);

    header1Sample.setToolTipText("The slices to be used in the sample.");
    header2SampleBottom
        .setToolTipText("The bottom slices to be used in the sample.  The bottom slices should be matched against the top slices of the previous section.");
    header3SampleBottomStart
        .setToolTipText("The starting bottom slice to be used in the sample.  The bottom slices should be matched against the top slices of the previous section.");
    header3SampleBottomEnd
        .setToolTipText("The ending bottom slice to be used in the sample.  The bottom slices should be matched against the top slices of the previous section.");
    header2SampleTop
        .setToolTipText("The top slices to be used in the sample.  The top slices should be matched against the bottom slices of the next section.");
    header3SampleTopStart
        .setToolTipText("The starting top slice to be used in the sample.  The top slices should be matched against the bottom slices of the next section.");
    header3SampleTopEnd
        .setToolTipText("The ending top slice to be used in the sample.  The top slices should be matched against the bottom slices of the next section.");

    text = "Shows where each of the sample slices comes from.";
    header1SlicesInSample.setToolTipText(text);
    header2SlicesInSample.setToolTipText(text);

    text = "Shows how the sample is divided up in Midas.";
    header1CurrentChunk.setToolTipText(text);
    header2CurrentChunk.setToolTipText(text);

    text = "The reference section slices for each chunk in Midas.";
    header1ReferenceSection.setToolTipText(text);
    header2ReferenceSection.setToolTipText(text);

    text = "The current section slices for each chunk in Midas.";
    header1CurrentSection.setToolTipText(text);
    header2CurrentSection.setToolTipText(text);

    text = "Enter to starting and ending Z values to be used to trim each section in the joined tomogram.";
    header1SetupFinal.setToolTipText(text);
    header2SetupFinal.setToolTipText(text);
    header3SetupFinalStart
        .setToolTipText("Enter to starting Z value to be used to trim each section in the joined tomogram.");
    header3SetupFinalEnd
        .setToolTipText("Enter to ending Z value to be used to trim the section in the joined tomogram.");

    text = "Enter to starting and ending Z values to be used to trim each section or rotated section in the joined tomogram.";
    header1JoinFinal.setToolTipText(text);
    header2JoinFinal.setToolTipText(text);
    header3JoinFinalStart
        .setToolTipText("Enter to starting Z values to be used to trim each section or rotated section in the joined tomogram.");
    header3JoinFinalEnd
        .setToolTipText("Enter to ending Z values to be used to trim each section or rotated section in the joined tomogram.");

    text = "The rotation in X, Y, and Z of each section.";
    header1Rotation.setToolTipText(text);
    header2Rotation.setToolTipText(text);
    header3RotationX.setToolTipText("The rotation in X of each section.");
    header3RotationY.setToolTipText("The rotation in Y of each section.");
    header3RotationZ.setToolTipText("The rotation in Z of each section.");
    b3bOpen3dmod.setButtonToolTipText("Press to open a section in 3dmod.");
    text = "Order of the sections in Z.";
    header1ZOrder.setToolTipText(text);
    header2ZOrder.setToolTipText(text);
    header3ZOrder.setToolTipText(text);
    btnInvertTable
        .setToolTipText("Reverse the order of the sections in the table.");
  } //  //  Action listener adapters  //

  final class SectionTableActionListener implements ActionListener {

    SectionTablePanel adaptee;

    SectionTableActionListener(SectionTablePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      action(event);
    }
  }
}