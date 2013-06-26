package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;
import etomo.ui.FieldLabels;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.28  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.27  2009/12/23 02:24:33  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.  Added tooltips to the actual fields in the tables instead of the column headers.
 * <p>
 * <p> Revision 1.26  2009/12/08 02:46:39  sueh
 * <p> Added size().
 * <p>
 * <p> Revision 1.25  2009/12/02 04:40:41  sueh
 * <p> bug# 1226 Added move up and move down functionality.
 * <p>
 * <p> Revision 1.24  2009/12/01 23:42:20  sueh
 * <p> bug# 1290 In addRow calling PeetDialog.updateDisplay.
 * <p>
 * <p> Revision 1.23  2009/11/20 17:16:09  sueh
 * <p> bug# 1282 Added headers for duplicateShiftTolerance and
 * <p> duplicateAngularTolerance.  When updating the display, moved the
 * <p> responsibility for responding to sample sphere in the first row only to this
 * <p> class.
 * <p>
 * <p> Revision 1.22  2009/10/15 23:36:04  sueh
 * <p> bug# 1274 Made header names avaible becauese of increased error
 * <p> checking in the row.  Changed hiCutoffCutoff to hiCutoff.  Change
 * <p> hiCutoffSigma to lowCutoff.  Passing manager rather then the
 * <p> managerKey, which can change, to rows.
 * <p>
 * <p> Revision 1.21  2009/09/28 18:35:30  sueh
 * <p> bug# 1235 Added IterationRow.setNames.
 * <p>
 * <p> Revision 1.20  2009/04/20 16:38:12  sueh
 * <p> bug# 1214 Share the increment header.
 * <p>
 * <p> Revision 1.19  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.18  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.17  2008/09/30 21:41:45  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.16  2008/08/21 00:09:15  sueh
 * <p> bug# 1132 Sharing the Max header.
 * <p>
 * <p> Revision 1.15  2008/04/02 17:35:13  sueh
 * <p> bug# 1098 Improved user error messages.
 * <p>
 * <p> Revision 1.14  2008/04/02 02:27:23  sueh
 * <p> bug# 1097 Fixed a null pointer error in action and deleteRow.
 * <p>
 * <p> Revision 1.13  2008/03/06 00:27:53  sueh
 * <p> bug# 1088 Added updateDisplay.
 * <p>
 * <p> Revision 1.12  2008/02/27 23:01:19  sueh
 * <p> bug# 1090 Added "Add Row" button to iteration table.
 * <p>
 * <p> Revision 1.11  2007/07/10 00:43:09  sueh
 * <p> bug# 1022 Published some header strings.
 * <p>
 * <p> Revision 1.10  2007/06/14 19:36:33  sueh
 * <p> bug# 1020 WIdening cutoff an sigma.
 * <p>
 * <p> Revision 1.9  2007/06/08 22:21:38  sueh
 * <p> bug# 1014 Added reset().
 * <p>
 * <p> Revision 1.8  2007/05/15 19:58:17  sueh
 * <p> bug# 964 Added btnDeleteRow, deleteRow(IterationRow).  In RowList
 * <p> added delete(IterationRow...) and remove().
 * <p>
 * <p> Revision 1.7  2007/05/08 01:19:48  sueh
 * <p> bug# 964 Removed the alternative tooltip keys.
 * <p>
 * <p> Revision 1.6  2007/05/07 17:22:53  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.  Added alternative
 * <p> tooltip location for hiCutoff (in case relying on lowCutoff comment).
 * <p>
 * <p> Revision 1.5  2007/05/03 21:11:01  sueh
 * <p> bug# 964 Made searchRadius wider, since it can be three integers.
 * <p>
 * <p> Revision 1.4  2007/04/26 02:49:17  sueh
 * <p> bug# 964 Changed dPhiEnd to dPhiMax.  Did the same for dTheta and dPsi.
 * <p>
 * <p> Revision 1.3  2007/04/19 22:01:06  sueh
 * <p> bug# 964 Added get/setParamters(MatlabParamFile)
 * <p>
 * <p> Revision 1.2  2007/04/02 21:52:06  sueh
 * <p> bug# 964 Disabling initMotlFile column and button when PeetDialog.rbInitMotlFile
 * <p> is not selected.  Disabling tiltRange column when PeetDialog.cbTiltRange is not
 * <p> selected.
 * <p>
 * <p> Revision 1.1  2007/04/02 16:03:05  sueh
 * <p> bug# 964 Contains per iteration PEET data.
 * <p> </p>
 */
final class IterationTable implements Highlightable {
  public static final String rcsid = "$Id$";
  static final String D_PHI_D_THETA_D_PSI_HEADER1 = "Angular Search Range";
  static final String INCR_HEADER3 = "Step";
  static final String SEARCH_RADIUS_HEADER1 = "Search";
  static final String SEARCH_RADIUS_HEADER2 = "Distance";
  static final String LABEL = "Iteration Table";
  static final String MAX_HEADER3 = "Max";
  static final String CUTOFF_HEADER1 = "Hi Freq";
  static final String CUTOFF_HEADER2 = "Filter";
  static final String HI_CUTOFF_HEADER3 = "Cutoff";
  static final String LOW_CUTOFF_HEADER3 = "Sigma";
  static final String REF_THRESHOLD_HEADER1 = "Ref";
  static final String REF_THRESHOLD_HEADER2 = "Threshold";
  static final String DUPLICATE_TOLERANCE_HEADER1 = "Duplicate";
  static final String DUPLICATE_TOLERANCE_HEADER2 = "Tolerance";
  static final String DUPLICATE_SHIFT_TOLERANCE_HEADER3 = "Shift";
  static final String DUPLICATE_ANGULAR_TOLERANCE_HEADER3 = "Angle";

  private final JPanel rootPanel = new JPanel();
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final RowList rowList;
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final HeaderCell header1IterationNumber = new HeaderCell("Run #");
  private final HeaderCell header2IterationNumber = new HeaderCell();
  private final HeaderCell header3IterationNumber = new HeaderCell();
  private final HeaderCell header1DPhiDThetaDPsi = new HeaderCell(
      D_PHI_D_THETA_D_PSI_HEADER1);
  private final HeaderCell header2DPhi = new HeaderCell(FieldLabels.D_PHI_LABEL);
  private final HeaderCell header2DTheta = new HeaderCell(FieldLabels.D_THETA_LABEL);
  private final HeaderCell header2DPsi = new HeaderCell(FieldLabels.D_PSI_LABEL);
  private final HeaderCell header3DPhiMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPhiIncrement = new HeaderCell(INCR_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaIncrement = new HeaderCell(INCR_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiIncrement = new HeaderCell(INCR_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1SearchRadius = new HeaderCell(SEARCH_RADIUS_HEADER1);
  private final HeaderCell header2SearchRadius = new HeaderCell(SEARCH_RADIUS_HEADER2);
  private final HeaderCell header3SearchRadius = new HeaderCell(
      UIParameters.INSTANCE.getIntegerTripletWidth());
  private final HeaderCell header1Cutoff = new HeaderCell(CUTOFF_HEADER1);
  private final HeaderCell header2Cutoff = new HeaderCell(CUTOFF_HEADER2);
  private final HeaderCell header3HiCutoff = new HeaderCell(HI_CUTOFF_HEADER3,
      UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header3LowCutoff = new HeaderCell(LOW_CUTOFF_HEADER3,
      UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header1RefThreshold = new HeaderCell(REF_THRESHOLD_HEADER1);
  private final HeaderCell header2RefThreshold = new HeaderCell(REF_THRESHOLD_HEADER2);
  private final HeaderCell header3RefThreshold = new HeaderCell();
  private final HeaderCell header1DuplicateTolerance = new HeaderCell(
      DUPLICATE_TOLERANCE_HEADER1);
  private final HeaderCell header2DuplicateTolerance = new HeaderCell(
      DUPLICATE_TOLERANCE_HEADER2);
  private final HeaderCell header3DuplicateShiftTolerance = new HeaderCell(
      DUPLICATE_SHIFT_TOLERANCE_HEADER3);
  private final HeaderCell header3DuplicateAngularTolerance = new HeaderCell(
      DUPLICATE_ANGULAR_TOLERANCE_HEADER3);
  private final MultiLineButton btnMoveUp = new MultiLineButton("Up");
  private final MultiLineButton btnMoveDown = new MultiLineButton("Down");
  private final MultiLineButton btnAddRow = new MultiLineButton("Insert");
  private final MultiLineButton btnDeleteRow = new MultiLineButton("Delete");
  private final MultiLineButton btnCopyRow = new MultiLineButton("Dup");
  private final CheckBox cbFlgRemoveDuplicates = new CheckBox(
      FieldLabels.FLG_REMOVE_DUPLICATES_LABEL);
  private final JPanel pnlTableAndCheckbox = new JPanel();
  private final JPanel pnlFlgRemoveDuplicates = new JPanel();

  private final BaseManager manager;
  private final IterationParent parent;
  private Component verticalRigidArea1 = null;

  private IterationTable(BaseManager manager, IterationParent parent) {
    this.manager = manager;
    this.parent = parent;
    rowList = new RowList(manager, this);
    createTable();
    rowList.add(this, pnlTable, layout, constraints);
    display();
    updateDisplay();
    refreshVerticalPadding();
    setToolTipText();
  }

  static IterationTable getInstance(BaseManager manager, IterationParent parent) {
    IterationTable instance = new IterationTable(manager, parent);
    instance.addListeners();
    return instance;
  }

  public void highlight(final boolean highlight) {
    updateDisplay();
  }

  boolean validateRun() {
    return rowList.validateRun();
  }

  Container getContainer() {
    return rootPanel;
  }

  void reset() {
    rowList.remove();
    addRow();
    cbFlgRemoveDuplicates.setSelected(false);
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    rowList.getParameters(matlabParamFile);
    matlabParamFile.setFlgRemoveDuplicates(cbFlgRemoveDuplicates.isSelected());
  }

  /**
   * Update display in rows.
   * @param sampleSphere
   * @param flgRemoveDuplicates
   */
  void updateDisplay(final boolean sampleSphere) {
    rowList.updateDisplay(sampleSphere, cbFlgRemoveDuplicates.isSelected());
  }

  void setParameters(final MatlabParam matlabParamFile) {
    // overwrite existing rows
    int rowListSize = rowList.size();
    for (int i = 0; i < rowListSize; i++) {
      rowList.getRow(i).setParameters(matlabParamFile);
    }
    // add new rows
    for (int j = rowListSize; j < matlabParamFile.getIterationListSize(); j++) {
      IterationRow row = addRow();
      row.setParameters(matlabParamFile);
    }
    cbFlgRemoveDuplicates.setSelected(matlabParamFile.isFlgRemoveDuplicates());
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private IterationRow addRow() {
    IterationRow row = rowList.add(this, pnlTable, layout, constraints);
    row.display();
    parent.updateDisplay();
    refreshVerticalPadding();
    return row;
  }

  int size() {
    return rowList.size();
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    header3DuplicateShiftTolerance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.DUPLICATE_SHIFT_TOLERANCE_KEY));
    header3DuplicateAngularTolerance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.DUPLICATE_ANGULAR_TOLERANCE_KEY));

    btnAddRow.setToolTipText("Add a new iteration row to the table.");
    btnCopyRow.setToolTipText("Create a new row that is a duplicate of the highlighted "
        + "row.");
    btnMoveUp.setToolTipText("Move highlighted row up in the table.");
    btnMoveDown.setToolTipText("Move highlighted row down in the table");
    btnDeleteRow.setToolTipText("Remove highlighted row from table.");
    cbFlgRemoveDuplicates
        .setToolTipText("Remove mulitple references to the same particle after"
            + "each iteration.");
  }

  private void addListeners() {
    ITActionListener actionListener = new ITActionListener(this);
    btnAddRow.addActionListener(actionListener);
    btnCopyRow.addActionListener(actionListener);
    btnDeleteRow.addActionListener(actionListener);
    btnMoveUp.addActionListener(actionListener);
    btnMoveDown.addActionListener(actionListener);
    cbFlgRemoveDuplicates.addActionListener(actionListener);
  }

  private void action(final ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (actionCommand.equals(btnAddRow.getActionCommand())) {
      addRow();
      UIHarness.INSTANCE.pack(manager);
    }
    else if (actionCommand.equals(btnCopyRow.getActionCommand())) {
      IterationRow row = rowList.getHighlightedRow();
      if (row == null) {
        return;
      }
      copyRow(row);
    }
    else if (actionCommand.equals(btnDeleteRow.getActionCommand())) {
      IterationRow row = rowList.getHighlightedRow();
      if (row == null) {
        return;
      }
      deleteRow(row);
    }
    else if (actionCommand.equals(btnMoveUp.getActionCommand())) {
      moveRowUp();
    }
    else if (actionCommand.equals(btnMoveDown.getActionCommand())) {
      moveRowDown();
    }
    else if (actionCommand.equals(cbFlgRemoveDuplicates.getActionCommand())) {
      updateDisplay(parent.isSampleSphere());
    }
  }

  private void copyRow(IterationRow row) {
    rowList.copy(row, this, pnlTable, layout, constraints);
    parent.updateDisplay();
    refreshVerticalPadding();
    UIHarness.INSTANCE.pack(manager);
  }

  private void deleteRow(IterationRow row) {
    rowList.remove();
    int index = rowList.delete(row, this, pnlTable, layout, constraints);
    rowList.highlight(index);
    rowList.display();
    updateDisplay();
    refreshVerticalPadding();
    UIHarness.INSTANCE.pack(manager);
  }

  /**
   * Swap the highlighted row with the one above it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveRowUp() {
    int index = rowList.getHighlightIndex();
    if (index == -1) {
      return;
    }
    if (index == 0) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Can't move the row up.  Its at the top.", "Wrong Row", AxisID.ONLY);
      return;
    }
    rowList.moveRowUp(index);
    rowList.remove();
    rowList.reindex(index - 1);
    rowList.display();
    updateDisplay();
    manager.getMainPanel().repaint();
  }

  /**
   * Swap the highlighted row with the one below it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveRowDown() {
    int index = rowList.getHighlightIndex();
    if (index == -1) {
      return;
    }
    if (index == rowList.size() - 1) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Can't move the row down.  Its at the bottom.", "Wrong Row", AxisID.ONLY);
      return;
    }
    rowList.moveRowDown(index);
    rowList.remove();
    rowList.reindex(index);
    rowList.display();
    updateDisplay();
    manager.getMainPanel().repaint();
  }

  private void updateDisplay() {
    int highlightIndex = rowList.getHighlightIndex();
    btnCopyRow.setEnabled(highlightIndex != -1);
    btnDeleteRow.setEnabled(highlightIndex != -1);
    btnMoveUp.setEnabled(highlightIndex > 0);
    btnMoveDown.setEnabled(highlightIndex != -1 && highlightIndex < rowList.size() - 1);
  }

  private void createTable() {
    // local panels
    JPanel pnlButtons = new JPanel();
    // table
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    constraints.fill = GridBagConstraints.BOTH;
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.gridheight = 1;
    // button panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.Y_AXIS));
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlButtons.add(btnMoveUp.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlButtons.add(btnMoveDown.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlButtons.add(btnAddRow.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlButtons.add(btnDeleteRow.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlButtons.add(btnCopyRow.getComponent());
    pnlButtons.add(Box.createVerticalGlue());
    pnlButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    // border
    SpacedPanel pnlBorder = SpacedPanel.getInstance();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.add(pnlTable);
    // checkbox
    pnlFlgRemoveDuplicates.add(cbFlgRemoveDuplicates);
    // table and checkbox
    pnlTableAndCheckbox.setLayout(new BoxLayout(pnlTableAndCheckbox, BoxLayout.Y_AXIS));
    pnlTableAndCheckbox.add(pnlBorder.getContainer());
    refreshVerticalPadding();
    // root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.X_AXIS));
    rootPanel.setBorder(new EtchedBorder(LABEL).getBorder());
    rootPanel.add(pnlTableAndCheckbox);
    rootPanel.add(Box.createRigidArea(FixedDim.x3_y0));
    rootPanel.add(pnlButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x3_y0));
  }

  private void refreshVerticalPadding() {
    int size = rowList.size();
    int noPadding = 3;
    if (verticalRigidArea1 != null) {
      pnlTableAndCheckbox.remove(verticalRigidArea1);
      pnlTableAndCheckbox.remove(pnlFlgRemoveDuplicates);
    }
    int height = Math.max(0 + (noPadding - size) * 22, 0);
    verticalRigidArea1 = Box.createRigidArea(new Dimension(0, height));
    pnlTableAndCheckbox.add(verticalRigidArea1);
    pnlTableAndCheckbox.add(pnlFlgRemoveDuplicates);
  }

  private void display() {
    constraints.weighty = 0.0;
    // first header row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header1IterationNumber.add(pnlTable, layout, constraints);
    constraints.gridwidth = 6;
    header1DPhiDThetaDPsi.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header1SearchRadius.add(pnlTable, layout, constraints);
    constraints.gridwidth = 2;
    header1Cutoff.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header1RefThreshold.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1DuplicateTolerance.add(pnlTable, layout, constraints);

    // Second header row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header2IterationNumber.add(pnlTable, layout, constraints);
    header2DPhi.add(pnlTable, layout, constraints);
    header2DTheta.add(pnlTable, layout, constraints);
    header2DPsi.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header2SearchRadius.add(pnlTable, layout, constraints);
    constraints.gridwidth = 2;
    header2Cutoff.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header2RefThreshold.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2DuplicateTolerance.add(pnlTable, layout, constraints);

    // Third header row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header3IterationNumber.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header3DPhiMax.add(pnlTable, layout, constraints);
    header3DPhiIncrement.add(pnlTable, layout, constraints);
    header3DThetaMax.add(pnlTable, layout, constraints);
    header3DThetaIncrement.add(pnlTable, layout, constraints);
    header3DPsiMax.add(pnlTable, layout, constraints);
    header3DPsiIncrement.add(pnlTable, layout, constraints);
    header3SearchRadius.add(pnlTable, layout, constraints);
    header3HiCutoff.add(pnlTable, layout, constraints);
    header3LowCutoff.add(pnlTable, layout, constraints);
    header3RefThreshold.add(pnlTable, layout, constraints);
    header3DuplicateShiftTolerance.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header3DuplicateAngularTolerance.add(pnlTable, layout, constraints);
    rowList.display();
  }

  HeaderCell getDPhiDThetaDPsiHeaderCell() {
    return header1DPhiDThetaDPsi;
  }

  HeaderCell getSearchRadiusHeaderCell() {
    return header1SearchRadius;
  }

  HeaderCell getCutoffHeaderCell() {
    return header1Cutoff;
  }

  HeaderCell getRefThresholdHeaderCell() {
    return header1RefThreshold;
  }

  HeaderCell getDuplicateToleranceHeaderCell() {
    return header1DuplicateTolerance;
  }

  HeaderCell getIterationNumberHeaderCell() {
    return header1IterationNumber;
  }

  private class ITActionListener implements ActionListener {
    private final IterationTable iterationTable;

    private ITActionListener(final IterationTable iterationTable) {
      this.iterationTable = iterationTable;
    }

    public void actionPerformed(final ActionEvent event) {
      iterationTable.action(event);
    }
  }

  private static final class RowList {
    private final List<IterationRow> list = new ArrayList<IterationRow>();
    private final BaseManager manager;
    private final IterationTable table;

    private RowList(BaseManager manager, IterationTable table) {
      this.manager = manager;
      this.table = table;
    }

    private synchronized IterationRow add(final Highlightable parent, final JPanel panel,
        final GridBagLayout layout, final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow row = new IterationRow(index, parent, panel, layout, constraints,
          manager, table);
      row.setNames();
      list.add(row);
      return row;
    }

    private void getParameters(final MatlabParam matlabParamFile) {
      matlabParamFile.setIterationListSize(list.size());
      for (int i = 0; i < list.size(); i++) {
         list.get(i).getParameters(matlabParamFile);
      }
    }

    private synchronized int delete(IterationRow row, final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = row.getIndex();
      list.remove(index);
      for (int i = index; i < list.size(); i++) {
        getRow(i).setIndex(i);
      }
      return index;
    }

    private boolean validateRun() {
      if (list.size() < 1) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Must enter at least one row in "
            + LABEL, "Entry Error");
        return false;
      }
      for (int i = 0; i < list.size(); i++) {
        IterationRow row =  list.get(i);
        if (!row.validateRun()) {
          return false;
        }
      }
      return true;
    }

    /**
     * Update display in all rows.
     * @param sampleSphere
     * @param flgRemoveDuplicates
     */
    private void updateDisplay(final boolean sampleSphere,
        final boolean flgRemoveDuplicates) {
      for (int i = 0; i < list.size(); i++) {
        IterationRow row =  list.get(i);
        row.updateDisplay(sampleSphere, flgRemoveDuplicates);
      }
    }

    private void remove() {
      for (int i = 0; i < list.size(); i++) {
        getRow(i).remove();
      }
    }

    private synchronized void copy(IterationRow row, final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow copy = new IterationRow(index, row, manager, table);
      copy.setNames();
      list.add(copy);
      copy.display();
    }

    private int size() {
      return list.size();
    }

    private void display() {
      for (int i = 0; i < list.size(); i++) {
         list.get(i).display();
      }
    }

    private IterationRow getRow(int index) {
      if (index < 0 || index >= list.size()) {
        return null;
      }
      return  list.get(index);
    }

    private IterationRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        IterationRow row =  list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
      UIHarness.INSTANCE.openMessageDialog(manager, "Please highlight a row.",
          "Entry Error");
      return null;
    }

    private int getHighlightIndex() {
      for (int i = 0; i < list.size(); i++) {
        if ( list.get(i).isHighlighted()) {
          return i;
        }
      }
      return -1;
    }
    
    /**
     * Highlight the row in list at rowIndex.
     * @param rowIndex
     */
    private void highlight(final int rowIndex) {
      if (rowIndex >= 0 && rowIndex < list.size()) {
        list.get(rowIndex).setHighlighterSelected(true);
      }
    }

    /**
     * Swap two rows.
     * @param rowIndex
     */
    private void moveRowUp(final int rowIndex) {
      IterationRow rowMoveUp = list.remove(rowIndex);
      IterationRow rowMoveDown = list.remove(rowIndex - 1);
      list.add(rowIndex - 1, rowMoveUp);
      list.add(rowIndex, rowMoveDown);
    }

    /**
     * Swap two rows
     * @param rowIndex
     */
    private void moveRowDown(final int rowIndex) {
      IterationRow rowMoveUp = list.remove(rowIndex + 1);
      IterationRow rowMoveDown = list.remove(rowIndex);
      list.add(rowIndex, rowMoveUp);
      list.add(rowIndex + 1, rowMoveDown);
    }

    /**
     * Renumber the table starting from the row in the ArrayList at startIndex.
     * @param startIndex
     */
    private void reindex(final int startIndex) {
      for (int i = startIndex; i < list.size(); i++) {
         list.get(i).setIndex(i);
      }
    }
  }
}
