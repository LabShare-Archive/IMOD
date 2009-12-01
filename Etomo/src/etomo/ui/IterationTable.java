package etomo.ui;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.EtomoAutodoc;

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
  static final String D_PHI_HEADER2 = "Phi";
  static final String INCR_HEADER3 = "Incr.";
  static final String D_THETA_HEADER2 = "Theta";
  static final String D_PSI_HEADER2 = "Psi";
  static final String SEARCH_RADIUS_HEADER1 = "Search";
  static final String SEARCH_RADIUS_HEADER2 = "Radius";
  static final String LABEL = "Iteration Table";
  static final String MAX_HEADER3 = "Max";
  static final String CUTOFF_HEADER1 = "High-Freq.";
  static final String CUTOFF_HEADER2 = "Filter";
  static final String HI_CUTOFF_HEADER3 = "Cutoff";
  static final String LOW_CUTOFF_HEADER3 = "Sigma";
  static final String REF_THRESHOLD_HEADER1 = "Reference";
  static final String REF_THRESHOLD_HEADER2 = "Threshold";
  static final String DUPLICATE_TOLERANCE_HEADER1 = "Duplicate";
  static final String DUPLICATE_TOLERANCE_HEADER2 = "Tolerance";
  static final String DUPLICATE_SHIFT_TOLERANCE_HEADER3 = "Shift";
  static final String DUPLICATE_ANGULAR_TOLERANCE_HEADER3 = "Angular";

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
  private final HeaderCell header2DPhi = new HeaderCell(D_PHI_HEADER2);
  private final HeaderCell header2DTheta = new HeaderCell(D_THETA_HEADER2);
  private final HeaderCell header2DPsi = new HeaderCell(D_PSI_HEADER2);
  private final HeaderCell header3DPhiMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPhiIncrement = new HeaderCell(INCR_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaIncrement = new HeaderCell(
      INCR_HEADER3, UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiMax = new HeaderCell(MAX_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiIncrement = new HeaderCell(INCR_HEADER3,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1SearchRadius = new HeaderCell(
      SEARCH_RADIUS_HEADER1);
  private final HeaderCell header2SearchRadius = new HeaderCell(
      SEARCH_RADIUS_HEADER2);
  private final HeaderCell header3SearchRadius = new HeaderCell(
      UIParameters.INSTANCE.getIntegerTripletWidth());
  private final HeaderCell header1Cutoff = new HeaderCell(CUTOFF_HEADER1);
  private final HeaderCell header2Cutoff = new HeaderCell(CUTOFF_HEADER2);
  private final HeaderCell header3HiCutoff = new HeaderCell(HI_CUTOFF_HEADER3,
      UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header3LowCutoff = new HeaderCell(
      LOW_CUTOFF_HEADER3, UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header1RefThreshold = new HeaderCell(
      REF_THRESHOLD_HEADER1);
  private final HeaderCell header2RefThreshold = new HeaderCell(
      REF_THRESHOLD_HEADER2);
  private final HeaderCell header3RefThreshold = new HeaderCell();
  private final MultiLineButton btnAddRow = new MultiLineButton("Add Row");
  private final MultiLineButton btnCopyRow = new MultiLineButton("Copy Row");
  private final MultiLineButton btnDeleteRow = new MultiLineButton("Delete Row");
  private final HeaderCell header1DuplicateTolerance = new HeaderCell(
      DUPLICATE_TOLERANCE_HEADER1);
  private final HeaderCell header2DuplicateTolerance = new HeaderCell(
      DUPLICATE_TOLERANCE_HEADER2);
  private final HeaderCell header3DuplicateShiftTolerance = new HeaderCell(
      DUPLICATE_SHIFT_TOLERANCE_HEADER3);
  private final HeaderCell header3DuplicateAngularTolerance = new HeaderCell(
      DUPLICATE_ANGULAR_TOLERANCE_HEADER3);

  private final BaseManager manager;
  private final IterationParent parent;

  private IterationTable(BaseManager manager, IterationParent parent) {
    this.manager = manager;
    this.parent = parent;
    rowList = new RowList(manager, this);
    createTable();
    rowList.add(this, pnlTable, layout, constraints);
    display();
    updateDisplay();
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
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    rowList.getParameters(matlabParamFile);
  }

  /**
   * Update display in rows.
   * @param sampleSphere
   * @param flgRemoveDuplicates
   */
  void updateDisplay(final boolean sampleSphere,
      final boolean flgRemoveDuplicates) {
    rowList.updateDisplay(sampleSphere, flgRemoveDuplicates);
  }

  void setParameters(final MatlabParam matlabParamFile) {
    //overwrite existing rows
    int rowListSize = rowList.size();
    for (int i = 0; i < rowListSize; i++) {
      rowList.getRow(i).setParameters(matlabParamFile);
    }
    //add new rows
    for (int j = rowListSize; j < matlabParamFile.getIterationListSize(); j++) {
      IterationRow row = addRow();
      row.setParameters(matlabParamFile);
    }
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private IterationRow addRow() {
    IterationRow row = rowList.add(this, pnlTable, layout, constraints);
    row.display();
    parent.updateDisplay();
    return row;
  }

  private void setToolTipText() {
    String tooltip = "Order of iteration.";
    header1IterationNumber.setToolTipText(tooltip);
    header2IterationNumber.setToolTipText(tooltip);
    header3IterationNumber.setToolTipText(tooltip);
    btnAddRow.setToolTipText("Add a new row.");
    btnCopyRow
        .setToolTipText("Add a new row with values from the highlighted row.");
    btnDeleteRow.setToolTipText("Delete the highlighted row.");
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.D_PHI_KEY);
      String tooltip1 = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.D_THETA_KEY);
      if (tooltip1 == null) {
        tooltip1 = "";
      }
      String tooltip2 = EtomoAutodoc.getTooltip(autodoc, MatlabParam.D_PSI_KEY);
      if (tooltip2 == null) {
        tooltip2 = "";
      }
      header1DPhiDThetaDPsi.setToolTipText(tooltip + "  " + tooltip1 + "  "
          + tooltip2);
      header2DPhi.setToolTipText(tooltip);
      header2DTheta.setToolTipText(tooltip1);
      header2DPsi.setToolTipText(tooltip2);
      header3DPhiMax.setToolTipText(tooltip);
      header3DPhiIncrement.setToolTipText(tooltip);
      header3DThetaMax.setToolTipText(tooltip1);
      header3DThetaIncrement.setToolTipText(tooltip1);
      header3DPsiMax.setToolTipText(tooltip2);
      header3DPsiIncrement.setToolTipText(tooltip2);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.SEARCH_RADIUS_KEY);
      header1SearchRadius.setToolTipText(tooltip);
      header2SearchRadius.setToolTipText(tooltip);
      header3SearchRadius.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.HI_CUTOFF_KEY);
      header1Cutoff.setToolTipText(tooltip);
      header2Cutoff.setToolTipText(tooltip);
      header3HiCutoff.setToolTipText(tooltip);
      header3LowCutoff.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.REF_THRESHOLD_KEY);
      header1RefThreshold.setToolTipText(tooltip);
      header2RefThreshold.setToolTipText(tooltip);
      header3RefThreshold.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FLG_REMOVE_DUPLICATES_KEY);
      header1DuplicateTolerance.setToolTipText(tooltip);
      header2DuplicateTolerance.setToolTipText(tooltip);
      header3DuplicateShiftTolerance.setToolTipText(EtomoAutodoc.getTooltip(
          autodoc, MatlabParam.DUPLICATE_SHIFT_TOLERANCE_KEY));
      header3DuplicateAngularTolerance.setToolTipText(EtomoAutodoc.getTooltip(
          autodoc, MatlabParam.DUPLICATE_ANGULAR_TOLERANCE_KEY));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }

  private void addListeners() {
    ITActionListener actionListener = new ITActionListener(this);
    btnAddRow.addActionListener(actionListener);
    btnCopyRow.addActionListener(actionListener);
    btnDeleteRow.addActionListener(actionListener);
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
  }

  private void copyRow(IterationRow row) {
    rowList.copy(row, this, pnlTable, layout, constraints);
    UIHarness.INSTANCE.pack(manager);
  }

  private void deleteRow(IterationRow row) {
    rowList.remove();
    rowList.delete(row, this, pnlTable, layout, constraints);
    rowList.display();
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private void updateDisplay() {
    btnCopyRow.setEnabled(rowList.isHighlighted());
    btnDeleteRow.setEnabled(rowList.isHighlighted());
  }

  private void createTable() {
    //table
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    constraints.fill = GridBagConstraints.BOTH;
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.gridheight = 1;
    //buttons
    JPanel pnlButtons = new JPanel();
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    btnAddRow.setSize();
    pnlButtons.add(btnAddRow.getComponent());
    btnCopyRow.setSize();
    pnlButtons.add(btnCopyRow.getComponent());
    btnDeleteRow.setSize();
    pnlButtons.add(btnDeleteRow.getComponent());
    //border
    SpacedPanel pnlBorder = SpacedPanel.getInstance();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder(LABEL).getBorder());
    pnlBorder.add(pnlTable);
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons);
  }

  private void display() {
    constraints.weighty = 0.0;
    //first header row
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

    //Second header row
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

    //Third header row
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
    private final List list = new ArrayList();
    private final BaseManager manager;
    private final IterationTable table;

    private RowList(BaseManager manager, IterationTable table) {
      this.manager = manager;
      this.table = table;
    }

    private synchronized IterationRow add(final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow row = new IterationRow(index, parent, panel, layout,
          constraints, manager, table);
      row.setNames();
      list.add(row);
      return row;
    }

    private void getParameters(final MatlabParam matlabParamFile) {
      matlabParamFile.setIterationListSize(list.size());
      for (int i = 0; i < list.size(); i++) {
        ((IterationRow) list.get(i)).getParameters(matlabParamFile);
      }
    }

    private synchronized void delete(IterationRow row,
        final Highlightable parent, final JPanel panel,
        final GridBagLayout layout, final GridBagConstraints constraints) {
      int index = row.getIndex();
      list.remove(index);
      for (int i = index; i < list.size(); i++) {
        getRow(i).setIndex(i);
      }
    }

    private boolean validateRun() {
      if (list.size() < 1) {
        UIHarness.INSTANCE.openMessageDialog("Must enter at least one row in "
            + LABEL, "Entry Error", manager.getManagerKey());
        return false;
      }
      for (int i = 0; i < list.size(); i++) {
        IterationRow row = (IterationRow) list.get(i);
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
        IterationRow row = (IterationRow) list.get(i);
        row.updateDisplay(sampleSphere, flgRemoveDuplicates);
      }
    }

    private void remove() {
      for (int i = 0; i < list.size(); i++) {
        getRow(i).remove();
      }
    }

    private synchronized void copy(IterationRow row,
        final Highlightable parent, final JPanel panel,
        final GridBagLayout layout, final GridBagConstraints constraints) {
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
        ((IterationRow) list.get(i)).display();
      }
    }

    private IterationRow getRow(int index) {
      if (index < 0 || index >= list.size()) {
        return null;
      }
      return (IterationRow) list.get(index);
    }

    private IterationRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        IterationRow row = (IterationRow) list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error", manager.getManagerKey());
      return null;
    }

    private boolean isHighlighted() {
      for (int i = 0; i < list.size(); i++) {
        if (((IterationRow) list.get(i)).isHighlighted()) {
          return true;
        }
      }
      return false;
    }
  }
}
