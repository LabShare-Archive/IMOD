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
import etomo.ManagerKey;
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
  static final String INCR_HEADER = "Incr.";
  static final String D_THETA_HEADER2 = "Theta";
  static final String D_PSI_HEADER2 = "Psi";
  static final String SEARCH_RADIUS_HEADER1 = "Search";
  static final String SEARCH_RADIUS_HEADER2 = "Radius";
  static final String TABLE_HEADER = "Iteration Table";
  static final String MAX_HEADER = "Max";

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
  private final HeaderCell header3DPhiMax = new HeaderCell(MAX_HEADER,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPhiIncrement = new HeaderCell(INCR_HEADER,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaMax = new HeaderCell(MAX_HEADER,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaIncrement = new HeaderCell(
      INCR_HEADER, UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiMax = new HeaderCell(MAX_HEADER,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiIncrement = new HeaderCell(INCR_HEADER,
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1SearchRadius = new HeaderCell(
      SEARCH_RADIUS_HEADER1);
  private final HeaderCell header2SearchRadius = new HeaderCell(
      SEARCH_RADIUS_HEADER2);
  private final HeaderCell header3SearchRadius = new HeaderCell(
      UIParameters.INSTANCE.getIntegerTripletWidth());
  private final HeaderCell header1HiCutoff = new HeaderCell("High-Freq.");
  private final HeaderCell header2HiCutoff = new HeaderCell("Filter");
  private final HeaderCell header3HiCutoff = new HeaderCell("Cutoff",
      UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header3HiCutoffSigma = new HeaderCell("Sigma",
      UIParameters.INSTANCE.getWideNumericWidth());
  private final HeaderCell header1RefThreshold = new HeaderCell("Reference");
  private final HeaderCell header2RefThreshold = new HeaderCell("Threshold");
  private final HeaderCell header3RefThreshold = new HeaderCell();
  private final MultiLineButton btnAddRow = new MultiLineButton("Add Row");
  private final MultiLineButton btnCopyRow = new MultiLineButton("Copy Row");
  private final MultiLineButton btnDeleteRow = new MultiLineButton("Delete Row");
  private final BaseManager manager;

  private IterationTable(BaseManager manager) {
    this.manager = manager;
    rowList = new RowList(manager.getManagerKey());
    createTable();
    rowList.add(this, pnlTable, layout, constraints);
    display();
    updateDisplay();
    setToolTipText();
  }

  static IterationTable getInstance(BaseManager manager) {
    IterationTable instance = new IterationTable(manager);
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
   * SampleSphere effects the first row only.
   * @param sampleSphere
   */
  void updateDisplay(final boolean sampleSphere) {
    IterationRow row = rowList.getRow(0);
    if (row == null) {
      return;
    }
    row.updateDisplay(sampleSphere);
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
      header1HiCutoff.setToolTipText(tooltip);
      header2HiCutoff.setToolTipText(tooltip);
      header3HiCutoff.setToolTipText(tooltip);
      header3HiCutoffSigma.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.REF_THRESHOLD_KEY);
      header1RefThreshold.setToolTipText(tooltip);
      header2RefThreshold.setToolTipText(tooltip);
      header3RefThreshold.setToolTipText(tooltip);
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
    pnlBorder.setBorder(new EtchedBorder(TABLE_HEADER).getBorder());
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
    header1HiCutoff.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1RefThreshold.add(pnlTable, layout, constraints);
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
    header2HiCutoff.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2RefThreshold.add(pnlTable, layout, constraints);
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
    header3HiCutoffSigma.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header3RefThreshold.add(pnlTable, layout, constraints);
    rowList.display();
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
    private final ManagerKey managerKey;

    private RowList(ManagerKey managerKey) {
      this.managerKey = managerKey;
    }

    private synchronized IterationRow add(final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow row = new IterationRow(index, parent, panel, layout,
          constraints, managerKey);
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
      for (int i = 0; i < list.size(); i++) {
        IterationRow row = (IterationRow) list.get(i);
        if (!row.validateRun()) {
          return false;
        }
      }
      return true;
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
      IterationRow copy = new IterationRow(index, row, managerKey);
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
          "Entry Error", managerKey);
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
