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
import etomo.storage.MatlabParamFile;
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
 * <p> Revision 1.1  2007/04/02 16:03:05  sueh
 * <p> bug# 964 Contains per iteration PEET data.
 * <p> </p>
 */
final class IterationTable implements Highlightable {
  public static final String rcsid = "$Id$";

  private final JPanel rootPanel = new JPanel();
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final RowList rowList = new RowList();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final HeaderCell header1IterationNumber = new HeaderCell("Run #");
  private final HeaderCell header2IterationNumber = new HeaderCell();
  private final HeaderCell header3IterationNumber = new HeaderCell();
  private final HeaderCell header1DPhiDThetaDPsi = new HeaderCell(
      "Angular Search Range");
  private final HeaderCell header2DPhi = new HeaderCell("Phi");
  private final HeaderCell header2DTheta = new HeaderCell("Theta");
  private final HeaderCell header2DPsi = new HeaderCell("Psi");
  private final HeaderCell header3DPhiStart = new HeaderCell("Max",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPhiIncrement = new HeaderCell("Incr.",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaStart = new HeaderCell("Max",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DThetaIncrement = new HeaderCell("Incr.",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiStart = new HeaderCell("Max",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3DPsiIncrement = new HeaderCell("Incr.",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1SearchRadius = new HeaderCell("Search");
  private final HeaderCell header2SearchRadius = new HeaderCell("Radius");
  private final HeaderCell header3SearchRadius = new HeaderCell(
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1HiCutoff = new HeaderCell("High-Freq.");
  private final HeaderCell header2HiCutoff = new HeaderCell("Filter");
  private final HeaderCell header3HiCutoff = new HeaderCell("Cutoff",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header3HiCutoffSigma = new HeaderCell("Sigma",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header1RefThreshold = new HeaderCell("Reference");
  private final HeaderCell header2RefThreshold = new HeaderCell("Threshold");
  private final HeaderCell header3RefThreshold = new HeaderCell();
  private final MultiLineButton btnCopyRow = new MultiLineButton("Copy Row");
  private final BaseManager manager;

  private IterationTable(BaseManager manager) {
    this.manager = manager;
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

  Container getContainer() {
    return rootPanel;
  }

  private void setToolTipText() {
    String tooltip = "Order of iteration.";
    header1IterationNumber.setToolTipText(tooltip);
    header2IterationNumber.setToolTipText(tooltip);
    header3IterationNumber.setToolTipText(tooltip);
    tooltip = "Add a new copy with values from the highlighted row.";
    btnCopyRow.setToolTipText(tooltip);
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory
          .getInstance(AutodocFactory.PEET_PRM);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.D_PHI_KEY);
      String tooltip1 = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.D_THETA_KEY);
      if (tooltip1 == null) {
        tooltip1 = "";
      }
      String tooltip2 = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.D_PSI_KEY);
      if (tooltip2 == null) {
        tooltip2 = "";
      }
      header1DPhiDThetaDPsi.setToolTipText(tooltip + "  " + tooltip1 + "  "
          + tooltip2);
      header2DPhi.setToolTipText(tooltip);
      header2DTheta.setToolTipText(tooltip1);
      header2DPsi.setToolTipText(tooltip2);
      header3DPhiStart.setToolTipText(tooltip);
      header3DPhiIncrement.setToolTipText(tooltip);
      header3DThetaStart.setToolTipText(tooltip1);
      header3DThetaIncrement.setToolTipText(tooltip1);
      header3DPsiStart.setToolTipText(tooltip2);
      header3DPsiIncrement.setToolTipText(tooltip2);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.SEARCH_RADIUS_KEY);
      header1SearchRadius.setToolTipText(tooltip);
      header2SearchRadius.setToolTipText(tooltip);
      header3SearchRadius.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.HI_CUTOFF_KEY);
      header1HiCutoff.setToolTipText(tooltip);
      header2HiCutoff.setToolTipText(tooltip);
      header3HiCutoff.setToolTipText(tooltip);
      header3HiCutoffSigma.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.REF_THRESHOLD_KEY);
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
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
  }

  private void addListeners() {
    btnCopyRow.addActionListener(new ITActionListener(this));
  }

  private void action(final ActionEvent event) {
    if (event.getActionCommand().equals(btnCopyRow.getActionCommand())) {
      copyRow(rowList.getHighlightedRow());
    }
  }

  private void copyRow(IterationRow row) {
    rowList.copy(row, this, pnlTable, layout, constraints);
    UIHarness.INSTANCE.pack(manager);
  }

  private void updateDisplay() {
    btnCopyRow.setEnabled(rowList.isHighlighted());
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
    btnCopyRow.setSize();
    pnlButtons.add(btnCopyRow.getComponent());
    //border
    SpacedPanel pnlBorder = new SpacedPanel();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder("Iteration Table").getBorder());
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
    header3DPhiStart.add(pnlTable, layout, constraints);
    header3DPhiIncrement.add(pnlTable, layout, constraints);
    header3DThetaStart.add(pnlTable, layout, constraints);
    header3DThetaIncrement.add(pnlTable, layout, constraints);
    header3DPsiStart.add(pnlTable, layout, constraints);
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

    private synchronized void add(final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow row = new IterationRow(index, parent, panel, layout,
          constraints);
      list.add(row);
    }

    private synchronized void copy(IterationRow row,
        final Highlightable parent, final JPanel panel,
        final GridBagLayout layout, final GridBagConstraints constraints) {
      int index = list.size();
      IterationRow copy = new IterationRow(index, row);
      list.add(copy);
      copy.display();
    }

    private void display() {
      for (int i = 0; i < list.size(); i++) {
        ((IterationRow) list.get(i)).display();
      }
    }

    private IterationRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        IterationRow row = (IterationRow) list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
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
