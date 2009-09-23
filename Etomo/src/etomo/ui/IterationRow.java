package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;

import etomo.ManagerKey;
import etomo.storage.MatlabParam;
import etomo.type.EtomoNumber;

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
 * <p> Revision 1.16  2009/09/23 17:09:07  sueh
 * <p> Set minium search radius to 0 for John.  Functionality is only available when --newstuff is used.
 * <p>
 * <p> Revision 1.15  2009/04/20 16:37:34  sueh
 * <p> bug# 1214 Make sure that phi, theta, and psi aren't negative.
 * <p>
 * <p> Revision 1.14  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2008/08/21 00:08:53  sueh
 * <p> bug# 1132 In validateRun fixed phi, theta, and psi validation so that it
 * <p> only fails on an empty Max.
 * <p>
 * <p> Revision 1.12  2008/04/02 17:34:54  sueh
 * <p> bug# 1098 Improved user error messages.
 * <p>
 * <p> Revision 1.11  2008/04/02 02:26:46  sueh
 * <p> bug# 1097 FieldCell can now return a matlab syntax instance.
 * <p>
 * <p> Revision 1.10  2008/03/06 00:27:40  sueh
 * <p> bug# 1088 Added updateDisplay.
 * <p>
 * <p> Revision 1.9  2007/07/31 20:45:03  sueh
 * <p> bug# 1028 In validateRun() checking all the values in searchRadius.
 * <p>
 * <p> Revision 1.8  2007/07/18 23:22:05  sueh
 * <p> bug# 1022 In validateRun using getting the numbers with etomo number
 * <p> so that lists can be handled as nulls.
 * <p>
 * <p> Revision 1.7  2007/07/10 00:38:03  sueh
 * <p> bug# 1022 added validateRun.
 * <p>
 * <p> Revision 1.6  2007/05/15 19:56:50  sueh
 * <p> bug# 964 Added getIndex(),setIndex(int), and remove().
 * <p>
 * <p> Revision 1.5  2007/05/07 17:21:53  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.
 * <p>
 * <p> Revision 1.4  2007/04/26 02:49:06  sueh
 * <p> bug# 964 Changed dPhiEnd to dPhiMax.  Did the same for dTheta and dPsi.
 * <p>
 * <p> Revision 1.3  2007/04/19 21:59:32  sueh
 * <p> bug# 964 Added getParameters(MatlabParamFile) and setParameters(MallabParamFile).
 * <p>
 * <p> Revision 1.2  2007/04/02 21:50:05  sueh
 * <p> bug# 964 Added FieldCell.editable to make instances of FieldCell that can't be
 * <p> edited.  This allows FieldCell.setEditable and setEnabled to be called without
 * <p> checking whether a field should be editable.
 * <p>
 * <p> Revision 1.1  2007/04/02 16:02:49  sueh
 * <p> bug# 964 A row of the iteration table, which contain per iteration PEET data.
 * <p> </p>
 */
final class IterationRow implements Highlightable {
  public static final String rcsid = "$Id$";

  private final HeaderCell number = new HeaderCell();
  private final FieldCell dPhiMax = FieldCell.getEditableMatlabInstance();
  private final FieldCell dPhiIncrement = FieldCell.getEditableMatlabInstance();
  private final FieldCell dThetaMax = FieldCell.getEditableMatlabInstance();
  private final FieldCell dThetaIncrement = FieldCell
      .getEditableMatlabInstance();
  private final FieldCell dPsiMax = FieldCell.getEditableMatlabInstance();
  private final FieldCell dPsiIncrement = FieldCell.getEditableMatlabInstance();
  private final FieldCell searchRadius = FieldCell.getEditableMatlabInstance();
  private final FieldCell hiCutoffCutoff = FieldCell
      .getEditableMatlabInstance();
  private final FieldCell hiCutoffSigma = FieldCell.getEditableMatlabInstance();
  private final FieldCell refThreshold = FieldCell.getEditableMatlabInstance();

  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final HighlighterButton btnHighlighter;
  private final Highlightable parent;
  private final ManagerKey managerKey;

  private int index;

  IterationRow(final int index, final Highlightable parent, final JPanel panel,
      final GridBagLayout layout, final GridBagConstraints constraints,
      ManagerKey managerKey) {
    this.index = index;
    this.parent = parent;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.managerKey = managerKey;
    btnHighlighter = HighlighterButton.getInstance(this, parent);
    number.setText(String.valueOf(index + 1));
  }

  IterationRow(final int index, final IterationRow iterationRow,
      ManagerKey managerKey) {
    this.index = index;
    this.parent = iterationRow.parent;
    this.panel = iterationRow.panel;
    this.layout = iterationRow.layout;
    this.constraints = iterationRow.constraints;
    this.managerKey = managerKey;
    btnHighlighter = HighlighterButton.getInstance(this, parent);
    number.setText(String.valueOf(index + 1));
    dPhiMax.setValue(iterationRow.dPhiMax.getValue());
    dPhiIncrement.setValue(iterationRow.dPhiIncrement.getValue());
    dThetaMax.setValue(iterationRow.dThetaMax.getValue());
    dThetaIncrement.setValue(iterationRow.dThetaIncrement.getValue());
    dPsiMax.setValue(iterationRow.dPsiMax.getValue());
    dPsiIncrement.setValue(iterationRow.dPsiIncrement.getValue());
    searchRadius.setValue(iterationRow.searchRadius.getValue());
    hiCutoffCutoff.setValue(iterationRow.hiCutoffCutoff.getValue());
    hiCutoffSigma.setValue(iterationRow.hiCutoffSigma.getValue());
    refThreshold.setValue(iterationRow.refThreshold.getValue());
  }

  public void highlight(final boolean highlight) {
    dPhiMax.setHighlight(highlight);
    dPhiIncrement.setHighlight(highlight);
    dThetaMax.setHighlight(highlight);
    dThetaIncrement.setHighlight(highlight);
    dPsiMax.setHighlight(highlight);
    dPsiIncrement.setHighlight(highlight);
    searchRadius.setHighlight(highlight);
    hiCutoffCutoff.setHighlight(highlight);
    hiCutoffSigma.setHighlight(highlight);
    refThreshold.setHighlight(highlight);
  }

  /**
   * Turn off theta and psi when sampleSphere is on.
   * @param sampleSphere
   */
  void updateDisplay(boolean sampleSphere) {
    dThetaMax.setEnabled(!sampleSphere);
    dThetaIncrement.setEnabled(!sampleSphere);
    dPsiMax.setEnabled(!sampleSphere);
    dPsiIncrement.setEnabled(!sampleSphere);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    MatlabParam.Iteration iteration = matlabParamFile.getIteration(index);
    iteration.setDPhiEnd(dPhiMax.getValue());
    iteration.setDPhiIncrement(dPhiIncrement.getValue());
    iteration.setDThetaEnd(dThetaMax.getValue());
    iteration.setDThetaIncrement(dThetaIncrement.getValue());
    iteration.setDPsiEnd(dPsiMax.getValue());
    iteration.setDPsiIncrement(dPsiIncrement.getValue());
    iteration.setSearchRadius(searchRadius.getValue());
    iteration.setHiCutoffCutoff(hiCutoffCutoff.getValue());
    iteration.setHiCutoffSigma(hiCutoffSigma.getValue());
    iteration.setRefThreshold(refThreshold.getValue());
  }

  void setParameters(final MatlabParam matlabParamFile) {
    MatlabParam.Iteration iteration = matlabParamFile.getIteration(index);
    dPhiMax.setValue(iteration.getDPhiEnd());
    dPhiIncrement.setValue(iteration.getDPhiIncrement());
    dThetaMax.setValue(iteration.getDThetaEnd());
    dThetaIncrement.setValue(iteration.getDThetaIncrement());
    dPsiMax.setValue(iteration.getDPsiEnd());
    dPsiIncrement.setValue(iteration.getDPsiIncrement());
    searchRadius.setValue(iteration.getSearchRadiusString());
    hiCutoffCutoff.setValue(iteration.getHiCutoffCutoff());
    hiCutoffSigma.setValue(iteration.getHiCutoffSigma());
    refThreshold.setValue(iteration.getRefThresholdString());
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }

  int getIndex() {
    return index;
  }

  void setIndex(int index) {
    this.index = index;
    number.setText(String.valueOf(index + 1));
  }

  void remove() {
    number.remove();
    btnHighlighter.remove();
    dPhiMax.remove();
    dPhiIncrement.remove();
    dThetaMax.remove();
    dThetaIncrement.remove();
    dPsiMax.remove();
    dPsiIncrement.remove();
    searchRadius.remove();
    hiCutoffCutoff.remove();
    hiCutoffSigma.remove();
    refThreshold.remove();
  }

  void display() {
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    number.add(panel, layout, constraints);
    btnHighlighter.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    dPhiMax.add(panel, layout, constraints);
    dPhiIncrement.add(panel, layout, constraints);
    dThetaMax.add(panel, layout, constraints);
    dThetaIncrement.add(panel, layout, constraints);
    dPsiMax.add(panel, layout, constraints);
    dPsiIncrement.add(panel, layout, constraints);
    searchRadius.add(panel, layout, constraints);
    hiCutoffCutoff.add(panel, layout, constraints);
    hiCutoffSigma.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    refThreshold.add(panel, layout, constraints);
  }

  /**
   * Validates n.  Assumes that number is not null.
   * @param n
   * @return
   */
  private boolean validatePositiveNumber(EtomoNumber n, String[] headerArray) {
    StringBuffer header = new StringBuffer();
    if (headerArray != null) {
      for (int i = 0; i < headerArray.length; i++) {
        header.append(", " + headerArray[i]);
      }
    }
    if (!n.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.TABLE_HEADER
          + ":  In row " + number.toString() + header + " must be numeric.",
          "Entry Error", managerKey);
      return false;
    }
    if (n.isNegative()) {
      UIHarness.INSTANCE.openMessageDialog(
          IterationTable.TABLE_HEADER + ":  In row " + number.getText()
              + header + " must not be negative.", "Entry Error", managerKey);
      return false;
    }
    return true;
  }

  boolean validateRun() {
    if (dPhiMax.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.TABLE_HEADER
          + ":  In row " + number.getText() + ", "
          + IterationTable.D_PHI_D_THETA_D_PSI_HEADER1 + " "
          + IterationTable.D_PHI_HEADER2 + " " + IterationTable.MAX_HEADER
          + " must not be empty.  Use 0 to not search on the angle.",
          "Entry Error", managerKey);
      return false;
    }
    EtomoNumber n = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    n.set(dPhiMax.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_PHI_HEADER2, IterationTable.MAX_HEADER })) {
      return false;
    }

    n.set(dPhiIncrement.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_PHI_HEADER2, IterationTable.INCR_HEADER })) {
      return false;
    }

    if (dThetaMax.isEnabled() && dThetaMax.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.TABLE_HEADER
          + ":  In row " + number.getText() + ", "
          + IterationTable.D_PHI_D_THETA_D_PSI_HEADER1 + " "
          + IterationTable.D_THETA_HEADER2 + " " + IterationTable.MAX_HEADER
          + " must not be empty.  Use 0 to not search on the angle.",
          "Entry Error", managerKey);
      return false;
    }
    n.set(dThetaMax.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_THETA_HEADER2, IterationTable.MAX_HEADER })) {
      return false;
    }

    n.set(dThetaIncrement.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_THETA_HEADER2, IterationTable.INCR_HEADER })) {
      return false;
    }

    if (dPsiMax.isEnabled() && dPsiMax.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.TABLE_HEADER
          + ":  In row " + number.getText() + ", "
          + IterationTable.D_PHI_D_THETA_D_PSI_HEADER1 + " "
          + IterationTable.D_PSI_HEADER2 + " " + IterationTable.MAX_HEADER
          + " must not be empty.  Use 0 to not search on the angle.",
          "Entry Error", managerKey);
      return false;
    }
    n.set(dPsiMax.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_PSI_HEADER2, IterationTable.MAX_HEADER })) {
      return false;
    }

    n.set(dPsiIncrement.getValue());
    if (!validatePositiveNumber(n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_THETA_HEADER2, IterationTable.INCR_HEADER })) {
      return false;
    }

    int minSearchRadius = 0;
    if (!searchRadius.getParsedArray().ge(minSearchRadius)) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.TABLE_HEADER
          + ":  In row " + number.getText() + ", "
          + IterationTable.SEARCH_RADIUS_HEADER1 + " "
          + IterationTable.SEARCH_RADIUS_HEADER2 + " must not be less then "
          + minSearchRadius + ".", "Entry Error", managerKey);
      return false;
    }
    return true;
  }
}
