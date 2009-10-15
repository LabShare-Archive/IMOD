package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;

import etomo.BaseManager;
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
 * <p> Revision 1.18  2009/09/28 18:35:10  sueh
 * <p> bug# 1235 Added setNames.
 * <p>
 * <p> Revision 1.17  2009/09/23 19:54:26  sueh
 * <p> bug# 1271 Made search min search radius = 0 the standard.
 * <p>
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
  private final FieldCell hiCutoff = FieldCell.getEditableMatlabInstance();
  private final FieldCell lowCutoff = FieldCell.getEditableMatlabInstance();
  private final FieldCell refThreshold = FieldCell.getEditableMatlabInstance();

  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final HighlighterButton btnHighlighter;
  private final Highlightable parent;
  private final BaseManager manager;
  private final IterationTable table;

  private int index;

  IterationRow(final int index, final Highlightable parent, final JPanel panel,
      final GridBagLayout layout, final GridBagConstraints constraints,
      BaseManager manager, IterationTable table) {
    this.index = index;
    this.parent = parent;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.manager = manager;
    this.table = table;
    btnHighlighter = HighlighterButton.getInstance(this, parent);
    number.setText(String.valueOf(index + 1));
  }

  IterationRow(final int index, final IterationRow iterationRow,
      BaseManager manager, IterationTable table) {
    this.index = index;
    this.parent = iterationRow.parent;
    this.panel = iterationRow.panel;
    this.layout = iterationRow.layout;
    this.constraints = iterationRow.constraints;
    this.manager = manager;
    this.table = table;
    btnHighlighter = HighlighterButton.getInstance(this, parent);
    number.setText(String.valueOf(index + 1));
    dPhiMax.setValue(iterationRow.dPhiMax.getValue());
    dPhiIncrement.setValue(iterationRow.dPhiIncrement.getValue());
    dThetaMax.setValue(iterationRow.dThetaMax.getValue());
    dThetaIncrement.setValue(iterationRow.dThetaIncrement.getValue());
    dPsiMax.setValue(iterationRow.dPsiMax.getValue());
    dPsiIncrement.setValue(iterationRow.dPsiIncrement.getValue());
    searchRadius.setValue(iterationRow.searchRadius.getValue());
    hiCutoff.setValue(iterationRow.hiCutoff.getValue());
    lowCutoff.setValue(iterationRow.lowCutoff.getValue());
    refThreshold.setValue(iterationRow.refThreshold.getValue());
  }

  void setNames() {
    btnHighlighter.setHeaders(IterationTable.LABEL, number, table
        .getIterationNumberHeaderCell());
    dPhiMax.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    dPhiIncrement.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    dThetaMax.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    dThetaIncrement.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    dPsiMax.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    dPsiIncrement.setHeaders(IterationTable.LABEL, number, table
        .getDPhiDThetaDPsiHeaderCell());
    searchRadius.setHeaders(IterationTable.LABEL, number, table
        .getSearchRadiusHeaderCell());
    hiCutoff.setHeaders(IterationTable.LABEL, number, table
        .getCutoffHeaderCell());
    lowCutoff.setHeaders(IterationTable.LABEL, number, table
        .getCutoffHeaderCell());
    refThreshold.setHeaders(IterationTable.LABEL, number, table
        .getRefThresholdHeaderCell());
  }

  public void highlight(final boolean highlight) {
    dPhiMax.setHighlight(highlight);
    dPhiIncrement.setHighlight(highlight);
    dThetaMax.setHighlight(highlight);
    dThetaIncrement.setHighlight(highlight);
    dPsiMax.setHighlight(highlight);
    dPsiIncrement.setHighlight(highlight);
    searchRadius.setHighlight(highlight);
    hiCutoff.setHighlight(highlight);
    lowCutoff.setHighlight(highlight);
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
    iteration.setHiCutoffCutoff(hiCutoff.getValue());
    iteration.setHiCutoffSigma(lowCutoff.getValue());
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
    hiCutoff.setValue(iteration.getHiCutoffCutoff());
    lowCutoff.setValue(iteration.getHiCutoffSigma());
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
    hiCutoff.remove();
    lowCutoff.remove();
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
    hiCutoff.add(panel, layout, constraints);
    lowCutoff.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    refThreshold.add(panel, layout, constraints);
  }

  private String buildHeaderDescription(String[] headerArray) {
    StringBuffer header = new StringBuffer();
    if (headerArray != null) {
      for (int i = 0; i < headerArray.length; i++) {
        header.append(", " + headerArray[i]);
      }
    }
    return header.toString();
  }

  /**
   * Validates empty and n.  Empty must always be false.  If n is not null, it
   * must be valid and not negative.
   * @param n
   * @return
   */
  private boolean validateRun(boolean empty, EtomoNumber n,
      String[] headerArray, String additionalEmptyErrorMessage) {
    if (empty) {
      UIHarness.INSTANCE.openMessageDialog(IterationTable.LABEL
          + ":  In row "
          + number.toString()
          + buildHeaderDescription(headerArray)
          + " must not be empty."
          + (additionalEmptyErrorMessage == null ? ""
              : additionalEmptyErrorMessage), "Entry Error", manager
          .getManagerKey());
      return false;
    }
    if (n != null) {
      if (!n.isValid()) {
        UIHarness.INSTANCE.openMessageDialog(IterationTable.LABEL
            + ":  In row " + number.toString()
            + buildHeaderDescription(headerArray) + " must be numeric.",
            "Entry Error", manager.getManagerKey());
        return false;
      }
      if (n.isNegative()) {
        UIHarness.INSTANCE.openMessageDialog(IterationTable.LABEL
            + ":  In row " + number.getText()
            + buildHeaderDescription(headerArray) + " must not be negative.",
            "Entry Error", manager.getManagerKey());
        return false;
      }
    }
    return true;
  }

  boolean validateRun() {
    //Phi
    EtomoNumber n = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    n.set(dPhiMax.getValue());
    if (!validateRun(dPhiMax.isEmpty(), n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_PHI_HEADER2, IterationTable.MAX_HEADER3 },
        "Use 0 to not search on the angle.")) {
      return false;
    }
    n.set(dPhiIncrement.getValue());
    if (!validateRun(dPhiIncrement.isEmpty(), n, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
        IterationTable.D_PHI_HEADER2, IterationTable.INCR_HEADER3 }, null)) {
      return false;
    }
    //Theta
    if (dThetaMax.isEnabled()) {
      n.set(dThetaMax.getValue());
      if (!validateRun(dThetaMax.isEmpty(), n, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
          IterationTable.D_THETA_HEADER2, IterationTable.MAX_HEADER3 },
          "Use 0 to not search on the angle.")) {
        return false;
      }
    }
    if (dThetaIncrement.isEnabled()) {
      n.set(dThetaIncrement.getValue());
      if (!validateRun(dThetaIncrement.isEmpty(), n, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
          IterationTable.D_THETA_HEADER2, IterationTable.INCR_HEADER3 }, null)) {
        return false;
      }
    }
    //Psi
    if (dPsiMax.isEnabled()) {
      n.set(dPsiMax.getValue());
      if (!validateRun(dPsiMax.isEmpty(), n, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
          IterationTable.D_PSI_HEADER2, IterationTable.MAX_HEADER3 },
          "Use 0 to not search on the angle.")) {
        return false;
      }
    }
    if (dPsiIncrement.isEnabled()) {
      n.set(dPsiIncrement.getValue());
      if (!validateRun(dPsiIncrement.isEmpty(), n, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1,
          IterationTable.D_PSI_HEADER2, IterationTable.INCR_HEADER3 }, null)) {
        return false;
      }
    }
    //search radius
    n.set(searchRadius.getValue());
    if (!validateRun(searchRadius.isEmpty(), n, new String[] {
        IterationTable.SEARCH_RADIUS_HEADER1,
        IterationTable.SEARCH_RADIUS_HEADER2 }, null)) {
      return false;
    }
    //hiCutoff
    if (!validateRun(hiCutoff.isEmpty(), null, new String[] {
        IterationTable.CUTOFF_HEADER1, IterationTable.CUTOFF_HEADER2,
        IterationTable.HI_CUTOFF_HEADER3 }, null)) {
      return false;
    }
    //lowCutoff
    if (!validateRun(lowCutoff.isEmpty(), null, new String[] {
        IterationTable.CUTOFF_HEADER1, IterationTable.CUTOFF_HEADER2,
        IterationTable.LOW_CUTOFF_HEADER3 }, null)) {
      return false;
    }
    //refThreshold
    if (!validateRun(refThreshold.isEmpty(), null, new String[] {
        IterationTable.REF_THRESHOLD_HEADER1,
        IterationTable.REF_THRESHOLD_HEADER2 }, null)) {
      return false;
    }
    return true;
  }
}
