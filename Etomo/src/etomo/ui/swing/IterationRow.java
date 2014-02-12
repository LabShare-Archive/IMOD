package etomo.ui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
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
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.24  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.23  2010/01/22 03:43:11  sueh
 * <p> bug# 1307 Allow searchRadius to be either one number or an array of 3.  Check each number to make sure its not negative.
 * <p>
 * <p> Revision 1.22  2009/12/23 02:24:15  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.  Added tooltips to the actual fields in the tables instead of the column headers.
 * <p>
 * <p> Revision 1.21  2009/12/01 15:21:03  sueh
 * <p> bug# 1282 In validateRun() made the EtomoNumber type match the type
 * <p> of the number being validated.
 * <p>
 * <p> Revision 1.20  2009/11/20 17:15:12  sueh
 * <p> bug# 1282 Added duplicateShiftTolerance and duplicateAngularTolerance.
 * <p> When updating the display, moved the responsibility for responding to
 * <p> sample sphere in the first row only to this class.
 * <p>
 * <p> Revision 1.19  2009/10/15 23:35:41  sueh
 * <p> bug# 1274 Made header names avaible becauese of increased error
 * <p> checking in the row.  Changed hiCutoffCutoff to hiCutoff.  Change
 * <p> hiCutoffSigma to lowCutoff.  Passing manager rather then the
 * <p> managerKey, which can change, to rows.
 * <p>
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
  private final FieldCell dThetaIncrement = FieldCell.getEditableMatlabInstance();
  private final FieldCell dPsiMax = FieldCell.getEditableMatlabInstance();
  private final FieldCell dPsiIncrement = FieldCell.getEditableMatlabInstance();
  private final FieldCell searchRadius = FieldCell.getEditableMatlabInstance();
  private final FieldCell hiCutoff = FieldCell.getEditableMatlabInstance();
  private final FieldCell lowCutoff = FieldCell.getEditableMatlabInstance();
  private final FieldCell refThreshold = FieldCell.getEditableMatlabInstance();
  private final FieldCell duplicateShiftTolerance = FieldCell.getEditableMatlabInstance();
  private final FieldCell duplicateAngularTolerance = FieldCell
      .getEditableMatlabInstance();

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
    setTooltips();
  }

  IterationRow(final int index, final IterationRow iterationRow, BaseManager manager,
      IterationTable table) {
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
    duplicateShiftTolerance.setValue(iterationRow.duplicateShiftTolerance.getValue());
    duplicateAngularTolerance.setValue(iterationRow.duplicateAngularTolerance.getValue());
    setTooltips();
  }

  void setNames() {
    btnHighlighter.setHeaders(IterationTable.LABEL, number,
        table.getIterationNumberHeaderCell());
    dPhiMax.setHeaders(IterationTable.LABEL, number, table.getDPhiDThetaDPsiHeaderCell());
    dPhiIncrement.setHeaders(IterationTable.LABEL, number,
        table.getDPhiDThetaDPsiHeaderCell());
    dThetaMax.setHeaders(IterationTable.LABEL, number,
        table.getDPhiDThetaDPsiHeaderCell());
    dThetaIncrement.setHeaders(IterationTable.LABEL, number,
        table.getDPhiDThetaDPsiHeaderCell());
    dPsiMax.setHeaders(IterationTable.LABEL, number, table.getDPhiDThetaDPsiHeaderCell());
    dPsiIncrement.setHeaders(IterationTable.LABEL, number,
        table.getDPhiDThetaDPsiHeaderCell());
    searchRadius.setHeaders(IterationTable.LABEL, number,
        table.getSearchRadiusHeaderCell());
    hiCutoff.setHeaders(IterationTable.LABEL, number, table.getCutoffHeaderCell());
    lowCutoff.setHeaders(IterationTable.LABEL, number, table.getCutoffHeaderCell());
    refThreshold.setHeaders(IterationTable.LABEL, number,
        table.getRefThresholdHeaderCell());
    duplicateShiftTolerance.setHeaders(IterationTable.LABEL, number,
        table.getDuplicateToleranceHeaderCell());
    duplicateAngularTolerance.setHeaders(IterationTable.LABEL, number,
        table.getDuplicateToleranceHeaderCell());
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
    duplicateShiftTolerance.setHighlight(highlight);
    duplicateAngularTolerance.setHighlight(highlight);
  }

  void setHighlighterSelected(final boolean select) {
    btnHighlighter.setSelected(select);
  }

  /**
   * In the first row, turn off theta and psi when sampleSphere is on.  In all
   * rows turn off duplicates columns when flgRemoveDuplicates is off.
   * @param sampleSphere
   * @param flgRemoveDuplicates
   */
  void updateDisplay(final boolean sampleSphere, final boolean flgRemoveDuplicates) {
    if (getIndex() == 0) {
      dThetaMax.setEnabled(!sampleSphere);
      dThetaIncrement.setEnabled(!sampleSphere);
      dPsiMax.setEnabled(!sampleSphere);
      dPsiIncrement.setEnabled(!sampleSphere);
    }
    duplicateShiftTolerance.setEnabled(flgRemoveDuplicates);
    duplicateAngularTolerance.setEnabled(flgRemoveDuplicates);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    MatlabParam.Iteration iteration = matlabParamFile.getIteration(index);
    if (!dPhiMax.isEmpty() || !dPhiIncrement.isEmpty()) {
      iteration.setDPhiEnd(dPhiMax.getValue());
      iteration.setDPhiIncrement(dPhiIncrement.getValue());
    }
    else {
      iteration.clearDPhi();
    }
    if (!dThetaMax.isEmpty() || !dThetaIncrement.isEmpty()) {
      iteration.setDThetaEnd(dThetaMax.getValue());
      iteration.setDThetaIncrement(dThetaIncrement.getValue());
    }
    else {
      iteration.clearDTheta();
    }
    if (!dPsiMax.isEmpty() || !dPsiIncrement.isEmpty()) {
      iteration.setDPsiEnd(dPsiMax.getValue());
      iteration.setDPsiIncrement(dPsiIncrement.getValue());
    }
    else {
      iteration.clearDPsi();
    }
    iteration.setSearchRadius(searchRadius.getValue());
    iteration.setHiCutoffCutoff(hiCutoff.getValue());
    iteration.setHiCutoffSigma(lowCutoff.getValue());
    iteration.setRefThreshold(refThreshold.getValue());
    iteration.setDuplicateShiftTolerance(duplicateShiftTolerance.getValue());
    iteration.setDuplicateAngularTolerance(duplicateAngularTolerance.getValue());
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
    duplicateShiftTolerance.setValue(iteration.getDuplicateShiftToleranceString());
    duplicateAngularTolerance.setValue(iteration.getDuplicateAngularToleranceString());
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
    duplicateShiftTolerance.remove();
    duplicateAngularTolerance.remove();
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
    refThreshold.add(panel, layout, constraints);
    duplicateShiftTolerance.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    duplicateAngularTolerance.add(panel, layout, constraints);
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
  private boolean validateRun(boolean empty, EtomoNumber n, String[] headerArray,
      String additionalEmptyErrorMessage) {
    if (empty) {
      UIHarness.INSTANCE.openMessageDialog(manager, IterationTable.LABEL + ":  In row "
          + number.toString() + buildHeaderDescription(headerArray)
          + " must not be empty."
          + (additionalEmptyErrorMessage == null ? "" : additionalEmptyErrorMessage),
          "Entry Error");
      return false;
    }
    if (n != null) {
      if (!n.isValid()) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            IterationTable.LABEL + ":  In row " + number.toString()
                + buildHeaderDescription(headerArray) + ":   " + n.getInvalidReason(),
            "Entry Error");
        return false;
      }
      if (n.isNegative()) {
        UIHarness.INSTANCE.openMessageDialog(manager, IterationTable.LABEL + ":  In row "
            + number.getText() + buildHeaderDescription(headerArray)
            + " must not be negative.", "Entry Error");
        return false;
      }
    }
    return true;
  }

  boolean validateRun() {
    // Phi
    EtomoNumber nDouble = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    EtomoNumber nInteger = new EtomoNumber();
    nDouble.set(dPhiMax.getValue());
    if (!validateRun(dPhiMax.isEmpty(), nDouble, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_PHI_LABEL,
        IterationTable.MAX_HEADER3 }, "Use 0 to not search on the angle.")) {
      return false;
    }
    nDouble.set(dPhiIncrement.getValue());
    if (!validateRun(dPhiIncrement.isEmpty(), nDouble, new String[] {
        IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_PHI_LABEL,
        IterationTable.INCR_HEADER3 }, null)) {
      return false;
    }
    // Theta
    if (dThetaMax.isEnabled()) {
      nDouble.set(dThetaMax.getValue());
      if (!validateRun(dThetaMax.isEmpty(), nDouble, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_THETA_LABEL,
          IterationTable.MAX_HEADER3 }, "Use 0 to not search on the angle.")) {
        return false;
      }
    }
    if (dThetaIncrement.isEnabled()) {
      nDouble.set(dThetaIncrement.getValue());
      if (!validateRun(dThetaIncrement.isEmpty(), nDouble, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_THETA_LABEL,
          IterationTable.INCR_HEADER3 }, null)) {
        return false;
      }
    }
    // Psi
    if (dPsiMax.isEnabled()) {
      nDouble.set(dPsiMax.getValue());
      if (!validateRun(dPsiMax.isEmpty(), nDouble, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_PSI_LABEL,
          IterationTable.MAX_HEADER3 }, "Use 0 to not search on the angle.")) {
        return false;
      }
    }
    if (dPsiIncrement.isEnabled()) {
      nDouble.set(dPsiIncrement.getValue());
      if (!validateRun(dPsiIncrement.isEmpty(), nDouble, new String[] {
          IterationTable.D_PHI_D_THETA_D_PSI_HEADER1, FieldLabels.D_PSI_LABEL,
          IterationTable.INCR_HEADER3 }, null)) {
        return false;
      }
    }
    // search radius
    String searchRadiusString = searchRadius.getValue().trim();
    String[] headerArray = new String[] { IterationTable.SEARCH_RADIUS_HEADER1,
        IterationTable.SEARCH_RADIUS_HEADER2 };
    nInteger.set(searchRadiusString);
    if (searchRadius.isEmpty() || nInteger.isValid()) {
      if (!validateRun(searchRadius.isEmpty(), nInteger, headerArray, null)) {
        return false;
      }
    }
    else {
      // If its not a single number then it must be a list of three numbers
      // divided by "," or " ".
      String[] searchRadiusArray = searchRadiusString.split("\\s*,\\s*");
      if (searchRadiusArray.length != 3) {
        searchRadiusArray = searchRadiusString.split("\\s+");
        if (searchRadiusArray.length != 3) {
          UIHarness.INSTANCE.openMessageDialog(manager, IterationTable.LABEL
              + ":  In row " + number.toString() + buildHeaderDescription(headerArray)
              + " must have either 1 or 3 elements.", "Entry Error");
          return false;
        }
      }
      // Validate each number in the array.
      for (int i = 0; i < searchRadiusArray.length; i++) {
        nInteger.set(searchRadiusArray[i]);
        if (!validateRun(false, nInteger, headerArray, null)) {
          return false;
        }
      }
    }
    // hiCutoff
    if (!validateRun(hiCutoff.isEmpty(), null, new String[] {
        IterationTable.CUTOFF_HEADER1, IterationTable.CUTOFF_HEADER2,
        IterationTable.HI_CUTOFF_HEADER3 }, null)) {
      return false;
    }
    // lowCutoff
    if (!validateRun(lowCutoff.isEmpty(), null, new String[] {
        IterationTable.CUTOFF_HEADER1, IterationTable.CUTOFF_HEADER2,
        IterationTable.LOW_CUTOFF_HEADER3 }, null)) {
      return false;
    }
    // refThreshold
    if (!validateRun(refThreshold.isEmpty(), null, new String[] {
        IterationTable.REF_THRESHOLD_HEADER1, IterationTable.REF_THRESHOLD_HEADER2 },
        null)) {
      return false;
    }
    // duplicateShiftTolerance
    if (duplicateShiftTolerance.isEnabled()) {
      nInteger.set(duplicateShiftTolerance.getValue());
      if (!validateRun(duplicateShiftTolerance.isEmpty(), nInteger, new String[] {
          IterationTable.DUPLICATE_TOLERANCE_HEADER1,
          IterationTable.DUPLICATE_TOLERANCE_HEADER2,
          IterationTable.DUPLICATE_SHIFT_TOLERANCE_HEADER3 }, null)) {
        return false;
      }
    }
    // duplicateAngularTolerance
    if (duplicateAngularTolerance.isEnabled()) {
      nInteger.set(duplicateAngularTolerance.getValue());
      if (!validateRun(duplicateAngularTolerance.isEmpty(), nInteger, new String[] {
          IterationTable.DUPLICATE_TOLERANCE_HEADER1,
          IterationTable.DUPLICATE_TOLERANCE_HEADER2,
          IterationTable.DUPLICATE_ANGULAR_TOLERANCE_HEADER3 }, null)) {
        return false;
      }
    }
    return true;
  }

  private void setTooltips() {
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
    duplicateShiftTolerance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.DUPLICATE_SHIFT_TOLERANCE_KEY));
    duplicateAngularTolerance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.DUPLICATE_ANGULAR_TOLERANCE_KEY));

    number.setToolTipText("Iteration number");
    dPhiMax.setToolTipText("Maximum magnitude of rotation about the particle Y axis "
        + "in degrees.  Search will range from -(Phi Max) to +(Phi Max) in "
        + "steps of (Phi Incr).");
    dPhiIncrement
        .setToolTipText("Increment between sample points for rotation about Y in "
            + "degrees.  Search will range from -(Phi Max) to +(Phi Max) in "
            + "steps of (Phi Incr).");
    dThetaMax.setToolTipText("Maximum magnitude of rotation about the particle Z axis "
        + "in degrees.  Search will range from -(Theta Max) to +(Theta Max) "
        + "in steps of (Theta Incr).");
    dThetaIncrement
        .setToolTipText("Increment between sample points for rotation about Z in "
            + "degrees.  Search will range from -(Theta Max) to +(Theta Max) in "
            + "steps of (Theta Incr).");
    dPsiMax.setToolTipText("Maximum magnitude of rotation about the particle X axis "
        + "in degrees.  Search will range from -(Psi Max) to +(Psi Max) in "
        + "steps of (Psi Incr).");
    dPsiIncrement
        .setToolTipText("Increment between sample points for rotation about X in "
            + "degrees.  Search will range from -(Psi Max) to +(Psi Max) in "
            + "steps of (Psi Incr).");
    searchRadius.setToolTipText("The number of pixels to search in the X, Y, and Z "
        + "directions.  A single, integer number of pixels can be specified, "
        + "which will be applied to all 3 dimensions, or a vector of 3 "
        + "integers can be specified, giving the X, Y, and Z search "
        + "distances individually. E.g. '3' is equivalent to '3 3 3'.");
    hiCutoff.setToolTipText("The normalized spatial frequency above which high "
        + "frequencies are attenuated.  0.5 corresponds to the Nyquist "
        + "frequency, and values of 0.866 or larger disable low-pass " + "filtering.");
    lowCutoff.setToolTipText("The width (standard deviation) in normalized frequency "
        + "units of a Gaussian determining the rate at which attenuation "
        + "increases above the cutoff.");
    refThreshold
        .setToolTipText("Determines the number of particles averaged to form the "
            + "reference for the next alignment iteration. If less than 1, it "
            + "represents a cross-correlation coefficient threshold, with "
            + "particles having a larger correlation eligible for inclusion in "
            + "the reference.  If greater than 1, it is the number of particles "
            + "to include.");
  }
}
