package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;

import etomo.storage.MatlabParam;

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
final class IterationRow implements Highlightable{
  public static final String rcsid = "$Id$";

  private final HeaderCell number = new HeaderCell();
  private final FieldCell dPhiMax =  FieldCell.getEditableInstance();
  private final FieldCell dPhiIncrement =  FieldCell.getEditableInstance();
  private final FieldCell dThetaMax = FieldCell.getEditableInstance();
  private final FieldCell dThetaIncrement =  FieldCell.getEditableInstance();
  private final FieldCell dPsiMax =  FieldCell.getEditableInstance();
  private final FieldCell dPsiIncrement =  FieldCell.getEditableInstance();
  private final FieldCell searchRadius =  FieldCell.getEditableInstance();
  private final FieldCell hiCutoffCutoff= FieldCell.getEditableInstance();
  private final FieldCell hiCutoffSigma= FieldCell.getEditableInstance();
  private final FieldCell refThreshold =  FieldCell.getEditableInstance();
  
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final HighlighterButton btnHighlighter;
  private final Highlightable parent;
  
  private int index;

  IterationRow(final int index, final Highlightable parent, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    this.index = index;
    this.parent=parent;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    btnHighlighter = HighlighterButton.getInstance(this, parent);
    number.setText(String.valueOf(index + 1));
  }
  
  IterationRow(final int index, final IterationRow iterationRow) {
    this.index = index;
    this.parent=iterationRow.parent;
    this.panel = iterationRow.panel;
    this.layout = iterationRow.layout;
    this.constraints = iterationRow.constraints;
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
    number.add(panel,layout,constraints);
    btnHighlighter.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    dPhiMax.add(panel,layout,constraints);
    dPhiIncrement.add(panel,layout,constraints);
    dThetaMax.add(panel,layout,constraints);
    dThetaIncrement.add(panel,layout,constraints);
    dPsiMax.add(panel,layout,constraints);
    dPsiIncrement.add(panel,layout,constraints);
    searchRadius.add(panel,layout,constraints);
    hiCutoffCutoff.add(panel,layout,constraints);
    hiCutoffSigma.add(panel,layout,constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    refThreshold.add(panel,layout,constraints);
  }
}
