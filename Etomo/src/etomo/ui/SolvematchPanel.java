package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstSolvematchParam;
import etomo.comscript.SolvematchParam;
import etomo.type.FiducialMatch;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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
 * <p> Revision 3.3  2004/06/15 21:37:16  rickg
 * <p> Bug #383 Correct synchronization of solvematch sub-panel
 * <p>
 * <p> Revision 3.2  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.1  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> </p>
 */

public class SolvematchPanel implements InitialCombineFields {

  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();
  private JPanel pnlFiducialRadio = new JPanel();
  private JPanel pnlFiducialSelect = new JPanel();
  private ButtonGroup bgFiducialParams = new ButtonGroup();
  private JRadioButton rbBothSides = new JRadioButton("Fiducials on both sides");
  private JRadioButton rbOneSide = new JRadioButton(
    "Fiducials on one side, NOT inverted");
  private JRadioButton rbOneSideInverted = new JRadioButton(
    "Fiducials on one side, inverted");
  private JRadioButton rbUseModel = new JRadioButton(
    "Use matching models and fiducials");
  private JRadioButton rbUseModelOnly = new JRadioButton(
    "Use matching models only");

  private JPanel pnlImodMatchModels = new JPanel();
  private JCheckBox cbBinBy2 = new JCheckBox("Load binned by 2");
  private MultiLineButton btnImodMatchModels = new MultiLineButton(
    "<html><b>Create Matching Models in 3dmod</b>");
  private LabeledTextField ltfFiducialMatchListA = new LabeledTextField(
    "Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB = new LabeledTextField(
    "Corresponding fiducial list B: ");

  private LabeledTextField ltfResidulThreshold = new LabeledTextField(
    "Limit on maximum residual: ");

  private TomogramCombinationDialog tomogramCombinationDialog;
  private String parentTitle;

  public SolvematchPanel(TomogramCombinationDialog parent, String title,
    ApplicationManager appMgr) {
    tomogramCombinationDialog = parent;
    parentTitle = title;
    applicationManager = appMgr;
    //  Create the fiducial relationship panel
    pnlFiducialRadio
      .setLayout(new BoxLayout(pnlFiducialRadio, BoxLayout.Y_AXIS));

    rbBothSides.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSide.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSideInverted.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbUseModel.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgFiducialParams.add(rbBothSides);
    bgFiducialParams.add(rbOneSide);
    bgFiducialParams.add(rbOneSideInverted);
    bgFiducialParams.add(rbUseModel);
    bgFiducialParams.add(rbUseModelOnly);
    pnlFiducialRadio.add(rbBothSides);
    pnlFiducialRadio.add(rbOneSide);
    pnlFiducialRadio.add(rbOneSideInverted);
    pnlFiducialRadio.add(rbUseModel);
    pnlFiducialRadio.add(rbUseModelOnly);

    pnlImodMatchModels.setLayout(new BoxLayout(pnlImodMatchModels,
      BoxLayout.Y_AXIS));
    pnlImodMatchModels.add(cbBinBy2);
    pnlImodMatchModels.add(btnImodMatchModels);
    UIUtilities.setButtonSizeAll(pnlImodMatchModels, UIParameters.dimButton);

    pnlFiducialSelect.setLayout(new BoxLayout(pnlFiducialSelect,
      BoxLayout.X_AXIS));
    UIUtilities.addWithSpace(pnlFiducialSelect, pnlFiducialRadio,
      FixedDim.x20_y0);
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlFiducialSelect.add(pnlImodMatchModels);

    pnlRoot.setBorder(new EtchedBorder("Solvematch Parameters").getBorder());
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    UIUtilities.addWithSpace(pnlRoot, pnlFiducialSelect, FixedDim.x0_y10);
    UIUtilities.addWithYSpace(pnlRoot, ltfFiducialMatchListA.getContainer());
    UIUtilities.addWithYSpace(pnlRoot, ltfFiducialMatchListB.getContainer());
    UIUtilities.addWithYSpace(pnlRoot, ltfResidulThreshold.getContainer());

    //  Bind the ui elements to their listeners
    SolvematchPanelActionListener actionListener = new SolvematchPanelActionListener(
      this);
    btnImodMatchModels.addActionListener(actionListener);

    RBFiducialListener rbFiducialListener = new RBFiducialListener(this);
    rbBothSides.addActionListener(rbFiducialListener);
    rbOneSide.addActionListener(rbFiducialListener);
    rbOneSideInverted.addActionListener(rbFiducialListener);
    rbUseModel.addActionListener(rbFiducialListener);
    rbUseModelOnly.addActionListener(rbFiducialListener);

    setToolTipText();
  }

  public Container getContainer() {
    return pnlRoot;
  }

  // FIXME there are current two ways to get the parameters into and out of the
  // panel.  Does this need to be the case?  It seem redundant.
  void setParameters(ConstCombineParams combineParams) {
    if (combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      rbBothSides.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      rbOneSide.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      rbOneSideInverted.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      rbUseModel.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL_ONLY) {
      rbUseModelOnly.setSelected(true);
    }
    ltfFiducialMatchListA.setText(combineParams.getFiducialMatchListA());
    ltfFiducialMatchListB.setText(combineParams.getFiducialMatchListB());
  }

  /**
   * Get the parameters from the ui and filling in the appropriate fields in the
   * CombineParams object 
   * @param combineParams
   */
  void getParameters(CombineParams combineParams) {
    if (rbBothSides.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.BOTH_SIDES);
    }
    if (rbOneSide.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE);
    }
    if (rbOneSideInverted.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE_INVERTED);
    }
    if (rbUseModel.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.USE_MODEL);
    }
    if (rbUseModelOnly.isSelected()) {
      combineParams.setFiducialMatch(FiducialMatch.USE_MODEL_ONLY);
    }

    combineParams.setFiducialMatchListA(ltfFiducialMatchListA.getText());
    combineParams.setFiducialMatchListB(ltfFiducialMatchListB.getText());
  }

  void setParameters(ConstSolvematchParam solvematchParam) {

    setSurfacesOrModels(solvematchParam.getSurfacesOrModel());
    if (solvematchParam.isMatchBToA()) {
      ltfFiducialMatchListA.setText(solvematchParam.getToCorrespondenceList()
        .toString());
      ltfFiducialMatchListB.setText(solvematchParam.getFromCorrespondenceList()
        .toString());
    }
    else {
      ltfFiducialMatchListB.setText(solvematchParam.getToCorrespondenceList()
        .toString());
      ltfFiducialMatchListA.setText(solvematchParam.getFromCorrespondenceList()
        .toString());
    }
    ltfResidulThreshold.setText(solvematchParam.getMaximumResidual());
  }

  void visibleResidual(boolean state) {
    ltfResidulThreshold.setVisible(state);
  }

  /**
   * Get the parameters from the ui and filling in the appropriate fields in the
   * SolvematchParam object 
   * @param combineParams
   */
  void getParameters(SolvematchParam solvematchParam) {

    solvematchParam.setSurfacesOrModel(getSurfacesOrModels());

    if (solvematchParam.isMatchBToA()) {
      solvematchParam.setToCorrespondenceList(ltfFiducialMatchListA.getText());
      solvematchParam
        .setFromCorrespondenceList(ltfFiducialMatchListB.getText());
    }
    else {
      solvematchParam
        .setFromCorrespondenceList(ltfFiducialMatchListA.getText());
      solvematchParam.setToCorrespondenceList(ltfFiducialMatchListB.getText());
    }
    solvematchParam.setMaximumResidual(ltfResidulThreshold.getText());
  }

  /**
   * 
   * @return
   */
  public FiducialMatch getSurfacesOrModels() {
    if (rbBothSides.isSelected()) {
      return FiducialMatch.BOTH_SIDES;
    }
    if (rbOneSide.isSelected()) {
      return FiducialMatch.ONE_SIDE;
    }
    if (rbOneSideInverted.isSelected()) {
      return FiducialMatch.ONE_SIDE_INVERTED;
    }
    if (rbUseModel.isSelected()) {
      return FiducialMatch.USE_MODEL;
    }
    if (rbUseModelOnly.isSelected()) {
      return FiducialMatch.USE_MODEL_ONLY;
    }
    return FiducialMatch.NOT_SET;
  }

  public void setSurfacesOrModels(FiducialMatch value) {
    if (value == FiducialMatch.USE_MODEL_ONLY) {
      rbUseModelOnly.setSelected(true);
    }
    if (value == FiducialMatch.ONE_SIDE_INVERTED) {
      rbOneSideInverted.setSelected(true);
    }
    if (value == FiducialMatch.USE_MODEL) {
      rbUseModel.setSelected(true);
    }
    if (value == FiducialMatch.ONE_SIDE) {
      rbOneSide.setSelected(true);
    }
    if (value == FiducialMatch.BOTH_SIDES) {
      rbBothSides.setSelected(true);
    }
    updateUseFiducialModel();
  }

  public boolean isBinBy2() {
    return cbBinBy2.isSelected();
  }

  public void setBinBy2(boolean state) {
    cbBinBy2.setSelected(state);
  }

  public void setFiducialMatchListA(String fiducialMatchListA) {
    ltfFiducialMatchListA.setText(fiducialMatchListA);
  }

  public String getFiducialMatchListA() {
    return ltfFiducialMatchListA.getText();
  }

  public void setFiducialMatchListB(String fiducialMatchListB) {
    ltfFiducialMatchListB.setText(fiducialMatchListB);
  }

  public String getFiducialMatchListB() {
    return ltfFiducialMatchListB.getText();
  }

  //  Action functions for setup panel buttons
  private void buttonAction(ActionEvent event) {
    //  Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(parentTitle, true,
      TomogramCombinationDialog.ALL_FIELDS);

    String command = event.getActionCommand();
    if (event.getActionCommand().equals(btnImodMatchModels.getActionCommand())) {
      applicationManager.imodMatchingModel(cbBinBy2.isSelected());
    }
  }

  /**
   * Manage fiducial radio button action
   * 
   * @param event
   */
  private void rbFiducialAction(ActionEvent event) {
    updateUseFiducialModel();
  }

  /**
   * Enable/disable the matching model button
   */
  void updateUseFiducialModel() {
    boolean enable = rbUseModel.isSelected() || rbUseModelOnly.isSelected();
    btnImodMatchModels.setEnabled(enable);
    cbBinBy2.setEnabled(enable);
  }

  /**
   * Manage the matching models button
   */
  class SolvematchPanelActionListener implements ActionListener {

    SolvematchPanel adaptee;

    public SolvematchPanelActionListener(SolvematchPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

  class RBFiducialListener implements ActionListener {

    SolvematchPanel adaptee;

    public RBFiducialListener(SolvematchPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbFiducialAction(event);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Select this option when there are fiducials distributed in Z.";
    rbBothSides.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Select this option when the fiducials lie on one surface and the "
      + "tomograms are not inverted in Z with respect to each other.";
    rbOneSide.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Select this option when the fiducials lie on one surface and the "
      + "top of one tomogram in Z corresponds to the bottom of the other.";
    rbOneSideInverted.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Select this option to use both fiducials and models of "
        + "corresponding points to find the transformation between tomograms.";
    rbUseModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Select this option to use ONLY models of corresponding points to "
        + "find the transformation between tomograms.";
    rbUseModelOnly.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use binning by 2 when opening matching models to allow the two 3dmods "
      + "to fit into the computer's memory.";
    cbBinBy2.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Create models of corresponding points.";
    btnImodMatchModels.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter the list of fiducials in A for which you know the corresponding "
      + "fiducial in B.  Use the point number in *fid.xyz, not the contour "
      + "number.";
    ltfFiducialMatchListA.setToolTipText(tooltipFormatter.setText(text)
      .format());

    text = "Enter the list of fiducials in B that correspond to the ones in the "
      + "list entered for A.  Use the point number in *fid.xyz, not the "
      + "contour number.";
    ltfFiducialMatchListB.setToolTipText(tooltipFormatter.setText(text)
      .format());
    
    text = "Maximum residual allowed when fitting a 3D transformation to the "
        + "corresponding points.";
    ltfResidulThreshold.setToolTipText(tooltipFormatter.setText(text).format());
  }
}