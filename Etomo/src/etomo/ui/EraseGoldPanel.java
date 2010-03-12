package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.3  2009/09/22 23:54:41  sueh
 * <p> bug# 1269 Added setEnabledTiltParameters.
 * <p>
 * <p> Revision 1.2  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class EraseGoldPanel implements Beads3dFindParent {
  public static final String rcsid = "$Id$";
  
  static final String ERASE_GOLD_TAB_LABEL="Erase Gold";

  private final JPanel pnlRoot = new JPanel();
  private final EraseGoldPanelActionListener actionListener = new EraseGoldPanelActionListener(
      this);
  private final ButtonGroup bgModel = new ButtonGroup();
  private final RadioButton rbModelUseFid = new RadioButton(
      "Use the existing fiducial model", bgModel);
  private final RadioButton rbModelUseFindBeads3d = new RadioButton(
      "Use findbeads3d", bgModel);

  private final XfModelPanel xfModelPanel;
  private final Beads3dFindPanel beads3dFindPanel;
  private final CcdEraserBeadsPanel ccdEraserBeadsPanel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;
  private final EraseGoldParent parent;

  private EraseGoldPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, EraseGoldParent parent,
      GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
    xfModelPanel = XfModelPanel.getInstance(manager, axisID, dialogType);
    beads3dFindPanel = Beads3dFindPanel.getInstance(manager, axisID,
        dialogType, this, globalAdvancedButton);
    ccdEraserBeadsPanel = CcdEraserBeadsPanel.getInstance(manager, axisID,
        dialogType);
  }

  static EraseGoldPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, EraseGoldParent parent,
      GlobalExpandButton globalAdvancedButton) {
    EraseGoldPanel instance = new EraseGoldPanel(manager, axisID, dialogType,
        parent, globalAdvancedButton);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    rbModelUseFid.addActionListener(actionListener);
    rbModelUseFindBeads3d.addActionListener(actionListener);
  }

  private void createPanel() {
    //Local panels
    JPanel pnlModel = new JPanel();
    //Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder("Bead Eraser").getBorder());
    pnlRoot.add(pnlModel);
    pnlRoot.add(xfModelPanel.getComponent());
    pnlRoot.add(beads3dFindPanel.getComponent());
    pnlRoot.add(ccdEraserBeadsPanel.getComponent());
    //Model panel
    pnlModel.setLayout(new BoxLayout(pnlModel, BoxLayout.Y_AXIS));
    pnlModel.setBorder(new EtchedBorder("Model Creation Method").getBorder());
    pnlModel.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlModel.add(rbModelUseFid.getComponent());
    pnlModel.add(rbModelUseFindBeads3d.getComponent());
  }

  Component getComponent() {
    return pnlRoot;
  }

  public void updateParallelProcess() {
    parent.updateParallelProcess();
  }

  void updateAdvanced(boolean advanced) {
    beads3dFindPanel.updateAdvanced(advanced);
  }

  BlendmontDisplay getBlendmont3dFindDisplay() {
    return beads3dFindPanel.getBlendmont3dFindDisplay();
  }

  NewstackDisplay getNewstack3dFindDisplay() {
    return beads3dFindPanel.getNewstack3dFindDisplay();
  }

  TiltDisplay getTilt3dFindDisplay() {
    return beads3dFindPanel.getTilt3dFindDisplay();
  }

  FindBeads3dDisplay getFindBeads3dDisplay() {
    return beads3dFindPanel.getFindBeads3dDisplay();
  }

  CcdEraserDisplay getCcdEraserBeadsDisplay() {
    return ccdEraserBeadsPanel;
  }

  void done() {
    xfModelPanel.done();
    beads3dFindPanel.done();
    ccdEraserBeadsPanel.done();
  }

  private void updateDisplay() {
    boolean modelUseFidIsSelected = rbModelUseFid.isSelected();
    xfModelPanel.setVisible(modelUseFidIsSelected);
    beads3dFindPanel.setVisible(!modelUseFidIsSelected);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public boolean usingParallelProcessing() {
    return rbModelUseFindBeads3d.isSelected()
        && beads3dFindPanel.usingParallelProcessing();
  }

  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    ccdEraserBeadsPanel.getParameters(metaData);
    metaData.setEraseGoldModelUseFid(axisID, rbModelUseFid.isSelected());
    beads3dFindPanel.getParameters(metaData);
  }

  void setParameters(ReconScreenState screenState) {
    beads3dFindPanel.setParameters(screenState);
  }

  void getParameters(ReconScreenState screenState) {
    beads3dFindPanel.getParameters(screenState);
  }
  
  public void setEnabledTiltParameters(TomogramState state,
      ConstMetaData metaData) {
    beads3dFindPanel.setEnabledTiltParameters(state, metaData);
  }

  void setParameters(BlendmontParam param) {
    beads3dFindPanel.setParameters(param);
  }

  void setParameters(NewstParam param) {
    beads3dFindPanel.setParameters(param);
  }

  void setParameters(ConstTiltParam param, boolean initialize)
      throws FileNotFoundException, IOException, LogFile.LockException {
    beads3dFindPanel.setParameters(param, initialize);
  }

  void setParameters(ConstMetaData metaData) {
    ccdEraserBeadsPanel.setParameters(metaData);
    if (metaData.getEraseGoldModelUseFid(axisID)) {
      rbModelUseFid.setSelected(true);
    }
    else {
      rbModelUseFindBeads3d.setSelected(true);
    }
    beads3dFindPanel.setParameters(metaData);
    updateDisplay();
  }

  void setParameters(ConstFindBeads3dParam param, boolean initialize) {
    beads3dFindPanel.setParameters(param, initialize);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(rbModelUseFid.getActionCommand())
        || command.equals(rbModelUseFindBeads3d.getActionCommand())) {
      updateDisplay();
      updateParallelProcess();
    }
  }

  private void setToolTipText() {
    rbModelUseFid
        .setToolTipText("Erase the fiducials selected in the fiducial model.");
    rbModelUseFindBeads3d
        .setToolTipText("Use findbeads3d to find fiducial for erasure.");
  }

  private final class EraseGoldPanelActionListener implements ActionListener {
    private final EraseGoldPanel adaptee;

    private EraseGoldPanelActionListener(final EraseGoldPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
