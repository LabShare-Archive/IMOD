package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.ApplicationManager;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class ReprojectModelPanel implements TiltDisplay, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  static final String REPROJECT_MODEL_LABEL = "Reproject Model";
  private final SpacedPanel pnlRoot = SpacedPanel.getInstance(true);
  private final ActionListener actionListener = new ReprojectModelPanelActionListener(
      this);
  private final Run3dmodButton btn3dmodReprojectModel = Run3dmodButton
      .get3dmodInstance("View 2D Model on Aligned Stack", this);

  private final Run3dmodButton btnReprojectModel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private ReprojectModelPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    btnReprojectModel = (Run3dmodButton) manager
        .getProcessResultDisplayFactory(axisID).getReprojectModel();
  }

  static ReprojectModelPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType) {
    ReprojectModelPanel instance = new ReprojectModelPanel(manager, axisID,
        dialogType);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  public static ProcessResultDisplay getReprojectModelDisplay(
      DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance(REPROJECT_MODEL_LABEL,
        dialogType);
  }

  private void addListeners() {
    btnReprojectModel.addActionListener(actionListener);
    btn3dmodReprojectModel.addActionListener(actionListener);
  }

  private void createPanel() {
    //Initialize
    btnReprojectModel.setContainer(this);
    btnReprojectModel.setDeferred3dmodButton(btn3dmodReprojectModel);
    btnReprojectModel.setSize();
    btn3dmodReprojectModel.setSize();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.add(btnReprojectModel.getComponent());
    pnlRoot.add(btn3dmodReprojectModel.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void done() {
    btnReprojectModel.removeActionListener(actionListener);
  }

  void setParameters(ReconScreenState screenState) {
    btnReprojectModel.setButtonState(screenState
        .getButtonState(btnReprojectModel.getButtonStateKey()));
  }

  public boolean isParallelProcess() {
    return false;
  }

  /**
   * Don't need parallel processing for reprojection of a model.
   */
  public boolean getParameters(final SplittiltParam param) {
    return false;
  }

  public boolean getParameters(TiltParam param) {
    return true;
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnReprojectModel.getActionCommand())) {
      manager.reprojectModelAction(btnReprojectModel, null,
          deferred3dmodButton, run3dmodMenuOptions, this, axisID, dialogType);
    }
    else if (command.equals(btn3dmodReprojectModel.getActionCommand())) {
      manager.imodReprojectModel(axisID, run3dmodMenuOptions);
    }
  }

  private void setToolTipText() {
    btnReprojectModel.setToolTipText("Run tilt to reproject the model.");
    btn3dmodReprojectModel.setToolTipText("View model of gold particles.");
  }

  private final class ReprojectModelPanelActionListener implements
      ActionListener {
    private final ReprojectModelPanel adaptee;

    private ReprojectModelPanelActionListener(final ReprojectModelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
