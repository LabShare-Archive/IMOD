package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.ApplicationManager;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.type.AxisID;
import etomo.type.DialogType;
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
 * <p> Revision 1.4  2011/02/22 19:07:48  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:16:36  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.2  2010/03/19 02:42:25  sueh
 * <p> bug# 1325 Changed a button label.
 * <p>
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
  private final Run3dmodButton btn3dmodReprojectModel = Run3dmodButton.get3dmodInstance(
      "View 2D Model on Aligned Stack", this);

  private final Run3dmodButton btnReprojectModel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private ReprojectModelPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    btnReprojectModel = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getReprojectModel();
  }

  static ReprojectModelPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    ReprojectModelPanel instance = new ReprojectModelPanel(manager, axisID, dialogType);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
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
  
  public boolean allowTiltComSave() {
    return true;
  }
  
  public void msgTiltComSaved() {
  }

  void done() {
    btnReprojectModel.removeActionListener(actionListener);
  }

  void setParameters(ReconScreenState screenState) {
    btnReprojectModel.setButtonState(screenState.getButtonState(btnReprojectModel
        .getButtonStateKey()));
  }

  /**
   * Don't need parallel processing for reprojection of a model.
   */
  public boolean getParameters(final SplittiltParam param,final boolean doValidation) {
    return false;
  }

  public boolean getParameters(TiltParam param,final boolean doValidation) {
    return true;
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnReprojectModel.getActionCommand())) {
      manager.reprojectModelAction(btnReprojectModel, null, deferred3dmodButton,
          run3dmodMenuOptions, this, axisID, dialogType);
    }
    else if (command.equals(btn3dmodReprojectModel.getActionCommand())) {
      manager.imodReprojectModel(axisID, run3dmodMenuOptions);
    }
  }

  private void setToolTipText() {
    btnReprojectModel.setToolTipText("Run tilt to reproject the model.");
    btn3dmodReprojectModel.setToolTipText("View model of gold particles.");
  }

  private final class ReprojectModelPanelActionListener implements ActionListener {
    private final ReprojectModelPanel adaptee;

    private ReprojectModelPanelActionListener(final ReprojectModelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
