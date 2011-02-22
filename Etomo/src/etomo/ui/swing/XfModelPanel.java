package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class XfModelPanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final XfModelPanelActionListener actionListener = new XfModelPanelActionListener(
      this);
  private final Run3dmodButton btn3dmodXfModel = Run3dmodButton.get3dmodInstance(
      "View Transformed Model", this);

  private final Run3dmodButton btnXfModel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private XfModelPanel(ApplicationManager manager, AxisID axisID, DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    btnXfModel = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getXfModel();
  }

  static XfModelPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    XfModelPanel instance = new XfModelPanel(manager, axisID, dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    btnXfModel.addActionListener(actionListener);
    btn3dmodXfModel.addActionListener(actionListener);
  }

  private void createPanel() {
    //Initialize
    btnXfModel.setContainer(this);
    btnXfModel.setDeferred3dmodButton(btn3dmodXfModel);
    btnXfModel.setSize();
    btn3dmodXfModel.setSize();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.add(btnXfModel.getComponent());
    pnlRoot.add(btn3dmodXfModel.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  void done() {
    btnXfModel.removeActionListener(actionListener);
  }

  void setParameters(ReconScreenState screenState) {
    btnXfModel.setButtonState(screenState.getButtonState(btnXfModel.getButtonStateKey()));
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnXfModel.getActionCommand())) {
      manager.xfmodel(btnXfModel, null, deferred3dmodButton, run3dmodMenuOptions, axisID,
          dialogType);
    }
    else if (command.equals(btn3dmodXfModel.getActionCommand())) {
      manager.seedEraseFiducialModel(run3dmodMenuOptions, axisID, dialogType);
    }
  }

  private void setToolTipText() {
    btnXfModel.setToolTipText("Transform .fid mode built on prealigned stack to "
        + "_erase.fid model that fits the aligned stack.");
    btn3dmodXfModel.setToolTipText("View the _erase.fid model on the aligned stack.");
  }

  private final class XfModelPanelActionListener implements ActionListener {
    private final XfModelPanel adaptee;

    private XfModelPanelActionListener(final XfModelPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
