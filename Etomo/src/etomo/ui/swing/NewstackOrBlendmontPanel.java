package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.InvalidParameterException;

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
 * <p> Revision 1.4  2011/03/02 00:00:12  sueh
 * <p> bug# 1452 Removing image rotation conversion between float and
 * <p> double.  Using string where possible.
 * <p>
 * <p> Revision 1.3  2011/02/24 23:37:34  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:16:52  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.2  2009/09/17 19:12:58  sueh
 * <p> bug# 1257 In NewstParam.setSizeToOutputInXandY forgot to read the
 * <p> header.  Adding read call and throwing InvalidParameterException and
 * <p> IOException.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
abstract class NewstackOrBlendmontPanel implements Run3dmodButtonContainer, Expandable,
    NewstackDisplay, BlendmontDisplay {
  public static final String rcsid = "$Id$";

  static final String RUN_BUTTON_LABEL = "Create Full Aligned Stack";

  private final JPanel pnlRoot = new JPanel();

  private final ActionListener actionListener = new NewstackOrBlendmontPanelActionListener(
      this);
  private final PanelHeader header;
  private final SpacedPanel pnlBody = SpacedPanel.getInstance();
  private final Run3dmodButton btn3dmodFull = Run3dmodButton.get3dmodInstance(
      "View Full Aligned Stack", this);

  private final NewstackAndBlendmontParamPanel newstackAndBlendmontParamPanel;
  private final Run3dmodButton btnRunProcess;
  final AxisID axisID;
  final ApplicationManager manager;
  final DialogType dialogType;

  NewstackOrBlendmontPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    header = PanelHeader.getAdvancedBasicOnlyInstance(getHeaderTitle(), this, dialogType,
        globalAdvancedButton, false);
    newstackAndBlendmontParamPanel = NewstackAndBlendmontParamPanel.getInstance(manager,
        axisID, dialogType);
    btnRunProcess = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getFullAlignedStack();
  }

  abstract String getHeaderTitle();

  void addListeners() {
    btnRunProcess.addActionListener(actionListener);
    btn3dmodFull.addActionListener(actionListener);
  }

  Component getComponent() {
    return pnlRoot;
  }

  void createPanel() {
    // Initialize
    btnRunProcess.setContainer(this);
    btnRunProcess.setDeferred3dmodButton(btn3dmodFull);
    btnRunProcess.setSize();
    btn3dmodFull.setSize();
    // Local panels
    SpacedPanel pnlButtons = SpacedPanel.getInstance();
    // Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(header.getContainer());
    pnlRoot.add(pnlBody.getContainer());
    UIUtilities.alignComponentsX(pnlRoot, Component.LEFT_ALIGNMENT);
    // Body Panel
    pnlBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBody.add(newstackAndBlendmontParamPanel.getComponent());
    pnlBody.add(pnlButtons);
    // Button panel
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlButtons.add(btnRunProcess.getComponent());
    pnlButtons.add(btn3dmodFull.getComponent());
  }

  private void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  void done() {
    btnRunProcess.removeActionListener(actionListener);
  }

  FiducialessParams getFiducialessParams() {
    return newstackAndBlendmontParamPanel;
  }

  final void setParameters(ReconScreenState screenState) {
    header.setState(screenState.getNewstHeaderState());
    btnRunProcess.setButtonState(screenState.getButtonState(btnRunProcess
        .getButtonStateKey()));
  }

  final void getParameters(ReconScreenState screenState) {
    header.getState(screenState.getNewstHeaderState());
  }

  public final void setParameters(ConstNewstParam newstParam) {
    newstackAndBlendmontParamPanel.setParameters(newstParam);
  }

  // Copy the newstack parameters from the GUI to the NewstParam object
  public final boolean getParameters(NewstParam newstParam, final boolean doValidation)
      throws FortranInputSyntaxException, InvalidParameterException, IOException {
    return newstackAndBlendmontParamPanel.getParameters(newstParam, doValidation);
  }

  public final void setParameters(BlendmontParam param) {
    newstackAndBlendmontParamPanel.setParameters(param);
  }

  // Copy the newstack parameters from the GUI to the NewstParam object
  public final boolean getParameters(BlendmontParam param, final boolean doValidation)
      throws FortranInputSyntaxException, InvalidParameterException, IOException {
    return newstackAndBlendmontParamPanel.getParameters(param, doValidation);
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * Must save data from the two instances under separate keys.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    newstackAndBlendmontParamPanel.getParameters(metaData);
  }

  /**
   * Must save data from the two instances under separate keys.
   * @param metaData
   */
  void setParameters(ConstMetaData metaData) {
    newstackAndBlendmontParamPanel.setParameters(metaData);
  }

  void setFiducialessAlignment(boolean input) {
    newstackAndBlendmontParamPanel.setFiducialessAlignment(input);
  }

  void setImageRotation(String input) {
    newstackAndBlendmontParamPanel.setImageRotation(input);
  }

  public boolean validate() {
    return true;
  }

  String getRunProcessButtonActionCommand() {
    return btnRunProcess.getActionCommand();
  }

  String get3dmodFullButtonActionCommand() {
    return btn3dmodFull.getActionCommand();
  }

  ProcessResultDisplay getRunProcessResultDisplay() {
    return btnRunProcess;
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  abstract void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions);

  public void expand(GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (header != null) {
      if (header.equalsAdvancedBasic(button)) {
        newstackAndBlendmontParamPanel.updateAdvanced(button.isExpanded());
      }
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  void updateAdvanced(boolean advanced) {
    newstackAndBlendmontParamPanel.updateAdvanced(advanced);
  }

  void setToolTipText() {
    btnRunProcess
        .setToolTipText("Generate the complete aligned stack for input into the "
            + "tilt process.");
    btn3dmodFull.setToolTipText("Open the complete aligned stack in 3dmod");
  }

  private static final class NewstackOrBlendmontPanelActionListener implements
      ActionListener {
    private final NewstackOrBlendmontPanel adaptee;

    private NewstackOrBlendmontPanelActionListener(final NewstackOrBlendmontPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
