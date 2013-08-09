package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;

import etomo.ApplicationManager;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.type.AxisID;
import etomo.type.ConstIntKeyList;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.IntKeyList;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.InvalidParameterException;

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
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:24:51  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.  Added
 * <p> setEnabled(boolean).
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.2  2010/04/28 16:48:38  sueh
 * <p> bug# 1344 In getParameters(TiltParam) calling param.setCommandMode.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class TrialTiltPanel implements Expandable, Run3dmodButtonContainer,
    TrialTiltDisplay {
  public static final String rcsid = "$Id$";

  private final TrialTiltActionListener actionListener = new TrialTiltActionListener(this);
  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final SpacedPanel pnlBody = SpacedPanel.getInstance();
  private final JLabel lblTrialTomogramName = new JLabel("Trial tomogram filename: ");
  private final ComboBox cmboTrialTomogramName =  ComboBox.getUnlabeledInstance(lblTrialTomogramName);
  private final MultiLineButton btnTrial = new MultiLineButton("Generate Trial Tomogram");
  private final Run3dmodButton btn3dmodTrial = Run3dmodButton.get3dmodInstance(
      "View Trial in 3dmod", this);
  private final MultiLineButton btnUseTrial;

  private final PanelHeader header;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;
  private final TrialTiltParent parent;

  // A way to know what items are currently in the trial tomogram combo box.
  // It is set from MetaData, which is assumed to be not null.
  private IntKeyList trialTomogramList = null;
  private boolean resume = false;

  private TrialTiltPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, TrialTiltParent parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
    header = PanelHeader.getInstance("Trial Tilt", this, dialogType);
    ProcessResultDisplayFactory displayFactory = manager
        .getProcessResultDisplayFactory(axisID);
    btnUseTrial = (MultiLineButton) displayFactory.getUseTrialTomogram();
  }

  static TrialTiltPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, TrialTiltParent parent) {
    TrialTiltPanel instance = new TrialTiltPanel(manager, axisID, dialogType, parent);
    instance.createPanel();
    instance.updateDisplay();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  /**
   * Layout the trial tomogram panel
   */
  private void createPanel() {
    // Initialize
    cmboTrialTomogramName.setEditable(true);
    btnTrial.setSize();
    btn3dmodTrial.setSize();
    btnUseTrial.setSize();
    // Local panels
    SpacedPanel northPanel = SpacedPanel.getInstance();
    SpacedPanel buttonPanel = SpacedPanel.getInstance();
    // Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(header);
    pnlRoot.add(pnlBody.getContainer());
    // Body panel
    pnlBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBody.addRigidArea();
    pnlBody.add(northPanel);
    pnlBody.add(buttonPanel);
    // North panel
    northPanel.setBoxLayout(BoxLayout.X_AXIS);
    northPanel.add(lblTrialTomogramName);
    northPanel.add(cmboTrialTomogramName.getComponent());
    // Button panel
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    buttonPanel.add(btnTrial);
    buttonPanel.add(btn3dmodTrial);
    buttonPanel.add(btnUseTrial);
  }

  private void addListeners() {
    btnTrial.addActionListener(actionListener);
    btn3dmodTrial.addActionListener(actionListener);
    btnUseTrial.addActionListener(actionListener);
  }

  public boolean allowTiltComSave() {
    return true;
  }

  public void msgTiltComSaved() {
  }

  Component getComponent() {
    return pnlRoot;
  }

  void done() {
    btnUseTrial.removeActionListener(actionListener);
  }

  void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  public final void setResume(boolean resume) {
    this.resume = resume;
    updateDisplay();
  }

  private void updateDisplay() {
    cmboTrialTomogramName.setComboBoxEnabled(!resume);
    btnTrial.setEnabled(!resume);
    btn3dmodTrial.setEnabled(!resume);
    btnUseTrial.setEnabled(!resume);
  }

  void setTrialTomogramNameList(ConstIntKeyList input) {
    IntKeyList.Walker walker = input.getWalker();
    while (walker.hasNext()) {
      cmboTrialTomogramName.addItem(walker.nextString());
    }
  }

  void addToTrialTomogramName(String trialTomogramName) {
    cmboTrialTomogramName.addItem(trialTomogramName);
  }

  public void addTrialTomogramName(String trialTomogramName) {
    trialTomogramList.add(trialTomogramName);
    addToTrialTomogramName(trialTomogramName);
  }

  public boolean containsTrialTomogramName(String trialTomogramName) {
    return trialTomogramList.containsValue(trialTomogramName);
  }

  /**
   * Return the selected trial tomogram name
   * 
   * @return
   */
  public String getTrialTomogramName() {
    String trialTomogramName = (String) cmboTrialTomogramName.getSelectedItem();
    if (trialTomogramName == null) {
      trialTomogramName = "";
    }
    return trialTomogramName;
  }

  public boolean getParameters(TiltParam tiltParam, final boolean doValidation)
      throws NumberFormatException, InvalidParameterException, IOException {
    tiltParam.setCommandMode(TiltParam.Mode.TRIAL_TILT);
    return parent.getParameters(tiltParam, doValidation);
  }

  void getParameters(ReconScreenState screenState) {
    header.getState(screenState.getTomoGenTrialTiltHeaderState());
  }

  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    metaData.setTomoGenTrialTomogramNameList(axisID, trialTomogramList);
  }

  public boolean getParameters(final SplittiltParam param, final boolean doValidation) {
    return parent.getParameters(param, doValidation);
  }

  void setParameters(ConstMetaData metaData) {
    trialTomogramList = metaData.getTomoGenTrialTomogramNameList(axisID);
    setTrialTomogramNameList(trialTomogramList);
  }

  final void setParameters(ReconScreenState screenState) {
    header.setState(screenState.getTomoGenTrialTiltHeaderState());
    btnUseTrial
        .setButtonState(screenState.getButtonState(btnUseTrial.getButtonStateKey()));
  }

  public void expand(final GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
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
  void action(final String command, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnTrial.getActionCommand())) {
      manager.trialAction(
          btnTrial,
          null,
          this,
          axisID,
          dialogType,
          manager.getProcessingMethodMediator(axisID).getRunMethodForProcessInterface(
              parent.getProcessingMethod()));
    }
    else if (command.equals(btnUseTrial.getActionCommand())) {
      manager.commitTestVolume(btnUseTrial, axisID, this);
    }
    else if (command.equals(btn3dmodTrial.getActionCommand())) {
      manager.imodTestVolume(run3dmodMenuOptions, axisID, this);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text = "Current name of trial tomogram, which will be generated, viewed, or"
        + " used by the buttons below.";
    lblTrialTomogramName.setToolTipText(text);
    cmboTrialTomogramName.setToolTipText(TooltipFormatter.INSTANCE.format(text));
    btnTrial
        .setToolTipText("Compute a trial tomogram with the current parameters, using the "
            + "filename in the \" Trial tomogram filename \" box.");
    btn3dmodTrial
        .setToolTipText("View the trial tomogram whose name is shown in \"Trial "
            + "tomogram filename\" box.");
    btnUseTrial
        .setToolTipText("Rename the trial tomogram whose name is shown in the \"Trial "
            + "tomogram filename\" box to be the final tomogram.");
  }

  private static final class TrialTiltActionListener implements ActionListener {
    private final TrialTiltPanel adaptee;

    private TrialTiltActionListener(final TrialTiltPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
