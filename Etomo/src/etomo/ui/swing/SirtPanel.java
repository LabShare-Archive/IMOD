package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
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
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.2  2010/12/05 05:17:34  sueh
* <p> bug# 1421 Main class for running SIRT.
* <p>
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p> </p>
*/
final class SirtPanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbSubarea = new CheckBox("Subarea");
  private final LabeledTextField ltfOffsetInY = new LabeledTextField(" Offset in Y: ");
  private final LabeledTextField ltfSizeInXAndY = new LabeledTextField(
      "Size in X and Y: ");
  private final RadialPanel radiusAndSigmaPanel = RadialPanel.getInstance();
  private final LabeledTextField ltfLeaveIterations = new LabeledTextField(
      "Iteration #'s to retain: ");
  private final CheckBox cbScaleToIntegers = new CheckBox(
      "Scale retained volumes to integers");
  private final ButtonGroup bgStartingIteration = new ButtonGroup();
  private final RadioButton rbStartFromZero = new RadioButton("Start from beginning",
      bgStartingIteration);
  private final RadioButton rbResumeFromLastIteration = new RadioButton(
      "Resume from last iteration", bgStartingIteration);
  private final RadioButton rbResumeFromIteration = new RadioButton(
      "Go back, resume from iteration:", bgStartingIteration);
  private final ComboBox cmbResumeFromIteration = new ComboBox(rbResumeFromIteration
      .getText());
  private final ActionListener listener = new SirtActionListener(this);
  private final Run3dmodButton btn3dmodSirt = Run3dmodButton.get3dmodInstance(
      "View Tomogram In 3dmod", this);

  private final TiltPanel tiltPanel;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final Run3dmodButton btnSirtsetup;
  private final MultiLineButton btnUseSirt;

  private SirtPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton) {
    this.axisID = axisID;
    this.manager = manager;
    tiltPanel = TiltPanel.getSirtInstance(manager, axisID, dialogType,
        globalAdvancedButton);
    ProcessResultDisplayFactory factory = manager.getProcessResultDisplayFactory(axisID);
    btnSirtsetup = (Run3dmodButton) factory.getSirtsetup();
    btnUseSirt = (MultiLineButton) factory.getUseSirt();
  }

  static SirtPanel getInstance(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton) {
    SirtPanel instance = new SirtPanel(manager, axisID, dialogType, globalAdvancedButton);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  Component getRoot() {
    return pnlRoot;
  }

  private void createPanel() {
    //initialize
    SpacedPanel pnlSubarea = SpacedPanel.getInstance();
    JPanel pnlSizeAndOffset = new JPanel();
    SpacedPanel pnlSirtParams = SpacedPanel.getInstance();
    SpacedPanel pnlStartingIteration = SpacedPanel.getInstance();
    JPanel pnlResumeFromIteration = new JPanel();
    JPanel pnlButtons = new JPanel();
    btnSirtsetup.setSize();
    btnSirtsetup.setContainer(this);
    btnSirtsetup.setDeferred3dmodButton(btn3dmodSirt);
    btn3dmodSirt.setSize();
    btnUseSirt.setSize();
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(tiltPanel.getRoot());
    pnlRoot.add(pnlSubarea.getContainer());
    pnlRoot.add(pnlSirtParams.getContainer());
    pnlRoot.add(pnlButtons);
    //SIRT panel
    //Subarea panel
    pnlSubarea.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSubarea.setBorder(BorderFactory.createEtchedBorder());
    pnlSubarea.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlSubarea.add(cbSubarea);
    pnlSubarea.add(pnlSizeAndOffset);
    //Offset and size panel
    pnlSizeAndOffset.setLayout(new BoxLayout(pnlSizeAndOffset, BoxLayout.X_AXIS));
    pnlSizeAndOffset.add(ltfSizeInXAndY.getContainer());
    pnlSizeAndOffset.add(ltfOffsetInY.getContainer());
    //SIRT params panel
    pnlSirtParams.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSirtParams.setBorder(new EtchedBorder("SIRT").getBorder());
    pnlSirtParams.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlSirtParams.add(radiusAndSigmaPanel.getRoot());
    pnlSirtParams.add(ltfLeaveIterations.getContainer());
    pnlSirtParams.add(cbScaleToIntegers);
    pnlSirtParams.add(pnlStartingIteration);
    //Starting iteration panel
    pnlStartingIteration.setBoxLayout(BoxLayout.Y_AXIS);
    pnlStartingIteration.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlStartingIteration.add(rbStartFromZero.getComponent());
    pnlStartingIteration.add(rbResumeFromLastIteration.getComponent());
    pnlStartingIteration.add(pnlResumeFromIteration);
    //Resume from iteration
    pnlResumeFromIteration.setLayout(new BoxLayout(pnlResumeFromIteration,
        BoxLayout.X_AXIS));
    pnlResumeFromIteration.add(rbResumeFromIteration.getComponent());
    pnlResumeFromIteration.add(cmbResumeFromIteration);
    //Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(btnSirtsetup.getComponent());
    pnlButtons.add(btn3dmodSirt.getComponent());
    pnlButtons.add(btnUseSirt.getComponent());
  }

  private void addListeners() {
    rbStartFromZero.addActionListener(listener);
    rbResumeFromLastIteration.addActionListener(listener);
    rbResumeFromIteration.addActionListener(listener);
    cbSubarea.addActionListener(listener);
    btnSirtsetup.addActionListener(listener);
    btn3dmodSirt.addActionListener(listener);
    btnUseSirt.addActionListener(listener);
  }

  void updateAdvanced(final boolean advanced) {
    tiltPanel.updateAdvanced(advanced);
  }

  private void updateDisplay() {
    boolean resumeFromIteration = rbResumeFromLastIteration.isSelected()
        || rbResumeFromIteration.isSelected();
    boolean subarea = cbSubarea.isSelected();
    tiltPanel.setEnabled(!resumeFromIteration);
    cbSubarea.setEnabled(!resumeFromIteration);
    ltfOffsetInY.setEnabled(subarea && !resumeFromIteration);
    ltfSizeInXAndY.setEnabled(subarea && !resumeFromIteration);
  }

  final void done() {
    tiltPanel.done();
    btnSirtsetup.removeActionListener(listener);
    btnUseSirt.removeActionListener(listener);
  }

  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    tiltPanel.getParameters(metaData);
  }

  void getParameters(ReconScreenState screenState) {
    tiltPanel.getParameters(screenState);
    btnSirtsetup.setButtonState(screenState.getButtonState(btnSirtsetup
        .getButtonStateKey()));
    btnUseSirt.setButtonState(screenState.getButtonState(btnUseSirt.getButtonStateKey()));
  }

  void setParameters(ConstMetaData metaData) {
    tiltPanel.setParameters(metaData);
  }

  void setParameters(ConstTiltParam tiltParam, boolean initialize) {
    tiltPanel.setParameters(tiltParam, initialize);
  }

  final void setParameters(ReconScreenState screenState) {
    tiltPanel.setParameters(screenState);
  }

  TiltDisplay getTiltDisplay() {
    return tiltPanel;
  }

  public void setTiltState(TomogramState state, ConstMetaData metaData) {
    tiltPanel.setState(state, metaData);
  }

  public final void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param actionCommand
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  final void action(final String actionCommand,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(btnSirtsetup.getActionCommand())) {

    }
    else if (actionCommand.equals(btn3dmodSirt.getActionCommand())) {

    }
    else if (actionCommand.equals(btnUseSirt.getActionCommand())) {

    }
    else {
      updateDisplay();
    }
  }

  /**
   * Right mouse button context menu
   */
  void popUpContextMenu(final String anchor, final Component rootPanel,
      final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Sirtsetup", "3dmod" };
    String[] manPage = { "sirtsetup", "3dmod.html" };
    String[] logFileLabel = { "Sirtsetup" };
    String[] logFile = new String[1];
    logFile[0] = "sirtsetup" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, anchor,
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  private static final class SirtActionListener implements ActionListener {
    private final SirtPanel adaptee;

    private SirtActionListener(final SirtPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
