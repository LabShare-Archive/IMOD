package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.SqueezevolParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
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
 * <p> $Log$ </p>
 */
final class SqueezeVolPanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final SqueezeVolPanelActionListener actionListener = new SqueezeVolPanelActionListener(
      this);
  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final ButtonGroup bgInputFile = new ButtonGroup();
  private final RadioButton rbInputFileTrimVol = new RadioButton(
      "Squeeze the trimvol output", bgInputFile);
  private final RadioButton rbInputFileFlattenWarp = new RadioButton(
      "Squeeze the flatten output", bgInputFile);
  private final LabeledTextField ltfReductionFactorXY = new LabeledTextField(
      "Reduction factor in X and Y ");
  private final LabeledTextField ltfReductionFactorZ = new LabeledTextField(
      "in Z ");
  private final CheckBox cbLinearInterpolation = new CheckBox(
      "Linear interpolation");
  private final Run3dmodButton btnImodSqueezedVolume = Run3dmodButton
      .get3dmodInstance("Open Squeezed Volume in 3dmod", this);

  private final Run3dmodButton btnSqueezeVolume;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private SqueezeVolPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    btnSqueezeVolume = (Run3dmodButton) manager.getProcessResultDisplayFactory(
        axisID).getSqueezeVolume();
  }

  static SqueezeVolPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    SqueezeVolPanel instance = new SqueezeVolPanel(manager, axisID, dialogType);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    btnSqueezeVolume.addActionListener(actionListener);
    btnImodSqueezedVolume.addActionListener(actionListener);
  }

  private void createPanel() {
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new BeveledBorder("Squeeze Volume").getBorder());
    //Choose input file
    JPanel pnlInputFile = new JPanel();
    pnlInputFile.setLayout(new BoxLayout(pnlInputFile, BoxLayout.Y_AXIS));
    pnlInputFile.setBorder(new BeveledBorder("Set Input File").getBorder());
    pnlInputFile.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlInputFile.add(rbInputFileTrimVol.getComponent());
    rbInputFileTrimVol.setSelected(true);
    pnlInputFile.add(rbInputFileFlattenWarp.getComponent());
    pnlRoot.add(pnlInputFile);
    //Reduction factor panel
    SpacedPanel pnlReductionFactor = SpacedPanel.getInstance();
    pnlReductionFactor.setBoxLayout(BoxLayout.X_AXIS);
    pnlReductionFactor.add(ltfReductionFactorXY);
    pnlReductionFactor.add(ltfReductionFactorZ);
    pnlRoot.add(pnlReductionFactor);
    //Linear interpolation panel
    cbLinearInterpolation.setAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlRoot.add(cbLinearInterpolation);
    //third component
    SpacedPanel pnlButtons = SpacedPanel.getInstance();
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    btnSqueezeVolume.setContainer(this);
    btnSqueezeVolume.setDeferred3dmodButton(btnImodSqueezedVolume);
    btnSqueezeVolume.setSize();
    pnlButtons.add(btnSqueezeVolume);
    pnlButtons.addHorizontalGlue();
    btnImodSqueezedVolume.setSize();
    pnlButtons.add(btnImodSqueezedVolume);
    pnlRoot.add(pnlButtons);
  }

  private void setToolTipText() {
    rbInputFileTrimVol.setToolTipText("Choose the input file for squeezevol.");
    rbInputFileFlattenWarp.setToolTipText("Choose the input file for squeezevol.");
    ltfReductionFactorXY.setToolTipText("Factor to squeeze by in X and Y.");
    ltfReductionFactorZ.setToolTipText("Factor to squeeze by in Z.");
    cbLinearInterpolation
        .setToolTipText("Use linear instead of quadratic interpolation for transforming the "
            + "volume with Matchvol.");
    btnSqueezeVolume
        .setToolTipText("Squeeze the trimmed volume by the given factors.");
    btnImodSqueezedVolume.setToolTipText("View the squeezed volume.");
  }

  /**
   * Set the panel values with the specified parameters
   * @param squeezevolParam
   */
  public void setParameters(ConstSqueezevolParam squeezevolParam) {
    ltfReductionFactorXY.setText(squeezevolParam.getReductionFactorX()
        .toString());
    if (manager.isSqueezevolFlipped()) {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorZ()
          .toString());
    }
    else {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorY()
          .toString());
    }
    cbLinearInterpolation.setSelected(squeezevolParam.isLinearInterpolation());
  }

  void getParameters(MetaData metaData) {
    metaData.setPostSqueezeVolInputTrimVol(rbInputFileTrimVol.isSelected());
  }

  void setParameters(ConstMetaData metaData) {
    rbInputFileTrimVol.setSelected(metaData.isPostSqueezeVolInputTrimVol());
    if (!rbInputFileTrimVol.isSelected()) {
      rbInputFileFlattenWarp.setSelected(true);
    }
  }

  /**
   * Get the panel values
   * @param squeezevolParam
   */
  public void getParameters(SqueezevolParam squeezevolParam) {
    squeezevolParam.setReductionFactorX(ltfReductionFactorXY.getText());
    boolean flipped = squeezevolParam.setFlipped(manager.isTrimvolFlipped());
    if (flipped) {
      squeezevolParam.setReductionFactorY(ltfReductionFactorXY.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorZ.getText());
    }
    else {
      squeezevolParam.setReductionFactorY(ltfReductionFactorZ.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorXY.getText());
    }
    squeezevolParam.setLinearInterpolation(cbLinearInterpolation.isSelected());
    if (rbInputFileTrimVol.isSelected()) {
      squeezevolParam.setInputFile(FileType.TRIM_VOL_OUTPUT);
    }
    else {
      squeezevolParam.setInputFile(FileType.FLATTEN_OUTPUT);
    }
  }

  public void setParameters(ReconScreenState screenState) {
    btnSqueezeVolume.setButtonState(screenState.getButtonState(btnSqueezeVolume
        .getButtonStateKey()));
  }

  void done() {
    btnSqueezeVolume.removeActionListener(actionListener);
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnSqueezeVolume.getActionCommand())) {
      FileType fileType;
      if (rbInputFileTrimVol.isSelected()) {
        fileType = FileType.TRIM_VOL_OUTPUT;
      }
      else {
        fileType = FileType.SQUEEZE_VOL_OUTPUT;
      }
      manager.squeezevol(btnSqueezeVolume, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType);
    }
    else if (command.equals(btnImodSqueezedVolume.getActionCommand())) {
      manager.imodSqueezedVolume(run3dmodMenuOptions, axisID);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  private final class SqueezeVolPanelActionListener implements ActionListener {
    private final SqueezeVolPanel adaptee;

    private SqueezeVolPanelActionListener(final SqueezeVolPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
