package etomo.ui;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.FindBeads3dParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EnumeratedType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
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
final class FindBeads3dPanel implements FindBeads3dDisplay, Expandable,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  static final String BEAD_SIZE_LABEL = "Bead size";

  private final JPanel pnlRoot = new JPanel();
  private final ActionListener actionListener = new FindBeads3dPanelActionListener(
      this);
  private final SpacedPanel pnlBody = SpacedPanel.getInstance(true);
  private final LabeledTextField ltfBeadSize = new LabeledTextField(
      BEAD_SIZE_LABEL + ": ");
  private final LabeledTextField ltfMinSpacing = new LabeledTextField(
      "Minimum spacing: ");
  private final LabeledTextField ltfGuessNumBeads = new LabeledTextField(
      "Estimated number of beads: ");
  private final LabeledTextField ltfMinRelativeStrength = new LabeledTextField(
      "Minimum peak strength: ");
  private final LabeledTextField ltfThresholdForAveraging = new LabeledTextField(
      "Threshold for averaging: ");
  private final ButtonGroup bgStorageThreshold = new ButtonGroup();
  private final RadioButton rbStorageThresholdSomeBelow = new RadioButton(
      "Store some points below threshold", StorageThresholdEnum.SOME_BELOW,
      bgStorageThreshold);
  private final RadioButton rbStorageThresholdOnlyAbove = new RadioButton(
      "Store only points above threshold", StorageThresholdEnum.ONLY_ABOVE,
      bgStorageThreshold);
  private final RadioTextField rtfStorageThreshold = RadioTextField
      .getInstance("Set threshold for storing: ", bgStorageThreshold);
  private final LabeledTextField ltfMaxNumBeads = new LabeledTextField(
      "Max points to analyze: ");
  private final Run3dmodButton btn3dmodFindBeads3d = Run3dmodButton
      .get3dmodInstance("View Model", this);

  private final Run3dmodButton btnFindBeads3d;
  private final PanelHeader header;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private FindBeads3dPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    header = PanelHeader.getAdvancedBasicInstance("Find Beads 3d", this,
        dialogType, globalAdvancedButton);
    btnFindBeads3d = (Run3dmodButton) manager.getProcessResultDisplayFactory(
        axisID).getFindBeads3d();
  }

  static FindBeads3dPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    FindBeads3dPanel instance = new FindBeads3dPanel(manager, axisID,
        dialogType, globalAdvancedButton);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  static ProcessResultDisplay getFindBeads3dButton(DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Run Findbeads3d",
        dialogType);
  }

  private void addListeners() {
    btnFindBeads3d.addActionListener(actionListener);
    btn3dmodFindBeads3d.addActionListener(actionListener);
  }

  void done() {
    btnFindBeads3d.removeActionListener(actionListener);
  }

  private void createPanel() {
    //Initialize
    btnFindBeads3d.setContainer(this);
    btnFindBeads3d.setDeferred3dmodButton(btn3dmodFindBeads3d);
    btnFindBeads3d.setSize();
    btn3dmodFindBeads3d.setSize();
    //Local panels
    JPanel pnlStorageThreshold = new JPanel();
    SpacedPanel pnlButtons = SpacedPanel.getInstance();
    SpacedPanel pnlA = SpacedPanel.getInstance();
    SpacedPanel pnlB = SpacedPanel.getInstance();
    //Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(header.getContainer());
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(pnlBody.getContainer());
    //Body panel
    pnlBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBody.add(ltfBeadSize.getContainer());
    pnlBody.add(pnlA);
    pnlBody.add(pnlB);
    pnlBody.add(pnlStorageThreshold);
    pnlBody.add(ltfMaxNumBeads.getContainer());
    pnlBody.add(pnlButtons);
    //Panel A
    pnlA.setBoxLayout(BoxLayout.X_AXIS);
    pnlA.add(ltfMinSpacing.getContainer());
    pnlA.add(ltfGuessNumBeads.getContainer());
    //Panel B
    pnlB.setBoxLayout(BoxLayout.X_AXIS);
    pnlB.add(ltfMinRelativeStrength.getContainer());
    pnlB.add(ltfThresholdForAveraging.getContainer());
    //Storage threshold panel
    pnlStorageThreshold.setLayout(new GridLayout(3, 2, 3, 3));
    pnlStorageThreshold.setBorder(new EtchedBorder("Storage Threshold")
        .getBorder());
    pnlStorageThreshold.add(rbStorageThresholdSomeBelow.getComponent());
    pnlStorageThreshold.add(rbStorageThresholdOnlyAbove.getComponent());
    pnlStorageThreshold.add(rtfStorageThreshold.getContainer());
    //Button panel
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlButtons.add(btnFindBeads3d.getComponent());
    pnlButtons.add(btn3dmodFindBeads3d.getComponent());
  }

  Component getComponent() {
    return pnlRoot;
  }

  boolean isAdvanced() {
    return header.isAdvanced();
  }

  public void expand(GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      updateAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  void updateAdvanced(boolean advanced) {
    ltfMinSpacing.setVisible(advanced);
    ltfGuessNumBeads.setVisible(advanced);
    ltfMinRelativeStrength.setVisible(advanced);
    ltfThresholdForAveraging.setVisible(advanced);
    ltfMaxNumBeads.setVisible(advanced);
  }

  void getParameters(ReconScreenState screenState) {
    header.getState(screenState.getStackFindBeads3dHeaderState());
  }

  void setParameters(ReconScreenState screenState) {
    header.setState(screenState.getStackFindBeads3dHeaderState());
    btnFindBeads3d.setButtonState(screenState.getButtonState(btnFindBeads3d
        .getButtonStateKey()));
  }

  void setParameters(ConstFindBeads3dParam param, boolean initialize) {
    if (initialize) {
      //Bead size starts out as unbinned bead diameter is pixels.
      ltfBeadSize.setText(manager.calcUnbinnedBeadDiameterPixels());
      ltfMinSpacing.setText(.9);
      ltfMinRelativeStrength.setText(.05);
    }
    else {
      ltfBeadSize.setText(param.getBeadSize());
      ltfMinSpacing.setText(param.getMinSpacing());
      ltfGuessNumBeads.setText(param.getGuessNumBeads());
      ltfMinRelativeStrength.setText(param.getMinRelativeStrength());
      ltfThresholdForAveraging.setText(param.getThresholdForAveraging());
      //Set StorageThreshold
      ConstEtomoNumber storageThreshold = param.getStorageThreshold();
      StorageThresholdEnum storageThresholdEnum = StorageThresholdEnum
          .getInstance(storageThreshold);
      if (storageThresholdEnum == null) {
        rtfStorageThreshold.setText(storageThreshold);
      }
      else if (storageThresholdEnum == StorageThresholdEnum.SOME_BELOW) {
        rbStorageThresholdSomeBelow.setSelected(true);
      }
      else if (storageThresholdEnum == StorageThresholdEnum.ONLY_ABOVE) {
        rbStorageThresholdOnlyAbove.setSelected(true);
      }
      ltfMaxNumBeads.setText(param.getMaxNumBeads());
    }
  }

  public void getParameters(FindBeads3dParam param) {
    param.setInputFile(FileType.TILT_3D_FIND_OUTPUT
        .getFileName(manager, axisID));
    param.setOutputFile(FileType.FIND_BEADS_3D_OUTPUT_MODEL.getFileName(
        manager, axisID));
    param.setBeadSize(ltfBeadSize.getText());
    param.setMinSpacing(ltfMinSpacing.getText());
    param.setGuessNumBeads(ltfGuessNumBeads.getText());
    param.setMinRelativeStrength(ltfMinRelativeStrength.getText());
    param.setThresholdForAveraging(ltfThresholdForAveraging.getText());
    if (!rtfStorageThreshold.isSelected()) {
      param
          .setStorageThreshold(((RadioButton.RadioButtonModel) bgStorageThreshold
              .getSelection()).getEnumeratedType().getValue());
    }
    else {
      param.setStorageThreshold(rtfStorageThreshold.getText());
    }
    param.setMaxNumBeads(ltfMaxNumBeads.getText());
  }

  public String getBeadSize() {
    return ltfBeadSize.getText();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnFindBeads3d.getActionCommand())) {
      manager.findBeads3d(btnFindBeads3d, null, deferred3dmodButton, axisID,
          run3dmodMenuOptions, dialogType, this);
    }
    else if (command.equals(btn3dmodFindBeads3d.getActionCommand())) {
      manager.imodFindBeads3d(axisID, run3dmodMenuOptions, null,
          FileType.TILT_3D_FIND_OUTPUT.getImodManagerKey(),
          FileType.FIND_BEADS_3D_OUTPUT_MODEL.getFileName(manager, axisID),
          null, dialogType);
    }
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager,
          AutodocFactory.FIND_BEADS_3D, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    ltfBeadSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.BEAD_SIZE_TAG));
    ltfMinSpacing.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.MIN_SPACING_TAG));
    ltfGuessNumBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.GUESS_NUM_BEADS_TAG));
    ltfMinRelativeStrength.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.MIN_RELATIVE_STRENGTH_TAG));
    ltfThresholdForAveraging.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.THRESHOLD_FOR_AVERAGING_TAG));
    rbStorageThresholdSomeBelow.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.STORAGE_THRESHOLD_TAG));
    rbStorageThresholdOnlyAbove.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.STORAGE_THRESHOLD_TAG));
    rtfStorageThreshold.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.STORAGE_THRESHOLD_TAG));
    ltfMaxNumBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        FindBeads3dParam.MAX_NUM_BEADS_TAG));
    btnFindBeads3d
        .setToolTipText("Run findbeads3d to find gold particles in the tomogram.");
    btn3dmodFindBeads3d.setToolTipText("View model of gold particles.");
  }

  private final class FindBeads3dPanelActionListener implements ActionListener {
    private final FindBeads3dPanel adaptee;

    private FindBeads3dPanelActionListener(final FindBeads3dPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class StorageThresholdEnum implements EnumeratedType {
    private static final StorageThresholdEnum SOME_BELOW = new StorageThresholdEnum(
        0);
    private static final StorageThresholdEnum ONLY_ABOVE = new StorageThresholdEnum(
        -1);

    private final EtomoNumber value = new EtomoNumber();

    private StorageThresholdEnum(int value) {
      this.value.set(value);
    }

    private static StorageThresholdEnum getInstance(
        ConstEtomoNumber storageThreshold) {
      if (SOME_BELOW.value.equals(storageThreshold)) {
        return SOME_BELOW;
      }
      if (ONLY_ABOVE.value.equals(storageThreshold)) {
        return ONLY_ABOVE;
      }
      //Don't return default because some values do not belong in one of these
      //categories.
      return null;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public boolean isDefault() {
      if (this == SOME_BELOW) {
        return true;
      }
      return false;
    }
  }
}
