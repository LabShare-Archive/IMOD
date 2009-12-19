package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.FlattenWarpParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
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
 * <p> Revision 1.4  2009/10/01 18:50:59  sueh
 * <p> bug# 1239 Changed getFlattenWarpDisplay to getFlattenWarpButton.
 * <p>
 * <p> Revision 1.3  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2009/06/11 16:52:52  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Implemented
 * <p> FlattenWarpDisplay.
 * <p>
 * <p> Revision 1.1  2009/06/05 02:11:44  sueh
 * <p> bug# 1219 A panel that can run flattenwarp.
 * <p> </p>
 */
final class FlattenWarpPanel implements Run3dmodButtonContainer,
    FlattenWarpDisplay, SmoothingAssessmentParent {
  public static final String rcsid = "$Id$";

  static final String WARP_SPACING_X_LABEL = "Spacing in X";
  static final String WARP_SPACING_Y_LABEL = "and Y";
  private static final String LAMBDA_FOR_SMOOTHING_LABEL = "Smoothing factor";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final BinnedXY3dmodButton btnMakeSurfaceModel = new BinnedXY3dmodButton(
      "Make Surface Model", this);
  private final CheckBox cbOneSurface = new CheckBox(
      "Contours are all on one surface");
  private final LabeledTextField ltfWarpSpacingX = new LabeledTextField(
      WARP_SPACING_X_LABEL + ": ");
  private final LabeledTextField ltfWarpSpacingY = new LabeledTextField(" "
      + WARP_SPACING_Y_LABEL + ": ");
  private final LabeledTextField ltfLambdaForSmoothing = new LabeledTextField(
      LAMBDA_FOR_SMOOTHING_LABEL + ": ");

  private final SmoothingAssessmentPanel smoothingAssessmentPanel;
  private final MultiLineButton btnFlattenWarp;
  private final FlattenWarpParent parent;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private FlattenWarpPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, FlattenWarpParent parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
    btnFlattenWarp = (MultiLineButton) manager.getProcessResultDisplayFactory(
        axisID).getFlattenWarp();
    smoothingAssessmentPanel = SmoothingAssessmentPanel.getInstance(manager,
        axisID, dialogType, this);
  }

  static FlattenWarpPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType, FlattenWarpParent parent) {
    FlattenWarpPanel instance = new FlattenWarpPanel(manager, axisID,
        dialogType, parent);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  static MultiLineButton getFlattenWarpButton() {
    return new MultiLineButton("Run Flattenwarp");
  }

  private void addListeners() {
    FlattenWarpPanelActionListener actionListener = new FlattenWarpPanelActionListener(
        this);
    btnMakeSurfaceModel.addActionListener(actionListener);
    btnFlattenWarp.addActionListener(actionListener);
  }

  private void createPanel() {
    JPanel pnlOneSurface = new JPanel();
    JPanel pnlWarpSpacing = new JPanel();
    JPanel pnlButtons = new JPanel();
    //initialize
    btnFlattenWarp.setSize();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlRoot.add(btnMakeSurfaceModel.getContainer());
    pnlRoot.add(pnlOneSurface);
    pnlRoot.add(pnlWarpSpacing);
    pnlRoot.add(smoothingAssessmentPanel.getComponent());
    pnlRoot.add(ltfLambdaForSmoothing);
    pnlRoot.add(pnlButtons);
    //One surface panel
    pnlOneSurface.setLayout(new BoxLayout(pnlOneSurface, BoxLayout.Y_AXIS));
    pnlOneSurface.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlOneSurface.add(cbOneSurface);
    //Warp Spacing panel
    pnlWarpSpacing.setLayout(new BoxLayout(pnlWarpSpacing, BoxLayout.X_AXIS));
    pnlWarpSpacing.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlWarpSpacing.add(ltfWarpSpacingX.getContainer());
    pnlWarpSpacing.add(ltfWarpSpacingY.getContainer());
    //Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.Y_AXIS));
    pnlButtons.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlButtons.add(btnFlattenWarp.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void setParameters(ConstMetaData metaData) {
    cbOneSurface.setSelected(metaData.isPostFlattenWarpContoursOnOneSurface());
    ltfWarpSpacingX.setText(metaData.getPostFlattenWarpSpacingInX());
    ltfWarpSpacingY.setText(metaData.getPostFlattenWarpSpacingInY());
    ltfLambdaForSmoothing.setText(metaData.getLambdaForSmoothing());
    smoothingAssessmentPanel.setParameters(metaData);
  }

  void getParameters(MetaData metaData) {
    metaData.setPostFlattenWarpContoursOnOneSurface(cbOneSurface.isSelected());
    metaData.setPostFlattenWarpSpacingInX(ltfWarpSpacingX.getText());
    metaData.setPostFlattenWarpSpacingInY(ltfWarpSpacingY.getText());
    metaData.setLambdaForSmoothing(ltfLambdaForSmoothing.getText());
    smoothingAssessmentPanel.getParameters(metaData);
  }

  public boolean getParameters(FlattenWarpParam param) {
    String errorMessage = param.setLambdaForSmoothing(ltfLambdaForSmoothing
        .getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in "
          + LAMBDA_FOR_SMOOTHING_LABEL + ":  " + errorMessage, "Entry Error",
          axisID, manager.getManagerKey());
      return false;
    }
    param.setOneSurface(cbOneSurface.isSelected());
    errorMessage = param.setWarpSpacingX(ltfWarpSpacingX.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in " + WARP_SPACING_X_LABEL
          + ":  " + errorMessage, "Entry Error", axisID, manager
          .getManagerKey());
      return false;
    }
    errorMessage = param.setWarpSpacingY(ltfWarpSpacingY.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in " + WARP_SPACING_Y_LABEL
          + ":  " + errorMessage, "Entry Error", axisID, manager
          .getManagerKey());
      return false;
    }
    return true;
  }

  public boolean isOneSurface() {
    return cbOneSurface.isSelected();
  }

  public String getWarpSpacingX() {
    return ltfWarpSpacingX.getText();
  }

  public String getWarpSpacingY() {
    return ltfWarpSpacingY.getText();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnMakeSurfaceModel.getActionCommand())) {
      manager.imodMakeSurfaceModel(run3dmodMenuOptions, axisID,
          btnMakeSurfaceModel.getBinningInXandY(), parent.getInputFileType());
    }
    else if (command.equals(btnFlattenWarp.getActionCommand())) {
      manager.flattenWarp(btnFlattenWarp, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType, axisID, this);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.FLATTEN_WARP, axisID,
          manager.getManagerKey());
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    if (autodoc != null) {
      ltfLambdaForSmoothing.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          FlattenWarpParam.LAMBDA_FOR_SMOOTHING_OPTION));
      cbOneSurface.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          FlattenWarpParam.ONE_SURFACE_OPTION));
      ltfWarpSpacingX.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          FlattenWarpParam.WARP_SPACING_X_AND_Y_OPTION));
      ltfWarpSpacingY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          FlattenWarpParam.WARP_SPACING_X_AND_Y_OPTION));
    }
    btnMakeSurfaceModel
        .setButtonToolTipText("Add contours to describe the location of the "
            + "sectioned material.");
    btnFlattenWarp.setToolTipText("Run flattenwarp.");
  }

  private final class FlattenWarpPanelActionListener implements ActionListener {
    private final FlattenWarpPanel adaptee;

    private FlattenWarpPanelActionListener(final FlattenWarpPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
