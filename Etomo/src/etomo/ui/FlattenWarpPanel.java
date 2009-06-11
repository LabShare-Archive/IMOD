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
 * <p> Revision 1.1  2009/06/05 02:11:44  sueh
 * <p> bug# 1219 A panel that can run flattenwarp.
 * <p> </p>
 */
final class FlattenWarpPanel implements Run3dmodButtonContainer,
    FlattenWarpDisplay {
  public static final String rcsid = "$Id$";

  private static final String WARP_SPACING_X_LABEL = "Spacing in X";
  private static final String WARP_SPACING_Y_LABEL = "and Y";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final BinnedXY3dmodButton btnMakeSurfaceModel = new BinnedXY3dmodButton(
      "Make Surface Model", this);
  private final CheckBox cbOneSurface = new CheckBox(
      "Contours are all on one surface");
  private final LabeledTextField ltfWarpSpacingX = new LabeledTextField(
      WARP_SPACING_X_LABEL + ": ");
  private final LabeledTextField ltfWarpSpacingY = new LabeledTextField(" "
      + WARP_SPACING_Y_LABEL + ": ");
  private final MultiLineButton btnFlattenWarp = new MultiLineButton(
      "Run Flattenwarp");

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;
  private final FlattenWarpParent parent;

  private FlattenWarpPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, FlattenWarpParent parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
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
  }

  void getParameters(MetaData metaData) {
    metaData.setPostFlattenWarpContoursOnOneSurface(cbOneSurface.isSelected());
    metaData.setPostFlattenWarpSpacingInX(ltfWarpSpacingX.getText());
    metaData.setPostFlattenWarpSpacingInY(ltfWarpSpacingY.getText());
  }

  public boolean getParameters(FlattenWarpParam param) {
    param.setOneSurface(cbOneSurface.isSelected());
    String errorMessage = param.setWarpSpacingX(ltfWarpSpacingX.getText());
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
          btnMakeSurfaceModel.getBinningInXandY(), parent
              .getFileTypeForSurfaceModel());
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
