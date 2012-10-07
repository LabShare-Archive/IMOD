package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.ToolsManager;
import etomo.comscript.FlattenWarpParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
 * <p> Revision 1.3  2011/02/22 19:30:05  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:18:34  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2010/05/27 16:52:07  sueh
 * <p> bug# 1378 Changed LAMBDA_FOR_SMOOTHING_LABEL.  In createPanel set ltfLambdaForSmoothing to its default.  In setParameters checking that metaData.lambdaForSmoothing is not empty before using it.
 * <p>
 * <p> Revision 1.3  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.2  2010/01/12 22:09:01  sueh
 * <p> bug# 1206 Added SmoothingAssessmentPanel.done.
 * <p>
 * <p> Revision 1.1  2009/12/19 01:19:33  sueh
 * <p> bug# 1294 Panel for running flattenwarp with multiple
 * <p> lambdaForSmoothing values.
 * <p> </p>
 */
final class SmoothingAssessmentPanel implements FlattenWarpDisplay,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private static final String LAMBDA_FOR_SMOOTHING_LABEL = "Smoothing factors to try";
  static final String FLATTEN_WARP_LABEL = "Run Flattenwarp to Assess Smoothing";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final LabeledTextField ltfLambdaForSmoothing = new LabeledTextField(
      FieldType.FLOATING_POINT_ARRAY, LAMBDA_FOR_SMOOTHING_LABEL + ": ");
  private final Run3dmodButton btn3dmod = Run3dmodButton.get3dmodInstance(
      "Open Assessment in 3dmod", this);
  private final ActionListener actionListener = new SmoothingAssessmentActionListener(
      this);

  private final Run3dmodButton btnFlattenWarp;
  private final BaseManager manager;
  private final ApplicationManager applicationManager;
  private final ToolsManager toolsManager;
  private final AxisID axisID;
  private final SmoothingAssessmentParent parent;
  private final DialogType dialogType;
  private final PanelId panelId;

  private SmoothingAssessmentPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final PanelId panelId,
      final SmoothingAssessmentParent parent) {
    this.manager = manager;
    applicationManager = manager;
    toolsManager = null;
    this.axisID = axisID;
    this.parent = parent;
    this.dialogType = dialogType;
    this.panelId = panelId;
    btnFlattenWarp = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getSmoothingAssessment();
    btnFlattenWarp.setContainer(this);
  }

  private SmoothingAssessmentPanel(final ToolsManager manager, final AxisID axisID,
      final DialogType dialogType, final PanelId panelId,
      final SmoothingAssessmentParent parent) {
    this.manager = manager;
    applicationManager = null;
    toolsManager = manager;
    this.axisID = axisID;
    this.parent = parent;
    this.dialogType = dialogType;
    this.panelId = panelId;
    btnFlattenWarp = Run3dmodButton.getDeferred3dmodInstance(FLATTEN_WARP_LABEL, this);
    btnFlattenWarp.setContainer(this);
  }

  static SmoothingAssessmentPanel getPostInstance(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType, final PanelId panelId,
      final SmoothingAssessmentParent parent) {
    SmoothingAssessmentPanel instance = new SmoothingAssessmentPanel(manager, axisID,
        dialogType, panelId, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  static SmoothingAssessmentPanel getToolsInstance(final ToolsManager manager,
      final AxisID axisID, final DialogType dialogType, final PanelId panelId,
      final SmoothingAssessmentParent parent) {
    SmoothingAssessmentPanel instance = new SmoothingAssessmentPanel(manager, axisID,
        dialogType, panelId, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    btnFlattenWarp.addActionListener(actionListener);
    btn3dmod.addActionListener(actionListener);
  }

  void done() {
    btnFlattenWarp.removeActionListener(actionListener);
  }

  private void createPanel() {
    // initialize
    btnFlattenWarp.setSize();
    btnFlattenWarp.setContainer(this);
    btnFlattenWarp.setDeferred3dmodButton(btn3dmod);
    btn3dmod.setSize();
    ltfLambdaForSmoothing
        .setText(FlattenWarpParam.LAMBDA_FOR_SMOOTHING_ASSESSMENT_DEFAULT);
    // Local panels
    JPanel pnlButtons = new JPanel();
    // root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder("Smoothing Assessment").getBorder());
    pnlRoot.add(ltfLambdaForSmoothing.getContainer());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlButtons);
    // Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(btnFlattenWarp.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlButtons.add(btn3dmod.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void setParameters(final ConstMetaData metaData) {
    if (!metaData.isLambdaForSmoothingListEmpty()) {
      ltfLambdaForSmoothing.setText(metaData.getLambdaForSmoothingList());
    }
  }

  void getParameters(final MetaData metaData) {
    metaData.setLambdaForSmoothingList(ltfLambdaForSmoothing.getText());
  }

  private boolean validateFlattenWarp() {
    if (ltfLambdaForSmoothing.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(manager, LAMBDA_FOR_SMOOTHING_LABEL
          + " is required.", "Entry Error", axisID);
      return false;
    }
    return true;
  }

  public boolean getParameters(final FlattenWarpParam param, final boolean doValidation) {
    try {
      String errorMessage = param.setLambdaForSmoothing(ltfLambdaForSmoothing
          .getText(doValidation));
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Error in "
            + LAMBDA_FOR_SMOOTHING_LABEL + ":  " + errorMessage, "Entry Error", axisID);
        return false;
      }
      param.setMiddleContourFile(FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL.getFileName(
          manager, axisID));
      param.setOneSurface(parent.isOneSurface());
      errorMessage = param.setWarpSpacingX(parent.getWarpSpacingX(doValidation));
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Error in "
            + FlattenVolumePanel.WARP_SPACING_X_LABEL + ":  " + errorMessage,
            "Entry Error", axisID);
        return false;
      }
      errorMessage = param.setWarpSpacingY(parent.getWarpSpacingY(doValidation));
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Error in "
            + FlattenVolumePanel.WARP_SPACING_Y_LABEL + ":  " + errorMessage,
            "Entry Error", axisID);
        return false;
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  private void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (panelId == PanelId.POST_FLATTEN_VOLUME) {
      if (command.equals(btnFlattenWarp.getActionCommand())) {
        if (validateFlattenWarp()) {
          applicationManager.flattenWarp(btnFlattenWarp, null, deferred3dmodButton,
              run3dmodMenuOptions, dialogType, axisID, this);
        }
      }
      else if (command.equals(btn3dmod.getActionCommand())) {
        applicationManager.imodViewModel(axisID,
            FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL);
      }
      else {
        throw new IllegalStateException("Unknown command " + command);
      }
    }
    else if (panelId == PanelId.TOOLS_FLATTEN_VOLUME) {
      if (command.equals(btnFlattenWarp.getActionCommand())) {
        if (validateFlattenWarp()) {
          toolsManager.flattenWarp(btnFlattenWarp, null, deferred3dmodButton,
              run3dmodMenuOptions, dialogType, axisID, this);
        }
      }
      else if (command.equals(btn3dmod.getActionCommand())) {
        toolsManager.imodViewModel(axisID, FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL);
      }
      else {
        throw new IllegalStateException("Unknown command " + command);
      }
    }
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  void setTooltips() {
    btnFlattenWarp.setToolTipText("Run flattenwarp with different smoothing factors.");
    btn3dmod.setToolTipText("Open model created by flattenwarp.");
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.FLATTEN_WARP, axisID);
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
      ltfLambdaForSmoothing
          .setToolTipText("A list of different LambdaForSmoothing values.  "
              + EtomoAutodoc.getTooltip(autodoc,
                  FlattenWarpParam.LAMBDA_FOR_SMOOTHING_OPTION));
    }
  }

  private final class SmoothingAssessmentActionListener implements ActionListener {
    private final SmoothingAssessmentPanel adaptee;

    private SmoothingAssessmentActionListener(final SmoothingAssessmentPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
