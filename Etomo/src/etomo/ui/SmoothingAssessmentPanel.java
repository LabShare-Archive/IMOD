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
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
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
 * <p> $Log$ </p>
 */
final class SmoothingAssessmentPanel implements FlattenWarpDisplay,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private static final String LAMBDA_FOR_SMOOTHING_LABEL = "Smoothing factors";

  private final JPanel pnlRoot = new JPanel();
  private final LabeledTextField ltfLambdaForSmoothing = new LabeledTextField(
      LAMBDA_FOR_SMOOTHING_LABEL + ": ");
  private final Run3dmodButton btn3dmod = Run3dmodButton.get3dmodInstance(
      "Open Assessment in 3dmod", this);

  private final Run3dmodButton btnFlattenWarp;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final SmoothingAssessmentParent parent;
  private final DialogType dialogType;

  private SmoothingAssessmentPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, SmoothingAssessmentParent parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.parent = parent;
    this.dialogType = dialogType;
    btnFlattenWarp = (Run3dmodButton) manager.getProcessResultDisplayFactory(
        axisID).getSmoothingAssessment();
    btnFlattenWarp.setContainer(this);
  }

  static SmoothingAssessmentPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType, SmoothingAssessmentParent parent) {
    SmoothingAssessmentPanel instance = new SmoothingAssessmentPanel(manager,
        axisID, dialogType, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  public static ProcessResultDisplay getSmoothingAssessmentButton(
      DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Assess Smoothing Factors", dialogType);
  }

  private void addListeners() {
    ActionListener actionListener = new SmoothingAssessmentActionListener(this);
    btnFlattenWarp.addActionListener(actionListener);
    btn3dmod.addActionListener(actionListener);
  }

  private void createPanel() {
    //initialize
    btnFlattenWarp.setSize();
    btnFlattenWarp.setContainer(this);
    btnFlattenWarp.setDeferred3dmodButton(btn3dmod);
    btn3dmod.setSize();
    //Local panels
    JPanel pnlButtons = new JPanel();
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder("Smoothing Assessment").getBorder());
    pnlRoot.add(ltfLambdaForSmoothing.getContainer());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlButtons);
    //Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(btnFlattenWarp.getComponent());
    pnlButtons.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlButtons.add(btn3dmod.getComponent());
  }

  Component getComponent() {
    return pnlRoot;
  }
  
  void setParameters(ConstMetaData metaData) {
    ltfLambdaForSmoothing.setText(metaData.getLambdaForSmoothingList());
  }

  void getParameters(MetaData metaData) {
    metaData.setLambdaForSmoothingList(ltfLambdaForSmoothing.getText());
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
    param.setMiddleContourFile(FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL
        .getFileName(manager, axisID));
    param.setOneSurface(parent.isOneSurface());
    errorMessage = param.setWarpSpacingX(parent.getWarpSpacingX());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in "
          + FlattenWarpPanel.WARP_SPACING_X_LABEL + ":  " + errorMessage,
          "Entry Error", axisID, manager.getManagerKey());
      return false;
    }
    errorMessage = param.setWarpSpacingY(parent.getWarpSpacingY());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in "
          + FlattenWarpPanel.WARP_SPACING_Y_LABEL + ":  " + errorMessage,
          "Entry Error", axisID, manager.getManagerKey());
      return false;
    }
    return true;
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnFlattenWarp.getActionCommand())) {
      manager.flattenWarp(btnFlattenWarp, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType, axisID, this);
    }
    else if (command.equals(btn3dmod.getActionCommand())) {
      manager.imodViewModel(axisID, FileType.SMOOTHING_ASSESSMENT_OUTPUT_MODEL);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  public void action(Run3dmodButton button,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  void setTooltips() {
    btnFlattenWarp
        .setToolTipText("Run flattenwarp with different smoothing factors.");
    btn3dmod.setToolTipText("Open model created by flattenwarp.");
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
      ltfLambdaForSmoothing
          .setToolTipText("A list of different LambdaForSmoothing values.  "
              + EtomoAutodoc.getTooltip(autodoc,
                  FlattenWarpParam.LAMBDA_FOR_SMOOTHING_OPTION));
    }
  }

  private final class SmoothingAssessmentActionListener implements
      ActionListener {
    private final SmoothingAssessmentPanel adaptee;

    private SmoothingAssessmentActionListener(
        final SmoothingAssessmentPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
