package etomo.ui;

import java.awt.Component;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.storage.TaAnglesLog;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.InvalidParameterException;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2000</p>
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
 * <p> Revision 3.5  2010/03/05 04:04:37  sueh
 * <p> bug# 1319 Added getParamters(MetaData) to make sure that the parent
 * <p> version of this function cannot be called.
 * <p>
 * <p> Revision 3.4  2010/03/03 05:08:16  sueh
 * <p> bug# 1311 Changed FileType.NEWST_OR_BLEND_OUTPUT to
 * <p> ALIGNED_STACK.  Added file types for patch tracking.
 * <p>
 * <p> Revision 3.3  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.2  2009/09/21 18:09:23  sueh
 * <p> bug# 1267 TiltParam must know what the input file is to be correct, so set
 * <p> the input file in getParameters(TiltParam) before calling
 * <p> super.getParameters.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:24  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class Tilt3dFindPanel extends AbstractTiltPanel {
  public static final String rcsid = "$Id$";

  private static final String CENTER_TO_CENTER_THICKNESS_LABEL = "Center to center thickness";
  private static final String ADDITION_UNBINNED_DIAMETERS_TO_ADD = "Additional unbinned diameters to add ";
  static final String TILT_3D_FIND_LABEL = "Align and Build Tomogram";

  private final LabeledTextField ltfCenterToCenterThickness = new LabeledTextField(
      CENTER_TO_CENTER_THICKNESS_LABEL + ": ");
  private final LabeledTextField ltfAdditionalDiameters = new LabeledTextField(
      ADDITION_UNBINNED_DIAMETERS_TO_ADD + ": ");

  private final Tilt3dFindParent parent;
  private final Component extraButton;

  private Tilt3dFindPanel(final ApplicationManager manager,
      final AxisID axisID, DialogType dialogType, Tilt3dFindParent parent,
      Component extraButton) {
    super(manager, axisID, dialogType, parent, null);
    this.parent = parent;
    this.extraButton = extraButton;
    //Change some labels.
    ltfZShift.setLabel("Added Z Shift: ");
    ltfTomoThickness.setLabel("Thickness: ");
  }

  static Tilt3dFindPanel getInstance(final ApplicationManager manager,
      final AxisID axisID, DialogType dialogType, Tilt3dFindParent parent,
      Component extraButton) {
    Tilt3dFindPanel instance = new Tilt3dFindPanel(manager, axisID, dialogType,
        parent, extraButton);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  /**
   * Get the correction tilt buttn 
   */
  Run3dmodButton getTiltButton(final ApplicationManager manager,
      final AxisID axisID) {
    return (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getTilt3dFind();
  }

  static Run3dmodButton getTilt3dFindButton(final DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance(TILT_3D_FIND_LABEL,
        dialogType);
  }

  /**
   * Completely different panel.
   */
  void createPanel() {
    //Initialize
    initializePanel();
    //Informational fields should not be editable.
    ltfCenterToCenterThickness.setEditable(false);
    ltfAdditionalDiameters.setEditable(false);
    //Local panels
    JPanel pnlParallelProcess = new JPanel();
    SpacedPanel pnlA = SpacedPanel.getInstance();
    JPanel pnlButtons = new JPanel();
    JPanel pnlUseGpu = new JPanel();
    //Root panel
    SpacedPanel pnlRoot = getRootPanel();
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlParallelProcess);
    pnlRoot.add(pnlUseGpu);
    pnlRoot.add(ltfCenterToCenterThickness);
    pnlRoot.add(ltfAdditionalDiameters);
    pnlRoot.add(pnlA);
    pnlRoot.add(pnlButtons);
    //Parallel process panel
    pnlParallelProcess.setLayout(new BoxLayout(pnlParallelProcess,
        BoxLayout.X_AXIS));
    pnlParallelProcess.add(getParallelProcessCheckBox());
    pnlParallelProcess.add(Box.createHorizontalGlue());
    //use GPU panel
    pnlUseGpu.setLayout(new BoxLayout(pnlUseGpu, BoxLayout.X_AXIS));
    pnlUseGpu.add(getUseGpuCheckBox());
    pnlUseGpu.add(Box.createHorizontalGlue());
    //Panel A
    pnlA.setBoxLayout(BoxLayout.X_AXIS);
    pnlA.add(ltfTomoThickness);
    pnlA.add(ltfZShift);
    //Buttons panel
    pnlButtons.add(getTiltButton());
    if (extraButton != null) {
      pnlButtons.add(extraButton);
    }
    pnlButtons.add(get3dmodTomogramButton());
  }

  /**
   * Setting the usual parameters, then also setting input file, output file,
   * and process name.
   */
  public boolean getParameters(final TiltParam param)
      throws NumberFormatException, InvalidParameterException, IOException {
    //param.setThickness(ltfTomoThickness.getText());
    //param.setZShift(ltfZShift.getText());
    if (manager.getState().isStackUsingNewstOrBlend3dFindOutput(axisID)) {
      param.setInputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(
          manager, axisID));
    }
    else {
      param.setInputFile(FileType.ALIGNED_STACK.getFileName(manager, axisID));
    }
    param.setOutputFile(FileType.TILT_3D_FIND_OUTPUT.getFileName(manager,
        axisID));
    param.setProcessName(ProcessName.TILT_3D_FIND);
    super.getParameters(param);
    return true;
  }

  void setParameters(ConstTiltParam param, boolean initialize) {
    super.setParameters(param, initialize);
    //set center to center thickness and additional diameters
    TaAnglesLog taAnglesLog = TaAnglesLog.getInstance(manager
        .getPropertyUserDir(), axisID);
    ConstEtomoNumber centerToCenterThickness = null;
    try {
      centerToCenterThickness = taAnglesLog.getCenterToCenterThickness();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    if (centerToCenterThickness != null && centerToCenterThickness.isValid()) {
      ltfCenterToCenterThickness.setText(centerToCenterThickness);
    }
    int additionalDiameters = 3;
    ltfAdditionalDiameters.setText(additionalDiameters);
    if (initialize) {
      //The first time the dialog is created tilt_3dfind.com is copied from
      //tilt.com and these values are calculated from align log values.
      if (centerToCenterThickness != null && centerToCenterThickness.isValid()
          && !centerToCenterThickness.isNull()) {
        ltfTomoThickness.setText(Math.round(centerToCenterThickness.getFloat()
            + manager.calcUnbinnedBeadDiameterPixels() * additionalDiameters));
      }
      try {
        ltfZShift.setText(taAnglesLog.getIncrementalShiftToCenter());
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * This function in AbstractTiltPanel is oriented towards the TomoGen dialog.
   * Don't call it from the Stack dialog.
   */
  void getParameters(final MetaData metaData)
      throws FortranInputSyntaxException {
  }

  public boolean getParameters(final SplittiltParam param) {
    if (!super.getParameters(param)) {
      return false;
    }
    param.setName(ProcessName.TILT_3D_FIND.toString());
    return true;
  }

  void tilt3dFindAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.tilt3dFindAction(getTiltProcessResultDisplay(), null,
        deferred3dmodButton, run3dmodMenuOptions, this, axisID, dialogType);
  }

  void tiltAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    parent.tilt3dFindAction(deferred3dmodButton, run3dmodMenuOptions);
  }

  void imodTomogramAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.imod(FileType.TILT_3D_FIND_OUTPUT, axisID, run3dmodMenuOptions);
  }

  void setToolTipText() {
    super.setToolTipText();
    ltfCenterToCenterThickness
        .setToolTipText("Used to calculate the thickness of the findbeads3d "
            + "input tomogram.  From the taAngles log.");
    ltfAdditionalDiameters
        .setToolTipText("Used to calculate the thickness of the findbeads3d.");
    ltfTomoThickness
        .setToolTipText("Thickness of tomogram in unbinned pixels.  The default "
            + "is calculated from \""
            + CENTER_TO_CENTER_THICKNESS_LABEL
            + "\" plus \""
            + FindBeads3dPanel.BEAD_SIZE_LABEL
            + "\" multipled by \"" + ADDITION_UNBINNED_DIAMETERS_TO_ADD + "\".");
    ltfZShift
        .setToolTipText("Incremental unbinned shift needed to center range of "
            + "fiducials in Z.  From the taAngles log.");
    setTiltButtonTooltip("If binning has changed, create a separate full aligned "
        + "stack.  Then compute a tomogram (tilt_3dfind.com).");
  }
}
