package etomo.ui.swing;

import java.awt.Component;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.storage.TaAnglesLog;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
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
 * <p> Revision 1.6  2011/04/26 00:04:11  sueh
 * <p> bug# 1416 Moved allowTiltComSave to the parent class, since it returned the default value.
 * <p>
 * <p> Revision 1.5  2011/04/04 17:36:05  sueh
 * <p> bug# 1416 Added allowTiltComSave.  Removed updateDisplay.
 * <p>
 * <p> Revision 1.4  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.3  2010/12/15 17:55:24  sueh
 * <p> bug# 1425 In updateDisplay call super.updateDisplay to correctly enable
 * <p> or disable the undisplayed parts of the panel.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:19:33  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.  Getting rid of some of the panel parents by handling common needs with generic
 * <p> interfaces:  ParallelProcessEnabledDialog.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.10  2010/04/28 16:46:09  sueh
 * <p> bug# 1344 In getParameters(TiltParam) calling param.setCommandMode.
 * <p>
 * <p> Revision 3.9  2010/04/09 03:01:55  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.8  2010/03/27 05:10:37  sueh
 * <p> bug# 1333 Added panel id.
 * <p>
 * <p> Revision 3.7  2010/03/19 02:45:01  sueh
 * <p> bug# 1325 Move set parameter functionality involving the align log from
 * <p> setParameters(ConstTiltParam...) to setParameters(ConstTiltalignParam...).
 * <p> Pulling the Z shift (negated) from align.com when 2 surfaces was not used.
 * <p>
 * <p> Revision 3.6  2010/03/12 04:25:53  sueh
 * <p> bug# 1325 Exposed the use GPU checkbox.
 * <p>
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
  private static final PanelId PANEL_ID = PanelId.TILT_3D_FIND;

  private final LabeledTextField ltfCenterToCenterThickness = new LabeledTextField(
      FieldType.FLOATING_POINT, CENTER_TO_CENTER_THICKNESS_LABEL + ": ");
  private final LabeledTextField ltfAdditionalDiameters = new LabeledTextField(
      FieldType.INTEGER, ADDITION_UNBINNED_DIAMETERS_TO_ADD + ": ");

  private final Tilt3dFindParent parent;
  private final Component extraButton;

  private Tilt3dFindPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final Tilt3dFindParent parent,
      final Component extraButton) {
    super(manager, axisID, dialogType, null, PANEL_ID, false);
    this.parent = parent;
    this.extraButton = extraButton;
    // Change some labels.
    ltfZShift.setLabel("Added Z Shift: ");
    ltfTomoThickness.setLabel("Thickness: ");
  }

  static Tilt3dFindPanel getInstance(final ApplicationManager manager,
      final AxisID axisID, DialogType dialogType, Tilt3dFindParent parent,
      Component extraButton) {
    Tilt3dFindPanel instance = new Tilt3dFindPanel(manager, axisID, dialogType, parent,
        extraButton);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  /**
   * Completely different panel.
   */
  void createPanel() {
    // Initialize
    initializePanel();
    // Informational fields should not be editable.
    ltfCenterToCenterThickness.setEditable(false);
    ltfAdditionalDiameters.setEditable(false);
    // Local panels
    JPanel pnlParallelProcess = new JPanel();
    SpacedPanel pnlA = SpacedPanel.getInstance();
    JPanel pnlButtons = new JPanel();
    JPanel pnlUseGpu = new JPanel();
    // Root panel
    SpacedPanel pnlRoot = getRootPanel();
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlParallelProcess);
    pnlRoot.add(pnlUseGpu);
    pnlRoot.add(ltfCenterToCenterThickness);
    pnlRoot.add(ltfAdditionalDiameters);
    pnlRoot.add(pnlA);
    pnlRoot.add(pnlButtons);
    // Parallel process panel
    pnlParallelProcess.setLayout(new BoxLayout(pnlParallelProcess, BoxLayout.X_AXIS));
    pnlParallelProcess.add(getParallelProcessCheckBox());
    pnlParallelProcess.add(Box.createHorizontalGlue());
    // use GPU panel
    pnlUseGpu.setLayout(new BoxLayout(pnlUseGpu, BoxLayout.X_AXIS));
    pnlUseGpu.add(getUseGpuCheckBox());
    pnlUseGpu.add(Box.createHorizontalGlue());
    // Panel A
    pnlA.setBoxLayout(BoxLayout.X_AXIS);
    pnlA.add(ltfTomoThickness);
    pnlA.add(ltfZShift);
    // Buttons panel
    Component button = getTiltButton();
    if (button != null) {
      pnlButtons.add(button);
    }
    if (extraButton != null) {
      pnlButtons.add(extraButton);
    }
    button = get3dmodTomogramButton();
    if (button != null) {
      pnlButtons.add(button);
    }
  }

  /**
   * Setting the usual parameters, then also setting input file, output file,
   * and process name.
   */
  public boolean getParameters(final TiltParam param, final boolean doValidation)
      throws NumberFormatException, InvalidParameterException, IOException {
    // param.setThickness(ltfTomoThickness.getText());
    // param.setZShift(ltfZShift.getText());
    if (manager.getState().isStackUsingNewstOrBlend3dFindOutput(axisID)) {
      param.setInputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager,
          axisID));
    }
    else {
      param.setInputFile(FileType.ALIGNED_STACK.getFileName(manager, axisID));
    }
    param.setOutputFile(FileType.TILT_3D_FIND_OUTPUT.getFileName(manager, axisID));
    param.setCommandMode(TiltParam.Mode.TILT_3D_FIND);
    param.setProcessName(ProcessName.TILT_3D_FIND);
    if (!super.getParameters(param, doValidation)) {
      return false;
    }
    return true;
  }

  void setParameters(ConstTiltParam param, boolean initialize) {
    super.setParameters(param, initialize);
  }

  /**
   * This mainly pulls data from the align log.
   * @param param
   * @param initialize
   */
  void setParameters(ConstTiltalignParam param, boolean initialize) {
    // set center to center thickness and additional diameters
    TaAnglesLog taAnglesLog = TaAnglesLog.getInstance(manager.getPropertyUserDir(),
        axisID);
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
      // The first time the dialog is created tilt_3dfind.com is copied from
      // tilt.com and these values are calculated from align log values.
      if (centerToCenterThickness != null && centerToCenterThickness.isValid()
          && !centerToCenterThickness.isNull()) {
        ltfTomoThickness.setText(Math.round(centerToCenterThickness.getDouble()
            + manager.calcUnbinnedBeadDiameterPixels() * additionalDiameters));
      }
      try {
        if (param.getSurfacesToAnalyze().getInt() == 2) {
          ltfZShift.setText(taAnglesLog.getIncrementalShiftToCenter());
        }
        else {
          ltfZShift.setText(param.getAxisZShift().getDouble() * -1);
        }
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
  }
  
  /**
   * Get values from meta data that override the com script values.
   * @param metaData
   */
  void setOverrideParameters(final ConstMetaData metaData) {
    if (metaData.isStack3dFindThicknessSet(axisID)) {
      ltfTomoThickness.setText(metaData.getStack3dFindThickness(axisID));
    }
  }

  void getParameters(final MetaData metaData) throws FortranInputSyntaxException {
    metaData.setTiltParallel(axisID, PANEL_ID, isParallelProcess());
  }

  public boolean getParameters(final SplittiltParam param, final boolean doValidation) {
    if (!super.getParameters(param, doValidation)) {
      return false;
    }
    param.setName(ProcessName.TILT_3D_FIND.toString());
    return true;
  }

  void tilt3dFindAction(ProcessResultDisplay processResultDisplay,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.tilt3dFindAction(processResultDisplay, null, deferred3dmodButton,
        run3dmodMenuOptions, this, axisID, dialogType,
        mediator.getRunMethodForProcessInterface(getProcessingMethod()));
  }

  void tiltAction(final ProcessResultDisplay processResultDisplay,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final ProcessingMethod tiltProcessingMethod) {
    parent.tilt3dFindAction(processResultDisplay, deferred3dmodButton,
        run3dmodMenuOptions, tiltProcessingMethod);
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
            + "is calculated from \"" + CENTER_TO_CENTER_THICKNESS_LABEL + "\" plus \""
            + FindBeads3dPanel.BEAD_SIZE_LABEL + "\" multipled by \""
            + ADDITION_UNBINNED_DIAMETERS_TO_ADD + "\".");
    ltfZShift.setToolTipText("Incremental unbinned shift needed to center range of "
        + "fiducials in Z.  From the taAngles log.");
    setTiltButtonTooltip("If binning has changed, create a separate full aligned "
        + "stack.  Then compute a tomogram (tilt_3dfind.com).");
  }
}
