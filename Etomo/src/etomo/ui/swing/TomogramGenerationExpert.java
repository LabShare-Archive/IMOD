package etomo.ui.swing;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SirtsetupParam;
import etomo.comscript.TiltParam;
import etomo.process.ImodManager;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ReconScreenState;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class TomogramGenerationExpert extends ReconUIExpert {
  public static final String rcsid = "$Id$";

  public static final String SIRT_DONE = "sirtDone";

  private final ComScriptManager comScriptMgr;
  private final TomogramState state;
  private final ReconScreenState screenState;

  private TomogramGenerationDialog dialog = null;
  private boolean advanced = false;
  private boolean getBinningFromNewst = true;

  public TomogramGenerationExpert(ApplicationManager manager,
      MainTomogramPanel mainPanel, ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID, DialogType.TOMOGRAM_GENERATION);
    comScriptMgr = manager.getComScriptManager();
    state = manager.getState();
    screenState = manager.getScreenState(axisID);
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openDialog() {
    if (!canShowDialog()) {
      return;
    }
    String actionMessage = manager.setCurrentDialogType(dialogType, axisID);
    if (showDialog(dialog, actionMessage)) {
      return;
    }
    // Create the dialog and show it.
    Utilities.timestamp("new", "TomogramGenerationDialog", Utilities.STARTED_STATUS);
    dialog = TomogramGenerationDialog.getInstance(manager, this, axisID);
    Utilities.timestamp("new", "TomogramGenerationDialog", Utilities.FINISHED_STATUS);
    // no longer managing image size
    setParameters(metaData);
    setParameters(screenState);
    // load SIRT first because the resume radio buttons disable tilt fields
    SirtsetupParam sirtsetupParam;
    if (comScriptMgr.loadSirtsetup(axisID)) {
      sirtsetupParam = comScriptMgr.getSirtsetupParam(axisID);
    }
    else {
      // create an empty param to get the defaults.
      sirtsetupParam = new SirtsetupParam(manager, axisID);
    }
    setParameters(sirtsetupParam);
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    tiltParam.setFiducialess(metaData.isFiducialess(axisID));
    // If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      // upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldTiltCom(manager, axisID, tiltParam);
    }
    boolean genExists = metaData.isGenExists(axisID);
    setParameters(tiltParam, !genExists);
    checkpoint();
    // Set the fidcialess state and tilt axis angle
    setTiltState();
    metaData.setGenExists(axisID, true);
    openDialog(dialog, actionMessage);
  }

  public void msgSirtsetupSucceeded() {
    checkpoint();
  }

  private void checkpoint() {
    if (dialog == null) {
      return;
    }
    comScriptMgr.loadTiltForSirt(axisID);
    dialog.checkpoint(comScriptMgr.getTiltParamFromTiltForSirt(axisID), state);
  }

  public void msgSirtSucceeded() {
    if (dialog != null) {
      dialog.msgSirtSucceeded();
    }
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public boolean startNextProcess(ProcessSeries.Process process,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display) {
    if (process.equals(ProcessName.PROCESSCHUNKS.toString())) {
      if (process.getSubprocessName() == ProcessName.TILT) {
        processchunks(manager, dialog, processResultDisplay, processSeries,
            ProcessName.TILT.toString() + axisID.getExtension(),
            process.getOutputImageFileType(), process.getProcessingMethod(), false);
      }
      else {
        processchunks(manager, dialog, processResultDisplay, processSeries,
            ProcessName.TILT.toString() + axisID.getExtension() + "_sirt",
            process.getOutputImageFileType(), process.getProcessingMethod(), false);
      }
      return true;
    }
    if (process.equals(SIRT_DONE)) {
      msgSirtSucceeded();
      return true;
    }
    return false;
  }

  public boolean reconnectTilt(ProcessName processName) {
    ProcessResultDisplay display = manager.getProcessResultDisplayFactory(axisID)
        .getTilt(DialogType.TOMOGRAM_GENERATION);
    sendMsgProcessStarting(display);
    return manager.reconnectTilt(axisID, processName, display);
  }

  public void setTiltState() {
    if (dialog == null) {
      return;
    }
    dialog.setTiltState(state, metaData);
  }

  void doneDialog() {
    if (dialog == null) {
      return;
    }
    DialogExitState exitState = dialog.getExitState();
    if (exitState == DialogExitState.EXECUTE) {
      manager.closeImods(ImodManager.TRIAL_TOMOGRAM_KEY, axisID, "Trial tomogram");
    }
    if (exitState != DialogExitState.CANCEL) {
      saveDialog();
    }
    // Clean up the existing dialog
    leaveDialog(exitState);
    // Hold onto the finished dialog in case anything is running that needs it or
    // there are next processes that need it.
  }

  void saveDialog() {
    if (dialog == null) {
      return;
    }
    advanced = dialog.isAdvanced();
    // Get the user input data from the dialog box
    getParameters(screenState);
    manager.updateTiltCom(dialog.getTiltDisplay(), axisID, false);
    try {
      getParameters(metaData);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Data File Error");
    }
    manager.updateSirtSetupCom(axisID, dialog.getSirtsetupDisplay(), false, false);
    manager.saveStorables(axisID);
  }

  protected ProcessDialog getDialog() {
    return dialog;
  }

  private void setParameters(ConstMetaData metaData) {
    if (dialog == null) {
      return;
    }
    dialog.setParameters(metaData);
  }

  private final void setParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.setParameters(screenState);
  }

  private void getParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.getParameters(screenState);
  }

  private void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    dialog.getParameters(metaData);
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   */
  private void setParameters(ConstTiltParam tiltParam, boolean initialize) {
    if (dialog == null) {
      return;
    }
    dialog.setParameters(tiltParam, initialize);
  }

  private void setParameters(SirtsetupParam param) {
    if (dialog == null) {
      return;
    }
    dialog.setParameters(param);
  }

  public TiltDisplay getTiltDisplay() {
    if (dialog == null) {
      return null;
    }
    return dialog.getTiltDisplay();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.6  2011/04/09 06:38:48  sueh
 * <p> bug# 1416 Eliminating dead code in openDialog.
 * <p>
 * <p> Revision 1.5  2011/04/04 17:41:59  sueh
 * <p> bug# 1416 Added SIRT_DONE, getTiltDisplay, msgSirtSucceeded, setParameters(SirtsetupParam).  Modified
 * <p> constructor openDialog, reconnectTilt, saveDialog, startNextProcess.
 * <p>
 * <p> Revision 1.4  2011/02/10 16:30:49  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:23:38  sueh
 * <p> bug# 1421 Changed setEnabledTiltParameters to setTiltState.  Removed
 * <p> unused function getParameters(TiltParam).
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.35  2010/04/28 16:47:02  sueh
 * <p> bug# 1344 In startNextProcessing passing
 * <p> process.getOutputImageFileType to processchunks.
 * <p>
 * <p> Revision 1.34  2010/03/05 04:06:04  sueh
 * <p> bug# 1319 Added boolean initialize to setParameters(ConstTiltParam).
 * <p>
 * <p> Revision 1.33  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.32  2009/09/22 23:56:16  sueh
 * <p> bug# 1269 Moved setEnabledTiltParameters to abstract tilt panel so it can be use by tilt 3dfind.
 * <p>
 * <p> Revision 1.31  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.30  2009/04/15 16:52:58  sueh
 * <p> bug# 1190 Returning false and logging failure for major reconnection
 * <p> failures.
 * <p>
 * <p> Revision 1.29  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.28  2009/02/13 02:37:54  sueh
 * <p> bug# 1148 In doneDialog no longer setting trialTomogramList to null
 * <p> because this causes a null pointer exception on close.
 * <p>
 * <p> Revision 1.27  2009/02/05 23:45:52  sueh
 * <p> bug# 1148 Setting and getting trialTomogramList from metadata.
 * <p>
 * <p> Revision 1.26  2008/11/11 23:54:47  sueh
 * <p> bug# 1149 Removed getBinning(), which was used to save binning to
 * <p> MetaData.tomoGenBinning.  TomoGenBinning is now finalStackBinning
 * <p> and comes from the FinalStack dialog.  Is was always supposed to be saved from the binning in the Align box.
 * <p>
 * <p> Revision 1.25  2008/10/27 20:45:10  sueh
 * <p> bug# 1141 Added getParameters(SplittiltParam).
 * <p>
 * <p> Revision 1.24  2008/10/16 22:32:35  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 1.23  2008/07/19 01:12:45  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.22  2008/07/15 17:48:06  sueh
 * <p> bug# 1124 In getTiltParams(TiltParam) corrected the setting of
 * <p> TiltParam.fiducialess.  Basing it on the last time final alignment was run.
 * <p>
 * <p> Revision 1.21  2008/05/28 02:51:46  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.20  2008/05/13 23:07:58  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.19  2008/05/03 00:57:31  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.18  2008/02/01 01:37:31  sueh
 * <p> bug# 1075 Handling header failure in setSubsetStart.
 * <p>
 * <p> Revision 1.17  2008/01/25 22:29:34  sueh
 * <p> bug# 1070 Don't use parallel processing unless the cpu.adoc or
 * <p> IMOD_PROCESSORS has been set by the user.
 * <p>
 * <p> Revision 1.16  2007/12/13 01:14:00  sueh
 * <p> bug# 1056 Removed the Storables inner class from TiltParam.
 * <p>
 * <p> Revision 1.15  2007/12/10 22:49:29  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.  Removed getParameters
 * <p> ProcesschunksParam) because it is empty.
 * <p>
 * <p> Revision 1.14  2007/08/16 16:36:37  sueh
 * <p> bug# 1035 Added ltfSizeToOutputInXandY updating in newst.  Converting to
 * <p> startingAndEndingX and Y in blend.  Calculating SUBSETSTART in tilt.
 * <p>
 * <p> Revision 1.13  2007/07/17 21:45:21  sueh
 * <p> bug# 1018 Getting cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 1.12  2007/05/18 23:54:20  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.11  2007/03/21 19:47:27  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.10  2007/02/05 23:45:46  sueh
 * <p> bug# 962 Moved comscript mode info to inner class.
 * <p>
 * <p> Revision 1.9  2006/12/02 04:59:21  sueh
 * <p> bug# 944 Added get/setProcessName ProcesschunksParam so the process
 * <p> being run can be identified.
 * <p>
 * <p> Revision 1.8  2006/11/15 21:35:49  sueh
 * <p> bug# 872 Changed saveIntermediateParamFile to saveStorables.
 * <p>
 * <p> Revision 1.7  2006/09/19 22:38:06  sueh
 * <p> bug# 920 Refreshing meta data values in TiltParam each time tilt.com is loaded.
 * <p>
 * <p> Revision 1.6  2006/09/14 00:00:47  sueh
 * <p> bug# 920 Rename X offset and Z offset to X shift and Z shift.
 * <p>
 * <p> Revision 1.5  2006/08/03 21:36:24  sueh
 * <p> bug# Added reconnectTilt().
 * <p>
 * <p> Revision 1.4  2006/07/31 21:46:10  sueh
 * <p> Removed unnecessary print
 * <p>
 * <p> Revision 1.3  2006/07/28 21:27:42  sueh
 * <p> bug# 868 Moved complex button actions from dialog to expert
 * <p>
 * <p> Revision 1.2  2006/07/28 20:14:12  sueh
 * <p> bug# 868 Adding sets and gets to dialog, moving functionality to expert
 * <p>
 * <p> Revision 1.1  2006/07/26 16:41:52  sueh
 * <p> bug# 868 Moved functions associated with TomogramGenerationDialog from
 * <p> ApplicationManager to TomogramGenerationExpert.
 * <p> </p>
 */
