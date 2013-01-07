package etomo.ui.swing;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.ProcessSeries;
import etomo.comscript.ProcesschunksParam;
import etomo.process.ProcessState;
import etomo.type.AxisID;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ProcessEndState;
import etomo.type.ProcessResult;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ProcessingMethod;

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
public abstract class ReconUIExpert implements UIExpert {
  public static final String rcsid = "$Id$";

  final ApplicationManager manager;
  final MetaData metaData;
  final AxisID axisID;
  final DialogType dialogType;

  private final MainTomogramPanel mainPanel;
  private final ProcessTrack processTrack;

  private boolean dialogOutOfDate = false;

  abstract void doneDialog();

  abstract void saveDialog();

  abstract ProcessDialog getDialog();

  public ReconUIExpert(ApplicationManager manager, MainTomogramPanel mainPanel,
      ProcessTrack processTrack, AxisID axisID, DialogType dialogType) {
    this.manager = manager;
    metaData = manager.getMetaData();
    this.mainPanel = mainPanel;
    this.processTrack = processTrack;
    this.axisID = axisID;
    this.dialogType = dialogType;
  }

  /**
   * @return false if the com scripts where never created in Tomogram Setup.
   */
  final boolean canShowDialog() {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(manager, metaData, axisID)) {
      mainPanel.showBlankProcess(axisID);
      return false;
    }
    return true;
  }

  /**
   * Turn on the button associated with dialog and set the current dialog type.
   * Display dialog if it already exists and is up to date.
   * @param dialog
   * @return true an existing, up to date dialog is shown.
   */
  final boolean showDialog(ProcessDialog dialog, final String actionMessage) {
    mainPanel.selectButton(axisID, dialogType.toString());
    if (dialogOutOfDate || dialog == null) {
      return false;
    }
    mainPanel.showProcess(dialog.getContainer(), axisID);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
    return true;
  }

  /**
   * Display dialog and set parallel dialog if necessary.  This function is used
   * when a dialog has just been created.
   * @param dialog
   */
  final void openDialog(ProcessDialog dialog, final String actionMessage) {
    dialogOutOfDate = false;
    mainPanel.showProcess(dialog.getContainer(), axisID);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  final void sendMsgProcessStarting(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessStarting();
  }

  final void sendMsg(ProcessResult displayState, ProcessResultDisplay processResultDisplay) {
    if (displayState == null || processResultDisplay == null) {
      return;
    }
    processResultDisplay.msg(displayState);
  }

  final void leaveDialog(DialogExitState exitState) {
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else if (exitState == DialogExitState.POSTPONE) {
      setDialogState(ProcessState.INPROGRESS);
      mainPanel.showBlankProcess(axisID);
    }
    else if (exitState == DialogExitState.EXECUTE) {
      setDialogState(ProcessState.COMPLETE);
      manager.openNextDialog(axisID, dialogType);
    }
    dialogOutOfDate = true;
  }

  final void setDialogState(ProcessState processState) {
    if (processTrack != null) {
      processTrack.setState(processState, axisID, dialogType);
    }
    mainPanel.setState(processState, axisID, dialogType);
  }

  public final void doneDialog(DialogExitState exitState) {
    ProcessDialog dialog = getDialog();
    if (dialog == null) {
      return;
    }
    dialog.setExitState(exitState);
    doneDialog();
  }

  public final void saveAction() {
    ProcessDialog dialog = getDialog();
    if (dialog == null) {
      return;
    }
    dialog.saveAction();
  }

  public final void saveDialog(DialogExitState exitState) {
    ProcessDialog dialog = getDialog();
    if (dialog == null) {
      return;
    }
    dialog.setExitState(exitState);
    saveDialog();
  }

  /**
   * Run processchunks.
   * @param axisID
   */
  final void processchunks(final BaseManager manager,
      final AbstractParallelDialog dialog,
      final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final String processName,
      final FileType outputImageFileType, final ProcessingMethod processingMethod,
      final boolean multiLineMessages) {
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(manager, axisID, processName,
        outputImageFileType);
    ParallelPanel parallelPanel = manager.getMainPanel().getParallelPanel(axisID);
    dialog.getParameters(param);
    if (!parallelPanel.getParameters(param, true)) {
      manager.getMainPanel().stopProgressBar(axisID, ProcessEndState.FAILED);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    // param should never be set to resume
    parallelPanel.getParallelProgressDisplay().resetResults();
    manager.processchunks(axisID, param, processResultDisplay, processSeries, true,
        processingMethod, multiLineMessages, dialogType);
  }

  final ParallelPanel getParallelPanel() {
    return mainPanel.getParallelPanel(axisID);
  }

  final public void setProgressBar(String label, int nSteps, AxisID axisID) {
    setProgressBar(label, nSteps, axisID, null);
  }

  final public void setProgressBar(String label, int nSteps, AxisID axisID,
      ProcessName processName) {
    mainPanel.setProgressBar(label, nSteps, axisID);
  }

  final public void startProgressBar(final String label, final AxisID axisID) {
    mainPanel.startProgressBar(label, axisID);
  }

  final public void stopProgressBar(AxisID axisID) {
    mainPanel.stopProgressBar(axisID);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2011/04/04 17:27:17  sueh
 * <p> bug# 1416 Passing the process name as a string to processchunks.
 * <p>
 * <p> Revision 1.3  2011/02/22 19:07:28  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.15  2010/07/02 03:19:49  sueh
 * <p> bug# 1388 Calling processchunks with popupChunkWarnings equals to true.
 * <p>
 * <p> Revision 1.14  2010/04/28 16:45:23  sueh
 * <p> bug# 1344 Passing outputImageFileType to the ProcesschunksParam
 * <p> constructor.
 * <p>
 * <p> Revision 1.13  2010/03/03 05:06:43  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.12  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.11  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.10  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.9  2008/05/28 02:51:12  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.  Removed nextProcess.
 * <p>
 * <p> Revision 1.8  2008/05/13 23:04:14  sueh
 * <p> bug# 847 Factored canShowDialog out of showDialog so that the boolean
 * <p> returned from each function would mean only one thing.
 * <p>
 * <p> Revision 1.7  2008/05/03 00:53:51  sueh
 * <p> bug# 847 Passing ProcessSeries to all process functions so they can be
 * <p> checked in the done process functions.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:45:38  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.5  2007/09/27 21:05:48  sueh
 * <p> bug# 1044 Moved implementation of ParallelProgressDisplay from ParallelPanel
 * <p> to ProcessorTable.
 * <p>
 * <p> Revision 1.4  2007/08/22 14:59:00  sueh
 * <p> bug# 1036 In showDialog, showing a blank process when opening a dialog fails.
 * <p>
 * <p> Revision 1.3  2006/10/24 23:34:19  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.2  2006/07/28 19:57:37  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing because isParallel is too similar to a standard get
 * <p> function.
 * <p>
 * <p> Revision 1.1  2006/07/26 16:41:20  sueh
 * <p> bug# 868 A base class for Expert used for tomogram reconstruction
 * <p> </p>
 */
