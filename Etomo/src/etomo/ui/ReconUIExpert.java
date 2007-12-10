package etomo.ui;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.comscript.ProcesschunksParam;
import etomo.process.ProcessState;
import etomo.type.AxisID;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessEndState;
import etomo.type.ProcessResult;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;

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

  protected final ApplicationManager manager;
  protected final MetaData metaData;
  protected final AxisID axisID;
  protected final DialogType dialogType;

  private final MainTomogramPanel mainPanel;
  private final ProcessTrack processTrack;

  private ProcessName nextProcess = null;

  abstract boolean doneDialog();

  protected abstract boolean saveDialog();

  protected abstract ProcessDialog getDialog();

  public ReconUIExpert(ApplicationManager manager, MainTomogramPanel mainPanel,
      ProcessTrack processTrack, AxisID axisID, DialogType dialogType) {
    this.manager = manager;
    metaData = manager.getMetaData();
    this.mainPanel = mainPanel;
    this.processTrack = processTrack;
    this.axisID = axisID;
    this.dialogType = dialogType;
  }

  protected boolean showDialog(ProcessDialog dialog) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, axisID)) {
      mainPanel.showBlankProcess(axisID);
      return false;
    }
    manager.setCurrentDialogType(dialogType, axisID);
    mainPanel.selectButton(axisID, dialogType.toString());
    if (dialog != null) {
      mainPanel.showProcess(dialog.getContainer(), axisID);
      return true;
    }
    return true;
  }

  protected void openDialog(ProcessDialog dialog) {
    mainPanel.showProcess(dialog.getContainer(), axisID);
    mainPanel.setParallelDialog(axisID, dialog.usingParallelProcessing());
  }

  protected void setNextProcess(ProcessName nextProcess) {
    this.nextProcess = nextProcess;
    manager.setProcessDialogType(axisID, dialogType);
  }

  protected void resetNextProcess() {
    nextProcess = null;
    manager.resetProcessDialogType(axisID);
  }

  protected ProcessName getNextProcess() {
    return nextProcess;
  }

  protected void sendMsgProcessStarting(
      ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessStarting();
  }

  protected void sendMsg(ProcessResult displayState,
      ProcessResultDisplay processResultDisplay) {
    if (displayState == null || processResultDisplay == null) {
      return;
    }
    processResultDisplay.msg(displayState);
  }

  protected void leaveDialog(DialogExitState exitState) {
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
  }

  protected void setDialogState(ProcessState processState) {
    if (processTrack != null) {
      processTrack.setState(processState, axisID, dialogType);
    }
    mainPanel.setState(processState, axisID, dialogType);
  }

  public void doneDialog(DialogExitState exitState) {
    ProcessDialog dialog = getDialog();
    if (dialog == null) {
      return;
    }
    dialog.setExitState(exitState);
    doneDialog();
  }

  public void saveAction() {
    ProcessDialog dialog = getDialog();
    if (dialog == null) {
      return;
    }
    dialog.saveAction();
  }

  public void saveDialog(DialogExitState exitState) {
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
  protected void processchunks(BaseManager manager,
      AbstractParallelDialog dialog, ProcessResultDisplay processResultDisplay,
      ProcessName processName) {
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(manager, axisID,
        processName);
    ParallelPanel parallelPanel = manager.getMainPanel().getParallelPanel(
        axisID);
    dialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      manager.getMainPanel().stopProgressBar(AxisID.ONLY,
          ProcessEndState.FAILED);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    //param should never be set to resume
    parallelPanel.getParallelProgressDisplay().resetResults();
    manager.processchunks(axisID, param, processResultDisplay);
  }

  protected ParallelPanel getParallelPanel() {
    return mainPanel.getParallelPanel(axisID);
  }

  public void setProgressBar(String label, int nSteps, AxisID axisID) {
    setProgressBar(label, nSteps, axisID, null);
  }

  public void setProgressBar(String label, int nSteps, AxisID axisID,
      ProcessName processName) {
    mainPanel.setProgressBar(label, nSteps, axisID, processName);
  }

  public void stopProgressBar(AxisID axisID) {
    mainPanel.stopProgressBar(axisID);
  }
}
/**
 * <p> $Log$
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
