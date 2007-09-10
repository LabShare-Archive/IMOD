package etomo.type;

import java.util.Vector;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ProcessResultDisplayState {
  public static final String rcsid = "$Id$";

  private final ProcessResultDisplay display;
  private Vector dependentDisplayList = null;
  private Vector failureDisplayList = null;
  private Vector successDisplayList = null;
  private int dependencyIndex = -1;

  //will go back to the original state if the process failed to run
  private boolean originalState = false;
  //tells which process is current being run
  private boolean secondaryProcess = false;
  //will ignore most messages when the process is not running
  private boolean processRunning = false;

  public ProcessResultDisplayState(ProcessResultDisplay display) {
    this.display = display;
  }

  public final void setOriginalState(boolean originalState) {
    this.originalState = originalState;
  }

  /**
   * Call this function when the first process starts.
   *
   */
  public final synchronized void msgProcessStarting() {
    if (!processRunning) {
      originalState = display.getOriginalState();
      //on average the process will complete, so set process done to true at the
      //beginning in case the user exits etomo.
      display.setProcessDone(true);
      secondaryProcess = false;
    }
    processRunning = true;
  }

  public final void msg(ProcessResult processResult) {
    if (processResult == ProcessResult.SUCCEEDED) {
      msgProcessSucceeded();
    }
    else if (processResult == ProcessResult.FAILED) {
      msgProcessFailed();
    }
    else if (processResult == ProcessResult.FAILED_TO_START) {
      msgProcessFailedToStart();
    }
  }

  /**
   * Call this function if the final process succeeds.
   *
   */
  public final void msgProcessSucceeded() {
    if (!processRunning) {
      return;
    }
    display.setProcessDone(true);
    setProcessDone(false, dependentDisplayList);
    setProcessDone(true, successDisplayList);
    processRunning = false;
  }

  /**
   * Call this function if the final process fails.
   *
   */
  public final void msgProcessFailed() {
    if (!processRunning) {
      return;
    }
    display.setProcessDone(false);
    setProcessDone(false, dependentDisplayList);
    setProcessDone(false, failureDisplayList);
    processRunning = false;
  }

  /**
   * sets process done in the ProcessResultDisplay's in displays
   * @param done
   * @param displays
   */
  private final void setProcessDone(boolean done, Vector displayList) {
    if (displayList == null) {
      return;
    }
    for (int i = 0; i < displayList.size(); i++) {
      ProcessResultDisplay display = (ProcessResultDisplay) displayList.get(i);
      display.setProcessDone(done);
      display.setOriginalState(done);
    }
  }

  /**
   * Call this function if any process fails to start.
   * For a secondary process (any process after the first process) this is
   * equivalent to calling msgProcessFailed.
   */
  public final void msgProcessFailedToStart() {
    if (!processRunning) {
      return;
    }
    if (secondaryProcess) {
      msgProcessFailed();
    }
    else {
      display.setProcessDone(originalState);
      processRunning = false;
    }
  }

  public final void addDependentDisplay(ProcessResultDisplay dependentDisplay) {
    if (dependentDisplay == null) {
      return;
    }
    if (dependentDisplayList == null) {
      dependentDisplayList = new Vector();
    }
    dependentDisplayList.add(dependentDisplay);
  }

  public final void addFailureDisplay(ProcessResultDisplay failureDisplay) {
    if (failureDisplay == null) {
      return;
    }
    if (failureDisplayList == null) {
      failureDisplayList = new Vector();
    }
    failureDisplayList.add(failureDisplay);
  }

  public final void addSuccessDisplay(ProcessResultDisplay successDisplay) {
    if (successDisplay == null) {
      return;
    }
    if (successDisplayList == null) {
      successDisplayList = new Vector();
    }
    successDisplayList.add(successDisplay);
  }

  /**
   * Call this function when a secondary process (any process after the first
   * process) starts.
   *
   */
  public final void msgSecondaryProcess() {
    secondaryProcess = true;
  }

  protected final boolean isOriginalState() {
    return originalState;
  }

  protected final boolean isProcessRunning() {
    return processRunning;
  }

  protected final boolean isSecondaryProcess() {
    return secondaryProcess;
  }

  public void setDependencyIndex(int dependencyIndex) {
    this.dependencyIndex = dependencyIndex;
  }

  public int getDependencyIndex() {
    return dependencyIndex;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2006/07/26 16:38:42  sueh
 * <p> bug# 868 Added msg(ProcessResult)
 * <p>
 * <p> Revision 1.3  2006/02/06 21:18:45  sueh
 * <p> bug# 521 Changed following display to dependent display.  Added
 * <p> dependecy index and initialized.
 * <p>
 * <p> Revision 1.2  2006/01/31 20:48:32  sueh
 * <p> bug# 521 Added failureDisplayList, successDisplayList, and
 * <p> followingDisplayList to change the state of other displays when the
 * <p> process associated with the current instance succeeds or fails.
 * <p>
 * <p> Revision 1.1  2006/01/26 21:59:31  sueh
 * <p> bug# 401 Turn ProcessResultDisplay into an interface.  Place the
 * <p> functionality into ProcessResultDisplayState.  This allows a greater
 * <p> variety of classes to be ProcessResultDisplay's.
 * <p> </p>
 */
