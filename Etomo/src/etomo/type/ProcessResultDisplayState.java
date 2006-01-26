package etomo.type;

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

  public ProcessResultDisplayState(ProcessResultDisplay display) {
    this.display = display;
  }

  //will go back to the original state if the process failed to run
  private boolean originalState = false;
  //tells which process is current being run
  private boolean secondaryProcess = false;
  //will ignore most messages when the process is not running
  private boolean processRunning = false;

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

  /**
   * Call this function if the final process succeeds.
   *
   */
  public final void msgProcessSucceeded() {
    if (!processRunning) {
      return;
    }
    display.setProcessDone(true);
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
    processRunning = false;
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
}
/**
 * <p> $Log$ </p>
 */