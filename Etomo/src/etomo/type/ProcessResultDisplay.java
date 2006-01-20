package etomo.type;
/**
* <p>Description: A parent class for a class which can display the result of a
* process.  The display is assumed to binary:  done or not done.  This class can
* handle a single process or a sequence of processes.</p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public abstract class ProcessResultDisplay {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * tells child that process is either done or not done
   */
  protected abstract void setProcessDone(boolean done);
  /**
   * gets the state to got back to if the process fails to start
   * @return
   */
  protected abstract boolean getOriginalState();
  
  //will go back to the original state if the process failed to run
  private boolean originalState = false;
  //tells which process is current being run
  private boolean secondaryProcess = false;
  //will ignore most messages when the process is not running
  private boolean processRunning = false;
  
  protected final void setState(boolean state) {
    originalState = state;
  }
  
  /**
   * Call this function when the first process starts.
   *
   */
  public final synchronized void msgProcessStarting() {
    if (!processRunning) {
      originalState = getOriginalState();
      //on average the process will complete, so set process done to true at the
      //beginning in case the user exits etomo.
      setProcessDone(true);
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
    setProcessDone(true);
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
    setProcessDone(false);
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
      setProcessDone(originalState);
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