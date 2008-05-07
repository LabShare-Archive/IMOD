package etomo;

import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.Run3dmodDeferredCommand;

/**
 * <p>Description: Represents a series of processes to be executed.</p>
 * 
 * <p>This was originally done in BaseManager with a nextProcess string and a
 * lastProcess string.  The problem with this method is that a button press that
 * triggers axis-is-busy functionality could cause the process series to change
 * before the axis is checked.  This became clear when the run3dmodProcess
 * was added.  So we need to create a new process series each a series of
 * processes is run.</p>
 * 
 * <p>A ProcessSeries instance is created in the function that runs the first
 * process of a serious and the next, last, and/or 3dmod process(es) are added.
 * The ProcessSeries instance is passed as a constant to the run process
 * function in the process manager.  It goes to the process object and
 * eventually gets to a processDone function, where ProcessSeries.
 * startNextProcess() is called.  ProcessSeries.startNextProcess calls the
 * manager startNextProcess and passes the instance of ProcessSeries</p>
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
 * 
 * <p> $Log$
 * <p> Revision 1.2  2008/05/06 23:55:34  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 1.1  2008/05/03 00:34:21  sueh
 * <p> bug# 847 Passing ProcessSeries to the process manager,
 * <p> startNextProcess, and to all process functions.  To avoid having to decide
 * <p> which processes are next processes, pass it everywhere, even to
 * <p> processes that don't use ProcessResultDisplay.  The UI should not create
 * <p> any ProcessSeries and should pass them as null (since they don't know
 * <p> about processes).  Before adding to a process series, create it if it doesn't
 * <p> exist.  Before checking a process series, make sure it exists.  Since a
 * <p> series is only created when it doesn't exist and is needed, I don't have to
 * <p> keep track of which process comes first in a series.
 * <p> </p>
 */
public final class ProcessSeries implements ConstProcessSeries {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  private String nextProcess = null;
  private String lastProcess = null;
  //3dmod is opened after the last process.
  private Run3dmodDeferredCommand run3dmodDeferredCommand = null;
  private Run3dmodMenuOptions run3dmodMenuOptions = null;
  private boolean debug = false;

  public ProcessSeries(final BaseManager manager) {
    this.manager = manager;
  }

  /**
   * Start next process from the start process queue.  If it is 
   * empty then start next process from the end process queue.  If there are no
   * processes in either queue, run 3dmod based on run3dmodProcess and
   * run3dmodMenuOptions.  The started process is removed from its queue.
   * @param axisID
   * @param processResultDisplay
   * @return true if a process is run.
   */
  public boolean startNextProcess(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
    if (isProcessQueueEmpty()) {
      return false;
    }
    //Get the next process.
    String process = null;
    if (nextProcess != null) {
      process = nextProcess;
      nextProcess = null;
    }
    else if (lastProcess != null) {
      process = lastProcess;
      lastProcess = null;
    }
    else if (run3dmodMenuOptions != null) {
      start3dmodProcess();
      return true;
    }
    sendMsgSecondaryProcess(processResultDisplay);
    manager.startNextProcess(axisID, process, processResultDisplay, this);
    return true;
  }

  /**
   * Keep final.  Adds process to the end of nextProcessQueue.
   * @param axisID
   * @param process
   */
  void setNextProcess(final String process) {
    nextProcess = process;
  }

  boolean isProcessQueueEmpty() {
    return nextProcess == null && lastProcess == null
        && run3dmodMenuOptions == null;
  }

  void clearProcesses() {
    nextProcess = null;
    lastProcess = null;
    run3dmodDeferredCommand = null;
    run3dmodMenuOptions = null;
  }

  public String peekNextProcess() {
    if (nextProcess != null) {
      return nextProcess;
    }
    if (lastProcess != null) {
      return lastProcess;
    }
    if (run3dmodDeferredCommand != null) {
      return "3dmod";
    }
    return null;
  }

  /**
   * Set lastProcess.
   * @param axisID
   * @param process
   */
  void setLastProcess(final String process) {
    lastProcess = process;
  }

  /**
   * Sets the option to open a 3dmod after all the processes are
   * done.
   * @param axisID
   * @param run3dmodProcess
   * @param run3dmodMenuOptions
   */
public  void setRun3dmodDeferred(
      final Run3dmodDeferredCommand run3dmodDeferredCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    this.run3dmodDeferredCommand = run3dmodDeferredCommand;
    this.run3dmodMenuOptions = run3dmodMenuOptions;
  }

  private void sendMsgSecondaryProcess(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgSecondaryProcess();
  }

  /**
   * Calls the action(Run3dmodMenuOptions) function in Run3dmodDeferredCommand
   * (Run3dmodButton).  Blanks out run3dmodDeferredCommand so it can't be run
   * more then once.
   */
  private void start3dmodProcess() {
    Run3dmodDeferredCommand tempRun3dmodDeferredCommand = run3dmodDeferredCommand;
    Run3dmodMenuOptions tempRun3dmodMenuOptions = run3dmodMenuOptions;
    run3dmodDeferredCommand = null;
    run3dmodMenuOptions = null;
    tempRun3dmodDeferredCommand.action(tempRun3dmodMenuOptions);
  }
}
