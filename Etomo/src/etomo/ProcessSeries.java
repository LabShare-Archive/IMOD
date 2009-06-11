package etomo;

import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.Deferred3dmodButton;
import etomo.ui.ProcessDisplay;

/**
 * <p>Description: Represents a series of processes to be executed.</p>
 * 
 * <p>This was originally done in BaseManager with a nextProcess string and a
 * lastProcess string.  The problem with this method is that a button press that
 * triggers axis-is-busy functionality could cause the process series to change
 * before the axis is checked.  This became clear when the run3dmodProcess
 * was added.  So we need to create a new process series when each series of
 * processes is run.</p>
 * 
 * <p>A ProcessSeries instance is created in the function that first decides to
 * set up a process series.  Next, last and/or 3dmod process(es) can be added at
 * any time and it is not necessary for any process function to know whether it
 * is the first process in the series.  This is handled by only constructing a
 * processSeries when it is null and will be added to.  3dmod processes, which
 * are added blindly (without checking if they are null), will only be set to
 * ProcessSeries.run3dmodButton if they are not null.</P>
 * 
 * <P>The ProcessSeries instance is passed as a constant to the run process
 * function in the process manager.  It goes into the process object and
 * eventually gets to a processDone function, where
 * ProcessSeries.startNextProcess() is called.  If nextProcess or lastProcess is
 * set, ProcessSeries.startNextProcess calls the manager startNextProcess and
 * passes it a reference to the instance of ProcessSeries.  The manager
 * startNextProcess starts the next process, passing the processSeries instance.
 * ProcessSeries should be passed to process functions whether or not a process
 * series is being started or added to.  This means that you do not have to keep
 * track of when it is in use.</P>
 * 
 * <P>If nextProcess and lastProcess are both empty (they are cleared when their
 * process is run), then run3dmodButton is checked.  If it is set, then it is
 * used to run a 3dmod process.  ProcessSeries.run3dmodButton is a
 * Run3dmodButton whose main function is to run a 3dmod immediately (not
 * deferred).  It was added to a deferred Run3dmodButton whose main function is
 * to run the first process in the process series.  ProcessSeries.run3dmodButton
 * still has a reference to the dialog it was last placed on (its container), so
 * its action function, which is called with ProcessSeries.run3dmodMenuOptions,
 * will call the dialog's action function.</P>
 * 
 * <P>The risk in running 3dmod this way is that the function called by the
 * dialog may try to access the dialog through the normal channel (through the
 * manager or UIExpert), and user may have left that dialog.  This is why the
 * UIExperts don't set their dialog pointers to null on done().  It is unlikely
 * that there will be much communication with a dialog for a 3dmod process.  If
 * this does happen for a dialog managed by a manager instead of a UIExpert, it
 * may be a good idea create a UIExpert for this dialog.  This way the UIExpert
 * 3dmod function can talk to an out-of-date dialog that the user has left.
 * Note that, if the users goes back into the dialog, the 3dmod function will
 * talk to the up to date dialog, but the action function called by the
 * Run3dmodButton will call the action function in the out-of-date instance of
 * the dialog.  This is true unless the the Run3dmodButton is managed by the
 * ProcessResultsDisplayFactory.  In that case new instance of the dialog
 * contains the same instance of the Run3dmodButton as the ProcessSeries, and
 * container reference would be updated to that new dialog instance.</p>
 * 
 * <H3>Problems</H3>
 * 
 * <P>The dialogs being managed directly by the manager won't stick around after
 * the user leaves them (except for Tomogram Combination).  The Run3dmodButton
 * has access to them but the 3dmod function doesn't.  If this becomes a
 * problem, it can be handled by creating a UIExpert to manage the dialog.</P>
 * 
 * <P>The rule that the state of the dialog when a button is pushed is state
 * referred to by all subsequent processes, which may already have been broken
 * in Tomogram Combination, it being broken here in two ways.  One is when the
 * user stays on the dialog and changes something that action function or the
 * 3dmod function references.  The other is when the Run3dmodButton that runs
 * the 3dmod process is being managed by ProcessResultDisplayFactory, so that it
 * loses contact with the original dialog when the user leaves and goes back
 * into the dialog.</P>
 * 
 * <P>When the Run3dmodButton that runs the 3dmod process is not managed by the
 * ProcessResultsDisplayFactory then, if the user leaves and re-enters the
 * dialog, then dialog action function is called from the old instance of the
 * dialog, and the 3dmod function would talk to the new instance of the dialog.</P>
 * 
 * <H3>Possible upgrades</H3>
 * 
 * <P>If more then two regular processes have to be set by a process function in
 * a manager or UIExpert, or there is a ordering requirement that can't be
 * handled by the current next, last, 3dmod setup, it should be easy to add more
 * capacity to this class.  Add another process string or convert an existing
 * process to a queue of processes.</P>
 * 
 * <H3>What Not To Do</H3>
 * 
 * <P>Because the method used to run 3dmod is already problematic, don't use it
 * to allow the user to create their own series of processes (see the original
 * idea of bug# 847).  The problems associated with a process function talking
 * to a dialog are minor for 3dmod, but would be huge for other types of
 * processes.</P>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.6  2008/06/24 20:08:19  sueh
 * <p> bug# 1111 Added setDebug.
 * <p>
 * <p> Revision 1.5  2008/05/28 02:48:12  sueh
 * <p> bug# 1111 Removed processDialogTypeA and B from BaseManager.
 * <p> The dialogType for processes should be handled by ProcessSeries.
 * <p> Passing a DialogType parameter to startNextProcess.
 * <p>
 * <p> Revision 1.4  2008/05/13 22:57:11  sueh
 * <p> bug# 847 Added documentation.  Fixed a bug where
 * <p> isProcessQueueEmpty was returning an incorrect result.  Made sure that
 * <p> run3dmodButton could not be turned off except by its being used.
 * <p>
 * <p> Revision 1.3  2008/05/07 02:37:59  sueh
 * <p> bug# 847 Making some ProcessSeries functions public so they can be used by expert classes.
 * <p>
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
  private final DialogType dialogType;
  private final ProcessDisplay processDisplay;

  private String nextProcess = null;
  private String lastProcess = null;
  //3dmod is opened after the last process.
  private Deferred3dmodButton run3dmodButton = null;
  private Run3dmodMenuOptions run3dmodMenuOptions = null;
  private boolean debug = false;

  public ProcessSeries(final BaseManager manager, DialogType dialogType) {
    this.manager = manager;
    this.dialogType = dialogType;
    this.processDisplay = null;
  }

  public ProcessSeries(final BaseManager manager, DialogType dialogType,
      ProcessDisplay processDisplay) {
    this.manager = manager;
    this.dialogType = dialogType;
    this.processDisplay = processDisplay;
  }

  /**
   * Start next process from the start process queue.  If it is 
   * empty then start next process from the end process queue.  If next process
   * and last process are empty, run 3dmod based on deferred3dmodButton and
   * run3dmodMenuOptions.  The started process is removed.
   * @param axisID
   * @param processResultDisplay
   * @return true if a process is run.
   */
  public boolean startNextProcess(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
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
    else if (run3dmodButton != null) {
      start3dmodProcess();
      return true;
    }
    else {
      return false;
    }
    sendMsgSecondaryProcess(processResultDisplay);
    if (debug) {
      System.out.println("ProcessSeries.startNextProcess:process=" + process);
    }
    manager.startNextProcess(axisID, process, processResultDisplay, this,
        dialogType, processDisplay);
    return true;
  }

  public void setDebug(boolean input) {
    debug = input;
  }

  /**
   * Keep final.  Adds process to the end of nextProcessQueue.
   * @param axisID
   * @param process
   */
  public void setNextProcess(final String process) {
    nextProcess = process;
  }

  void clearProcesses() {
    nextProcess = null;
    lastProcess = null;
    run3dmodButton = null;
    run3dmodMenuOptions = null;
  }

  public String peekNextProcess() {
    if (nextProcess != null) {
      return nextProcess;
    }
    if (lastProcess != null) {
      return lastProcess;
    }
    if (run3dmodButton != null) {
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

  void setLastProcess(final String process, ProcessDisplay display) {
    lastProcess = process;
  }

  /**
   * Sets the option to open a 3dmod after all the processes are
   * done.  This function cannot be used to reset a 3dmod process.
   * @param axisID
   * @param run3dmodProcess
   * @param run3dmodMenuOptions
   */
  public void setRun3dmodDeferred(
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    //Don't want to keep track of when deferred3dmodButton is null or not in the
    //manager or UIExpert class.  So once a deferred3dmodButton is set, it
    //should stay set until it is used.  This does not have to be done for
    //nextProcess and lastProcess because they currently are always set
    //explicitly.
    if (deferred3dmodButton == null) {
      return;
    }
    run3dmodButton = deferred3dmodButton;
    this.run3dmodMenuOptions = run3dmodMenuOptions;
  }

  private void sendMsgSecondaryProcess(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgSecondaryProcess();
  }

  /**
   * Calls the action(Run3dmodMenuOptions) function in run3dmodButton
   * (Run3dmodButton).  Blanks out deferred3dmodButton so it can't be run
   * more then once.
   */
  private void start3dmodProcess() {
    Deferred3dmodButton tempRun3dmodButton = run3dmodButton;
    Run3dmodMenuOptions tempRun3dmodMenuOptions = run3dmodMenuOptions;
    run3dmodButton = null;
    run3dmodMenuOptions = null;
    tempRun3dmodButton.action(tempRun3dmodMenuOptions);
  }
}
