package etomo.process;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.Command;
import etomo.comscript.CommandDetails;
import etomo.comscript.DetachedCommand;
import etomo.comscript.IntermittentCommand;
import etomo.comscript.ProcessDetails;
import etomo.comscript.ComscriptState;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TomosnapshotParam;
import etomo.comscript.XfmodelParam;
import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.ParallelProgressDisplay;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

/**
 * <p>Description: Process manager for processes not associated with one
 * interface such as processchunks.  It also contains axis busy functions, start
 * process functions, and end process functions.  It also handles killing most
 * kinds of processes.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.82  2009/04/14 23:02:20  sueh
 * <p> bug# 1207 Made getSavedProcessData public.  Removed getRunningProcessData.
 * <p>
 * <p> Revision 1.81  2009/04/13 22:29:00  sueh
 * <p> bug# 1207 In isAxisBusy, if the process is running on a different host, put the host
 * <p> name in the error message.
 * <p>
 * <p> Revision 1.80  2009/04/06 22:38:49  sueh
 * <p> bug# 1206 Moved the popups to Tomosnapshot.run.
 * <p>
 * <p> Revision 1.79  2009/04/02 19:16:09  sueh
 * <p> bug# 1206 Calling tomosnapshot in the main thread using SystemProgram.  Popping
 * <p> up message if it succeeds or fails.
 * <p>
 * <p> Revision 1.78  2009/03/17 00:34:31  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.77  2009/03/02 18:58:33  sueh
 * <p> bug# 1193 Added blockAxisA and B.  Testing them in isAxisBusy.  Turning
 * <p> them off in unblockAxis.
 * <p>
 * <p> Revision 1.76  2009/03/01 01:17:19  sueh
 * <p> bug# 1193 In reconnectProcesschunks setting the process in the monitor.
 * <p>
 * <p> Revision 1.75  2009/03/01 00:53:15  sueh
 * <p> bug# 1193 In startDetachedProcess setting the process in the monitor.
 * <p>
 * <p> Revision 1.74  2009/02/04 23:23:18  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.73  2009/01/26 22:41:43  sueh
 * <p> bug# 1173 Added boolean nonBlocking to msgComScriptDone functions,
 * <p> so that processDone knows not to pop up an error message that thread
 * <p> name is not set.
 * <p>
 * <p> Revision 1.72  2008/12/11 19:25:51  sueh
 * <p> bug# 1167 In msgProcessDone(BackgroundProcess, int, boolean) looking
 * <p> for warning messages after postProcess call.
 * <p>
 * <p> Revision 1.71  2008/11/20 01:31:49  sueh
 * <p> bug# 1149 Moved xfmodel from JoinProcessManager to BaseProcessManager.
 * <p>
 * <p> Revision 1.70  2008/10/27 17:51:01  sueh
 * <p> bug# 1141 Added startNonBlockingComScript functions to run ctfplotter
 * <p> without checking or blocking the axis.
 * <p>
 * <p> Revision 1.69  2008/05/16 22:26:05  sueh
 * <p> bug# 1109 Added class description.
 * <p>
 * <p> Revision 1.68  2008/05/03 00:37:31  sueh
 * <p> bug# 847 Passing a ProcessSeries instance to all processes that use
 * <p> process objects.  The goal is to pass then back to process done functions.
 * <p>
 * <p> Revision 1.67  2008/01/31 20:17:44  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.66  2008/01/14 20:28:34  sueh
 * <p> bug# 1050 Moved getRunningProcessData from ProcessManager to
 * <p> BaseProcessManager.  Added reconnectProcesschunks.
 * <p>
 * <p> Revision 1.65  2007/12/26 22:12:24  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.64  2007/12/13 01:08:21  sueh
 * <p> bug# 1056 added startComScript(String command,...,CommandDetails)
 * <p>
 * <p> Revision 1.63  2007/12/10 22:00:44  sueh
 * <p> bug# 1041 Combined the two processchunks functions.
 * <p>
 * <p> Revision 1.62  2007/11/06 19:21:01  sueh
 * <p> bug# 1-47 Allowed processchunks to be executed in a subdirectory.
 * <p>
 * <p> Revision 1.61  2007/09/27 19:27:28  sueh
 * <p> bug# 1044 Generalized load functionality.
 * <p>
 * <p> Revision 1.60  2007/09/07 00:18:34  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.59  2007/07/30 18:33:51  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.58  2007/05/31 22:25:39  sueh
 * <p> bug# 1004 Added createNewFile(), a more reliable way to create a new
 * <p> file.  Changed touch() to check for the files existence after it has been
 * <p> created.
 * <p>
 * <p> Revision 1.57  2007/05/14 17:32:56  sueh
 * <p> bug# 964 Removed print statements.
 * <p>
 * <p> Revision 1.56  2007/05/11 15:39:14  sueh
 * <p> bug# 964 Added msgProcessDone(DetachedProcess,int,boolean) and
 * <p> postProcess(DetachedProcess) to handle processchunks, since
 * <p> processName is not set for Peet.
 * <p>
 * <p> Revision 1.55  2007/02/05 22:52:30  sueh
 * <p> bug# 962 Added a generalized writeLogFile for transferfid and xfjointomo.
 * <p>
 * <p> Revision 1.54  2006/12/02 04:33:52  sueh
 * <p> bug# 944 In processchunks, use ProcesschunksVolcombineMonitor if the param
 * <p> was created for volcombine.
 * <p>
 * <p> Revision 1.53  2006/12/01 00:54:40  sueh
 * <p> bug# 937 In processchunks, don't have to set process in monitor because its
 * <p> being passed to the kill function.
 * <p>
 * <p> Revision 1.52  2006/11/30 19:58:47  sueh
 * <p> bug# 937 In processchunks(), setting the process in the monitor.
 * <p>
 * <p> Revision 1.51  2006/11/28 22:52:31  sueh
 * <p> bug# 934 Added endGetLoadAverage().  Does the same thing as
 * <p> stopGetLoadAverage(), but also removes the monitor.  Uses when a manager
 * <p> exits.
 * <p>
 * <p> Revision 1.50  2006/11/15 18:53:21  sueh
 * <p> bug# 872 Changed getParamFileStorableArray to getStorables in the managers.
 * <p>
 * <p> Revision 1.49  2006/10/16 22:36:52  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p> Revision 1.48  2006/10/10 05:06:54  sueh
 * <p> bug# 931 In startBackgroundProcess() and startComScript() set
 * <p> etomo.processThreadGroup to be the ThreadGroup, so that exceptions can be
 * <p> handled during a UITest.
 * <p>
 * <p> Revision 1.47  2006/08/03 21:28:01  sueh
 * <p> bug# 769 MsgReconnectDone():  When calling manager.processDone, also
 * <p> setting failed to true when the process end state is not done.
 * <p>
 * <p> Revision 1.46  2006/08/02 22:15:39  sueh
 * <p> bug# 769 Added msgReconnectDone()
 * <p>
 * <p> Revision 1.45  2006/07/27 19:37:41  sueh
 * <p> bug# 908 MsgComScriptDone():  removing unnecessary print
 * <p>
 * <p> Revision 1.44  2006/07/19 20:08:43  sueh
 * <p> bug# 902 MsgProcessDone(BackgroundProcess...):  Passing process name to
 * <p> manager.processDone().
 * <p>
 * <p> Revision 1.43  2006/06/05 16:21:59  sueh
 * <p> bug# 766 Added saved ProcessData member variables, which are loaded
 * <p> from the data file.  Changed isAxisBusy() to check the saved ProcessData and
 * <p> checking the thread variable.  The axis is busy if the data stored in the saved
 * <p> ProcessData matches a running process.
 * <p>
 * <p> Revision 1.42  2006/05/22 22:45:52  sueh
 * <p> bug# 577 Placed commands in a String[] rather then a String.
 * <p>
 * <p> Revision 1.41  2006/05/11 19:51:55  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.40  2006/03/30 16:38:23  sueh
 * <p> bug# 839 print any recursive kills in the error log.
 * <p>
 * <p> Revision 1.39  2006/03/27 19:17:15  sueh
 * <p> Adding a print to killProcessAndDescendants.  We want to see how many
 * <p> processes are killed this was as apposed to the group kill
 * <p>
 * <p> Revision 1.38  2006/01/31 20:39:31  sueh
 * <p> bug# 521 startBackgoundComScript:  added the process to combine
 * <p> monitor.  This allows the last ProcessResultDisplay used by the monitor
 * <p> to be assigned to the process.
 * <p>
 * <p> Revision 1.37  2006/01/26 21:52:54  sueh
 * <p> Added processResultDisplay parameters to all the functions associated
 * <p> with toggle buttons.
 * <p>
 * <p> Revision 1.36  2006/01/20 20:50:38  sueh
 * <p> bug# 401 Added boolean error parameter to processDone().
 * <p>
 * <p> Revision 1.35  2006/01/06 02:39:16  sueh
 * <p> bug# 792 Using DetachedCommand instead of Command because it can
 * <p> create a safe command string that can go into a run file.
 * <p>
 * <p> Revision 1.34  2005/12/14 01:27:13  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.33  2005/12/12 21:59:17  sueh
 * <p> bug# 778 Made isAxisBusy protected and added public inUse, which
 * <p> doesn't throw an exception.
 * <p>
 * <p> Revision 1.32  2005/12/09 20:26:11  sueh
 * <p> bug# 776 Added tomosnapshot.
 * <p>
 * <p> Revision 1.31  2005/11/19 02:17:33  sueh
 * <p> bug# 744 Changed msgBackgroundProcessDone to msgProcessDone.
 * <p> Moved the error message display functionality to BackgroundProcess.
 * <p> Moved functions only used by process manager post processing and
 * <p> error processing from Commands to ProcessDetails.  This allows
 * <p> ProcesschunksParam to be passed to DetachedProcess without having
 * <p> to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.30  2005/11/02 21:43:39  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.  Put
 * <p> error messages created in msgComScriptDone() directly into
 * <p> processMessages.
 * <p>
 * <p> Revision 1.29  2005/10/27 00:29:27  sueh
 * <p> bug# 725 Added another startBackgroundProcess() function with a
 * <p> forceNextProcess parameter.
 * <p>
 * <p> Revision 1.28  2005/09/29 18:39:17  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 1.27  2005/09/22 20:52:25  sueh
 * <p> bug# 532 for processchunks, added the status string to "killed", which can
 * <p> be resumed just like "paused".
 * <p>
 * <p> Revision 1.26  2005/09/21 16:09:30  sueh
 * <p> bug# 532 moved processchunks() from processManager to
 * <p> BaseProcessManager.  This allows BaseManager to handle
 * <p> processchunks.
 * <p>
 * <p> Revision 1.25  2005/09/13 00:14:46  sueh
 * <p> bug# 532 Made isAxisBusy() public so that BaseManager can use it.
 * <p>
 * <p> Revision 1.24  2005/09/10 01:48:39  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed and
 * <p> keeping separate lists of standard output for separate monitors.
 * <p>
 * <p> Revision 1.23  2005/09/09 21:21:52  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
 * <p> Revision 1.22  2005/08/30 18:37:38  sueh
 * <p> bug# 532 Changing monitor interfaces for
 * <p> startInteractiveBackgroundProcess() because the combine monitor now
 * <p> implements BackgroundComScriptMonitor.
 * <p>
 * <p> Revision 1.21  2005/08/27 22:23:50  sueh
 * <p> bug# 532 In msgBackgroundPRocessDone() exclude errors starting with
 * <p> "CHUNK ERROR:".  These error may be repeats many times and should
 * <p> be handled by the monitor.  Also try to append an error string from the
 * <p> monitor.  This allows the processchunks monitor to supply the last chunk
 * <p> error it found.
 * <p>
 * <p> Revision 1.20  2005/08/22 16:17:46  sueh
 * <p> bug# 532 Added start and stopGetLoadAverage().
 * <p>
 * <p> Revision 1.19  2005/08/15 17:55:29  sueh
 * <p> bug# 532   Processchunks needs to be killed with an interrupt instead of
 * <p> a kill, so a processchunks specific class has to make the decision of
 * <p> what type of signal to send.  Change BaseProcessManager.kill() to call
 * <p> SystemProcessInterface.kill().  When the correct signal is chosen,
 * <p> SystemProcessInterface will call either signalInterrupt or signalKill.  Also
 * <p> added pause(), which will be associaated with the Pause button and will
 * <p> work like kill.  Added functions:  getThread, interruptProcess, pause,
 * <p> signalInterrupt, signalKill.
 * <p>
 * <p> Revision 1.18  2005/08/04 19:42:50  sueh
 * <p> bug# 532 Passing monitor to BackgroundProcess when necessary.
 * <p>
 * <p> Revision 1.17  2005/08/01 18:00:42  sueh
 * <p> bug# 532 In msgBackgroundProcessDone() passed process.axisID
 * <p> instead of null to processDone().
 * <p>
 * <p> Revision 1.16  2005/07/29 00:51:13  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.15  2005/07/26 17:57:12  sueh
 * <p> bug# 701 Changing all comscript monitors to implement ProcessMonitor
 * <p> so that they call all be passed to the ComScriptProcess constructor.
 * <p> Changed kill(AxisID).  Before killing, call
 * <p> SystemProcessInterface.setProcessEndState().  After killing call
 * <p> SystemProcessInterface.notifyKilled().  This sets the end state to kill as
 * <p> soon as possible.  NotifyKilled() (was notifyKill) is used to stop monitors
 * <p> that can't receives interruption from the processes they are monitoring.
 * <p> Modify msgComScriptDone() and msgBackgroundProcessDone().  Do not
 * <p> open an error dialog if a kill or pause ProcessEndState is set.  Set the
 * <p> process's ProcessEndState to FAILED if an error dialo is being opened.
 * <p> The process may or may not find out about the error, so the function that
 * <p> finds the problem needs to set ProcessEndState.
 * <p>
 * <p> Revision 1.14  2005/07/01 21:08:00  sueh
 * <p> bug# 619 demo:  temporarily made isAxisBusy public
 * <p>
 * <p> Revision 1.13  2005/06/21 00:42:24  sueh
 * <p> bug# 522 Added moved touch() from JoinProcessManager to
 * <p> BaseProcessManager for MRCHeaderTest.
 * <p>
 * <p> Revision 1.12  2005/05/18 22:34:19  sueh
 * <p> bug# 662 Added member variable boolean forceNextProcess to force
 * <p> BaseManager.startNextProcess() to be run regardless of the value of
 * <p> exitValue.
 * <p>
 * <p> Revision 1.11  2005/04/26 17:36:26  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 1.10  2005/04/25 20:44:28  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 1.9  2005/01/05 19:52:39  sueh
 * <p> bug# 578 Moved startBackgroundComScript(String, Runnable, AxisID,
 * <p> ComscriptState, String) and startComScript(String, Runnable, AxisID,
 * <p> String) from ProcessManager to BaseProcessManager since they are
 * <p> generic.  Added startComScript(Command, Runnable, AxisID) to handle
 * <p> situations where postProcess(ComScriptProcess) needs to query the
 * <p> command.
 * <p>
 * <p> Revision 1.8  2004/12/14 21:33:25  sueh
 * <p> bug# 565: Fixed bug:  Losing process track when backing up .edf file and
 * <p> only saving metadata.  Removed unnecessary class JoinProcessTrack.
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.
 * <p> Saving all objects to the .edf/ejf file each time a save is done.
 * <p>
 * <p> Revision 1.7  2004/12/13 19:08:56  sueh
 * <p> bug# 565 Saving process track to edf file as well as meta data in the
 * <p> start... functions.
 * <p>
 * <p> Revision 1.6  2004/12/09 05:04:34  sueh
 * <p> bug# 565 Added save meta data to each msg...Done function regardless
 * <p> of success or failure.
 * <p>
 * <p> Revision 1.5  2004/12/09 04:52:54  sueh
 * <p> bug# 565 Saving meta data on each top of start function.
 * <p>
 * <p> Revision 1.4  2004/11/24 00:59:23  sueh
 * <p> bug# 520 msgBackgroundProcess:  call errorProcess is exitValue != 0.
 * <p>
 * <p> Revision 1.3  2004/11/20 01:58:22  sueh
 * <p> bug# 520 Passing exitValue to postProcess(BackgroundProcess).
 * <p>
 * <p> Revision 1.2  2004/11/19 23:17:50  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.8  2004/11/12 22:52:59  sueh
 * <p> bug# 520 Using overloading to simiplify the postProcess function names.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/25 23:10:39  sueh
 * <p> bug# 520 Added a call to backgroundErrorProcess() for post processing
 * <p> when BackgroundProcess fails.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/21 02:39:49  sueh
 * <p> bug# 520 Created functions to manager InteractiveSystemProgram:
 * <p> startInteractiveSystemProgram, msgInteractivesystemProgramDone,
 * <p> interactiveSystemProgramPostProcess.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/18 19:08:18  sueh
 * <p> bug# 520 Replaced manager with abstract BaseManager getManager().
 * <p> The type of manager that is stored will be decided by
 * <p> BaseProcessManager's children.  Moved startSystemProgramThread() to
 * <p> the base class.  Added an interface to this function to handle
 * <p> String[] command.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/11 02:02:37  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/08 15:55:17  sueh
 * <p> bug# 520 Handled command array in BackgroundProcess.  Since
 * <p> EtomoDirector is a singleton, made all functions and member variables
 * <p> non-static.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/06 01:38:00  sueh
 * <p> bug# 520 Added abstract backgroundPostProcessing to handle
 * <p> non-generic processing during msgBackgroundProcessDone().  Added
 * <p> startBackgroundProcess() functions to handle constructing
 * <p> BackgroundProcess with a Command rather then a String.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 17:48:18  sueh
 * <p> bug# 520 Contains functionality that is command for ProcessManager and
 * <p> JoinProcessManager.
 * <p> </p>
 */
public abstract class BaseProcessManager {
  public static final String rcsid = "$Id$";

  UIHarness uiHarness = UIHarness.INSTANCE;

  private SystemProcessInterface threadAxisA = null;

  private SystemProcessInterface threadAxisB = null;

  private Thread processMonitorA = null;

  private Thread processMonitorB = null;

  private boolean debug = false;

  private final HashMap killedList = new HashMap();

  private final ProcessData savedProcessDataA;

  private final ProcessData savedProcessDataB;

  private final BaseManager manager;

  private boolean blockAxisA = true;

  private boolean blockAxisB = true;

  private final EtomoDirector etomoDirector = EtomoDirector.INSTANCE;

  abstract void postProcess(ComScriptProcess script);

  abstract void errorProcess(BackgroundProcess process);

  abstract void postProcess(InteractiveSystemProgram program);

  abstract void errorProcess(ComScriptProcess process);

  abstract void errorProcess(ReconnectProcess script);

  abstract void postProcess(ReconnectProcess script);

  BaseProcessManager(final BaseManager manager) {
    this.manager = manager;
    savedProcessDataA = new ProcessData(AxisID.FIRST, manager);
    savedProcessDataB = new ProcessData(AxisID.SECOND, manager);
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  final String paramString() {
    return "threadAxisA=" + threadAxisA + ",threadAxisB=" + threadAxisB
        + ",\nprocessMonitorA=" + processMonitorA + ",processMonitorB="
        + processMonitorB + ",\nkilledList=" + killedList + ",uiHarness="
        + uiHarness + "," + super.toString();
  }

  public final void startLoad(final IntermittentCommand param,
      final LoadMonitor monitor) {
    IntermittentBackgroundProcess.startInstance(manager, param, monitor);
  }

  public final void endLoad(final IntermittentCommand param,
      final LoadMonitor monitor) {
    IntermittentBackgroundProcess.endInstance(manager, param, monitor);
  }

  public final void stopLoad(final IntermittentCommand param,
      final LoadMonitor monitor) {
    IntermittentBackgroundProcess.stopInstance(manager, param, monitor);
  }

  public String xfmodel(XfmodelParam param, AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, axisID,
        processResultDisplay, ProcessName.XFMODEL, processSeries);
    return backgroundProcess.getName();
  }

  public final boolean reconnectProcesschunks(final AxisID axisID,
      final ProcessData processData,
      final ParallelProgressDisplay parallelProgressDisplay,
      final ProcessResultDisplay processResultDisplay) {
    ProcesschunksProcessMonitor monitor = ProcesschunksProcessMonitor
        .getReconnectInstance(manager, axisID, parallelProgressDisplay,
            processData);
    monitor.setSubdirName(processData.getSubDirName());
    boolean ret;
    try {
      ReconnectProcess process = ReconnectProcess.getMonitorInstance(manager,
          this, monitor, getSavedProcessData(axisID), axisID, monitor
              .getLogFileName(), ProcesschunksProcessMonitor.SUCCESS_TAG,
          processData.getSubDirName());
      monitor.setProcess(process);
      process.setProcessResultDisplay(processResultDisplay);
      Thread thread = new Thread(process);
      thread.start();
      mapAxisThread(process, axisID);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(
          "Unable to reconnect to processchunks.\n" + e.getMessage(),
          "Reconnect Failure", axisID, manager.getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * run processchunks
   * @param axisID
   * @param param
   * @return
   * @throws SystemProcessException
   */
  public final String processchunks(final AxisID axisID,
      final ProcesschunksParam param,
      final ParallelProgressDisplay parallelProgressDisplay,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    // Instantiate the process monitor
    ProcesschunksProcessMonitor monitor;
    if (param.equalsRootName(ProcessName.VOLCOMBINE, axisID)) {
      monitor = new ProcesschunksVolcombineMonitor(manager, axisID,
          parallelProgressDisplay, param.getRootName(), param.getMachineList());
    }
    else {
      monitor = new ProcesschunksProcessMonitor(manager, axisID,
          parallelProgressDisplay, param.getRootName(), param.getMachineList());
    }
    BackgroundProcess process;
    if (param.isSubdirNameEmpty()) {
      process = startDetachedProcess(param, axisID, monitor,
          processResultDisplay, ProcessName.PROCESSCHUNKS, processSeries);

    }
    else {
      monitor.setSubdirName(param.getSubdirName());
      process = startDetachedProcess(param, axisID, monitor,
          processResultDisplay, ProcessName.PROCESSCHUNKS, param
              .getSubdirName(), param.getShortCommandName(), processSeries);
    }
    return process.getName();
  }

  public final void createNewFile(final String absolutePath) {
    File file = new File(absolutePath);
    if (file.exists()) {
      return;
    }
    File dir = file.getParentFile();
    if (!dir.exists()) {
      if (!dir.mkdirs()) {
        uiHarness.openMessageDialog(
            "Unable to create " + dir.getAbsolutePath(), "File Error", manager
                .getManagerKey());
        return;
      }
    }
    if (!dir.canWrite()) {
      uiHarness.openMessageDialog("Cannot write to " + dir.getAbsolutePath(),
          "File Error", manager.getManagerKey());
      return;
    }
    try {
      if (!file.createNewFile()) {
        uiHarness.openMessageDialog("Cannot create  " + file.getAbsolutePath(),
            "File Error", manager.getManagerKey());
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Cannot create  " + file.getAbsolutePath()
          + ".\n" + e.getMessage(), "File Error", manager.getManagerKey());
    }
  }

  /**
   * run touch command on file
   * @param file
   */
  public final void touch(final String absolutePath) {
    File file = new File(absolutePath);
    File dir = file.getParentFile();
    if (!dir.exists()) {
      if (!dir.mkdirs()) {
        uiHarness.openMessageDialog(
            "Unable to create " + dir.getAbsolutePath(), "File Error", manager
                .getManagerKey());
        return;
      }
    }
    if (!dir.canWrite()) {
      uiHarness.openMessageDialog("Cannot write to " + dir.getAbsolutePath(),
          "File Error", manager.getManagerKey());
      return;
    }
    String[] commandArray = { "touch", absolutePath };
    startSystemProgramThread(commandArray, AxisID.ONLY);
    final int timeout = 5;
    int t = 0;
    while (!file.exists() && t < timeout) {
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
      t++;
    }
  }

  final ComScriptProcess startComScript(final String command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ProcessDetails processDetails,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        null, processMonitor, processResultDisplay, processDetails,
        processSeries), command, processMonitor, axisID);
  }

  final ComScriptProcess startComScript(final String command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final CommandDetails commandDetails,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        null, processMonitor, processResultDisplay, commandDetails,
        processSeries), command, processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(final String command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        null, processMonitor, processResultDisplay, processSeries), command,
        processMonitor, axisID);
  }

  /**
   * 
   * @param command
   * @param axisID
   * @param processResultDisplay
   * @return
   * @throws SystemProcessException
   */
  final void startNonBlockingComScript(final String command,
      final AxisID axisID, final ProcessResultDisplay processResultDisplay)
      throws SystemProcessException {
    startNonBlockingComScript(new ComScriptProcess(manager, command, this,
        axisID, processResultDisplay), command, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(final String command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        null, processMonitor, processSeries), command, processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(final CommandDetails commandDetails,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, commandDetails, this,
        axisID, null, processMonitor, processSeries), commandDetails
        .getCommandLine(), processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(final CommandDetails commandDetails,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, commandDetails, this,
        axisID, null, processMonitor, processResultDisplay, processSeries),
        commandDetails.getCommandLine(), processMonitor, axisID);
  }

  final ComScriptProcess startComScript(final Command command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        null, processMonitor, processResultDisplay, processSeries), command
        .getCommandLine(), processMonitor, axisID);
  }

  /**
   * Start a managed background command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startBackgroundComScript(final String comscript,
      final DetachedProcessMonitor processMonitor, final AxisID axisID,
      final ComscriptState comscriptState, final String watchedFileName,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundComScriptProcess process = new BackgroundComScriptProcess(
        manager, comscript, this, axisID, watchedFileName, processMonitor,
        comscriptState, processSeries);
    processMonitor.setProcess(process);
    return startComScript(process, comscript, processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(final String command,
      final ProcessMonitor processMonitor, final AxisID axisID,
      final String watchedFileName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    return startComScript(new ComScriptProcess(manager, command, this, axisID,
        watchedFileName, processMonitor, processSeries), command,
        processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  final ComScriptProcess startComScript(
      final ComScriptProcess comScriptProcess, final String command,
      Runnable processMonitor, final AxisID axisID)
      throws SystemProcessException {
    // Make sure there isn't something going on in the current axis
    isAxisBusy(axisID, comScriptProcess.getProcessResultDisplay());

    // Run the script as a thread in the background
    comScriptProcess
        .setWorkingDirectory(new File(manager.getPropertyUserDir()));
    comScriptProcess.setDebug(etomoDirector.getArguments().isDebug());
    comScriptProcess.setDemoMode(etomoDirector.getArguments().isDemo());
    manager.saveStorables(axisID);
    comScriptProcess.start();

    // Map the thread to the correct axis
    mapAxisThread(comScriptProcess, axisID);

    if (etomoDirector.getArguments().isDebug()) {
      System.err.println("Started " + command);
      System.err.println("  Name: " + comScriptProcess.getName());
    }

    Thread processMonitorThread = null;
    // Replace the process monitor with a DemoProcessMonitor if demo mode is on
    if (etomoDirector.getArguments().isDemo()) {
      processMonitor = new DemoProcessMonitor(manager, axisID, command,
          comScriptProcess.getDemoTime());
    }

    // Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the comScriptProcess, this ensures
      // that log file has already been moved
      while (!comScriptProcess.isStarted() && !comScriptProcess.isError()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException e) {
          break;
        }
      }
      processMonitorThread = new Thread(new etomo.process.ThreadGroup(
          "startComScript"), processMonitor);
      processMonitorThread.start();
      mapAxisProcessMonitor(processMonitorThread, axisID);
    }

    return comScriptProcess;
  }

  /**
   * Start an unmanaged comscript.
   * @param comScriptProcess
   * @param command
   * @param axisID
   * @throws SystemProcessException
   */
  final void startNonBlockingComScript(final ComScriptProcess comScriptProcess,
      final String command, final AxisID axisID) throws SystemProcessException {
    // Run the script as a thread in the background
    comScriptProcess
        .setWorkingDirectory(new File(manager.getPropertyUserDir()));
    comScriptProcess.setDebug(etomoDirector.getArguments().isDebug());
    comScriptProcess.setDemoMode(etomoDirector.getArguments().isDemo());
    comScriptProcess.setNonBlocking();
    manager.saveStorables(axisID);
    comScriptProcess.start();

    if (etomoDirector.getArguments().isDebug()) {
      System.err.println("Started " + command);
      System.err.println("  Name: " + comScriptProcess.getName());
    }
  }

  public final boolean inUse(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
    try {
      isAxisBusy(axisID, processResultDisplay);
    }
    catch (SystemProcessException e) {
      if (debug) {
        e.printStackTrace();
      }
      uiHarness.openMessageDialog(
          "A process is already executing in the current axis",
          "Cannot run process", axisID, manager.getManagerKey());
      return true;
    }
    return false;
  }

  /**
   * Check to see if specified axis is busy, throw a system a
   * ProcessProcessException if it is.
   * 
   * @param axisID
   * @throws SystemProcessException
   */
  final void isAxisBusy(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay)
      throws SystemProcessException {
    // Check to make sure there is not another process already running on this
    // axis.
    boolean busy = false;
    if (axisID == AxisID.SECOND) {
      if (threadAxisB != null) {
        busy = true;
      }
    }
    else if (threadAxisA != null) {
      busy = true;
    }
    if (busy) {
      if (processResultDisplay != null) {
        processResultDisplay.msgProcessFailedToStart();
      }
      throw new SystemProcessException(
          "A process is already executing in the current axis");
    }
    //check for running processes that are not managed by Etomo because the user
    // exited and then reran Etomo
    ProcessData savedProcessData = getSavedProcessData(axisID);
    if (savedProcessData.isRunning()) {
      if (processResultDisplay != null) {
        processResultDisplay.msgProcessFailedToStart();
      }
      StringBuffer message = new StringBuffer();
      message
          .append("A process is running on the current axis.\nThe process was started the last time Etomo was run");
      if (savedProcessData.isOnDifferentHost()) {
        message.append(" on " + savedProcessData.getHostName());
      }
      message.append("." + "\n\nReferences:" + "\nProcessName="
          + savedProcessData.getProcessName() + "\n" + manager.getParamFile()
          + "\nPID = " + savedProcessData.getPid());
      throw new SystemProcessException(message.toString());

    }
    else {
      // ensure that out of date process info won't be resaved
      savedProcessData.reset();
      saveProcessData(savedProcessData);
    }
    if (axisID == AxisID.SECOND) {
      if (blockAxisB) {
        throw new SystemProcessException(
            "Process attempting to restart - axis B is blocked.");
      }
    }
    else if (blockAxisA) {
      throw new SystemProcessException(
          "Process attempting to restart - axis A is blocked.");
    }
  }

  public final void unblockAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      blockAxisB = false;
    }
    else {
      blockAxisA = false;
    }
  }

  public final ProcessData getSavedProcessData(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return savedProcessDataB;
    }
    return savedProcessDataA;
  }

  private void saveProcessData(final ProcessData processData) {
    try {
      ParameterStore paramStore = ParameterStore.getInstance(manager
          .getParamFile(), manager.getManagerKey());
      if (paramStore == null) {
        return;
      }
      paramStore.save(processData);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Save the process thread reference for the appropriate axis
   * 
   * @param thread
   * @param axisID
   */
  final void mapAxisThread(final SystemProcessInterface thread,
      final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadAxisB = thread;
    }
    else {
      threadAxisA = thread;
    }
  }

  /**
   * Save the process monitor thread reference for the appropriate axis
   * 
   * @param processMonitor
   * @param axisID
   */
  private void mapAxisProcessMonitor(final Thread processMonitor,
      final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      processMonitorB = processMonitor;
    }
    else {
      processMonitorA = processMonitor;
    }
  }

  public final SystemProcessInterface getThread(final AxisID axisID) {
    SystemProcessInterface thread = null;
    if (axisID == AxisID.SECOND) {
      thread = threadAxisB;
    }
    else {
      thread = threadAxisA;
    }
    return thread;
  }

  public final ProcessData getProcessData(final AxisID axisID) {
    SystemProcessInterface thread = getThread(axisID);
    if (thread == null) {
      return getSavedProcessData(axisID);
    }
    ProcessData processData = thread.getProcessData();
    if (processData == null) {
      return getSavedProcessData(axisID);
    }
    return processData;
  }

  public final void pause(final AxisID axisID) {
    SystemProcessInterface thread = getThread(axisID);
    if (thread == null) {
      return;
    }
    thread.pause(axisID);
  }

  public final void kill(final AxisID axisID) {
    SystemProcessInterface thread = getThread(axisID);
    if (thread == null) {
      return;
    }
    thread.kill(axisID);
  }

  /**
   * Kill the thread for the specified axis
   */
  final void signalKill(final SystemProcessInterface thread, final AxisID axisID) {
    String processID = "";
    thread.setProcessEndState(ProcessEndState.KILLED);

    processID = thread.getShellProcessID();
    killProcessGroup(processID, axisID);
    killProcessAndDescendants(processID, axisID);
    try {
      Thread.sleep(100);
    }
    catch (InterruptedException e) {
    }
    thread.notifyKilled();
  }

  private void killProcessGroup(final String processID, final AxisID axisID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    long pid = Long.parseLong(processID);
    if (pid == 0 || pid == 1) {
      return;
    }
    long groupPid = pid * -1;
    String groupProcessID = Long.toString(groupPid);
    kill("-19", groupProcessID, axisID);
    kill("-9", groupProcessID, axisID);
  }

  /**
   * Recursively kill all the descendents of a process and then kill the
   * process.  Function assumes that the process will continue spawning while
   * the descendant processes are being killed.  Function attempts to stop
   * spawning with a Stop signal.  The Stop signal may not work in all cases and
   * OS's, so the function refreshes the list of child processes until there are
   * no more child processes.  The function avoids getting stuck on an
   * unkillable process by recording each PID it sent a "kill -9" to.
   * 
   * The algorithm:
   * 1. Stop the root process.
   * 2. Go down to a leaf, stopping each process encountered.
   * 3. Kill the leaf.
   * 4. Go up to the parent of the killed leaf.
   * 5. If the parent is now a leaf, kill it and continue from step 4.
   * 6. If the parent is not a leaf, continue from step 2.
   * 
   * @param processID
   */
  private void killProcessAndDescendants(final String processID,
      final AxisID axisID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    // try to prevent process from spawning with a SIGSTOP signal
    kill("-19", processID, axisID);

    // kill all decendents of process before killing process
    String[] childProcessIDList = null;
    do {
      // get unkilled child processes
      childProcessIDList = getChildProcessList(processID, axisID);
      if (childProcessIDList != null) {
        for (int i = 0; i < childProcessIDList.length; i++) {
          killProcessAndDescendants(childProcessIDList[i], axisID);
        }
      }
    } while (childProcessIDList != null);
    // there are no more unkilled child processes so kill process with a SIGKILL
    // signal
    kill("-9", processID, axisID);
    System.err.println("killProcessAndDescendants:kill " + "-9" + " "
        + processID);
    // record killed process
    killedList.put(processID, "");
  }

  private void kill(final String signal, final String processID,
      final AxisID axisID) {
    SystemProgram killShell = new SystemProgram(manager.getPropertyUserDir(),
        new String[] { "kill", signal, processID }, axisID, manager
            .getManagerKey());
    killShell.run();
    //"kill " + signal + " " + processID + " at " + killShell.getRunTimestamp());
    Utilities.debugPrint("kill " + signal + " " + processID + " at "
        + killShell.getRunTimestamp());
  }

  /**
   * Return a the PIDs of child processes for the specified parent process. A
   * new ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used. Only processes that have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  private String[] getChildProcessList(final String processID,
      final AxisID axisID) {
    Utilities.debugPrint("in getChildProcessList: processID=" + processID);
    // ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram(manager.getPropertyUserDir(),
        new String[] { "ps", "axl" }, axisID, manager.getManagerKey());
    ps.run();
    // System.out.println("ps axl date=" + ps.getRunTimestamp());
    // Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    if (stdout == null) {
      return null;
    }
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int idxPGID = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (labels[i].equals("PGID")) {
        idxPGID = i;
      }
      if (found >= 3) {
        break;
      }
    }
    // Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    ArrayList childrenPID = new ArrayList();
    String[] fields;
    // System.out.println(stdout[0]);
    for (int i = 1; i < stdout.length; i++) {
      // System.out.println(stdout[i]);
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
          && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint("child found:PID=" + fields[idxPID] + ",PPID="
              + fields[idxPPID] + ",name=" + fields[idxCMD]);
        }
        childrenPID.add(fields[idxPID]);
      }
    }

    // If there are no children return null
    if (childrenPID.size() == 0) {
      return null;
    }

    // Connvert the ArrayList into a String[]
    String[] children = (String[]) childrenPID.toArray(new String[childrenPID
        .size()]);
    return children;
  }

  /**
   * Return a PID of a child process for the specified parent process.  A new
   * ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used. Only processes the have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  private String getChildProcess(final String processID, final AxisID axisID) {
    Utilities.debugPrint("in getChildProcess: processID=" + processID);
    // ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram(manager.getPropertyUserDir(),
        new String[] { "ps", "axl" }, axisID, manager.getManagerKey());
    ps.run();

    // Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    if (stdout == null) {
      return null;
    }
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (found >= 3) {
        break;
      }
    }
    // Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    String[] fields;
    for (int i = 1; i < stdout.length; i++) {
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
          && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint("child found:PID=" + fields[idxPID] + ",PPID="
              + fields[idxPPID] + ",name=" + fields[idxCMD]);
        }
        return fields[idxPID];
      }
    }
    return null;
  }

  /**
   * A message specifying that a com script has finished execution
   * 
   * @param script
   *          the ComScriptProcess execution object that finished
   * @param exitValue
   *          the exit value for the com script
   */
  public final void msgComScriptDone(final ComScriptProcess script,
      final int exitValue, final boolean nonBlocking) {
    System.err
        .println("msgComScriptDone:scriptName=" + script.getComScriptName()
            + ",processName=" + script.getProcessName());
    if (exitValue != 0) {
      String[] stdError = script.getStdError();
      ProcessMessages combinedMessages = ProcessMessages.getInstance();
      // Is the last string "Killed"
      if (stdError != null && stdError.length > 0
          && stdError[stdError.length - 1].trim().equals("Killed")) {
        combinedMessages.addError("<html>Terminated: "
            + script.getComScriptName());
      }
      else {
        ProcessMessages messages = script.getProcessMessages();/* Error */
        int j = 0;
        combinedMessages.addError("<html>Com script failed: "
            + script.getComScriptName());
        combinedMessages.addError("\n<html><U>Log file errors:</U>");
        combinedMessages.addError(messages);
        combinedMessages.addError("\n<html><U>Standard error output:</U>");
        combinedMessages.addError(stdError);
      }
      if (script.getProcessEndState() != ProcessEndState.KILLED
          && script.getProcessEndState() != ProcessEndState.PAUSED) {
        uiHarness.openErrorMessageDialog(combinedMessages, script
            .getComScriptName()
            + " terminated", script.getAxisID(), manager.getManagerKey());
        // make sure script knows about failure
        script.setProcessEndState(ProcessEndState.FAILED);
      }
      errorProcess(script);
    }
    else {
      postProcess(script);
      ProcessMessages messages = script.getProcessMessages();/* Warning */
      if (messages.warningListSize() > 0) {
        messages.addWarning("Com script: " + script.getComScriptName());
        uiHarness.openWarningMessageDialog(messages, script.getComScriptName()
            + " warnings", script.getAxisID(), manager.getManagerKey());
      }
    }
    manager.saveStorables(script.getAxisID());
    // Null out the correct thread
    // Interrupt the process monitor and nulll out the appropriate references
    if (threadAxisA == script) {
      if (processMonitorA != null) {
        processMonitorA.interrupt();
        processMonitorA = null;
      }
      threadAxisA = null;
    }
    if (threadAxisB == script) {
      if (processMonitorB != null) {
        processMonitorB.interrupt();
        processMonitorB = null;
      }
      threadAxisB = null;
    }

    // Inform the app manager that this process is complete
    manager.processDone(script.getName(), exitValue, script.getProcessName(),
        script.getAxisID(), script.getProcessEndState(), exitValue != 0, script
            .getProcessResultDisplay(), script.getProcessSeries(), nonBlocking);
  }

  public final void msgReconnectDone(final ReconnectProcess script,
      final int exitValue) {
    String name = script.getProcessData().getProcessName().toString();
    System.err.println("msgReconnectDone:processName=" + name);
    if (exitValue != 0) {
      String[] stdError = script.getStdError();
      ProcessMessages combinedMessages = ProcessMessages.getInstance();
      // Is the last string "Killed"
      if (stdError != null && stdError.length > 0
          && stdError[stdError.length - 1].trim().equals("Killed")) {
        combinedMessages.addError("<html>Terminated: " + name);
      }
      else {
        ProcessMessages messages = script.getProcessMessages();/* Error */
        int j = 0;
        combinedMessages.addError("<html>Com script failed: " + name);
        combinedMessages.addError("\n<html><U>Log file errors:</U>");
        combinedMessages.addError(messages);
        combinedMessages.addError("\n<html><U>Standard error output:</U>");
        combinedMessages.addError(stdError);
      }
      if (script.getProcessEndState() != ProcessEndState.KILLED
          && script.getProcessEndState() != ProcessEndState.PAUSED) {
        uiHarness.openErrorMessageDialog(combinedMessages,
            name + " terminated", script.getAxisID(), manager.getManagerKey());
        // make sure script knows about failure
        script.setProcessEndState(ProcessEndState.FAILED);
      }
      errorProcess(script);
    }
    else {
      postProcess(script);
      ProcessMessages messages = script.getProcessMessages();/* Warning */
      if (messages.warningListSize() > 0) {
        messages.addWarning("Com script: " + name);
        uiHarness.openWarningMessageDialog(messages, name + " warnings", script
            .getAxisID(), manager.getManagerKey());
      }
    }
    manager.saveStorables(script.getAxisID());
    // Null out the correct thread
    // Interrupt the process monitor and nulll out the appropriate references
    if (threadAxisA == script) {
      if (processMonitorA != null) {
        processMonitorA.interrupt();
        processMonitorA = null;
      }
      threadAxisA = null;
    }
    if (threadAxisB == script) {
      if (processMonitorB != null) {
        processMonitorB.interrupt();
        processMonitorB = null;
      }
      threadAxisB = null;
    }
    // Inform the app manager that this process is complete
    manager.processDone(name, exitValue, script.getProcessData()
        .getProcessName(), script.getAxisID(), script.getProcessEndState(),
        script.getProcessEndState() != ProcessEndState.DONE || exitValue != 0,
        script.getProcessResultDisplay(), script.getProcessSeries(), false);
  }

  /**
   * Start a managed background process
   * 
   * @param command
   * @param axisID
   * @throws SystemProcessException
   */
  final BackgroundProcess startBackgroundProcess(final ArrayList command,
      final AxisID axisID, final ProcessResultDisplay processResultDisplay,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        command, this, axisID, processResultDisplay, processName, processSeries);
    return startBackgroundProcess(backgroundProcess, backgroundProcess
        .getCommandLine(), axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(final String[] commandArray,
      final AxisID axisID, final ProcessResultDisplay processResultDisplay,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        commandArray, this, axisID, processResultDisplay, processName,
        processSeries);
    return startBackgroundProcess(backgroundProcess, commandArray.toString(),
        axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(final String[] commandArray,
      final AxisID axisID, final boolean forceNextProcess,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries, final ProcessName processName)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        commandArray, this, axisID, forceNextProcess, processResultDisplay,
        processSeries, processName);
    return startBackgroundProcess(backgroundProcess, commandArray.toString(),
        axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(final String[] commandArray,
      final AxisID axisID, final ProcessName processName,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        commandArray, this, axisID, processName, processSeries);
    return startBackgroundProcess(backgroundProcess, commandArray.toString(),
        axisID, null);
  }

  final BackgroundProcess startDetachedProcess(
      final DetachedCommand detachedCommand, final AxisID axisID,
      final OutfileProcessMonitor monitor,
      final ProcessResultDisplay processResultDisplay,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    DetachedProcess detachedProcess = new DetachedProcess(manager,
        detachedCommand, this, axisID, monitor, processResultDisplay,
        processName, processSeries);
    if (monitor != null) {
      monitor.setProcess(detachedProcess);
    }
    return startBackgroundProcess(detachedProcess, detachedCommand
        .getCommandLine(), axisID, monitor);
  }

  final BackgroundProcess startDetachedProcess(
      final DetachedCommand detachedCommand, final AxisID axisID,
      final OutfileProcessMonitor monitor,
      final ProcessResultDisplay processResultDisplay,
      final ProcessName processName, final String subdirName,
      final String shortCommandName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    DetachedProcess detachedProcess = new DetachedProcess(manager,
        detachedCommand, this, axisID, monitor, processResultDisplay,
        processName, processSeries);
    detachedProcess.setSubdirName(subdirName);
    detachedProcess.setShortCommandName(shortCommandName);
    if (monitor != null) {
      monitor.setProcess(detachedProcess);
    }
    return startBackgroundProcess(detachedProcess, detachedCommand
        .getCommandLine(), axisID, monitor);
  }

  final BackgroundProcess startBackgroundProcess(
      final CommandDetails commandDetails, final AxisID axisID,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        commandDetails, this, axisID, processName, processSeries);
    return startBackgroundProcess(backgroundProcess, commandDetails
        .getCommandLine(), axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(
      final CommandDetails commandDetails, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        commandDetails, this, axisID, processResultDisplay, processName,
        processSeries);
    return startBackgroundProcess(backgroundProcess, commandDetails
        .getCommandLine(), axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(final Command command,
      final AxisID axisID, final ProcessName processName,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        command, this, axisID, processName, processSeries);
    return startBackgroundProcess(backgroundProcess, command.getCommandLine(),
        axisID, null);
  }

  final BackgroundProcess startBackgroundProcess(final Command command,
      final AxisID axisID, final boolean forceNextProcess,
      final ProcessName processName, final ConstProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = new BackgroundProcess(manager,
        command, this, axisID, forceNextProcess, processName, processSeries);
    return startBackgroundProcess(backgroundProcess, command.getCommandLine(),
        axisID, null);
  }

  private BackgroundProcess startBackgroundProcess(
      final BackgroundProcess backgroundProcess, final String commandLine,
      final AxisID axisID, final Runnable processMonitor)
      throws SystemProcessException {
    backgroundProcess
        .setWorkingDirectory(new File(manager.getPropertyUserDir()));
    backgroundProcess.setDemoMode(etomoDirector.getArguments().isDemo());
    backgroundProcess.setDebug(etomoDirector.getArguments().isDebug());
    manager.saveStorables(axisID);
    isAxisBusy(axisID, backgroundProcess.getProcessResultDisplay());
    backgroundProcess.start();
    if (etomoDirector.getArguments().isDebug()) {
      System.err.println("Started " + commandLine);
      System.err.println("  Name: " + backgroundProcess.getName());
    }
    mapAxisThread(backgroundProcess, axisID);
    // Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the backgroundProcess
      while (!backgroundProcess.isStarted()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException e) {
          break;
        }
      }
      Thread processMonitorThread = new Thread(new etomo.process.ThreadGroup(
          "startBackgroundProcess"), processMonitor);
      processMonitorThread.start();
      mapAxisProcessMonitor(processMonitorThread, axisID);
    }

    return backgroundProcess;
  }

  final InteractiveSystemProgram startInteractiveSystemProgram(
      final Command command) throws SystemProcessException {
    InteractiveSystemProgram program = new InteractiveSystemProgram(manager,
        command, this, command.getAxisID());
    program.setWorkingDirectory(new File(manager.getPropertyUserDir()));
    Thread thread = new Thread(program);
    manager.saveStorables(command.getAxisID());
    thread.start();
    program.setName(thread.getName());
    if (etomoDirector.getArguments().isDebug()) {
      System.err.println("Started " + program.getCommandLine());
      System.err.println("  Name: " + thread.getName());
    }
    return program;
  }

  public final void tomosnapshot(final AxisID axisID) {
    try {
      TomosnapshotProcess process = new TomosnapshotProcess(axisID);
      new Thread(process).start();
        }
    catch (Exception e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Process Exception",
          axisID, null);
    }
    }

  /**
   * Start an arbitrary command as an unmanaged background thread
   */
  public final void startSystemProgramThread(final String[] command,
      final AxisID axisID) {
    // Initialize the SystemProgram object
    SystemProgram sysProgram = new SystemProgram(manager.getPropertyUserDir(),
        command, axisID, manager.getManagerKey());
    startSystemProgramThread(sysProgram);
  }

  /*
   protected void startSystemProgramThread(String command, AxisID axisID) {
   // Initialize the SystemProgram object
   SystemProgram sysProgram = new SystemProgram(getManager()
   .getPropertyUserDir(), command, axisID);
   startSystemProgramThread(sysProgram);
   }*/

  private void startSystemProgramThread(final SystemProgram sysProgram) {
    sysProgram.setWorkingDirectory(new File(manager.getPropertyUserDir()));
    sysProgram.setDebug(etomoDirector.getArguments().isDebug());

    // Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    manager.saveStorables(sysProgram.getAxisID());
    sysProgThread.start();
    if (etomoDirector.getArguments().isDebug()) {
      System.err.println("Started " + sysProgram.getCommandLine());
      System.err
          .println("  working directory: " + manager.getPropertyUserDir());
    }
  }

  public final void msgProcessDone(final DetachedProcess process,
      final int exitValue, final boolean errorFound) {
    if (exitValue != 0 || errorFound) {
      errorProcess(process);
    }
    else {
      postProcess(process);
    }
    manager.saveStorables(process.getAxisID());

    // Null the reference to the appropriate thread
    if (process == threadAxisA) {
      threadAxisA = null;
    }
    if (process == threadAxisB) {
      threadAxisB = null;
    }
    // Inform the manager that this process is complete
    ProcessEndState endState = process.getProcessEndState();
    if (endState == null || endState == ProcessEndState.DONE) {
      manager.processDone(process.getName(), exitValue, process
          .getProcessName(), process.getAxisID(), process.isForceNextProcess(),
          process.getProcessEndState(), exitValue != 0 || errorFound, process
              .getProcessResultDisplay(), process.getProcessSeries(), false);
    }
    else {
      manager.processDone(process.getName(), exitValue, process
          .getProcessName(), process.getAxisID(), process.isForceNextProcess(),
          process.getProcessEndState(), process.getStatusString(),
          exitValue != 0 || errorFound, process.getProcessResultDisplay(),
          process.getProcessSeries(), false);
    }
  }

  /**
   * A message specifying that a background process has finished execution
   * 
   * @param script
   *          the BackgroundProcess execution object that finished
   * @param exitValue
   *          the exit value for the process
   */
  public final void msgProcessDone(final BackgroundProcess process,
      final int exitValue, final boolean errorFound) {
    if (exitValue != 0 || errorFound) {
      errorProcess(process);
    }
    else {
      postProcess(process);
      ProcessMessages messages = process.getProcessMessages();
      if (messages != null && messages.warningListSize() > 0) {
        uiHarness.openWarningMessageDialog(messages, process.getName()
            + " warnings", process.getAxisID(), manager.getManagerKey());
      }
    }
    manager.saveStorables(process.getAxisID());

    // Null the reference to the appropriate thread
    if (process == threadAxisA) {
      threadAxisA = null;
    }
    if (process == threadAxisB) {
      threadAxisB = null;
    }
    // Inform the manager that this process is complete
    ProcessEndState endState = process.getProcessEndState();
    if (endState == null || endState == ProcessEndState.DONE) {
      manager.processDone(process.getName(), exitValue, process
          .getProcessName(), process.getAxisID(), process.isForceNextProcess(),
          process.getProcessEndState(), exitValue != 0 || errorFound, process
              .getProcessResultDisplay(), process.getProcessSeries(), false);
    }
    else {
      manager.processDone(process.getName(), exitValue, process
          .getProcessName(), process.getAxisID(), process.isForceNextProcess(),
          process.getProcessEndState(), process.getStatusString(),
          exitValue != 0 || errorFound, process.getProcessResultDisplay(),
          process.getProcessSeries(), false);
    }
  }

  public final void msgInteractiveSystemProgramDone(
      final InteractiveSystemProgram program, final int exitValue) {
    postProcess(program);
    manager.saveStorables(program.getAxisID());
  }

  final void writeLogFile(final BackgroundProcess process, final AxisID axisID,
      final String fileName) {
    LogFile logFile = null;
    LogFile.WriterId writerId = null;
    try {
      // Write the standard output to a the log file
      String[] stdOutput = process.getStdOutput();
      try {
        logFile = LogFile.getInstance(manager.getPropertyUserDir(), fileName,
            manager.getManagerKey());
      }
      catch (LogFile.LockException e) {
        uiHarness.openMessageDialog(e.getMessage(), "log File Write Error",
            axisID, manager.getManagerKey());
        return;
      }
      writerId = logFile.openWriter();
      if (stdOutput != null) {
        for (int i = 0; i < stdOutput.length; i++) {
          logFile.write(stdOutput[i], writerId);
          logFile.newLine(writerId);
        }
      }
      logFile.closeWriter(writerId);
    }
    catch (LogFile.LockException except) {
      logFile.closeWriter(writerId);
      uiHarness.openMessageDialog(except.getMessage(), "log File Write Error",
          axisID, manager.getManagerKey());
    }
    catch (IOException except) {
      logFile.closeWriter(writerId);
      uiHarness.openMessageDialog(except.getMessage(), "log File Write Error",
          axisID, manager.getManagerKey());
    }
  }

  void postProcess(final BackgroundProcess process) {
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    if (ProcessName.TOMOSNAPSHOT.equals(commandName)) {
      Utilities.findMessageAndOpenDialog(process.getAxisID(), process
          .getStdOutput(), TomosnapshotParam.OUTPUT_LINE,
          "Tomosnapshot Complete", manager.getManagerKey());
    }
  }

  void postProcess(final DetachedProcess process) {
    try {
      if (process.getCommand().getCommandName().equals(
          ProcessName.PROCESSCHUNKS.toString())) {
        manager.resetCurrentProcesschunks(process.getAxisID());
      }
    }
    catch (NullPointerException e) {
  }
}
}