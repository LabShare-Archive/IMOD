package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.ProcessingMethodMediator;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstStringProperty;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.ui.swing.ParallelProgressDisplay;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

class ProcesschunksProcessMonitor implements OutfileProcessMonitor,
    ParallelProcessMonitor {
  public static final String rcsid = "$Id$";

  private static final int NO_TCSH_ERROR = 5;

  private static final String TITLE = "Processchunks";
  static final String SUCCESS_TAG = "Finished reassembling";
  private static boolean debug = false;
  private final EtomoNumber nChunks = new EtomoNumber();
  private final EtomoNumber chunksFinished = new EtomoNumber();
  private final String rootName;
  private final ProcessMessages messages;
  private final ProcessingMethodMediator mediator;
  private String subdirName = null;
  private boolean setProgressBarTitle = false;// turn on to changed the progress bar title
  private boolean reassembling = false;
  private ProcessEndState endState = null;
  private LogFile commandsPipe = null;
  private LogFile.WriterId commandsPipeWriterId = null;
  // private BufferedWriter commandsWriter = null;
  private boolean useCommandsPipe = true;
  // private BufferedReader bufferedReader = null;
  private LogFile processOutput = null;
  private LogFile.ReaderId processOutputReaderId = null;
  private boolean processRunning = true;
  private boolean pausing = false;
  private boolean killing = false;
  // private File processOutputFile = null;
  private String pid = null;
  private boolean starting = true;
  private boolean finishing = false;
  private boolean stop = false;
  private boolean running = false;
  private boolean reconnect = false;
  private SystemProcessInterface process = null;
  final BaseManager manager;
  final AxisID axisID;
  final Map<String, String> computerMap;
  private final boolean multiLineMessages;
  private int tcshErrorCountDown = NO_TCSH_ERROR;
  private ParallelProgressDisplay parallelProgressDisplay = null;
  private MessageReporter messageReporter = null;
  private boolean willResume = false;

  public void dumpState() {
    System.err.print("[rootName:" + rootName + ",subdirName:" + subdirName
        + ",\nsetProgressBarTitle:" + setProgressBarTitle + ",useCommandsPipe:"
        + useCommandsPipe + ",\nprocessRunning:" + processRunning + ",pausing:" + pausing
        + ",\nkilling:" + killing + ",pid:" + pid + ",starting:" + starting
        + ",\nfinishing:" + finishing + ",stop:" + stop + ",running:" + running
        + ",\nreconnect:" + reconnect + ",multiLineMessages:" + multiLineMessages
        + ",\ntcshErrorCountDown:" + tcshErrorCountDown + "]");
  }

  ProcesschunksProcessMonitor(final BaseManager manager, final AxisID axisID,
      final String rootName, final Map<String, String> computerMap,
      final boolean multiLineMessages) {
    this.manager = manager;
    this.axisID = axisID;
    this.rootName = rootName;
    this.computerMap = computerMap;
    this.multiLineMessages = multiLineMessages;
    messages = ProcessMessages.getInstanceForParallelProcessing(manager,
        multiLineMessages);
    mediator = manager.getProcessingMethodMediator(axisID);
    debug = EtomoDirector.INSTANCE.getArguments().isDebug();
  }

  public static ProcesschunksProcessMonitor getReconnectInstance(
      final BaseManager manager, final AxisID axisID, final ProcessData processData,
      final boolean multiLineMessages) {
    ProcesschunksProcessMonitor instance = new ProcesschunksProcessMonitor(manager,
        axisID, processData.getSubProcessName(), processData.getComputerMap(),
        multiLineMessages);
    instance.reconnect = true;
    return instance;
  }

  /**
   * Sets the process.  Then, because this is a parallel process monitor, it
   * sets computerMap in the process if this isn't a reconnect.  This causes the
   * process to set the computerMap in ProcessData.
   */
  public final void setProcess(SystemProcessInterface process) {
    this.process = process;
    if (!reconnect && process != null) {
      process.setComputerMap(computerMap);
    }
  }

  public final boolean isRunning() {
    return running;
  }

  public final void stop() {
    stop = true;
  }

  public final void run() {
    running = true;
    mediator.register(this);
    if (parallelProgressDisplay == null) {
      loadParallelProgressDisplay();
    }
    if (reconnect && parallelProgressDisplay != null) {
      parallelProgressDisplay.setComputerMap(computerMap);
    }
    if (parallelProgressDisplay != null) {
      parallelProgressDisplay.msgStartingProcessOnSelectedComputers();
    }
    nChunks.set(0);
    chunksFinished.set(0);
    try {
      /* Wait for processchunks or prochunks to delete .cmds file before enabling the Kill
       * Process button and Pause button. The main loop uses a sleep of 2000 millisecs.
       * This change pushes the first sleep back before the command buttons are turned on.
       * The monitor starts running before processchunks starts, so its easy to send a
       * command to a file which is not being watched and will be deleted by
       * processchunks. Not allowing commands to be sent for the period of the first sleep
       * also reduces the chance of a collision on Windows - where processchunks cannot
       * delete the command pipe file (.cmds file) because it is in use. */
      Thread.sleep(2000);
    }
    catch (InterruptedException e) {
    }
    // Get ready to respond to the Kill Process button and Pause button.
    useCommandsPipe = true;
    // Turn on the Kill Process button and Pause button.
    initializeProgressBar();
    try {
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
      while (processRunning && !stop) {
        try {
          if (updateState() || setProgressBarTitle) {
            updateProgressBar();
          }
          Thread.sleep(2000);
        }
        catch (LogFile.LockException e) {
          // File creation may be slow, so give this more tries.
          e.printStackTrace();
        }
        catch (FileNotFoundException e) {
          // File creation may be slow, so give this more tries.
          e.printStackTrace();
        }
      }
      closeProcessOutput();
    }
    catch (InterruptedException e) {
      endMonitor(ProcessEndState.DONE);
    }
    catch (IOException e) {
      endMonitor(ProcessEndState.FAILED);
    }
    // Disable the use of the commands pipe.
    useCommandsPipe = false;
    if (parallelProgressDisplay == null) {
      loadParallelProgressDisplay();
    }
    if (parallelProgressDisplay != null) {
      parallelProgressDisplay.msgEndingProcess();
    }
    running = false;
    mediator.deregister(this);
  }

  public final void msgLogFileRenamed() {
  }

  public final void endMonitor(ProcessEndState endState) {
    setProcessEndState(endState);
    processRunning = false;// the only place that this should be changed
    setProgressBarTitle();
  }

  public final String getPid() {
    return pid;
  }

  public final String getLogFileName() {
    try {
      return getProcessOutputFileName();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return "";
    }
  }

  public final String getProcessOutputFileName() throws LogFile.LockException {
    createProcessOutput();
    return processOutput.getName();
  }

  /**
   * set end state
   * @param endState
   */
  public synchronized final void setProcessEndState(ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  public final ProcessEndState getProcessEndState() {
    return endState;
  }

  public final ProcessMessages getProcessMessages() {
    return messages;
  }

  public final String getSubProcessName() {
    return rootName;
  }

  public final void kill(SystemProcessInterface process, AxisID axisID) {
    try {
      writeCommand("Q");
      if (parallelProgressDisplay == null) {
        loadParallelProgressDisplay();
      }
      if (parallelProgressDisplay != null) {
        parallelProgressDisplay.msgKillingProcess();
      }
      killing = true;
      setProgressBarTitle = true;
      if (starting) {
        // wait to see if processchunks is already starting chunks.
        try {
          Thread.sleep(2001);
        }
        catch (InterruptedException e) {
        }
        if (starting) {
          // processchunks hasn't started chunks and it won't because the "Q" has
          // been sent. So it is safe to kill it in the usual way.
          if (process != null) {
            process.signalKill(axisID);
          }
        }
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  public final void pause(SystemProcessInterface process, AxisID axisID) {
    try {
      writeCommand("P");
      if (parallelProgressDisplay == null) {
        loadParallelProgressDisplay();
      }
      if (parallelProgressDisplay != null) {
        parallelProgressDisplay.msgPausingProcess();
      }
      pausing = true;
      setProgressBarTitle = true;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  public boolean isPausing() {
    return pausing && processRunning;
  }

  public void setWillResume() {
    willResume = true;
    setProgressBarTitle();
  }

  public final String getStatusString() {
    return chunksFinished + " of " + nChunks + " completed";
  }

  public final void drop(String computer) {
    if (computerMap == null || computerMap.containsKey(computer)) {
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("try to drop " + computer);
      }
      try {
        writeCommand("D " + computer);
        setProgressBarTitle = true;
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public final boolean isProcessRunning() {
    if (!processRunning) {
      return false;
    }
    String[] array;
    if (process != null) {
      if (process.isDone()) {
        processRunning = false;
      }
      else {
        boolean debug = EtomoDirector.INSTANCE.getArguments().isDebug();
        array = process.getStdError();
        if (array != null) {
          for (int i = 0; i < array.length; i++) {
            if (debug) {
              System.err.println(array[i]);
            }
            if (array[i].startsWith("ERROR:") || array[i].startsWith("Traceback")
                || array[i].indexOf("Errno") != -1) {
              endMonitor(ProcessEndState.FAILED);
              processRunning = false;
            }
          }
        }
        array = process.getStdOutput();
        if (array != null) {
          for (int i = 0; i < array.length; i++) {
            if (debug) {
              System.err.println(array[i]);
            }
            if (array[i].startsWith("ERROR:") || array[i].startsWith("Traceback")
                || array[i].indexOf("Errno") != -1) {
              endMonitor(ProcessEndState.FAILED);
              processRunning = false;
            }
          }
        }
      }
    }
    return processRunning;
  }

  final void setSubdirName(String input) {
    subdirName = input;
  }

  final void setSubdirName(ConstStringProperty input) {
    if (input == null || input.isEmpty()) {
      subdirName = null;
    }
    else {
      subdirName = input.toString();
    }
  }

  final AxisID getAxisID() {
    return axisID;
  }

  final boolean isStarting() {
    return starting;
  }

  final boolean isFinishing() {
    return finishing;
  }

  synchronized void closeProcessOutput() {
    if (messageReporter != null) {
      messageReporter.close();
    }
    if (processOutput != null && processOutputReaderId != null
        && !processOutputReaderId.isEmpty()) {
      processOutput.closeRead(processOutputReaderId);
      processOutput = null;
    }
  }

  void loadParallelProgressDisplay() {
    parallelProgressDisplay = manager.getProcessingMethodMediator(axisID)
        .getParallelProgressDisplay();
  }

  boolean updateState() throws LogFile.LockException, FileNotFoundException, IOException {
    createProcessOutput();
    boolean returnValue = false;
    boolean failed = false;
    if (isProcessRunning()
        && (processOutputReaderId == null || processOutputReaderId.isEmpty())) {
      processOutputReaderId = processOutput.openReader();
    }

    if (processOutputReaderId == null || processOutputReaderId.isEmpty()) {
      return returnValue;
    }
    String line;
    while ((line = processOutput.readLine(processOutputReaderId)) != null) {
      line = line.trim();
      // get the first pid
      if (pid == null && line.startsWith("Shell PID:")) {
        String[] array = line.split("\\s+");
        if (array.length == 3) {
          pid = array[2].trim();
        }
      }
      if (debug) {
        System.err.println(line);
      }
      if (line.indexOf("imodkillgroup") == -1) {
        messages.addProcessOutput(line);
      }
      if (messages.isError()) {
        // Set failure boolean but continue to add all the output lines to
        // messages.
        failed = true;
      }
      // If it got an error message, then it seems like the best thing to do is
      // stop processing.
      if (failed) {
        continue;
      }
      if (line.endsWith("to reassemble")) {
        // handle all chunks finished, starting the reassemble
        Utilities.timestamp(TITLE, rootName, "reassembling");
        // all chunks finished, turn off pause
        if (endState == ProcessEndState.PAUSED) {
          endState = null;
        }
        reassembling = true;
        setProgressBarTitle = true;
        returnValue = true;
      }
      else if (line.indexOf("BAD COMMAND IGNORED") != -1) {
        throw new IllegalStateException("Bad command sent to processchunks\n" + line);
      }
      else if (line.equals(SUCCESS_TAG)) {
        endMonitor(ProcessEndState.DONE);
      }
      else if (line
          .equals("When you rerun with a different set of machines, be sure to use")) {
        endMonitor(ProcessEndState.KILLED);
      }
      else if (line
          .equals("All previously running chunks are done - exiting as requested")) {
        endMonitor(ProcessEndState.PAUSED);
      }
      // A tcsh error can cause processchunks to exit immediately without killing
      // chunks.
      // "No match" has been removed. I may not be producing fatal errors.
      else if ((line.indexOf("Syntax Error") != -1
          || line.indexOf("Subscript error") != -1
          || line.indexOf("Undefined variable") != -1
          || line.indexOf("Expression Syntax") != -1
          || line.indexOf("Subscript out of range") != -1
          || line.indexOf("Illegal variable name") != -1
          || line.indexOf("Variable syntax") != -1
          || line.indexOf("Badly placed (") != -1 || line.indexOf("Badly formed number") != -1)
          && (process == null || process.getProcessData() == null || !process
              .getProcessData().isRunning())) {
        // If a tcsh error is found, start a countdown that progresses each time
        // updateState is run. Sometimes a tcsh error will result in an error
        // being generated and processchunks terminating normally, so give this
        // time to happen rather then popping up the scary error message as soon
        // as the tcsh error is found.
        tcshErrorCountDown--;
      }
      else {
        String[] strings = line.split("\\s+");
        // set nChunks and chunksFinished
        if (parallelProgressDisplay == null) {
          loadParallelProgressDisplay();
        }
        if (strings.length > 2 && line.endsWith("DONE SO FAR")) {
          starting = false;
          if (!nChunks.equals(strings[2])) {
            nChunks.set(strings[2]);
            setProgressBarTitle = true;
          }
          chunksFinished.set(strings[0]);
          if (chunksFinished.equals(nChunks)) {
            finishing = true;
          }
          returnValue = true;
        }
        else if (strings.length > 1) {
          if (line.startsWith("Dropping")) {
            String failureReason;
            if (line.indexOf("it cannot cd to") != -1) {
              failureReason = "cd failed";
            }
            else if (line.indexOf("cannot connect") != -1) {
              failureReason = "connect failed";
            }
            else if (line.indexOf("it cannot run IMOD commands") != -1) {
              failureReason = "run error";
            }
            else if (line.indexOf("it failed (with time out)") != -1) {
              failureReason = "chunk timed out";
            }
            else if (line.indexOf("it failed (with chunk error)") != -1) {
              failureReason = "chunk error";
            }
            else {
              failureReason = "unknown error";
            }
            // handle a dropped CPU
            parallelProgressDisplay.msgDropped(strings[1], failureReason);
          }
          // handle commandsPipeWriteIda finished chunk
          else if (strings[1].equals("finished")) {
            if (parallelProgressDisplay != null) {
              parallelProgressDisplay.addSuccess(strings[3]);
            }
          }
          // handle a failed chunk
          else if (strings[1].equals("failed")) {
            parallelProgressDisplay.addRestart(strings[3]);
          }
        }
      }
    }
    if (failed) {
      endMonitor(ProcessEndState.FAILED);
      returnValue = false;
    }
    else if (tcshErrorCountDown < 0) {
      // The tcsh error did not cause processchunks to terminate normally
      // before the countdown ended, so assume that it died without killing
      // chunks. Currently not ending the monitor because we don't want the
      // user to rerun processchunks before all the chunk processes end.
      System.err.println("ERROR: Tcsh error in processchunks log");
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unrecoverable error in processchunks.  Please contact the "
              + "programmer.  The chunk processes are still running, but "
              + "they won't show up in the progress bar or appear in the "
              + "Finished Chunks column.  IMPORTANT:  Let the chunk "
              + "processes complete before rerunning the parallel process.  "
              + "The load may decline when your chunks are done.  Also you "
              + "can ssh to each computer and run top.  After the chunk "
              + "processes are complete, exit Etomo to clear the parallel "
              + "processing panel.  To attempt to continue the parallel "
              + "process, rerun Etomo, and press Resume.", "Fatal Error");
      tcshErrorCountDown = NO_TCSH_ERROR;
    }
    else if (tcshErrorCountDown < NO_TCSH_ERROR) {
      // Continue the countdown.
      tcshErrorCountDown--;
    }
    return returnValue;
  }

  void updateProgressBar() {
    if (setProgressBarTitle) {
      setProgressBarTitle = false;
      setProgressBarTitle();
    }
    manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(),
        getStatusString(), axisID);
    if (messageReporter != null) {
      messageReporter.checkForMessages(manager);
    }
  }

  private void initializeProgressBar() {
    setProgressBarTitle();
    if (reconnect) {
      manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(),
          "Reconnecting...", axisID);
    }
    else {
      manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(), "Starting...",
          axisID);
    }
  }

  public void useMessageReporter() {
  }

  private final void setProgressBarTitle() {
    StringBuffer title = new StringBuffer(TITLE);
    if (rootName != null) {
      title.append(" " + rootName);
    }
    if (processRunning) {
      if (reassembling) {
        title.append(":  reassembling");
      }
      else if (killing) {
        title.append(" - killing:  exiting current chunks");
      }
      else if (pausing) {
        title.append(" - pausing:  finishing current chunks");
        if (willResume) {
          title.append(" - will resume");
        }
      }
    }
    else if (killing) {
      title.append(" - killed");
    }
    else if (pausing) {
      title.append(" - paused");
      if (willResume) {
        title.append(" - will resume");
      }
    }
    manager.getMainPanel().setProgressBar(title.toString(), nChunks.getInt(), axisID,
        !reassembling && !killing);
  }

  /**
   * create commandsWriter if necessary, write command, add newline, flush
   * @param command
   * @throws IOException
   */
  private final void writeCommand(String command) throws LogFile.LockException,
      IOException {
    if (!useCommandsPipe) {
      return;
    }
    if (commandsPipe == null) {
      commandsPipe = LogFile.getInstance(manager.getPropertyUserDir(),
          DatasetFiles.getCommandsFileName(subdirName, rootName));
    }
    if (commandsPipeWriterId == null || commandsPipeWriterId.isEmpty()) {
      commandsPipeWriterId = commandsPipe.openWriter(true);
    }
    if (commandsPipeWriterId == null || commandsPipeWriterId.isEmpty()) {
      return;
    }
    commandsPipe.write(command, commandsPipeWriterId);
    commandsPipe.newLine(commandsPipeWriterId);
    commandsPipe.flush(commandsPipeWriterId);
    // Close writer after each write. If it is kept open, the file would not be writeable
    // from the command line in Windows.
    commandsPipe.closeWriter(commandsPipeWriterId);
    commandsPipeWriterId = null;
  }

  /**
   * make sure process output file is new and set processOutputFile.  This
   * function should be first run before the process starts.
   */
  private final synchronized void createProcessOutput() throws LogFile.LockException {
    if (processOutput == null) {
      processOutput = LogFile.getInstance(
          manager.getPropertyUserDir(),
          DatasetFiles.getOutFileName(manager, subdirName,
              ProcessName.PROCESSCHUNKS.toString(), axisID));
      messageReporter = new MessageReporter(axisID, processOutput);
      // Don't remove the file if this is a reconnect.
      if (!reconnect) {
        // Avoid looking at a file from a previous run.
        processOutput.backup();
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.57  2011/07/22 23:37:08  sueh
 * <p> Bug# 1521 Added a MessageReporter member variable.
 * <p>
 * <p> Revision 1.56  2011/02/23 05:09:00  sueh
 * <p> bug# 1450 Setting parallelProgressDisplay right before it is used.  The
 * <p> mediator doesn't have it immediately.
 * <p>
 * <p> Revision 1.55  2011/02/22 04:07:41  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.54  2011/02/03 06:02:00  sueh
 * <p> bug# 1422 Registering class with process method mediator.
 * <p>
 * <p> Revision 1.53  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.52  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.51  2010/02/18 01:15:08  sueh
 * <p> bub# 1283 Removed "no match" from tcsh error string list.  Performing a
 * <p> countdown to allow processchunks to fail normally before popping up the
 * <p> crash message.
 * <p>
 * <p> Revision 1.50  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.49  2010/01/11 23:56:32  sueh
 * <p> bug# 1299 Added useMessageReporter.
 * <p>
 * <p> Revision 1.48  2009/04/20 20:01:13  sueh
 * <p> bug# 1192  Constructing with Map computerMap instead of String
 * <p> computerList.  When doing a reconnect, send the computerMap to the
 * <p> parallelProcessDisplay (computer table) when run() is run.  When not doing
 * <p> a reconnect, send the computerMap to the process.
 * <p>
 * <p> Revision 1.47  2009/04/15 19:58:30  sueh
 * <p> bug# 1205 Calling setProgressBarTitle from endMonitor.
 * <p>
 * <p> Revision 1.46  2009/03/17 00:43:03  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.45  2009/03/03 20:40:19  sueh
 * <p> bug# 1193 In updateState fixed comparison which checked
 * <p> ProcessData.isRunning to decide whether to pop up error message.
 * <p>
 * <p> Revision 1.44  2009/03/01 00:53:55  sueh
 * <p> bug# 1193 In startDetachedProcess setting the process in the monitor.
 * <p>
 * <p> Revision 1.43  2009/02/27 03:49:38  sueh
 * <p> bug# 1189 In updateState changed tcsh failure error message.
 * <p>
 * <p> Revision 1.42  2009/02/24 23:51:23  sueh
 * <p> bug# 1183 In updateState catching another tcsh error.
 * <p>
 * <p> Revision 1.41  2009/02/04 23:26:53  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.40  2009/01/28 00:24:49  sueh
 * <p> bug# 1174 In updateState warn the user if a tcsh error happens.
 * <p>
 * <p> Revision 1.39  2008/05/16 22:44:16  sueh
 * <p> bug# 1109 Added getSubProcess so that the process that processchunks
 * <p> is running can be saved in ProcessData.
 * <p>
 * <p> Revision 1.38  2008/05/05 19:57:58  sueh
 * <p> bug# 1108 In run, when catching a read exception do not leave the while
 * <p> loop.
 * <p>
 * <p> Revision 1.37  2008/01/31 20:19:06  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.36  2008/01/23 21:10:50  sueh
 * <p> Removed print statements.
 * <p>
 * <p> Revision 1.35  2008/01/14 21:31:47  sueh
 * <p> bug# 1050 Added stop() and isRunning() to allow ProcessMonitor classes to work
 * <p> with ReconnectProcess.  Added boolean reconnect, which prevents the process
 * <p> output file from being deleted.
 * <p>
 * <p> Revision 1.34  2007/12/26 22:14:38  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.33  2007/12/10 22:29:11  sueh
 * <p> bug# 1041 Removed subdirName from the constructor because it is optional.
 * <p>
 * <p> Revision 1.32  2007/11/06 19:25:21  sueh
 * <p> bug# 1-47 Allowed processchunks to be executed in a subdirectory.
 * <p>
 * <p> Revision 1.31  2007/09/27 20:27:11  sueh
 * <p> bug# 1044 Giving a message that the process is ending at the end of the run()
 * <p> function so that the "Use a cluster" checkbox can be turned back on.  No longer
 * <p> setting the parallel process monitor in the parallel progress display since it is
 * <p> never used.
 * <p>
 * <p> Revision 1.30  2007/09/07 00:19:14  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.29  2007/08/29 21:43:14  sueh
 * <p> bug# 1041 Set BaseState.KilledProcesschunksProcessName when a kill or
 * <p> pause is done.
 * <p>
 * <p> Revision 1.28  2007/02/22 20:36:51  sueh
 * <p> bug# 964 Printing processchunks output to the etomo_err.log for now.
 * <p>
 * <p> Revision 1.27  2006/12/02 04:38:06  sueh
 * <p> bug# 944 Made this class a parent of ProcesschunksVolcombineMonitor.
 * <p>
 * <p> Revision 1.26  2006/12/01 00:58:06  sueh
 * <p> bug# 937 Don't have to set process in monitor because its being passed to the
 * <p> kill function.  Made endMonitor public so that the process could stop the monitor.
 * <p>
 * <p> Revision 1.25  2006/11/30 20:10:28  sueh
 * <p> bug# 937 In kill(), kill processchunk chunks with signalKill, if it hasn't started yet.
 * <p>
 * <p> Revision 1.24  2006/11/08 00:24:05  sueh
 * <p> bug# 935  createProcessOutput():  backup the old process output file instead of
 * <p> deleting it.
 * <p>
 * <p> Revision 1.23  2006/10/24 21:38:10  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.22  2006/10/18 15:42:06  sueh
 * <p> bug# 929  updateState():  Improving failure reasons.
 * <p>
 * <p> Revision 1.21  2006/10/11 10:10:14  sueh
 * <p> bug# 931 Managing the commands pipe and the process output with LogFile so
 * <p> that the file access problem which appears in Windows will show up in Linux.
 * <p>
 * <p> Revision 1.20  2006/10/10 12:06:40  sueh
 * <p> bug# 931 deleteCommandPipe():  throwing an exception if can't delete
 * <p> the .cmds file.
 * <p>
 * <p> Revision 1.19  2006/09/25 16:36:18  sueh
 * <p> bug# 931 Added empty function msgLogFileRenamed().
 * <p>
 * <p> Revision 1.18  2006/06/05 16:30:51  sueh
 * <p> bug# 766 Implementing OutfileProcessMonitor - adding getPid.  Getting the pid
 * <p> from the output file.
 * <p>
 * <p> Revision 1.17  2006/01/31 20:45:04  sueh
 * <p> bug# 521 Added the process to combine monitor.  This allows the last
 * <p> ProcessResultDisplay used by the monitor to be assigned to the process.
 * <p>
 * <p> Revision 1.16  2006/01/06 23:17:08  sueh
 * <p> bug# 795 Fixed a bug in updateState where it ends the monitor as soon as
 * <p> it sees an error message.  This means that the error messages popped
 * <p> up to the user where incomplete.  When an error is found, collect all the
 * <p> messages and then end the monitor.
 * <p>
 * <p> Revision 1.15  2005/11/29 22:24:10  sueh
 * <p> bug# 744 Stop putting processchunks output into the error log, since its
 * <p> going into the .out file.
 * <p>
 * <p> Revision 1.14  2005/11/19 02:36:13  sueh
 * <p> bug# 744 Converted to a detached program.  Managing the process
 * <p> output file.  No longer using the process.  Reading the process output file
 * <p> to update the program display.  Handling kills and pauses in
 * <p> updateState().  Handling errors and chunk errors with ProcessMessages.
 * <p>
 * <p> Revision 1.13  2005/11/04 00:53:00  sueh
 * <p> fixed pausing message
 * <p>
 * <p> Revision 1.12  2005/10/21 19:55:54  sueh
 * <p> bug# 742 Sending text from standard error to the error log.
 * <p>
 * <p> Revision 1.11  2005/09/27 21:16:18  sueh
 * <p> bug# 532 Temporarily printing the processchunks output to the error log
 * <p> without the debug setting.
 * <p>
 * <p> Revision 1.10  2005/09/10 01:52:41  sueh
 * <p> bug# 532 Setting the reason for all dropped computers to "not responding"
 * <p> to distinguish it from load average timeouts.
 * <p>
 * <p> Revision 1.9  2005/09/09 21:42:22  sueh
 * <p> bug# 532 Throwing an exception if a command is sent that processchunks
 * <p> doesn't recognize.
 * <p>
 * <p> Revision 1.8  2005/09/07 20:51:06  sueh
 * <p> bug# 532 Added commandsPipe and commandsWriter to send commands
 * <p> for processchunks to a file.
 * <p>
 * <p> Revision 1.7  2005/09/01 17:54:58  sueh
 * <p> bug# 532 handle multiple drop reasons.  Fix drop function:  use interrupt
 * <p> signal only when the computer is recognized.
 * <p>
 * <p> Revision 1.6  2005/08/30 22:41:45  sueh
 * <p> bug# 532 Added error log print statement to drop().
 * <p>
 * <p> Revision 1.5  2005/08/30 18:50:23  sueh
 * <p> bug# 532 Added drop(String computer).  Added dropComputer which is set
 * <p> while waiting for the interrupt to happen.  Added computerList so that
 * <p> the monitor can avoid interrupting for computers that processchunks isn't
 * <p> using.  Added code to updateState() to send "D computer_name" to
 * <p> processchunks.
 * <p>
 * <p> Revision 1.4  2005/08/27 22:31:47  sueh
 * <p> bug# 532 In updateState() look for CHUNK ERROR: and save the most
 * <p> recent one found.  Return the last chunk error message with
 * <p> getErrorMessage().
 * <p>
 * <p> Revision 1.3  2005/08/22 17:03:43  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.  Added "pausing" string to progress title as soon as a
 * <p> pause is detected since pausing takes a lot of time.
 * <p>
 * <p> Revision 1.2  2005/08/15 18:23:54  sueh
 * <p> bug# 532  Added kill and pause functions to implement ProcessMonitor.
 * <p> Both kill and pause signal interrupt.  Change updateState to handle the
 * <p> interrupt message and send the correct string, based on whether a kill or
 * <p> a pause was requested.
 * <p>
 * <p> Revision 1.1  2005/08/04 19:46:15  sueh
 * <p> bug# 532 Class to monitor processchunks.  Monitors the standard output.
 * <p> Sends updates to the progress panel and to a parallel process display.
 * <p> </p>
 */
