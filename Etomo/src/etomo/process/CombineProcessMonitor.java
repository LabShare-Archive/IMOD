package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.CombineComscriptState;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.CombineProcessType;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.swing.ProcessResultDisplayFactory;
import etomo.ui.swing.UIHarness;
import etomo.util.Utilities;

/**
 * <p>
 * Description: Provides a threadable class to execute IMOD com scripts in the
 * background.  An instance of this class can be run only once.
 * </p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.33  2010/12/05 04:42:56  sueh
 * <p> $bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.
 * <p> $
 * <p> $Revision 1.32  2010/11/13 16:03:45  sueh
 * <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p> $
 * <p> $Revision 1.31  2010/02/17 04:49:20  sueh
 * <p> $bug# 1301 Using the manager instead of the manager key do pop up
 * <p> $messages.
 * <p> $
 * <p> $Revision 1.30  2010/01/11 23:51:04  sueh
 * <p> $bug# 1299 Added useMessageReporter.
 * <p> $
 * <p> $Revision 1.29  2009/03/17 00:34:48  sueh
 * <p> $bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p> $
 * <p> $Revision 1.28  2009/02/04 23:24:07  sueh
 * <p> $bug# 1158 Changed id and exceptions classes in LogFile.
 * <p> $
 * <p> $Revision 1.27  2008/01/31 20:17:53  sueh
 * <p> $bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p> $
 * <p> $Revision 1.26  2008/01/14 20:34:54  sueh
 * <p> $bug# 1050 Added stop() and isRunning() to allow ProcessMonitor classes to work
 * <p> $with ReconnectProcess.
 * <p> $
 * <p> $Revision 1.25  2007/12/26 22:12:59  sueh
 * <p> $bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p> $
 * <p> $Revision 1.24  2007/12/10 22:06:29  sueh
 * <p> $bug# 1041 working with the changes in ProcessName.
 * <p> $
 * <p> $Revision 1.23  2007/09/07 00:18:43  sueh
 * <p> $bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> $instead of getInstance and createInstance.
 * <p> $
 * <p> $Revision 1.22  2006/10/24 21:18:13  sueh
 * <p> $bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p> $
 * <p> $Revision 1.21  2006/10/11 10:07:34  sueh
 * <p> $bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> $FileException.
 * <p> $
 * <p> $Revision 1.20  2006/10/10 05:07:55  sueh
 * <p> $bug# 931 Managing the log file with LogFile.
 * <p> $
 * <p> $Revision 1.19  2006/09/25 16:33:32  sueh
 * <p> $bug# 931 Added msgLogFileRenamed().
 * <p> $
 * <p> $Revision 1.18  2006/01/31 20:42:41  sueh
 * <p> $bug# 521 Creating instance with the ProcessResultDisplay which started
 * <p> $the combine.  When changing monitors, call
 * <p> $setNextProcessResultDisplay() to send a success message to the
 * <p> $previous display and start the new display.
 * <p> $
 * <p> $Revision 1.17  2006/01/26 21:53:42  sueh
 * <p> $bug# 401 Using CombinePRocessType for combine indexes
 * <p> $
 * <p> $Revision 1.16  2005/11/19 02:19:29  sueh
 * <p> $bug# 744 Consolidated kill() and kill(SystemProcessInterface, axisID) into
 * <p> $kill(SystemProcessInterface, axisID), which is the standard kill.  Added
 * <p> $add call to setProcessEndState to endMonitor to make sure that the
 * <p> $process end state is always set before processRunning is set to false.
 * <p> $
 * <p> $Revision 1.15  2005/08/30 18:38:04  sueh
 * <p> $bug# 532 Changed BackgroundProcessMonitor to
 * <p> $BackgroundComScriptMonitor.  Using BackgroundProcessMonitor for
 * <p> $BackgroundProcess's that need monitors.
 * <p> $
 * <p> $Revision 1.14  2005/08/27 22:25:19  sueh
 * <p> $bug# 532 Add empty getErrorMessage() to implement ProcessMonitor.
 * <p> $This is used by ProcesschunksProcessMonitor.
 * <p> $
 * <p> $Revision 1.13  2005/08/22 16:20:10  sueh
 * <p> $bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> $status string is used to add more information to the progress bar when
 * <p> $the process ends.  It is currently being used only for pausing
 * <p> $processchunks.
 * <p> $
 * <p> $Revision 1.12  2005/08/15 18:09:57  sueh
 * <p> $bug# 532 Added kill and paused functions to implement ProcessMonitor.
 * <p> $Pause is invalid for this monitor.
 * <p> $
 * <p> $Revision 1.11  2005/08/04 19:43:51  sueh
 * <p> $bug# 532 Added empty setProcess() to implement ProcessMonitor.
 * <p> $
 * <p> $Revision 1.10  2005/07/26 18:09:18  sueh
 * <p> $bug# 701 Implementing ProcessMonitor, which extends Runnable.
 * <p> $Added a ProcessEndState member variable.  Set it to DONE or FAILED
 * <p> $when detect the end of the process.
 * <p> $
 * <p> $Revision 1.9  2005/04/25 20:44:53  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 1.8  2005/01/26 23:41:20  sueh
 * <p> $bug# 83 Added matchvol1 and matchorwarp process monitors.
 * <p> $
 * <p> $Revision 1.7  2004/11/19 23:19:07  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
 * <p> $Revision 1.6.2.3  2004/10/11 02:02:48  sueh
 * <p> $bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> $property.  This property would need a different value for each manager.
 * <p> $This variable can be retrieved from the manager if the object knows its
 * <p> $manager.  Otherwise it can retrieve it from the current manager using the
 * <p> $EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> $gets the value from the "user.dir" property.
 * <p> $
 * <p> $Revision 1.6.2.2  2004/10/08 15:56:41  sueh
 * <p> $bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> $member variables non-static.
 * <p> $
 * <p> $Revision 1.6.2.1  2004/09/03 21:08:47  sueh
 * <p> $bug# 520 calling isSelfTest from EtomoDirector
 * <p> $
 * <p> $Revision 1.6  2004/08/28 00:47:16  sueh
 * <p> $bug# 508 setting "processRunning = true" only in the constructor.
 * <p> $A kill monitor call from an error that happens very early
 * <p> $(like combine.com is already running) was being ignored when
 * <p> $processRunning was turned back on.
 * <p> $
 * <p> $Revision 1.5  2004/08/24 20:31:18  sueh
 * <p> $bug# 508 make kill() interrupt the thread which is executing
 * <p> $the run function
 * <p> $
 * <p> $Revision 1.4  2004/08/23 23:35:45  sueh
 * <p> $bug# 508 made this class more like LogFileProcessMonitor.
 * <p> $Calling interrupt on child monitors and this monitor to make
 * <p> $run() complete faster
 * <p> $
 * <p> $Revision 1.3  2004/08/20 21:41:53  sueh
 * <p> $bug# 508 CombineComscriptState match string is now static.
 * <p> $Improved selfTest()
 * <p> $
 * <p> $Revision 1.2  2004/08/19 20:09:01  sueh
 * <p> $bug# 508 Made finding the .com file names more robust.  After 
 * <p> $finding the string "running" or "Running", find a string that 
 * <p> $matched a regular expression generated by 
 * <p> $CombineComscriptState.
 * <p> $Changed:
 * <p> $getCurrentSection()
 * <p> $setCurrentChildCommand(String comscriptName)
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:59:11  sueh
 * <p> $bug# 508 Watches combine.com.  Runs monitors for child .com
 * <p> $processes that have monitors.  For other child .com processes, starts
 * <p> $a progress bar and displays the process name.  Uses the combine.log
 * <p> $file to figure out when child process is running.  Uses
 * <p> $CombineComscriptState to figure out which child .com processes are
 * <p> $valid.  Also uses CombineComscriptState to know which dialog pane
 * <p> $to tell ApplicationManager to set.  Does not inherit
 * <p> $LogFileProcessMonitor.  Figures out when the process ends by
 * <p> $watching the combine.log or by setKill() being called by another object.
 * <p> $Provides information to other objects about the status of the combine
 * <p> $process.
 * <p> $$ </p>
 */
public class CombineProcessMonitor implements DetachedProcessMonitor {
  public static final String rcsid = "$$Id$$";
  public static final String COMBINE_LABEL = "Combine";
  private static final long SLEEP = 100;

  private final ApplicationManager manager;
  private AxisID axisID = null;
  // private BufferedReader logFileReader = null;
  private LogFile.ReaderId logFileReaderId = null;
  private int sleepCount = 0;
  private ProcessEndState endState = null;

  // if processRunning is false at any time before the process ends, it can
  // cause wait loops to end prematurely. This is because the wait loop can
  // start very repidly for a background process.
  // See BackgroundSystemProgram.waitForProcess().
  private boolean processRunning = true;

  private LogFile logFile = null;
  private LogFileProcessMonitor childMonitor = null;
  Thread childThread = null;
  private CombineComscriptState combineComscriptState = null;
  private ProcessName currentCommand = null;

  private static final int CONSTRUCTED_STATE = 1;
  private static final int WAITED_FOR_LOG_STATE = 2;
  private static final int RAN_STATE = 3;
  private boolean selfTest = false;
  private Thread runThread = null;
  private ProcessResultDisplay processResultDisplay = null;
  private SystemProcessInterface process = null;
  private final ProcessResultDisplayFactory displayFactory;
  private boolean firstChildProcessSet = false;
  private LogFile childLog = null;
  private LogFile.WritingId childLogWritingId = null;
  private boolean stop = false;
  private boolean running = false;

  public void dumpState() {
  }

  /**
   * @param applicationManager
   * @param axisID
   */
  public CombineProcessMonitor(ApplicationManager manager, AxisID axisID,
      CombineComscriptState combineComscriptState,
      ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    this.combineComscriptState = combineComscriptState;
    this.processResultDisplay = processResultDisplay;
    selfTest = EtomoDirector.INSTANCE.getArguments().isSelfTest();
    runSelfTest(CONSTRUCTED_STATE);
    displayFactory = manager.getProcessResultDisplayFactory(axisID);
  }

  public void setProcess(SystemProcessInterface process) {
    this.process = process;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * returns false if the process has stopped, after giving run() a chance to
   * finish
   */
  public boolean isProcessRunning() {
    if (!processRunning) {
      // give run a chance to finish
      try {
        Thread.sleep(SLEEP);
      }
      catch (InterruptedException e) {
      }
      return false;
    }
    return true;
  }

  public void kill(SystemProcessInterface process, AxisID axisID) {
    endMonitor(ProcessEndState.KILLED);
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

  private void initializeProgressBar() {
    manager.startProgressBar(COMBINE_LABEL, axisID, ProcessName.SOLVEMATCH);
    return;
  }

  /**
   * get each .com file run by combine.com
   * @throws NumberFormatException
   * @throws IOException
   */
  private void getCurrentSection() throws LogFile.LockException, IOException {
    String line;
    String matchString = CombineComscriptState.getComscriptMatchString();
    while ((line = logFile.readLine(logFileReaderId)) != null) {
      int index = -1;
      if ((line.indexOf("running ") != -1 || line.indexOf("Running ") != -1)
          && line.matches(matchString)) {
        String[] fields = line.split("\\s+");
        for (int i = 0; i < fields.length; i++) {
          if (fields[i].matches(matchString)) {
            String comscriptName = fields[i];
            setCurrentChildCommand(comscriptName);
            runCurrentChildMonitor();
          }
        }
      }
      else if (line.startsWith("ERROR:") || line.startsWith("Traceback")) {
        process.setProcessResultDisplay(processResultDisplay);
        endMonitor(ProcessEndState.FAILED);
      }
      else if (line.startsWith(CombineComscriptState.getSuccessText())) {
        process.setProcessResultDisplay(processResultDisplay);
        endMonitor(ProcessEndState.DONE);
      }
    }
  }

  /**
   * get current .com file run by combine.com
   * run the monitor associated with the current .com file, if these is one
   * @param comscriptName
   */
  private void setCurrentChildCommand(String comscriptName) throws LogFile.LockException {
    if (childLog != null && childLogWritingId != null && !childLogWritingId.isEmpty()) {
      childLog.closeForWriting(childLogWritingId);
      childLogWritingId = null;
    }
    manager.progressBarDone(axisID, ProcessEndState.DONE);
    String childCommandName = comscriptName.substring(0, comscriptName.indexOf(".com"));
    currentCommand = ProcessName.getInstance(childCommandName, axisID);
    if (currentCommand != null) {
      childLog = LogFile
          .getInstance(manager.getPropertyUserDir(), axisID, currentCommand);
      childLogWritingId = childLog.openForWriting();
    }
    if (currentCommand == ProcessName.MATCHVOL1) {
      setNextProcessResultDisplay(displayFactory.getRestartMatchvol1());
      manager
          .showPane(CombineComscriptState.COMSCRIPT_NAME, CombineProcessType.MATCHVOL1);
      childMonitor = new Matchvol1ProcessMonitor(manager, axisID);
    }
    else if (currentCommand == ProcessName.PATCHCORR) {
      setNextProcessResultDisplay(displayFactory.getRestartPatchcorr());
      manager
          .showPane(CombineComscriptState.COMSCRIPT_NAME, CombineProcessType.PATCHCORR);
      childMonitor = new PatchcorrProcessWatcher(manager, axisID);
    }
    else if (currentCommand == ProcessName.MATCHORWARP) {
      setNextProcessResultDisplay(displayFactory.getRestartMatchorwarp());
      manager.showPane(CombineComscriptState.COMSCRIPT_NAME,
          CombineProcessType.MATCHORWARP);
      childMonitor = new MatchorwarpProcessMonitor(manager, axisID);
    }
    else if (currentCommand == ProcessName.VOLCOMBINE) {
      setNextProcessResultDisplay(displayFactory.getRestartVolcombine());
      manager.showPane(CombineComscriptState.COMSCRIPT_NAME,
          CombineProcessType.VOLCOMBINE);
      childMonitor = new VolcombineProcessMonitor(manager, axisID);
    }
    else {
      startProgressBar(childCommandName, currentCommand);
    }
  }

  private void setNextProcessResultDisplay(ProcessResultDisplay nextProcessResultDisplay) {
    if (!firstChildProcessSet) {
      firstChildProcessSet = true;
      return;
    }
    if (processResultDisplay != null) {
      processResultDisplay.msgProcessSucceeded();
    }
    endCurrentChildMonitor();
    processResultDisplay = nextProcessResultDisplay;
    if (processResultDisplay != null) {
      processResultDisplay.msgProcessStarting();
    }
  }

  /**
   * run the monitor associated with the current .com file run by combine.com
   *
   */
  private void runCurrentChildMonitor() {
    if (childMonitor == null) {
      return;
    }
    childMonitor.setLastProcess(false);
    childThread = new Thread(childMonitor);
    childThread.start();
  }

  /**
   * stop the current monitor associated with the current .com file run by
   * combine.com
   *
   */
  private void endCurrentChildMonitor() {
    if (childMonitor != null) {
      childMonitor.haltProcess(childThread);
    }
    childMonitor = null;
    childThread = null;
  }

  /**
   * end this monitor
   *
   */
  private void endMonitor(ProcessEndState endState) {
    if (childLog != null && childLogWritingId != null && !childLogWritingId.isEmpty()) {
      childLog.closeForWriting(childLogWritingId);
      childLogWritingId = null;
    }
    setProcessEndState(endState);
    endCurrentChildMonitor();
    manager.progressBarDone(axisID, endState);
    if (runThread != null) {
      runThread.interrupt();
      runThread = null;
    }
    processRunning = false;// the only place that this should be changed
  }

  /**
   * Start  a progress bar for the current .com file run by combine.com.
   * Used when there is no monitor available for the child process
   * Used for the process that runs before matchavol1
   * @param childCommandName
   */
  private void startProgressBar(String childCommandName, ProcessName processName) {
    CombineProcessType combineProcessType = CombineProcessType
        .getInstance(childCommandName);
    if (combineProcessType == null) {
      // must be a command that is not monitored
      return;
    }
    setNextProcessResultDisplay(null);
    manager.showPane(CombineComscriptState.COMSCRIPT_NAME, combineProcessType);
    manager
        .startProgressBar(COMBINE_LABEL + ": " + childCommandName, axisID, processName);
  }

  public void stop() {
    stop = true;
  }

  public boolean isRunning() {
    return running;
  }

  /**
   * Get log file.  Initialize progress bar.  Loop until processRunning is 
   * turned off or there is a timeout.  Call getCurrentSection for each loop.
   * After loop, turn off the monitor if that hasn't been done already.
   */
  public void run() {
    running = true;
    runThread = Thread.currentThread();
    initializeProgressBar();
    // Instantiate the logFile object
    try {
      logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
          CombineComscriptState.COMSCRIPT_NAME);

      // Wait for the log file to exist
      waitForLogFile();
      if (!processRunning) {
        running = false;
        return;
      }
      initializeProgressBar();

      while (processRunning && !stop) {
        Thread.sleep(SLEEP);
        getCurrentSection();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      endMonitor(ProcessEndState.FAILED);
      e.printStackTrace();
      UIHarness.INSTANCE
          .openMessageDialog(manager, e.getMessage(), "Etomo Error", axisID);
    }
    catch (InterruptedException e) {
      endMonitor(ProcessEndState.DONE);
    }
    catch (NumberFormatException e) {
      endMonitor(ProcessEndState.FAILED);
      e.printStackTrace();
    }
    catch (IOException e) {
      endMonitor(ProcessEndState.FAILED);
      e.printStackTrace();
    }
    // Close the log file reader
    Utilities.debugPrint("LogFileProcessMonitor: Closing the log file reader for "
        + logFile.getAbsolutePath());
    if (logFile != null) {
      logFile.closeRead(logFileReaderId);
      logFileReaderId = null;
    }
    runSelfTest(RAN_STATE);
    running = false;
  }

  public void msgLogFileRenamed() {
  }

  /**
   * Wait for the process to start and the appropriate log file to be created 
   * @return a buffered reader of the log file
   */
  private void waitForLogFile() throws LogFile.LockException, InterruptedException,
      FileNotFoundException {
    if (logFile == null) {
      throw new NullPointerException("logFile");
    }
    boolean newLogFile = false;
    boolean debug = EtomoDirector.INSTANCE.getArguments().isDebug();
    while (!newLogFile) {
      // Check to see if the log file exists that signifies that the process
      // has started
      if (logFile.exists()) {
        newLogFile = true;
      }
      else {
        if (process != null) {
          String[] array = process.getStdError();
          if (array != null) {
            for (int i = 0; i < array.length; i++) {
              if (debug) {
                System.err.println(array[i]);
              }
              if (array[i].startsWith("ERROR:") || array[i].startsWith("Traceback")
                  || array[i].indexOf("Errno") != -1) {
                endMonitor(ProcessEndState.FAILED);
                return;
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
                return;
              }
            }
          }
        }
        Thread.sleep(SLEEP);
      }
    }
    // Open the log file
    logFileReaderId = logFile.openReader();
    // logFileReader = new BufferedReader(new FileReader(logFile));
    runSelfTest(WAITED_FOR_LOG_STATE);
  }

  /**
   * Runs selfTest(int) when selfTest is set
   * @param selfTest
   * @param state
   */
  private void runSelfTest(int state) {
    if (!selfTest) {
      return;
    }
    selfTest(state);
  }

  /**
   * test for incorrect member variable settings.
   * @param state
   */
  public void selfTest(int state) {
    String stateString = null;
    switch (state) {
    case CONSTRUCTED_STATE:
      stateString = "After construction:  ";
      if (axisID == null) {
        throw new NullPointerException(stateString + "AxisID should not be null");
      }
      if (combineComscriptState == null) {
        throw new NullPointerException(stateString
            + "CombineComscriptState should not be null");
      }
      if (!processRunning) {
        throw new IllegalStateException(stateString + "ProcessRunning must be true");
      }

      break;

    case WAITED_FOR_LOG_STATE:
      stateString = "After waitForLogFile():  ";
      if (logFile.exists() && sleepCount != 0) {
        throw new IllegalStateException(stateString
            + "The sleepCount should be reset when the log file is found.  "
            + "sleepCount=" + sleepCount);
      }

      break;

    case RAN_STATE:
      stateString = "After run():  ";
      if (processRunning) {
        throw new IllegalStateException(stateString + "ProcessRunning should be false.");
      }
      break;

    default:
      throw new IllegalStateException("Unknown state.  state=" + state);
    }
  }

  public void pause(SystemProcessInterface process, AxisID axisID) {
    throw new IllegalStateException("can't pause a combine process");
  }

  public String getStatusString() {
    return null;
  }

  public final String getErrorMessage() {
    return null;
  }

  public ProcessMessages getProcessMessages() {
    return null;
  }

  public void useMessageReporter() {
  }

  public final String getProcessOutputFileName() throws LogFile.LockException {
    return null;
  }

  public boolean isPausing() {
    return false;
  }

  public void setWillResume() {
  }
}