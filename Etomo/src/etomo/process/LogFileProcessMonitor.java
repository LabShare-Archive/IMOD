package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.38  2011/02/22 04:04:22  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.37  2010/10/07 04:48:31  sueh
 * <p> bug$ 1409 Increased stop waiting timeout.
 * <p>
 * <p> Revision 3.36  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 3.35  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.34  2010/01/11 23:56:32  sueh
 * <p> bug# 1299 Added useMessageReporter.
 * <p>
 * <p> Revision 3.33  2009/06/10 17:24:55  sueh
 * <p> bug# 1202 Parse the mrc header based on
 * <p> EtomoDirector.ImodBriefHeader.
 * <p>
 * <p> Revision 3.32  2009/06/05 01:57:32  sueh
 * <p> bug# 1219 Reduced visibility of functions.
 * <p>
 * <p> Revision 3.31  2009/03/17 00:41:56  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.30  2009/02/04 23:26:03  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.29  2008/01/31 20:18:44  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 3.28  2008/01/14 21:30:30  sueh
 * <p> bug# 1050 Added stop() and isRunning() to allow ProcessMonitor classes to work
 * <p> with ReconnectProcess.
 * <p>
 * <p> Revision 3.27  2006/10/24 21:19:34  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 3.26  2006/10/10 05:09:33  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 3.25  2006/09/25 16:36:08  sueh
 * <p> bug# 931 Added empty function msgLogFileRenamed().
 * <p>
 * <p> Revision 3.24  2006/09/19 22:23:46  sueh
 * <p> bug# 928 Added postProcess().  If the process is not the last process, give the
 * <p> option of post processing by adding a post process call.
 * <p>
 * <p> Revision 3.23  2006/08/09 20:14:05  sueh
 * <p> bug# 631 Make updateProgressBar() overrideable.
 * <p>
 * <p> Revision 3.22  2005/11/19 02:32:56  sueh
 * <p> bug# 744 Moving pause, getStatusString, and getProcessMessages to
 * <p> ProcessMonitor because they are potentially valid things to do for any
 * <p> monitor, not just monitors of detached processes.
 * <p>
 * <p> Revision 3.21  2005/08/30 18:44:51  sueh
 * <p> bug# 532 Removed functions that only belong to BackgroundProcessMonitor:
 * <p> getErrorMessage, getStatusString, pause, and setProcess.
 * <p>
 * <p> Revision 3.20  2005/08/27 22:30:04  sueh
 * <p> bug# 532 Add an empty getErrorMessage() to implement ProcessMonitor.
 * <p> This is used by ProcesschunksProcessMonitor.
 * <p>
 * <p> Revision 3.19  2005/08/22 16:35:34  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.
 * <p>
 * <p> Revision 3.18  2005/08/15 18:21:38  sueh
 * <p> bug# 532  Added kill and pause functions to implement ProcessMonitor.
 * <p> Only kill is valid to use with this class.  Allows processchunks to signal
 * <p> interrupt instead of kill.
 * <p>
 * <p> Revision 3.17  2005/08/04 19:45:07  sueh
 * <p> bug# 532 Added empty setProcess() to implement ProcessMonitor.
 * <p>
 * <p> Revision 3.16  2005/07/26 21:34:41  sueh
 * <p> bug# 701 Implementing ProcessMonitor, which extends Runnable.
 * <p> Added a ProcessEndState member variable.  Set it to DONE when the
 * <p> end of the process is detected.  In the future, will set the
 * <p> ProcessEndState variable to FAILED, if necessary to get the correct
 * <p> progress bar behavior.
 * <p>
 * <p> Revision 3.15  2005/03/11 01:33:42  sueh
 * <p> bug# 533 correcting a comment.
 * <p>
 * <p> Revision 3.14  2005/03/09 22:29:00  sueh
 * <p> bug# 533 Reducing the timeout count to 10 so blendmont monitor
 * <p> finishes faster.
 * <p>
 * <p> Revision 3.13  2005/03/09 18:05:19  sueh
 * <p> bug# 533 Added done boolean so that XcorrProcessWatcher knows when
 * <p> BlendmontProcessMonitor is done.  When TiltxcorrProcessWatcher runs
 * <p> under XcorrProcessWatcher, it doesn't get its interrupt exception and is
 * <p> doing a time out instead.  This makes the process bar act strangely.
 * <p> When the timeout is in use (waitingForExit) display "Ending" on the
 * <p> progress bar.
 * <p>
 * <p> Revision 3.12  2004/11/19 23:22:25  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.11.2.2  2004/10/11 02:03:23  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.11.2.1  2004/09/29 17:55:09  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.11  2004/08/24 20:33:44  sueh
 * <p> bug# 508 create a haltProcess function to halt the monitor
 * <p> from another thread.  Make sure that the progress bar won't
 * <p> be updated after haltProcess is run
 * <p>
 * <p> Revision 3.10  2004/08/23 23:37:00  sueh
 * <p> bug# 508 backed out most recently checked in changes, except
 * <p> for lastProcess
 * <p>
 * <p> Revision 3.9  2004/08/19 02:27:17  sueh
 * <p> bug# 508 Preventing progress bar updates that prevent
 * <p> CombineProcessMonitor from working.  Checking processRunning to
 * <p> see if the progress bar should be changed.  Using a new member
 * <p> variable lastProcess to prevent LogFileProcessMonitor from controlling
 * <p> when a progress bar should be shut off.  If lastProcess is false, then
 * <p> LogFileProcessMonitor is too slow to shut off the progress bar and
 * <p> sometimes shuts it off after it has started for the next process.
 * <p> Created a way to force the monitor to shut off, so I made
 * <p> processRunning a member variable.  It starts as true, and then is set
 * <p> to false perminently.  This helped CombineProcessMonitor be more
 * <p> accurate.  Looks pretty good in this object.  Will have to test it some
 * <p> more.  It shouldn't be a problem because we always create new
 * <p> monitors for each process.
 * <p> Added:
 * <p> boolean lastProcess
 * <p> boolean processRunning
 * <p> setLastProcess(boolean lastProcess)
 * <p> setProcessRunning(boolean processRunning)
 * <p> Changed:
 * <p> run()
 * <p> updateProgressBar()
 * <p>
 * <p> Revision 3.8  2004/05/21 02:15:51  sueh
 * <p> bug# 83 fixing a null pointer bug
 * <p>
 * <p> Revision 3.7  2004/04/23 20:04:09  sueh
 * <p> bug# 83 using initializeProgressBar() to initialize progress bar
 * <p> before running waitForLogFile()
 * <p>
 * <p> Revision 3.6  2004/04/23 19:36:56  sueh
 * <p> bug# 83 adding a "starting" comment when starting the process bar
 * <p>
 * <p> Revision 3.5  2004/04/08 17:33:59  rickg
 * <p> Use Utilities.milliesToMinAndSecs to get time string
 * <p>
 * <p> Revision 3.4  2004/03/22 23:44:09  sueh
 * <p> bug# 83 allowed findNSections() to be overridden, allowed for a 
 * <p> non-standard log file name
 * <p>
 * <p> Revision 3.3  2004/03/16 21:52:26  sueh
 * <p> bug# 413 reset process bar after waiting for exit if exit signal doesn't
 * <p> come
 * <p>
 * <p> Revision 3.2  2004/03/13 01:55:29  sueh
 * <p> bug# 413 possible solution infinite run() loop in comments
 * <p>
 * <p> Revision 3.1  2003/11/27 00:01:27  rickg
 * <p> logFile is now a member object
 * <p> made sure the the logFile is closed when the monitor is done
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.3  2003/09/08 22:23:37  rickg
 * <p> Limit percentage done to between 0 and 99
 * <p>
 * <p> Revision 1.2  2003/08/05 21:17:23  rickg
 * <p> Initial revision (really)
 * <p>
 * <p> Revision 1.1  2003/08/04 22:23:16  rickg
 * <p> Initial revision
 * <p> </p>
 */

public abstract class LogFileProcessMonitor implements ProcessMonitor {
  public static final String rcsid = "$Id$";

  static final int UPDATE_PERIOD = 500;

  private long processStartTime;
  // protected BufferedReader logFileReader;
  int nSections = Integer.MIN_VALUE;
  int currentSection;
  private boolean processRunning = true;
  private boolean done = false;

  private boolean lastProcess = true;

  boolean standardLogFileName = true;
  private ProcessEndState endState = null;
  private boolean stop = false;
  private boolean running = false;
  boolean ending = false;

  // This needs to be set in the concrete class constructor
  String logFileBasename;
  private LogFile logFile;
  private LogFile.ReaderId logFileReaderId = null;

  final BaseManager manager;
  final AxisID axisID;

  final String nSectionsHeader;
  final int nSectionsIndex;

  private double averageSectionTime = 0;
  private double updatedAverageSectionTime = 0;
  private double timestamp = 0;
  private double waitTime = 0;
  private int doneSections = 0;
  private long percentDone = 0;
  private double etc = -1;

  public void dumpState() {
  }

  abstract void initializeProgressBar();

  abstract void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException;

  /**
   * Default constructor
   * @param appMgr  The application manager object
   * @param id  The axis ID to be monitored
   */
  LogFileProcessMonitor(final BaseManager manager, final AxisID id) {
    this.manager = manager;
    axisID = id;
    if (!EtomoDirector.INSTANCE.isImodBriefHeader()) {
      nSectionsHeader = MRCHeader.SIZE_HEADER;
      nSectionsIndex = MRCHeader.N_SECTIONS_INDEX;
    }
    else {
      nSectionsHeader = MRCHeader.SIZE_HEADER_BRIEF;
      nSectionsIndex = MRCHeader.N_SECTIONS_INDEX_BRIEF;
    }
  }

  public final void stop() {
    stop = true;
  }

  public void useMessageReporter() {
  }

  public final boolean isRunning() {
    return running;
  }

  public final void run() {
    running = true;
    initializeProgressBar();
    // Instantiate the logFile object
    try {
      if (standardLogFileName) {
        // logFileName = logFileBasename + axisID.getExtension() + ".log";
        logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
            logFileBasename);
      }
      else {
        // logFileName = logFileBasename;
        logFile = LogFile.getInstance(manager.getPropertyUserDir(), logFileBasename);

      }
      // Wait for the log file to exist
      waitForLogFile();
      findNSections();
      initializeProgressBar();
      timestamp = new Date().getTime();
      while (processRunning && !stop) {
        Thread.sleep(UPDATE_PERIOD);
        getCurrentSection();
        calcRemainingTime();
        updateProgressBar();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
      processRunning = false;
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    setProcessEndState(ProcessEndState.DONE);
    // Close the log file reader
    Utilities.debugPrint("LogFileProcessMonitor: Closing the log file reader for "
        + logFile.getAbsolutePath());
    // if (logFileReader != null) {
    // logFileReader.close();
    // }
    if (logFile != null && logFileReaderId != null && !logFileReaderId.isEmpty()) {
      logFile.closeRead(logFileReaderId);
      logFileReaderId = null;
    }
    if (lastProcess) {
      manager.progressBarDone(axisID, endState);
    }
    done = true;
    if (!lastProcess) {
      postProcess();
    }
    running = false;
  }

  public final void msgLogFileRenamed() {
  }

  void postProcess() {
  }

  final String readLogFileLine() throws LogFile.LockException, IOException {
    return logFile.readLine(logFileReaderId);
  }

  /**
   * set end state
   * @param endState
   */
  public synchronized final void setProcessEndState(final ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  public final ProcessEndState getProcessEndState() {
    return endState;
  }

  final boolean isDone() {
    return done;
  }

  /**
   * set lastProcess
   * @param lastProcess
   */
  final void setLastProcess(final boolean lastProcess) {
    this.lastProcess = lastProcess;
  }

  /**
   * Wait for the process to start and the appropriate log file to be created 
   * @return a buffered reader of the log file
   */
  private final void waitForLogFile() throws InterruptedException, LogFile.LockException,
      FileNotFoundException {

    processStartTime = System.currentTimeMillis();

    boolean newLogFile = false;
    while (!newLogFile) {
      // Check to see if the log file exists that signifies that the process
      // has started
      if (logFile.exists()) {
        newLogFile = true;
      }
      else {
        Thread.sleep(UPDATE_PERIOD);
      }
    }
    // Open the log file
    // logFileReader = new BufferedReader(new FileReader(logFile));
    logFileReaderId = logFile.openReader();
  }

  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  void findNSections() throws InterruptedException, NumberFormatException,
      LogFile.LockException, InvalidParameterException, IOException {

    // Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;

    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(UPDATE_PERIOD);
      String line;
      while ((line = logFile.readLine(logFileReaderId)) != null) {
        line = line.trim();
        if (line.startsWith(nSectionsHeader)) {
          String[] fields = line.split("\\s+");
          if (fields.length > nSectionsIndex) {
            nSections = Integer.parseInt(fields[nSectionsIndex]);
            foundNSections = true;
            break;
          }
          else {
            throw new NumberFormatException("Incomplete size line in header");
          }
        }
      }
    }
  }

  /**
   * Calculate the amount of time remaining and percent done.
  */
  private void calcRemainingTime() {
    double newtimeStamp = new Date().getTime();
    waitTime += newtimeStamp - timestamp;
    timestamp = newtimeStamp;
    // Has the current section been incremented?
    if (doneSections < currentSection - 1) {
      // At least one section has been completed since the last time this function was
      // called, so recalculate average section completion time: the total time over the
      // total number of sections done.
      averageSectionTime = ((doneSections * averageSectionTime) + waitTime)
          / (currentSection - 1);
      doneSections = currentSection - 1;
      waitTime = 0;
      updatedAverageSectionTime = averageSectionTime;
      percentDone = Math.round((double) doneSections / nSections * 100);
    }
    // If the current section has not been incremented and the wait for the current
    // section to be completed exceeds the average section completion time, then
    // recalculate updatedAverageSectionTime.
    else if (waitTime > averageSectionTime) {
      updatedAverageSectionTime = ((doneSections * averageSectionTime) + waitTime)
          / (doneSections + 1);
    }
    // Calculate the new estimated time to completion.
    if (updatedAverageSectionTime > 0) {
      etc = updatedAverageSectionTime * (nSections - doneSections) - waitTime;
    }
  }

  /**
   * Update the progress bar with percentage done and estimated time to
   * completion message. 
   */
  void updateProgressBar() {
    if (ending) {
      manager.getMainPanel().setProgressBarValue(0, "Ending...", axisID);
      return;
    }
    String message;
    if (etc >= 0) {
      // Format the progress bar string
      message = String.valueOf(percentDone) + "%   ETC: "
          + Utilities.millisToMinAndSecs(etc);
    }
    else {
      message = String.valueOf(percentDone) + "%";
    }
    if (processRunning) {
      manager.getMainPanel().setProgressBarValue(doneSections, message, axisID);
    }
  }

  final void haltProcess(final Thread runThread) {
    processRunning = false;
    runThread.interrupt();
  }

  public final void kill(final SystemProcessInterface process, final AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalKill(axisID);
  }

  public final ProcessMessages getProcessMessages() {
    return null;
  }

  public final String getStatusString() {
    return null;
  }

  public final void pause(final SystemProcessInterface process, final AxisID axisID) {
    throw new IllegalStateException("pause illegal in this monitor");
  }

  public boolean isPausing() {
    return false;
  }

  public void setWillResume() {
  }
}
