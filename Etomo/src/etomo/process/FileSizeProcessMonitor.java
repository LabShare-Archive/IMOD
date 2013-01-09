package etomo.process;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.channels.FileChannel;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.ui.swing.UIHarness;
import etomo.util.InvalidParameterException;
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
 * <p> Revision 3.39  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.38  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 3.37  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.36  2010/01/11 23:50:41  sueh
 * <p> bug# 1299 Monitoring the log file with MessageReporter.
 * <p>
 * <p> Revision 3.35  2009/09/17 19:15:00  sueh
 * <p> bug# 1257 Added getModeBytes to handle getting the right number of
 * <p> bytes based on the mode in a single location.  Also added mode 6 and
 * <p> removed modes not supported by IMOD.
 * <p>
 * <p> Revision 3.34  2009/03/17 00:35:55  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.33  2009/02/13 02:14:27  sueh
 * <p> bug# 1176 Checking return value of calcFileSize.
 * <p>
 * <p> Revision 3.32  2009/02/04 23:25:10  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.31  2008/10/27 23:19:52  sueh
 * <p> bug# 1141 Fixed monitor - In CTF correction,log file doesn't show the
 * <p> watched file's name, so turn off findWatchedFileName to avoid looking
 * <p> for it.
 * <p>
 * <p> Revision 3.30  2008/01/31 20:18:19  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 3.29  2008/01/23 21:10:25  sueh
 * <p> bug# 1064 In openLogFileReader do not wait for the log file to be renamed if this
 * <p> is a reconnect.
 * <p>
 * <p> Revision 3.28  2008/01/14 21:30:06  sueh
 * <p> bug# 1050 Made stop() and isRunning() public to implement ProcessMonitor.
 * <p>
 * <p> Revision 3.27  2006/10/24 21:19:10  sueh
 * <p> bug# 947 Changed ProcessName.fromString() to getInstance().
 * <p>
 * <p> Revision 3.26  2006/10/10 05:08:51  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 3.25  2006/09/25 16:35:48  sueh
 * <p> bug# 931 Added msgLogFileRenamed() and logFileRenamed.  use
 * <p> logFileRenamed to open the correct log file.
 * <p>
 * <p> Revision 3.24  2006/09/22 23:47:04  sueh
 * <p> bug # 931 Making sure that the objects that can block files in Windows
 * <p> always run their close() function each time they are instanciated.
 * <p>
 * <p> Revision 3.23  2006/09/22 18:17:53  sueh
 * <p> bug# 931 Changed waitForFile():  Using the log file to find out when the output
 * <p> file has been backed up.
 * <p>
 * <p> Revision 3.22  2006/08/11 00:14:34  sueh
 * <p> bug# 739 Added lastSize.  Changed waitForFile() to stop checking the file time,
 * <p> since this is unrealible across computers.  Wait until the watched file starts
 * <p> growing instead.
 * <p>
 * <p> Revision 3.21  2006/08/09 20:10:01  sueh
 * <p> bug# 631 UpdateProgressBar():  check usingLog() before doing the percentage
 * <p> calculation.
 * <p>
 * <p> Revision 3.20  2006/08/02 22:23:30  sueh
 * <p> bug# 769 Added booleans reconnect, stop, and running to allow a file size
 * <p> monitor to alter its display for reconnecting and be controlled by its stop boolean.
 * <p>
 * <p> Revision 3.19  2006/03/27 19:37:17  sueh
 * <p> bug# 836 Removed print statements
 * <p>
 * <p> Revision 3.18  2006/03/23 18:17:56  sueh
 * <p> bug# 836 Added temporary diagnostics
 * <p>
 * <p> Revision 3.17  2005/11/19 02:26:22  sueh
 * <p> bug# 744 Moving pause, getStatusString, and getProcessMessages to
 * <p> ProcessMonitor because they are potentially valid things to do for any
 * <p> monitor, not just monitors of detached processes.
 * <p>
 * <p> Revision 3.16  2005/08/30 18:42:16  sueh
 * <p> bug# Removed functions that only belong to BackgroundProcessMonitor:
 * <p> getErrorMessage, getStatusString, pause, and setProcess.
 * <p>
 * <p> Revision 3.15  2005/08/27 22:25:34  sueh
 * <p> bug# 532 Add an empty getErrorMessage() to implement ProcessMonitor.
 * <p> This is used by ProcesschunksProcessMonitor.
 * <p>
 * <p> Revision 3.14  2005/08/22 16:20:49  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.
 * <p>
 * <p> Revision 3.13  2005/08/15 18:20:54  sueh
 * <p> bug# 532  Added kill and pause functions to implement ProcessMonitor.
 * <p> Only kill is valid to use with this class.  Allows processchunks to signal
 * <p> interrupt instead of kill.
 * <p>
 * <p> Revision 3.12  2005/08/04 19:44:57  sueh
 * <p> bug# 532 Added empty setProcess() to implement ProcessMonitor.
 * <p>
 * <p> Revision 3.11  2005/07/26 18:46:04  sueh
 * <p> bug# 701 Implementing ProcessMonitor, which extends Runnable.
 * <p> Added a ProcessEndState member variable.  Set it to DONE when the
 * <p> end of the process is detected.  In the future, will set the
 * <p> ProcessEndState variable to FAILED, if necessary to get the correct
 * <p> progress bar behavior.
 * <p>
 * <p> Revision 3.10  2005/07/20 17:44:06  sueh
 * <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
 * <p> MRCHeader, because its filling up the error log with exceptions that are
 * <p> related to real problems.
 * <p>
 * <p> Revision 3.9  2005/07/11 22:47:18  sueh
 * <p> bug# 619 Made closeChannel() protected so it can be called by
 * <p> overridden functions.
 * <p>
 * <p> Revision 3.8  2004/11/19 23:19:40  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.7.4.1  2004/09/29 17:54:02  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.7  2004/06/28 22:49:18  rickg
 * <p> Bug #478  Added calls to .close in any possible exception.
 * <p>
 * <p> Revision 3.6  2004/06/17 23:55:51  rickg
 * <p> Bug #460 moved getting of current time into FileSizeProcessMonitor on
 * <p> instantiation
 * <p>
 * <p> Revision 3.5  2004/06/17 23:34:17  rickg
 * <p> Bug #460 added script starting time to differentiate old data files
 * <p>
 * <p> Revision 3.4  2004/06/17 23:06:21  rickg
 * <p> Bug #460 Using nio FileChannle.size() method to monitor file since it seems 
 * <p> to be much more reliable than the File.length() method
 * <p>
 * <p> Revision 3.3  2004/06/17 01:29:12  sueh
 * <p> removed unnecessary import
 * <p>
 * <p> Revision 3.2  2004/06/14 17:26:08  sueh
 * <p> bug# 460 set startTime earlier, allow startTime to be set in
 * <p> the child class, make sure the instance is still reuseable
 * <p>
 * <p> Revision 3.1  2004/04/08 17:33:59  rickg
 * <p> Use Utilities.milliesToMinAndSecs to get time string
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.5  2003/09/08 22:21:31  rickg
 * <p> Limit percentage done to between 0 and 99
 * <p>
 * <p> Revision 1.4  2003/08/04 22:22:57  rickg
 * <p> Fixed typo
 * <p>
 * <p> Revision 1.3  2003/07/01 22:55:02  rickg
 * <p> Added starting text to progress for slow startup scripts
 * <p>
 * <p> Revision 1.2  2003/07/01 19:26:30  rickg
 * <p> Changed all sizes to k bytes
 * <p> Change round to floor to prevent 1:60 times
 * <p>
 * <p> Revision 1.1  2003/06/27 20:16:36  rickg
 * <p> Initial revision
 * <p> </p>
 */

abstract class FileSizeProcessMonitor implements ProcessMonitor {
  public static final String rcsid = "$Id$";
  final BaseManager manager;
  final AxisID axisID;
  long processStartTime;
  long scriptStartTime;
  File watchedFile;
  private FileChannel watchedChannel;
  private ProcessEndState endState = null;
  private boolean reconnect = false;
  private boolean stop = false;
  private boolean running = false;
  private boolean usingLog = false;
  int nKBytes;
  int updatePeriod = 250;
  long lastSize = 0;
  final ProcessName processName;
  private FileInputStream stream = null;
  // private BufferedReader logFileReader = null;
  // private FileReader fileReader = null;
  private boolean logFileRenamed = false;
  private LogFile logFile;
  private LogFile.ReaderId logReaderId = null;
  private boolean findWatchedFileName = true;
  private MessageReporter messageReporter = null;

  public void dumpState() {
  }

  public FileSizeProcessMonitor(BaseManager manager, AxisID id, ProcessName processName) {
    this.manager = manager;
    axisID = id;
    scriptStartTime = System.currentTimeMillis();
    this.processName = processName;
    try {
      logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID, processName);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to create log file.\n" + e.getMessage(),
          "File Size Monitor Log File Failure");
      logFile = null;
    }
  }

  // The derived class must implement this function to
  // - set the expected number of bytes in the output file
  // - initialize the progress bar through the application manager, the maximum
  // value should be the expected size of the file in k bytes
  // - set the watchedFile reference to the output file being monitored.
  abstract boolean calcFileSize() throws InvalidParameterException, IOException;

  public int getModeBytes(int mode) throws InvalidParameterException {
    switch (mode) {
    case 0:
      return 1;
    case 1:
      return 2;
    case 2:
      return 4;
    case 6:
      return 2;
    default:
      throw new InvalidParameterException("Unknown mode parameter");
    }
  }

  abstract void reloadWatchedFile();

  public void run() {
    running = true;
    try {
      // Reset the progressBar
      manager.getMainPanel().setProgressBar(" ", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0,
          reconnect ? "Reconnecting..." : "Starting...", axisID);

      // Calculate the expected file size in bytes, initialize the progress bar
      // and set the File object.
      if (!calcFileSize()) {
        setProcessEndState(ProcessEndState.DONE);
        closeOpenFiles();
        return;
      }
      // Wait for the output file to be backed up and set the process start time
      waitForFile();
    }
    // Interrupted ??? kill the thread by exiting
    catch (InterruptedException except) {
      setProcessEndState(ProcessEndState.DONE);
      closeOpenFiles();
      return;
    }
    catch (InvalidParameterException except) {
      setProcessEndState(ProcessEndState.DONE);
      except.printStackTrace();
      closeOpenFiles();
      return;
    }
    catch (IOException except) {
      setProcessEndState(ProcessEndState.DONE);
      closeOpenFiles();
      return;
    }

    setProcessEndState(ProcessEndState.DONE);
    // Periodically update the process bar by checking the size of the file
    updateProgressBar();
    try {
      Thread.sleep(updatePeriod);
    }
    catch (InterruptedException e) {
    }
    logFileRenamed = false;
    running = false;
  }

  public void useMessageReporter() {
    messageReporter = new MessageReporter(axisID, logFile);
  }

  public void stop() {
    stop = true;
  }

  public boolean isRunning() {
    return running;
  }

  LogFile getLogFile() {
    return logFile;
  }

  void setReconnect(boolean reconnect) {
    this.reconnect = reconnect;
  }

  /**
   * set end state
   */
  public synchronized final void setProcessEndState(ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  public synchronized final ProcessEndState getProcessEndState() {
    return endState;
  }

  public void msgLogFileRenamed() {
    logFileRenamed = true;
  }

  /**
   * Wait for the new output file to be created.  Make sure it is current by
   * comparing the modification time of the file to the start time of this
   * function. Set the process start time to the first new file modification
   * time since we don't have access to the file creation time.  
   */
  void waitForFile() throws InterruptedException {
    // look for signal that the watched file has been backed up in the log file
    openLogFileReader();
    boolean watchedFileBackedUp = false;
    while (!watchedFileBackedUp) {
      String line = null;
      try {
        line = logFile.readLine(logReaderId);
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      if (line == null) {
        Thread.sleep(updatePeriod);
      }
      else {
        reloadWatchedFile();
        if (!findWatchedFileName
            || (line.indexOf(watchedFile.getName()) != -1 && line.trim().toLowerCase()
                .startsWith("new image file on unit"))) {
          watchedFileBackedUp = true;
        }
      }
    }
    closeLogFileReader();
    // Get the watched file
    long currentSize = 0;
    boolean newOutputFile = false;
    while (!newOutputFile) {
      openChannel();
      processStartTime = System.currentTimeMillis();
      try {
        currentSize = watchedChannel.size();
        // Hang out in this loop until the file size is growing, otherwise the
        // progress bar update with have a divide by zero
        // The ~ file won't change, so the correct file should be loaded when
        // the loop ends, however, the first comparison will probably be
        // between the initial lastSize value of 0 and the ~ size, so ignore
        // the first comparison
        if (lastSize > 0 && currentSize > lastSize) {
          newOutputFile = true;
        }
        lastSize = currentSize;
      }
      catch (IOException except) {
        except.printStackTrace();
      }
      Thread.sleep(updatePeriod);
    }
  }

  /**
   * Watch the file size, comparing it to the expected completed file size and
   * update the progress bar 
   *
   */
  void updateProgressBar() {
    boolean fileWriting = true;
    while (fileWriting && !stop) {
      if (!gotStatusFromLog()) {
        int currentLength = 0;
        long size = 0;
        try {
          currentLength = (int) (watchedChannel.size() / 1024);
        }
        catch (IOException e) {
          e.printStackTrace();
        }
        double fractionDone = (double) currentLength / nKBytes;
        int percentage = (int) Math.round(fractionDone * 100);

        // Catch any wierd values before they get displayed
        if (percentage < 0) {
          percentage = 0;
        }
        if (percentage > 99) {
          percentage = 99;
        }

        long elapsedTime = System.currentTimeMillis() - processStartTime;
        double remainingTime = elapsedTime / fractionDone - elapsedTime;
        String message = String.valueOf(percentage) + "%   ETC: "
            + Utilities.millisToMinAndSecs(remainingTime);
        manager.getMainPanel().setProgressBarValue(currentLength, message, axisID);
      }
      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException exception) {
        fileWriting = false;
      }
      if (messageReporter != null) {
        messageReporter.checkForMessages(manager);
      }
    }
    if (messageReporter != null) {
      messageReporter.close();
    }
    closeOpenFiles();
  }

  boolean gotStatusFromLog() {
    return false;
  }

  /**
   * Attempt to close the watched channel file.
   */
  void closeOpenFiles() {
    closeChannel();
    closeLogFileReader();
    if (messageReporter != null) {
      messageReporter.close();
    }
  }

  void setFindWatchedFileName(boolean input) {
    findWatchedFileName = input;
  }

  private void openLogFileReader() throws InterruptedException {
    if (!reconnect) {
      while (!logFileRenamed) {
        Thread.sleep(updatePeriod);
      }
    }
    // File logFile = null;
    boolean logFileExists = false;
    while (!logFileExists) {
      // logFile = DatasetFiles
      // .getLogFile(applicationManager, axisID, processName);
      // Check to see if the log file exists that signifies that the process
      // has started
      if (/* logFile != null && */logFile.exists()) {
        logFileExists = true;
      }
      else {
        Thread.sleep(updatePeriod);
      }
    }
    // closeLogFileReader();
    try {
      // fileReader = new FileReader(logFile);
      logReaderId = logFile.openReader();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    // logFileReader = new BufferedReader(fileReader);
  }

  private void closeLogFileReader() {
    if (logReaderId != null && !logReaderId.isEmpty()) {
      logFile.closeRead(logReaderId);
      logReaderId = null;
    }
  }

  private void openChannel() throws InterruptedException {
    boolean watchedFileExists = false;
    while (!watchedFileExists) {
      reloadWatchedFile();
      if (watchedFile.exists()) {
        watchedFileExists = true;
      }
      else {
        Thread.sleep(updatePeriod);
      }
    }
    closeChannel();
    try {
      stream = new FileInputStream(watchedFile);
      watchedChannel = stream.getChannel();
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
  }

  private void closeChannel() {
    if (watchedChannel != null) {
      try {
        watchedChannel.close();
        watchedChannel = null;
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
    }
    if (stream != null) {
      try {
        stream.close();
        stream = null;
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
    }
  }

  public void kill(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalKill(axisID);
  }

  public ProcessMessages getProcessMessages() {
    return null;
  }

  public String getStatusString() {
    return null;
  }

  public void pause(SystemProcessInterface process, AxisID axisID) {
    throw new IllegalStateException("pause illegal in this monitor");
  }

  public boolean isPausing() {
    return false;
  }

  public void setWillResume() {
  }
}