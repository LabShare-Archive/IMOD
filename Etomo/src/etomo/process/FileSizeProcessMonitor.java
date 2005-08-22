package etomo.process;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.channels.FileChannel;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
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

public abstract class FileSizeProcessMonitor implements ProcessMonitor {
  public static final String rcsid = "$Id$";
  ApplicationManager applicationManager;
  AxisID axisID;
  long processStartTime;
  long scriptStartTime;
  File watchedFile;
  FileChannel watchedChannel;
  private ProcessEndState endState = null;

  int nKBytes;

  int updatePeriod = 250;

  public FileSizeProcessMonitor(ApplicationManager appMgr, AxisID id) {
    applicationManager = appMgr;
    axisID = id;
    scriptStartTime = System.currentTimeMillis();
  }

  // The dervied class must implement this function to 
  // - set the expected number of bytes in the output file
  // - initialize the progress bar through the application manager, the maximum
  //   value should be the expected size of the file in k bytes
  // - set the watchedFile reference to the output file being monitored.
  abstract void calcFileSize() throws InvalidParameterException, IOException;

  public void run() {
    try {
      // Reset the progressBar 
      applicationManager.getMainPanel().setProgressBar(" ", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);

      //  Calculate the expected file size in bytes, initialize the progress bar
      //  and set the File object.
      calcFileSize();

      //  Wait for the output file to be created and set the process start time
      waitForFile();
    }
    //  Interrupted ???  kill the thread by exiting
    catch (InterruptedException except) {
      setProcessEndState(ProcessEndState.DONE);
      closeChannel();
      return;
    }
    catch (InvalidParameterException except) {
      setProcessEndState(ProcessEndState.DONE);
      except.printStackTrace();
      closeChannel();
      return;
    }
    catch (IOException except) {
      setProcessEndState(ProcessEndState.DONE);
      closeChannel();
      return;
    }
    setProcessEndState(ProcessEndState.DONE);
    // Periodically update the process bar by checking the size of the file
    updateProgressBar();
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

  /**
   * Wait for the new output file to be created.  Make sure it is current by
   * comparing the modification time of the file to the start time of this
   * function. Set the process start time to the first new file modification
   * time since we don't have access to the file creation time.  
   */
  void waitForFile() throws InterruptedException {
    long currentSize = 0;
    boolean newOutputFile = false;
    boolean needChannel = true;
    while (!newOutputFile) {
      if (watchedFile.exists() && watchedFile.lastModified() > scriptStartTime) {
        if (needChannel) {
          FileInputStream stream = null;
          try {
            stream = new FileInputStream(watchedFile);
          }
          catch (FileNotFoundException e) {
            e.printStackTrace();
            System.err
              .println("Shouldn't be in here, we already checked for existence");
          }
          watchedChannel = stream.getChannel();
          needChannel = false;
          processStartTime = System.currentTimeMillis();
        }

        try {
          currentSize = watchedChannel.size();

          // Hang out in this loop until the current size is something greater
          // than zero, otherwise the progress bar update with have a divide
          // by zero
          if (currentSize > 0) {
            newOutputFile = true;
          }
        }
        catch (IOException except) {
          except.printStackTrace();
        }
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

    while (fileWriting) {
      int currentLength = 0;
      try {
        currentLength = (int) (watchedChannel.size() / 1024);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      double fractionDone = (double) currentLength / nKBytes;
      int percentage = (int) Math.round(fractionDone * 100);

      //  Catch any wierd values before they get displayed
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
      applicationManager.getMainPanel().setProgressBarValue(currentLength, message, axisID);

      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException exception) {
        fileWriting = false;
        closeChannel();
      }
    }
  }
  
  /**
   * Attempt to close the watched channel file.
   */
  protected void closeChannel() {
    if (watchedChannel == null) {
      return;
    }
    try {
      watchedChannel.close();
    }
    catch (IOException e1) {
      e1.printStackTrace();
    }

  }
  
  public void setProcess(SystemProcessInterface process) {
    //process is not required
  }
  
  public void kill(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalKill(axisID);
  }
  
  public void pause(SystemProcessInterface process, AxisID axisID) {
    throw new IllegalStateException("can't pause process");
  }
  
  public String getStatusString() {
    return null;
  }
}