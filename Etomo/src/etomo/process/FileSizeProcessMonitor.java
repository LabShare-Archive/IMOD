package etomo.process;

import java.io.File;

import etomo.ApplicationManager;
import etomo.type.AxisID;

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
 * <p> $Log$ </p>
 */

public abstract class FileSizeProcessMonitor implements Runnable {
  public static final String rcsid = "$Id$";
  ApplicationManager applicationManager;
  AxisID axisID;
  long processStartTime;
  File watchedFile;
  long nBytes;

  int updatePeriod = 200;

  public FileSizeProcessMonitor(ApplicationManager appMgr, AxisID id) {
    applicationManager = appMgr;
    axisID = id;

  }

  // The dervied class must implement this function to 
  // - set the expected number of bytes in the output file
  // - initialize the progress bar through the application manager, the maximum
  //   value should be the expected size of the file in k bytes
  // - set the watchedFile reference to the output file being monitored.
  abstract long calcFileSize();

  /* (non-Javadoc)
   * @see java.lang.Runnable#run()
   */
  public void run() {
    //  Calculate the expected file size in bytes, initialize the progress bar
    // and set the File object.
    calcFileSize();

    //  Wait for the output file to be created and set the process start time
    try {
      waitForFile();
    }
    //  Interrupted ???  kill the thread by exiting
    catch (InterruptedException e) {
      return;
    }
    
    updateProgressBar();
  }

  /**
   * Wait for the new output file to be created.  Make sure it is current by
   * comparing the modification time of the file to the start time of this
   * function. Set the process start time to the first new file modification
   * time since we don't have access to the file creation time.  
   */
  void waitForFile() throws InterruptedException {
    long startTime = System.currentTimeMillis();
    long modTime;
    boolean newOutputFile = false;
    while (!newOutputFile) {
      if (watchedFile.exists()) {
        modTime = watchedFile.lastModified();
        if (modTime > startTime) {
          processStartTime = modTime;
          newOutputFile = true;
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
      long currentLength = watchedFile.length();
      double fractionDone = (double) currentLength / nBytes;
      int percentage = (int) Math.round(fractionDone * 100);

      long elapsedTime = System.currentTimeMillis() - processStartTime;
      double remainingTime = elapsedTime / fractionDone - elapsedTime;
      int minutes = (int) remainingTime / 60000;
      int seconds =
        (int) Math.round((remainingTime - minutes * 60000) / 1000.0);

      String message =
        String.valueOf(percentage)
          + "%   ETC: "
          + String.valueOf(minutes)
          + ":";
      if (seconds < 10) {
        message = message + "0" + String.valueOf(seconds);
      }
      else {
        message = message + String.valueOf(seconds);
      }
      applicationManager.setProgressBarValue(
        (int) (currentLength / 1024),
        message,
        axisID);

      //  TODO: need to put a fail safe in here to
      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException exceptio) {
        fileWriting = false;
      }
    }
  }
}