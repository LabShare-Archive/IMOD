package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

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

public abstract class LogFileProcessMonitor implements Runnable {
  public static final String rcsid = "$Id$";
  protected SystemProcessInterface process;
  protected ApplicationManager applicationManager;
  protected AxisID axisID;
  protected long processStartTime;
  protected BufferedReader logFileBuffer;
  protected int nSections;
  protected int currentSection;
  protected int remainingTime;

  protected int updatePeriod = 500;

  //  This needs to be set in the concrete class constructor
  protected String logFileBasename;

  protected abstract void initializeProgressBar();
  protected abstract void getCurrentSection()
    throws NumberFormatException, IOException;

  /**
   * Default constructor
   * @param appMgr  The application manager object
   * @param id  The axis ID to be monitored
   */
  public LogFileProcessMonitor(
    SystemProcessInterface proc,
    ApplicationManager appMgr,
    AxisID id) {
    process = proc;
    applicationManager = appMgr;
    axisID = id;
  }

  public void run() {
    try {
      //  Wait for the log file to exist
      waitForLogFile(logFileBasename);
      System.err.println("Log file detected");
      findNSections();
      System.err.println("Number of sections: " + String.valueOf(nSections));

      initializeProgressBar();

      while (!process.isDone()) {
        Thread.sleep(updatePeriod);
        getCurrentSection();
        System.err.println(
          "Current section: " + String.valueOf(currentSection));
        calcRemainingTime();
        updateProgressBar();
      }

    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }

    applicationManager.progressBarDone(axisID);

  }

  /**
   * Wait for the process to start and the appropriate log file to be created 
   * @param logFileBasename
   * @return a buffered reader of the log file
   */
  private void waitForLogFile(String logFileBasename)
    throws InterruptedException, FileNotFoundException {

    processStartTime = System.currentTimeMillis();

    // Instantiate a new log file object
    File logFile =
      new File(
        System.getProperty("user.dir"),
        logFileBasename + axisID.getExtension() + ".log");

    boolean newLogFile = false;
    while (!newLogFile) {
      // Check to see if the log file exists that signifies that the process
      // has started
      if (logFile.exists()) {
        newLogFile = true;
      }
      else {
        Thread.sleep(updatePeriod);
      }
    }
    //  Open the log file
    logFileBuffer = new BufferedReader(new FileReader(logFile));
  }

  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  private void findNSections()
    throws InterruptedException, NumberFormatException, IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    String line;
    int nSections = -1;
    while (!foundNSections) {
      Thread.sleep(updatePeriod);
      while ((line = logFileBuffer.readLine()) != null) {
        if (line.startsWith(" Number of columns, rows, sections")) {
          String[] fields = line.split("\\s+");
          if (fields.length > 9) {
            nSections = Integer.parseInt(fields[9]);
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
   * Update the progress bar with percentage done and estimated time to
   * completion message. 
   * @param percentage
   * @param remainingTime
   */
  private void updateProgressBar() {
    double fractionDone = (double) currentSection / nSections;
    int percentage = (int) Math.round(fractionDone * 100);
    int minutes = (int) Math.floor(remainingTime / 60000);
    int seconds = (int) Math.floor((remainingTime - minutes * 60000) / 1000.0);

    String message =
      String.valueOf(percentage) + "%   ETC: " + String.valueOf(minutes) + ":";
    if (seconds < 10) {
      message = message + "0" + String.valueOf(seconds);
    }
    else {
      message = message + String.valueOf(seconds);
    }

    applicationManager.setProgressBarValue(currentSection, message, axisID);
  }

  /**
   * Calculate the amount of time remaining, this can be overridden if the
   * derived class has a better way to calculate.
   *
   */
  private void calcRemainingTime() {
    double fractionDone = (double) currentSection / nSections;
    long elapsedTime = System.currentTimeMillis() - processStartTime;
    double remainingTime = elapsedTime / fractionDone - elapsedTime;
  }
}
