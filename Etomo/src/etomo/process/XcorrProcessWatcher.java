/*
 * Created on May 22, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;

public class XcorrProcessWatcher implements Runnable {
  SystemProcessInterface systemProcess;
  ApplicationManager applicationManager;
  AxisID axisID;
  long processStartTime;
  BufferedReader fileBuffer;

  /**
   * Construct a proce
   * @param process
   * @param appMgr
   * @param id
   */
  public XcorrProcessWatcher(
    SystemProcessInterface process,
    ApplicationManager appMgr,
    AxisID id) {
    systemProcess = process;
    applicationManager = appMgr;
    axisID = id;
  }

  public void run() {
  
    fileBuffer = waitForLogFile("xcorr");
    
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    String line;
    try {
      int nSections = -1;
      while (!foundNSections) {
        Thread.sleep(100);
        while ((line = fileBuffer.readLine()) != null) {
          if (line.startsWith(" Number of columns, rows, sections")) {
            String[] fields = line.split("\\s+");
            if (fields.length > 9) {
              nSections = Integer.parseInt(fields[9]);
              applicationManager.setProgressBar(
                "Cross-correlating stack",
                nSections,
                axisID);
              foundNSections = true;
              break;
            }
          }
        }
      }

      while (!systemProcess.isDone()) {
        Thread.sleep(100);
        while ((line = fileBuffer.readLine()) != null) {
          if (line.startsWith("View")) {
            String[] fields = line.split("\\s+");
            if (fields.length > 1) {
              String number = fields[1];
              int idxSection =
                Integer.parseInt(number.substring(0, number.length() - 1));

              double fractionDone = (double) idxSection / nSections;
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
                idxSection,
                message,
                axisID);

            }
          }
        }
      }
      applicationManager.progressBarDone(axisID);
    }
    catch (InterruptedException e) {
      applicationManager.progressBarDone(axisID);
    }
    catch (NumberFormatException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }
  
  /**
   * Wait for the process to start and the appropriate log file to be created 
   * @param logFileBasename
   * @return a buffered reader of the log file
   */
  private BufferedReader waitForLogFile(String logFileBasename) {
    try {

      //  Wait for the process to start
      while (!systemProcess.isStarted()) {
        Thread.sleep(100);
      }

      processStartTime = System.currentTimeMillis();

      //  Don't do anything until the (possibly new) logfile is created
      File logFile = null;
      boolean newLogFile = false;
      while (!newLogFile) {
        // Instantiate a new log file object
        logFile =
          new File(
            System.getProperty("user.dir"),
            logFileBasename + axisID.getExtension() + ".log");

        // Check to see if the log file exists and if it recent enough
        // Since this thread and the shell thread are asynchronous we want to
        // make sure that processStartTime estimate is not greater than the log
        // log file modification time by 5 second
        if (logFile.exists()) {
          if (processStartTime - logFile.lastModified() > 5000) {
            Thread.sleep(100);
          }
          else {
            newLogFile = true;
          }
        }
      }
      //  Open the log file
      fileBuffer = new BufferedReader(new FileReader(logFile));
    }
    catch (InterruptedException excption) {
      return null;
    }
    catch (FileNotFoundException e1) {
      return null;
    }
    return fileBuffer;
  }
}