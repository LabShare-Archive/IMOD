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

  public XcorrProcessWatcher(
    SystemProcessInterface process,
    ApplicationManager appMgr,
    AxisID id) {
    systemProcess = process;
    applicationManager = appMgr;
    axisID = id;
  }

  public void run() {
    while(! systemProcess.isStarted()) {
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException excption) {
        return;
      }
      
    }
    //  Wait for the process to start signified by the existance of the log file
    File logFile =
      new File(
        System.getProperty(
          "user.dir"),
          "xcorr" + axisID.getExtension() + ".log");

    while (!logFile.exists()) {
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException excption) {
        return;
      }
    }

    //  Open the log file
    BufferedReader fileBuffer;
    try {
      fileBuffer = new BufferedReader(new FileReader(logFile));
    }
    catch (FileNotFoundException e1) {
      return;
    }

    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    String line;
    try {
      while (!foundNSections) {

        Thread.sleep(100);
        while ((line = fileBuffer.readLine()) != null) {
          if (line.startsWith(" Number of columns, rows, sections")) {
            String[] fields = line.split("\\s+");
            if (fields.length > 9) {
             
              int nSections = Integer.parseInt(fields[9]);
               System.err.println(nSections);
              applicationManager.setProgressBar(
                "Creating coarse stack",
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
          System.err.println(line);
          if (line.startsWith("View")) {
            String[] fields = line.split("\\s+");
            if (fields.length > 1) {
              System.err.println (fields[1]);
              String number = fields[1];
              int idxSection = Integer.parseInt(number.substring(0,number.length()-1));
              System.err.println(idxSection);
              applicationManager.setProgressBarValue(idxSection, axisID);
              
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
}