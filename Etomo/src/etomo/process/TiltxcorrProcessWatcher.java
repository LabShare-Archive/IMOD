package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class TiltxcorrProcessWatcher extends LogFileProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  String lastLineRead = null;
  boolean blendmontRan = false;
  
  /**
   * Construct an xcorr process watcher
   * @param appMgr
   * @param id
   */
  public TiltxcorrProcessWatcher(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
    logFileBasename = "xcorr";
  }

  /**
   * Construct an xcorr process watcher
   * @param appMgr
   * @param id
   * @param blendmontRan - True if blendmont output is in the log file prior to
   *        tiltxcorr output.
   */
  public TiltxcorrProcessWatcher(ApplicationManager appMgr, AxisID id, boolean blendmontRan) {
    super(appMgr, id);
    logFileBasename = "xcorr";
    this.blendmontRan = blendmontRan;
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Cross-correlating stack", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(
      "Cross-correlating stack",
      nSections,
      axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.startsWith("View")) {
        currentSection++;
      }
      lastLineRead = line;
    }

    if (lastLineRead != null
      && lastLineRead.trim().startsWith("PROGRAM EXECUTED TO END.")) {
      waitingForExit++;
    }
  }
  
  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  protected void findNSections() throws InterruptedException,
      NumberFormatException, IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;

    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(updatePeriod);
      String line;
      while ((line = logFileReader.readLine()) != null) {
        if (line.startsWith(" Number of columns, rows, sections")) {
          String[] fields = line.split("\\s+");
          if (fields.length > 9) {
            //Take the second header output if there is blendmont output in the
            //log file
            if (blendmontRan) {
              blendmontRan = false;
            }
            else {
              nSections = Integer.parseInt(fields[9]);
              foundNSections = true;
            }
            break;
          }
          else {
            throw new NumberFormatException("Incomplete size line in header");
          }
        }
      }
    }
  }
}
/**
* <p> $Log$ </p>
*/
