package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$$ </p>
 */

public class VolcombineProcessMonitor extends LogFileProcessMonitor {

  public static final String rcsid = "$$Id$$";

  /**
   * Default constructor
  * @param appMgr
   * @param id
   */
  public VolcombineProcessMonitor(
    ApplicationManager appMgr,
    AxisID id) {

    super(appMgr, id);
    logFileBasename = "volcombine";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#initializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.setProgressBar("Combine: volcombine", 1, axisID);
      applicationManager.setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.setProgressBar("Combine: volcombine", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.startsWith("STATUS: EXTRACTING AND COMBINING")) {
        String[] fields = line.split("\\s+");
        currentSection = parseFields(fields, 5, currentSection);
        System.out.println("currentSection:" + currentSection);
      }
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
      while ((line = logFileReader.readLine()) != null && !foundNSections) {
        if (line.startsWith("STATUS: EXTRACTING AND COMBINING")) {
          String[] fields = line.split("\\s+");
          nSections = parseFields(fields, 7, nSections);
          if (nSections != Integer.MIN_VALUE) {
            foundNSections = true;
            System.out.println("nSections:" + nSections);
          } else {
            throw new NumberFormatException("Unable to read first STATUS: EXTRACTING AND COMBINING line");
          }
          currentSection = parseFields(fields, 5, currentSection);
          System.out.println("currentSection:" + currentSection);
        }
      }
    }
  }

  private int parseFields(String[] fields, int location, int oldValue) {
    if (fields.length > location) {
      return Integer.parseInt(fields[location]);
    }
    return oldValue;
  }
}
