package etomo.process;

import etomo.ApplicationManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessName;

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
 * <p> $$Log$
 * <p> $Revision 1.6  2006/10/24 21:41:18  sueh
 * <p> $bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p> $
 * <p> $Revision 1.5  2006/10/10 05:14:48  sueh
 * <p> $bug# 931 Managing the log file with LogFile.
 * <p> $
 * <p> $Revision 1.4  2006/08/09 20:15:11  sueh
 * <p> $bug# 631 Checking for reassembing and filltomo.
 * <p> $
 * <p> $Revision 1.3  2004/11/19 23:26:21  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
 * <p> $Revision 1.2.4.1  2004/09/29 19:12:10  sueh
 * <p> $bug# 520 Removing pass-through function calls.
 * <p> $
 * <p> $Revision 1.2  2004/05/24 16:29:12  sueh
 * <p> $removing prints
 * <p> $
 * <p> $Revision 1.1  2004/05/21 02:20:26  sueh
 * <p> $bug# 83 volcombine process monitor uses the volcombine.log file
 * <p> $and the status lines which count which piece has been extracted.
 * <p> $findNSections() has been overridden.  It sets the first currentSection
 * <p> $because the currentSection value is on the same line as the
 * <p> $nSections value.
 * <p> $$ </p>
 */

public class VolcombineProcessMonitor extends LogFileProcessMonitor {

  public static final String rcsid = "$$Id$$";

  private boolean reassembling = false;
  private boolean filltomo = false;
  private boolean densmatch = false;

  /**
   * Default constructor
   * @param appMgr
   * @param id
   */
  public VolcombineProcessMonitor(ApplicationManager appMgr, AxisID id) {

    super(appMgr, id, ProcessName.VOLCOMBINE);
    logFileBasename = "volcombine";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#initializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Combine: volcombine",
          1, axisID, processName);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...",
          axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar("Combine: volcombine",
        nSections, axisID, processName);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection() throws NumberFormatException,
      LogFile.ReadException {
    String line;
    while ((line = readLogFileLine()) != null) {
      if (line.startsWith("STATUS:")) {
        if (!setSubprocess(line)&&line.indexOf("EXTRACTING AND COMBINING") != -1) {
          String[] fields = line.split("\\s+");
          currentSection = parseFields(fields, 5, currentSection);
        }
      }
    }
  }

  private boolean setSubprocess(String line) {
    if (line.indexOf("REASSEMBLING PIECES") != -1) {
      reassembling = true;
      filltomo = false;
      densmatch = false;
      return true;
    }
    if (line.indexOf("RUNNING FILLTOMO") != -1) {
      filltomo = true;
      reassembling = false;
      densmatch = false;
      return true;
    }
    if (line.indexOf("RUNNING DENSMATCH TO MATCH DENSITIES") != -1) {
      densmatch = true;
      filltomo = false;
      reassembling = false;
      return true;
    }
    densmatch = false;
    filltomo = false;
    reassembling = false;
    return false;
  }

  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  protected void findNSections() throws InterruptedException,
      LogFile.ReadException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;

    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(updatePeriod);
      String line;
      while ((line = readLogFileLine()) != null && !foundNSections) {
        if (line.startsWith("STATUS:")) {
          if (setSubprocess(line)) {
            updateProgressBar();
          }
          else if (line.indexOf("EXTRACTING AND COMBINING") != -1) {
            String[] fields = line.split("\\s+");
            nSections = parseFields(fields, 7, nSections);
            if (nSections != Integer.MIN_VALUE) {
              foundNSections = true;
            }
            else {
              throw new NumberFormatException(
                  "Unable to read first STATUS: EXTRACTING AND COMBINING line");
            }
            currentSection = parseFields(fields, 5, currentSection);
          }
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

  protected void updateProgressBar() {
    if (waitingForExit <= 0) {
      if (filltomo) {
        applicationManager.getMainPanel().setProgressBarValue(0, "filltomo",
            axisID);
      }
      else if (reassembling) {
        applicationManager.getMainPanel().setProgressBarValue(0,
            "Reassembling", axisID);
      }
      else if (densmatch) {
        applicationManager.getMainPanel().setProgressBarValue(0, "densmatch",
            axisID);
      }
      else {
        super.updateProgressBar();
      }
    }
    else {
      super.updateProgressBar();
    }
  }
}
