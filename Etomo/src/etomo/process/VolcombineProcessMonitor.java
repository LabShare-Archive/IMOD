package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.storage.LogFile;
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
 * <p> $$Log$
 * <p> $Revision 1.13  2011/02/22 04:13:07  sueh
 * <p> $bug# 1437 Reformatting.
 * <p> $
 * <p> $Revision 1.12  2010/03/03 04:55:35  sueh
 * <p> $bug# 1311 Removed unnecessary ProcessName references.
 * <p> $
 * <p> $Revision 1.11  2010/02/17 04:49:20  sueh
 * <p> $bug# 1301 Using the manager instead of the manager key do pop up
 * <p> $messages.
 * <p> $
 * <p> $Revision 1.10  2009/06/05 02:00:04  sueh
 * <p> $bug# 1219 Kept up with changes in parent class.
 * <p> $
 * <p> $Revision 1.9  2009/02/04 23:26:53  sueh
 * <p> $bug# 1158 Changed id and exceptions classes in LogFile.
 * <p> $
 * <p> $Revision 1.8  2006/12/02 04:58:08  sueh
 * <p> $bug# 944 Made subprocess static so it could be used by
 * <p> $ProcesschunksVolcombineMonitor.
 * <p> $
 * <p> $Revision 1.7  2006/11/29 20:17:10  sueh
 * <p> $bug# 944 Added display for densmatch.
 * <p> $
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

  private Subprocess subprocess = new Subprocess();

  /**
   * Default constructor
   * @param appMgr
   * @param id
   */
  public VolcombineProcessMonitor(BaseManager manager, AxisID id) {
    super(manager, id);
    logFileBasename = "volcombine";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#initializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar("Combine: volcombine", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar("Combine: volcombine", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      if (line.startsWith("STATUS:")) {
        if (!setSubprocess(line, subprocess)
            && line.indexOf("EXTRACTING AND COMBINING") != -1) {
          String[] fields = line.split("\\s+");
          currentSection = parseFields(fields, 5, currentSection);
        }
      }
    }
  }

  /**
   * Sets the subprocess.  Returns the subprocess.  Returns null if there is no
   * current subprocess.
   * @param line
   * @return
   */
  static boolean setSubprocess(String line, Subprocess subprocess) {
    if (line.indexOf("REASSEMBLING PIECES") != -1) {
      subprocess.setReassembling();
      return true;
    }
    if (line.indexOf("RUNNING FILLTOMO") != -1) {
      subprocess.setFilltomo();
      return true;
    }
    if (line.indexOf("RUNNING DENSMATCH TO MATCH DENSITIES") != -1) {
      subprocess.setDensmatch();
      return true;
    }
    subprocess.reset();
    return false;
  }

  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  protected void findNSections() throws InterruptedException, LogFile.LockException,
      IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;

    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(UPDATE_PERIOD);
      String line;
      while ((line = readLogFileLine()) != null && !foundNSections) {
        if (line.startsWith("STATUS:")) {
          if (setSubprocess(line, subprocess)) {
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
    if (!ending) {
      if (subprocess.isFilltomo()) {
        manager.getMainPanel().setProgressBarValue(0, "Filltomo", axisID);
      }
      else if (subprocess.isReassembling()) {
        manager.getMainPanel().setProgressBarValue(0, "Reassembling", axisID);
      }
      else if (subprocess.isDensmatch()) {
        manager.getMainPanel().setProgressBarValue(0, "Densmatch", axisID);
      }
      else {
        super.updateProgressBar();
      }
    }
    else {
      super.updateProgressBar();
    }
  }

  static final class Subprocess {
    private boolean reassembling = false;
    private boolean filltomo = false;
    private boolean densmatch = false;

    Subprocess() {
    }

    void setReassembling() {
      reassembling = true;
      filltomo = false;
      densmatch = false;
    }

    void setFilltomo() {
      filltomo = true;
      reassembling = false;
      densmatch = false;
    }

    void setDensmatch() {
      densmatch = true;
      filltomo = false;
      reassembling = false;
    }

    void reset() {
      densmatch = false;
      filltomo = false;
      reassembling = false;
    }

    boolean isFilltomo() {
      return filltomo;
    }

    boolean isDensmatch() {
      return densmatch;
    }

    boolean isReassembling() {
      return reassembling;
    }
  }
}
