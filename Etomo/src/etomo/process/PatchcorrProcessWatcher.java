package etomo.process;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.2.4.1  2004/09/29 19:09:27  sueh
 * <p> $bug# 520 Removing pass-through function calls.
 * <p> $
 * <p> $Revision 1.2  2004/04/23 20:02:53  sueh
 * <p> $bug# 83 allowing initializeProgressBar() to be called before
 * <p> $nSections is set
 * <p> $
 * <p> $Revision 1.1  2004/03/22 23:45:55  sueh
 * <p> $bug# 83 process watcher for patchcorr.  Watches patch.out.  Nsections is set to
 * <p> $the total number of positions.
 * <p> $$ </p>
 */

import java.io.IOException;
import etomo.ApplicationManager;
import etomo.type.AxisID;

public class PatchcorrProcessWatcher extends LogFileProcessMonitor {
  public static final String rcsid = "$$Id$$";
  String lastLineRead = null;
  /**
   * Construct a xcorr process watcher
   * @param appMgr
   * @param id
   */
  public PatchcorrProcessWatcher(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
    standardLogFileName = false;
    logFileBasename = "patch.out";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Combine: patchcorr", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(
      "Combine: patchcorr",
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
      if (!line.trim().endsWith("positions")) {
        currentSection++;
      }
      lastLineRead = line;
    }
    if (currentSection >= nSections) {
      waitingForExit++;
    }
  }

  /**
   * Search patch.out file for the number of positions
   */
  protected void findNSections()
    throws InterruptedException, NumberFormatException, IOException {

    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(updatePeriod);
      String line = logFileReader.readLine();
      if (line != null && line.trim().endsWith("positions")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[0]);
        foundNSections = true;
      }
    }
  }

}