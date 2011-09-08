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
 * <p> $Revision 1.12  2011/02/22 04:06:39  sueh
 * <p> $bug# 1437 Reformatting.
 * <p> $
 * <p> $Revision 1.11  2010/03/03 04:55:35  sueh
 * <p> $bug# 1311 Removed unnecessary ProcessName references.
 * <p> $
 * <p> $Revision 1.10  2010/02/17 04:49:20  sueh
 * <p> $bug# 1301 Using the manager instead of the manager key do pop up
 * <p> $messages.
 * <p> $
 * <p> $Revision 1.9  2009/06/05 01:59:21  sueh
 * <p> $bug# 1219 Kept up with changes in parent class.
 * <p> $
 * <p> $Revision 1.8  2009/02/04 23:26:43  sueh
 * <p> $bug# 1158 Changed id and exceptions classes in LogFile.
 * <p> $
 * <p> $Revision 1.7  2006/10/24 21:28:18  sueh
 * <p> $bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p> $
 * <p> $Revision 1.6  2006/10/16 22:43:06  sueh
 * <p> $bug# 933 Added ProcessResultDisplay to ApplicationManager.postProcess().
 * <p> $
 * <p> $Revision 1.5  2006/10/10 05:11:15  sueh
 * <p> $bug# 931 Managing the log file with LogFile.
 * <p> $
 * <p> $Revision 1.4  2006/09/19 22:27:56  sueh
 * <p> $bug# 928 Add post processing for patchcorr to the watcher because patchcorr is
 * <p> $run independently by combine and doesn't return to ProcessManager.
 * <p> $
 * <p> $Revision 1.3  2004/11/19 23:24:00  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
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
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

public class PatchcorrProcessWatcher extends LogFileProcessMonitor {
  public static final String rcsid = "$$Id$$";

  private String lastLineRead = null;

  private final ApplicationManager applicationManager;

  /**
   * Construct a xcorr process watcher
   * @param appMgr
   * @param id
   */
  public PatchcorrProcessWatcher(ApplicationManager manager, AxisID id) {
    super(manager, id);
    applicationManager = manager;
    standardLogFileName = false;
    logFileBasename = DatasetFiles.PATCH_OUT;
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar("Combine: patchcorr", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar("Combine: patchcorr", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      if (!line.trim().endsWith("positions")) {
        currentSection++;
      }
      lastLineRead = line;
    }
    if (currentSection >= nSections) {
      ending = true;
    }
  }

  /**
   * Search patch.out file for the number of positions
   */
  void findNSections() throws InterruptedException, NumberFormatException, IOException,
      LogFile.LockException {

    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(UPDATE_PERIOD);
      String line = readLogFileLine();
      if (line != null && line.trim().endsWith("positions")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[0]);
        foundNSections = true;
      }
    }
  }

  void postProcess() {
    applicationManager.postProcess(axisID, ProcessName.PATCHCORR, null, null);
  }
}