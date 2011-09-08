package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.storage.LogFile;
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
 * 
 * <p> $Log$
 * <p> Revision 1.10  2011/02/22 04:04:37  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.9  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.8  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.7  2009/06/05 01:58:11  sueh
 * <p> bug# 1219 Kept up with changes in parent class.
 * <p>
 * <p> Revision 1.6  2009/02/04 23:26:15  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.5  2006/10/24 21:19:49  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.4  2006/10/10 05:10:17  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.3  2006/09/19 22:26:08  sueh
 * <p> bug# 928 Find the line that announces that patch_vector.mod has been created.
 * <p>
 * <p> Revision 1.2  2006/01/06 22:18:06  sueh
 * <p> bug# 794 Fixed findNSections().  It was sleeping each time it read a line.
 * <p> Now it sleeps when there are no lines to read.
 * <p>
 * <p> Revision 1.1  2005/01/26 23:41:40  sueh
 * <p> bug# 83 Matchorwarp process monitor.
 * <p> </p>
 */
public final class MatchorwarpProcessMonitor extends LogFileProcessMonitor {
  public static final String rcsid = "$Id$";

  private String lastLineRead = null;

  private final ApplicationManager applicationManager;

  /**
   * Construct a matchvol1 process watcher
   * @param appMgr
   * @param id
   */
  public MatchorwarpProcessMonitor(final ApplicationManager manager, final AxisID id) {
    super(manager, id);
    applicationManager = manager;
    logFileBasename = "matchorwarp";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar() */
  void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar("Combine: matchorwarp", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar("Combine: matchorwarp", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection() */
  void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      line = line.trim();
      if (line.startsWith("Finished")) {
        String[] strings = line.split("\\s+");
        currentSection = Integer.parseInt(strings[1]);
      }
      lastLineRead = line;
    }
    if (currentSection >= nSections) {
      ending = true;
    }
  }

  /**
   * Search matchvol1.log.out file for the number of positions
   */
  void findNSections() throws InterruptedException, NumberFormatException,
      LogFile.LockException, IOException {
    // Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    Thread.sleep(UPDATE_PERIOD);
    while (!foundNSections) {
      String line = readLogFileLine();
      if (line == null) {
        Thread.sleep(UPDATE_PERIOD);
      }
      if (line != null) {
        if (line.trim().startsWith("Finished")) {
          line = line.trim();
          String[] strings = line.split("\\s+");
          nSections = Integer.parseInt(strings[3]);
          foundNSections = true;
        }
        else if (line.trim().startsWith("MATCHORWARP: CREATED patch_vector.mod")) {
          applicationManager.msgPatchVectorCreated();
        }
      }
    }
    Thread.sleep(UPDATE_PERIOD);
  }
}
