package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.util.InvalidParameterException;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessName;

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
 * <p> Revision 1.4  2006/10/24 21:20:09  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.3  2006/10/10 05:10:34  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.2  2006/01/06 22:28:38  sueh
 * <p> bug# 794 Fixed findNSections().  It was sleeping each time it read a line.
 * <p> Now it sleeps when there are no lines to read.
 * <p>
 * <p> Revision 1.1  2005/01/26 23:41:56  sueh
 * <p> bug# 83 Matchvol1 process monitor.
 * <p> </p>
 */
public class Matchvol1ProcessMonitor extends LogFileProcessMonitor {
  public static final String rcsid = "$Id$";

  String lastLineRead = null;

  /**
   * Construct a matchvol1 process watcher
   * @param appMgr
   * @param id
   */
  public Matchvol1ProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id,ProcessName.MATCHVOL1);
    logFileBasename = "matchvol1";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Combine: matchvol1", 1,
          axisID,processName);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...",
          axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar("Combine: matchvol1",
        nSections, axisID,processName);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection() throws NumberFormatException,
      LogFile.LockException,IOException {
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
      waitingForExit++;
    }
  }

  /**
   * Search matchvol1.log.out file for the number of positions
   */
  protected void findNSections() throws InterruptedException,
      NumberFormatException, LogFile.LockException, InvalidParameterException,IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    Thread.sleep(updatePeriod);
    while (!foundNSections) {
      String line = readLogFileLine();
      if (line == null) {
        Thread.sleep(updatePeriod);
      }
      if (line != null && line.trim().startsWith("Finished")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[3]);
        foundNSections = true;
      }
    }
    Thread.sleep(updatePeriod);
  }
}
