package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.util.InvalidParameterException;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.FileType;
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
 * <p> Revision 1.9  2011/02/22 04:04:49  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.8  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.7  2010/02/17 04:48:40  sueh
 * <p> bug# 1301 Added the option to pass in fileType when creating the
 * <p> flattening instance.
 * <p>
 * <p> Revision 1.6  2009/06/05 01:58:56  sueh
 * <p> bug# 1219 Added getFlattenInstance.
 * <p>
 * <p> Revision 1.5  2009/02/04 23:26:24  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
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
final class Matchvol1ProcessMonitor extends LogFileProcessMonitor {
  public static final String rcsid = "$Id$";

  private final boolean calledFromFlatten;

  private String lastLineRead = null;

  /**
   * Construct a matchvol1 process watcher
   * @param appMgr
   * @param id
   */
  private Matchvol1ProcessMonitor(final BaseManager manager, final AxisID id,
      final boolean calledFromFlatten, final FileType fileType) {
    super(manager, id);
    this.calledFromFlatten = calledFromFlatten;
    if (calledFromFlatten) {
      if (fileType == null) {
        logFileBasename = ProcessName.FLATTEN.toString();
      }
      else {
        logFileBasename = fileType.getRoot(manager, id);
      }
    }
    else {
      logFileBasename = "matchvol1";
    }
  }

  Matchvol1ProcessMonitor(final BaseManager manager, final AxisID id) {
    this(manager, id, false, null);
  }

  static Matchvol1ProcessMonitor getFlattenInstance(final BaseManager manager,
      final AxisID id, final FileType fileType) {
    return new Matchvol1ProcessMonitor(manager, id, true, fileType);
  }

  /**
   * Sets the title and the number of steps of the progress bar.
   */
  void initializeProgressBar() {
    String title;
    if (calledFromFlatten) {
      title = ProcessName.FLATTEN.toString();
    }
    else {
      title = "Combine: matchvol1";
    }
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar(title, 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar(title, nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
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
      LogFile.LockException, InvalidParameterException, IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    Thread.sleep(UPDATE_PERIOD);
    while (!foundNSections) {
      String line = readLogFileLine();
      if (line == null) {
        Thread.sleep(UPDATE_PERIOD);
      }
      if (line != null && line.trim().startsWith("Finished")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[3]);
        foundNSections = true;
      }
    }
    Thread.sleep(UPDATE_PERIOD);
  }
}
