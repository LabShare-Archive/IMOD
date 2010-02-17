package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.type.AxisID;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.1  2009/02/04 23:29:21  sueh
 * <p> bug# 1158 Class representing the track.log file.  Used to send entries to
 * <p> LogPanel.
 * <p> </p>
 */

public final class TrackLog implements Loggable {
  public static final String rcsid = "$Id$";

  private static TrackLog INSTANCE_A = null;
  private static TrackLog INSTANCE_B = null;

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private TrackLog(String userDir, AxisID axisID) {
    this.userDir = userDir;
    this.axisID = axisID;
  }

  /**
   * Gets either INSTANCE_A or INSTANCE_B depending on the axisID.
   * @param userDir
   * @param axisID
   * @param processName
   * @return
   */
  public static TrackLog getInstance(String userDir, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (INSTANCE_B == null) {
        INSTANCE_B = new TrackLog(userDir, axisID);
      }
      return INSTANCE_B;
    }
    if (INSTANCE_A == null) {
      INSTANCE_A = new TrackLog(userDir, axisID);
    }
    return INSTANCE_A;
  }

  public String getName() {
    return ProcessName.TRACK.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException,
      FileNotFoundException, IOException {
    lineList.clear();
    //refresh the log file
    LogFile trackLog = LogFile.getInstance(userDir, axisID, ProcessName.TRACK);
    if (trackLog.exists()) {
      LogFile.ReaderId readerId = trackLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = trackLog.readLine(readerId);
        while (line != null) {
          if (line.trim().startsWith("Total points missing")) {
            lineList.add(line);
            trackLog.closeReader(readerId);
            return lineList;
          }
          line = trackLog.readLine(readerId);
        }
        trackLog.closeReader(readerId);
      }
    }
    return lineList;
  }
}
