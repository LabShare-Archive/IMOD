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
 * <p> Revision 1.4  2010/03/19 22:00:01  sueh
 * <p> bug# 1335 Class can't be a n'ton because of the dataset tabs.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
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

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private TrackLog(String userDir, AxisID axisID) {
    this.userDir = userDir;
    this.axisID = axisID;
  }

  /**
   * @param userDir
   * @param axisID
   * @param processName
   * @return
   */
  public static TrackLog getInstance(String userDir, AxisID axisID) {
    return new TrackLog(userDir, axisID);
  }

  public String getName() {
    return ProcessName.TRACK.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
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
            trackLog.closeRead(readerId);
            return lineList;
          }
          line = trackLog.readLine(readerId);
        }
        trackLog.closeRead(readerId);
      }
    }
    return lineList;
  }
}
