package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

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
 * <p> Revision 3.3  2010/03/19 22:00:09  sueh
 * <p> bug# 1335 Class can't be a n'ton because of the dataset tabs.
 * <p>
 * <p> Revision 3.2  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.1  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p> </p>
 */
public final class TransferFidLog implements Loggable {
  public static final String rcsid = "$Id$";

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private TransferFidLog(String userDir, AxisID axisID) {
    this.userDir = userDir;
    this.axisID = axisID;
  }

  /**
   * @param userDir
   * @param axisID
   * @param processName
   * @return
   */
  public static TransferFidLog getInstance(String userDir, AxisID axisID) {
    return new TransferFidLog(userDir, axisID);
  }

  public String getName() {
    return ProcessName.TRANSFERFID.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    lineList.clear();
    //refresh the log file
    LogFile logFile = LogFile.getInstance(userDir, DatasetFiles.TRANSFER_FID_LOG);
    if (logFile.exists()) {
      LogFile.ReaderId readerId = logFile.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = logFile.readLine(readerId);
        while (line != null) {
          if (line.trim().startsWith("Points in")) {
            lineList.add(line);
          }
          line = logFile.readLine(readerId);
        }
        logFile.closeRead(readerId);
      }
    }
    return lineList;
  }
}
