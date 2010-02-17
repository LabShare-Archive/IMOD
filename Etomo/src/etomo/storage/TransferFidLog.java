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
 * <p> Revision 3.1  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p> </p>
 */
public final class TransferFidLog implements Loggable {
  public static final String rcsid = "$Id$";

  private static TransferFidLog INSTANCE_A = null;
  private static TransferFidLog INSTANCE_B = null;

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private TransferFidLog(String userDir, AxisID axisID) {
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
  public static TransferFidLog getInstance(String userDir, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (INSTANCE_B == null) {
        INSTANCE_B = new TransferFidLog(userDir, axisID);
      }
      return INSTANCE_B;
    }
    if (INSTANCE_A == null) {
      INSTANCE_A = new TransferFidLog(userDir, axisID);
    }
    return INSTANCE_A;
  }

  public String getName() {
    return ProcessName.TRANSFERFID.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException,
      FileNotFoundException, IOException {
    lineList.clear();
    //refresh the log file
    LogFile logFile = LogFile.getInstance(userDir,
        DatasetFiles.TRANSFER_FID_LOG);
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
        logFile.closeReader(readerId);
      }
    }
    return lineList;
  }
}
