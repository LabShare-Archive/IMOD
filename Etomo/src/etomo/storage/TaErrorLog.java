package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.process.AlignLogGenerator;
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
 * <p> Revision 1.4  2009/10/19 15:24:05  sueh
 * <p> bug# 1247 In getLogMessage added the global ratio.
 * <p>
 * <p> Revision 1.3  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.2  2009/02/19 18:20:23  sueh
 * <p> bug# 1179 Excluding local area output lines.
 * <p>
 * <p> Revision 1.1  2009/02/04 23:28:52  sueh
 * <p> bug# 1158 Class representing the taError.log file.  Used to send entries to
 * <p> LogPanel.
 * <p> </p>
 */
public final class TaErrorLog implements Loggable {
  public static final String rcsid = "$Id$";

  private static TaErrorLog INSTANCE_A = null;
  private static TaErrorLog INSTANCE_B = null;

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private TaErrorLog(String userDir, AxisID axisID) {
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
  public static TaErrorLog getInstance(String userDir, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (INSTANCE_B == null) {
        INSTANCE_B = new TaErrorLog(userDir, axisID);
      }
      return INSTANCE_B;
    }
    if (INSTANCE_A == null) {
      INSTANCE_A = new TaErrorLog(userDir, axisID);
    }
    return INSTANCE_A;
  }

  public String getName() {
    return ProcessName.ALIGN.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException,
      FileNotFoundException, IOException {
    lineList.clear();
    //refresh the log file
    LogFile taErrorLog = LogFile.getInstance(userDir, axisID,
        AlignLogGenerator.ERROR_LOG_NAME);
    if (taErrorLog.exists()) {
      LogFile.ReaderId readerId = taErrorLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = taErrorLog.readLine(readerId);
        boolean globalRatioFound = false;
        while (line != null) {
          if (!globalRatioFound
              && line.trim().startsWith(
                  "Ratio of total measured values to all unknowns")) {
            globalRatioFound = true;
            lineList.add(line);
          }
          else if (line.trim().startsWith("Residual error mean and sd")
              && line.indexOf("Local area") == -1) {
            lineList.add(line);
          }
          else if (line.trim().startsWith("Residual error local mean")) {
            lineList.add(line);
          }
          line = taErrorLog.readLine(readerId);
        }
        taErrorLog.closeReader(readerId);
      }
    }
    return lineList;
  }
}
