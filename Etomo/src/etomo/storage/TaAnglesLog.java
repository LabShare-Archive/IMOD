package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.process.AlignLogGenerator;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

/**
 * <p>Description: Class for reading the tasurfaceangles.log file.</p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 1.4  2010/03/19 21:59:34  sueh
 * <p> bug# 1335 Class can't be a n'ton because the dataset tabs.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2009/09/25 22:22:47  sueh
 * <p> bug# 1272 In get... handle line == null.
 * <p>
 * <p> Revision 1.1  2009/09/01 03:18:06  sueh
 * <p> bug# 1222
 * <p>
 */
public final class TaAnglesLog {
  public static final String rcsid = "$Id$";

  private static final String CENTER_TO_CENTER_THICKNESS_TAG = "Unbinned thickness needed to contain centers of all fiducials";

  private final String userDir;
  private final AxisID axisID;

  private TaAnglesLog(String userDir, AxisID axisID) {
    this.userDir = userDir;
    this.axisID = axisID;
  }

  /**
   * @param userDir
   * @param axisID
   * @param processName
   * @return
   */
  public static TaAnglesLog getInstance(String userDir, AxisID axisID) {
    return new TaAnglesLog(userDir, axisID);
  }

  /**
   * Get center to center thickness from the log.
   */
  public ConstEtomoNumber getCenterToCenterThickness() throws LogFile.LockException,
      FileNotFoundException, IOException {
    EtomoNumber centerToCenterThickness = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    //refresh the log file
    LogFile taAnglesLog = LogFile.getInstance(userDir, axisID,
        AlignLogGenerator.ANGLES_LOG_NAME);
    if (taAnglesLog.exists()) {
      LogFile.ReaderId readerId = taAnglesLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = taAnglesLog.readLine(readerId);
        while (line != null) {
          line = line.trim();
          if (line
              .startsWith("Unbinned thickness needed to contain centers of all fiducials")) {
            String[] stringArray = line.split("\\s+");
            centerToCenterThickness.set(stringArray[10]);
            taAnglesLog.closeRead(readerId);
            return centerToCenterThickness;
          }
          line = taAnglesLog.readLine(readerId);
        }
        taAnglesLog.closeRead(readerId);
      }
    }
    return centerToCenterThickness;
  }

  /**
   * Get incremental shift to center from the log.
   */
  public ConstEtomoNumber getIncrementalShiftToCenter() throws LogFile.LockException,
      FileNotFoundException, IOException {
    EtomoNumber incrementalShiftToCenter = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    //refresh the log file
    LogFile taAnglesLog = LogFile.getInstance(userDir, axisID,
        AlignLogGenerator.ANGLES_LOG_NAME);
    if (taAnglesLog.exists()) {
      LogFile.ReaderId readerId = taAnglesLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = taAnglesLog.readLine(readerId);
        while (line != null) {
          line = line.trim();
          if (line
              .startsWith("Incremental unbinned shift needed to center range of fiducials in Z")) {
            String[] stringArray = line.split("\\s+");
            incrementalShiftToCenter.set(stringArray[12]);
            taAnglesLog.closeRead(readerId);
            return incrementalShiftToCenter;
          }
          line = taAnglesLog.readLine(readerId);
        }
        taAnglesLog.closeRead(readerId);
      }
    }
    return incrementalShiftToCenter;
  }
}
