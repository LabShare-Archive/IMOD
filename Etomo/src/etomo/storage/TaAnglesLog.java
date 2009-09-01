package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.ManagerKey;
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
 */
public final class TaAnglesLog {
  public static final String rcsid = "$Id$";

  private static TaAnglesLog INSTANCE_A = null;
  private static TaAnglesLog INSTANCE_B = null;

  private static final String CENTER_TO_CENTER_THICKNESS_TAG = "Unbinned thickness needed to contain centers of all fiducials";

  private final String userDir;
  private final AxisID axisID;

  private TaAnglesLog(String userDir, AxisID axisID) {
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
  public static TaAnglesLog getInstance(String userDir, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (INSTANCE_B == null) {
        INSTANCE_B = new TaAnglesLog(userDir, axisID);
      }
      return INSTANCE_B;
    }
    if (INSTANCE_A == null) {
      INSTANCE_A = new TaAnglesLog(userDir, axisID);
    }
    return INSTANCE_A;
  }

  /**
   * Get center to center thickness from the log.
   */
  public ConstEtomoNumber getCenterToCenterThickness(ManagerKey managerKey)
      throws LogFile.LockException, FileNotFoundException, IOException {
    EtomoNumber centerToCenterThickness = new EtomoNumber(
        EtomoNumber.Type.FLOAT);
    //refresh the log file
    LogFile taAnglesLog = LogFile.getInstance(userDir, axisID,
        AlignLogGenerator.ANGLES_LOG_NAME, managerKey);
    if (taAnglesLog.exists()) {
      LogFile.ReaderId readerId = taAnglesLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = taAnglesLog.readLine(readerId).trim();
        while (line != null) {
          if (line
              .startsWith("Unbinned thickness needed to contain centers of all fiducials")) {
            String[] stringArray = line.split("\\s+");
            centerToCenterThickness.set(stringArray[10]);
            taAnglesLog.closeReader(readerId);
            return centerToCenterThickness;
          }
          line = taAnglesLog.readLine(readerId).trim();
        }
        taAnglesLog.closeReader(readerId);
      }
    }
    return centerToCenterThickness;
  }
  
  /**
   * Get incremental shift to center from the log.
   */
  public ConstEtomoNumber getIncrementalShiftToCenter(ManagerKey managerKey)
      throws LogFile.LockException, FileNotFoundException, IOException {
    EtomoNumber incrementalShiftToCenter = new EtomoNumber(
        EtomoNumber.Type.FLOAT);
    //refresh the log file
    LogFile taAnglesLog = LogFile.getInstance(userDir, axisID,
        AlignLogGenerator.ANGLES_LOG_NAME, managerKey);
    if (taAnglesLog.exists()) {
      LogFile.ReaderId readerId = taAnglesLog.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = taAnglesLog.readLine(readerId).trim();
        while (line != null) {
          if (line
              .startsWith("Incremental unbinned shift needed to center range of fiducials in Z")) {
            String[] stringArray = line.split("\\s+");
            incrementalShiftToCenter.set(stringArray[12]);
            taAnglesLog.closeReader(readerId);
            return incrementalShiftToCenter;
          }
          line = taAnglesLog.readLine(readerId).trim();
        }
        taAnglesLog.closeReader(readerId);
      }
    }
    return incrementalShiftToCenter;
  }
}
