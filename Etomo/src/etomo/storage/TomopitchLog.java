package etomo.storage;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class TomopitchLog {
  public static final String rcsid = "$Id$";

  private static final String WHITESPACE = "\\s+";
  private static final int ORIGINAL_LABEL_INDEX = 1;
  private static final String ORIGINAL_LABEL = "Original:";
  private static final int ADDED_LABEL_INDEX = 3;
  private static final String ADDED_LABEL = "Added:";
  private static final int TOTAL_LABEL_INDEX = 5;
  private static final String TOTAL_LABEL = "Total:";
  private static final String ANGLE_OFFSET_TAG = "Angle offset";
  private static final int ANGLE_OFFSET_OFFSET = ANGLE_OFFSET_TAG.split(WHITESPACE).length;
  private static final String AXIS_Z_SHIFT_TAG = "Z shift";
  private static final int AXIS_Z_SHIFT_OFFSET = AXIS_Z_SHIFT_TAG.split(WHITESPACE).length;
  private static final String X_AXIS_TILT_TAG = "X axis tilt";
  private static final int X_AXIS_TILT_OFFSET = X_AXIS_TILT_TAG.split(WHITESPACE).length;
  private static final String THICKNESS_TAG = "x-tilted";
  private static final String THICKNESS_LABEL = "to";
  private static final int THICKNESS_LABEL_INDEX = 12;

  private final BaseManager manager;
  private final AxisID axisID;

  private final EtomoNumber angleOffsetOriginal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber angleOffsetAdded = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber angleOffsetTotal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber axisZShiftOriginal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber axisZShiftAdded = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber axisZShiftTotal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber xAxisTiltOriginal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber xAxisTiltAdded = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber xAxisTiltTotal = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber thickness = new EtomoNumber();

  private File logFile = null;
  private boolean containsData = false;

  public TomopitchLog(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  private void read() {
    if (logFile != null) {
      return;
    }
    logFile = new File(manager.getPropertyUserDir(),
        DatasetFiles.getTomopitchLogFileName(manager, axisID));
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(logFile));
      String line;
      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.startsWith(ANGLE_OFFSET_TAG)) {
          String[] array = line.split(WHITESPACE);
          angleOffsetOriginal.set(get(array, ANGLE_OFFSET_OFFSET, ORIGINAL_LABEL_INDEX,
              ORIGINAL_LABEL));
          angleOffsetAdded.set(get(array, ANGLE_OFFSET_OFFSET, ADDED_LABEL_INDEX,
              ADDED_LABEL));
          angleOffsetTotal.set(get(array, ANGLE_OFFSET_OFFSET, TOTAL_LABEL_INDEX,
              TOTAL_LABEL));
        }
        else if (line.startsWith(AXIS_Z_SHIFT_TAG)) {
          String[] array = line.split(WHITESPACE);
          axisZShiftOriginal.set(get(array, AXIS_Z_SHIFT_OFFSET, ORIGINAL_LABEL_INDEX,
              ORIGINAL_LABEL));
          axisZShiftAdded.set(get(array, AXIS_Z_SHIFT_OFFSET, ADDED_LABEL_INDEX,
              ADDED_LABEL));
          axisZShiftTotal.set(get(array, AXIS_Z_SHIFT_OFFSET, TOTAL_LABEL_INDEX,
              TOTAL_LABEL));
        }
        else if (line.startsWith(X_AXIS_TILT_TAG)) {
          String[] array = line.split(WHITESPACE);
          xAxisTiltOriginal.set(get(array, X_AXIS_TILT_OFFSET, ORIGINAL_LABEL_INDEX,
              ORIGINAL_LABEL));
          xAxisTiltAdded.set(get(array, X_AXIS_TILT_OFFSET, ADDED_LABEL_INDEX,
              ADDED_LABEL));
          xAxisTiltTotal.set(get(array, X_AXIS_TILT_OFFSET, TOTAL_LABEL_INDEX,
              TOTAL_LABEL));
        }
        else if (line.startsWith(THICKNESS_TAG)) {
          String[] array = line.split(WHITESPACE);
          if (array[THICKNESS_LABEL_INDEX].equals(THICKNESS_LABEL)) {
            containsData = true;
            thickness.set(array[THICKNESS_LABEL_INDEX + 1]);
          }
        }
      }
      reader.close();
    }
    catch (IOException e) {
      if (reader != null) {
        try {
          reader.close();
        }
        catch (IOException e1) {
          e1.printStackTrace();
        }
      }
      e.printStackTrace();
    }
  }

  private String get(String[] array, int offset, int labelIndex, String label) {
    int index = offset + labelIndex;
    if (array[index].equals(label)) {
      containsData = true;
      return array[index + 1];
    }
    return null;
  }

  public boolean containsData() {
    read();
    return containsData;
  }

  public ConstEtomoNumber getAngleOffsetAdded() {
    read();
    return angleOffsetAdded;
  }

  public ConstEtomoNumber getAngleOffsetOriginal() {
    read();
    return angleOffsetOriginal;
  }

  public ConstEtomoNumber getAngleOffsetTotal() {
    read();
    return angleOffsetTotal;
  }

  public ConstEtomoNumber getAxisZShiftAdded() {
    read();
    return axisZShiftAdded;
  }

  public ConstEtomoNumber getAxisZShiftOriginal() {
    read();
    return axisZShiftOriginal;
  }

  public ConstEtomoNumber getAxisZShiftTotal() {
    read();
    return axisZShiftTotal;
  }

  public ConstEtomoNumber getXAxisTiltAdded() {
    read();
    return xAxisTiltAdded;
  }

  public ConstEtomoNumber getXAxisTiltOriginal() {
    read();
    return xAxisTiltOriginal;
  }

  public ConstEtomoNumber getXAxisTiltTotal() {
    read();
    return xAxisTiltTotal;
  }

  public ConstEtomoNumber getThickness() {
    read();
    return thickness;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2007/02/05 23:08:18  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.1  2006/05/11 19:56:28  sueh
 * <p> bug# 838 Parses the tomopitch log file.
 * <p> </p>
 */
