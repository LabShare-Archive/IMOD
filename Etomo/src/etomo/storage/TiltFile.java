package etomo.storage;

import java.io.File;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

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
 * 
 * <p> $Log$
 * <p> Revision 1.2  2007/07/25 22:56:37  sueh
 * <p> bug# 1027 Change start and end angles to min and max angles.
 * <p>
 * <p> Revision 1.1  2007/03/26 23:32:47  sueh
 * <p> bug# 964 File to read a tilt file (.tlt).
 * <p> </p>
 */

public final class TiltFile {
  public static final String rcsid = "$Id$";

  private final EtomoNumber minAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);
  private final EtomoNumber maxAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);
  private final File file;

  public TiltFile(File file) {
    this.file = file;
    try {
      LogFile fileReader = LogFile.getInstance(file);
      long readId = fileReader.openReader();
      minAngle.set(fileReader.readLine(readId));
      //read until end of file, preserving last line read
      String prevLine = null;
      String line = null;
      while ((line = fileReader.readLine(readId)) != null) {
        prevLine = line;
      }
      maxAngle.set(prevLine);
      //minAngle must be smaller then maxAngle
      if (maxAngle.lt(minAngle)) {
        float temp = minAngle.getFloat();
        minAngle.set(maxAngle);
        maxAngle.set(temp);
      }
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
    }
  }

  public ConstEtomoNumber getMinAngle() {
    return minAngle;
  }

  public ConstEtomoNumber getMaxAngle() {
    return maxAngle;
  }
}
