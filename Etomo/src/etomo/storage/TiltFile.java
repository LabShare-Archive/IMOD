package etomo.storage;

import java.io.File;
import java.io.IOException;

import etomo.ManagerKey;
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
 * <p> Revision 1.4  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.3  2008/01/31 20:23:18  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
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

  public TiltFile(File file, ManagerKey managerKey) {
    this.file = file;
    try {
      LogFile fileReader = LogFile.getInstance(file, managerKey);
      LogFile.ReaderId readerId = fileReader.openReader();
      minAngle.set(fileReader.readLine(readerId));
      //read until end of file, preserving last line read
      String prevLine = null;
      String line = null;
      while ((line = fileReader.readLine(readerId)) != null) {
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
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
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
