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
 * <p> $Log$ </p>
 */

public final class TiltFile {
  public static final String rcsid = "$Id$";

  private final EtomoNumber startAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);
  private final EtomoNumber endAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);
  private final File file;

  public TiltFile(File file) {
    this.file = file;
    LogFile fileReader = LogFile.getInstance(file);
    try {
      long readId = fileReader.openReader();
      startAngle.set(fileReader.readLine(readId));
      //read until end of file, preserving last line read
      String prevLine = null;
      String line = null;
      while ((line = fileReader.readLine(readId)) != null) {
        prevLine = line;
      }
      endAngle.set(prevLine);
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
  }
  
  public ConstEtomoNumber getStartAngle() {
    return startAngle;
  }
  
  public ConstEtomoNumber getEndAngle() {
    return endAngle;
  }
}
