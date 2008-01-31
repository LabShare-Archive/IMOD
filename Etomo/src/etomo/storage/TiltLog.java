package etomo.storage;

import java.io.File;

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
 */
public final class TiltLog {
  public static final String rcsid = "$Id$";

  //created in initialization
  private LogFile file;
  
  private final EtomoNumber minAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);
  private final EtomoNumber maxAngle = new EtomoNumber(EtomoNumber.Type.FLOAT);

  private TiltLog() {
  }
  
  public static TiltLog getInstance(final File file) throws LogFile.FileException{
    TiltLog instance = new TiltLog();
    instance.file = LogFile.getInstance(file);
    return instance;
  }

  /**
   * Resets the data, and opens, reads, and closes the log file.
   * @return
   */
  public boolean read() {
    reset();
    try {
      long readId = file.openReader();
      boolean retval = read(readId);
      file.closeReader(readId);
      return retval;
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      return false;
    }
  }

  public String getMinAngle() {
    return minAngle.toString();
  }

  public String getMaxAngle() {
    return maxAngle.toString();
  }

  /**
   * Reads data from the log file.
   * @param readId
   * @return
   */
  private boolean read(long readId) {
    try {
      //find angle list
      String line;
      while ((line = file.readLine(readId)) != null
          && !line.endsWith("Projection angles:")) {
      }
      if (line == null) {
        return false;
      }
      //remove blank line at start of angle list
      while ((line = file.readLine(readId)) != null && !line.matches("\\s*")) {
      }
      if (line == null) {
        return false;
      }
      //set minAngle to the first angle in the angle list
      line = file.readLine(readId);
      minAngle.set(line.trim().split("\\s+")[0]);
      //find the last line of the angle list (assume there is a blank line after
      //the angle list)
      String prevLine = line;
      while ((line = file.readLine(readId)) != null && !line.matches("\\s*")) {
        prevLine = line;
      }
      //set maxAngle to the last angle in the angle list
      String[] angleArray = prevLine.split("\\s+");
      maxAngle.set(angleArray[angleArray.length - 1]);
      return true;
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      return false;
    }
  }

  private void reset() {
    minAngle.reset();
    maxAngle.reset();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2007/07/25 22:57:28  sueh
 * <p> bug# 1027 Class to read the tilt log file.
 * <p> </p>
 */
