package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

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

  private final EtomoNumber minAngle = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber maxAngle = new EtomoNumber(EtomoNumber.Type.DOUBLE);

  private TiltLog() {
  }

  public static TiltLog getInstance(final File file) throws LogFile.LockException {
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
      LogFile.ReaderId readerId = file.openReader();
      boolean retval = read(readerId);
      file.closeRead(readerId);
      return retval;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return false;
    }
    catch (FileNotFoundException e) {
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
  private boolean read(LogFile.ReaderId readerId) {
    try {
      //find angle list
      String line;
      while ((line = file.readLine(readerId)) != null
          && !line.endsWith("Projection angles:")) {
      }
      if (line == null) {
        return false;
      }
      //remove blank line at start of angle list
      while ((line = file.readLine(readerId)) != null && !line.matches("\\s*")) {
      }
      if (line == null) {
        return false;
      }
      //set minAngle to the first angle in the angle list
      line = file.readLine(readerId);
      minAngle.set(line.trim().split("\\s+")[0]);
      //find the last line of the angle list (assume there is a blank line after
      //the angle list)
      String prevLine = line;
      while ((line = file.readLine(readerId)) != null && !line.matches("\\s*")) {
        prevLine = line;
      }
      //set maxAngle to the last angle in the angle list
      String[] angleArray = prevLine.split("\\s+");
      maxAngle.set(angleArray[angleArray.length - 1]);
      return true;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return false;
    }
    catch (IOException e) {
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
 * <p> Revision 1.5  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.4  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.3  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.2  2008/01/31 20:23:56  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.1  2007/07/25 22:57:28  sueh
 * <p> bug# 1027 Class to read the tilt log file.
 * <p> </p>
 */
