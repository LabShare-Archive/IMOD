package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class BlendmontLog {
  public static final String rcsid = "$Id:$";

  private String unalignedStartingXandYLine = null;

  /**
   * Finds and saves the line containing the data for unalignedStartingXandY.
   * @param log
   * @return true if found
   */
  public boolean findUnalignedStartingXandY(final File log) {
    try {
      LogFile logFile = LogFile.getInstance(log);
      LogFile.ReaderId id = logFile.openReader();
      String line = null;
      while ((line = logFile.readLine(id)) != null) {
        if (line.indexOf("Starting coordinates of output in X and Y") != -1) {
          unalignedStartingXandYLine = line;
          return true;
        }
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return false;
  }

  /**
   * Parses unalignedStartingXandYLine and returns X and Y.  FindUnalignedStartingXandY
   * must be run first.
   * @return String[2] or null
   */
  public String[] getUnalignedStartingXandY() {
    if (unalignedStartingXandYLine == null) {
      return null;
    }
    String[] array = unalignedStartingXandYLine.trim().split("\\s*=\\s*");
    if (array == null || array.length < 2) {
      System.out.println("WARNING: Unrecognized blendmont output:\n"
          + unalignedStartingXandYLine);
      return null;
    }
    array = array[1].split("\\s+");
    if (array == null || array.length < 2) {
      System.out.println("WARNING: Unrecognized blendmont output:\n"
          + unalignedStartingXandYLine);
      return null;
    }
    return array;
  }
}
