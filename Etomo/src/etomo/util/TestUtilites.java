/**
 * <p>Description: This class prodives utility functions for working unit,
 * functional and integration testing.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2004/11/24 01:30:24  sueh
 * <p> bug# 520 Getting working directory from EtomoDirector instead of
 * <p> property.  makeDirectories:  making sure to get a valid path for
 * <p> badDirectory.
 * <p>
 * <p> Revision 1.3  2004/04/06 22:56:56  rickg
 * <p> Workaround for buggy cvs exports
 * <p>
 * <p> Revision 1.2  2004/04/06 02:48:13  rickg
 * <p> Added makeDirectories method
 * <p>
 * <p> Revision 1.1  2004/04/02 18:44:26  rickg
 * <p> Initial revision
 * <p> </p>
 */

package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.process.SystemProgram;

public class TestUtilites {
  public static final String rcsid = "$Id$";

  /**
   * Make all of the directories on on the specified path if necessary.  If the
   * path begins with the systems separator then it is an absolute path, if not
   * it is relative to the current directory specified by propertyUserDir from
   * EtomoDirector. 
   * @param newDirectory
   */
  public static void makeDirectories(String newDirectory) throws IOException {
    //  Create the test directories
    File directory;
    if (newDirectory.startsWith(File.separator)) {
      directory = new File(newDirectory);
    }
    else {
      directory = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), newDirectory);
    }
    if (!directory.exists()) {
      if (!directory.mkdirs()) {
        throw new IOException("Creating directory: "
            + directory.getAbsolutePath());
      }
    }
  }

  /**
   * Check out the specified test vector into the specified directory. Note the
   * cvs export cannot handle a full path as an argument to -d.  The directory
   * must reside in the current directory
   * @param directory
   * @param vector
   */
  public static void checkoutVector(String directory, String vector)
      throws SystemProcessException, InvalidParameterException {
    System.err.println("checkoutVector:directory=" + directory + ",vector=" + vector);
    if (vector.matches(File.separator)) {
      throw new InvalidParameterException(
        "vector can not contain path separators");
    }
    File fileVector = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir() + directory,
      vector);
    System.err.println("fileVector=" + fileVector.getAbsolutePath());
    if (fileVector.exists()) {
      if (!fileVector.delete()) {
        throw new SystemProcessException("Cannot delete vector: " + vector);
      }
    }
    String[] cvsCommand = new String[7];
    cvsCommand[0] = "cvs";
    cvsCommand[1] = "export";
    cvsCommand[2] = "-D";
    cvsCommand[3] = "today";
    cvsCommand[4] = "-d";
    cvsCommand[5] = directory;
    cvsCommand[6] = "ImodTests/EtomoTests/vectors/" + vector;
    SystemProgram cvs = new SystemProgram(cvsCommand);
    cvs.setDebug(true);
    cvs.run();

    if (cvs.getExitValue() > 0) {
      throw new SystemProcessException(cvs.getStdErrorString());
    }

    // NOTE: some version of cvs (1.11.2) have bug that results in a checkout
    // (CVS directory is created) instead of an export when using the -d flag
    // This is a work around to handle that case

    File badDirectory = new File(new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), directory).getAbsolutePath(),
      "CVS");
    if (badDirectory.exists()) {
      String[] rmCommand = new String[3];
      rmCommand[0] = "rm";
      rmCommand[1] = "-rf";
      rmCommand[2] = badDirectory.getAbsolutePath();
      SystemProgram rm = new SystemProgram(rmCommand);
      rm.run();
    }
  }

}