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
 * <p> $Log$ </p>
 */

package etomo.util;

import etomo.process.SystemProgram;

public class TestUtilites {
  public static final String rcsid = "$Id$";

  /**
   * Check out the specified test vector into the specified directory
   * @param directory
   * @param vector
   */
  public static void checkoutVector(String directory, String vector) {

    String[] cvsCommand = new String[7];
    cvsCommand[0] = "cvs";
    cvsCommand[1] = "export";
    cvsCommand[2] = "-D";
    cvsCommand[3] = "today";
    cvsCommand[4] = "-d";
    cvsCommand[5] = directory;
    cvsCommand[6] = "ImodTests/EtomoTests/vectors/" + vector;
    SystemProgram cvs = new SystemProgram(cvsCommand);
    cvs.run();
  }

}