package etomo.util;

import java.io.File;

import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.type.AxisID;

/**
 * <p>Description: A class containing utility methods.</p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$
 *
 * <p> $$Log$
 * <p> $Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> $Version 1.0.0
 * <p> $
 * <p> $Revision 1.2  2003/10/10 23:17:01  sueh
 * <p> $bug251 removing marks
 * <p> $
 * <p> $Revision 1.1  2003/10/07 22:43:13  sueh
 * <p> $bug251 moved transferfid from fine alignment dialog
 * <p> $to fiducial model dialog
 * <p> $$</p>
 */

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class Utilities {
  private Utilities() {
  }

  static public boolean fileExists(
    ConstMetaData metaData,
    String fileName,
    AxisID axisID) {
    String workingDirectory = System.getProperty("user.dir");
    File file =
      new File(
        workingDirectory,
        metaData.getDatasetName() + axisID.getExtension() + fileName);
    if (file.exists()) {
      return true;
    }
    return false;
  }
  
  /**
   * Print out the specified string if the debug flag is set
   *
   * @param string
   */
  static public void debugPrint(String string) {
  	if(ApplicationManager.isDebug()) {
  		System.err.println(string);
  	}
  }

}
