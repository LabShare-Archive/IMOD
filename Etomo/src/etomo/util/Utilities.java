package etomo.util;

import java.io.File;

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
 * <p> $$Log$$</p>
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
//MARK 251 done fileExists
  static public boolean fileExists(
    ConstMetaData metaData,
    String fileName,
    AxisID axisID) {
    String workingDirectory = System.getProperty("user.dir");
    //MARK 251 print
    System.out.println("in fileExists: workingDirectory=" + workingDirectory + ", filename=" + metaData.getDatasetName() + axisID.getExtension() + fileName);
    File file =
      new File(
        workingDirectory,
        metaData.getDatasetName() + axisID.getExtension() + fileName);
    if (file.exists()) {
      //MARK 251 print
      System.out.println("in fileExists: file exists.");

      return true;
    }
    return false;
  }

}
