package etomo.storage;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.ui.swing.UIHarness;

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
public final class AutofidseedSelectionAndSorting implements FileFilter {
  public static final String rcsid = "$Id:$";

  private static final String NAME = "afs";
  private static final String EXT = ".sortmod";

  private AutofidseedSelectionAndSorting() {

  }

  public static boolean exists(final BaseManager manager, final AxisID axisID) {
    File[] fileList = FileType.AUTOFIDSEED_DIR.getFile(manager, axisID).listFiles(
        new AutofidseedSelectionAndSorting());
    return fileList != null && fileList.length > 0;
  }

  /**
   * Returns a space-dividered string containing the list of file names.
   * @param manager
   * @param axisID
   * @return
   */
  public static List<String> getFileNameList(final BaseManager manager, final AxisID axisID) {
    File[] fileList = FileType.AUTOFIDSEED_DIR.getFile(manager, axisID).listFiles(
        new AutofidseedSelectionAndSorting());
    if (fileList == null || fileList.length == 0) {
      UIHarness.INSTANCE.openMessageDialog(manager, "No " + getDescr(manager, axisID)
          + " files available.", "No Such File", axisID);
      return null;
    }
    String subdir = FileType.AUTOFIDSEED_DIR.getFileName(manager, axisID);
    List<String> fileNameList = new ArrayList<String>();
    for (int i = 0; i < fileList.length; i++) {
      if (fileList[i] != null) {
        fileNameList.add(subdir + File.separator + fileList[i].getName());
      }
    }
    Collections.sort(fileNameList);
    return fileNameList;
  }

  private static String getDescr(final BaseManager manager, final AxisID axisID) {
    return FileType.AUTOFIDSEED_DIR.getFileName(manager, axisID) + "/" + NAME + "nnn.n"
        + EXT;
  }

  public boolean accept(final File pathname) {
    return pathname != null && pathname.isFile()
        && pathname.getName().matches(NAME + "\\d+\\.\\d+\\" + EXT);
  }
}
