package etomo.storage;

import java.io.File;
import java.io.FileFilter;

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
public final class AutofidseedInitFileFilter implements FileFilter {
  public static final String rcsid = "$Id:$";

  private static final String NAME = "afs";
  private static final String EXT = ".pkmod";

  private AutofidseedInitFileFilter() {
  }

  public static boolean exists(final BaseManager manager, final AxisID axisID) {
    File[] fileList = FileType.AUTOFIDSEED_DIR.getFile(manager, axisID).listFiles(
        new AutofidseedInitFileFilter());
    return fileList != null && fileList.length > 0 && fileList[0] != null;
  }

  public static String getFileName(final BaseManager manager, final AxisID axisID) {
    File[] fileList = FileType.AUTOFIDSEED_DIR.getFile(manager, axisID).listFiles(
        new AutofidseedInitFileFilter());
    if (fileList == null || fileList.length == 0 || fileList[0] == null) {
      UIHarness.INSTANCE.openMessageDialog(manager, "No " + getDescr(manager, axisID)
          + " file available.", "No Such File", axisID);
      return null;
    }
    // there should be only one .pkmod file
    return FileType.AUTOFIDSEED_DIR.getFileName(manager, axisID) + File.separator
        + fileList[0].getName();
  }

  private static String getDescr(final BaseManager manager, final AxisID axisID) {
    return FileType.AUTOFIDSEED_DIR.getFileName(manager, axisID) + "/" + NAME + "nnn" + EXT;
  }

  public boolean accept(final File pathname) {
    return pathname != null && pathname.isFile()
        && pathname.getName().matches(NAME + "\\d+\\" + EXT);
  }
}
