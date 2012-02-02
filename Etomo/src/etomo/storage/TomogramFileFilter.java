package etomo.storage;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.5  2009/10/29 19:52:45  sueh
 * <p> bug# 1280 Allow additions of file extensions that the user uses.
 * <p>
 * <p> Revision 1.4  2006/08/08 20:48:56  sueh
 * <p> bug# 592 Accepting .join files.
 * <p>
 * <p> Revision 1.3  2004/12/04 01:26:53  sueh
 * <p> bug# 557 Added.sqz files to the filter.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:29:54  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/28 22:14:02  sueh
 * <p> bug# 520 Allow .flip files.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/21 17:57:00  sueh
 * <p> bug# 520 A file filter for .rec files
 * <p> </p>
 */
public class TomogramFileFilter extends ExtensibleFileFilter implements java.io.FileFilter {
  public static final String rcsid = "$Id$";

  private static final List extraExtensionList = new ArrayList();

  private boolean allowAll = false;

  /**
   * Accept the if it is a directory or has one of the defined extensions (.rec,
   * .flip, .sqz, or .join).  Also accept if allowAll is on, or if its extension
   * is in extraExtensionList.
   */
  public boolean accept(File file) {
    if (!file.isFile()) {
      return true;
    }
    if (allowAll) {
      return true;
    }
    String filePath = file.getAbsolutePath();
    //  If this is a file test its extension, all others should return true
    if (filePath.endsWith(".rec") || filePath.endsWith(".flip")
        || filePath.endsWith(".sqz") || filePath.endsWith(".join")) {
      return true;
    }
    Iterator iterator = extraExtensionList.iterator();
    while (iterator.hasNext()) {
      if (filePath.endsWith((String) iterator.next())) {
        return true;
      }
    }
    return false;
  }

  /**
   * If accept fails with this file, adds its extension to extraExtensionList.
   * If the file has no extension, turn on allowAll.
   * @param file
   */
  public void addExtension(File file) {
    if (accept(file)) {
      return;
    }
    //New extension
    String fileName = file.getName();
    int extensionIndex = fileName.lastIndexOf(".");
    if (extensionIndex == -1) {
      allowAll = true;
    }
    else {
      extraExtensionList.add(fileName.substring(extensionIndex));
    }
  }

  int getExtraExtensionListSize() {
    return extraExtensionList.size();
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Tomogram file";
  }
}
