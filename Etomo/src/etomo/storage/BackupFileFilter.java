/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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
 * <p> Revision 1.1  2003/10/16 17:04:28  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * @author rickg
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */

public class BackupFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";
  /*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
	 */
  public boolean accept(File f) {
		if (f.isFile() && f.getAbsolutePath().endsWith("~")) {
			return true;
		}
    return false;
  }

  /*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.filechooser.FileFilter#getDescription()
	 */
  public String getDescription() {
		return "Backup files";
  }

}
