package etomo.ui.swing;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2011</p>
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

interface FileTextFieldInterface {
  public static final String rcsid = "$Id:$";

  public File getFile();

  public void setFile(final File file);
  
  public FileFilter getFileFilter();
}
