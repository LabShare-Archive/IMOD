package etomo.ui.swing;

import java.io.File;

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
interface CurrentDirectory {
  public static final String rcsid = "$Id:$";

  File getCurrentDirectory();

  void setCurrentDirectory(File file);
}
