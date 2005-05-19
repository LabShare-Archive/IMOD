package etomo.storage;

import java.io.File;
import java.io.FilenameFilter;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class XrayStackArchiveFilter implements FilenameFilter {
  public static  final String  rcsid =  "$Id$";
  
  public boolean accept(File dir, String name) {
    if (name.matches(".*_xray.st.gz.\\d+")) {
      return true;
    }
    return false;
  }

}
/**
* <p> $Log$ </p>
*/