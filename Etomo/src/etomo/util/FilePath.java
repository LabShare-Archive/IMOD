package etomo.util;

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
public final class FilePath {
  public static final String rcsid = "$Id:$";

  public static String getRelativePath(final String fromAbsolutePath,
      final String toAbsolutePath) {
    System.out.println("A:pathSeparator:" + File.pathSeparator + ",pathSeparatorChar:"
        + File.pathSeparatorChar + ",separator:" + File.separator + ",separatorChar:"
        + File.separatorChar);
    return null;
  }
}
