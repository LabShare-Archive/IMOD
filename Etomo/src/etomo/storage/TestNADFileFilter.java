package etomo.storage;

import java.io.File;
import java.io.FileFilter;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/11/06 19:34:47  sueh
 * <p> bug# 1047 Filter for nad_eed_3d chunk scripts.
 * <p> </p>
 */
public final class TestNADFileFilter implements FileFilter{
  public static final String rcsid = "$Id$";
  
  public static final String FILE_NAME_BODY = "nad_eed_3d-";
  public static final String FILE_NAME_EXT = ".com";

  public boolean accept(File file) {
    if (file.isDirectory()) {
      return false;
    }
    String fileName = file.getName();
    if (fileName.startsWith(FILE_NAME_BODY) && fileName.endsWith(FILE_NAME_EXT)) {
      return true;
    }
    return false;
  }
}
