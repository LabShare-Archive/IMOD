package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import etomo.util.DatasetFiles;

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
* <p> Revision 1.1  2007/02/05 23:05:20  sueh
* <p> bug# 962 Filter for model files.
* <p> </p>
*/
public final class ModelFileFilter extends FileFilter implements java.io.FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    String filePath = f.getAbsolutePath();
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !filePath.endsWith(DatasetFiles.MODEL_EXT)) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "Model file";
  }
}
