package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import etomo.type.DataFileType;
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
 * <p> Revision 1.1  2007/06/04 23:06:34  sueh
 * <p> bug# 1005 Accepts .prm and .epe files.
 * <p> </p>
 */
public final class PeetAndMatlabParamFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(DatasetFiles.MATLAB_PARAM_FILE_EXT)
        && !f.getAbsolutePath().endsWith(DataFileType.PEET.extension)) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "PEET file or Matlap param file(" + DatasetFiles.MATLAB_PARAM_FILE_EXT + ", "
        + DataFileType.PEET.extension + ")";
  }

}
