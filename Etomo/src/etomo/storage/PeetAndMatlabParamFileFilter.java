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
 * <p> $Log$ </p>
 */
public final class PeetAndMatlabParamFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile()
        && !f.getAbsolutePath().endsWith(DatasetFiles.MATLAB_PARAM_FILE_EXT)
        && !f.getAbsolutePath().endsWith(DatasetFiles.PEET_DATA_FILE_EXT)) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "MATLAB param file (" + DatasetFiles.MATLAB_PARAM_FILE_EXT + ", "
        + DatasetFiles.PEET_DATA_FILE_EXT + ")";
  }

}
