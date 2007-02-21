package etomo.storage;

import java.io.File;

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
* <p> Revision 1.1  2007/02/19 21:53:53  sueh
* <p> bug# 964 File filter for PEET interface files (.epe).
* <p> </p>
*/
public class PeetFileFilter extends DataFileFilter {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * returns true if a file is a peet process data file
   * File must be in the form:  {rootname}.epe
   * Example: particles.epe
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return true;
    }
    String fileName = file.getName();
    if (fileName.endsWith(DatasetFiles.PEET_DATA_FILE_EXT) && fileName.length() > 4) {
      return true;
    }
    return false;
  }

  public String getDescription() {
    return "PEET data file";
  }
}
