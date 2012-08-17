package etomo.storage;

import java.io.File;

import etomo.type.DataFileType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ParallelFileFilter extends javax.swing.filechooser.FileFilter implements
    java.io.FileFilter {
  public static final String rcsid = "$Id$";

  /**
   * returns true if a file is a parallel process data file
   * File must be in the form:  {rootname}.epp
   * Example: tilta.epp
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return true;
    }
    String fileName = file.getName();
    if (fileName.endsWith(DataFileType.PARALLEL.extension) && fileName.length() > 4) {
      return true;
    }
    return false;
  }

  public String getDescription() {
    return "Parallel process data file (" + DataFileType.PARALLEL.extension + ")";
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/03/20 17:53:30  sueh
* <p> bug# 835 A file filter for the data file associated with ParallelManager (.epp).
* <p> </p>
*/
