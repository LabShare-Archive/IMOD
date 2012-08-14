package etomo.storage;

import java.io.File;

import etomo.type.DataFileType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsFileFilter extends DataFileFilter {
  public static final String rcsid = "$Id:$";

  /**
   * returns true if a file is a serial sections process data file
   * File must be in the form:  {rootname}.ess
   * Example: sections.ess
   */
  public boolean accept(final File file) {
    if (file.isDirectory()) {
      return false;
    }
    String fileName = file.getName();
    if (fileName.endsWith(DataFileType.SERIAL_SECTIONS.extension)
        && fileName.length() > 4) {
      return true;
    }
    return false;
  }

  public String getDescription() {
    return "Serial sections data file (" + DataFileType.SERIAL_SECTIONS.extension + ")";
  }
}
