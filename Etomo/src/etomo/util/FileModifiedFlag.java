package etomo.util;

import java.io.File;

import etomo.type.EtomoNumber;

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
* 
* <p> $Log$
* <p> Revision 1.3  2007/09/11 21:37:38  sueh
* <p> bug# 1035 Added toString().
* <p>
* <p> Revision 1.2  2006/06/14 00:44:26  sueh
* <p> bug# Removed selfTestInvariants() because there isn't much to test.
* <p>
* <p> Revision 1.1  2005/06/20 17:04:28  sueh
* <p> bug# 522 A class to hold the last read state of a file.  Can be used to
* <p> prevent unnecessary reads.
* <p> </p>
*/
class FileModifiedFlag {
  public static final String rcsid = "$Id$";

  private File file;
  private long lastModified = EtomoNumber.LONG_NULL_VALUE;

  FileModifiedFlag(File file) {
    this.file = file;
  }

  boolean isModifiedSinceLastRead() {
    long fileLastModified = file.lastModified();
    boolean modified = lastModified == EtomoNumber.LONG_NULL_VALUE
        || fileLastModified > lastModified;
    return modified;
  }

  void setReadingNow() {
    lastModified = file.lastModified();
  }

  long getLastModified() {
    return lastModified;
  }

  void reset() {
    lastModified = EtomoNumber.LONG_NULL_VALUE;
  }

  public String toString() {
    return file.getAbsolutePath() + ": " + lastModified;
  }
}
