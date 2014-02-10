package etomo.type;

import etomo.logic.DatasetTool;

/**
* <p>Description: Describes the types of of data files and rules about directory sharing.</p>
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
public final class DataFileType {
  public static final String rcsid = "$Id$";

  public static final DataFileType RECON = new DataFileType(".edf", true,
      DatasetTool.STANDARD_DATASET_EXT);
  public static final DataFileType JOIN = new DataFileType(".ejf", false, null);
  public static final DataFileType PARALLEL = new DataFileType(".epp", false, null);
  public static final DataFileType PEET = new DataFileType(".epe", false, null);
  public static final DataFileType SERIAL_SECTIONS = new DataFileType(".ess", false, null);
  public static final DataFileType TOOLS = new DataFileType(null, false, null);
  public static final DataFileType DIRECTIVE_EDITOR = new DataFileType(".adoc", false,
      null);

  public final String extension;
  public final boolean hasAxisType;
  public final String inputFileExt;

  private DataFileType(final String extension, final boolean hasAxisType,
      final String inputFileExt) {
    this.extension = extension;
    /**
     * HasAxisType is true when it is possible for the data file type be a dual axis.
     */
    this.hasAxisType = hasAxisType;
    this.inputFileExt = inputFileExt;
  }

  /**
   * Return a DataFileType instance based on the extension of fileName.  Cannot return the
   * TOOLS instance because it has no extension associated with it.
   * @param fileName
   * @return
   */
  public static DataFileType getInstance(final String fileName) {
    if (fileName == null) {
      return null;
    }
    String ext = fileName;
    int extIndex = fileName.lastIndexOf('.');
    if (extIndex != -1) {
      ext = fileName.substring(extIndex).trim();
    }
    if (ext == null) {
      return null;
    }
    if (ext.equals(RECON.extension)) {
      return RECON;
    }
    if (ext.equals(JOIN.extension)) {
      return JOIN;
    }
    if (ext.equals(PARALLEL.extension)) {
      return PARALLEL;
    }
    if (ext.equals(PEET.extension)) {
      return PEET;
    }
    if (ext.equals(SERIAL_SECTIONS.extension)) {
      return SERIAL_SECTIONS;
    }
    return null;
  }

  public String toString() {
    if (this == RECON) {
      return "Reconstruction";
    }
    if (this == JOIN) {
      return "Join";
    }
    if (this == PARALLEL) {
      return "Parallel";
    }
    if (this == PEET) {
      return "PEET";
    }
    if (this == SERIAL_SECTIONS) {
      return "Serial Sections";
    }
    if (this == TOOLS) {
      return "Tools";
    }
    return extension;
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2009/10/23 19:43:41  sueh
* <p> bug# 1275 Changed the name of TabType to DataFileType to clarify what it
* <p> corresponds to.
* <p>
* <p> Revision 1.2  2007/02/19 22:00:03  sueh
* <p> bug# 964 Added PEET tab type.
* <p>
* <p> Revision 1.1  2006/03/20 18:00:26  sueh
* <p> bug# 835 Type of manager (ApplicationManager, JoinManager, or
* <p> ParallelManager).
* <p> </p>
*/
