package etomo.type;

import java.io.File;
import java.lang.IllegalStateException;

import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.comscript.TransferfidParam;

/*
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.11  2003/10/28 02:23:59  sueh
 * <p> bug267
 * <p>
 * <p> Revision 2.10  2003/10/23 23:04:59  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.9  2003/10/23 22:34:35  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.8  2003/10/23 19:11:16  sueh
 * <p> bug267 look for stack in the working directory and the
 * <p> backup directory when validating directories.
 * <p>
 * <p> Revision 2.7  2003/10/06 22:35:18  sueh
 * <p> transferfidNumberViews needs a default because there is
 * <p> no conscript
 * <p>
 * <p> Revision 2.6  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.5  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.4  2003/05/12 01:24:24  rickg
 * <p> Return invalid working directory in reason
 * <p>
 * <p> Revision 2.3  2003/05/07 17:54:08  rickg
 * <p> Working direcotry is no longer stored in the metadata
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.2  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.1  2003/03/18 23:47:20  rickg
 * <p> Changed method name to get CombineParams reference
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/10/08 23:53:42  rickg
 * <p> getCombineParams now returns a ConstCombineParam object
 * <p>
 * <p> Revision 1.2  2002/09/30 23:48:32  rickg
 * <p> Reformatted after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ConstMetaData {
  public static final String rcsid =
    "$Id$";

  protected String revisionNumber = "1.5";
  protected String datasetName = "";
  protected String backupDirectory = "";

  protected DataSource dataSource = DataSource.CCD;
  protected AxisType axisType = AxisType.SINGLE_AXIS;
  protected ViewType viewType = ViewType.SINGLE_VIEW;
  protected SectionType sectionType = SectionType.SINGLE;

  protected double pixelSize = 0.0;
  protected boolean useLocalAlignments = true;
  protected double fiducialDiameter = 0.0;
  protected double imageRotation = 0.0;

  //  Axis specific data
  protected TiltAngleSpec tiltAngleSpecA = new TiltAngleSpec();
  protected String excludeProjectionsA = "";

  protected TiltAngleSpec tiltAngleSpecB = new TiltAngleSpec();
  protected String excludeProjectionsB = "";

  protected boolean comScriptsCreated = false;

  protected CombineParams combineParams = new CombineParams();

  protected String invalidReason = "";

  protected int transferfidNumberViews = 5;

  public ConstMetaData() {
  }

  public void initializeTransferfid(TransferfidParam param) {
    param.setNumberViews(transferfidNumberViews);
  }

  public String getRevisionNumber() {
    return revisionNumber;
  }

  public String getDatasetName() {
    return datasetName;
  }

  public String getBackupDirectory() {
    return backupDirectory;
  }

  public DataSource getDataSource() {
    return dataSource;
  }

  public AxisType getAxisType() {
    return axisType;
  }

  public ViewType getViewType() {
    return viewType;
  }

  public SectionType getSectionType() {
    return sectionType;
  }

  public double getPixelSize() {
    return pixelSize;
  }

  public boolean getUseLocalAlignments() {
    return useLocalAlignments;
  }

  public double getFiducialDiameter() {
    return fiducialDiameter;
  }

  public double getImageRotation() {
    return imageRotation;
  }

  public TiltAngleSpec getTiltAngleSpecA() {
    return tiltAngleSpecA;
  }

  public TiltAngleSpec getTiltAngleSpecB() {
    return tiltAngleSpecB;
  }

  public String getExcludeProjectionsA() {
    return excludeProjectionsA;
  }

  public String getExcludeProjectionsB() {
    return excludeProjectionsB;
  }

  public boolean getComScriptCreated() {
    return comScriptsCreated;
  }

  public String getInvalidReason() {
    return invalidReason;
  }

  public ConstCombineParams getConstCombineParams() {
    return combineParams;
  }

  public boolean isValid() {
    boolean datasetNameValid = isDatasetNameValid();
    
    if (!datasetNameValid) {
      return datasetNameValid;
    }

    // Is the pixel size greater than zero
    if (pixelSize <= 0.0) {
      invalidReason = "Pixel size is not greater than zero";
      return false;
    }

    // Is the fiducial diameter greater than zero
    if (fiducialDiameter <= 0.0) {
      invalidReason = "Fiducial diameter is not greater than zero";
      return false;
    }

    return true;
  }
  
  public boolean isDatasetNameValid() {
    // Does the working directory exist
    // If is doesn't then use the backup directory.    
    File workingDir = new File(System.getProperty("user.dir"));
    File backupDir = new File(backupDirectory);
    File currentDir;

    //find a valid directory and set directory and type
    if (isValid(workingDir, true)) {
      currentDir = workingDir;
    }
    else if (isValid(backupDir, true)) {
      currentDir = backupDir;
    }
    else {
      //can't find a valid directory, report error

      //if no directory exists then exit
      if (!workingDir.exists() && !backupDir.exists()) {
        invalidReason =
          "The working directory: "
            + workingDir.getAbsolutePath()
            + " and the backup directory: "
            + backupDir.getAbsolutePath()
            + " do not exist";
        return false;
      }

      //decide which directory to complain about:
      //complain about the working directory, if it exists
      if (workingDir.exists()) {
        currentDir = workingDir;
      }
      else {
        currentDir = backupDir;
      }

      if (!currentDir.canRead()) {
        invalidReason =
          "Can't read " + currentDir.getAbsolutePath() + " directory";
        return false;
      }

      if (!currentDir.canWrite()) {
        invalidReason =
          "Can't write " + currentDir.getAbsolutePath() + " directory";
        return false;
      }

      throw new IllegalStateException(
        "Working directory ="
          + workingDir.toString()
          + ",backupDir="
          + backupDir.toString());
    }

    // Does the appropriate image stack exist in the working directory
    if (axisType == AxisType.DUAL_AXIS) {
      currentDir = findValidFile(datasetName + "a.st", currentDir, backupDir);
      if (currentDir == null) {
        return false;
      }
      if (findValidFile(datasetName + "b.st", currentDir) == null) {
        return false;
      }
    }
    else {
      if (findValidFile(datasetName + ".st", currentDir, backupDir) == null) {
        return false;
      }
    }
    return true;
  }

  /**
   * Checks a file's state.  Checks whether a file exists and
   * is readable.  Optionally checks whether a file is
   * writable.
   * 
   * @param file
   * @param writeable - If true, the file must be writeable
   * @return boolean
   */
  protected static boolean isValid(File file, boolean writeable) {
    if (file == null) {
      return false;
    }
    if (!file.exists()) {
      return false;
    }

    return file.canRead() && (!writeable || file.canWrite());
  }

  /**
   * Finds a file in either the current directory or an
   * alternate directory.  The file must be readable.
   * 
   * The current directory state variable can point to the 
   * alternative directory state instance.
   * In this case, only the alternative directory is checked.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @param altDir - The alternate directory.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  protected File findValidFile(String fileName, File curDir, File altDir) {

    if (fileName == null || curDir == null || altDir == null || !isValid(curDir, true)) {
      throw new IllegalArgumentException("ConstMetaData.findValidFile(String,File,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);

    while (!file.exists()) {
      if (curDir == altDir || !isValid(altDir, true)) {
        invalidReason =
          fileName + " does not exist in  " + curDir.getAbsolutePath();
        return null;
      }
      curDir = altDir;
      file = new File(curDir, fileName);
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  /**
   * Finds a file in the current directory.  The file must be readable.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  protected File findValidFile(String fileName, File curDir) {
    if (fileName == null || curDir == null || !isValid(curDir, true)) {
      throw new IllegalArgumentException("ConstMetaData.findValidFile(String,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);

    if (!file.exists()) {
      invalidReason =
        fileName + " does not exist in " + curDir.getAbsolutePath();
      return null;
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

}
