package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.EtomoDirector;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;

/**
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
 * <p> Revision 3.13  2004/12/07 22:47:38  sueh
 * <p> bug# 564 Added TomogramState member variable.
 * <p>
 * <p> Revision 3.12  2004/12/02 18:29:16  sueh
 * <p> bug# 557 Added a SqueezevolParam instance to be stored in the .edf file.
 * <p>
 * <p> Revision 3.11  2004/11/19 23:33:52  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.10.4.4  2004/11/19 00:15:59  sueh
 * <p> bug# 520 Changed the file extension to contain the period.
 * <p>
 * <p> Revision 3.10.4.3  2004/10/11 02:07:25  sueh
 * <p> bug# 520 Fixed a bug in ConstMetaData where the open edf file menu
 * <p> item wasn't working because it was validating the propertyUserDir of the
 * <p> current manager, not the parent of the edf file being opened.  Now able
 * <p> to pass in the edf file to get the parent from to use in validation.
 * <p>
 * <p> Revision 3.10.4.2  2004/10/01 19:47:26  sueh
 * <p> bug# 520 provide a standard way to get the identifier of a meta data file
 * <p> (getName).  Define a new join string that will go in the menu.  Set a file
 * <p> extension value.
 * <p>
 * <p> Revision 3.10.4.1  2004/09/29 19:23:26  sueh
 * <p> bug# 520 Added base class BaseMetaData.  Made
 * <p> latestRevisionNumber static.  Moved revision functionality to base class.
 * <p> Moved axisType and invalid reason to base class.  Moved store()
 * <p> functions to this class.  Implemented Storable with abstract load
 * <p> functions.
 * <p>
 * <p> Revision 3.10  2004/06/22 02:01:52  sueh
 * <p> bug# 441 added TrimvolParam, updated equals().
 * <p>
 * <p> Revision 3.9  2004/06/01 18:54:49  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.8  2004/05/25 23:57:49  sueh
 * <p> bug# 355 Change isValid() so it can be used with setup dialog
 * <p> or a .edf file.  Tell the user to check the .edf file, when necessary.
 * <p>
 * <p> Revision 3.7  2004/05/25 23:23:40  rickg
 * <p> Bug #391 method refactor
 * <p>
 * <p> Revision 3.6  2004/04/06 03:00:40  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.5  2004/02/24 18:52:22  sueh
 * <p> bug# 385 initialized binning to null
 * <p>
 * <p> Revision 3.4  2004/02/20 23:44:45  sueh
 * <p> bug# 386 added distortionFile and binning
 * <p>
 * <p> Revision 3.3  2004/02/07 03:10:10  sueh
 * <p> bug# 169 Created dataset validation function that returns
 * <p> the valid directory
 * <p>
 * <p> Revision 3.2  2003/12/23 21:53:16  sueh
 * <p> bug# 371 Remove validation test for b stack.
 * <p>
 * <p> Revision 3.1  2003/12/08 22:31:16  sueh
 * <p> bug# 169 adding a new function isDatasetNameValid.
 * <p>
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
public abstract class ConstMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  private static final String latestRevisionNumber = "1.7";
  private static final String newTomogramTitle = "Setup Tomogram";
  
  protected String datasetName = "";
  protected String backupDirectory = "";
  protected String distortionFile = "";

  protected DataSource dataSource = DataSource.CCD;
  protected ViewType viewType = ViewType.SINGLE_VIEW;
  protected SectionType sectionType = SectionType.SINGLE;

  protected double pixelSize = Double.NaN;
  protected boolean useLocalAlignments = true;
  protected double fiducialDiameter = Double.NaN;
  protected float imageRotationA = Float.NaN;
  protected float imageRotationB = Float.NaN;
  protected int binning = Integer.MIN_VALUE;
  protected boolean fiducialessAlignment = false;
  protected boolean wholeTomogramSample = false;

  //  Axis specific data
  protected TiltAngleSpec tiltAngleSpecA = new TiltAngleSpec();
  protected String excludeProjectionsA = "";

  protected TiltAngleSpec tiltAngleSpecB = new TiltAngleSpec();
  protected String excludeProjectionsB = "";

  protected boolean comScriptsCreated = false;

  protected CombineParams combineParams = new CombineParams();
  protected TrimvolParam trimvolParam = new TrimvolParam();
  protected SqueezevolParam squeezevolParam = new SqueezevolParam();
  protected TomogramState state = new TomogramState();

  protected int transferfidNumberViews = 5;

  public abstract void load(Properties props);
  public abstract void load(Properties props, String prepend);
    
  public ConstMetaData() {
    fileExtension = ".edf";
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = "Setup";
    }
    else {
      prepend += ".Setup";
    }
    group = prepend  + ".";
    props.setProperty(group + "RevisionNumber", latestRevisionNumber);
    props.setProperty(group + "ComScriptsCreated", String
        .valueOf(comScriptsCreated));
    props.setProperty(group + "DatasetName", datasetName);
    props.setProperty(group + "BackupDirectory", backupDirectory);

    props.setProperty(group + "DataSource", dataSource.toString());
    props.setProperty(group + "AxisType", axisType.toString());
    props.setProperty(group + "ViewType", viewType.toString());
    props.setProperty(group + "SectionType", sectionType.toString());

    props.setProperty(group + "PixelSize", String.valueOf(pixelSize));
    props.setProperty(group + "UseLocalAlignments", String
        .valueOf(useLocalAlignments));
    props.setProperty(group + "FiducialDiameter", String
        .valueOf(fiducialDiameter));
    props.setProperty(group + "ImageRotationA", String.valueOf(imageRotationA));
    props.setProperty(group + "ImageRotationB", String.valueOf(imageRotationB));
    tiltAngleSpecA.store(props, group + "AxisA");
    props.setProperty(group + "AxisA.ExcludeProjections", String
        .valueOf(excludeProjectionsA));

    tiltAngleSpecB.store(props, group + "AxisB");
    props.setProperty(group + "AxisB.ExcludeProjections", String
        .valueOf(excludeProjectionsB));

    props.setProperty(group + "TransferfidNumberViews", String
        .valueOf(transferfidNumberViews));
    combineParams.store(props, group);
    props.setProperty(group + "DistortionFile", distortionFile);
    props.setProperty(group + "Binning", String.valueOf(binning));
    props.setProperty(group + "FiducialessAlignment", String
        .valueOf(fiducialessAlignment));
    props.setProperty(group + "WholeTomogramSample", String
        .valueOf(wholeTomogramSample));
    trimvolParam.store(props, group);
    squeezevolParam.store(props, prepend);
    state.store(props, prepend);
  }

  public TomogramState getState() {
    return state;
  }
  public TrimvolParam getTrimvolParam() {
    return trimvolParam;
  }
  
  public SqueezevolParam getSqueezevolParam() {
    return squeezevolParam;
  }
  
  public void initializeTransferfid(TransferfidParam param) {
    param.setNumberViews(transferfidNumberViews);
  }

  public String getDatasetName() {
    return datasetName;
  }
  
  public String getMetaDataFileName() {
    if (datasetName.equals("")) {
      return "";
    }
    return datasetName + fileExtension;
  }
  
  public String getName() {
    if (datasetName.equals("")) {
      return newTomogramTitle;
    }
    return datasetName;
  }
  
  public static String getNewFileTitle() {
    return newTomogramTitle;
  }

  public String getBackupDirectory() {
    return backupDirectory;
  }

  public String getDistortionFile() {
    return distortionFile;
  }

  public DataSource getDataSource() {
    return dataSource;
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

  public float getImageRotation(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return imageRotationB;
    }
    return imageRotationA;
  }

  public int getBinning() {
    return binning;
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

  public boolean isFiducialessAlignment() {
    return fiducialessAlignment;
  }

  public boolean isWholeTomogramSample() {
    return wholeTomogramSample;
  }

  public ConstCombineParams getConstCombineParams() {
    return combineParams;
  }

  public boolean isValid() {
    return isValid(true, null);
  }
  
  public boolean isValid(boolean fromScreen) {
    return isValid(fromScreen, null);
  }
  
  public boolean isValid(File paramFile) {
    return isValid(false, paramFile);
  }
  
  public boolean isValid(boolean fromScreen, File paramFile) {
    invalidReason = "";
    
    String helpString;
    if (!fromScreen) {
      helpString = "  Check the Etomo data file."; 
    }
    else {
      helpString = "";
    }
    
    if (axisType == null  || axisType == AxisType.NOT_SET) {
      invalidReason =
        "Axis type should be either Dual Axis or Single Axis." + helpString;
      return false;
    }

    if (!isDatasetNameValid(paramFile)) {
      invalidReason += helpString;
      return false;
    }

    // Is the pixel size greater than zero
    if (fromScreen && pixelSize <= 0.0) {
      invalidReason = "Pixel size is not greater than zero.";
      return false;
    }

    // Is the fiducial diameter greater than zero
    if (fromScreen && fiducialDiameter <= 0.0) {
      invalidReason = "Fiducial diameter is not greater than zero.";
      return false;
    }
    
    return true;
  }

  public boolean isDatasetNameValid() {
    return isDatasetNameValid(null);
  }
  
  public boolean isDatasetNameValid(File paramFile) {
    invalidReason = "";
    if (datasetName.equals("")) {
      invalidReason = "Dataset name has not been set.";
      return false;
    }
    if (paramFile == null) {
      if (getValidDatasetDirectory(EtomoDirector.getInstance().getCurrentPropertyUserDir()) != null) {
        return true;
      }
    }
    else {
      if (getValidDatasetDirectory(new File(paramFile.getParent()).getAbsolutePath()) != null) {
        return true;
      }
    }
    return false;
  }

  public File getValidDatasetDirectory(String workingDirName) {
    // Does the working directory exist
    // If is doesn't then use the backup directory.    
    File workingDir = new File(workingDirName);
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
        invalidReason = "The working directory: "
            + workingDir.getAbsolutePath() + " and the backup directory: "
            + backupDir.getAbsolutePath() + " do not exist";
        return null;
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
        invalidReason = "Can't read " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      if (!currentDir.canWrite()) {
        invalidReason = "Can't write " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      throw new IllegalStateException("Working directory ="
          + workingDir.toString() + ",backupDir=" + backupDir.toString());
    }

    // Does the appropriate image stack exist in the working directory
    if (axisType == AxisType.DUAL_AXIS) {
      currentDir = findValidFile(datasetName + "a.st", currentDir, backupDir);
    }
    else {
      currentDir = findValidFile(datasetName + ".st", currentDir, backupDir);
    }
    return currentDir;
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
    if (fileName == null || curDir == null || altDir == null
        || !isValid(curDir, true)) {
      throw new IllegalArgumentException(
        "ConstMetaData.findValidFile(String,File,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);
    while (!file.exists()) {
      if (curDir == altDir || !isValid(altDir, true)) {
        invalidReason = fileName + " does not exist in  "
            + curDir.getAbsolutePath();
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
      throw new IllegalArgumentException(
        "ConstMetaData.findValidFile(String,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);

    if (!file.exists()) {
      invalidReason = fileName + " does not exist in "
          + curDir.getAbsolutePath();
      return null;
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  public boolean equals(Object object) {
    if (!(object instanceof ConstMetaData))
      return false;

    ConstMetaData cmd = (ConstMetaData) object;
    if (!datasetName.equals(cmd.getDatasetName()))
      return false;
    if (!backupDirectory.equals(cmd.getBackupDirectory()))
      return false;
    if (!distortionFile.equals(cmd.getDistortionFile()))
      return false;
    if (!dataSource.equals(cmd.getDataSource()))
      return false;
    if (axisType != cmd.getAxisType())
      return false;
    if (!viewType.equals(cmd.getViewType()))
      return false;
    if (!sectionType.equals(cmd.getSectionType()))
      return false;
    if (!(pixelSize == cmd.getPixelSize()))
      return false;
    if (!(useLocalAlignments == cmd.getUseLocalAlignments()))
      return false;
    if (!(fiducialDiameter == cmd.getFiducialDiameter()))
      return false;
    if (!(imageRotationA == cmd.getImageRotation(AxisID.FIRST)))
      return false;
    if (!(imageRotationB == cmd.getImageRotation(AxisID.SECOND)))
      return false;
    if (!(binning == cmd.getBinning()))
      return false;
    if (!(fiducialessAlignment == cmd.isFiducialessAlignment()))
      return false;

    // TODO tilt angle spec needs to be more complete
    if (!(tiltAngleSpecA.getType() == cmd.getTiltAngleSpecA().getType()))
      return false;
    if (!excludeProjectionsA.equals(cmd.getExcludeProjectionsA()))
      return false;

    if (!(tiltAngleSpecB.getType() == cmd.getTiltAngleSpecB().getType()))
      return false;
    if (!excludeProjectionsB.equals(cmd.getExcludeProjectionsB()))
      return false;
    if (!(comScriptsCreated == cmd.getComScriptCreated()))
      return false;
    if (!combineParams.equals(cmd.getConstCombineParams())) {
      return false;
    }
    if (!trimvolParam.equals(cmd.getTrimvolParam())) {
      return false;
    }
    if (!squeezevolParam.equals(cmd.getSqueezevolParam())) {
      return false;
    }
    if (!state.equals(cmd.state)) {
      return false;
    }

    return true;
  }

}