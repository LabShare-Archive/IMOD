package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.comscript.CombineParams;
import etomo.comscript.TransferfidParam;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.15  2005/02/15 21:05:46  sueh
 * <p> bug# 603 Removed SectionType (single or serial sections).
 * <p>
 * <p> Revision 3.14  2005/01/21 23:28:50  sueh
 * <p> bug# 509 bug# 591  Removed transferfidNumberViews.  Added
 * <p> transferfidParamA and transferfidParamB to hold the user-modifiable
 * <p> transferfid values.  Removed initializeTransferfidParam() and added
 * <p> getTransferfidParamAFields() and getTransferfidParamBFields() to get/set
 * <p> storable fields in TransferfidParam.
 * <p>
 * <p> Revision 3.13  2005/01/12 00:43:43  sueh
 * <p> bug# 579 Reusing useLocalAlignments, which isn't being used for anything
 * <p> Reseting useLocalAlignments to true;
 * <p>
 * <p> Revision 3.12  2005/01/11 18:07:38  sueh
 * <p> bug# 578 Added useZFactors.
 * <p>
 * <p> Revision 3.11  2004/12/14 21:47:48  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 3.10  2004/12/07 22:47:47  sueh
 * <p> bug# 564 Added TomogramState member variable.
 * <p>
 * <p> Revision 3.9  2004/12/02 18:29:29  sueh
 * <p> bug# 557 Added a SqueezevolParam instance to be stored in the .edf file.
 * <p>
 * <p> Revision 3.8  2004/11/19 23:35:45  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.7.4.1  2004/09/29 19:30:10  sueh
 * <p> bug# 520 Moved Storable to BaseMetaData.  Moved store() to
 * <p> ConstMetaData and BaseMetaData.  Moved revision and axisType to
 * <p> baseMEtaData.
 * <p>
 * <p> Revision 3.7  2004/06/22 02:02:35  sueh
 * <p> bug# 441 Added TrimvolParam
 * <p>
 * <p> Revision 3.6  2004/06/01 18:55:27  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.5  2004/05/25 23:59:54  sueh
 * <p> bug# 355 when axis type is not available it should be set to
 * <p> "not set"
 * <p>
 * <p> Revision 3.4  2004/04/06 03:00:40  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.3  2004/02/24 18:53:22  sueh
 * <p> bug# 385 added resetToDefault() - for defaults need before
 * <p> MetaData is loaded
 * <p>
 * <p> Revision 3.2  2004/02/21 00:27:44  sueh
 * <p> bug# 386 save/load distortionFile and binning
 * <p>
 * <p> Revision 3.1  2004/02/20 23:45:37  sueh
 * <p> bug# 386 added setDistortionFile() and setBinning()
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.8  2003/11/06 22:44:07  sueh
 * <p> cleaning up tasks
 * <p>
 * <p> Revision 2.7  2003/10/08 22:03:21  sueh
 * <p> Bug263
 * <p> UI Changes
 * <p> Removed data source from Setup dialog.  Removed setDataSource() from 
 * <p> MetaData.
 * <p> DataSource is always the default (CCD) in ConstMetaData
 * <p> Grayed out ViewType.
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
 * <p> Revision 2.4  2003/05/07 17:53:59  rickg
 * <p> Working direcotry is no longer stored in the metadata
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.3  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.2  2003/03/18 23:46:52  rickg
 * <p> Added method to get CombineParams reference
 * <p>
 * <p> Revision 2.1  2003/01/27 15:25:45  rickg
 * <p> Static function fix
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4  2002/10/22 23:25:44  rickg
 * <p> setFilesetName method now sets the working directory as well
 * <p> by parsing the string argument
 * <p>
 * <p> Revision 1.3  2002/10/07 22:28:47  rickg
 * <p> removed unused imports
 * <p>
 * <p> Revision 1.2  2002/09/30 23:49:04  rickg
 * <p> Reformatted after emacs trashed it.
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class MetaData extends ConstMetaData {
  public static final String rcsid = "$Id$";
  
  public MetaData() {
    super();
    resetToDefault();
  }

  public void setRevisionNumber(String revNumber) {
    revisionNumber = revNumber;
  }

  protected void resetToDefault() {
    revisionNumber = "";
    distortionFile = "";
    binning = 1;
    useLocalAlignmentsA = true;
    useLocalAlignmentsB = true;
    useZFactorsA.reset();
    useZFactorsB.reset();
  }

  /**
   * Set the dataset name, trimming any white space from the beginning and
   * end of the string
   */
  public void setDatasetName(String fileName) {
    String pathName = fileName.trim();
    File file = new File(pathName);
    String path = file.getPath();
    datasetName = file.getName();
    fixDatasetName();
  }

  /**
   * Remove the ".st", "a.st", or "b.st" as approrpiate to the
   */
  private void fixDatasetName() {
    if (axisType == AxisType.SINGLE_AXIS) {
      if (datasetName.endsWith(".st")) {
        int nChars = datasetName.length();
        datasetName = datasetName.substring(0, nChars - 3);
      }
    }
    else {
      if (datasetName.endsWith("a.st") | datasetName.endsWith("b.st")) {
        int nChars = datasetName.length();
        datasetName = datasetName.substring(0, nChars - 4);
      }
    }
  }

  public CombineParams getCombineParams() {
    return combineParams;
  }

  public void setTransferfidAFields(TransferfidParam param) {
    transferfidParamA.setStorableFields(param);
  }
  
  public void setTransferfidBFields(TransferfidParam param) {
    transferfidParamB.setStorableFields(param);
  }

  /**
   * Set the backup diretory, trimming any white space from the beginning and
   * end of the string
   */
  public void setBackupDirectory(String backupDir) {
    backupDirectory = backupDir.trim();
  }

  public void setDistortionFile(String distortionFile) {
    this.distortionFile = distortionFile;
  }

  public void setAxisType(AxisType at) {
    axisType = at;
  }

  public void setViewType(ViewType vt) {
    viewType = vt;
  }

  public void setPixelSize(double pixelSize) {
    this.pixelSize = pixelSize;
  }

  public void setUseLocalAlignments(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      useLocalAlignmentsB = state;
    }
    else {
      useLocalAlignmentsA = state;
    }
  }
  
  public void setUseZFactors(AxisID axisID, boolean useZFactors) {
    if (axisID == AxisID.SECOND) {
      this.useZFactorsB.set(useZFactors);
    }
    else {
      this.useZFactorsA.set(useZFactors);
    }
  }

  public void setFiducialDiameter(double fiducialDiameter) {
    this.fiducialDiameter = fiducialDiameter;
  }

  public void setImageRotation(float rotation, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      imageRotationB = rotation;
    }
    else {
      imageRotationA = rotation;
    }
  }

  public void setBinning(int binning) {
    this.binning = binning;
  }

  public void setTiltAngleSpecA(TiltAngleSpec tiltAngleSpec) {
    tiltAngleSpecA = tiltAngleSpec;
  }

  public void setExcludeProjectionsA(String list) {
    excludeProjectionsA = list;
  }

  public void setTiltAngleSpecB(TiltAngleSpec tiltAngleSpec) {
    tiltAngleSpecB = tiltAngleSpec;
  }

  public void setExcludeProjectionsB(String list) {
    excludeProjectionsB = list;
  }

  public void setComScriptCreated(boolean state) {
    comScriptsCreated = state;
  }

  public void setCombineParams(CombineParams combine) {
    combineParams = combine;
  }

  public void setFiducialessAlignment(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      fiducialessAlignmentB = state;
    }
    else {
      fiducialessAlignmentA = state;
    }
  }

  public void setWholeTomogramSample(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      wholeTomogramSampleB = state;
    }
    else {
      wholeTomogramSampleA = state;
    }
  }


  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }
  public void load(Properties props, String prepend) {
    resetToDefault();
    String group;
    if (prepend == "") {
      prepend = "Setup";
    }
    else {
      prepend += ".Setup";
    }
    group = prepend  + ".";
    revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");

    // Make this true for now until the variable is present in all of the
    // data files so as to not break existing files
    // May-03-2002
    comScriptsCreated = Boolean.valueOf(
        props.getProperty(group + "ComScriptsCreated", "true")).booleanValue();

    // Backwards compaitibility with FilesetName string
    datasetName = props.getProperty(group + "FilesetName", "");
    datasetName = props.getProperty(group + "DatasetName", datasetName);
    backupDirectory = props.getProperty(group + "BackupDirectory", "");

    dataSource = DataSource.fromString(props.getProperty(group + "DataSource",
        "CCD"));
    axisType = AxisType.fromString(props.getProperty(group + "AxisType",
        "Not Set"));
    viewType = ViewType.fromString(props.getProperty(group + "ViewType",
        "Single View"));
    pixelSize = Double.parseDouble(props
        .getProperty(group + "PixelSize", "0.0"));

    useLocalAlignmentsA = Boolean.valueOf(
        props.getProperty(group + "UseLocalAlignmentsA", "false"))
        .booleanValue();
    useLocalAlignmentsB = Boolean.valueOf(
        props.getProperty(group + "UseLocalAlignmentsB", "false"))
        .booleanValue();
    fiducialDiameter = Double.parseDouble(props.getProperty(group
        + "FiducialDiameter", "0.0"));

    // Read in the old single image rotation or the newer separate image
    // rotation for each axis
    String strOldRotation = props.getProperty(group + "ImageRotation", "0.0");
    imageRotationA = Float.parseFloat(props.getProperty(group
        + "ImageRotationA", strOldRotation));
    imageRotationB = Float.parseFloat(props.getProperty(group
        + "ImageRotationB", strOldRotation));
    excludeProjectionsA = props.getProperty(group + "AxisA.ExcludeProjections",
        "");
    tiltAngleSpecA.load(props, group + "AxisA");

    excludeProjectionsB = props.getProperty(group + "AxisB.ExcludeProjections",
        "");
    tiltAngleSpecB.load(props, group + "AxisB");
    combineParams.load(props, group);
    distortionFile = props
        .getProperty(group + "DistortionFile", distortionFile);
    binning = Integer.parseInt(props.getProperty(group + "Binning", Integer
        .toString(binning)));

    fiducialessAlignmentA = Boolean.valueOf(
        props.getProperty(group + "FiducialessAlignmentA", "false"))
        .booleanValue();
    fiducialessAlignmentB = Boolean.valueOf(
        props.getProperty(group + "FiducialessAlignmentB", "false"))
        .booleanValue();
    wholeTomogramSampleA = Boolean.valueOf(
        props.getProperty(group + "WholeTomogramSampleA", "false"))
        .booleanValue();
    wholeTomogramSampleB = Boolean.valueOf(
        props.getProperty(group + "WholeTomogramSampleB", "false"))
        .booleanValue();
    trimvolParam.load(props, group);
    squeezevolParam.load(props, prepend);
    useZFactorsA.load(props, prepend);
    useZFactorsB.load(props, prepend);
    transferfidParamA.load(props, prepend);
    transferfidParamB.load(props, prepend);
  }
}