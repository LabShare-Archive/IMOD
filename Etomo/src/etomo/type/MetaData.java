package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.comscript.CombineParams;
import etomo.comscript.TransferfidParam;
import etomo.storage.Storable;

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
 * <p> Removed data source from Setup dialog.  Removed setDataSource() from MetaData.
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
public class MetaData extends ConstMetaData implements Storable {
  public static final String rcsid =
    "$Id$";

  public MetaData() {
    super();
    resetToDefault();
  }

  public void setRevisionNumber(String revNumber) {
    revisionNumber = revNumber;
  }
  
  protected void resetToDefault() {
    distortionFile = "";
    binning = 1;
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

	public void saveTransferfid(TransferfidParam param) {

		transferfidNumberViews = param.getNumberViews();
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

  public void setSectionType(SectionType st) {
    sectionType = st;
  }

  public void setPixelSize(double pixelSize) {
    this.pixelSize = pixelSize;
  }

  public void setUseLocalAlignments(boolean state) {
    useLocalAlignments = state;
  }

  public void setFiducialDiameter(double fiducialDiameter) {
    this.fiducialDiameter = fiducialDiameter;
  }

  public void setImageRotation(double rotation) {
    imageRotation = rotation;
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

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "Setup.";
    }
    else {
      group = prepend + ".Setup.";
    }

    props.setProperty(group + "RevisionNumber", revisionNumber);
    props.setProperty(
      group + "ComScriptsCreated",
      String.valueOf(comScriptsCreated));
    props.setProperty(group + "DatasetName", datasetName);
    props.setProperty(group + "BackupDirectory", backupDirectory);

    props.setProperty(group + "DataSource", dataSource.toString());
    props.setProperty(group + "AxisType", axisType.toString());
    props.setProperty(group + "ViewType", viewType.toString());
    props.setProperty(group + "SectionType", sectionType.toString());

    props.setProperty(group + "PixelSize", String.valueOf(pixelSize));
    props.setProperty(
      group + "UseLocalAlignments",
      String.valueOf(useLocalAlignments));
    props.setProperty(
      group + "FiducialDiameter",
      String.valueOf(fiducialDiameter));
    props.setProperty(group + "ImageRotation", String.valueOf(imageRotation));

    tiltAngleSpecA.store(props, group + "AxisA");
    props.setProperty(
      group + "AxisA.ExcludeProjections",
      String.valueOf(excludeProjectionsA));

    tiltAngleSpecB.store(props, group + "AxisB");
    props.setProperty(
      group + "AxisB.ExcludeProjections",
      String.valueOf(excludeProjectionsB));

		props.setProperty(group + "TransferfidNumberViews", String.valueOf(transferfidNumberViews));
    combineParams.store(props, group);
    props.setProperty(group + "DistortionFile", distortionFile);
    props.setProperty(group + "Binning", String.valueOf(binning));
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
      group = "Setup.";
    }
    else {
      group = prepend + ".Setup.";
    }
    revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");

    // Make this true for now until the variable is present in all of the
    // data files so as to not break existing files
    // May-03-2002
    comScriptsCreated =
      Boolean
        .valueOf(props.getProperty(group + "ComScriptsCreated", "true"))
        .booleanValue();

    // Backwards compaitibility with FilesetName string
    datasetName = props.getProperty(group + "FilesetName", "");
    datasetName = props.getProperty(group + "DatasetName", datasetName);
    backupDirectory = props.getProperty(group + "BackupDirectory", "");

    dataSource =
      DataSource.fromString(props.getProperty(group + "DataSource", "CCD"));
    axisType =
      AxisType.fromString(props.getProperty(group + "AxisType", "Single Axis"));
    viewType =
      ViewType.fromString(props.getProperty(group + "ViewType", "Single View"));
    sectionType =
      SectionType.fromString(
        props.getProperty(group + "SectionType", "Single"));
    pixelSize =
      Double.parseDouble(props.getProperty(group + "PixelSize", "0.0"));

    useLocalAlignments =
      Boolean
        .valueOf(props.getProperty(group + "UseLocalAlignments", "false"))
        .booleanValue();
    fiducialDiameter =
      Double.parseDouble(props.getProperty(group + "FiducialDiameter", "0.0"));

    imageRotation =
      Double.parseDouble(props.getProperty(group + "ImageRotation", "0.0"));

    excludeProjectionsA =
      props.getProperty(group + "AxisA.ExcludeProjections", "");
    tiltAngleSpecA.load(props, group + "AxisA");

    excludeProjectionsB =
      props.getProperty(group + "AxisB.ExcludeProjections", "");
    tiltAngleSpecB.load(props, group + "AxisB");
    
	  transferfidNumberViews =
			Integer.parseInt(props.getProperty(group + "TransferfidNumberViews", "5"));

    combineParams.load(props, group);
    distortionFile = props.getProperty(group + "DistortionFile", distortionFile);
    binning = Integer.parseInt(props.getProperty(group + "Binning", Integer.toString(binning)));
  }
}
