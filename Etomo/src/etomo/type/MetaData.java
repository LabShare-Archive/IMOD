package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.comscript.CombineParams;
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
	}

	public void setRevisionNumber(String revNumber) {
		revisionNumber = revNumber;
	}

	/**
	 * Set the fileset name, trimming any white space from the beginning and
	 * end of the string
	 */
	public void setFilesetName(String fileset) {
		String pathName = fileset.trim();
		File file = new File(pathName);
		String path = file.getPath();
		workingDirectory = path.substring(0, path.lastIndexOf(File.separator));
		filesetName = file.getName();
		fixFilesetName();
	}

	/**
	 * Remove the ".st", "a.st", or "b.st" as approrpiate to the
	 */
	private void fixFilesetName() {
		if (axisType == AxisType.SINGLE_AXIS) {
			if (filesetName.endsWith(".st")) {
				int nChars = filesetName.length();
				filesetName = filesetName.substring(0, nChars - 3);
			}
		} else {
			if (filesetName.endsWith("a.st") | filesetName.endsWith("b.st")) {
				int nChars = filesetName.length();
				filesetName = filesetName.substring(0, nChars - 4);
			}
		}
	}

  public CombineParams getCombineParams() {
  return combineParams;
}

	/**
	 * Set the backup diretory, trimming any white space from the beginning and
	 * end of the string
	 */
	public void setBackupDirectory(String backupDir) {
		backupDirectory = backupDir.trim();
	}

	/**
	 * Set the working diretory, trimming any white space from the beginning and
	 * end of the string
	 */
	public void setWorkingDirectory(String workDir) {
		workingDirectory = workDir.trim();
	}

	public void setDataSource(DataSource ds) {
		dataSource = ds;
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
		} else {
			group = prepend + ".Setup.";
		}
		props.setProperty(group + "RevisionNumber", revisionNumber);
		props.setProperty(
			group + "ComScriptsCreated",
			String.valueOf(comScriptsCreated));
		props.setProperty(group + "FilesetName", filesetName);
		props.setProperty(group + "BackupDirectory", backupDirectory);
		props.setProperty(group + "WorkingDirectory", workingDirectory);

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
		props.setProperty(
			group + "ImageRotation",
			String.valueOf(imageRotation));

		tiltAngleSpecA.store(props, group + "AxisA");
		props.setProperty(
			group + "AxisA.ExcludeProjections",
			String.valueOf(excludeProjectionsA));

		tiltAngleSpecB.store(props, group + "AxisB");
		props.setProperty(
			group + "AxisB.ExcludeProjections",
			String.valueOf(excludeProjectionsB));

		combineParams.store(props, group);
	}

	/**
	 *  Get the objects attributes from the properties object.
	 */
	public void load(Properties props) {
		load(props, "");
	}
	public void load(Properties props, String prepend) {
		String group;
		if (prepend == "") {
			group = "Setup.";
		} else {
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
		filesetName = props.getProperty(group + "FilesetName", "");
		backupDirectory = props.getProperty(group + "BackupDirectory", "");
		workingDirectory = props.getProperty(group + "WorkingDirectory", "");

		dataSource =
			DataSource.fromString(
				props.getProperty(group + "DataSource", "CCD"));
		axisType =
			AxisType.fromString(
				props.getProperty(group + "AxisType", "Single Axis"));
		viewType =
			ViewType.fromString(
				props.getProperty(group + "ViewType", "Single View"));
		sectionType =
			SectionType.fromString(
				props.getProperty(group + "SectionType", "Single"));
		pixelSize =
			Double.parseDouble(props.getProperty(group + "PixelSize", "0.0"));

		useLocalAlignments =
			Boolean
				.valueOf(
					props.getProperty(group + "UseLocalAlignments", "false"))
				.booleanValue();
		fiducialDiameter =
			Double.parseDouble(
				props.getProperty(group + "FiducialDiameter", "0.0"));

		imageRotation =
			Double.parseDouble(
				props.getProperty(group + "ImageRotation", "0.0"));

		excludeProjectionsA =
			props.getProperty(group + "AxisA.ExcludeProjections", "");
		tiltAngleSpecA.load(props, group + "AxisA");

		excludeProjectionsB =
			props.getProperty(group + "AxisB.ExcludeProjections", "");
		tiltAngleSpecB.load(props, group + "AxisB");

		combineParams.load(props, group);
	}

}
