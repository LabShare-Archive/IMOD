package etomo.type;

import java.io.File;

import etomo.comscript.CombineParams;

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
 * <p> $Log$ </p>
 */
public class ConstMetaData {
  public static final String rcsid = "$Id$";

  protected String revisionNumber = "1.2";
  protected String filesetName = "";
  protected String backupDirectory = "";
  protected String workingDirectory = "";

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

  public ConstMetaData() {
  }

  public String getRevisionNumber() {
    return revisionNumber;
  }

  public String getFilesetName() {
    return filesetName;
  }

  public String getBackupDirectory() {
    return backupDirectory;
  }

  public String getWorkingDirectory() {
    return workingDirectory;
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

  public CombineParams getCombineParams() {
    return combineParams;
  }

  public boolean isValid() {

    // Does the working directory exist
    File cwd = new File(workingDirectory);
    if(!cwd.exists()) {
      invalidReason = "Working directory does not exist";
      return false;
    }

    if(!cwd.canRead()) {
      invalidReason = "Can't read working directory";
      return false;
    }

    if(!cwd.canWrite()) {
      invalidReason = "Can't write working directory";
      return false;
    }

    // Does the appropriate image stack exist in the working directory
    if(axisType == AxisType.DUAL_AXIS) {
      File stack = new File(workingDirectory, filesetName + "a.st");
      if(!stack.exists()) {
	invalidReason = filesetName +
	  "a.st does not exist in the working directory";
	return false;
      }

      if(!stack.canRead()) {
	invalidReason = "Can't read " + filesetName + "a.st";
	return false;
      }

      stack = new File(workingDirectory, filesetName + "b.st");
      if(!stack.exists()) {
	invalidReason = filesetName +
	  "b.st does not exist in the working directory";
	return false;
      }

      if(!stack.canRead()) {
	invalidReason = "Can't read " + filesetName + "b.st";
	return false;
      }

    }
    else {
      File stack = new File(workingDirectory, filesetName + ".st");
      if(!stack.exists()) {
	invalidReason = filesetName +
	  ".st does not exist in the working directory";
	return false;
      }

      if(!stack.canRead()) {
	invalidReason = "Can't read " + filesetName + ".st";
	return false;
      }
    }
    // Is the pixel size greater than zero

    // Is the fiducial diameter greater than zero
    return true;
  }

}
