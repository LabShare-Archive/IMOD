package etomo.type;

import java.io.File;

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

  protected int transferfidNumberViews;

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

    // Does the working directory exist
    File workingDirectory = new File(System.getProperty("user.dir"));
    if (!workingDirectory.exists()) {
      invalidReason =
        "Working directory: "
          + workingDirectory.getAbsolutePath()
          + " does not exist";
      return false;
    }

    if (!workingDirectory.canRead()) {
      invalidReason = "Can't read working directory";
      return false;
    }

    if (!workingDirectory.canWrite()) {
      invalidReason = "Can't write working directory";
      return false;
    }

    // Does the appropriate image stack exist in the working directory
    if (axisType == AxisType.DUAL_AXIS) {
      File stack = new File(workingDirectory, datasetName + "a.st");
      if (!stack.exists()) {
        invalidReason =
          datasetName + "a.st does not exist in the working directory";
        return false;
      }

      if (!stack.canRead()) {
        invalidReason = "Can't read " + datasetName + "a.st";
        return false;
      }

      stack = new File(workingDirectory, datasetName + "b.st");
      if (!stack.exists()) {
        invalidReason =
          datasetName + "b.st does not exist in the working directory";
        return false;
      }

      if (!stack.canRead()) {
        invalidReason = "Can't read " + datasetName + "b.st";
        return false;
      }

    }
    else {
      File stack = new File(workingDirectory, datasetName + ".st");
      if (!stack.exists()) {
        invalidReason =
          datasetName + ".st does not exist in the working directory";
        return false;
      }

      if (!stack.canRead()) {
        invalidReason = "Can't read " + datasetName + ".st";
        return false;
      }
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

}
