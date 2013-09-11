package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.logic.MultiparticleReference;
import etomo.ui.LogProperties;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * @notthreadsafe
 * 
 * <p> $Log$
 * <p> Revision 1.30  2011/05/19 16:32:09  sueh
 * <p> bug# 1473 Added getRootName and setRootName.
 * <p>
 * <p> Revision 1.29  2011/04/25 16:46:52  sueh
 * <p> bug# 1475 Improved toString.
 * <p>
 * <p> Revision 1.28  2011/02/22 05:49:48  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.27  2009/11/23 23:26:45  sueh
 * <p> bug# 1292 Removing yaxisContour.
 * <p>
 * <p> Revision 1.26  2009/09/16 16:28:26  sueh
 * <p> bug# 1227 In getName correct check for an empty rootName.
 * <p>
 * <p> Revision 1.25  2009/09/01 03:11:07  sueh
 * <p> bug# 1222 Changed yaxis... to yAxis...
 * <p>
 * <p> Revision 1.24  2009/01/13 19:37:18  sueh
 * <p> bug# 1170 Added useNWeightGroup.
 * <p>
 * <p> Revision 1.23  2008/12/10 18:34:18  sueh
 * <p> bug# 1162 Added a manager stamp to setName.
 * <p>
 * <p> Revision 1.22  2008/09/10 21:33:19  sueh
 * <p> bug# 1135 Added EtomoBoolean2 tiltRange to MetaData so that the state
 * <p> of the check box on the screen can be saved.
 * <p>
 * <p> Revision 1.21  2008/08/22 17:51:44  sueh
 * <p> bug# 1136 Added nWeightGroup.
 * <p>
 * <p> Revision 1.20  2008/04/02 02:24:52  sueh
 * <p> bug# 1095 Added mask fields.
 * <p>
 * <p> Revision 1.19  2007/12/10 22:38:01  sueh
 * <p> bug# 1041 Moved resets to load since they are only done once.
 * <p>
 * <p> Revision 1.18  2007/07/25 22:58:56  sueh
 * <p> bug# 1027 Change start and end tilt range angles to min and max angles.
 * <p>
 * <p> Revision 1.17  2007/06/08 22:20:59  sueh
 * <p> bug# 1014 Fixed the function copy and added revisionNumber to the
 * <p> reset function.
 * <p>
 * <p> Revision 1.16  2007/06/07 21:33:15  sueh
 * <p> Removed print statement.
 * <p>
 * <p> Revision 1.15  2007/06/06 20:44:16  sueh
 * <p> bug# 1016 Added function load1_0 to provide backward compatibility
 * <p> functionality for version 1.0.  Storing revisionNumber.
 * <p>
 * <p> Revision 1.14  2007/06/05 21:28:33  sueh
 * <p> bug# 1010 Added flgWedgeWeight.
 * <p>
 * <p> Revision 1.13  2007/06/04 23:07:42  sueh
 * <p> bug# 1005 Fixed the tilt range key.
 * <p>
 * <p> Revision 1.12  2007/05/16 22:59:05  sueh
 * <p> bug# 964 Added copy(PeetMetaData).
 * <p>
 * <p> Revision 1.11  2007/05/16 01:53:30  sueh
 * <p> bug# 964 Added resetInitMotlFile(), resetTiltRangeEnd(), and resetTiltRangeString().
 * <p>
 * <p> Revision 1.10  2007/05/01 22:27:36  sueh
 * <p> bug# 964 Added yaxisType and yaxisContour.
 * <p>
 * <p> Revision 1.9  2007/04/11 22:21:35  sueh
 * <p> bug# 964 Added edgeShift.
 * <p>
 * <p> Revision 1.8  2007/04/09 21:11:33  sueh
 * <p> bug# 964 Added support for reference.
 * <p>
 * <p> Revision 1.7  2007/03/26 23:35:39  sueh
 * <p> bug# 964 Setting axisType.
 * <p>
 * <p> Revision 1.6  2007/03/20 23:04:01  sueh
 * <p> bug# 964 Added initMotlFile, and tiltRangeStart, and tiltRangeEnd.
 * <p>
 * <p> Revision 1.5  2007/03/01 01:26:47  sueh
 * <p> bug# 964 removed unnecesary protected modifier
 * <p>
 * <p> Revision 1.4  2007/02/21 22:29:31  sueh
 * <p> bug# 964 Fixing validation.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:21:02  sueh
 * <p> bug# 964 Added validations.
 * <p>
 * <p> Revision 1.2  2007/02/20 20:35:50  sueh
 * <p> bug# 964 Added setName, to set the root name.
 * <p>
 * <p> Revision 1.1  2007/02/19 21:59:45  sueh
 * <p> bug# 964 Meta data for the PEET interface.
 * <p> </p>
 */
public class PeetMetaData extends BaseMetaData implements ConstPeetMetaData {
  public static final String rcsid = "$Id$";

  public static final String NEW_TITLE = "PEET";

  // do not change these unless backward compatibility work is done
  private static final String TILT_RANGE_KEY = "TiltRange";
  private static final String GROUP_KEY = "Peet";
  private static final String REFERENCE_KEY = "Reference";
  private static final EtomoVersion VERSION_1_1 = EtomoVersion.getDefaultInstance("1.1");
  private static final String START_KEY = "Start";
  private static final String END_KEY = "End";
  private static final String MASK_MODEL_PTS_KEY = "MaskModelPts";
  private static final String MODEL_NUMBER_KEY = "ModelNumber";
  private static final String PARTICLE_KEY = "Particle";
  private static final String VOLUME_KEY = "Volume";

  // latest version should change when new backwards compatibility issue come up
  private static final EtomoVersion LATEST_VERSION = VERSION_1_1;

  // do not change the names of these veriables unless backward compatibility work is done
  private final StringProperty rootName = new StringProperty("RootName");
  private final IntKeyList initMotlFile = IntKeyList.getStringInstance("InitMotlFile");
  private final IntKeyList tiltRangeMultiAxesFile = IntKeyList
      .getStringInstance("TiltRangeMultiAxesFile");
  private final IntKeyList tiltRangeMin = IntKeyList.getStringInstance(TILT_RANGE_KEY
      + "." + START_KEY);
  private final IntKeyList tiltRangeMax = IntKeyList.getStringInstance(TILT_RANGE_KEY
      + "." + END_KEY);
  private final EtomoNumber referenceVolume = new EtomoNumber(REFERENCE_KEY + "."
      + VOLUME_KEY);
  private final EtomoNumber referenceParticle = new EtomoNumber(REFERENCE_KEY + "."
      + PARTICLE_KEY);
  private final StringProperty referenceFile = new StringProperty(REFERENCE_KEY + ".File");
  private final EtomoNumber edgeShift = new EtomoNumber("EdgeShift");
  private final EtomoBoolean2 flgWedgeWeight = new EtomoBoolean2("FlgWedgeWeight");
  /**
   * @deprecated
   */
  private final EtomoBoolean2 maskUseReferenceParticle = new EtomoBoolean2(
      "Mask.UseReferenceParticle");
  /**
   * @deprecated replaced by maskModelPtsZRotation
   * Replacement fields is not equivalent - no backward compatibility.
   */
  private final EtomoNumber maskModelPtsModelNumber = new EtomoNumber(MASK_MODEL_PTS_KEY
      + "." + MODEL_NUMBER_KEY);
  private final EtomoNumber maskModelPtsZRotation = new EtomoNumber(MASK_MODEL_PTS_KEY
      + ".ZRotation");
  /**
   * @deprecated replaced by maskModelPtsYRotation
   * Replacement fields is not equivalent - no backward compatibility.
   */
  private final EtomoNumber maskModelPtsParticle = new EtomoNumber(MASK_MODEL_PTS_KEY
      + "." + PARTICLE_KEY);
  private final EtomoNumber maskModelPtsYRotation = new EtomoNumber(MASK_MODEL_PTS_KEY
      + ".YRotation");
  private final StringProperty maskTypeVolume = new StringProperty("MastType."
      + VOLUME_KEY);
  private final EtomoNumber nWeightGroup = new EtomoNumber("NWeightGroup");
  private final EtomoBoolean2 tiltRange = new EtomoBoolean2("TiltRange");
  private final EtomoBoolean2 manualCylinderOrientation = new EtomoBoolean2(
      "MaskType.ManualCylinderOrientation");
  private final EtomoNumber referenceMultiparticleLevel = new EtomoNumber(REFERENCE_KEY
      + ".Multiparticle.level");
  private final EtomoBoolean2 tiltRangeMultiAxes = new EtomoBoolean2("TiltRangeMultiAxes");

  public PeetMetaData(final LogProperties logProperties) {
   super(logProperties);
    fileExtension = DataFileType.PEET.extension;
    axisType = AxisType.SINGLE_AXIS;
    referenceMultiparticleLevel.setDefault(MultiparticleReference.DEFAULT_LEVEL);
  }

  public void copy(PeetMetaData input) {
    rootName.set(input.rootName);
    initMotlFile.reset();
    initMotlFile.set(input.initMotlFile);
    tiltRangeMin.reset();
    tiltRangeMin.set(input.tiltRangeMin);
    tiltRangeMax.reset();
    tiltRangeMax.set(input.tiltRangeMax);
    referenceVolume.set(input.referenceVolume);
    referenceParticle.set(input.referenceParticle);
    referenceFile.set(input.referenceFile);
    edgeShift.set(input.edgeShift);
    flgWedgeWeight.set(input.flgWedgeWeight);
    maskModelPtsZRotation.set(input.maskModelPtsZRotation);
    maskModelPtsYRotation.set(input.maskModelPtsYRotation);
    maskTypeVolume.set(input.maskTypeVolume);
    nWeightGroup.set(input.nWeightGroup);
    tiltRange.set(input.tiltRange);
    revisionNumber.set(input.revisionNumber);
    manualCylinderOrientation.set(input.manualCylinderOrientation);
    referenceMultiparticleLevel.set(input.referenceMultiparticleLevel);
    tiltRangeMultiAxes.set(input.tiltRangeMultiAxes);
    tiltRangeMultiAxesFile.reset();
    tiltRangeMultiAxesFile.set(input.tiltRangeMultiAxesFile);
  }

  public String getMetaDataFileName() {
    if (rootName.isEmpty()) {
      return null;
    }
    return DatasetFiles.getPeetDataFileName(rootName.toString());
  }

  public String getName() {
    if (rootName.toString() == null || rootName.toString().matches("\\s*")) {
      return NEW_TITLE;
    }
    return rootName.toString();
  }

  public String getDatasetName() {
    return rootName.toString();
  }

  public String toString() {
    return "[rootName:" + rootName + "," + super.toString() + "]";
  }

  public void setName(final String name) {
    rootName.set(name);
    Utilities.managerStamp(null, rootName.toString());
  }

  public boolean isValid() {
    return validate() == null;
  }

  /**
   * returns null if valid
   * @return error message if invalid
   */
  public String validate() {
    if (rootName.isEmpty()) {
      return "Missing root name.";
    }
    if (rootName.toString().indexOf(File.pathSeparatorChar) != -1
        || rootName.toString().indexOf(File.separatorChar) != -1) {
      return "Invalid root name, " + rootName + ".";
    }
    return null;
  }

  public void load(final Properties props) {
    load(props, "");
  }

  public void load(final Properties props, String prepend) {
    super.load(props, prepend);
    // reset
    initMotlFile.reset();
    tiltRangeMin.reset();
    tiltRangeMax.reset();
    referenceVolume.reset();
    referenceParticle.reset();
    referenceFile.reset();
    nWeightGroup.reset();
    tiltRange.reset();
    revisionNumber.reset();
    referenceMultiparticleLevel.reset();
    tiltRangeMultiAxes.reset();
    tiltRangeMultiAxesFile.reset();
    // load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    rootName.load(props, prepend);
    initMotlFile.load(props, prepend);
    tiltRangeMin.load(props, prepend);
    tiltRangeMax.load(props, prepend);
    referenceVolume.load(props, prepend);
    referenceParticle.load(props, prepend);
    referenceFile.load(props, prepend);
    edgeShift.load(props, prepend);
    flgWedgeWeight.load(props, prepend);
    maskModelPtsZRotation.load(props, prepend);
    maskModelPtsYRotation.load(props, prepend);
    maskTypeVolume.load(props, prepend);
    nWeightGroup.load(props, prepend);
    tiltRange.load(props, prepend);
    manualCylinderOrientation.load(props, prepend);
    referenceMultiparticleLevel.load(props, prepend);
    tiltRangeMultiAxes.load(props, prepend);
    tiltRangeMultiAxesFile.load(props, prepend);

    revisionNumber.load(props, prepend);
    if (revisionNumber.isNull() || revisionNumber.lt(LATEST_VERSION)) {
      // backwards compatibility
      if (revisionNumber.isNull() || revisionNumber.lt(VERSION_1_1)) {
        load1_0(props, prepend);
      }
      revisionNumber.set(LATEST_VERSION);
    }
  }

  /**
   * Backwards compatability function for versions earlier then 1.1.
   * @param props
   * @param prepend
   */
  public void load1_0(final Properties props, final String prepend) {
    tiltRangeMin.load(props, prepend, "TILT_RANGE_KEY" + "." + START_KEY);
    tiltRangeMin.remove(props, prepend, "TILT_RANGE_KEY" + "." + START_KEY);
    tiltRangeMax.load(props, prepend, "TILT_RANGE_KEY" + "." + END_KEY);
    tiltRangeMax.remove(props, prepend, "TILT_RANGE_KEY" + "." + END_KEY);
  }

  public void store(final Properties props, String prepend) {
    super.store(props, prepend);
    revisionNumber.set(LATEST_VERSION);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    rootName.store(props, prepend);
    initMotlFile.store(props, prepend);
    tiltRangeMin.store(props, prepend);
    tiltRangeMax.store(props, prepend);
    referenceVolume.store(props, prepend);
    referenceParticle.store(props, prepend);
    referenceFile.store(props, prepend);
    edgeShift.store(props, prepend);
    flgWedgeWeight.store(props, prepend);
    maskUseReferenceParticle.remove(props, prepend);
    maskModelPtsModelNumber.remove(props, prepend);
    maskModelPtsZRotation.store(props, prepend);
    maskModelPtsParticle.remove(props, prepend);
    maskModelPtsYRotation.store(props, prepend);
    maskTypeVolume.store(props, prepend);
    nWeightGroup.store(props, prepend);
    tiltRange.store(props, prepend);
    revisionNumber.store(props, prepend);
    manualCylinderOrientation.store(props, prepend);
    referenceMultiparticleLevel.store(props, prepend);
    tiltRangeMultiAxes.store(props, prepend);
    tiltRangeMultiAxesFile.store(props, prepend);
  }

  public void setRootName(final String input) {
    rootName.set(input);
  }

  public String getRootName() {
    return rootName.toString();
  }

  public void setMaskModelPtsZRotation(final String input) {
    maskModelPtsZRotation.set(input);
  }

  public void setMaskModelPtsYRotation(final String input) {
    maskModelPtsYRotation.set(input);
  }

  public void setMaskTypeVolume(final String input) {
    maskTypeVolume.set(input);
  }

  public void setManualCylinderOrientation(final boolean input) {
    manualCylinderOrientation.set(input);
  }

  public ConstEtomoNumber getMaskModelPtsZRotation() {
    return maskModelPtsZRotation;
  }

  public String getMaskModelPtsYRotation() {
    return maskModelPtsYRotation.toString();
  }

  public String getMaskTypeVolume() {
    return maskTypeVolume.toString();
  }

  public boolean isManualCylinderOrientation() {
    return manualCylinderOrientation.is();
  }

  public ConstEtomoNumber getEdgeShift() {
    return edgeShift;
  }

  public void setEdgeShift(final Number edgeShift) {
    this.edgeShift.set(edgeShift);
  }

  public String getInitMotlFile(final int key) {
    return initMotlFile.getString(key);
  }

  public String getTiltRangeMultiAxesFile(final int key) {
    return tiltRangeMultiAxesFile.getString(key);
  }

  public void setInitMotlFile(final String initMotlFile, final int key) {
    this.initMotlFile.put(key, initMotlFile);
  }

  public void setTiltRangeMultiAxesFile(final String input, final int key) {
    tiltRangeMultiAxesFile.put(key, input);
  }

  public void resetInitMotlFile() {
    this.initMotlFile.reset();
  }

  public void setTiltRangeMin(final String input, final int key) {
    tiltRangeMin.put(key, input);
  }

  public void resetTiltRangeMin() {
    tiltRangeMin.reset();
  }

  public void setTiltRangeMax(final String input, final int key) {
    tiltRangeMax.put(key, input);
  }

  public void resetTiltRangeMax() {
    tiltRangeMax.reset();
  }

  public String getTiltRangeMin(final int key) {
    return tiltRangeMin.getString(key);
  }

  public String getTiltRangeMax(final int key) {
    return tiltRangeMax.getString(key);
  }

  public String getReferenceFile() {
    return referenceFile.toString();
  }

  public ConstEtomoNumber getReferenceParticle() {
    return referenceParticle;
  }

  public ConstEtomoNumber getReferenceVolume() {
    return referenceVolume;
  }

  public int getReferenceMultiparticleLevel() {
    return referenceMultiparticleLevel.getDefaultedInt();
  }

  public void setReferenceMultiparticleLevel(final String input) {
    referenceMultiparticleLevel.set(input);
  }

  public ConstEtomoNumber getNWeightGroup() {
    return nWeightGroup;
  }

  public void setTiltRange(final boolean input) {
    tiltRange.set(input);
  }

  public boolean isTiltRange() {
    return tiltRange.is();
  }

  public void setReferenceFile(final String referenceFile) {
    this.referenceFile.set(referenceFile);
  }

  public void setReferenceParticle(String referenceParticle) {
    this.referenceParticle.set(referenceParticle);
  }

  public void setFlgWedgeWeight(final boolean input) {
    flgWedgeWeight.set(input);
  }

  public void setTiltRangeMultiAxes(final boolean input) {
    tiltRangeMultiAxes.set(input);
  }

  public boolean isTiltRangeMultiAxes() {
    return tiltRangeMultiAxes.is();
  }

  public boolean isFlgWedgeWeight() {
    return flgWedgeWeight.is();
  }

  public void setReferenceVolume(final Number referenceVolume) {
    this.referenceVolume.set(referenceVolume);
  }

  public void setNWeightGroup(final Number input) {
    nWeightGroup.set(input);
  }

  String getGroupKey() {
    return GROUP_KEY;
  }
}
