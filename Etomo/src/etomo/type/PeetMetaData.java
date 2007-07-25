package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.util.DatasetFiles;

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

  //do not change these unless backward compatibility work is done
  private static final String TILT_RANGE_KEY = "TiltRange";
  private static final String GROUP_KEY = "Peet";
  private static final String REFERENCE_KEY = "Reference";
  private static final String YAXIS_CONTOUR_KEY = "YaxisContour";
  private static final EtomoVersion VERSION_1_1 = EtomoVersion
      .getDefaultInstance("1.1");
  private static final String START_KEY = "Start";
  private static final String END_KEY = "End";

  //latest version should change when new backwards compatibility issue come up
  private static final EtomoVersion LATEST_VERSION = VERSION_1_1;

  //do not change the names of these veriables unless backward compatibility work is done
  private final StringProperty rootName = new StringProperty("RootName");
  private final IntKeyList initMotlFile = IntKeyList
      .getStringInstance("InitMotlFile");
  private final IntKeyList tiltRangeMin = IntKeyList
      .getStringInstance(TILT_RANGE_KEY + "." + START_KEY);
  private final IntKeyList tiltRangeMax = IntKeyList
      .getStringInstance(TILT_RANGE_KEY + "." + END_KEY);
  private final EtomoNumber referenceVolume = new EtomoNumber(REFERENCE_KEY
      + ".Volume");
  private final EtomoNumber referenceParticle = new EtomoNumber(REFERENCE_KEY
      + ".Particle");
  private final StringProperty referenceFile = new StringProperty(REFERENCE_KEY
      + ".File");
  private final EtomoNumber edgeShift = new EtomoNumber("EdgeShift");
  private final EtomoNumber yaxisContourModelNumber = new EtomoNumber(
      YAXIS_CONTOUR_KEY + ".ModelNumber");
  private final EtomoNumber yaxisContourObjectNumber = new EtomoNumber(
      YAXIS_CONTOUR_KEY + ".ObjectNumber");
  private final EtomoNumber yaxisContourContourNumber = new EtomoNumber(
      YAXIS_CONTOUR_KEY + ".ContourNumber");
  private final EtomoBoolean2 flgWedgeWeight = new EtomoBoolean2(
      "FlgWedgeWeight");

  public PeetMetaData() {
    fileExtension = DatasetFiles.PEET_DATA_FILE_EXT;
    axisType = AxisType.SINGLE_AXIS;
    reset();
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
    yaxisContourModelNumber.set(input.yaxisContourModelNumber);
    yaxisContourObjectNumber.set(input.yaxisContourObjectNumber);
    yaxisContourContourNumber.set(input.yaxisContourContourNumber);
    flgWedgeWeight.set(input.flgWedgeWeight);
    revisionNumber.set(input.revisionNumber);
  }

  public String getMetaDataFileName() {
    if (rootName.isEmpty()) {
      return null;
    }
    return DatasetFiles.getPeetDataFileName(rootName.toString());
  }

  public String getName() {
    if (rootName.equals("")) {
      return NEW_TITLE;
    }
    return rootName.toString();
  }

  public void setName(String name) {
    rootName.set(name);
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

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
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
    yaxisContourModelNumber.load(props, prepend);
    yaxisContourObjectNumber.load(props, prepend);
    yaxisContourContourNumber.load(props, prepend);
    flgWedgeWeight.load(props, prepend);
    revisionNumber.load(props, prepend);
    if (revisionNumber.isNull() || revisionNumber.lt(LATEST_VERSION)) {
      //backwards compatibility
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
  public void load1_0(Properties props, String prepend) {
    tiltRangeMin.load(props, prepend, "TILT_RANGE_KEY" + "." + START_KEY);
    tiltRangeMin.remove(props, prepend, "TILT_RANGE_KEY" + "." + START_KEY);
    tiltRangeMax.load(props, prepend, "TILT_RANGE_KEY" + "." + END_KEY);
    tiltRangeMax.remove(props, prepend, "TILT_RANGE_KEY" + "." + END_KEY);
  }

  public void store(Properties props, String prepend) {
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
    yaxisContourModelNumber.store(props, prepend);
    yaxisContourObjectNumber.store(props, prepend);
    yaxisContourContourNumber.store(props, prepend);
    flgWedgeWeight.store(props, prepend);
    revisionNumber.store(props, prepend);
  }

  public String getEdgeShift() {
    return edgeShift.toString();
  }

  public void setEdgeShift(String edgeShift) {
    this.edgeShift.set(edgeShift);
  }

  public String getInitMotlFile(int key) {
    return initMotlFile.getString(key);
  }

  public void setInitMotlFile(String initMotlFile, int key) {
    this.initMotlFile.put(key, initMotlFile);
  }

  public void resetInitMotlFile() {
    this.initMotlFile.reset();
  }

  public void setTiltRangeMin(String input, int key) {
    tiltRangeMin.put(key, input);
  }

  public void resetTiltRangeMin() {
    tiltRangeMin.reset();
  }

  public void setTiltRangeMax(String input, int key) {
    tiltRangeMax.put(key, input);
  }

  public void resetTiltRangeMax() {
    tiltRangeMax.reset();
  }

  public String getTiltRangeMin(int key) {
    return tiltRangeMin.getString(key);
  }

  public String getTiltRangeMax(int key) {
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

  public ConstEtomoNumber getYaxisContourModelNumber() {
    return yaxisContourModelNumber;
  }

  public String getYaxisContourObjectNumber() {
    return yaxisContourObjectNumber.toString();
  }

  public String getYaxisContourContourNumber() {
    return yaxisContourContourNumber.toString();
  }

  public void setReferenceFile(String referenceFile) {
    this.referenceFile.set(referenceFile);
  }

  public void setReferenceParticle(String referenceParticle) {
    this.referenceParticle.set(referenceParticle);
  }

  public void setFlgWedgeWeight(boolean input) {
    flgWedgeWeight.set(input);
  }

  public boolean isFlgWedgeWeight() {
    return flgWedgeWeight.is();
  }

  public void setReferenceVolume(Number referenceVolume) {
    this.referenceVolume.set(referenceVolume);
  }

  public void setYaxisContourModelNumber(Number input) {
    yaxisContourModelNumber.set(input);
  }

  public void setYaxisContourObjectNumber(String input) {
    yaxisContourObjectNumber.set(input);
  }

  public void setYaxisContourContourNumber(String input) {
    yaxisContourContourNumber.set(input);
  }

  private static String createPrepend(String prepend) {
    if (prepend == "") {
      return GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY;
  }

  private void reset() {
    initMotlFile.reset();
    tiltRangeMin.reset();
    tiltRangeMax.reset();
    referenceVolume.reset();
    referenceParticle.reset();
    referenceFile.reset();
    yaxisContourModelNumber.reset();
    yaxisContourObjectNumber.reset();
    yaxisContourContourNumber.reset();
    revisionNumber.set(LATEST_VERSION);
  }
}
