package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.storage.Storable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.22  2007/02/05 23:25:15  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.21  2006/10/17 20:12:25  sueh
 * <p> bug# 939  Moved ScriptParameter.defaultValue to ConstEtomoNumber.  Making
 * <p> rotation values EtomoNumber instead of ScriptParameter.  Setting default value
 * <p> to that the rotation conversion of Z will work.
 * <p>
 * <p> Revision 1.20  2006/10/16 22:49:20  sueh
 * <p> bug# 919  Added getInverted().
 * <p>
 * <p> Revision 1.19  2006/06/29 22:01:06  sueh
 * <p> bug# 880 Removed orderCut because it doesn't need to be stored.
 * <p>
 * <p> Revision 1.18  2006/06/29 20:06:00  sueh
 * <p> bug# 880 Added orderCut
 * <p>
 * <p> Revision 1.17  2006/04/06 20:10:20  sueh
 * <p> bug# 808 Stopped using setDefault for the rotation angles.
 * <p>
 * <p> Revision 1.16  2006/03/21 19:34:41  sueh
 * <p> corrected typo
 * <p>
 * <p> Revision 1.15  2006/01/27 18:40:03  sueh
 * <p> bug# 801 Don't default joinFinalStart to 1
 * <p>
 * <p> Revision 1.14  2005/12/14 01:28:47  sueh
 * <p> bug# 782 Updated toString().
 * <p>
 * <p> Revision 1.13  2005/12/06 23:00:04  sueh
 * <p> bug# 757 Added COS_X_Y_THRESHOLD
 * <p>
 * <p> Revision 1.12  2005/11/30 21:15:58  sueh
 * <p> bug# 757 Adding getSetupXMax, YMax, and ZMax().
 * <p>
 * <p> Revision 1.11  2005/11/29 22:33:43  sueh
 * <p> bug# New file version = 1.1.  Split final fields into setup and join.  Split
 * <p> section into setup and join.
 * <p>
 * <p> Revision 1.10  2005/07/29 19:47:01  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.9  2005/06/16 20:03:06  sueh
 * <p> bug# 692 Fixed getChunkSize, which was setting the type instead of the
 * <p> value.
 * <p>
 * <p> Revision 1.8  2005/05/10 02:34:51  sueh
 * <p> bug#658 Removed ScriptParameter.setUseScreenDisplayValue()
 * <p> because it is confusing to turn ConstEtomoNumber.displayValue off and
 * <p> on and because this functionality is only used with the rotationAngles.
 * <p>
 * <p> Revision 1.7  2005/01/29 00:18:12  sueh
 * <p> Removed print statements
 * <p>
 * <p> Revision 1.6  2005/01/25 22:08:03  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 1.5  2005/01/21 23:21:24  sueh
 * <p> bug# 509 bug# 591  Removed initialValue from EtomoNumber constructor.
 * <p> Using set() instead.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:28:11  sueh
 * <p> bug# 564 Remove recommendedValue from EtomoNumber.  Using
 * <p> resetValue instead.
 * <p>
 * <p> Revision 1.3  2004/11/23 22:32:17  sueh
 * <p> bug# 520 Converted finalStart and end to EtomoNumbers.  Added
 * <p> description strings to fields that will display error messages.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:34:14  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.15  2004/11/19 00:17:52  sueh
 * <p> bug# 520 Fixed equals function and made it specific to
 * <p> ConstSectionTableRowData.  Added equalsSample to check whether the
 * <p> fields used to create the sample have changed.
 * <p>
 * <p> Revision 1.1.2.14  2004/11/17 02:22:48  sueh
 * <p> bug# 520 Added a copy constructor.just
 * <p>
 * <p> Revision 1.1.2.13  2004/11/16 02:26:35  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.12  2004/11/15 22:23:45  sueh
 * <p> bug# 520 Moving state variables from ConstSectionTableRow to this class
 * <p> so they can be saved in meta data.
 * <p>
 * <p> Revision 1.1.2.11  2004/11/09 15:14:57  sueh
 * <p> bug# 520 Added getZMax().
 * <p>
 * <p> Revision 1.1.2.10  2004/10/30 02:32:47  sueh
 * <p> bug# 520 Converted rotation angles to EtomoSimpleType.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/29 22:13:21  sueh
 * <p> bug# 520  Added remove() to remove section table data
 * <p> rows from the meta data file.  This gets rid of deleted rows.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/25 23:01:37  sueh
 * <p> bug# 520 Fixed chunk size by passing the number of rows to
 * <p> ConstJoinMetaData.getChunkSize and checking the rowNumber when
 * <p> calculating chunk size.  Added xmax and ymax from header.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/22 21:03:21  sueh
 * <p> bug# 520 Simplifying by passing EtomoSimpleType instead of String and
 * <p> int in get functions.  Removed validation.  Converted ints to
 * <p> EtomoInteger as necessary.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/22 03:23:19  sueh
 * <p> bug# 520 Converted rowNumber to an EtomoInteger.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/21 02:52:47  sueh
 * <p> bug# 520 Added get functions.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/15 00:18:24  sueh
 * <p> bug# 520 Fix createPrepend().  Fix store().  Prevent underflow in
 * <p> getRowIndex().
 * <p>
 * <p> Revision 1.1.2.3  2004/10/08 16:11:24  sueh
 * <p> bug# 520 Added toString() and moved initialization of invalidReason to
 * <p> SectionTableRowData.reset().
 * <p>
 * <p> Revision 1.1.2.2  2004/10/06 01:57:57  sueh
 * <p> bug# 520 Added Z max from header.  Added descriptions of the fields for
 * <p> invalidReason.  Added isValidForMakeSamples() - validate when Make
 * <p> Samples is pressed.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 19:26:26  sueh
 * <p> bug# 520 Divided the SectionTable row into document and view.  This
 * <p> class is the const part of the document.  It implements Storable with
 * <p> abstract load functions, has an equals function and get functions.
 * <p> </p>
 */
public abstract class ConstSectionTableRowData implements Storable {
  public static final String rcsid = "$Id$";

  protected static final String VERSION = "1.1";

  protected static final String VERSION_KEY = "SectionTableRowData.Version";
  protected static final String groupString = "SectionTableRow";
  protected static final String setupSectionString = "Section";
  // For conversion from version 1.0
  protected static final String setupXMaxString = "XMax";
  protected static final String setupYMaxString = "YMax";
  protected static final String setupZMaxString = "ZMax";
  protected static final double COS_X_Y_THRESHOLD = 0.5;

  protected final EtomoNumber rowNumber;// key in the .ejf file, not displayed
  protected final EtomoNumber sampleBottomStart;
  protected final EtomoNumber sampleBottomEnd;
  protected final EtomoNumber sampleTopStart;
  protected final EtomoNumber sampleTopEnd;
  protected final EtomoNumber setupFinalStart;
  protected final EtomoNumber setupFinalEnd;
  protected final EtomoNumber joinFinalStart;
  protected final EtomoNumber joinFinalEnd;
  protected final EtomoNumber rotationAngleX;
  protected final EtomoNumber rotationAngleY;
  protected final EtomoNumber rotationAngleZ;

  protected File setupSection = null;
  protected File joinSection = null;
  protected int setupXMax = EtomoNumber.INTEGER_NULL_VALUE;
  protected int joinXMax = EtomoNumber.INTEGER_NULL_VALUE;
  protected int setupYMax = EtomoNumber.INTEGER_NULL_VALUE;
  protected int joinYMax = EtomoNumber.INTEGER_NULL_VALUE;
  protected int setupZMax = EtomoNumber.INTEGER_NULL_VALUE;
  protected int joinZMax = EtomoNumber.INTEGER_NULL_VALUE;

  // state - these should not be saved to the .ejf file, but they are necessary
  // for remembering the state of a row that is being retrieved from meta data.
  private int imodIndex = -1;
  private int imodRotIndex = -1;
  private boolean sectionExpanded = false;
  protected StringBuffer invalidReason = null;
  protected final EtomoBoolean2 inverted = new EtomoBoolean2("Inverted");

  public abstract void load(Properties props);

  public abstract void load(Properties props, String prepend);

  /**
   * Construct an instance with a row number.
   * @param rowNumber
   */
  protected ConstSectionTableRowData(int rowNumber) {
    // construct
    this.rowNumber = new EtomoNumber("RowNumber");
    sampleBottomStart = new EtomoNumber("SampleBottomStart");
    sampleBottomEnd = new EtomoNumber("SampleBottomEnd");
    sampleTopStart = new EtomoNumber("SampleTopStart");
    sampleTopEnd = new EtomoNumber("SampleTopEnd");
    setupFinalStart = new EtomoNumber("FinalStart");
    setupFinalEnd = new EtomoNumber("FinalEnd");
    joinFinalStart = new EtomoNumber();
    joinFinalEnd = new EtomoNumber();
    rotationAngleX = new EtomoNumber(EtomoNumber.Type.DOUBLE, "RotationAngleX");
    rotationAngleY = new EtomoNumber(EtomoNumber.Type.DOUBLE, "RotationAngleY");
    rotationAngleZ = new EtomoNumber(EtomoNumber.Type.DOUBLE, "RotationAngleZ");
    // configure
    sampleBottomStart.setDescription("Sample Slices, Bottom, Start");
    sampleBottomEnd.setDescription("Sample Slices, Bottom, End");
    sampleTopStart.setDescription("Sample Slices, Top, Start");
    sampleTopEnd.setDescription("Sample Slices, Top, End");
    setupFinalStart.setDescription("Final, Start");
    setupFinalStart.setDisplayValue(1);
    setupFinalEnd.setDescription("Final, End");
    joinFinalStart.setDescription("Final, Start");
    joinFinalEnd.setDescription("Final, End");
    rotationAngleX.setDescription("Rotation Angles, X");
    rotationAngleX.setDefault(0);
    rotationAngleY.setDescription("Rotation Angles, Y");
    rotationAngleY.setDefault(0);
    rotationAngleZ.setDescription("Rotation Angles, Z");
    rotationAngleZ.setDefault(0);
    // initialize
    this.rowNumber.set(rowNumber);
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "rowNumber=" + rowNumber + ",inverted=" + inverted/* +",sampleBottomStart=" +
                                                              * sampleBottomStart +
                                                              * ",\nsampleBottomStart=" +
                                                              * sampleBottomStart +
                                                              * ",sampleBottomEnd=" +
                                                              * sampleBottomEnd +
                                                              * ",\nsampleTopStart=" +
                                                              * sampleTopStart +
                                                              * ",\nsampleTopEnd=" +
                                                              * sampleTopEnd +
                                                              * ",setupFinalStart=" +
                                                              * setupFinalStart +
                                                              * ",\nsetupFinalEnd=" +
                                                              * setupFinalEnd +
                                                              * ",\njoinFinalStart=" +
                                                              * joinFinalStart +
                                                              * ",joinFinalEnd=" +
                                                              * joinFinalEnd +
                                                              * ",\nrotationAngleX=" +
                                                              * rotationAngleX +
                                                              * ",\nrotationAngleY=" +
                                                              * rotationAngleY +
                                                              * ",rotationAngleZ=" +
                                                              * rotationAngleZ +
                                                              * ",\nsetupSection=" +
                                                              * setupSection +
                                                              * ",joinSection=" +
                                                              * joinSection +
                                                              * ",\nsetupXMax=" +
                                                              * setupXMax + ",joinXMax=" +
                                                              * joinXMax + ",\nsetupYMax="
                                                              * + setupYMax + ",joinYMax="
                                                              * + joinYMax + ",setupZMax="
                                                              * + setupZMax +
                                                              * ",\njoinZMax=" + joinZMax
                                                              * + ",imodIndex=" +
                                                              * imodIndex +
                                                              * ",\nimodRotIndex=" +
                                                              * imodRotIndex +
                                                              * ",sectionExpanded=" +
                                                              * sectionExpanded +
                                                              * ",\ninvalidReason=" +
                                                              * invalidReason +
                                                              * super.toString() */;
  }

  /**
   * Copy constructor.  Does a deep copy.
   * @param constSectionTableRowData
   */
  protected ConstSectionTableRowData(ConstSectionTableRowData constSectionTableRowData) {
    // deep copy
    imodIndex = constSectionTableRowData.imodIndex;
    imodRotIndex = constSectionTableRowData.imodRotIndex;
    sectionExpanded = constSectionTableRowData.sectionExpanded;
    rowNumber = new EtomoNumber(constSectionTableRowData.rowNumber);
    if (constSectionTableRowData.setupSection == null) {
      setupSection = null;
    }
    else {
      setupSection = new File(constSectionTableRowData.setupSection.getAbsolutePath());
    }
    if (constSectionTableRowData.joinSection == null) {
      joinSection = null;
    }
    else {
      joinSection = new File(constSectionTableRowData.joinSection.getAbsolutePath());
    }
    sampleBottomStart = new EtomoNumber(constSectionTableRowData.sampleBottomStart);
    sampleBottomEnd = new EtomoNumber(constSectionTableRowData.sampleBottomEnd);
    sampleTopStart = new EtomoNumber(constSectionTableRowData.sampleTopStart);
    sampleTopEnd = new EtomoNumber(constSectionTableRowData.sampleTopEnd);
    setupFinalStart = new EtomoNumber(constSectionTableRowData.setupFinalStart);
    setupFinalEnd = new EtomoNumber(constSectionTableRowData.setupFinalEnd);
    joinFinalStart = new EtomoNumber(constSectionTableRowData.joinFinalStart);
    joinFinalEnd = new EtomoNumber(constSectionTableRowData.joinFinalEnd);
    rotationAngleX = new ScriptParameter(constSectionTableRowData.rotationAngleX);
    rotationAngleY = new ScriptParameter(constSectionTableRowData.rotationAngleY);
    rotationAngleZ = new ScriptParameter(constSectionTableRowData.rotationAngleZ);
    setupXMax = constSectionTableRowData.setupXMax;
    joinXMax = constSectionTableRowData.joinXMax;
    setupYMax = constSectionTableRowData.setupYMax;
    joinYMax = constSectionTableRowData.joinYMax;
    setupZMax = constSectionTableRowData.setupZMax;
    joinZMax = constSectionTableRowData.joinZMax;
    inverted.set(constSectionTableRowData.inverted);
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.setProperty(group + VERSION_KEY, VERSION);
    rowNumber.store(props, prepend);
    props.setProperty(group + setupSectionString, setupSection.getAbsolutePath());
    sampleBottomStart.store(props, prepend);
    sampleBottomEnd.store(props, prepend);
    sampleTopStart.store(props, prepend);
    sampleTopEnd.store(props, prepend);
    setupFinalStart.store(props, prepend);
    setupFinalEnd.store(props, prepend);
    rotationAngleX.store(props, prepend);
    rotationAngleY.store(props, prepend);
    rotationAngleZ.store(props, prepend);
    inverted.store(props, prepend);
  }

  void remove(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.remove(group + VERSION_KEY);
    rowNumber.remove(props, prepend);
    props.remove(group + setupSectionString);
    sampleBottomStart.remove(props, prepend);
    sampleBottomEnd.remove(props, prepend);
    sampleTopStart.remove(props, prepend);
    sampleTopEnd.remove(props, prepend);
    setupFinalStart.remove(props, prepend);
    setupFinalEnd.remove(props, prepend);
    rotationAngleX.remove(props, prepend);
    rotationAngleY.remove(props, prepend);
    rotationAngleZ.remove(props, prepend);
    inverted.remove(props, prepend);
  }

  protected String createPrepend(String prepend) {
    return createPrepend(prepend, rowNumber);
  }

  public static String createPrepend(String prepend, ConstEtomoNumber rowNumber) {
    if (prepend == "") {
      return groupString + "." + rowNumber.toString();
    }
    return prepend + "." + groupString + "." + rowNumber.toString();
  }

  public boolean equals(ConstSectionTableRowData constSectionTableRowData) {
    if (!equalsSample(constSectionTableRowData)) {
      return false;
    }
    if (joinSection == null && constSectionTableRowData.joinSection != null) {
      return false;
    }
    if (joinSection != null && constSectionTableRowData.joinSection == null) {
      return false;
    }
    if (!joinSection.equals(constSectionTableRowData.joinSection)) {
      return false;
    }
    if (!setupFinalStart.equals(constSectionTableRowData.setupFinalStart)) {
      return false;
    }
    if (!setupFinalEnd.equals(constSectionTableRowData.setupFinalEnd)) {
      return false;
    }
    if (!joinFinalStart.equals(constSectionTableRowData.joinFinalStart)) {
      return false;
    }
    if (!joinFinalEnd.equals(constSectionTableRowData.joinFinalEnd)) {
      return false;
    }
    return true;
  }

  public boolean equalsSample(ConstSectionTableRowData constSectionTableRowData) {
    if (!rowNumber.equals(constSectionTableRowData.rowNumber)) {
      return false;
    }
    if (setupSection == null && constSectionTableRowData.setupSection != null) {
      return false;
    }
    if (setupSection != null && constSectionTableRowData.setupSection == null) {
      return false;
    }
    if (!setupSection.equals(constSectionTableRowData.setupSection)) {
      return false;
    }
    if (!sampleBottomStart.equals(constSectionTableRowData.sampleBottomStart)) {
      return false;
    }
    if (!sampleBottomEnd.equals(constSectionTableRowData.sampleBottomEnd)) {
      return false;
    }
    if (!sampleTopStart.equals(constSectionTableRowData.sampleTopStart)) {
      return false;
    }
    if (!sampleTopEnd.equals(constSectionTableRowData.sampleTopEnd)) {
      return false;
    }
    if (!rotationAngleX.equals(constSectionTableRowData.rotationAngleX)) {
      return false;
    }
    if (!rotationAngleY.equals(constSectionTableRowData.rotationAngleY)) {
      return false;
    }
    if (!rotationAngleZ.equals(constSectionTableRowData.rotationAngleZ)) {
      return false;
    }
    return true;
  }

  private static String convertToString(int value) {
    if (value == Integer.MIN_VALUE) {
      return "";
    }
    return Integer.toString(value);
  }

  private static String convertToString(double value) {
    if (Double.isNaN(value)) {
      return "";
    }
    return Double.toString(value);
  }

  public String getInvalidReason() {
    return invalidReason.toString();
  }

  public ConstEtomoNumber getRowNumber() {
    return rowNumber;
  }

  public int getRowIndex() {
    if (rowNumber.getInt() < 0) {
      return -1;
    }
    return rowNumber.getInt() - 1;
  }

  public File getSetupSection() {
    return setupSection;
  }

  public File getJoinSection() {
    return joinSection;
  }

  public int getJoinXMax() {
    return joinXMax;
  }

  public int getSetupXMax() {
    return setupXMax;
  }

  public int getJoinYMax() {
    return joinYMax;
  }

  public int getSetupYMax() {
    return setupYMax;
  }

  public int getJoinZMax() {
    return joinZMax;
  }

  public int getSetupZMax() {
    return setupZMax;
  }

  public ConstEtomoNumber getSampleBottomStart() {
    return sampleBottomStart;
  }

  public ConstEtomoNumber getSampleBottomEnd() {
    return sampleBottomEnd;
  }

  public ConstEtomoNumber getSampleTopStart() {
    return sampleTopStart;
  }

  public ConstEtomoNumber getSampleTopEnd() {
    return sampleTopEnd;
  }

  public ConstEtomoNumber getInverted() {
    return inverted;
  }

  public int getSampleTopNumberSlices(final int tableSize) {
    if (rowNumber.equals(tableSize) || tableSize < 2) {
      return -1;
    }
    int sampleTopEnd = this.sampleTopEnd.getInt();
    int sampleTopStart = this.sampleTopStart.getInt();
    if (sampleTopEnd >= sampleTopStart) {
      return sampleTopEnd - sampleTopStart + 1;
    }
    return 0;
  }

  public int getSampleBottomNumberSlices(final int tableSize) {
    if (rowNumber.equals(1) || tableSize < 2) {
      return -1;
    }
    int sampleBottomEnd = this.sampleBottomEnd.getInt();
    int sampleBottomStart = this.sampleBottomStart.getInt();
    if (sampleBottomEnd >= sampleBottomStart) {
      return sampleBottomEnd - sampleBottomStart + 1;
    }
    return 0;
  }

  public ConstEtomoNumber getSetupFinalStart() {
    return setupFinalStart;
  }

  public ConstEtomoNumber getSetupFinalEnd() {
    return setupFinalEnd;
  }

  public ConstEtomoNumber getJoinFinalStart() {
    return joinFinalStart;
  }

  public ConstEtomoNumber getJoinFinalEnd() {
    return joinFinalEnd;
  }

  public boolean isRotated() {
    return !rotationAngleX.isNull() || !rotationAngleY.isNull()
        || !rotationAngleZ.isNull();
  }

  public ConstEtomoNumber getRotationAngleX() {
    return rotationAngleX;
  }

  public ConstEtomoNumber getRotationAngleY() {
    return rotationAngleY;
  }

  public ConstEtomoNumber getRotationAngleZ() {
    return rotationAngleZ;
  }
}