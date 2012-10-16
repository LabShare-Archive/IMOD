package etomo.type;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import etomo.BaseManager;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>Description: Data from SectionTableRow.  Integrated with SectionTableRow.
 * Can also be stored by JoinMetaData.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>Version Log:
 * <p>1.1:  Converted FinalStart and FinalEnd from integers to longs.  The
 * integer null value from version 1.0 will have to be recongnized and changed
 * to a long null value.  Stopped saving setupXMax, setupYMax, and setupZMax.
 * They should come from the header of setupSection.
 * </p>
 * 
 * <p> $Log$
 * <p> Revision 1.20  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.19  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.18  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.17  2009/02/13 02:32:28  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 1.16  2007/03/26 18:37:52  sueh
 * <p> bug# 964 Changed getDouble(boolean defaultIfNull) to getDefaultDouble() so that
 * <p> the functionality will be remembered and used.
 * <p>
 * <p> Revision 1.15  2006/11/03 21:34:17  sueh
 * <p> bug# 955 Change the order of join final start and end, if necessary
 * <p>
 * <p> Revision 1.14  2006/10/17 20:18:07  sueh
 * <p> bug# 939  Remoing print statements.
 * <p>
 * <p> Revision 1.13  2006/10/16 22:50:03  sueh
 * <p> bug# 919  Added inverted.
 * <p>
 * <p> Revision 1.12  2006/06/29 22:01:19  sueh
 * <p> bug# 880 Removed orderCut because it doesn't need to be stored.
 * <p>
 * <p> Revision 1.11  2006/06/29 20:06:21  sueh
 * <p> bug# 880 Added orderCut.
 * <p>
 * <p> Revision 1.10  2006/04/06 20:14:25  sueh
 * <p> bug# 808 Removed setRowNumber(String) because it is not being used.
 * <p>
 * <p> Revision 1.9  2006/01/20 00:15:51  sueh
 * <p> bug# 804 The path of the rotated tomogram should be property user dir
 * <p>
 * <p> Revision 1.8  2005/12/14 01:29:21  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.7  2005/12/06 23:01:50  sueh
 * <p> bug# 757 Removed convertZ and added convertToRotatedZ and
 * <p> convertFromRotationZ.  Going to .rot and going from .rot now have
 * <p> different formulas.  Only convert if abs(cos(x) * cos(y)) is below
 * <p> COS_X_Y_THRESHOLD.
 * <p>
 * <p> Revision 1.6  2005/11/29 22:44:37  sueh
 * <p> bug# 757 Removed setXMax, YMax, and ZMax() because x, y, and zMax
 * <p> are derived from the section header.  Added convertSetupToJoin and
 * <p> convertJoinToSetup.  Added copySetupToJoin and copyJoinToSetup.
 * <p> Added readHeader.  Split final start and end into setup and join.  Split
 * <p> section into setup and join.  Only saving and loading setup values
 * <p> because the join screen only starts in the setup tab and the join values
 * <p> can be derived from the setup values.
 * <p>
 * <p> Revision 1.5  2005/01/26 00:00:23  sueh
 * <p> Converted ConstEtomoNumber.resetValue to displayValue.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:29:57  sueh
 * <p> bug# 564 Remove recommendedValue from EtomoNumber.  Using
 * <p> resetValue instead.
 * <p>
 * <p> Revision 1.3  2004/11/23 22:32:52  sueh
 * <p> bug# 520 Converted finalStart and end to EtomoNumbers.  Removed
 * <p> unnecessary functions parseDouble and parseInt.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:39:39  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.10  2004/11/17 02:23:18  sueh
 * <p> bug# 520 Added a copy constructor.
 * <p>
 * <p> Revision 1.1.2.9  2004/11/16 02:28:37  sueh
 * <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
 * <p> EtomoFloat, and EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/30 02:36:12  sueh
 * <p> bug# 520 Converted rotation angles to EtomoSimpleType.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/25 23:09:45  sueh
 * <p> bug# 520 Added xMax and yMax.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/22 21:07:27  sueh
 * <p> bug# 520 Converting ints to EtomoInteger as necessary.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/22 03:26:02  sueh
 * <p> bug# 520 Converted rowNumber to an EtomoInteger.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/15 00:46:03  sueh
 * <p> bug# 520 Initializing rowNumber on construction so that it can be used in
 * <p> load.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/08 16:24:37  sueh
 * <p> bug# 520 AMoved initialization of invalidReason to
 * <p> SectionTableRowData.reset().
 * <p>
 * <p> Revision 1.1.2.2  2004/10/06 02:18:37  sueh
 * <p> bug# 520 Made the defaults for Final start and end based on Z min and
 * <p> max.  Saved Z Max.  Added a generic parseInt() function to set
 * <p> invalidReason when Integer.parseInt() failed.  Did the same for
 * <p> parseDouble.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 19:32:58  sueh
 * <p> bug# 520 Divided the SectionTable row into document and view.  This
 * <p> class is the non-const part of the document.  It implements the Storable
 * <p> load functions, and has set functions.
 * <p> </p>
 */
public class SectionTableRowData extends ConstSectionTableRowData {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  /**
   * Construct an empty instance.  Must be passed a row number.
   * @param manager
   * @param rowNumber
   */
  public SectionTableRowData(BaseManager manager, int rowNumber) {
    super(rowNumber);
    this.manager = manager;
    reset();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  /**
   * Construct an instance from ConstSectionTableRowData.
   * @param manager
   * @param constSectionTableRowData
   */
  public SectionTableRowData(BaseManager manager,
      ConstSectionTableRowData constSectionTableRowData) {
    super(constSectionTableRowData);
    this.manager = manager;
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    String storedVersion = props.getProperty(VERSION_KEY);
    rowNumber.load(props, prepend);
    String sectionName = props.getProperty(group + setupSectionString, null);
    if (sectionName != null) {
      setSetupSection(new File(sectionName));
    }
    sampleBottomStart.load(props, prepend);
    sampleBottomEnd.load(props, prepend);
    sampleTopStart.load(props, prepend);
    sampleTopEnd.load(props, prepend);
    setupFinalStart.load(props, prepend);
    setupFinalEnd.load(props, prepend);
    rotationAngleX.load(props, prepend);
    rotationAngleY.load(props, prepend);
    rotationAngleZ.load(props, prepend);
    inverted.load(props, prepend);
    if (storedVersion == null || !storedVersion.equals(VERSION)) {
      convertVersion(storedVersion, props, prepend);
    }
  }

  /**
   * convert stored version to current version
   * @param storedVersion
   */
  private final void convertVersion(String storedVersion, Properties props, String prepend) {
    if (storedVersion == null) {
      // convert from version 1.0 to 1.1
      if (setupFinalStart.equals(EtomoNumber.INTEGER_NULL_VALUE)) {
        setupFinalStart.reset();
      }
      if (setupFinalEnd.equals(EtomoNumber.INTEGER_NULL_VALUE)) {
        setupFinalEnd.reset();
      }
      String group = prepend + ".";
      props.remove(group + setupXMaxString);
      props.remove(group + setupYMaxString);
      props.remove(group + setupZMaxString);
    }
  }

  /**
   * check for a .rot file
   * if the .rot file exists then convert the setup data into join data
   * otherwise copy the setup data to join data
   * Assumes setupSection is set.
   */
  public final void synchronizeSetupToJoin() {
    File rotatedSection = DatasetFiles.getRotatedTomogram(manager, setupSection);
    if (rotatedSection.exists()
        && (!rotationAngleX.isNull() || !rotationAngleY.isNull() || !rotationAngleZ
            .isNull())) {
      convertSetupToJoin(rotatedSection);
    }
    else {
      copySetupToJoin();
    }
    // If inverted: start must be greater then end
    // If not inverted: end must be greater then start
    if ((inverted.is() && joinFinalEnd.gt(joinFinalStart))
        || (!inverted.is() && joinFinalStart.gt(joinFinalEnd))) {
      int temp = joinFinalEnd.getInt();
      joinFinalEnd.set(joinFinalStart);
      joinFinalStart.set(temp);
    }
  }

  /**
   * check whether the join section is a .rot file
   * if it is, convert the join data into setup data
   * if it is not then copy the join data to setup data
   * Assumes joinSection is set.
   */
  public final void synchronizeJoinToSetup() {
    if (DatasetFiles.isRotatedTomogram(joinSection)) {
      convertJoinToSetup();
    }
    else {
      copyJoinToSetup();
    }
  }

  /**
   * Copies Setup tab variables to Join tab variables.
   */
  private final void copySetupToJoin() {
    joinSection = setupSection;
    joinFinalStart.set(setupFinalStart);
    joinFinalEnd.set(setupFinalEnd);
    joinXMax = setupXMax;
    joinYMax = setupYMax;
    joinZMax = setupZMax;
  }

  /**
   * Attempts to set the Join tab section and max variables to a rotated
   * tomogram.  Converts final start and end values from the Setup tab to their
   * equivalents in the rotated tomogram.
   * @param rotatedSection
   */
  private final void convertSetupToJoin(File rotatedSection) {
    if (!setJoinSection(rotatedSection)) {
      return;
    }
    joinFinalStart.set(convertToRotatedZ(setupFinalStart));
    joinFinalEnd.set(convertToRotatedZ(setupFinalEnd));
  }

  /**
   * Converts final start and end values from a rotated tomogram in the Join tab
   * to their equivalents in the original tomogram.
   */
  private final void convertJoinToSetup() {
    setupFinalStart.set(convertFromRotatedZ(joinFinalStart));
    setupFinalEnd.set(convertFromRotatedZ(joinFinalEnd));
  }

  /**
   * Copies Join tab final start and end to Setup tab variables.
   */
  private final void copyJoinToSetup() {
    setupFinalStart.set(joinFinalStart);
    setupFinalEnd.set(joinFinalEnd);
  }

  /**
   * Converts final start and end from the original tomogram to the rotated
   * tomogram.
   * Compute as floating point then round to nearest integer - i.e. do not do integer
   * arithmetic with (zsize + 1) / 2
   * Allowing for a different Z size for the rec and the rot, the formula to get
   * from rec slice to rot slice is:
   * cos(X angle) * cos(Y angle) * (slice - (Zsize_rec + 1) / 2) + (Zsize_rot + 1) / 2
   * If cos(X angle) * cos(Y angle) is less then COS_X_Y_THRESHOLD, do not
   * convert z.
   * @param z
   * @return converted z
   */
  private final int convertToRotatedZ(ConstEtomoNumber z) {
    double cosXY = Math.cos(Math.toRadians(rotationAngleX.getDefaultedDouble()))
        * Math.cos(Math.toRadians(rotationAngleY.getDefaultedDouble()));
    if (Math.abs(cosXY) <= COS_X_Y_THRESHOLD) {
      return z.getInt();
    }
    double zSlice = z.getDouble();
    double zSize = setupZMax;
    double zSizeRotated = joinZMax;
    // System.out.println("z=" + z + ",cosXY=" + cosXY + ",zSlice=" + zSlice
    // + ",zSize=" + zSize + ",zSizeRotated=" + zSizeRotated);
    double convertedZ = cosXY * (zSlice - (zSize + 1.) / 2.) + (zSizeRotated + 1.) / 2.;
    // System.out.println("convertedZ=" + convertedZ);
    return (int) Math.round(convertedZ);
  }

  /**
   * Converts final start and end from the rotated tomogram to the original
   * tomogram.
   * Compute as floating point then round to nearest integer - i.e. do not do integer
   * arithmetic with (zsize + 1) / 2
   * Allowing for a different Z size for the rec and the rot, the formula to get
   * The formula to get from rot slice to rec slice is:
   * (slice - (Zsize_rot + 1) / 2) / (cos(X angle) * cos(Y angle)) + (Zsize_rec + 1) / 2
   * If cos(X angle) * cos(Y angle) is less then COS_X_Y_THRESHOLD, do not
   * convert z.
   * @param z
   * @return converted z
   */
  private final int convertFromRotatedZ(ConstEtomoNumber z) {
    double cosXY = Math.cos(Math.toRadians(rotationAngleX.getDefaultedDouble()))
        * Math.cos(Math.toRadians(rotationAngleY.getDefaultedDouble()));
    if (Math.abs(cosXY) <= COS_X_Y_THRESHOLD) {
      return z.getInt();
    }
    double zSlice = z.getDouble();
    double zSize = setupZMax;
    double zSizeRotated = joinZMax;
    // System.out.println("z=" + z + ",cosXY=" + cosXY + ",zSlice=" + zSlice
    // + ",zSize=" + zSize + ",zSizeRotated=" + zSizeRotated);
    double convertedZ = (zSlice - (zSizeRotated + 1.) / 2.) / cosXY + (zSize + 1.) / 2.;
    // System.out.println("convertedZ=" + convertedZ);
    return (int)Math.round(convertedZ);
  }

  public void setRowNumber(int rowNumber) {
    this.rowNumber.set(rowNumber);
  }

  /**
   * Resets the member variables, except the row number.
   *
   */
  private final void reset() {
    invalidReason = null;
    setupSection = null;
    joinSection = null;
    sampleBottomStart.reset();
    sampleBottomEnd.reset();
    sampleTopStart.reset();
    sampleTopEnd.reset();
    setupFinalStart.reset();
    setupFinalEnd.reset();
    joinFinalStart.reset();
    joinFinalEnd.reset();
    rotationAngleX.reset();
    rotationAngleY.reset();
    rotationAngleZ.reset();
    setupXMax = EtomoNumber.INTEGER_NULL_VALUE;
    setupYMax = EtomoNumber.INTEGER_NULL_VALUE;
    setupZMax = EtomoNumber.INTEGER_NULL_VALUE;
    joinXMax = EtomoNumber.INTEGER_NULL_VALUE;
    joinYMax = EtomoNumber.INTEGER_NULL_VALUE;
    joinZMax = EtomoNumber.INTEGER_NULL_VALUE;
  }

  /**
   * Set join section.  Also read the join section header and sets joinXMax,
   * joinYMax, and joinZMax.
   * @param joinSection
   * @return
   */
  public boolean setJoinSection(File joinSection) {
    this.joinSection = joinSection;
    MRCHeader header = readHeader(joinSection.getAbsolutePath());
    if (header == null) {
      return false;
    }
    joinXMax = header.getNColumns();
    joinYMax = header.getNRows();
    joinZMax = header.getNSections();
    return true;
  }

  /**
   * Set setup section.  Also read the setup section header and sets setupXMax,
   * setupYMax, and setupZMax.
   * @param setupSection
   * @return
   */
  public boolean setSetupSection(File setupSection) {
    this.setupSection = setupSection;
    MRCHeader header = readHeader(setupSection.getAbsolutePath());
    if (header == null) {
      return false;
    }
    setupXMax = header.getNColumns();
    setupYMax = header.getNRows();
    setupZMax = header.getNSections();
    setupFinalEnd.setDisplayValue(setupZMax);
    return true;
  }

  /**
   * Reads an mrc header and pops up error messages if there is any kind of
   * failure.
   * @param path
   * @return
   */
  private final MRCHeader readHeader(String path) {
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(), path,
        AxisID.ONLY);
    try {
      if (!header.read(manager)) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read the header in"
            + path, "Setting Max Values Failed");
        return null;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read the header in" + path
          + ".\nInvalidParameterException:  " + e.getMessage(),
          "Setting Max Values Failed");
      return null;
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read the header in" + path
          + ".\nIOException:  " + e.getMessage(), "Setting Max Values Failed");
      return null;
    }
    return header;
  }

  public void setInverted(boolean inverted) {
    this.inverted.set(inverted);
  }

  public ConstEtomoNumber setSampleBottomStart(String sampleBottomStart) {
    return this.sampleBottomStart.set(sampleBottomStart);
  }

  public ConstEtomoNumber setSampleBottomEnd(String sampleBottomEnd) {
    return this.sampleBottomEnd.set(sampleBottomEnd);
  }

  public ConstEtomoNumber setSampleTopStart(String sampleTopStart) {
    return this.sampleTopStart.set(sampleTopStart);
  }

  public ConstEtomoNumber setSampleTopEnd(String sampleTopEnd) {
    return this.sampleTopEnd.set(sampleTopEnd);
  }

  public ConstEtomoNumber setSetupFinalStart(String setupFinalStart) {
    return this.setupFinalStart.set(setupFinalStart);
  }

  public ConstEtomoNumber setJoinFinalStart(String joinFinalStart) {
    return this.joinFinalStart.set(joinFinalStart);
  }

  public ConstEtomoNumber setSetupFinalEnd(String setupFinalEnd) {
    return this.setupFinalEnd.set(setupFinalEnd);
  }

  public ConstEtomoNumber setJoinFinalEnd(String joinFinalEnd) {
    return this.joinFinalEnd.set(joinFinalEnd);
  }

  public ConstEtomoNumber setRotationAngleX(String rotationAngleX) {
    return this.rotationAngleX.set(rotationAngleX);
  }

  public ConstEtomoNumber setRotationAngleY(String rotationAngleY) {
    return this.rotationAngleY.set(rotationAngleY);
  }

  public ConstEtomoNumber setRotationAngleZ(String rotationAngleZ) {
    return this.rotationAngleZ.set(rotationAngleZ);
  }
}