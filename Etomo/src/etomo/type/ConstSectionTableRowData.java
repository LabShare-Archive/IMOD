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
  public static  final String  rcsid =  "$Id$";
  
  protected static final String groupString = "SectionTableRow";
  protected static final String sectionString = "Section";
  protected static final String zMaxString = "ZMax";
  
  //state - these do not have to be saved to a file, but they are necessary
  //for remembering the state of a row that is being retrieved from meta data.
  private int imodIndex = -1;
  private int imodRotIndex = -1;
  private boolean sectionExpanded = false;
  
  protected EtomoNumber rowNumber = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "RowNumber");
  protected File section;
  protected EtomoNumber sampleBottomStart = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SampleBottomStart");
  protected EtomoNumber sampleBottomEnd = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SampleBottomEnd");
  protected EtomoNumber sampleTopStart = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SampleTopStart");
  protected EtomoNumber sampleTopEnd = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SampleTopEnd");
  protected EtomoNumber finalStart = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "FinalStart");
  protected EtomoNumber finalEnd = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "FinalEnd");
  protected EtomoNumber rotationAngleX = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "RotationAngleX");
  protected EtomoNumber rotationAngleY = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "RotationAngleY");
  protected EtomoNumber rotationAngleZ = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "RotationAngleZ");
  protected EtomoNumber xMax = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "XMax");
  protected EtomoNumber yMax = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "YMax");
  protected int zMax = Integer.MIN_VALUE;
  
  protected StringBuffer invalidReason;
  
  public abstract void load(Properties props);
  public abstract void load(Properties props, String prepend);
  
  protected ConstSectionTableRowData() {
    sampleBottomStart.setDescription("Sample Slices, Bottom, Start");
    sampleBottomEnd.setDescription("Sample Slices, Bottom, End");
    sampleTopStart.setDescription("Sample Slices, Top, Start");
    sampleTopEnd.setDescription("Sample Slices, Top, End");
    finalStart.setDescription("Final, Start");
    finalStart.setResetValue(1);
    finalEnd.setDescription("Final, End");
    rotationAngleX.setDescription("Rotation Angles, X");
    rotationAngleX.setDefault(0);
    rotationAngleY.setDescription("Rotation Angles, Y");
    rotationAngleY.setDefault(0);
    rotationAngleZ.setDescription("Rotation Angles, Z");
    rotationAngleZ.setDefault(0);
  }
  
  protected ConstSectionTableRowData(ConstSectionTableRowData that) {
    imodIndex = that.imodIndex;
    imodRotIndex = that.imodRotIndex;
    sectionExpanded = that.sectionExpanded;
    rowNumber = new EtomoNumber(that.rowNumber);
    section = that.section; //Section can't be changed so it can be shared
    sampleBottomStart = new EtomoNumber(that.sampleBottomStart);
    sampleBottomEnd = new EtomoNumber(that.sampleBottomEnd);
    sampleTopStart = new EtomoNumber(that.sampleTopStart);
    sampleTopEnd = new EtomoNumber(that.sampleTopEnd);
    finalStart = that.finalStart;
    finalEnd = that.finalEnd;
    rotationAngleX = new EtomoNumber(that.rotationAngleX);
    rotationAngleY = new EtomoNumber(that.rotationAngleY);
    rotationAngleZ = new EtomoNumber(that.rotationAngleZ);
    xMax = new EtomoNumber(that.xMax);
    yMax = new EtomoNumber(that.yMax);
    zMax = that.zMax;
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",\n" + rowNumber.getDescription() + "=" + rowNumber
        + ",\n" + sectionString + "=" + section + ",\n"
        + sampleBottomStart.getDescription() + "="
        + sampleBottomStart + ",\n"
        + sampleBottomEnd.getDescription() + "=" + sampleBottomEnd
        + ",\n" + sampleTopStart.getDescription() + "=" + sampleTopStart
        + ",\n" + sampleTopEnd.getDescription() + "="
        + sampleTopEnd + ",\n" + finalStart.getDescription() + "="
        + finalStart + ",\n" + finalEnd.getDescription() + "=" + finalEnd + ",\n"
        + rotationAngleX.getDescription() + "=" + rotationAngleX
        + ",\n" + rotationAngleY.getDescription() + "="
        + rotationAngleY + ",\n" + rotationAngleZ.getDescription()
        + "=" + rotationAngleZ + ",\n" + xMax.getDescription()
        + "=" + xMax + ",\n" + yMax.getDescription() + "="
        + yMax + ",\n" + zMaxString + "=" + zMax;
  } 
  
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    rowNumber.store(props, prepend);
    xMax.store(props, prepend);
    yMax.store(props, prepend);
    props.setProperty(group + zMaxString, Integer.toString(zMax));
    props.setProperty(group + sectionString, section.getAbsolutePath());  
    sampleBottomStart.store(props, prepend);
    sampleBottomEnd.store(props, prepend);
    sampleTopStart.store(props, prepend);
    sampleTopEnd.store(props, prepend);
    finalStart.store(props, prepend);
    finalEnd.store(props, prepend);
    rotationAngleX.store(props, prepend);
    rotationAngleY.store(props, prepend);
    rotationAngleZ.store(props, prepend);
  }
  
  void remove(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    rowNumber.store(props, prepend);
    xMax.remove(props, prepend);
    yMax.remove(props, prepend);
    props.remove(group + zMaxString);
    props.remove(group + sectionString);  
    sampleBottomStart.remove(props, prepend);
    sampleBottomEnd.remove(props, prepend);
    sampleTopStart.remove(props, prepend);
    sampleTopEnd.remove(props, prepend);
    finalStart.remove(props, prepend);
    finalEnd.remove(props, prepend);
    rotationAngleX.remove(props, prepend);
    rotationAngleY.remove(props, prepend);
    rotationAngleZ.remove(props, prepend);
  }

  
  protected String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString + "." + rowNumber.toString();
    }
    return prepend + "." + groupString + "." + rowNumber.toString();
  }
  
  public boolean equals(ConstSectionTableRowData that) {
    if (!rowNumber.equals(that.rowNumber)) {
      return false;
    }
    if (!section.equals(that.section)) {
      return false;
    }
    if (!sampleBottomStart.equals(that.sampleBottomStart)) {
      return false;
    }
    if (!sampleBottomEnd.equals(that.sampleBottomEnd)) {
      return false;
    }
    if (!sampleTopStart.equals(that.sampleTopStart)) {
      return false;
    }
    if (!sampleTopEnd.equals(that.sampleTopEnd)) {
      return false;
    }
    if (!finalStart.equals(that.finalStart)) {
      return false;
    }
    if (!finalEnd.equals(that.finalEnd)) {
      return false;
    }
    if (!rotationAngleX.equals(that.rotationAngleX)) {
      return false;
    }
    if (!rotationAngleY.equals(that.rotationAngleY)) {
      return false;
    }
    if (!rotationAngleZ.equals(that.rotationAngleZ)) {
      return false;
    }
    return true;
  }
  
  public boolean equalsSample(ConstSectionTableRowData that) {
    if (!rowNumber.equals(that.rowNumber)) {
      return false;
    }
    if (!section.equals(that.section)) {
      return false;
    }
    if (!sampleBottomStart.equals(that.sampleBottomStart)) {
      return false;
    }
    if (!sampleBottomEnd.equals(that.sampleBottomEnd)) {
      return false;
    }
    if (!sampleTopStart.equals(that.sampleTopStart)) {
      return false;
    }
    if (!sampleTopEnd.equals(that.sampleTopEnd)) {
      return false;
    }
    if (!rotationAngleX.equals(that.rotationAngleX)) {
      return false;
    }
    if (!rotationAngleY.equals(that.rotationAngleY)) {
      return false;
    }
    if (!rotationAngleZ.equals(that.rotationAngleY)) {
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
    if (rowNumber.getInteger() < 0) {
      return -1;
    }
    return rowNumber.getInteger() - 1;
  }
  
  public File getSection() {
    return section;
  }
  
  public ConstEtomoNumber getXMax() {
    return xMax;
  }
  
  public ConstEtomoNumber getYMax() {
    return yMax;
  }
  
  public int getZMax() {
    return zMax;
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
  
  public int getSampleBottomNumberSlices() {
    int sampleBottomEnd = this.sampleBottomEnd.getInteger();
    int sampleBottomStart = this.sampleBottomStart.getInteger();
    if (sampleBottomEnd >= sampleBottomStart) {
      return sampleBottomEnd - sampleBottomStart + 1;
    }
    return 0;
  }
  
  public int getSampleTopNumberSlices() {
    int sampleTopEnd = this.sampleTopEnd.getInteger();
    int sampleTopStart = this.sampleTopStart.getInteger();
    if (sampleTopEnd >= sampleTopStart) {
      return sampleTopEnd - sampleTopStart + 1;
    }
    return 0;
  }
  
  public ConstEtomoNumber getChunkSize(int tableSize) {
    if (tableSize <= 1) {
      return new EtomoNumber(0);
    }
    if (rowNumber.equals(1)) {
      return new EtomoNumber(EtomoNumber.INTEGER_TYPE, getSampleTopNumberSlices());
    }
    if (rowNumber.equals(tableSize)) {
      return new EtomoNumber(EtomoNumber.INTEGER_TYPE, getSampleBottomNumberSlices());
    }
    return new EtomoNumber(EtomoNumber.INTEGER_TYPE, getSampleBottomNumberSlices() + getSampleTopNumberSlices());
  }
  
  public ConstEtomoNumber getFinalStart() {
    return finalStart;
  }
  
  public ConstEtomoNumber getFinalEnd() {
    return finalEnd;
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
