package etomo.type;

import java.io.File;
import java.util.Properties;

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
  
  public SectionTableRowData(int rowNumber) {
    reset();
    this.rowNumber.set(rowNumber);
  }
  
  public SectionTableRowData(ConstSectionTableRowData that) {
    super(that);
  }
  
  private void reset() {
    invalidReason = null;
    section = null;
    sampleBottomStart.reset();
    sampleBottomEnd.reset();
    sampleTopStart.reset();
    sampleTopEnd.reset();
    finalStart.reset();
    finalEnd.reset();
    rotationAngleX.reset();
    rotationAngleY.reset();
    rotationAngleZ.reset();
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

    rowNumber.load(props, prepend);
    xMax.load(props, prepend);
    yMax.load(props, prepend);
    zMax = Integer.parseInt(props.getProperty(group + zMaxString,
        Integer.toString(Integer.MIN_VALUE)));
    finalEnd.setDisplayValue(zMax);
    String sectionName = props.getProperty(group + sectionString, null);
    if (sectionName != null) {
      section = new File(sectionName);
    }
    sampleBottomStart.load(props, prepend);
    sampleBottomEnd.load(props, prepend);
    sampleTopStart.load(props, prepend);
    sampleTopEnd.load(props, prepend);
    finalStart.load(props, prepend);
    finalEnd.load(props, prepend);
    rotationAngleX.load(props, prepend);
    rotationAngleY.load(props, prepend);
    rotationAngleZ.load(props, prepend);
  }
  
  
  public void setRowNumber(int rowNumber) {
    this.rowNumber.set(rowNumber);
  }
  public void setRowNumber(String rowNumber) {
    this.rowNumber.set(rowNumber);
  }
  
  public void setSection(File section) {
    this.section = section;
  }
  
  public void setYMax(int xMax) {
    this.xMax.set(xMax);
  }
  
  public void setXMax(int yMax) {
    this.yMax.set(yMax);
  }
  
  public void setZMax(int zMax) {
    this.zMax = zMax;
    finalEnd.setDisplayValue(zMax);
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
  
  public ConstEtomoNumber setFinalStart(String finalStart) {
    return this.finalStart.set(finalStart);
  }
  
  public ConstEtomoNumber setFinalEnd(String finalEnd) {
    return this.finalEnd.set(finalEnd);
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