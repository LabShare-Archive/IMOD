package etomo.type;

import java.io.File;
import java.util.ArrayList;
import java.util.Properties;

import etomo.ui.JoinDialog;
import etomo.util.Utilities;

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
* <p> Revision 1.5  2004/12/16 02:27:49  sueh
* <p> bug# 564 Remove recommendedValue.  Use resetValue instead.
* <p>
* <p> Revision 1.4  2004/12/14 21:43:50  sueh
* <p> bug# 572:  Removing state object from meta data and managing it with a
* <p> manager class.  All state variables saved after a process is run belong in
* <p> the state object.
* <p>
* <p> Revision 1.3  2004/12/04 01:00:17  sueh
* <p> bug# 569 Fixed the check to see if working directory is empty in isValid()
* <p>
* <p> Revision 1.2  2004/11/19 23:33:42  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.22  2004/11/19 03:04:15  sueh
* <p> bug# 520 Setting displayDefault to default to true for the shift variables and
* <p> most of the spinners.
* <p>
* <p> Revision 1.1.2.21  2004/11/19 00:09:15  sueh
* <p> bug# 520 Fixed null pointer bug in store
* <p>
* <p> Revision 1.1.2.20  2004/11/17 02:22:20  sueh
* <p> bug# 520 Created a isValid() function that takes workingDirName, so the
* <p> working dir name in join dialog can be tested before it is placed in
* <p> propertyUserDir and used to create paramFile.
* <p>
* <p> Revision 1.1.2.19  2004/11/16 02:26:22  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.18  2004/11/15 22:20:26  sueh
* <p> bug# 520 Moved all of file validation to Utilities so that is called be called
* <p> from other places.
* <p>
* <p> Revision 1.1.2.17  2004/11/13 02:38:18  sueh
* <p> bug# 520 Added sampleProduced state boolean.
* <p>
* <p> Revision 1.1.2.16  2004/11/12 22:58:00  sueh
* <p> bug# 520 Added finishjoinTrial values:  binning, size, and shift.
* <p>
* <p> Revision 1.1.2.15  2004/11/11 01:37:13  sueh
* <p> bug# 520 Added useEveryNSlices and trialBinning.
* <p>
* <p> Revision 1.1.2.14  2004/11/08 22:22:09  sueh
* <p> bug# 520 Remove default from shift in X and Y.
* <p>
* <p> Revision 1.1.2.13  2004/10/29 22:12:55  sueh
* <p> bug# 520  Added removeSectionTableData() to remove section table data
* <p> rows from the meta data file before adding them.  This gets rid of deleted
* <p> rows.
* <p>
* <p> Revision 1.1.2.12  2004/10/29 01:19:29  sueh
* <p> bug# 520 Removing workingDir.  Calling isValid with workingDir.  Moving
* <p> file validations to Utilities.
* <p>
* <p> Revision 1.1.2.11  2004/10/25 23:09:16  sueh
* <p> bug# 520 Added get functions.
* <p>
* <p> Revision 1.1.2.10  2004/10/22 21:02:12  sueh
* <p> bug# 520 Simplifying by passing EtomoSimpleType instead of String and
* <p> int in get functions.
* <p>
* <p> Revision 1.1.2.9  2004/10/22 03:22:52  sueh
* <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
* <p> passing EtomoInteger, EtomoFloat, etc and using their get() and
* <p> getString() functions.
* <p>
* <p> Revision 1.1.2.8  2004/10/21 02:50:06  sueh
* <p> bug# 520 Added get functions.
* <p>
* <p> Revision 1.1.2.7  2004/10/18 18:01:46  sueh
* <p> bug# 520 Added fields from JoinDialog.  Converted densityRefSection to
* <p> an EtomoInteger.  Added validation checks for rootName and workingDir.
* <p>
* <p> Revision 1.1.2.6  2004/10/15 00:17:15  sueh
* <p> bug# 520 Added toString().  Fixed createPrepend().
* <p>
* <p> Revision 1.1.2.5  2004/10/14 02:28:12  sueh
* <p> bug# 520 Added getWorkingDir().
* <p>
* <p> Revision 1.1.2.4  2004/10/11 02:07:17  sueh
* <p> bug# 520 Fixed a bug in ConstMetaData where the open edf file menu
* <p> item wasn't working because it was validating the propertyUserDir of the
* <p> current manager, not the parent of the edf file being opened.  Now able
* <p> to pass in the edf file to get the parent from to use in validation.
* <p>
* <p> Revision 1.1.2.3  2004/10/06 01:54:45  sueh
* <p> bug# 520 Removed Use density reference checkbox.  Created
* <p> isValidForMakeSamples() which validates for the situation when Make
* <p> Samples is pressed.
* <p>
* <p> Revision 1.1.2.2  2004/10/01 19:45:26  sueh
* <p> bug# 520 Define a new join string that will go in the menu.  Set a file
* <p> extension value.
* <p>
* <p> Revision 1.1.2.1  2004/09/29 19:17:41  sueh
* <p> bug# 520 The const part of the JoinMetaData class.  Implements
* <p> storable with abstract load functions.  Contains member variables and
* <p> get functions.
* <p> </p>
*/
public abstract class ConstJoinMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  protected static final String latestRevisionNumber = "1.0";
  private static final String newJoinTitle = "New Join";

  protected static final String groupString = "Join";
  protected static final String sectionTableDataSizeString = "SectionTableDataSize";
  protected static final String rootNameString = "RootName";
  protected static final String fullLinearTransformationString = "FullLinearTransformation";
  protected static final String rotationTranslationMagnificationString = "RotationTranslationMagnification";
  protected static final String rotationTranslationString = "RotationTranslation";
  protected static final String useAlignmentRefSectionString = "UseAlignmentRefSection";

  protected static final boolean defaultFullLinearTransformation = true;

  protected ArrayList sectionTableData;
  protected String rootName;
  protected ScriptParameter densityRefSection = new ScriptParameter(
      EtomoNumber.INTEGER_TYPE, "DensityRefSection");
  protected ScriptParameter sigmaLowFrequency = new ScriptParameter(
      EtomoNumber.DOUBLE_TYPE, "SigmaLowFrequency");
  protected ScriptParameter cutoffHighFrequency = new ScriptParameter(
      EtomoNumber.DOUBLE_TYPE, "CutoffHighFrequency");
  protected ScriptParameter sigmaHighFrequency = new ScriptParameter(
      EtomoNumber.DOUBLE_TYPE, "SigmaHighFrequency");
  protected boolean fullLinearTransformation;
  protected boolean rotationTranslationMagnification;
  protected boolean rotationTranslation;
  protected boolean useAlignmentRefSection;
  protected ScriptParameter alignmentRefSection = new ScriptParameter(
      EtomoNumber.INTEGER_TYPE, "AlignmentRefSection");
  protected ScriptParameter sizeInX = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
      "SizeInX");
  protected ScriptParameter sizeInY = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
      "SizeInY");
  protected ScriptParameter shiftInX = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
      "ShiftInX");
  protected ScriptParameter shiftInY = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
      "ShiftInY");
  protected EtomoNumber useEveryNSlices = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "UseEveryNSlices");
  protected ScriptParameter trialBinning = new ScriptParameter(
      EtomoNumber.INTEGER_TYPE, "TrialBinning");  

  public abstract void load(Properties props);
  public abstract void load(Properties props, String prepend);

  public ConstJoinMetaData() {
    fileExtension = ".ejf";
    densityRefSection.setDefault(1).setDisplayValue(1);
    alignmentRefSection.setDefault(1).setDisplayValue(1);
    trialBinning.setDefault(1).setDisplayValue(1);
    shiftInX.setDefault(0).setDisplayValue(0);
    shiftInY.setDefault(0).setDisplayValue(0);
    sigmaLowFrequency.setDefault(0).setDisplayValue(0.0);
    cutoffHighFrequency.setDefault(0).setDisplayValue(0.25);
    sigmaHighFrequency.setDefault(0).setDisplayValue(0.05);
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    StringBuffer buffer = new StringBuffer(super.paramString()
        + ",\nlatestRevisionNumber=" + latestRevisionNumber
        + ",\nnewJoinTitle=" + newJoinTitle + ",\ngroupString=" + groupString
        + ",\n" + densityRefSection.getDescription() + "="
        + densityRefSection + ",\n" + rootNameString + "="
        + rootName + ",\n" + sigmaLowFrequency.getDescription() + "="
        + sigmaLowFrequency + ",\n"
        + cutoffHighFrequency.getDescription() + "="
        + cutoffHighFrequency + ",\n"
        + sigmaHighFrequency.getDescription() + "="
        + sigmaHighFrequency + ",\n"
        + fullLinearTransformationString + "=" + fullLinearTransformation
        + ",\n" + rotationTranslationMagnificationString + "="
        + rotationTranslationMagnification + ",\n" + rotationTranslationString
        + "=" + rotationTranslation + ",\n" + useAlignmentRefSectionString
        + "=" + useAlignmentRefSection + ",\n"
        + alignmentRefSection.getDescription() + "="
        + alignmentRefSection + ",\n" + sizeInX.getDescription()
        + "=" + sizeInX + ",\n" + sizeInY.getDescription() + "="
        + sizeInY + ",\n" + shiftInX.getDescription() + "="
        + shiftInX + ",\n" + shiftInY.getDescription() + "="
        + shiftInY + ",\n" + useEveryNSlices.getDescription() + "="
        + useEveryNSlices + ",\n" + trialBinning.getDescription() + "="
        + trialBinning);
    if (sectionTableData != null) {
      buffer.append(",\n" + sectionTableDataSizeString + "="
          + sectionTableData.size());
      for (int i = 0; i < sectionTableData.size(); i++) {
        ConstSectionTableRowData row = (ConstSectionTableRowData) sectionTableData
            .get(i);
        buffer.append(row.toString());
      }
    }
    return buffer.toString();
  } 

  public ArrayList getSectionTableData() {
    return sectionTableData;
  }

  public void store(Properties props, String prepend) {
    removeSectionTableData(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.setProperty(group + revisionNumberString, latestRevisionNumber);
    props.setProperty(group + rootNameString, rootName);
    densityRefSection.store(props, prepend);
    if (sectionTableData == null) {
      props.setProperty(group + sectionTableDataSizeString, "0");
    }
    else {
      props.setProperty(group + sectionTableDataSizeString, Integer
          .toString(sectionTableData.size()));
    }
    sigmaLowFrequency.store(props, prepend);
    cutoffHighFrequency.store(props, prepend);
    sigmaHighFrequency.store(props, prepend);
    props.setProperty(group + fullLinearTransformationString, Boolean.toString(fullLinearTransformation));
    props.setProperty(group + rotationTranslationMagnificationString, Boolean.toString(rotationTranslationMagnification));
    props.setProperty(group + rotationTranslationString, Boolean.toString(rotationTranslation));
    props.setProperty(group + useAlignmentRefSectionString, Boolean.toString(useAlignmentRefSection));
    alignmentRefSection.store(props, prepend);
    sizeInX.store(props, prepend);
    sizeInY.store(props, prepend);
    shiftInX.store(props, prepend);
    shiftInY.store(props, prepend);
    useEveryNSlices.store(props, prepend);
    trialBinning.store(props, prepend);
    
    if (sectionTableData != null) {
      for (int i = 0; i < sectionTableData.size(); i++) {
        ((SectionTableRowData) sectionTableData.get(i)).store(props, prepend);
      }
    }
  }
  
  public void removeSectionTableData(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    if (sectionTableData != null) {
      for (int i = 0; i < sectionTableData.size(); i++) {
        ((SectionTableRowData) sectionTableData.get(i)).remove(props, prepend);
      }
    }
  }

  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public boolean isValid(String workingDirName) {
    if (workingDirName == null || workingDirName.length() == 0 || workingDirName.matches("\\s+")) {
      invalidReason = "Working directory is not set.";
      return false;
    }
    return isValid(new File(workingDirName));
  }
  
  public boolean isValid(File workingDir) {
    StringBuffer invalidBuffer = new StringBuffer();
    if (!Utilities.isValidFile(workingDir, JoinDialog.WORKING_DIRECTORY_TEXT,
        invalidBuffer, true, true, true, true)) {
      invalidReason = invalidBuffer.toString();
      return false;
    }
    return isValid();
  }
  
  public boolean isValid() {
    if (rootName == null || !rootName.matches("\\S+")) {
      invalidReason = rootNameString + " is empty.";
      return false;
    }
    return true;
  }


  public boolean equals(Object object) {
    if (!super.equals(object)) {
      return false;
    }

    if (!(object instanceof ConstJoinMetaData))
      return false;
    ConstJoinMetaData that = (ConstJoinMetaData) object;

    if ((sectionTableData == null && that.sectionTableData != null)
        || (sectionTableData != null && that.sectionTableData == null)) {
      return false;
    }
    if (sectionTableData.size() != that.sectionTableData.size()) {
      return false;
    }

    for (int i = 0; i < sectionTableData.size(); i++) {
      if (!((SectionTableRowData) sectionTableData.get(i))
          .equals(sectionTableData.get(i))) {
        return false;
      }
    }
    return true;
  }

  public String getRootName() {
    return rootName;
  }
  
  public boolean isRootNameSet() {
    return rootName != null && rootName.matches("\\S+");
  }
  
  public ConstEtomoNumber getDensityRefSection() {
    return densityRefSection;
  }
  public ScriptParameter getDensityRefSectionParameter() {
    return densityRefSection;
  }
  
  public ConstEtomoNumber getUseEveryNSlices() {
    return useEveryNSlices;
  }
  
  public ConstEtomoNumber getTrialBinning() {
    return trialBinning;
  }
  public ScriptParameter getTrialBinningParameter() {
    return trialBinning;
  }

  public String getMetaDataFileName() {
    if (rootName.equals("")) {
      return null;
    }
    return rootName + fileExtension;
  }

  public String getName() {
    if (rootName.equals("")) {
      return newJoinTitle;
    }
    return rootName;
  }
  
  public ConstEtomoNumber getSigmaLowFrequency() {
    return sigmaLowFrequency;
  }
  public ScriptParameter getSigmaLowFrequencyParameter() {
    return sigmaLowFrequency;
  }
  
  public ConstEtomoNumber getCutoffHighFrequency() {
    return cutoffHighFrequency;
  }
  public ScriptParameter getCutoffHighFrequencyParameter() {
    return cutoffHighFrequency;
  }
  
  public ConstEtomoNumber getSigmaHighFrequency() {
    return sigmaHighFrequency;
  }
  public ScriptParameter getSigmaHighFrequencyParameter() {
    return sigmaHighFrequency;
  }
  
  public static String getNewFileTitle() {
    return newJoinTitle;
  }
  
  public static int getSize(int min, int max) {
    return max - min + 1;
  }
  
  public int getCoordinate(ConstEtomoNumber coordinate, JoinState state) {
    return coordinate.getInteger() * state.getTrialBinning().getInteger();
  }
  
  public boolean isFullLinearTransformation() {
    return fullLinearTransformation;
  }
  
  public boolean isRotationTranslationMagnification() {
    return rotationTranslationMagnification;
  }
  
  public boolean isRotationTranslation() {
    return rotationTranslation;
  }
  
  public boolean isUseAlignmentRefSection() {
    return useAlignmentRefSection;
  }
  
  public ConstEtomoNumber getAlignmentRefSection() {
    return alignmentRefSection;
  }
  
  public ConstEtomoNumber getSizeInX() {
    return sizeInX;
  }
  public ScriptParameter getSizeInXParameter() {
    return sizeInX;
  }
  
  public ConstEtomoNumber getSizeInY() {
    return sizeInY;
  }
  public ScriptParameter getSizeInYParameter() {
    return sizeInY;
  }
  
  public boolean getUseAlignmentRefSection() {
    return useAlignmentRefSection;
  }
  
  public ConstEtomoNumber getShiftInX() {
    return shiftInX;
  }
  public ScriptParameter getShiftInXParameter() {
    return shiftInX;
  }
  
  public ConstEtomoNumber getShiftInY() {
    return shiftInY;
  }
  public ScriptParameter getShiftInYParameter() {
    return shiftInY;
  }
  
}