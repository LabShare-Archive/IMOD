package etomo.type;

import java.io.File;
import java.util.ArrayList;
import java.util.Properties;

import etomo.comscript.MakejoincomParam;
import etomo.ui.JoinDialog;
import etomo.util.DatasetFiles;
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
 * <p> Revision 1.13  2007/03/01 01:24:24  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.12  2007/02/08 02:02:39  sueh
 * <p> bug# 962 Added rejoinTrialBinning and rejoinUseEveryNSlices.
 * <p>
 * <p> Revision 1.11  2007/02/05 23:24:16  sueh
 * <p> bug# 962 Added Model and Rejoin fields.
 * <p>
 * <p> Revision 1.10  2005/12/14 01:28:27  sueh
 * <p> bug# 782 Updated toString().
 * <p>
 * <p> Revision 1.9  2005/11/02 23:59:35  sueh
 * <p> bug# 738 Added midas limit.
 * <p>
 * <p> Revision 1.8  2005/07/29 19:46:49  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.7  2005/05/10 02:24:23  sueh
 * <p> bug# 658 Added ScriptParameter.useDefaultAsDisplayValue() to set
 * <p> displayValue equal to defaultValue.  When default is used, these are
 * <p> usually the same.
 * <p>
 * <p> Revision 1.6  2005/01/25 21:58:38  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
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

  protected static final EtomoVersion latestRevisionNumber = EtomoVersion
      .getInstance(revisionNumberString, "1.1");
  private static final String newJoinTitle = "New Join";

  protected static final String groupString = "Join";
  protected static final String sectionTableDataSizeString = "SectionTableDataSize";
  protected static final String rootNameString = "RootName";
  protected static final String useAlignmentRefSectionString = "UseAlignmentRefSection";
  protected static final String REFINING_WITH_TRIAL_KEY = "RefiningWithTrial";
  protected static final String ALIGN_TRANFORM_KEY = "AlignTransform";
  protected static final String MODEL_TRANFORM_KEY = "ModelTransform";
  protected static final String BOUNDARIES_TO_ANALYZE_KEY = "BoundariesToAnalyze";
  protected static final String OBJECTS_TO_INCLUDE_KEY = "ObjectsToInclude";
  private static final String BOUNDARY_ROW_KEY="BoundaryRow";

  //Version 1.0
  protected static final String fullLinearTransformationString = "FullLinearTransformation";
  protected static final String rotationTranslationMagnificationString = "RotationTranslationMagnification";
  protected static final String rotationTranslationString = "RotationTranslation";

  public static final Transform TRANSFORM_DEFAULT = Transform.FULL_LINEAR_TRANSFORMATION;

  protected ArrayList sectionTableData;
  protected String rootName;
  protected String boundariesToAnalyze = null;
  protected String objectsToInclude = null;
  protected ScriptParameter densityRefSection = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "DensityRefSection");
  protected ScriptParameter sigmaLowFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaLowFrequency");
  protected ScriptParameter cutoffHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "CutoffHighFrequency");
  protected ScriptParameter sigmaHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaHighFrequency");
  protected Transform alignTransform = TRANSFORM_DEFAULT;
  protected Transform modelTransform = TRANSFORM_DEFAULT;
  protected boolean useAlignmentRefSection;
  protected ScriptParameter alignmentRefSection = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "AlignmentRefSection");
  protected ScriptParameter sizeInX = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "SizeInX");
  protected ScriptParameter sizeInY = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "SizeInY");
  protected ScriptParameter shiftInX = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "ShiftInX");
  protected ScriptParameter shiftInY = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "ShiftInY");
  protected EtomoNumber useEveryNSlices = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "UseEveryNSlices");
  protected final ScriptParameter trialBinning = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "TrialBinning");
  protected final ScriptParameter rejoinTrialBinning = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "RejoinTrialBinning");
  protected final EtomoNumber midasLimit = new EtomoNumber("MidasLimit");
  protected final EtomoBoolean2 gap = new EtomoBoolean2("Gap");
  protected final EtomoNumber gapStart = new EtomoNumber("GapStart");
  protected final EtomoNumber gapEnd = new EtomoNumber("GapEnd");
  protected final EtomoNumber gapInc= new EtomoNumber("GapInc");
  protected final EtomoNumber pointsToFitMin=new EtomoNumber("PointsToFitMin");
  protected final EtomoNumber pointsToFitMax=new EtomoNumber("PointsToFitMax");
  protected final IntKeyList boundaryRowStartList=IntKeyList.getNumberInstance("BoundaryRow"+'.'+"StartList");
  protected final IntKeyList boundaryRowEndList=IntKeyList.getNumberInstance("BoundaryRow"+'.'+"EndList");
  protected EtomoNumber rejoinUseEveryNSlices = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "RejoinUseEveryNSlices");

  public abstract void load(Properties props);

  public abstract void load(Properties props, String prepend);

  public ConstJoinMetaData() {
    axisType=AxisType.SINGLE_AXIS;
    fileExtension = DatasetFiles.JOIN_DATA_FILE_EXT;
    densityRefSection.setDefault(1).useDefaultAsDisplayValue();
    alignmentRefSection.setDefault(1).useDefaultAsDisplayValue();
    trialBinning.setDefault(1).useDefaultAsDisplayValue();
    rejoinTrialBinning.setDefault(1).useDefaultAsDisplayValue();
    shiftInX.setDefault(0).useDefaultAsDisplayValue();
    shiftInY.setDefault(0).useDefaultAsDisplayValue();
    sigmaLowFrequency.setDefault(0).setDisplayValue(0.0);
    cutoffHighFrequency.setDefault(0).setDisplayValue(0.25);
    sigmaHighFrequency.setDefault(0).setDisplayValue(0.05);
    midasLimit.setDisplayValue(MakejoincomParam.MIDAS_LIMIT_DEFAULT);
    gapStart.setDisplayValue(-4);
    gapEnd.setDisplayValue(8);
    gapInc.setDisplayValue(2);
    gap.setDisplayValue(true);
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "sectionTableData=" + sectionTableData + ",rootName=" + rootName
        + ",\ndensityRefSection=" + densityRefSection + ",sigmaLowFrequency="
        + sigmaLowFrequency + ",\ncutoffHighFrequency=" + cutoffHighFrequency
        + ",\nsigmaHighFrequency=" + sigmaHighFrequency
        + ",\nuseAlignmentRefSection=" + useAlignmentRefSection
        + ",\nalignmentRefSection=" + alignmentRefSection + ",\nsizeInX="
        + sizeInX + ",sizeInY=" + sizeInY + ",\nshiftInX=" + shiftInX
        + ",shiftInY=" + shiftInY + ",\nuseEveryNSlices=" + useEveryNSlices
        + ",trialBinning=" + trialBinning + ",\nmidasLimit=" + midasLimit
        + ",\nsuper[" + super.paramString() + "]";
  }

  public ArrayList getSectionTableData() {
    return sectionTableData;
  }

  /**
   * Remove data not used after version 1.0 of join meta data.
   * Assumes the that data has been loaded (see loadVersion1_0()).
   * @param props
   * @param prepend
   */
  private void removeVersion1_0(Properties props, String prepend) {
    String group = prepend + '.';
    props.remove(group + fullLinearTransformationString);
    props.remove(group + rotationTranslationMagnificationString);
    props.remove(group + rotationTranslationString);
  }

  public void store(Properties props, String prepend) {
    removeSectionTableData(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    //removing data used in old versions of join meta data
    //change this this when there are more then one old version
    if (revisionNumber.lt(latestRevisionNumber)) {
      removeVersion1_0(props, prepend);
    }
    latestRevisionNumber.store(props, prepend);
    props.setProperty(group + rootNameString, rootName);
    if (boundariesToAnalyze == null) {
      props.remove(group + BOUNDARIES_TO_ANALYZE_KEY);
    }
    else {
      props.setProperty(group + BOUNDARIES_TO_ANALYZE_KEY, boundariesToAnalyze);
    }
    if (objectsToInclude == null) {
      props.remove(group + OBJECTS_TO_INCLUDE_KEY);
    }
    else {
      props.setProperty(group + OBJECTS_TO_INCLUDE_KEY, objectsToInclude);
    }
    gapStart.store(props,prepend);
    gapEnd.store(props,prepend);
    gapInc.store(props,prepend);
    pointsToFitMin.store(props,prepend);
    pointsToFitMax.store(props,prepend);
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
    Transform.store(alignTransform, props, prepend, ALIGN_TRANFORM_KEY);
    Transform.store(modelTransform, props, prepend, MODEL_TRANFORM_KEY);
    props.setProperty(group + useAlignmentRefSectionString, Boolean
        .toString(useAlignmentRefSection));
    alignmentRefSection.store(props, prepend);
    sizeInX.store(props, prepend);
    sizeInY.store(props, prepend);
    shiftInX.store(props, prepend);
    shiftInY.store(props, prepend);
    useEveryNSlices.store(props, prepend);
    trialBinning.store(props, prepend);
    rejoinTrialBinning.store(props, prepend);
    gap.store(props, prepend);
    midasLimit.store(props, prepend);
    if (sectionTableData != null) {
      for (int i = 0; i < sectionTableData.size(); i++) {
        ((SectionTableRowData) sectionTableData.get(i)).store(props, prepend);
      }
    }
    boundaryRowStartList.store(props,prepend);
    boundaryRowEndList.store(props,prepend);
    rejoinUseEveryNSlices.store(props, prepend);
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
    if (workingDirName == null || workingDirName.length() == 0
        || workingDirName.matches("\\s+")) {
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
  
  public String getBoundariesToAnalyze() {
    return boundariesToAnalyze;
  }
  
  public String getObjectsToInclude() {
    return objectsToInclude;
  }
  
  public ConstEtomoNumber getGapStart() {
    return gapStart;
  }
  
  public ConstEtomoNumber getGapEnd() {
    return gapEnd;
  }
  
  public ConstEtomoNumber getGapInc() {
    return gapInc;
  }
  
  public ConstEtomoNumber getPointsToFitMin() {
    return pointsToFitMin;
  }
  
  public ConstEtomoNumber getPointsToFitMax() {
    return pointsToFitMax;
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
  
  public IntKeyList.Walker getBoundaryRowStartListWalker(){
    return boundaryRowStartList.getWalker();
  }
  
  public void setBoundaryRowStart(int key, String start) {
    boundaryRowStartList.put(key,start);
  }
  
  public void resetBoundaryRowStartList() {
    boundaryRowStartList.reset();
  }
  
  public IntKeyList.Walker getBoundaryRowEndListWalker(){
    return boundaryRowEndList.getWalker();
  }
  
  public boolean isBoundaryRowEndListEmpty() {
    return boundaryRowEndList.isEmpty();
  }
  
  public void resetBoundaryRowEndList() {
    boundaryRowEndList.reset();
  }
  
  public ConstEtomoNumber getBoundaryRowEnd(int key) {
    return boundaryRowEndList.getEtomoNumber(key);
  }
  
  public void setBoundaryRowEnd(int key, String end) {
    boundaryRowEndList.put(key,end);
  }

  public ConstEtomoNumber getUseEveryNSlices() {
    return useEveryNSlices;
  }
  
  public ConstEtomoNumber getRejoinUseEveryNSlices() {
    return rejoinUseEveryNSlices;
  }
  
  public ConstEtomoNumber getGap() {
    return gap;
  }

  public ConstEtomoNumber getTrialBinning() {
    return trialBinning;
  }

  public ScriptParameter getTrialBinningParameter() {
    return trialBinning;
  }
  
  public ConstEtomoNumber getRejoinTrialBinning() {
    return rejoinTrialBinning;
  }

  public ScriptParameter getRejoinTrialBinningParameter() {
    return rejoinTrialBinning;
  }

  public String getMetaDataFileName() {
    if (rootName.equals("")) {
      return null;
    }
    return rootName + fileExtension;
  }

  public final ConstEtomoNumber getMidasLimit() {
    return midasLimit;
  }
  
  public String getRootName() {
    return rootName;
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
    return coordinate.getInt() * state.getJoinTrialBinning().getInt();
  }

  public Transform getAlignTransform() {
    return alignTransform;
  }

  public Transform getModelTransform() {
    return modelTransform;
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