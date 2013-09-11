package etomo.type;

import java.io.File;
import java.util.ArrayList;
import java.util.Properties;

import etomo.BaseManager;
import etomo.comscript.MakejoincomParam;
import etomo.ui.LogProperties;
import etomo.ui.swing.JoinDialog;
import etomo.ui.swing.UIHarness;
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
 * <p> Revision 1.20  2011/04/22 02:15:21  sueh
 * <p> bug# 1474 In getCoordinate throwing NullRequiredNumberException when coordinate or state.joinTrialBinning
 * <p> is null..
 * <p>
 * <p> Revision 1.19  2011/02/22 05:45:59  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.18  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.17  2010/02/26 20:38:20  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.16  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.15  2009/09/20 21:29:46  sueh
 * <p> bug# 1268 Reformatted.
 * <p>
 * <p> Revision 1.14  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2008/12/10 18:32:18  sueh
 * <p> bug# 1162 Added a manager stamp to setRootName.
 * <p>
 * <p> Revision 1.12  2008/08/18 22:38:00  sueh
 * <p> bug# 1130 Added localFits.
 * <p>
 * <p> Revision 1.11  2007/12/10 22:36:34  sueh
 * <p> bug# 1041 Made Const class an interface so inheritance can come from
 * <p> BaseMetaData.
 * <p>
 * <p> Revision 1.10  2007/07/30 22:39:52  sueh
 * <p> bug# 963 Added DatasetFiles.JOIN_DATA_FILE_EXT.
 * <p>
 * <p> Revision 1.9  2007/02/08 02:03:01  sueh
 * <p> bug# 962 Added rejoinTrialBinning and rejoinUseEveryNSlices.
 * <p>
 * <p> Revision 1.8  2007/02/05 23:27:58  sueh
 * <p> bug# 962 Added Model and Rejoin fields.
 * <p>
 * <p> Revision 1.7  2005/12/14 01:28:58  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.6  2005/11/29 22:34:24  sueh
 * <p> bug# 757 Added manager to SectionTableRowData.
 * <p>
 * <p> Revision 1.5  2005/11/02 23:59:43  sueh
 * <p> bug# 738 Added midas limit.
 * <p>
 * <p> Revision 1.4  2005/04/25 20:51:29  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 1.3  2004/12/14 21:45:49  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:35:18  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.12  2004/11/16 02:28:23  sueh
 * <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
 * <p> EtomoFloat, and EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.11  2004/11/15 22:24:04  sueh
 * <p> bug# 520 Added setSampleProduced().
 * <p>
 * <p> Revision 1.1.2.10  2004/11/13 02:38:28  sueh
 * <p> bug# 520 Added sampleProduced state boolean.
 * <p>
 * <p> Revision 1.1.2.9  2004/11/12 22:59:25  sueh
 * <p> bug# 520 Added finishjoinTrial values:  binning, size, and shift.
 * <p>
 * <p> Revision 1.1.2.8  2004/11/11 01:37:30  sueh
 * <p> bug# 520 Added useEveryNSlices and trialBinning.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/29 01:19:48  sueh
 * <p> bug# 520 Removing workingDir.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/22 21:06:25  sueh
 * <p> bug# 520 Changed offsetInX, Y to shiftInX, Y.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/18 18:05:52  sueh
 * <p> bug# 520 Added fields from JoinDialog.  Converted densityRefSection to
 * <p> an EtomoInteger.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/15 00:30:02  sueh
 * <p> bug# 520 Fixed load().  Added the rowNumber to the
 * <p> SectionTableRowData constructor because rowNumber is used to store
 * <p> values.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/08 16:23:40  sueh
 * <p> bug# 520 Make sure  sectionTableData exists before it is used.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/06 02:14:13  sueh
 * <p> bug# 520 Removed Use density reference checkbox.  Changed string
 * <p> default to "", since their default when coming from store() is "".  Added
 * <p> variables to the load() function.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 19:28:03  sueh
 * <p> bug# 520 Meta data for serial sections.  Non-const class implements load
 * <p> and set functions.
 * <p> </p>
 */
public final class JoinMetaData extends BaseMetaData implements ConstJoinMetaData {
  public static final String rcsid = "$Id$";

  /**
   * @deprecated
   */
  private static final String ALIGN_TRANFORM_KEY = "AlignTransform";
  // Version 1.0
  /**
   * @deprecated
   */
  private static final String fullLinearTransformationString = "FullLinearTransformation";
  /**
   * @deprecated
   */
  private static final String rotationTranslationMagnificationString = "RotationTranslationMagnification";
  /**
   * @deprecated
   */
  private static final String rotationTranslationString = "RotationTranslation";

  private static final EtomoVersion latestRevisionNumber = EtomoVersion.getInstance(
      revisionNumberString, "1.2");
  private static final String newJoinTitle = "New Join";

  private static final String groupString = "Join";
  private static final String sectionTableDataSizeString = "SectionTableDataSize";
  private static final String rootNameString = "RootName";
  private static final String useAlignmentRefSectionString = "UseAlignmentRefSection";
  private static final String REFINING_WITH_TRIAL_KEY = "RefiningWithTrial";
  private static final String MODEL_TRANFORM_KEY = "ModelTransform";
  private static final String BOUNDARIES_TO_ANALYZE_KEY = "BoundariesToAnalyze";
  private static final String OBJECTS_TO_INCLUDE_KEY = "ObjectsToInclude";
  private static final String BOUNDARY_ROW_KEY = "BoundaryRow";

  private ArrayList sectionTableData = null;
  private String rootName = "";
  private String boundariesToAnalyze = null;
  private String objectsToInclude = null;
  private ScriptParameter densityRefSection = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "DensityRefSection");
  /**
   * @deprecated
   */
  private ScriptParameter sigmaLowFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaLowFrequency");
  /**
   * @deprecated
   */
  private ScriptParameter cutoffHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "CutoffHighFrequency");
  /**
   * @deprecated
   */
  private ScriptParameter sigmaHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaHighFrequency");
  /**
   * @deprecated
   */
  private Transform alignTransform = Transform.DEFAULT;
  private Transform modelTransform = Transform.DEFAULT;
  private boolean useAlignmentRefSection = false;
  private ScriptParameter alignmentRefSection = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "AlignmentRefSection");
  private ScriptParameter sizeInX = new ScriptParameter(EtomoNumber.Type.INTEGER,
      "SizeInX");
  private ScriptParameter sizeInY = new ScriptParameter(EtomoNumber.Type.INTEGER,
      "SizeInY");
  private ScriptParameter shiftInX = new ScriptParameter(EtomoNumber.Type.INTEGER,
      "ShiftInX");
  private ScriptParameter shiftInY = new ScriptParameter(EtomoNumber.Type.INTEGER,
      "ShiftInY");
  // FinishJoin -local. Checkbox in Join tab.
  private final EtomoBoolean2 localFits = new EtomoBoolean2("LocalFits");
  private EtomoNumber useEveryNSlices = new EtomoNumber(EtomoNumber.Type.INTEGER,
      "UseEveryNSlices");
  private final ScriptParameter trialBinning = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "TrialBinning");
  private final ScriptParameter rejoinTrialBinning = new ScriptParameter(
      EtomoNumber.Type.INTEGER, "RejoinTrialBinning");
  private final EtomoNumber midasLimit = new EtomoNumber("MidasLimit");
  private final EtomoBoolean2 gap = new EtomoBoolean2("Gap");
  private final EtomoNumber gapStart = new EtomoNumber("GapStart");
  private final EtomoNumber gapEnd = new EtomoNumber("GapEnd");
  private final EtomoNumber gapInc = new EtomoNumber("GapInc");
  private final EtomoNumber pointsToFitMin = new EtomoNumber("PointsToFitMin");
  private final EtomoNumber pointsToFitMax = new EtomoNumber("PointsToFitMax");
  private final IntKeyList boundaryRowStartList = IntKeyList
      .getNumberInstance("BoundaryRow" + '.' + "StartList");
  private final IntKeyList boundaryRowEndList = IntKeyList
      .getNumberInstance("BoundaryRow" + '.' + "EndList");
  private EtomoNumber rejoinUseEveryNSlices = new EtomoNumber(EtomoNumber.Type.INTEGER,
      "RejoinUseEveryNSlices");
  private AutoAlignmentMetaData autoAlignmentMetaData = new AutoAlignmentMetaData();

  private final BaseManager manager;

  public JoinMetaData(BaseManager manager, final LogProperties logProperties) {
    super(logProperties);
    this.manager = manager;
    axisType = AxisType.SINGLE_AXIS;
    fileExtension = DataFileType.JOIN.extension;
    densityRefSection.setDefault(1).useDefaultAsDisplayValue();
    alignmentRefSection.setDefault(1).useDefaultAsDisplayValue();
    trialBinning.setDefault(1).useDefaultAsDisplayValue();
    rejoinTrialBinning.setDefault(1).useDefaultAsDisplayValue();
    shiftInX.setDefault(0).useDefaultAsDisplayValue();
    shiftInY.setDefault(0).useDefaultAsDisplayValue();
    midasLimit.setDisplayValue(MakejoincomParam.MIDAS_LIMIT_DEFAULT);
    gapStart.setDisplayValue(-4);
    gapEnd.setDisplayValue(8);
    gapInc.setDisplayValue(2);
    gap.setDisplayValue(true);
  }

  public String toString() {
    return "[rootName:" + rootName + "," + super.toString() + "]";
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    // reset
    sectionTableData = null;
    densityRefSection.reset();
    rootName = "";
    boundariesToAnalyze = null;
    objectsToInclude = null;
    gapStart.reset();
    gapEnd.reset();
    gapInc.reset();
    pointsToFitMin.reset();
    pointsToFitMax.reset();
    modelTransform = Transform.DEFAULT;
    useAlignmentRefSection = false;
    alignmentRefSection.reset();
    sizeInX.reset();
    sizeInY.reset();
    shiftInX.reset();
    shiftInY.reset();
    localFits.reset();
    localFits.reset();
    trialBinning.reset();
    rejoinTrialBinning.reset();
    gap.reset();
    boundaryRowStartList.reset();
    boundaryRowEndList.reset();
    // load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    revisionNumber.reset();
    revisionNumber.load(props, prepend);
    autoAlignmentMetaData.load(props, prepend);
    if (revisionNumber.le("1.1")) {
      sigmaLowFrequency.reset();
      cutoffHighFrequency.reset();
      sigmaHighFrequency.reset();
      alignTransform = Transform.DEFAULT;
      sigmaLowFrequency.load(props, prepend);
      autoAlignmentMetaData.setSigmaLowFrequency(sigmaLowFrequency);
      cutoffHighFrequency.load(props, prepend);
      autoAlignmentMetaData.setCutoffHighFrequency(cutoffHighFrequency);
      sigmaHighFrequency.load(props, prepend);
      autoAlignmentMetaData.setSigmaHighFrequency(sigmaHighFrequency);
      if (revisionNumber.le("1.0")) {
        // handling version 1.0
        loadVersion1_0(props, prepend);
      }
      else {
        autoAlignmentMetaData.setAlignTransform(Transform.load(props, prepend,
            ALIGN_TRANFORM_KEY, Transform.DEFAULT));
      }
    }
    modelTransform = Transform
        .load(props, prepend, MODEL_TRANFORM_KEY, Transform.DEFAULT);
    rootName = props.getProperty(group + rootNameString, "");
    boundariesToAnalyze = props.getProperty(group + BOUNDARIES_TO_ANALYZE_KEY);
    objectsToInclude = props.getProperty(group + OBJECTS_TO_INCLUDE_KEY);
    gapStart.load(props, prepend);
    gap.load(props, prepend);
    gapEnd.load(props, prepend);
    gapInc.load(props, prepend);
    pointsToFitMin.load(props, prepend);
    pointsToFitMax.load(props, prepend);
    densityRefSection.load(props, prepend);
    useAlignmentRefSection = Boolean.valueOf(
        props.getProperty(group + useAlignmentRefSectionString, "false")).booleanValue();
    alignmentRefSection.load(props, prepend);
    sizeInX.load(props, prepend);
    sizeInY.load(props, prepend);
    shiftInX.load(props, prepend);
    shiftInY.load(props, prepend);
    localFits.load(props, prepend);
    useEveryNSlices.load(props, prepend);
    trialBinning.load(props, prepend);
    rejoinTrialBinning.load(props, prepend);
    midasLimit.load(props, prepend);

    int sectionTableRowsSize = Integer.parseInt(props.getProperty(group
        + sectionTableDataSizeString, "-1"));
    if (sectionTableRowsSize < 1) {
      return;
    }
    sectionTableData = new ArrayList(sectionTableRowsSize);
    for (int i = 0; i < sectionTableRowsSize; i++) {
      SectionTableRowData row = new SectionTableRowData(manager, i + 1);
      row.load(props, prepend);
      int rowIndex = row.getRowIndex();
      if (rowIndex < 0) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Invalid row index: " + rowIndex
            + ".  Corrupted: " + DataFileType.JOIN.extension + " file.",
            "Corrupted File", AxisID.ONLY);
      }
      sectionTableData.add(row.getRowIndex(), row);
    }
    boundaryRowStartList.load(props, prepend);
    boundaryRowEndList.load(props, prepend);
    rejoinUseEveryNSlices.load(props, prepend);
  }

  private void loadVersion1_0(Properties props, String prepend) {
    String group = prepend + '.';
    if (Boolean.valueOf(
        props.getProperty(group + fullLinearTransformationString, "false"))
        .booleanValue()) {
      alignTransform = Transform.FULL_LINEAR_TRANSFORMATION;
    }
    else if (Boolean.valueOf(
        props.getProperty(group + rotationTranslationMagnificationString, "false"))
        .booleanValue()) {
      alignTransform = Transform.ROTATION_TRANSLATION_MAGNIFICATION;
    }
    else if (Boolean.valueOf(
        props.getProperty(group + rotationTranslationString, "false")).booleanValue()) {
      alignTransform = Transform.FULL_LINEAR_TRANSFORMATION;
    }
    else {
      alignTransform = Transform.DEFAULT;
    }
    autoAlignmentMetaData.setAlignTransform(alignTransform);
  }

  public void setDensityRefSection(Object densityRefSection) {
    this.densityRefSection.set((Integer) densityRefSection);
  }

  public void setUseEveryNSlices(Object useEveryNSlices) {
    this.useEveryNSlices.set((Integer) useEveryNSlices);
  }

  public void setRejoinUseEveryNSlices(Object rejoinUseEveryNSlices) {
    this.rejoinUseEveryNSlices.set((Integer) rejoinUseEveryNSlices);
  }

  public void setTrialBinning(Object trialBinning) {
    this.trialBinning.set((Integer) trialBinning);
  }

  public void setRejoinTrialBinning(Object rejoinTrialBinning) {
    this.rejoinTrialBinning.set((Integer) rejoinTrialBinning);
  }

  public void setGap(boolean gap) {
    this.gap.set(gap);
  }

  public void setRootName(String rootName) {
    this.rootName = rootName;
    Utilities.managerStamp(null, this.rootName);
  }

  public void setBoundariesToAnalyze(String boundariesToAnalyze) {
    this.boundariesToAnalyze = boundariesToAnalyze;
  }

  public void setObjectsToInclude(String objectsToInclude) {
    this.objectsToInclude = objectsToInclude;
  }

  public void setGapStart(String gapStart) {
    this.gapStart.set(gapStart);
  }

  public void setPointsToFitMax(String pointsToFitMax) {
    this.pointsToFitMax.set(pointsToFitMax);
  }

  public void setPointsToFitMin(String pointsToFitMin) {
    this.pointsToFitMin.set(pointsToFitMin);
  }

  public void setLocalFits(boolean input) {
    localFits.set(input);
  }

  public void setGapEnd(String gapEnd) {
    this.gapEnd.set(gapEnd);
  }

  public void setGapInc(String gapInc) {
    this.gapInc.set(gapInc);
  }

  public void resetSectionTableData() {
    sectionTableData = null;
  }

  public void setSectionTableData(ConstSectionTableRowData row) {
    if (sectionTableData == null) {
      sectionTableData = new ArrayList();
    }
    sectionTableData.add(row);
  }

  public void setMidasLimit(String midasLimit) {
    this.midasLimit.set(midasLimit);
  }

  public void setUseAlignmentRefSection(boolean useAlignmentRefSection) {
    this.useAlignmentRefSection = useAlignmentRefSection;
  }

  public void setAlignmentRefSection(Object alignmentRefSection) {
    this.alignmentRefSection.set((Integer) alignmentRefSection);
  }

  public void setModelTransform(Transform modelTransform) {
    this.modelTransform = modelTransform;
  }

  public ConstEtomoNumber setSizeInX(String sizeInX) {
    return this.sizeInX.set(sizeInX);
  }

  public void setSizeInY(String sizeInY) {
    this.sizeInY.set(sizeInY);
  }

  public void setShiftInX(String shiftInX) {
    this.shiftInX.set(shiftInX);
  }

  public void setShiftInY(String shiftInY) {
    this.shiftInY.set(shiftInY);
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
    super.store(props, prepend);
    removeSectionTableData(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    // removing data used in old versions of join meta data
    // change this this when there are more then one old version
    if (revisionNumber.le("1.1")) {
      sigmaLowFrequency.remove(props, prepend);
      cutoffHighFrequency.remove(props, prepend);
      sigmaHighFrequency.remove(props, prepend);
      if (revisionNumber.le("1.0")) {
        removeVersion1_0(props, prepend);
      }
      else {
        Transform.remove(props, prepend, ALIGN_TRANFORM_KEY);
      }
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
    gapStart.store(props, prepend);
    gapEnd.store(props, prepend);
    gapInc.store(props, prepend);
    pointsToFitMin.store(props, prepend);
    pointsToFitMax.store(props, prepend);
    densityRefSection.store(props, prepend);
    if (sectionTableData == null) {
      props.setProperty(group + sectionTableDataSizeString, "0");
    }
    else {
      props.setProperty(group + sectionTableDataSizeString,
          Integer.toString(sectionTableData.size()));
    }
    autoAlignmentMetaData.store(props, prepend);
    Transform.store(modelTransform, props, prepend, MODEL_TRANFORM_KEY);
    props.setProperty(group + useAlignmentRefSectionString,
        Boolean.toString(useAlignmentRefSection));
    alignmentRefSection.store(props, prepend);
    sizeInX.store(props, prepend);
    sizeInY.store(props, prepend);
    shiftInX.store(props, prepend);
    shiftInY.store(props, prepend);
    localFits.store(props, prepend);
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
    boundaryRowStartList.store(props, prepend);
    boundaryRowEndList.store(props, prepend);
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

  String getGroupKey() {
    return groupString;
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

    if (!(object instanceof JoinMetaData))
      return false;
    JoinMetaData that = (JoinMetaData) object;

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

  public IntKeyList.Walker getBoundaryRowStartListWalker() {
    return boundaryRowStartList.getWalker();
  }

  public void setBoundaryRowStart(int key, String start) {
    boundaryRowStartList.put(key, start);
  }

  public void resetBoundaryRowStartList() {
    boundaryRowStartList.reset();
  }

  public IntKeyList.Walker getBoundaryRowEndListWalker() {
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
    boundaryRowEndList.put(key, end);
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

  public ConstEtomoNumber getMidasLimit() {
    return midasLimit;
  }

  public String getName() {
    if (rootName.equals("")) {
      return newJoinTitle;
    }
    return rootName;
  }

  public String getDatasetName() {
    return rootName;
  }

  public AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return autoAlignmentMetaData;
  }

  public static String getNewFileTitle() {
    return newJoinTitle;
  }

  public static int getSize(int min, int max) {
    return max - min + 1;
  }

  public int getCoordinate(ConstEtomoNumber coordinate, JoinState state)
      throws NullRequiredNumberException {
    ConstEtomoNumber binning = state.getJoinTrialBinning();
    if (coordinate.isNull()) {
      throw new NullRequiredNumberException("Coordinate is null");
    }
    if (binning.isNull()) {
      throw new NullRequiredNumberException("Binning is null");
    }
    return coordinate.getInt() * binning.getInt();
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

  public boolean isLocalFits() {
    return localFits.is();
  }

  public ScriptParameter getShiftInYParameter() {
    return shiftInY;
  }
}