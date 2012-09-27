package etomo.type;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Properties;

import etomo.BaseManager;

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
 * <p> Revision 1.17  2011/04/22 02:15:53  sueh
 * <p> bug# 1474 In load, handling a damaged .efj file by looking for essential trial value under different groups.  Only loads when parameter is null.  Should handle backwards compatibility as well.s
 * <p>
 * <p> Revision 1.16  2011/02/22 05:46:19  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.15  2009/09/01 03:04:00  sueh
 * <p> bug# 1222 in isJoinVersionGe, changed minimumVersion parameter to
 * <p> EtomoVersion.
 * <p>
 * <p> Revision 1.14  2008/12/11 01:41:27  sueh
 * <p> bug# 1165 In load fixing previous .ejf files when possible.  Added version.
 * <p>
 * <p> Revision 1.13  2008/08/18 22:38:30  sueh
 * <p> bug# 1130 Added joinLocalFits and joinTrialLocalFits.
 * <p>
 * <p> Revision 1.12  2008/05/22 00:17:15  sueh
 * <p> bug# 1110 Adding reset call before setting any of the IntKeyLists to
 * <p> another IntKeyList.  Otherwise deleted members of the original IntKeyList
 * <p> are not removed.
 * <p>
 * <p> Revision 1.11  2007/12/10 22:36:45  sueh
 * <p> bug# 1041 Made Const class an interface so inheritance can come from
 * <p> BaseMetaData.
 * <p>
 * <p> Revision 1.10  2007/08/29 21:44:45  sueh
 * <p> bug# 1041 Made BaseState an abstract class.
 * <p>
 * <p> Revision 1.9  2007/06/08 23:57:52  sueh
 * <p> bug# 995 Added refineTrialUseEveryNSlices.
 * <p>
 * <p> Revision 1.8  2007/03/01 01:25:47  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.7  2007/02/08 02:03:32  sueh
 * <p> bug# 962 removed unnecessary print.
 * <p>
 * <p> Revision 1.6  2007/02/05 23:29:07  sueh
 * <p> bug# 962 Added finishjoin state fields.
 * <p>
 * <p> Revision 1.5  2006/04/06 20:12:55  sueh
 * <p> bug# 808 Added rotationAnglesList, a sparsely populated array.  Added
 * <p> functionality to delete and move a row.  Added revert functionality.
 * <p>
 * <p> Revision 1.4  2005/12/16 18:26:06  sueh
 * <p> bug# 785 Added doneMode.
 * <p>
 * <p> Revision 1.3  2005/12/14 01:29:10  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.2  2005/07/29 19:47:24  sueh
 * <p> bug# 692 Added tests.  Changed ConstEtomoNumber.getInteger() to
 * <p> getInt.
 * <p>
 * <p> Revision 1.1  2004/12/14 21:47:35  sueh
 * <p> bug# 572 Represents the state of the join.  Contains items saved after
 * <p> processes are run.
 * <p> </p>
 */
public final class JoinState extends BaseState implements ConstJoinState {
  public static final String rcsid = "$Id$";

  public static final String ROTATION_ANGLE_X = "RotationAngleX";
  public static final String ROTATION_ANGLE_Y = "RotationAngleY";
  public static final String ROTATION_ANGLE_Z = "RotationAngleZ";
  public static final String GAPS_EXIST_KEY = "GapsExist";
  private static final String JOIN_KEY = "Join";
  private static final String TRIAL_KEY = "Trial";
  private static final String REFINE_KEY = "Refine";
  private static final String START_LIST_KEY = "StartList";
  private static final String END_LIST_KEY = "EndList";
  private static final String ALIGNMENT_REF_SECTION_KEY = "AlignmentRefSection";
  private static final String SHIFT_IN_X_KEY = "ShiftInX";
  private static final String SHIFT_IN_Y_KEY = "ShiftInY";
  private static final String SIZE_IN_X_KEY = "SizeInX";
  private static final String SIZE_IN_Y_KEY = "SizeInY";
  private static final String LOCAL_FITS_KEY = "LocalFits";
  private static final String USE_EVERY_N_SLICES_KEY = "UseEveryNSlices";
  public static final EtomoVersion MIN_REFINE_VERSION = EtomoVersion
      .getDefaultInstance("1.1");
  private static final String CURRENT_VERSION = "1.0";

  protected static final String sampleProducedString = "SampleProduced";
  protected static final boolean defaultSampleProduced = false;

  private static final String groupString = "JoinState";
  private static final String VERSION = "1.1";
  private static final String XFMODEL_INPUT_FILE = ProcessName.XFMODEL.toString()
      + "InputFile";
  private static final String XFMODEL_OUTPUT_FILE = ProcessName.XFMODEL.toString()
      + "OutputFile";

  private final EtomoNumber doneMode = new EtomoNumber("DoneMode");

  //set on the successful completion of finishjoin

  private final BaseManager manager;

  private Hashtable rotationAnglesList = null;
  private Hashtable revertRotationAnglesList = null;

  private final EtomoNumber totalRows = new EtomoNumber("TotalRows");
  private final EtomoNumber revertTotalRows = new EtomoNumber();

  //state variable for join setup tab
  protected boolean sampleProduced;
  private EtomoBoolean2 gapsExist = null;
  private final EtomoBoolean2 refineTrial = new EtomoBoolean2(REFINE_KEY + '.'
      + TRIAL_KEY);

  private final EtomoVersion joinVersion = EtomoVersion.getEmptyInstance(JOIN_KEY + '.'
      + EtomoVersion.DEFAULT_KEY);
  private final IntKeyList joinStartList = IntKeyList.getNumberInstance(JOIN_KEY + '.'
      + START_LIST_KEY);
  private final IntKeyList joinEndList = IntKeyList.getNumberInstance(JOIN_KEY + '.'
      + END_LIST_KEY);
  private final EtomoNumber joinAlignmentRefSection = new EtomoNumber(JOIN_KEY + '.'
      + ALIGNMENT_REF_SECTION_KEY);
  private final ScriptParameter joinShiftInX = new ScriptParameter(JOIN_KEY + '.'
      + SHIFT_IN_X_KEY);
  private final ScriptParameter joinShiftInY = new ScriptParameter(JOIN_KEY + '.'
      + SHIFT_IN_Y_KEY);
  private final ScriptParameter joinSizeInX = new ScriptParameter(JOIN_KEY + '.'
      + SIZE_IN_X_KEY);
  private final ScriptParameter joinSizeInY = new ScriptParameter(JOIN_KEY + '.'
      + SIZE_IN_Y_KEY);
  private final EtomoBoolean2 joinLocalFits = new EtomoBoolean2(JOIN_KEY + '.'
      + LOCAL_FITS_KEY);

  private final EtomoVersion joinTrialVersion = EtomoVersion.getEmptyInstance(JOIN_KEY
      + '.' + TRIAL_KEY + '.' + EtomoVersion.DEFAULT_KEY);
  private final IntKeyList joinTrialStartList = IntKeyList.getNumberInstance(JOIN_KEY
      + '.' + TRIAL_KEY + '.' + START_LIST_KEY);
  private final IntKeyList joinTrialEndList = IntKeyList.getNumberInstance(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + END_LIST_KEY);
  private final EtomoNumber joinTrialAlignmentRefSection = new EtomoNumber(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + ALIGNMENT_REF_SECTION_KEY);
  private final ScriptParameter joinTrialShiftInX = new ScriptParameter(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + SHIFT_IN_X_KEY);
  private final ScriptParameter joinTrialShiftInY = new ScriptParameter(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + SHIFT_IN_Y_KEY);
  private final ScriptParameter joinTrialSizeInX = new ScriptParameter(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + SIZE_IN_X_KEY);
  private final ScriptParameter joinTrialSizeInY = new ScriptParameter(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + SIZE_IN_Y_KEY);
  private final EtomoBoolean2 joinTrialLocalFits = new EtomoBoolean2(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + LOCAL_FITS_KEY);
  private final EtomoNumber joinTrialBinning = new EtomoNumber(JOIN_KEY + '.' + TRIAL_KEY
      + '.' + "Binning");
  private final IntKeyList refineStartList = IntKeyList.getNumberInstance(REFINE_KEY
      + '.' + START_LIST_KEY);
  private final IntKeyList refineEndList = IntKeyList.getNumberInstance(REFINE_KEY + '.'
      + END_LIST_KEY);
  private String xfModelOutputFile = null;
  private boolean debug = false;
  private final EtomoNumber joinTrialUseEveryNSlices = new EtomoNumber(JOIN_KEY + '.'
      + TRIAL_KEY + '.' + USE_EVERY_N_SLICES_KEY);
  private final EtomoNumber refineTrialUseEveryNSlices = new EtomoNumber(REFINE_KEY + '.'
      + TRIAL_KEY + '.' + USE_EVERY_N_SLICES_KEY);
  private final EtomoVersion version = EtomoVersion.getDefaultInstance();

  public JoinState(BaseManager manager) {
    this.manager = manager;
  }

  public String toString() {
    return "[joinVersion=" + joinVersion + ",joinSizeInX=" + joinSizeInX + "]";
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    joinVersion.store(props, prepend);
    joinTrialVersion.store(props, prepend);
    doneMode.store(props, prepend);
    props.setProperty(group + sampleProducedString, Boolean.toString(sampleProduced));
    totalRows.store(props, prepend);
    //store the rotation angles under their current row number
    if (rotationAnglesList != null) {
      for (int i = 0; i < totalRows.getInt(); i++) {
        SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList
            .get(new Integer(i));
        if (rotationAngles != null) {
          rotationAngles.store(props,
              SectionTableRowData.createPrepend(prepend, new EtomoNumber().set(i + 1)));
        }
      }
    }
    EtomoBoolean2.store(gapsExist, props, prepend, GAPS_EXIST_KEY);
    refineTrial.store(props, prepend);
    joinAlignmentRefSection.store(props, prepend);
    joinShiftInX.store(props, prepend);
    joinShiftInY.store(props, prepend);
    joinSizeInX.store(props, prepend);
    joinSizeInY.store(props, prepend);
    joinLocalFits.store(props, prepend);
    joinStartList.store(props, prepend);
    joinEndList.store(props, prepend);

    joinTrialAlignmentRefSection.store(props, prepend);
    joinTrialShiftInX.store(props, prepend);
    joinTrialShiftInY.store(props, prepend);
    joinTrialSizeInX.store(props, prepend);
    joinTrialSizeInY.store(props, prepend);
    joinTrialLocalFits.store(props, prepend);
    joinTrialBinning.store(props, prepend);
    joinTrialStartList.store(props, prepend);
    joinTrialEndList.store(props, prepend);
    joinTrialUseEveryNSlices.store(props, prepend);
    refineTrialUseEveryNSlices.store(props, prepend);
    refineStartList.store(props, prepend);
    refineEndList.store(props, prepend);
    if (xfModelOutputFile == null) {
      props.remove(prepend + '.' + XFMODEL_OUTPUT_FILE);
    }
    else {
      props.setProperty(prepend + '.' + XFMODEL_OUTPUT_FILE, xfModelOutputFile);
    }
    version.set(CURRENT_VERSION);
    version.store(props, prepend);
  }

  public boolean equals(JoinState that) {
    if (!super.equals(that)) {
      return false;
    }
    return true;
  }

  String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    //reset
    doneMode.reset();
    sampleProduced = defaultSampleProduced;
    gapsExist = null;
    refineTrial.reset();
    joinVersion.reset();
    joinStartList.reset();
    joinEndList.reset();
    joinAlignmentRefSection.reset();
    joinShiftInX.reset();
    joinShiftInY.reset();
    joinSizeInX.reset();
    joinSizeInY.reset();
    joinLocalFits.reset();
    joinTrialVersion.reset();
    joinTrialStartList.reset();
    joinTrialEndList.reset();
    joinTrialAlignmentRefSection.reset();
    joinTrialShiftInX.reset();
    joinTrialShiftInY.reset();
    joinTrialSizeInX.reset();
    joinTrialSizeInY.reset();
    joinTrialLocalFits.reset();
    joinTrialBinning.reset();
    refineStartList.reset();
    refineEndList.reset();
    joinTrialUseEveryNSlices.reset();
    refineTrialUseEveryNSlices.reset();
    version.reset();
    //load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    joinVersion.load(props, prepend);
    joinTrialVersion.load(props, prepend);
    if (!joinVersion.isNull() && isJoinVersionGe(false, MIN_REFINE_VERSION)) {
      joinAlignmentRefSection.load(props, prepend);
      joinShiftInX.load(props, prepend);
      joinShiftInY.load(props, prepend);
      joinSizeInX.load(props, prepend);
      joinSizeInY.load(props, prepend);
      joinStartList.load(props, prepend);
      joinEndList.load(props, prepend);
    }
    if (!joinTrialVersion.isNull() && isJoinVersionGe(true, MIN_REFINE_VERSION)) {
    joinTrialAlignmentRefSection.load(props, prepend);
    joinTrialShiftInX.load(props, prepend);
    joinTrialShiftInY.load(props, prepend);
    joinTrialSizeInX.load(props, prepend);
    joinTrialSizeInY.load(props, prepend);
    joinTrialBinning.load(props, prepend);
    joinTrialStartList.load(props, prepend);
    joinTrialEndList.load(props, prepend);
    joinTrialUseEveryNSlices.load(props, prepend);
    }
    else {
      loadJoinTrialVersion1_0(props, prepend);
    }
    joinLocalFits.load(props, prepend);
    joinTrialLocalFits.load(props, prepend);
    doneMode.load(props, prepend);
    sampleProduced = Boolean.valueOf(
        props.getProperty(group + sampleProducedString,
            Boolean.toString(defaultSampleProduced))).booleanValue();
    totalRows.load(props, prepend);
    //retrieve the rotation angles by row number
    rotationAnglesList = null;
    if (!totalRows.isNull()) {
      for (int i = 0; i < totalRows.getInt(); i++) {
        SlicerAngles rotationAngles = new SlicerAngles();
        rotationAngles.load(props,
            SectionTableRowData.createPrepend(prepend, new EtomoNumber().set(i + 1)));
        if (!rotationAngles.isEmpty()) {
          if (rotationAnglesList == null) {
            rotationAnglesList = new Hashtable();
          }
          rotationAnglesList.put(new Integer(i), rotationAngles);
        }
      }
    }
    gapsExist = EtomoBoolean2.load(gapsExist, GAPS_EXIST_KEY, props, prepend);
    refineTrial.load(props, prepend);
    refineTrialUseEveryNSlices.load(props, prepend);
    refineStartList.load(props, prepend);
    refineEndList.load(props, prepend);
    xfModelOutputFile = props.getProperty(prepend + '.' + XFMODEL_OUTPUT_FILE);
    version.load(props, prepend);
    //Bug# 1165 - fixing previous .ejf files when possible.
    if (version.lt(EtomoVersion.getDefaultInstance("1.0"))) {
      if (joinTrialBinning.isNull()) {
        joinTrialAlignmentRefSection.load(props, prepend);
        if (!joinTrialAlignmentRefSection.isNull()) {
          joinAlignmentRefSection.set(joinTrialAlignmentRefSection);
          joinTrialAlignmentRefSection.reset();

        }
      }
    }
  }

  /**
   * Returns true if version is the minimum version required to do a refine
   * @param trial
   * @param minimumVersion
   * @return
   */
  public boolean isJoinVersionGe(boolean trial, EtomoVersion minimumVersion) {
    if (trial) {
      return joinTrialVersion.ge(minimumVersion);
    }
    return joinVersion.ge(minimumVersion);
  }

  /**
   * Loads JoinState version 0.0 data into trialFinishjoin.  There is no
   * version 0.0 data available for finishjoin.
   * @param props
   * @param prepend
   */
  private void loadJoinTrialVersion1_0(Properties props, String prepend) {
    prepend = "JoinState.Trial";
    String key = prepend + "Binning";
    joinTrialBinning.set(props.getProperty(key));
    props.remove(key);
    key = prepend + "ShiftInX";
    joinTrialShiftInX.set(props.getProperty(key));
    props.remove(key);
    key = prepend + "ShiftInY";
    joinTrialShiftInY.set(props.getProperty(key));
    props.remove(key);
    key = prepend + "SizeInX";
    joinTrialSizeInX.set(props.getProperty(key));
    props.remove(key);
    key = prepend + "SizeInY";
    joinTrialSizeInY.set(props.getProperty(key));
    props.remove(key);
  }

  public void setCurrentJoinVersion(boolean trial) {
    if (trial) {
      joinTrialVersion.set(VERSION);
    }
    else {
      joinVersion.set(VERSION);
    }
  }

  /**
   * Sets refine parameters from meta data.  Assumes that
   * some of the trial parameters values where already set by loadJoinTrialVersion1_0().
   * Sets the refine version or the refine trial version of to the current version.
   * @param trial
   * @param metaData
   */
  public void setJoinVersion1_0(boolean trial, JoinMetaData metaData) {
    ArrayList dataList = metaData.getSectionTableData();
    int size = dataList.size();
    setCurrentJoinVersion(trial);
    setJoinAlignmentRefSection(trial, metaData.getAlignmentRefSection());
    if (trial) {
      joinTrialStartList.reset();
      joinTrialEndList.reset();
      for (int i = 0; i < size; i++) {
        SectionTableRowData data = (SectionTableRowData) dataList.get(i);
        joinTrialStartList.put(i, data.getJoinFinalStart());
        joinTrialEndList.put(i, data.getJoinFinalEnd());
      }
      joinTrialUseEveryNSlices.set(metaData.getUseEveryNSlices());
    }
    else {
      joinStartList.reset();
      joinEndList.reset();
      for (int i = 0; i < size; i++) {
        SectionTableRowData data = (SectionTableRowData) dataList.get(i);
        joinStartList.put(i, data.getJoinFinalStart());
        joinEndList.put(i, data.getJoinFinalEnd());
      }
      joinShiftInX.set(metaData.getShiftInX());
      joinShiftInY.set(metaData.getShiftInY());
      joinSizeInX.set(metaData.getSizeInX());
      joinSizeInY.set(metaData.getSizeInY());
    }
  }

  public void setRefineTrial(boolean trial) {
    refineTrial.set(trial);
  }

  public int getNewShiftInX(int min, int max) throws NullRequiredNumberException {
    if (joinTrialShiftInX.isNull()) {
      throw new NullRequiredNumberException(joinTrialShiftInY.getName() + " is null.");
    }
    if (joinTrialSizeInX.isNull()) {
      throw new NullRequiredNumberException(joinTrialSizeInY.getName() + " is null.");
    }
    return joinTrialShiftInX.getInt() + (joinTrialSizeInX.getInt() + 1) / 2 - (max + min)
        / 2;
  }

  public SlicerAngles getRotationAngles(Integer index) {
    if (rotationAnglesList == null) {
      return null;
    }
    return (SlicerAngles) rotationAnglesList.get(index);
  }

  public ConstEtomoNumber getRefineTrial() {
    return refineTrial;
  }

  public IntKeyList.Walker getJoinStartListWalker(boolean trial) {
    if (trial) {
      return joinTrialStartList.getWalker();
    }
    return joinStartList.getWalker();
  }

  public IntKeyList.Walker getJoinEndListWalker(boolean trial) {
    if (trial) {
      return joinTrialEndList.getWalker();
    }
    return joinEndList.getWalker();
  }

  public IntKeyList.Walker getRefineStartListWalker() {
    return refineStartList.getWalker();
  }

  public IntKeyList.Walker getRefineEndListWalker() {
    return refineEndList.getWalker();
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  public boolean isRefineStartListEmpty() {
    return refineStartList.isEmpty();
  }

  public void setJoinSizeInX(boolean trial, ConstEtomoNumber sizeInX) {
    if (trial) {
      joinTrialSizeInX.set(sizeInX);
    }
    else {
      joinSizeInX.set(sizeInX);
    }
  }

  public void setJoinSizeInY(boolean trial, ConstEtomoNumber sizeInY) {
    if (trial) {
      joinTrialSizeInY.set(sizeInY);
    }
    else {
      joinSizeInY.set(sizeInY);
    }
  }

  public void setJoinShiftInX(boolean trial, ConstEtomoNumber shiftInX) {
    if (trial) {
      joinTrialShiftInX.set(shiftInX);
    }
    else {
      joinShiftInX.set(shiftInX);
    }
  }

  public void setJoinShiftInY(boolean trial, ConstEtomoNumber shiftInY) {
    if (trial) {
      joinTrialShiftInY.set(shiftInY);
    }
    else {
      joinShiftInY.set(shiftInY);
    }
  }

  public void setJoinLocalFits(boolean trial, ConstEtomoNumber localFits) {
    if (trial) {
      joinTrialLocalFits.set(localFits);
    }
    else {
      joinLocalFits.set(localFits);
    }
  }

  public void setJoinStartList(boolean trial, ConstIntKeyList startList) {
    if (trial) {
      joinTrialStartList.reset();
      joinTrialStartList.set(startList);
    }
    else {
      joinStartList.reset();
      joinStartList.set(startList);
    }
  }

  public void setJoinTrialUseEveryNSlices(ConstEtomoNumber useEveryNSlices) {
    joinTrialUseEveryNSlices.set(useEveryNSlices);
  }

  public void setRefineTrialUseEveryNSlices(ConstEtomoNumber useEveryNSlices) {
    refineTrialUseEveryNSlices.set(useEveryNSlices);
  }

  public ConstEtomoNumber getJoinTrialUseEveryNSlices() {
    return joinTrialUseEveryNSlices;
  }

  public ConstEtomoNumber getRefineTrialUseEveryNSlices() {
    return refineTrialUseEveryNSlices;
  }

  public void setRefineStartList(ConstIntKeyList startList) {
    refineStartList.reset();
    refineStartList.set(startList);
  }

  public void setRefineEndList(ConstIntKeyList endList) {
    refineEndList.reset();
    refineEndList.set(endList);
  }

  public void setJoinEndList(boolean trial, ConstIntKeyList endList) {
    if (trial) {
      joinTrialEndList.reset();
      joinTrialEndList.set(endList);
    }
    else {
      joinEndList.reset();
      joinEndList.set(endList);
    }
  }

  public ConstEtomoNumber getJoinSizeInX(boolean trial) {
    if (trial) {
      return joinTrialSizeInX;
    }
    return joinSizeInX;
  }

  public ScriptParameter getJoinSizeInXParameter(boolean trial) {
    if (trial) {
      return joinTrialSizeInX;
    }
    return joinSizeInX;
  }

  public ConstEtomoNumber getJoinSizeInY(boolean trial) {
    if (trial) {
      return joinTrialSizeInY;
    }
    return joinSizeInY;
  }

  public ScriptParameter getJoinSizeInYParameter(boolean trial) {
    if (trial) {
      return joinTrialSizeInY;
    }
    return joinSizeInY;
  }

  public boolean isJoinLocalFits(boolean trial) {
    if (trial) {
      return joinTrialLocalFits.is();
    }
    return joinLocalFits.is();
  }

  public ConstEtomoNumber getJoinShiftInX(boolean trial) {
    if (trial) {
      return joinTrialShiftInX;
    }
    return joinShiftInX;
  }

  public ScriptParameter getJoinShiftInXParameter(boolean trial) {
    if (trial) {
      return joinTrialShiftInX;
    }
    return joinShiftInX;
  }

  public ConstEtomoNumber getJoinShiftInY(boolean trial) {
    if (trial) {
      return joinTrialShiftInY;
    }
    return joinShiftInY;
  }

  public ScriptParameter getJoinShiftInYParameter(boolean trial) {
    if (trial) {
      return joinTrialShiftInY;
    }
    return joinShiftInY;
  }

  public ConstEtomoNumber getJoinTrialBinning() {
    return joinTrialBinning;
  }

  public void setJoinTrialBinning(ConstEtomoNumber binning) {
    joinTrialBinning.set(binning);
  }

  public void setJoinAlignmentRefSection(boolean trial,
      ConstEtomoNumber alignmentRefSection) {
    if (trial) {
      joinTrialAlignmentRefSection.set(alignmentRefSection);
    }
    else {
      joinAlignmentRefSection.set(alignmentRefSection);
    }
  }

  public ConstEtomoNumber getJoinAlignmentRefSection(boolean trial) {
    if (trial) {
      return joinTrialAlignmentRefSection;
    }
    return joinAlignmentRefSection;
  }

  /**
   * calculate shift in y
   * @param min
   * @param max
   * @return
   */
  public int getNewShiftInY(int min, int max) throws NullRequiredNumberException {
    if (joinTrialShiftInY.isNull()) {
      throw new NullRequiredNumberException(joinTrialShiftInY.getName() + " is null.");
    }
    if (joinTrialSizeInY.isNull()) {
      throw new NullRequiredNumberException(joinTrialSizeInY.getName() + " is null.");
    }
    return joinTrialShiftInY.getInt() + (joinTrialSizeInY.getInt() + 1) / 2 - (max + min)
        / 2;
  }

  public boolean isSampleProduced() {
    return sampleProduced;
  }

  public void setGapsExist(boolean gapsExist) {
    this.gapsExist = EtomoBoolean2.set(this.gapsExist, gapsExist, GAPS_EXIST_KEY);
  }

  public boolean isGapsExist() {
    if (gapsExist == null) {
      return false;
    }
    return gapsExist.is();
  }

  public void setRevertState(boolean enableRevert) {
    if (enableRevert && rotationAnglesList != null) {
      revertRotationAnglesList = (Hashtable) rotationAnglesList.clone();
      revertTotalRows.set(totalRows);
    }
    else {
      revertRotationAnglesList = null;
      revertTotalRows.set("");
    }
  }

  public void revert() {
    rotationAnglesList = revertRotationAnglesList;
    totalRows.set(revertTotalRows);
  }

  public void deleteRow(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex >= totalRows.getInt()) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows=" + totalRows);
    }
    //delete row
    Integer prevIndex = new Integer(rowIndex);
    rotationAnglesList.remove(prevIndex);
    //move the other rows up one row
    for (int i = rowIndex + 1; i < totalRows.getInt(); i++) {
      Integer curIndex = new Integer(i);
      SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList.remove(curIndex);
      if (rotationAngles != null) {
        rotationAnglesList.put(prevIndex, rotationAngles);
      }
      prevIndex = curIndex;
    }
  }

  public void moveRowUp(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex == 0 || rowIndex >= totalRows.getInt()) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows=" + totalRows);
    }
    Integer curIndex = new Integer(rowIndex);
    Integer prevIndex = new Integer(rowIndex - 1);
    //swap the current row with the one above it
    SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList.remove(curIndex);
    SlicerAngles prevRotationAngles;
    if (rotationAngles != null) {
      prevRotationAngles = (SlicerAngles) rotationAnglesList.put(prevIndex,
          rotationAngles);
    }
    else {
      prevRotationAngles = (SlicerAngles) rotationAnglesList.remove(prevIndex);
    }
    if (prevRotationAngles != null) {
      rotationAnglesList.put(curIndex, prevRotationAngles);
    }
  }

  public void moveRowDown(int rowIndex) {
    if (rotationAnglesList == null) {
      return;
    }
    if (rowIndex >= totalRows.getInt() - 1) {
      throw new IllegalStateException("rowIndex=" + rowIndex + ",totalRows=" + totalRows);
    }
    Integer curIndex = new Integer(rowIndex);
    Integer nextIndex = new Integer(rowIndex + 1);
    //swap the current row with the one below it
    SlicerAngles rotationAngles = (SlicerAngles) rotationAnglesList.remove(curIndex);
    SlicerAngles nextRotationAngles;
    if (rotationAngles != null) {
      nextRotationAngles = (SlicerAngles) rotationAnglesList.put(nextIndex,
          rotationAngles);
    }
    else {
      nextRotationAngles = (SlicerAngles) rotationAnglesList.remove(nextIndex);
    }
    if (nextRotationAngles != null) {
      rotationAnglesList.put(curIndex, nextRotationAngles);
    }
  }

  public void setRotationAnglesList(Hashtable rotationAnglesList) {
    this.rotationAnglesList = rotationAnglesList;
  }

  public void setTotalRows(int totalRows) {
    this.totalRows.set(totalRows);
  }

  public void setXfModelOutputFile(String xfModelOutputFile) {
    this.xfModelOutputFile = xfModelOutputFile;
  }

  public String getXfModelOutputFile() {
    return xfModelOutputFile;
  }

  public void setSampleProduced(boolean sampleProduced) {
    this.sampleProduced = sampleProduced;
  }

  public final int getDoneMode() {
    return doneMode.getInt();
  }

  public final void setDoneMode(int doneMode) {
    this.doneMode.set(doneMode);
  }

  public final void clearDoneMode() {
    doneMode.reset();
  }
}