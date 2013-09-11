package etomo.type;

import java.util.Properties;

import etomo.ui.LogProperties;
import etomo.ui.swing.AnisotropicDiffusionDialog;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ParallelMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  public static final String NEW_GENERIC_PARALLEL_PROCESS_TITLE = "Parallel Processing";
  public static final String NEW_ANISOTROPIC_DIFFUSION_TITLE = "Nonlinear Anisotropic Diffusion";

  private static final String REVISION_KEY = "Revision";
  private static final String CURRENT_REVISION = "1.0";
  private static final String PARALLEL_GROUP_KEY = "Parallel";
  private static final String ROOT_NAME_KEY = "RootName";
  static final String ANISOTROPIC_DIFFUSION_GROUP_KEY = "AnisotropicDiffusion";

  private final EtomoBoolean2 loadWithFlipping = new EtomoBoolean2("LoadWithFlipping");
  private final StringProperty volume = new StringProperty("Volume");
  private final EtomoNumber xMin = new EtomoNumber("XMin");
  private final EtomoNumber xMax = new EtomoNumber("XMax");
  private final EtomoNumber yMin = new EtomoNumber("YMin");
  private final EtomoNumber yMax = new EtomoNumber("YMax");
  private final EtomoNumber zMin = new EtomoNumber("ZMin");
  private final EtomoNumber zMax = new EtomoNumber("ZMax");
  private final StringProperty testKValueList = new StringProperty("TestKValueList");
  private final EtomoNumber testIteration = new EtomoNumber("TestIteration");
  private final EtomoNumber testKValue = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "TestKValue");
  private final StringProperty testIterationList = new StringProperty("TestIterationList");
  private final EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.DOUBLE, "KValue");
  private final EtomoNumber iteration = new EtomoNumber("Iteration");
  private final EtomoNumber memoryPerChunk = new EtomoNumber("MemoryPerChunk");
  private final EtomoBoolean2 overlapTimesFour = new EtomoBoolean2("OverlapTimesFour");
  private final EtomoBoolean2 newStyleZ = new EtomoBoolean2("NewStyleZ");

  private DialogType dialogType = DialogType.getDefault(DataFileType.PARALLEL);
  private String revision = null;
  private String rootName = null;

  public ParallelMetaData(final LogProperties logProperties) {
    super(logProperties);
    axisType = AxisType.SINGLE_AXIS;
    fileExtension = DataFileType.PARALLEL.extension;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  String paramString() {
    return "revision=" + revision + ",rootName=" + rootName;
  }

  public String getName() {
    if (rootName == null) {
      if (dialogType == DialogType.ANISOTROPIC_DIFFUSION) {
        return NEW_ANISOTROPIC_DIFFUSION_TITLE;
      }
      return NEW_GENERIC_PARALLEL_PROCESS_TITLE;
    }
    return rootName;
  }

  public String getDatasetName() {
    return rootName;
  }

  public String getMetaDataFileName() {
    if (rootName == null) {
      return null;
    }
    return DatasetFiles.getParallelDataFileName(rootName);
  }

  public boolean isValid() {
    return validate() == null;
  }

  /**
   * returns null if valid
   * @return error message if invalid
   */
  public String validate() {
    if (rootName == null) {
      return "Missing root name.";
    }
    return null;
  }

  public void setRootName(String rootName) {
    this.rootName = rootName;
    Utilities.managerStamp(null, this.rootName);
  }

  public void setLoadWithFlipping(boolean input) {
    loadWithFlipping.set(input);
  }

  public boolean isLoadWithFlipping() {
    return loadWithFlipping.is();
  }

  public String getRootName() {
    return rootName;
  }

  public void setVolume(String input) {
    volume.set(input);
  }

  public String getVolume() {
    return volume.toString();
  }

  public void setXMin(String input) {
    xMin.set(input);
  }

  public String getXMin() {
    return xMin.toString();
  }

  public void setXMax(String input) {
    xMax.set(input);
  }

  public String getXMax() {
    return xMax.toString();
  }

  public void setYMin(String input) {
    yMin.set(input);
  }

  public String getYMin() {
    return yMin.toString();
  }

  public void setYMax(String input) {
    yMax.set(input);
  }

  public String getYMax() {
    return yMax.toString();
  }

  public void setZMin(String input) {
    zMin.set(input);
  }

  public String getZMin() {
    return zMin.toString();
  }

  public void setZMax(String input) {
    zMax.set(input);
  }

  public String getZMax() {
    return zMax.toString();
  }

  public void setTestKValueList(String input) {
    testKValueList.set(input);
  }

  public String getTestKValueList() {
    return testKValueList.toString();
  }

  public void setKValue(String input) {
    kValue.set(input);
  }

  public String getKValue() {
    return kValue.toString();
  }

  public void setIteration(Number input) {
    iteration.set(input);
  }

  public ConstEtomoNumber getIteration() {
    return iteration;
  }

  public void setTestIteration(Number input) {
    testIteration.set(input);
  }

  public ConstEtomoNumber getTestIteration() {
    return testIteration;
  }

  public void setMemoryPerChunk(Number input) {
    memoryPerChunk.set(input);
  }

  public ConstEtomoNumber getMemoryPerChunk() {
    return memoryPerChunk;
  }

  public void setOverlapTimesFour(boolean input) {
    overlapTimesFour.set(input);
  }

  public void setNewStyleZ(final String uiZMin, final String uiZMax) {
    if (newStyleZ.isNull() || !newStyleZ.is()) {
      newStyleZ.set(!zMin.equals(uiZMin) || !zMax.equals(uiZMax));
    }
  }

  public boolean isOverlapTimesFour() {
    return overlapTimesFour.is();
  }

  public boolean isNewStyleZ() {
    return newStyleZ.is();
  }

  public void setTestKValue(String input) {
    testKValue.set(input);
  }

  public String getTestKValue() {
    return testKValue.toString();
  }

  public void setTestIterationList(String input) {
    testIterationList.set(input);
  }

  public String getTestIterationList() {
    return testIterationList.toString();
  }

  public void setDialogType(DialogType input) {
    dialogType = input;
  }

  public DialogType getDialogType() {
    return dialogType;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    // reset
    revision = null;
    rootName = null;
    loadWithFlipping.reset();
    volume.reset();
    xMin.reset();
    xMax.reset();
    yMin.reset();
    yMax.reset();
    zMin.reset();
    zMax.reset();
    testKValueList.reset();
    testIteration.reset();
    dialogType = DialogType.getDefault(DataFileType.PARALLEL);
    testKValue.reset();
    testIterationList.reset();
    kValue.reset();
    iteration.reset();
    memoryPerChunk.reset();
    overlapTimesFour.reset();
    newStyleZ.reset();
    // load
    dialogType = DialogType.load(DataFileType.PARALLEL, props);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    revision = props.getProperty(group + REVISION_KEY, CURRENT_REVISION);
    rootName = props.getProperty(group + ROOT_NAME_KEY);
    loadWithFlipping.load(props, prepend);
    volume.load(props, prepend);
    xMin.load(props, prepend);
    xMax.load(props, prepend);
    yMin.load(props, prepend);
    yMax.load(props, prepend);
    zMin.load(props, prepend);
    zMax.load(props, prepend);
    testKValueList.load(props, prepend);
    testIteration.load(props, prepend);
    testKValue.load(props, prepend);
    testIterationList.load(props, prepend);
    kValue.load(props, prepend);
    iteration.load(props, prepend);
    memoryPerChunk.load(props, prepend,
        AnisotropicDiffusionDialog.MEMORY_PER_CHUNK_DEFAULT);
    overlapTimesFour.load(props, prepend);
    newStyleZ.load(props, prepend);
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    dialogType.store(props);
    props.setProperty(group + REVISION_KEY, CURRENT_REVISION);
    props.setProperty(group + ROOT_NAME_KEY, rootName);
    loadWithFlipping.store(props, prepend);
    volume.store(props, prepend);
    xMin.store(props, prepend);
    xMax.store(props, prepend);
    yMin.store(props, prepend);
    yMax.store(props, prepend);
    zMin.store(props, prepend);
    zMax.store(props, prepend);
    testKValueList.store(props, prepend);
    testIteration.store(props, prepend);
    testKValue.store(props, prepend);
    testIterationList.store(props, prepend);
    kValue.store(props, prepend);
    iteration.store(props, prepend);
    memoryPerChunk.store(props, prepend);
    overlapTimesFour.store(props, prepend);
    newStyleZ.store(props, prepend);
  }

  String getGroupKey() {
    if (dialogType == DialogType.ANISOTROPIC_DIFFUSION) {
      return ANISOTROPIC_DIFFUSION_GROUP_KEY;
    }
    return PARALLEL_GROUP_KEY;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.11  2011/02/22 05:47:35  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.10  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.9  2010/01/21 21:30:22  sueh
 * <p> bug# 1305 Added overlapTimesFour.
 * <p>
 * <p> Revision 1.8  2009/10/23 19:44:45  sueh
 * <p> bug# 1275 Separate generic parallel process and NAD but continue to use
 * <p> the same file type for both.
 * <p>
 * <p> Revision 1.7  2008/12/10 18:34:05  sueh
 * <p> bug# 1162 Added a manager stamp to setRootName.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:37:48  sueh
 * <p> bug# 1041 Moved resets to load since they are only done once.
 * <p>
 * <p> Revision 1.5  2007/11/09 17:45:56  sueh
 * <p> bug# 1047 Changed the names of NAD fields for clarity.
 * <p>
 * <p> Revision 1.4  2007/11/06 19:37:15  sueh
 * <p> bug# 1047 Added meta data for the anisotropic diffusion dialog.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:20:34  sueh
 * <p> bug# 964 Set fileExtension.
 * <p>
 * <p> Revision 1.2  2006/04/10 19:02:57  sueh
 * <p> buG# 835 enabling "New Parallel Process" when processchunks is run.
 * <p>
 * <p> Revision 1.1  2006/03/20 17:58:24  sueh
 * <p> bug# 835 Meta data for ParallelManager.
 * <p> </p>
 */
