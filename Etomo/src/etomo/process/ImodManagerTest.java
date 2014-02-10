package etomo.process;

import junit.framework.TestCase;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.MetaData;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ImodManagerTest extends TestCase {
  MetaData metaData;
  ImodManager imodManager;
  String datasetNameDual = new String("BB");
  String datasetNameSingle = new String("BBa");
  String datasetName;
  private static final String rawStack = "raw stack";
  private static final String erasedStack = "erased stack";
  private static final String coarseAligned = "coarse aligned";
  private static final String fineAligned = "fine aligned";
  private static final String sample = "sample";
  private static final String fullVolume = "full volume";
  private static final String combinedTomogram = "combined tomogram";
  private static final String fiducialModel = "fiducial model";
  private static final String trimmedVolume = "trimmed volume";
  private static final String patchVectorModel = "patch vector model";
  private static final String matchCheck = "match check";
  private static final String mtfFilter = "mtf filter";
  private static final String trialTomogram = "trial tomogram";
  private static final String preview = "preview";
  private final BaseManager manager;

  /* @see TestCase#setUp() */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /* @see TestCase#tearDown() */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Constructor for ImodManagerTest.
   * @param arg0
   */
  public ImodManagerTest(String arg0) {
    super(arg0);
    EtomoDirector.INSTANCE.openTomogram(true, AxisID.ONLY);
    manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
    metaData = new MetaData((ApplicationManager) manager, manager.getLogProperties());
  }

  // Regression test

  final public void testImodManager() throws AxisTypeException, SystemProcessException {
    ImodState tester;
    // Test single axis
    setUpSingle();
    imodManager = new ImodManager(manager);
    imodManager.setMetaData(metaData);
    // rawStack
    tester = newTester(rawStack);
    tester.equals(imodManager.get(rawStack));
    // erasedStack
    tester = newTester(erasedStack);
    tester.equals(imodManager.get(erasedStack));
    // coarseAligned
    tester = newTester(coarseAligned);
    tester.equals(imodManager.get(coarseAligned));
    // fineAligned
    tester = newTester(fineAligned);
    tester.equals(imodManager.get(fineAligned));
    // sample
    tester = newTester(sample);
    tester.equals(imodManager.get(sample));
    // fullVolume
    tester = newTester(fullVolume);
    tester.equals(imodManager.get(fullVolume));
    // combinedTomogram
    assertEquals(imodManager.get(imodManager.getPrivateKey(combinedTomogram)),
        imodManager.get(imodManager.getPrivateKey(fullVolume)));
    // fiducialModel
    tester = newTester(fiducialModel);
    tester.equals(imodManager.get(fiducialModel));
    // trimmedVolume
    tester = newTester(trimmedVolume);
    tester.equals(imodManager.get(trimmedVolume));
    // mtfFilter
    tester = newTester(mtfFilter);
    tester.equals(imodManager.get(mtfFilter));
    // optional imods
    // trialTomogram
    tester = newTester(trialTomogram);
    imodManager.newImod(trialTomogram, AxisID.ONLY, datasetName);
    tester.equals(imodManager.get(trialTomogram));
    // Test preview
    tester = newTester(preview);
    imodManager.newImod(preview, AxisID.ONLY);
    tester.equals(imodManager.get(preview));
    // Test dual axis
    // original code
    /* else { rawStackA = new ImodProcess(datasetName + "a.st"); rawStackB = new
     * ImodProcess(datasetName + "b.st"); erasedStackA = new ImodProcess(datasetName +
     * "a_fixed.st"); erasedStackB = new ImodProcess(datasetName + "b_fixed.st");
     * coarseAlignedA = new ImodProcess(datasetName + "a.preali"); coarseAlignedB = new
     * ImodProcess(datasetName + "b.preali"); fineAlignedA = new ImodProcess(datasetName +
     * "a.ali"); fineAlignedB = new ImodProcess(datasetName + "b.ali"); sampleA = new
     * ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod"); sampleB = new
     * ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod"); fullVolumeA = new
     * ImodProcess(datasetName + "a.rec"); fullVolumeA.setSwapYZ(true); fullVolumeB = new
     * ImodProcess(datasetName + "b.rec"); fullVolumeB.setSwapYZ(true); combinedTomogram =
     * new ImodProcess("sum.rec"); combinedTomogram.setSwapYZ(true); patchVectorModel =
     * new ImodProcess("patch_vector.mod"); patchVectorModel.setModelView(true);
     * matchCheck = new ImodProcess("matchcheck.mat matchcheck.rec");
     * matchCheck.setSwapYZ(true); matchCheck.setFillCache(true); fiducialModelA = new
     * ImodProcess(); fiducialModelB = new ImodProcess(); } trimmedVolume = new
     * ImodProcess(datasetName + ".rec"); */
    setUpDual();
    imodManager = new ImodManager(manager);
    imodManager.setMetaData(metaData);
    AxisID a = AxisID.FIRST;
    AxisID b = AxisID.SECOND;
    AxisType dual = AxisType.DUAL_AXIS;
    // rawStack
    tester = newTester(rawStack, a);
    tester.equals(imodManager.get(rawStack, a));
    tester = newTester(rawStack, b);
    tester.equals(imodManager.get(rawStack, b));
    // erasedStack
    tester = newTester(erasedStack, a);
    tester.equals(imodManager.get(erasedStack, a));
    tester = newTester(erasedStack, b);
    tester.equals(imodManager.get(erasedStack, b));
    // coarseAligned
    tester = newTester(coarseAligned, a);
    tester.equals(imodManager.get(coarseAligned, a));
    tester = newTester(coarseAligned, b);
    tester.equals(imodManager.get(coarseAligned, b));
    // fineAligned
    tester = newTester(fineAligned, a);
    tester.equals(imodManager.get(fineAligned, a));
    tester = newTester(fineAligned, b);
    tester.equals(imodManager.get(fineAligned, b));
    // sample
    tester = newTester(sample, a);
    tester.equals(imodManager.get(sample, a));
    tester = newTester(sample, b);
    tester.equals(imodManager.get(sample, b));
    // fullVolume
    tester = newTester(fullVolume, a);
    tester.equals(imodManager.get(fullVolume, a));
    tester = newTester(fullVolume, b);
    tester.equals(imodManager.get(fullVolume, b));
    // combinedTomogram
    tester = newTester(combinedTomogram, dual);
    tester.equals(imodManager.get(combinedTomogram));
    // patchVectorModel
    tester = newTester(patchVectorModel, dual);
    tester.equals(imodManager.get(patchVectorModel));
    // matchCheck
    tester = newTester(matchCheck, dual);
    tester.equals(imodManager.get(matchCheck));
    // fiducialModel
    tester = newTester(fiducialModel, a);
    tester.equals(imodManager.get(fiducialModel, a));
    tester = newTester(fiducialModel, b);
    tester.equals(imodManager.get(fiducialModel, b));
    // trimmedVolume
    tester = newTester(trimmedVolume, dual);
    tester.equals(imodManager.get(trimmedVolume));
    // mtfFilter
    tester = newTester(mtfFilter, a);
    tester.equals(imodManager.get(mtfFilter, a));
    tester = newTester(mtfFilter, b);
    tester.equals(imodManager.get(mtfFilter, b));
    // optional imods
    // trialTomogram
    tester = newTester(trialTomogram, a);
    imodManager.newImod(trialTomogram, a, datasetName);
    tester.equals(imodManager.get(trialTomogram, a));
    tester = newTester(trialTomogram, b);
    imodManager.newImod(trialTomogram, b, datasetName);
    tester.equals(imodManager.get(trialTomogram, b));
    // Test preview
    tester = newTester(preview, a);
    imodManager.newImod(preview, a);
    tester.equals(imodManager.get(preview, a));
    tester = newTester(preview, b);
    imodManager.newImod(preview, b);
    tester.equals(imodManager.get(preview, b));
  }

  private void setUpSingle() {
    datasetName = datasetNameSingle;
    metaData.setDatasetName(datasetName);
    metaData.setAxisType(AxisType.SINGLE_AXIS);
  }

  private void setUpDual() {
    datasetName = datasetNameDual;
    metaData.setDatasetName(datasetName);
    metaData.setAxisType(AxisType.DUAL_AXIS);
  }

  private ImodState newTester(String name) {
    return newTester(name, AxisType.SINGLE_AXIS, AxisID.ONLY);
  }

  private ImodState newTester(String name, AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      return newTester(name, AxisType.SINGLE_AXIS, axisID);
    }
    return newTester(name, AxisType.DUAL_AXIS, axisID);
  }

  private ImodState newTester(String name, AxisType axisType) {
    return newTester(name, axisType, null);
  }

  private ImodState newTester(String name, AxisType axisType, AxisID axisID) {
    ImodState tester;
    if (axisType == AxisType.SINGLE_AXIS) {
      /* Tester should mimic original code if (axisType == AxisType.SINGLE_AXIS) {
       * rawStackA = new ImodProcess(datasetName + ".st"); erasedStackA = new
       * ImodProcess(datasetName + "_fixed.st"); coarseAlignedA = new
       * ImodProcess(datasetName + ".preali"); fineAlignedA = new ImodProcess(datasetName
       * + ".ali"); sampleA = new ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod");
       * fullVolumeA = new ImodProcess(datasetName + "_full.rec");
       * fullVolumeA.setSwapYZ(true); combinedTomogram = fullVolumeA; fiducialModelA = new
       * ImodProcess(); } trimmedVolume = new ImodProcess(datasetName + ".rec"); */
      if (name.equals(rawStack)) {
        return new ImodState(manager, datasetName + DatasetTool.STANDARD_DATASET_EXT,
            axisID);
      }
      if (name.equals(erasedStack)) {
        return new ImodState(manager, datasetName + "_fixed"
            + DatasetTool.STANDARD_DATASET_EXT, axisID);
      }
      if (name.equals(coarseAligned)) {
        return new ImodState(manager, datasetName + ".preali", axisID);
      }
      if (name.equals(fineAligned)) {
        return new ImodState(manager, datasetName + ".ali", axisID);
      }
      if (name.equals(sample)) {
        tester = new ImodState(manager, "top.rec mid.rec bot.rec", "tomopitch.mod",
            axisID);
        tester.setInitialMode(ImodState.MODEL_MODE);
        return tester;
      }
      if (name.equals(fullVolume)) {
        tester = new ImodState(manager, datasetName + "_full.rec", axisID);
        tester.setInitialSwapYZ(true);
        return tester;
      }
      if (name.equals(fiducialModel)) {
        tester = new ImodState(manager, ImodState.MODV, axisID);
        return tester;
      }
      if (name.equals(trimmedVolume)) {
        return new ImodState(manager, datasetName + ".rec", axisID);
      }
      if (name.equals(trialTomogram)) {
        tester = new ImodState(manager, datasetName, axisID);
        tester.setInitialSwapYZ(true);
        return tester;
      }
      if (name.equals(mtfFilter)) {
        return new ImodState(manager, datasetName + "_filt.ali", axisID);
      }
      if (name.equals(preview)) {
        return new ImodState(manager, datasetName + DatasetTool.STANDARD_DATASET_EXT,
            axisID);
      }
      return null;
    }
    else {
      /* Tester should mimic original code else { rawStackA = new ImodProcess(datasetName
       * + "a.st"); rawStackB = new ImodProcess(datasetName + "b.st"); erasedStackA = new
       * ImodProcess(datasetName + "a_fixed.st"); erasedStackB = new
       * ImodProcess(datasetName + "b_fixed.st"); coarseAlignedA = new
       * ImodProcess(datasetName + "a.preali"); coarseAlignedB = new
       * ImodProcess(datasetName + "b.preali"); fineAlignedA = new ImodProcess(datasetName
       * + "a.ali"); fineAlignedB = new ImodProcess(datasetName + "b.ali"); sampleA = new
       * ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod"); sampleB = new
       * ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod"); fullVolumeA = new
       * ImodProcess(datasetName + "a.rec"); fullVolumeA.setSwapYZ(true); fullVolumeB =
       * new ImodProcess(datasetName + "b.rec"); fullVolumeB.setSwapYZ(true);
       * combinedTomogram = new ImodProcess("sum.rec"); combinedTomogram.setSwapYZ(true);
       * patchVectorModel = new ImodProcess("patch_vector.mod");
       * patchVectorModel.setModelView(true); matchCheck = new
       * ImodProcess("matchcheck.mat matchcheck.rec"); matchCheck.setSwapYZ(true);
       * matchCheck.setFillCache(true); fiducialModelA = new ImodProcess(); fiducialModelB
       * = new ImodProcess(); } trimmedVolume = new ImodProcess(datasetName + ".rec"); */
      if (name == rawStack) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
      }
      if (name.equals(erasedStack)) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a_fixed"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b_fixed"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
      }
      if (name.equals(coarseAligned)) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a.preali", axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b.preali", axisID);
        }
      }
      if (name.equals(fineAligned)) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a.ali", axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b.ali", axisID);
        }
      }
      if (name.equals(sample)) {
        if (axisID == AxisID.FIRST) {
          tester = new ImodState(manager, "topa.rec mida.rec bota.rec", "tomopitcha.mod",
              axisID);
          tester.setInitialMode(ImodState.MODEL_MODE);
          return tester;
        }
        else {
          tester = new ImodState(manager, "topb.rec midb.rec botb.rec", "tomopitchb.mod",
              axisID);
          tester.setInitialMode(ImodState.MODEL_MODE);
          return tester;
        }
      }
      if (name.equals(fullVolume)) {
        if (axisID == AxisID.FIRST) {
          tester = new ImodState(manager, datasetName + "a.rec", AxisID.ONLY);
          tester.setInitialSwapYZ(true);
          return tester;
        }
        else {
          tester = new ImodState(manager, datasetName + "b.rec", AxisID.ONLY);
          tester.setInitialSwapYZ(true);
          return tester;
        }
      }
      if (name.equals(combinedTomogram)) {
        tester = new ImodState(manager, "sum.rec", AxisID.ONLY);
        tester.setSwapYZ(true);
        return tester;
      }
      if (name.equals(patchVectorModel)) {
        tester = new ImodState(manager, "patch_vector.mod", ImodState.MODEL_VIEW,
            AxisID.ONLY);
        tester.setInitialMode(ImodState.MODEL_MODE);
        return tester;
      }
      if (name.equals(matchCheck)) {
        tester = new ImodState(manager, "matchcheck.mat matchcheck.rec", AxisID.ONLY);
        tester.setInitialSwapYZ(true);
        return tester;
      }
      if (name.equals(fiducialModel)) {
        if (axisID == AxisID.FIRST) {
          tester = new ImodState(manager, ImodState.MODV, axisID);
          return tester;
        }
        else {
          tester = new ImodState(manager, ImodState.MODV, axisID);
          return tester;
        }
      }
      if (name.equals(trimmedVolume)) {
        return new ImodState(manager, datasetName + ".rec", axisID);
      }
      if (name.equals(trialTomogram)) {
        tester = new ImodState(manager, datasetName, axisID);
        tester.setInitialSwapYZ(true);
        return tester;
      }
      if (name.equals(mtfFilter)) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a_filt.ali", axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b_filt.ali", axisID);
        }
      }
      if (name == preview) {
        if (axisID == AxisID.FIRST) {
          return new ImodState(manager, datasetName + "a"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
        else {
          return new ImodState(manager, datasetName + "b"
              + DatasetTool.STANDARD_DATASET_EXT, axisID);
        }
      }
      return null;
    }
  }
}