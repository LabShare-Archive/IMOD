/*
 * Created on Nov 17, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.process;

import junit.framework.TestCase;

import java.io.File;

import etomo.ApplicationManager;
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
  ApplicationManager applicationManager;
  MetaData metaData;
  ImodManager imodManager;
  String datasetNameDual = new String("BB");
  String datasetNameSingle = new String("BBa");
  String datasetName;
  String oldUserDir;

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    String[] args = new String[1];
    args[0] = new String("");
    oldUserDir = new String(System.getProperty("user.dir"));
    File testDir = new File(oldUserDir, "workspace/Etomo_test/vectors/stacks");
    assertTrue(testDir.exists());
    System.setProperty("user.dir", testDir.getAbsolutePath());
    applicationManager = new ApplicationManager(args);
    metaData = new MetaData();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
    System.setProperty("user.dir", oldUserDir);
  }

  /**
   * Constructor for ImodManagerTest.
   * @param arg0
   */
  public ImodManagerTest(String arg0) {
    super(arg0);
  }
  


  final public void testImodManager() {
    Tester tester;
    //Test single axis
    setUpSingle();
    imodManager = new ImodManager(applicationManager, metaData);
    //rawStack
    tester = newTester("rawStack");
    tester.equals(imodManager.get("rawStack"));
    //erasedStack
    tester = newTester("erasedStack");
    tester.equals(imodManager.get("erasedStack"));
    //coarseAligned
    tester = newTester("coarseAligned");
    tester.equals(imodManager.get("coarseAligned"));
    //fineAligned
    tester = newTester("fineAligned");
    tester.equals(imodManager.get("fineAligned"));
    //sample
    tester = newTester("sample");
    tester.equals(imodManager.get("sample"));
    //fullVolume
    tester = newTester("fullVolume");
    tester.equals(imodManager.get("fullVolume"));
    //combinedTomogram
    assertEquals(
      imodManager.get(imodManager.getPrivateKey("combinedTomogram")),
      imodManager.get(imodManager.getPrivateKey("fullVolume")));
    //fiducialModel
    tester = newTester("fiducialModel");
    tester.equals(imodManager.get("fiducialModel"));
    //trimmedVolume    
    tester = newTester("trimmedVolume");
    tester.equals(imodManager.get("trimmedVolume"));
    //Test dual axis
    //original code
    /*
        else {
          rawStackA = new ImodProcess(datasetName + "a.st");
          rawStackB = new ImodProcess(datasetName + "b.st");
          erasedStackA = new ImodProcess(datasetName + "a_fixed.st");
          erasedStackB = new ImodProcess(datasetName + "b_fixed.st");
          coarseAlignedA = new ImodProcess(datasetName + "a.preali");
          coarseAlignedB = new ImodProcess(datasetName + "b.preali");
          fineAlignedA = new ImodProcess(datasetName + "a.ali");
          fineAlignedB = new ImodProcess(datasetName + "b.ali");
          sampleA = new ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod");
          sampleB = new ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod");
          fullVolumeA = new ImodProcess(datasetName + "a.rec");
          fullVolumeA.setSwapYZ(true);
          fullVolumeB = new ImodProcess(datasetName + "b.rec");
          fullVolumeB.setSwapYZ(true);
          combinedTomogram = new ImodProcess("sum.rec");
          combinedTomogram.setSwapYZ(true);
          patchVectorModel = new ImodProcess("patch_vector.mod");
          patchVectorModel.setModelView(true);
          matchCheck = new ImodProcess("matchcheck.mat matchcheck.rec");
          matchCheck.setSwapYZ(true);
          matchCheck.setFillCache(true);
          fiducialModelA = new ImodProcess();
          fiducialModelB = new ImodProcess();
        }
        trimmedVolume = new ImodProcess(datasetName + ".rec");
    */
    setUpDual();
    imodManager = new ImodManager(applicationManager, metaData);
    AxisID a = AxisID.FIRST;
    AxisID b = AxisID.SECOND;
    AxisType dual = AxisType.DUAL_AXIS;
    //rawStack
    tester = newTester("rawStack", a);
    tester.equals(imodManager.get("rawStack", a));
    tester = newTester("rawStack", b);
    tester.equals(imodManager.get("rawStack", b));
    //erasedStack
    tester = newTester("erasedStack", a);
    tester.equals(imodManager.get("erasedStack", a));
    tester = newTester("erasedStack", b);
    tester.equals(imodManager.get("erasedStack", b));
    //coarseAligned
    tester = newTester("coarseAligned", a);
    tester.equals(imodManager.get("coarseAligned", a));
    tester = newTester("coarseAligned", b);
    tester.equals(imodManager.get("coarseAligned", b));
    //fineAligned
    tester = newTester("fineAligned", a);
    tester.equals(imodManager.get("fineAligned", a));
    tester = newTester("fineAligned", b);
    tester.equals(imodManager.get("fineAligned", b));
    //sample
    tester = newTester("sample", a);
    tester.equals(imodManager.get("sample", a));
    tester = newTester("sample", b);
    tester.equals(imodManager.get("sample", b));
    //fullVolume
    tester = newTester("fullVolume", a);
    tester.equals(imodManager.get("fullVolume", a));
    tester = newTester("fullVolume", b);
    tester.equals(imodManager.get("fullVolume", b));
    //combinedTomogram
    tester = newTester("combinedTomogram", dual);
    tester.equals(imodManager.get("combinedTomogram"));
    //patchVectorModel
    tester = newTester("patchVectorModel", dual);
    tester.equals(imodManager.get("patchVectorModel"));
    //matchCheck
    tester = newTester("matchCheck", dual);
    tester.equals(imodManager.get("matchCheck"));
    //fiducialModel
    tester = newTester("fiducialModel", a);
    tester.equals(imodManager.get("fiducialModel", a));
    tester = newTester("fiducialModel", b);
    tester.equals(imodManager.get("fiducialModel", b));
    //trimmedVolume
    tester = newTester("trimmedVolume", dual);
    tester.equals(imodManager.get("trimmedVolume"));
  }



  
  public void testRawStack()
    throws AxisTypeException, SystemProcessException {
    String key = "rawStack";
    AxisID axisID;
    setUpSingle();
    axisID = AxisID.ONLY;
    String modelName = "patch_vector.mod";
    boolean modelMode = true;
    Tester tester = newTester(key, axisID);
    
    //rawStack.openModel(modelName);
    //if (modelMode) {
    //  rawStack.modelMode();
    //}
    //else {
    //  rawStack.movieMode();
    //}
    tester.openModel(modelName);
    if (modelMode) {
      tester.modelMode();
    }
    else {
      tester.movieMode();
    }
      
    testModel(tester, key, axisID, modelName, modelMode);
  }

  final public void testErasedStack()
    throws AxisTypeException, SystemProcessException {
    String key = "erasedStack";
    AxisID axisID;
    String modelName = "BBa.fid";
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key, axisID);
    
    //erasedStack.openModel(modelName);
    tester.openModel(modelName);
    
    testModel(tester, key, axisID, modelName);
  }

 
  final public void testCoarseAligned()
    throws AxisTypeException, SystemProcessException {
    String key = "coarseAligned";
    AxisID axisID;
    String modelName = "BBa.fid";
    boolean preserveContrast = true;
    boolean modelMode = true;
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key, axisID);
    
    //if (preserveConstrast) {
    //  coarseAligned.openModelPreserveContrast(modelName);
    //}
    //else {
    //  coarseAligned.openModel(modelName);
    //}
    //coarseAligned.modelMode();
    //if (modelMode) {
    //  coarseAligned.modelMode();
    //}
    //else {
    //  coarseAligned.movieMode();
    //}
    //coarseAligned.configurePreserveContrast(preserveConstrast);
    //coarseAligned.model(modelName, modelMode);
    if (preserveContrast) {
      tester.openModelPreserveContrast(modelName);
    }
    else {
      tester.openModel(modelName);
    }
    tester.modelMode();
    if (modelMode) {
      tester.modelMode();
    }
    else {
      tester.movieMode();
    }

    testModel(tester, key, axisID, modelName, modelMode, preserveContrast);
  }

 
  public void testFineAligned()
    throws AxisTypeException, SystemProcessException {
    String key = "fineAligned";
    AxisID axisID;
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key, axisID);
    testOpen(tester, key, axisID);
  }
  
  final public void testSample()
    throws AxisTypeException, SystemProcessException {
    String key = "sample";
    AxisID axisID;
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key, axisID);   
    
    //sample.modelMode();
     
    testOpen(tester, key, axisID);
  }
  
  private void setupNewSample(Tester tester) {
    //sample.modelMode();
    tester.modelMode();
  }

  final public void testFullVolume()
    throws AxisTypeException, SystemProcessException {
    String key = "fullVolume";
    AxisID axisID;

    ImodManager imodManager;
    //Test single
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key, axisID);
    
    //fullVolume.setModelName("");
    tester.setModelName("");

    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.open(key, axisID);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
    assertEquals(imodManager.get(key, axisID), imodManager.get(imodManager.getPrivateKey("combinedTomogram")));
    
    //Test dual
    setUpDual();
    axisID = AxisID.FIRST;
    Tester testerA = newTester(key, axisID);
    Tester testerB = newTester(key, AxisID.SECOND);
    
    //fullVolume.setModelName("");
    testerA.setModelName("");
    testerB.setModelName("");
    
    //matchingModel
    
    //fullVolumeA.openModel(datasetName + "a.matmod");
    //fullVolumeA.modelMode();
    testerA.openModel(datasetName + "a.matmod");
    testerA.modelMode();

    //fullVolumeB.openModel(datasetName + "b.matmod");
    //fullVolumeB.modelMode();
    testerB.openModel(datasetName + "b.matmod");
    testerB.modelMode();

    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.matchingModel(datasetName);
    assertTrue(imodManager.isOpen(key, axisID));
    assertTrue(imodManager.isOpen(key, AxisID.SECOND));
    testerA.equals(imodManager.get(key, axisID));
    testerB.equals(imodManager.get(key, AxisID.SECOND));
    imodManager.quit(key, axisID);
    imodManager.quit(key, AxisID.SECOND);
    
    //patchRegionModel
    //fullVolumeA.openModel("patch_region.mod");
    //fullVolumeA.modelMode(); 
    testerA.openModel("patch_region.mod");
    testerA.modelMode();

    imodManager.patchRegionModel(axisID);
    assertTrue(imodManager.isOpen(key, axisID));
    testerA.equals(imodManager.get(key, axisID));
    imodManager.quit(key, axisID);
  }
  
  final public void testCombinedTomogram() throws SystemProcessException, AxisTypeException {
    String key = "combinedTomogram";
    setUpDual();
    Tester tester = newTester(key, AxisType.DUAL_AXIS);
    testOpen(tester, key);
  }

  final public void testPatchVectorModel() throws SystemProcessException, AxisTypeException {
    String key = "patchVectorModel";
    Tester tester;
    setUpDual();
    tester = newTester(key, AxisType.DUAL_AXIS);
    
    //patchVectorModel.modelMode();
    
    testOpen(tester, key);
  }
  
  private void setupNewPatchVectorModel(Tester tester) {
    //patchVectorModel.modelMode();
    tester.modelMode();
  }

  final public void testMatchCheck() throws SystemProcessException, AxisTypeException {
    String key = "matchCheck";
    setUpDual();
    Tester tester = newTester(key, AxisType.DUAL_AXIS);
    testOpen(tester, key);
  }

  final public void testFiducialModel()
    throws AxisTypeException, SystemProcessException {
    String key = "fiducialModel";
    AxisID axisID;
    String model = "BBa.fid";
    setUpSingle();
    axisID = AxisID.ONLY;
    Tester tester = newTester(key);
    
    //fiducialModel.setModelName(model);
    //fiducialModel.setUseModv(true);
    //fiducialModel.setOutputWindowID(false);
    tester.setModelName(model);
    
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.open(key, axisID, model);
    tester.equals(imodManager.get(key, axisID));
    try {
      imodManager.openBeadFixer(key, axisID);
      fail();
    }
    catch (UnsupportedOperationException e) {
    }
  }

  private void setupNewFiducialModel(Tester tester) {
    //fiducialModel.setUseModv(true);
    //fiducialModel.setOutputWindowID(false);
    tester.setUseModv(true);
    tester.setOutputWindowID(false);
  }
  final public void testTrimmedVolume() throws SystemProcessException, AxisTypeException {
    String key = "trimmedVolume";
    setUpSingle();
    Tester tester = newTester(key);
    testOpen(tester, key);
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


  private Tester newTester(String name) {
    return newTester(name, AxisType.SINGLE_AXIS, AxisID.ONLY);
  }

  private Tester newTester(String name, AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      return newTester(name, AxisType.SINGLE_AXIS, axisID);
    }
    return newTester(name, AxisType.DUAL_AXIS, axisID);
  }

  private Tester newTester(String name, AxisType axisType) {
    return newTester(name, axisType, null);
  }

  private Tester newTester(String name, AxisType axisType, AxisID axisID) {
    Tester tester;
    if (axisType == AxisType.SINGLE_AXIS) {
      /* Tester should mimic original code
          if (axisType == AxisType.SINGLE_AXIS) {
            rawStackA = new ImodProcess(datasetName + ".st");
            erasedStackA = new ImodProcess(datasetName + "_fixed.st");
            coarseAlignedA = new ImodProcess(datasetName + ".preali");
            fineAlignedA = new ImodProcess(datasetName + ".ali");
            sampleA = new ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod");
            fullVolumeA = new ImodProcess(datasetName + "_full.rec");
            fullVolumeA.setSwapYZ(true);
            combinedTomogram = fullVolumeA;
            fiducialModelA = new ImodProcess();
      
          }
          trimmedVolume = new ImodProcess(datasetName + ".rec");
      */
      if (name.equals("rawStack")) {
        return new Tester(datasetName + ".st");
      }
      if (name.equals("erasedStack")) {
        return new Tester(datasetName + "_fixed.st");
      }
      if (name.equals("coarseAligned")) {
        return new Tester(datasetName + ".preali");
      }
      if (name.equals("fineAligned")) {
        return new Tester(datasetName + ".ali");
      }
      if (name.equals("sample")) {
        tester = new Tester("top.rec mid.rec bot.rec", "tomopitch.mod");
        setupNewSample(tester);
        return tester;
      }
      if (name.equals("fullVolume")) {
        tester = new Tester(datasetName + "_full.rec");
        tester.setSwapYZ(true);
        return tester;
      }
      if (name.equals("fiducialModel")) {
        tester = new Tester();
        setupNewFiducialModel(tester);
        return tester;
      }
      if (name.equals("trimmedVolume")) {
        return new Tester(datasetName + ".rec");
      }
      return null;
    }
    else {
      /* Tester should mimic original code
          else {
            rawStackA = new ImodProcess(datasetName + "a.st");
            rawStackB = new ImodProcess(datasetName + "b.st");
            erasedStackA = new ImodProcess(datasetName + "a_fixed.st");
            erasedStackB = new ImodProcess(datasetName + "b_fixed.st");
            coarseAlignedA = new ImodProcess(datasetName + "a.preali");
            coarseAlignedB = new ImodProcess(datasetName + "b.preali");
            fineAlignedA = new ImodProcess(datasetName + "a.ali");
            fineAlignedB = new ImodProcess(datasetName + "b.ali");
            sampleA = new ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod");
            sampleB = new ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod");
            fullVolumeA = new ImodProcess(datasetName + "a.rec");
            fullVolumeA.setSwapYZ(true);
            fullVolumeB = new ImodProcess(datasetName + "b.rec");
            fullVolumeB.setSwapYZ(true);
            combinedTomogram = new ImodProcess("sum.rec");
            combinedTomogram.setSwapYZ(true);
            patchVectorModel = new ImodProcess("patch_vector.mod");
            patchVectorModel.setModelView(true);
            matchCheck = new ImodProcess("matchcheck.mat matchcheck.rec");
            matchCheck.setSwapYZ(true);
            matchCheck.setFillCache(true);
            fiducialModelA = new ImodProcess();
            fiducialModelB = new ImodProcess();
          }
          trimmedVolume = new ImodProcess(datasetName + ".rec");
      */
      if (name == "rawStack") {
        if (axisID == AxisID.FIRST) {
          return new Tester(datasetName + "a.st");
        }
        else {
          return new Tester(datasetName + "b.st");
        }
      }
      if (name.equals("erasedStack")) {
        if (axisID == AxisID.FIRST) {
          return new Tester(datasetName + "a_fixed.st");
        }
        else {
          return new Tester(datasetName + "b_fixed.st");
        }
      }
      if (name.equals("coarseAligned")) {
        if (axisID == AxisID.FIRST) {
          return new Tester(datasetName + "a.preali");
        }
        else {
          return new Tester(datasetName + "b.preali");
        }
      }
      if (name.equals("fineAligned")) {
        if (axisID == AxisID.FIRST) {
          return new Tester(datasetName + "a.ali");
        }
        else {
          return new Tester(datasetName + "b.ali");
        }
      }
      if (name.equals("sample")) {
        if (axisID == AxisID.FIRST) {
          tester = new Tester("topa.rec mida.rec bota.rec", "tomopitcha.mod");
          setupNewSample(tester);
          return tester;
        }
        else {
          tester = new Tester("topb.rec midb.rec botb.rec", "tomopitchb.mod");
          setupNewSample(tester);
          return tester;
        }
      }
      if (name.equals("fullVolume")) {
        if (axisID == AxisID.FIRST) {
          tester = new Tester(datasetName + "a.rec");
          tester.setSwapYZ(true);
          return tester;
        }
        else {
          tester = new Tester(datasetName + "b.rec");
          tester.setSwapYZ(true);
          return tester;
        }
      }
      if (name.equals("combinedTomogram")) {
        tester = new Tester("sum.rec");
        tester.setSwapYZ(true);
        return tester;
      }
      if (name.equals("patchVectorModel")) {
        tester = new Tester("patch_vector.mod");
        tester.setModelView(true);
        setupNewPatchVectorModel(tester);
        return tester;
      }
      if (name.equals("matchCheck")) {
        tester = new Tester("matchcheck.mat matchcheck.rec");
        tester.setSwapYZ(true);
        tester.setFillCache(true);
        return tester;
      }
      if (name.equals("fiducialModel")) {
        if (axisID == AxisID.FIRST) {
          tester = new Tester();
          setupNewFiducialModel(tester);
          return tester;
        }
        else {
          tester = new Tester();
          setupNewFiducialModel(tester);
          return tester;
        }
      }
      if (name.equals("trimmedVolume")) {
        return new Tester(datasetName + ".rec");
      }
      return null;
    }
  }
  
  //testOpen and testModel functions mirror ImodManager's open and model functions
  
  private void testOpen(Tester tester, String key, AxisID axisID) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.open(key, axisID);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
  }

  private void testOpen(Tester tester, String key) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.open(key);
    assertTrue(imodManager.isOpen(key));
    tester.equals(imodManager.get(key));
    imodManager.quit(key);
  }
  
  private void testOpen(Tester tester, String key, AxisID axisID, String model) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.open(key, axisID, model);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
  }


  private void testModel(Tester tester,
    String key,
    AxisID axisID,
    String modelName,
    boolean modelMode,
    boolean preserveContrast) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.model(key, axisID, modelName, modelMode, preserveContrast);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
  }

  private void testModel(Tester tester,
    String key,
    AxisID axisID,
    String modelName,
    boolean modelMode) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.model(key, axisID, modelName, modelMode);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
  }

  private void testModel(Tester tester,
    String key,
    AxisID axisID,
    String modelName) throws AxisTypeException, SystemProcessException {
    ImodManager imodManager = new ImodManager(applicationManager, metaData);
    imodManager.model(key, axisID, modelName);
    assertTrue(imodManager.isOpen(key, axisID));
    tester.equals(imodManager.get(key, axisID));
    imodManager.openBeadFixer(key, axisID);
    imodManager.quit(key, axisID);
  }

  /**
      * Tester has an interface similar to ImodProcess.  It sets variables and checks
      * them against ImodAssistant.  It is use to check the current functionalty
      * against the functionality that existed before ImodAssistant was introduced.
      * 
      * @author sueh
      */
  private class Tester {

    //Member variables mirror ImodAssistant
    
    private String datasetName = "";
    private boolean swapYZ = false;
    private boolean modelView = false;
    private boolean fillCache = false;
    private boolean useModv = false;
    private boolean outputWindowID = true;
    private boolean preserveContrast = false;   
    private String modelName = "";
    private String mode = ImodAssistant.movie;

    

    public String toString() {
      return getClass().getName() + "[" + paramString() + "]";
    }

    protected String paramString() {
      return ",datasetName="
        + datasetName
        + ",modelName="
        + modelName
        + ",swapYZ"
        + swapYZ
        + ",modelView"
        + modelView
        + ",fillCache="
        + fillCache
        + ",useModv="
        + useModv
        + ",outputWindowID="
        + outputWindowID
        + ",mode="
        + mode
 //       + ",modelModeSet="
 //       + modelModeSet
        + ",preserveContrast="
        + preserveContrast;
    }

    Tester() {
    }
    Tester(String datasetName) {
      this.datasetName = new String(datasetName);
    }
    Tester(String datasetName, String modelName) {
      this.datasetName = new String(datasetName);
      setModelName(modelName);
    }


    public void equals(ImodAssistant imod) {
      assertTrue(imod.getDatasetName().equals(datasetName));
      //modelName can be overridden
      assertTrue(modelName.equals(imod.getModelName()) || modelName.equals(imod.getModelNameUsed()));
      //mode can be overridden
      assertTrue(mode == imod.getMode() || mode == imod.getModeUsed());
      assertEquals(imod.isSwapYZ(), swapYZ);
      assertEquals(imod.isModelView(), modelView);
      assertEquals(imod.isFillCache(), fillCache);
      assertEquals(imod.isUseModv(), useModv);
      assertEquals(imod.isPreserveContrast(), preserveContrast);
    }


    private void print(String key, ImodAssistant imod) {
      if (key == "modelName") {
        System.out.println("imodAssistant:");
        System.out.println(key + "=" + imod.getModelName());
        System.out.println(key + "Used=" + imod.getModelNameUsed());
        System.out.println("this: " + key + "=" + modelName);
      }
      else if (key == "mode") {
        System.out.println("imodAssistant:");
        System.out.println(key + "=" + imod.getMode());
        System.out.println(key + "Used=" + imod.getModeUsed());
        System.out.println("this: " + key + "=" + mode);      }
      else if (key == "useModv") {
        System.out.println(key + ": imod=" + imod.isUseModv() + ", this=" + useModv);
      }
    }


    //Interface mirrors ImodProcess
    
    
    public void setSwapYZ(boolean swapYZ) {
      this.swapYZ = swapYZ;
    }

    public void setModelView(boolean modelView) {
      this.modelView = modelView;
    }

    public void setFillCache(boolean fillCache) {
      this.fillCache = fillCache;
    }

    public void setUseModv(boolean useModv) {
      this.useModv = useModv;
    }

    public void setOutputWindowID(boolean outputWindowID) {
      this.outputWindowID = outputWindowID;
    }

    public void openModelPreserveContrast(String modelName) {
      setModelName(modelName);
      preserveContrast = true;
    }

    public void openModel(String modelName) {
      setModelName(modelName);
    }

    public void modelMode() {
      setMode(true);
    }

    public void movieMode() {
      setMode(false);
    }
    
    public void setMode(boolean modelMode) {
      if (modelMode) {
        mode = ImodAssistant.model;
      }
      else {
        mode = ImodAssistant.movie;
      }
    }
    
    public void setModelName(String modelName) {
      this.modelName = new String(modelName);
    }


  }
  
}