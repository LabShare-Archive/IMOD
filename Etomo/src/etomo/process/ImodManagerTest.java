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

  private Tester newTester(String name) {
    return newTester(name, AxisType.SINGLE_AXIS, AxisID.ONLY);
  }

  private Tester newTester(String name, AxisID axisID) {
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
        return new Tester("top.rec mid.rec bot.rec", "tomopitch.mod");
      }
      if (name.equals("fullVolume")) {
        tester = new Tester(datasetName + "_full.rec");
        tester.setSwapYZ(true);
        return tester;
      }
      if (name.equals("fiducialModel")) {
        return new Tester();
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
          return new Tester("topa.rec mida.rec bota.rec", "tomopitcha.mod");
        }
        else {
          return new Tester("topb.rec midb.rec botb.rec", "tomopitchb.mod");
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
          return new Tester();
        }
        else {
          return new Tester();
        }
      }
      if (name.equals("trimmedVolume")) {
        return new Tester(datasetName + ".rec");
      }
      return null;
    }
  }

  final public void testImodManagerWithoutMap() {
    Tester tester;
    //Test single axis  
    setUpSingle();
    imodManager = new ImodManager(applicationManager, metaData, false);
    //rawStack
    tester = newTester("rawStack");
    tester.equals(imodManager.rawStackA);
    //erasedStack
    tester = newTester("erasedStack");
    tester.equals(imodManager.erasedStackA);
    //coarseAligned
    tester = newTester("coarseAligned");
    tester.equals(imodManager.coarseAlignedA);
    //fineAligned
    tester = newTester("fineAligned");
    tester.equals(imodManager.fineAlignedA);
    //sample
    tester = newTester("sample");
    tester.equals(imodManager.sampleA);
    //fullVolume
    tester = newTester("fullVolume");
    tester.equals(imodManager.fullVolumeA);
    //combinedTomogram
    assertEquals(imodManager.combinedTomogram, imodManager.fullVolumeA);
    //fiducialModel
    tester = newTester("fiducialModel");
    tester.equals(imodManager.fiducialModelA);
    //trimmedVolume    
    tester = newTester("trimmedVolume");
    tester.equals(imodManager.trimmedVolume);
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
    imodManager = new ImodManager(applicationManager, metaData, false);
    AxisID a = AxisID.FIRST;
    AxisID b = AxisID.SECOND;
    AxisType dual = AxisType.DUAL_AXIS;
    //rawStack
    tester = newTester("rawStack", a);
    tester.equals(imodManager.rawStackA);
    tester = newTester("rawStack", b);
    tester.equals(imodManager.rawStackB);
    //erasedStack
    tester = newTester("erasedStack", a);
    tester.equals(imodManager.erasedStackA);
    tester = newTester("erasedStack", b);
    tester.equals(imodManager.erasedStackB);
    //coarseAligned
    tester = newTester("coarseAligned", a);
    tester.equals(imodManager.coarseAlignedA);
    tester = newTester("coarseAligned", b);
    tester.equals(imodManager.coarseAlignedB);
    //fineAligned
    tester = newTester("fineAligned", a);
    tester.equals(imodManager.fineAlignedA);
    tester = newTester("fineAligned", b);
    tester.equals(imodManager.fineAlignedB);
    //sample
    tester = newTester("sample", a);
    tester.equals(imodManager.sampleA);
    tester = newTester("sample", b);
    tester.equals(imodManager.sampleB);
    //fullVolume 
    tester = newTester("fullVolume", a);
    tester.equals(imodManager.fullVolumeA);
    tester = newTester("fullVolume", b);
    tester.equals(imodManager.fullVolumeB);
    //combinedTomogram
    tester = newTester("combinedTomogram", dual);
    tester.equals(imodManager.combinedTomogram);
    //patchVectorModel
    tester = newTester("patchVectorModel", dual);
    tester.equals(imodManager.patchVectorModel);
    //matchCheck
    tester = newTester("matchCheck", dual);
    tester.equals(imodManager.matchCheck);
    //fiducialModel
    tester = newTester("fiducialModel", a);
    tester.equals(imodManager.fiducialModelA);
    tester = newTester("fiducialModel", b);
    tester.equals(imodManager.fiducialModelB);
    //trimmedVolume
    tester = newTester("trimmedVolume", dual);
    tester.equals(imodManager.trimmedVolume);
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
      imodManager.get("combinedTomogram"),
      imodManager.get("fullVolume"));
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

  final public void testOpenCoarseAligned()
    throws AxisTypeException, SystemProcessException {
    String key = "coarseAligned";
    Tester tester;
    AxisID axisID;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    //test wi
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openCoarseAligned(axisID);
    tester.equals(imodManager.coarseAlignedA);
    imodManager.quitCoarseAligned(axisID);
    //test new
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openCoarseAligned(axisID);
    test(imodManager, tester, key);
  }

  final public void testModelCoarseAligned()
    throws AxisTypeException, SystemProcessException {
    String key = "coarseAligned";
    Tester tester;
    AxisID axisID;
    String modelName = "BBa.fid";
    boolean preserveContrast = true;
    boolean modelMode = true;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpModel(tester, key, preserveContrast, modelName, modelMode);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.modelCoarseAligned(
      modelName,
      axisID,
      modelMode,
      preserveContrast);
    tester.equals(imodManager.coarseAlignedA);
    imodManager.quitCoarseAligned(axisID);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.modelCoarseAligned(
      modelName,
      axisID,
      modelMode,
      preserveContrast);
    test(imodManager, tester, key);
  }

  final public void testModelErasedStack()
    throws AxisTypeException, SystemProcessException {
    String key = "erasedStack";
    Tester tester;
    AxisID axisID;
    String modelName = "BBa.fid";
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpModel(tester, key, false, modelName, false);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.modelErasedStack(modelName, axisID);
    tester.equals(imodManager.erasedStackA);
    imodManager.quitErasedStack(axisID);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.modelErasedStack(modelName, axisID);
    test(imodManager, tester, key);
  }

  public void testModelRawStack()
    throws AxisTypeException, SystemProcessException {
    String key = "rawStack";
    Tester tester;
    AxisID axisID;
    String modelName = "patch_vector.mod";
    boolean modelMode = true;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpModel(tester, key, false, modelName, modelMode);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.modelRawStack(modelName, axisID, modelMode);
    tester.equals(imodManager.rawStackA);
    imodManager.quitRawStack(axisID);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.modelRawStack(modelName, axisID, modelMode);
    test(imodManager, tester, key);
  }

  public void testMatchingModel() throws AxisTypeException, SystemProcessException {
    String key = "fullVolume";
    Tester testerA, testerB;
    //only testing single - regression test rather then a full test.
    setUpDual();
    testerA = newTester(key, AxisID.FIRST);
    testerB = newTester(key, AxisID.SECOND);   
    setUpMatchingModel(testerA, key, AxisID.FIRST);
    setUpMatchingModel(testerB, key, AxisID.SECOND);
    //  test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.matchingModel(datasetName);
    testerA.equals(imodManager.fullVolumeA);
    testerB.equals(imodManager.fullVolumeB);
    imodManager.quitFullVolume(AxisID.FIRST);
    imodManager.quitFullVolume(AxisID.SECOND);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.matchingModel(datasetName);
    test(imodManager, testerA, key, AxisID.FIRST);
    test(imodManager, testerB, key, AxisID.SECOND);
  }

  public void testPatchRegionModel()
    throws AxisTypeException, SystemProcessException {
      String key = "fullVolume";
      Tester tester;
      AxisID axisID = AxisID.SECOND;
      //only testing single - regression test rather then a full test.
      setUpDual();
      tester = newTester(key, axisID);  
      setUpPatchRegionModel(tester, key, axisID);
      //  test without map
      imodManager = new ImodManager(applicationManager, metaData, false);
      imodManager.patchRegionModel(axisID);
      tester.equals(imodManager.fullVolumeB);
      imodManager.quitFullVolume(axisID);
      //test with map
      imodManager = new ImodManager(applicationManager, metaData);
      imodManager.patchRegionModel(axisID);
      test(imodManager, tester, key, axisID);
  }


  final public void testOpenFiducialModel()
    throws AxisTypeException, SystemProcessException {
    String key = "fiducialModel";
    Tester tester;
    AxisID axisID;
    String model = "BBa.fid";
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpOpen(tester, key, model);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openFiducialModel(model, axisID);
    tester.equals(imodManager.fiducialModelA);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openFiducialModel(model, axisID);
    test(imodManager, tester, key);
  }

  final public void testOpenFullVolume()
    throws AxisTypeException, SystemProcessException {
    String key = "fullVolume";
    Tester tester;
    AxisID axisID;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpOpen(tester, key, null);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openFullVolume(axisID);
    tester.equals(imodManager.fullVolumeA);
    imodManager.quitFullVolume(axisID);
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openFullVolume(axisID);
    test(imodManager, tester, key);
  }

  final public void testOpenSample()
    throws AxisTypeException, SystemProcessException {
    String key = "sample";
    Tester tester;
    AxisID axisID;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    axisID = AxisID.ONLY;
    tester = newTester(key);
    setUpOpen(tester, key, null);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openSample(axisID);
    tester.equals(imodManager.sampleA);
    imodManager.quitSample(axisID);
    //test new
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openSample(axisID);
    test(imodManager, tester, key);
  }

  final public void testOpenMatchCheck() throws SystemProcessException, AxisTypeException {
    String key = "matchCheck";
    Tester tester;
    //only testing single - regression test rather then a full test.
    setUpDual();
    tester = newTester(key, AxisType.DUAL_AXIS);
    //test without map
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openMatchCheck();
    tester.equals(imodManager.matchCheck);
    imodManager.quitMatchCheck();
    //test with map
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openMatchCheck();
    test(imodManager, tester, key);
  }

  private void setUpOpen(
    Tester tester,
    String key,
    String model) { //  fiducialModel.setModelName(model);
    //  fiducialModel.setUseModv(true);
    //  fiducialModel.setOutputWindowID(false);
    if (key == "fiducialModel") {
      tester.setModelName(model);
      tester.setUseModv(true);
      tester.setOutputWindowID(false);
    } //  fullVolume.setModelName("");
    else if (key == "fullVolume") {
      tester.setModelName("");
    } //  patchVectorModel.modelMode();
    else if (key == "patchVectorModel") {
      tester.modelMode();
    } //    sample.modelMode();
    else if (key == "sample") {
      tester.modelMode();
    }
  }

  private void setUpModel(
    Tester tester,
    String key,
    boolean preserveContrast,
    String modelName,
    boolean modelMode) { //    if (preserveConstrast) {
    //      coarseAligned.openModelPreserveContrast(modelName);
    //    }
    //    else {
    //      coarseAligned.openModel(modelName);
    //    }
    //    coarseAligned.modelMode();
    //    if (modelMode) {
    //      coarseAligned.modelMode();
    //    }
    //    else {
    //      coarseAligned.movieMode();
    //    }
    //    coarseAligned.configurePreserveContrast(preserveConstrast);
    //    coarseAligned.model(modelName, modelMode);
    if (key == "coarseAligned") {
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
    } //  erasedStack.openModel(modelName);
    else if (key.equals("erasedStack")) {
      tester.openModel(modelName);
    } //rawStack.openModel(modelName);
    //if (modelMode) {
    //  rawStack.modelMode();
    //}
    //else {
    //  rawStack.movieMode();
    //}
    else if (key.equals("rawStack")) {
      tester.openModel(modelName);
      if (modelMode) {
        tester.modelMode();
      }
      else {
        tester.movieMode();
      }
    }
  }

  private void setUpMatchingModel(Tester tester, String key, AxisID axisID) {
    //fullVolumeA.open();
    //fullVolumeA.openModel(datasetName + "a.matmod");
    //fullVolumeA.modelMode();
    //
    //fullVolumeB.open();
    //fullVolumeB.openModel(datasetName + "b.matmod");
    //fullVolumeB.modelMode();
    if (key.equals("fullVolume")) {
      if (axisID == AxisID.FIRST) {
        tester.openModel(datasetName + "a.matmod");
        tester.modelMode();
      }
      else if (axisID == AxisID.SECOND) {
        tester.openModel(datasetName + "b.matmod");
        tester.modelMode();
      }
    }
  }
  
  private void setUpPatchRegionModel(Tester tester, String key, AxisID axisID) {
    //if (axisID == AxisID.SECOND) {
      //fullVolumeB.open();
      //fullVolumeB.openModel("patch_region.mod");
      //fullVolumeB.modelMode();
    //}
    //else {
      //fullVolumeA.open();
      //fullVolumeA.openModel("patch_region.mod");
      //fullVolumeA.modelMode(); 
    //}
    if (key.equals("fullVolume")){
      tester.openModel("patch_region.mod");
      tester.modelMode();
    }
  }

  final public void testOpenCombinedTomogram() throws SystemProcessException, AxisTypeException {
    String key = "combinedTomogram";
    Tester tester;
    //only testing single - regression test rather then a full test.
    setUpSingle();
    tester = newTester("fullVolume");
    //test deprecated
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openCombinedTomogram();
    tester.equals(imodManager.combinedTomogram);
    imodManager.quitCombinedTomogram();
    //test new
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openCombinedTomogram();
    test(imodManager, tester, key);
  }

  final public void testOpenPatchVectorModel() throws SystemProcessException, AxisTypeException {
    String key = "patchVectorModel";
    Tester tester;
    //only testing single - regression test rather then a full test.
    setUpDual();
    tester = newTester(key, AxisType.DUAL_AXIS);
    setUpOpen(tester, key, null);
    //test deprecated
    imodManager = new ImodManager(applicationManager, metaData, false);
    imodManager.openPatchVectorModel();
    tester.equals(imodManager.patchVectorModel);
    imodManager.quitPatchVectorModel();
    //test new
    imodManager = new ImodManager(applicationManager, metaData);
    imodManager.openPatchVectorModel();
    test(imodManager, tester, key);
  }

  private void test(ImodManager imodManager, Tester tester, String key)
    throws SystemProcessException, AxisTypeException {
    ImodAssistant imod = imodManager.get(key);
    tester.equals(imod);
    imodManager.quit(key);
  }
  private void test(ImodManager imodManager, Tester tester, String key, AxisID axisID)
    throws AxisTypeException, SystemProcessException {
    ImodAssistant imod = imodManager.get(key, axisID);
    tester.equals(imod);
    imodManager.quit(key, axisID);
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
  } /**
      * Tester has an interface similar to ImodProcess.  It sets variables and checks
      * them against ImodAssistant.  It is use to check the current functionalty
      * against the functionality that existed before ImodAssistant was introduced.
      * 
      * @author sueh
      */
  private class Tester {

    private String datasetName = "";
    private boolean swapYZ = false;
    private boolean modelView = false;
    private boolean fillCache = false;
    private boolean useModv = false;
    private boolean outputWindowID = true;
    private boolean preserveContrast = false;
        
    private String modelName = null;
    private String modelName_ = null;
    private String modelName__ = null;

    private boolean modelMode = false;
    private boolean modelModeSet = false;
    private boolean modelMode_ = false;
    private boolean modelMode_Set = false;
    private boolean modelMode__ = false;
    private boolean modelMode__Set = false;
    

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
        + ",modelMode="
        + modelMode
        + ",modelModeSet="
        + modelModeSet
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
      String modelName = imod.getModelName();
      boolean modelMode = imod.isModelMode();
      assertTrue(modelName.equals("") ||
        modelName.equals(this.modelName)
          || modelName.equals(modelName_)
          || modelName.equals(modelName__)
          || (this.modelName == null
            && modelName_ == null
            && modelName__ == null));
      assertTrue(
        modelMode == this.modelMode
          || modelMode == modelMode_
          || modelMode == modelMode__
          || (!this.modelModeSet && !modelMode_Set && !modelMode__Set));
      assertEquals(imod.isUseModv(), useModv);
      assertEquals(imod.isPreserveContrast(), preserveContrast);
      equals(imod.process);
    }

    public void equals(ImodProcess imod) {
      String modelName = imod.getModelName();
      assertTrue(imod.getDatasetName().equals(datasetName));
      assertTrue(modelName.equals("")||
        modelName.equals(this.modelName)
          || modelName.equals(modelName_)
          || modelName.equals(modelName__)
          || (this.modelName == null
            && modelName_ == null
            && modelName__ == null));
      assertEquals(imod.getSwapYZ(), swapYZ);
      assertEquals(imod.isModelView(), modelView);
      assertEquals(imod.isFillCache(), fillCache);
      assertEquals(imod.isUseModv(), useModv);
      assertEquals(imod.isOutputWindowID(), outputWindowID);
    }

    public void print(String key, ImodAssistant imod) {
      if (key == "modelName") {
        System.out.println(
          key
            + ": assist="
            + imod.getModelName()
            + ",proc="
            + imod.process.getModelName());
            System.out.println("this: modelName=" + modelName + ",modelName_=" + modelName_ + ",modelName__=" + modelName__);
      }
      if (key == "modelMode") {
        System.out.println(
          key + ": assist=" + imod.isModelMode() + ",this=" + modelMode);
      }
    }

    public void print(String key, ImodProcess imod) {
      if (key == "modelName") {
        System.out.println(
          key + ": proc=" + imod.getModelName() + ",this=" + modelName);
      }
    }

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
      //do a backup
      if (modelModeSet) {
        if (modelMode_Set) {
          modelMode__Set = true;
          modelMode__ = modelMode_;
        }
        modelMode_Set = true;
        modelMode_ = this.modelMode;
      }
      modelModeSet = true;
      this.modelMode = modelMode;
    }
    
    public void setModelName(String modelName) {
      //do a backup
      if (this.modelName != null) {
        if (modelName_ != null) {
          modelName__ = modelName_;
        }
        modelName_ = this.modelName;
      }
      this.modelName = new String(modelName);
    }


  }
  
}