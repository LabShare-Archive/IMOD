/*
 * Created on Nov 17, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.process;

import junit.framework.TestCase;

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

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    String[] args = new String[1];
    args[0] = new String("");
    applicationManager = new ApplicationManager(args);
    metaData = new MetaData();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Constructor for ImodManagerTest.
   * @param arg0
   */
  public ImodManagerTest(String arg0) {
    super(arg0);
  }
  


  //Regression test
  
  final public void testImodManager() throws AxisTypeException {
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


  private void setupNewSample(Tester tester) {
    //sample.modelMode();
    tester.modelMode();
  }

  private void setupNewPatchVectorModel(Tester tester) {
    //patchVectorModel.modelMode();
    tester.modelMode();
  }

  private void setupNewFiducialModel(Tester tester) {
    //fiducialModel.setUseModv(true);
    //fiducialModel.setOutputWindowID(false);
    tester.setUseModv(true);
    tester.setOutputWindowID(false);
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
      if (name.equals("trialTomogram")) {
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
      if (name.equals("trialTomogram")) {
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
    private boolean preserveContrast = false;   
    private String modelName = "";
    private String mode = ImodState.MOVIE;

    

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
        + ",mode="
        + mode
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


    public void equals(ImodState imodState) {
      assertTrue(imodState.getDatasetName().equals(datasetName));
      assertTrue(imodState.getModelName().equals(modelName));
      assertTrue(imodState.getMode().equals(mode));
      assertEquals(imodState.isSwapYZ(), swapYZ);
      assertEquals(imodState.isModelView(), modelView);
      assertEquals(imodState.isFillCache(), fillCache);
      assertEquals(imodState.isUseModv(), useModv);
      assertEquals(imodState.isPreserveContrast(), preserveContrast);
    }


    private void print(String key, ImodState imodState) {
      if (key == "modelName") {
        System.out.println("imodState:");
        System.out.println(key + "=" + imodState.getModelName());
        System.out.println("this: " + key + "=" + modelName);
      }
      else if (key == "mode") {
        System.out.println("imodState:");
        System.out.println(key + "=" + imodState.getMode());
        System.out.println("this: " + key + "=" + mode);      }
      else if (key == "useModv") {
        System.out.println(key + ": imodState=" + imodState.isUseModv() + ", this=" + useModv);
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
        mode = ImodState.MODEL;
      }
      else {
        mode = ImodState.MOVIE;
      }
    }

    public void setModelName(String modelName) {
      this.modelName = new String(modelName);
    }


  }


}