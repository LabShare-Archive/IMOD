package etomo.process;

import java.lang.IllegalArgumentException;
//import java.lang.IllegalStateException;
import java.lang.NullPointerException;

import etomo.process.ImodProcess;
import etomo.process.SystemProcessException;
import etomo.type.AxisID;

/**
 * <p>Description:
 * ImodAssistant constructs a private ImodProcess instance.  It should be used
 * to perform any function on the ImodProcess instance that is required by the
 * ImodManager.</p>
 * 
 * <p>Use:
 * 1. Construct an ImodAssistant with all parameters required to create the
 *    correct ImodProcess instance.
 * 2. If necessary, run setup() to do the initial configuration of the
 *    ImodProcess instance.
 * 3. Use set functions and run other functions as needed.
 * 
 * Settings:
 * - All options are initially false, except for outputWindowID.
 * 
 * <p>Copyright: Copyright(c) 2002, 2003</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$$ </p>
 */

public class ImodAssistant {
  public static final String rcsid = "$$Id$$";

  private ImodProcess process = null;
  private boolean useModv = false;
  private boolean outputWindowID = true;
  private boolean preserveContrast = false;

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess().
   */
  public ImodAssistant() {
    System.out.println("in ImodAssistant()");
    System.out.println(toString());
    process = new ImodProcess();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset).
   */
  public ImodAssistant(String name) {
    System.out.println("in ImodAssistant(String name)");
    System.out.println(toString());
    process = new ImodProcess(name);
  }
  
  /**
   * Use this constructor to insert the axis letter and create an instance of
   * ImodProcess using ImodProcess(String dataset).  This will work for any kind
   * of AxisID.
   * 
   * Example:
   * ImodAssistant(axisID, "dataset", "_fixed.st");
   * will cause one of these calls:
   * ImodProcess("dataset_fixed.st");
   * ImodProcess("dataseta_fixed.st");
   * ImodProcess("datasetb_fixed.st");
   */
  public ImodAssistant(AxisID tempAxisID, String datasetName, String datasetExt) {
    System.out.println("in  ImodAssistant(AxisID tempAxisID, String datasetName, String datasetExt)");
    System.out.println(toString());
    String axisExtension = tempAxisID.getExtension();
    if (axisExtension == "ERROR") {
      throw new IllegalArgumentException(tempAxisID.toString());
    }
    process = new ImodProcess(datasetName + axisExtension + datasetExt);
  }
  
  /**
   * Use this constructor to insert the axis letter and create an instance of
   * ImodProcess using ImodProcess(String dataset, String model).  This will
   * work for any kind of AxisID.
   * 
   * Example:
   * ImodAssistant(axisID, "top", "mid", "bot" ,".rec", "tomopitch" ,".mod");
   * will cause one of these calls:
   * ImodProcess("top.rec mid.rec bot.rec", "tomopitch.mod");
   * ImodProcess("topa.rec mida.rec bota.rec", "tomopitcha.mod");
   * ImodProcess("topb.rec midb.rec botb.rec", "tomopitchb.mod");
   */
  public ImodAssistant(
    AxisID tempAxisID,
    String datasetName1,
    String datasetName2,
    String datasetName3,
    String datasetExt,
    String modelName,
    String modelExt) {
    System.out.println("in ImodAssistant(AxisID tempAxisID, String datasetName1, String datasetName2, String datasetName3, String datasetExt, String modelName, String modelExt)");
    System.out.println(toString());
    String axisExtension = tempAxisID.getExtension();
    if (axisExtension == "ERROR") {
      throw new IllegalArgumentException(tempAxisID.toString());
    }
    process =
      new ImodProcess(
        datasetName1
          + axisExtension
          + datasetExt
          + " "
          + datasetName2
          + axisExtension
          + datasetExt
          + " "
          + datasetName3
          + axisExtension
          + datasetExt,
        modelName + axisExtension + modelExt);
  }


  /**
   * Configures the ImodProcess instance.  Use when necessary before calling
   * open.
   * @param swapYZ
   * @param fillCache
   * @param modelView
   */
  public void setup(boolean swapYZ, boolean fillCache, boolean modelView) {
    System.out.println("in setup");
    System.out.println(toString());
    if (swapYZ) {
      process.setSwapYZ(swapYZ);
    }
    if (fillCache) {
      process.setFillCache(fillCache);
    }
    if (modelView) {
      process.setModelView(modelView);
    }
  }
  
  /**
   * Calls ImodProcess.open().  Use when opening a model is unnecessary or will
   * be done later.
   * @throws SystemProcessException
   */
  public void open() throws SystemProcessException {
    System.out.println("in open");
    System.out.println(toString());
    process.open();
  }
  
  /**
   * Sets the model name and calls ImodProcess.open().
   * 
   * To use "imod -view", call setUseModv(true) and setOutputWindowID(false)
   * before calling this function.
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void openWithModel(String modelName) throws SystemProcessException {
    System.out.println("in openWithModel(String modelName)");
    System.out.println(toString());
    if (modelName == null) {
      throw new NullPointerException(toString()); 
    }
    process.setModelName(modelName);
    if (useModv) {
      process.setUseModv(useModv);
    }
    if (!outputWindowID) {
      process.setOutputWindowID(outputWindowID);
    }
    process.open();
  }
  
  /**
   * Sets the model name, calls ImodProcess.open(), and sets the mode to model
   * if setModelMode is true.
   * 
   * This will use "imod -view" if useModv is true.
   *  
   * @param modelName
   * @param setModelMode
   * @throws SystemProcessException
   */
  public void openWithModel(String modelName, boolean setModelMode) throws SystemProcessException {
    System.out.println("in openWithModel(String modelName, boolean setModelMode)");
    System.out.println(toString());
    openWithModel(modelName);
    if (setModelMode) {
      process.modelMode();
    }
  }
  
  /**
   * Calls ImodProcess.open() and sets the mode to model.
   * @throws SystemProcessException
   */
  public void openInModelMode() throws SystemProcessException {
    System.out.println("in openInModelMode");
    System.out.println(toString());
    process.open();
    process.modelMode();
  }

  /**
   * Opens a model.
   * 
   * To preserve contrast, call setPreserveContrast(true) before calling this
   * function.
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void model(String modelName) throws SystemProcessException {
    System.out.println("in model(String modelName)");
    System.out.println(toString());
    if (modelName == null) {
      throw new NullPointerException(toString()); 
    }
    if (preserveContrast) {
      process.openModelPreserveContrast(modelName);
    }
    else {
      process.openModel(modelName);
    }
  }
  
  /**
   * Opens a model.
   * Sets the mode to model if modelMode is true.
   * Sets the mode to movie if modelMode is false.
   * 
   * To preserve contrast, call setPreserveContrast(true) before calling this
   * function.
   * 
   * @param modelName
   * @param modelMode
   * @throws SystemProcessException
   */
  public void model(String modelName, boolean modelMode) throws SystemProcessException {
    System.out.println("in model(String modelName, boolean modelMode)");
    System.out.println(toString());
    model(modelName);
    if (modelMode) {
      process.modelMode();
    }
    else {
      process.movieMode();
    }
  }
  
  /**
   * Calls ImodProcess.openBeadFixer().
   * @throws SystemProcessException
   */
  public void openBeadFixer() throws SystemProcessException {
    System.out.println("in openBeadFixer");
    System.out.println(toString());
    process.openBeadFixer();
  }
  /**
   * Calls ImodProcess.isRunning().
   * @return
   */
  public boolean isOpen() {
    System.out.println("in isOpen");
    System.out.println(toString());
    return process.isRunning();
  }
  
  /**
   * Calls ImodProcess.quit().
   * @throws SystemProcessException
   */
  public void quit() throws SystemProcessException {
    System.out.println("in quit");
    System.out.println(toString());
    process.quit();
  }


  public void setPreserveContrast(boolean setPreserveContrast) {
    preserveContrast = setPreserveContrast;
  }
  public void setUseModv(boolean setUseModv) {
    useModv = setUseModv;
  }
  public void setOutputWindowID(boolean setOutputWindowID) {
    outputWindowID = setOutputWindowID;
  }

  public boolean isPreserveContrast() {
    return preserveContrast;
  }
  public boolean isUseModv() {
    return useModv;
  }
  public boolean isOutputWindowID() {
    return outputWindowID;
  }
  
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",process="
      + process
      + ", useModv="
      + useModv
      + ", outputWindowID="
      + outputWindowID
      + ", preserveContrast="
      + preserveContrast;
  }


}
