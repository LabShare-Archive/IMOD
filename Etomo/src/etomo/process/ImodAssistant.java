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
 * Testing:
 * Functions to check the state of both ImodAssistant and ImodProcess are
 * available.
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
 * <p> $$Log$
 * <p> $Revision 1.1  2003/11/15 01:40:12  sueh
 * <p> $bug242 created class to run ImodProcess functions for
 * <p> $ImodManager
 * <p> $$ </p>
 */

public class ImodAssistant {
  public static final String rcsid = "$$Id$$";

  protected ImodProcess process = null;
  


  private boolean openWithModel = false;
  private String modelName = "";
  private boolean useModv = false;

  private boolean setToMode = false;
  private boolean modelMode = false;
  
  private boolean preserveContrast = false;
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess().
   */
  public ImodAssistant() {
    process = new ImodProcess();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset).
   */
  public ImodAssistant(String dataset) {
    process = new ImodProcess(dataset);
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset, String model).
   */
  public ImodAssistant(String dataset, String model) {
    process = new ImodProcess(dataset, model);
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
   * configuration and open.
   * @param swapYZ
   * @param fillCache
   * @param modelView
   */
  public void setup(boolean swapYZ, boolean fillCache, boolean modelView) {
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

  



  //Configuration functions
  /**
   * Configures opening a process with a given model name.
   */
  public void configureOpenWithModel(String modelName) {
    openWithModel = true;
    configureModel(modelName);
  }
  
  /**
   * Configures opening process with a given model name and setting the mode to
   * model if setModelMode is true.
   *  
   * @param modelName
   * @param setModelMode
   */
  public void configureOpenWithModel(String modelName, boolean setToModelMode){
    openWithModel = true;
    configureModel(modelName, setToModelMode);
  }
  /**
   * Sets the model name.
   * 
   * @param modelName
   */
  public void configureModel(String modelName){
    this.modelName = modelName;
  }
  
  /**
   * Sets the model name and configures setting the mode to model if
   * setModelMode is true.
   *  
   * @param modelName
   * @param setModelMode
   */
  public void configureModel(String modelName, boolean setToModelMode){
    configureModel(modelName);
    if (setToModelMode) {
      configureSetToMode(true);
    }
  }
  
  /**
   * Configures opening a process and setting the mode to model or movie.
   */
  public void configureSetToMode(boolean modelMode){
    this.setToMode = true;
    this.modelMode = modelMode;
  }
  
  /**
   * Configures the mode (model or movie)
   * 
   * @param newModelMode
   */
  public void configureMode(boolean modelMode) {
    this.modelMode = modelMode;
  }
  
  /**
   * Configures opening a process using the -view option
   *
   */
  public void configureUseModv(boolean useModv) {
    this.useModv = useModv;
  }
  
  public void configurePreserveContrast(boolean preserveContrast) {
    this.preserveContrast = preserveContrast;
  }
  
  
  //functionality
  /**
   * Opens a process.
   * 
   * Configuration functions you can use with this function:
   * configureOpenWithModel()
   * configureSetToMode()
   * configureUseModv()
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void open() throws SystemProcessException{
    if (openWithModel) {
      open(modelName, setToMode);
    }
    else {
      process.open();
      if (setToMode) {
        setMode(modelMode);
      }
    }
  }
  /**
   * Opens a process using modelName.  Ignores configuration effecting mode.
   * 
   * Configuration functions you can use with this function:
   * configureUseModv()
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void open(String modelName) throws SystemProcessException {
    if (modelName == null) {
      throw new NullPointerException(toString()); 
    }
    process.setModelName(modelName);
    if (useModv) {
      process.setUseModv(useModv);
      process.setOutputWindowID(false);
    }
    process.open();
  }
  
  /**
   * Opens a process using modelName.  Sets the mode if setToMode = true.
   * 
   * Configuration functions you can use with this function:
   * configureSetMode()
   * configureUseModv()
   *  
   * @param modelName
   * @param setModelMode
   * @throws SystemProcessException
   */
  public void open(String modelName, boolean setToMode) throws SystemProcessException {
    open(modelName);
    if (setToMode) {
      setMode(modelMode);
    }
  }
  

  /**
   * Opens a model using modelName.  Ignores configuration effecting mode.
   * 
   * Configuration functions you can use with this function:
   * configurePreserveContrast()
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void model(String modelName) throws SystemProcessException {
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
   * Opens a model.  Sets the mode.  Ignores model and mode configuration.
   * 
   * Configuration functions you can use with this function:
   * configurePreserveContrast()
   * 
   * @param modelName
   * @param modelMode
   * @throws SystemProcessException
   */
  public void model(String modelName, boolean modelMode) throws SystemProcessException {
    model(modelName);
    setMode(modelMode);
  }
  
  /**
   * Sets the mode.  Ignores mode configuration.
   * 
   * @param modelModeIn
   * @throws SystemProcessException
   */
  public void setMode(boolean modelMode) throws SystemProcessException {
    if (modelMode) {
      process.modelMode();
    }
    else {
      process.movieMode();
    }
  }
  
  /**
   * Opens the Bead Fixer window.
   * 
   * @throws SystemProcessException
   */
  public void openBeadFixer() throws SystemProcessException {
    process.openBeadFixer();
  }
  /**
   * @return true if process is running
   */
  public boolean isOpen() {
    return process.isRunning();
  }
  
  /**
   * Tells process to quit.
   * 
   * @throws SystemProcessException
   */
  public void quit() throws SystemProcessException {
    process.quit();
  }


  
  public boolean isPreserveContrast() {
    return preserveContrast;
  }
  
  public boolean isUseModv() {
    return useModv;
  }
  
  public boolean isOpenWithModel() {
    return openWithModel;
  }
  public String getModelName() {
    return modelName;
  }
  public boolean isSetToMode() {
    return setToMode;
  }
  public boolean isModelMode() {
    return modelMode;
  }
  


  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",process="
      + process
      + ", useModv="
      + useModv
      + ", preserveContrast="
      + preserveContrast
      + ", openWithModel="
      + openWithModel
      + ", modelName="
      + modelName
      + ", setToMode="
      + setToMode
      + ", modelMode="
      + modelMode;
  }

  



}
