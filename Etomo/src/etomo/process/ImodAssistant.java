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
 * ImodManager.
 * 
 * ImodAssistant should mirror ImodProcess's state as much as
 * possible without changing the previously existing functionality.</p>
 * 
 * <p>Use:
 * 1. Construct an ImodAssistant with all parameters required to create the
 *    correct ImodProcess instance.
 * 2. If necessary, run setup() to do the initial configuration of the
 *    ImodProcess instance.
 * 3. Settings that are constant can be set using the configure functions
 *    prior to open() and model().
 *    Examples:  
 *    fiducialModel.setUseModv(true) => configureUseModv(true);
 *    fullVolume.setModelName("") => configureModelName("");
 *    patchVectorModel.modelMode() => configureSetToMode(true);
 *    coarseAligned.openModelPreserveContrast(modelName); =>
 *      configurePreserveContrast();
 * 4. Settings that are variable can be passed to open() or model(), or to the
 *    configuration functions just before running open() or model().
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
 * <p> $$Log$
 * <p> $Revision 1.2  2003/11/21 23:49:46  sueh
 * <p> $bug242 ImodAssistant - made it more configureable,
 * <p> $improved the interface
 * <p> $
 * <p> $Revision 1.1  2003/11/15 01:40:12  sueh
 * <p> $bug242 created class to run ImodProcess functions for
 * <p> $ImodManager
 * <p> $$ </p>
 */

public class ImodAssistant {
  public static final String rcsid = "$$Id$$";

  protected ImodProcess process = null;
  
  protected static final String model = "model";
  protected static final String movie = "movie";

  private String datasetName = "";
  
  private boolean openWithModel = false;
  private String modelName = "";
  private String modelNameUsed = "";

  private boolean setToMode = false;
  private String mode = movie;
  private String modeUsed = "";
  
  private boolean swapYZ = false;
  private boolean fillCache = false;
  private boolean modelView = false;
  private boolean useModv = false;
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
  public ImodAssistant(String datasetName) {
    this.datasetName = datasetName;
    process = new ImodProcess(datasetName);
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset, String model).
   */
  public ImodAssistant(String datasetName, String modelName) {
    this.datasetName = datasetName;
    this.modelName = modelName;
    process = new ImodProcess(datasetName, modelName);
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
  public ImodAssistant(AxisID axisID, String datasetName, String datasetExt) {
    String axisExtension = axisID.getExtension();
    if (axisExtension == "ERROR") {
      throw new IllegalArgumentException(axisID.toString());
    }
    this.datasetName = datasetName + axisExtension + datasetExt;
    process = new ImodProcess(this.datasetName);
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
    datasetName = datasetName1 + axisExtension + datasetExt + " " + datasetName2 + axisExtension + datasetExt + " " + datasetName3 + axisExtension + datasetExt;
    this.modelName = modelName + axisExtension + modelExt;
    process = new ImodProcess(datasetName, this.modelName);
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
      this.swapYZ = swapYZ;
      process.setSwapYZ(this.swapYZ);
    }
    if (fillCache) {
      this.fillCache = fillCache;
      process.setFillCache(this.fillCache);
    }
    if (modelView) {
      this.modelView = modelView;
      process.setModelView(this.modelView);
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
    setToMode = true;
    configureMode(modelMode);
  }
  
  /**
   * Configures the mode (model or movie)
   * 
   * @param newModelMode
   */
  public void configureMode(boolean modelMode) {
    if (modelMode) {
      mode = model;
    }
    else {
      mode = movie;
    }
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
      open(modelName);
      if (setToMode) {
        setMode(mode);
      }
    }
    else {
      process.open();
      if (setToMode) {
        setMode(mode);
      }
    }
  }
  /**
   * Opens a process using the modelName parameter.  Ignores configuration 
   * effecting mode.  Does not alter configuration.
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
    modelNameUsed = modelName;
    if (useModv) {
      process.setUseModv(useModv);
      process.setOutputWindowID(false);
    }
    process.open();
  } 
  
  public void open(String modelName, boolean modelMode) throws SystemProcessException {
    open(modelName);
    setMode(modelMode);
  } 


  /**
   * Opens a model using the modelName parameter.  Ignores configuration 
   * effecting mode.  Does not alter configuration.
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
    modelNameUsed = modelName;
  }

  /**
   * Opens a model using the modelName parameter.  Sets the mode based on the 
   * modelModel parameter.  Does not alter configuration.
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
   * Sets the mode based on the modelMode parameter.  Does not alter
   * configuration.
   * 
   * @param modelModeIn
   * @throws SystemProcessException
   */
  private void setMode(String mode) throws SystemProcessException {
    if (mode == model) {
      process.modelMode();
    }
    else {
      process.movieMode();
    }
    modeUsed = mode;
  }
  
  private void setMode(boolean modelMode) throws SystemProcessException {
    if (modelMode) {
      setMode(model);
    }
    else {
      setMode(movie);
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


  
  public String getDatasetName() {
    return datasetName;
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
  public String getMode() {
    return mode;
  }
  public boolean isSwapYZ() {
    return swapYZ;
  }
  public boolean isFillCache() {
    return fillCache;
  }
  public boolean isModelView() {
    return modelView;
  }
  public boolean isUseModv() {
    return useModv;
  }
  public boolean isPreserveContrast() {
    return preserveContrast;
  }
  public String getModelNameUsed() {
    return modelNameUsed;
  }
  public String getModeUsed() {
    return modeUsed;
  }


  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",datasetName="
      + datasetName
      + ",openWithModel="
      + openWithModel
      + ",modelName="
      + modelName
      + ",modelNameUsed="
      + modelNameUsed
      + ",setToMode="
      + setToMode
      + ",mode="
      + mode
      + ",modeUsed="
      + modeUsed
      + ",swapYZ="
      + swapYZ
      + ",fillCache="      + fillCache
      + ",modelView="
      + modelView
      + ",useModv="
      + useModv
      + ",preserveContrast="
      + preserveContrast
      + ",process="
      + process;
  }

}
