package etomo.process;

//import java.lang.IllegalStateException;
import java.util.Vector;
import java.io.File;

import etomo.type.AxisID;

/**
 * <p>Description:
 * ImodState constructs a private ImodProcess instance.  It should be used
 * to perform any function on the ImodProcess instance that is required by the
 * ImodManager.
 * 
 * ImodState should mirror ImodProcess's state as much as
 * possible without changing the previously existing functionality.</p>
 * 
 * <p>Use:
 * 1. Construct an ImodState with all parameters required to create the
 *    correct ImodProcess instance.
 * 2. If necessary, run setup() to do the initial configuration of the
 *    ImodProcess instance.
 * 3. Settings that are don't change can be set using the set functions
 *    prior to open() and model().
 *    Examples:  
 *    fiducialModel.setUseModv(true) => setUseModv(true);
 *    patchVectorModel.modelMode() => setSetToMode(true);
 * 4. Settings that need to be passed as parameters can be passed to open() or
 *    model(), or to the set functions just before running open() or model().
 * 
 * <p>Copyright: Copyright(c) 2002, 2003</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado
 * 
 * Old Log:
 * <p> $Revision 1.3  2003/11/25 22:50:17  sueh
 * <p> $bug242 make ImodAssistant more testable
 * <p> $
 * <p> $Revision 1.2  2003/11/21 23:49:46  sueh
 * <p> $bug242 ImodAssistant - made it more configureable,
 * <p> $improved the interface
 * <p> $
 * <p> $Revision 1.1  2003/11/15 01:40:12  sueh
 * <p> $bug242 created class to run ImodProcess functions for
 * <p> $ImodManager
 * <p> $$ </p>

 * </p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.11  2004/05/07 19:53:49  sueh
 * <p> $bug# 33 correcting function name
 * <p> $
 * <p> $Revision 1.10  2004/05/06 20:22:12  sueh
 * <p> $bug# 33 added getRubberbandCoordinates()
 * <p> $
 * <p> $Revision 1.9  2004/05/03 22:22:53  sueh
 * <p> $bug# 416 added setBinning()
 * <p> $
 * <p> $Revision 1.8  2004/04/30 21:12:23  sueh
 * <p> $bug# 428 opening ZaP window on open() when not in model view mode
 * <p> $
 * <p> $Revision 1.7  2004/04/28 01:02:33  sueh
 * <p> $bug# 428 calling ImodProcess.viewModel() when reopening a
 * <p> $3dmod with a mode view window
 * <p> $
 * <p> $Revision 1.6  2004/04/27 23:18:07  sueh
 * <p> $bug# 320 adding boolean warnedStaleFile, to prevent ImodManager
 * <p> $from asking to close a 3dmod over and over.
 * <p> $
 * <p> $Revision 1.5  2004/02/07 03:05:56  sueh
 * <p> $bug# 169 Added setWorkingDirectory().
 * <p> $
 * <p> $Revision 1.4  2004/02/05 18:04:12  sueh
 * <p> $bug# 306 added setSwapYZ - used to set swapYZ before
 * <p> $opening 3dmod
 * <p> $
 * <p> $Revision 1.3  2004/02/04 18:11:03  sueh
 * <p> $bug# 171 return window id when running 3dmodv, so that
 * <p> $3dmodv can be closed automatically
 * <p> $
 * <p> $Revision 1.2  2003/12/04 22:07:35  sueh
 * <p> $bug242 fixing reset() - 3dmod remembers last modelName.
 * <p> $Must replace modelName to do a real reset.
 * <p> $
 * <p> $Revision 1.1  2003/12/02 23:20:29  sueh
 * <p> $bug242 Was ImodAssistant.  Same as ImodAssistant
 * <p> $without configuration functions.  State is changed by all
 * <p> $functions.  Added reset() to reset state
 * <p> $$ </p>
 */

public class ImodState {
  public static final String rcsid = "$$Id$$";

  protected ImodProcess process = null;
  
  public static final String MODEL = "model";
  public static final String MOVIE = "movie";

  private String datasetName = "";
  
  private boolean openWithModel = false;
  private String modelName = "";

  private boolean useMode = false;
  private String mode = MOVIE;
  
  private boolean swapYZ = false;
  private boolean fillCache = false;
  private boolean modelView = false;
  private boolean useModv = false;
  private boolean preserveContrast = false;
  private boolean warnedStaleFile = false;
  

  
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess().
   */
  public ImodState() {
    process = new ImodProcess();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset).
   */
  public ImodState(String datasetName) {
    this.datasetName = datasetName;
    process = new ImodProcess(datasetName);
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset, String model).
   */
  public ImodState(String datasetName, String modelName) {
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
  public ImodState(AxisID axisID, String datasetName, String datasetExt) {
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
  public ImodState(
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
  public void initialize(boolean swapYZ, boolean fillCache, boolean modelView) {
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

  
  public void reset() {
     openWithModel = true;
     modelName = "";
     useMode = false;
     mode = MOVIE; 
     useModv = false;
     preserveContrast = false;
  }


  public void setSwapYZ(boolean swapYZ) {
      this.swapYZ = swapYZ;
      process.setSwapYZ(swapYZ);
  }
  
  public void setWorkingDirectory(File workingDirectory) {
    process.setWorkingDirectory(workingDirectory);
  }


  //Set functions
  
  /**
   * Set opening a process and setting the mode to model or movie.
   */
  public void setUseMode(boolean useMode){
    this.useMode = useMode;
  }
  
  public void setMode(String mode) {
    this.mode = new String (mode);
  }

  
  /**
   * Sets opening a process using the -view option
   *
   */
  public void setUseModv(boolean useModv) {
    this.useModv = useModv;
  }
  
  public void setPreserveContrast(boolean preserveContrast) {
    this.preserveContrast = preserveContrast;
  }
  
  /**
   * Opens a process, opens a model.
   * 
   * @throws SystemProcessException
   */
  public void open() throws SystemProcessException, NullPointerException{
    //process is not running
    if (!process.isRunning()) {
      //set configuration
      if (openWithModel) {
        if (modelName == null) {
          throw new NullPointerException("modelName is empty in " + toString()); 
        }
        process.setModelName(modelName);
      }
      if (useModv) {
        process.setUseModv(useModv);
      }
      //open
      process.open();
      warnedStaleFile = false;
      //open model
      if (!openWithModel && modelName != null && modelName.matches("\\S+")) {
        if (preserveContrast) {
          process.openModelPreserveContrast(modelName);
        }
        else {
          process.openModel(modelName);
        }
      }
    }
    else {
      //process is running
      //raise 3dmod
      if (!modelView && !useModv) {
        process.openZapWindow();
      }
      else {
        process.raise3dmod();
      }
      //reopen model
      if (!useModv && modelName != null && modelName.matches("\\S+")) {
        if (preserveContrast) {
          process.openModelPreserveContrast(modelName);
        }
        else {
          process.openModel(modelName);
        }
      }
    }
    //set mode
    if (useMode) {
      if (mode.equals(MODEL)) {
        process.modelMode();
      }
      else {
        process.movieMode();
      }
    }
  }
  /**
   * Opens a process using the modelName parameter.  Ignores mode setting.
   * 
   * Configuration functions you can use with this function:
   * configureUseModv()
   * 
   * @param modelName
   * @throws SystemProcessException
   */
  public void open(String modelName) throws SystemProcessException {
    this.modelName = modelName;
    open();
  } 
  
  public void open(String modelName, boolean modelMode) throws SystemProcessException {
    this.modelName = modelName;
    useMode = true;
    setMode(modelMode);
    open();
  } 

  /**
   * Opens the Bead Fixer window.
   * 
   * @throws SystemProcessException
   */
  public void openBeadFixer() throws SystemProcessException {
    process.openBeadFixer();
  }
  
  public Vector getRubberbandCoordinates() throws SystemProcessException {
    return process.getRubberbandCoordinates();
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


  /**
   * Sets the mode (model or movie)
   * 
   * @param modelMode
   */
  private void setMode(boolean modelMode) {
    if (modelMode) {
      mode = MODEL;
    }
    else {
      mode = MOVIE;
    }
  }

  
  public String getDatasetName() {
    return datasetName;
  }
  public boolean isOpenWithModel() {
    return openWithModel;
  }
  public void setOpenWithModel(boolean openWithModel) {
    this.openWithModel = openWithModel;
  }
  public String getModelName() {
    return modelName;
  }
  public boolean isUseMode() {
    return useMode;
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
  public boolean isWarnedStaleFile() {
    return warnedStaleFile;
  }
  public void setWarnedStaleFile(boolean warnedStaleFile) {
    this.warnedStaleFile = warnedStaleFile;
  }
  public void setBinning(int binning) {
    process.setBinning(binning);
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
      + ",useMode="
      + useMode
      + ",mode="
      + mode
      + ",swapYZ="
      + swapYZ
      + ",fillCache="
      + fillCache
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
