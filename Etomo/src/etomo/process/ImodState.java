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
 * ImodState preserves the original state configuration during multiple opens,
 * raises and re-opens of 3dmod, all of which are handle by the open() function.
 * It also allows a new state configuration to temporarily override the original
 * state configuration for a single call to open().  Conflicting new
 * configurations can be created when calling open() several times, without any
 * negative effect.  To allow state information to be totally controlled by the 
 * user, don't change it in reset().
 * </p>
 * 
 * <p>Use:
 * Construction and initialization:
 * 1. Construct an ImodState.
 * 2. Use the setInitial functions to set the initial state.  ImodState resets
 *    to the initial state after each open call.
 * Opening and modeling:
 * 1. Use the set functions to set the current state.
 * 2. Call an open function.
 * </p>
 * 
 * <p>Upgrading:
 * 
 * Adding a state variable that is changed only during the construction and
 * initialization phase:
 * 1. For a variable called "foo", create ImodState members and methods:
 *    - A foo variable, set to a default value
 *    - A get or is function for foo.
 *    - A set function for foo or add it to the constructor(s).
 *    If setting foo with a set function:
 *    - An fooSet boolean variable, set to false
 *    - The setFoo function should:
 *        Do nothing if fooSet is true.
 *        Set fooSet to true.
 * 2. If the state can change ImodProcess.open(), create a variable for it in
 *    ImodProcess.  Pass the state variable's value to ImodProcess either when
 *    set or in open().
 * 3. Decide whether to create a "using" variable.  See Rules for upgrading 5-6.
 * 
 * Adding a state variable that is set during the construction and
 * initialization phase and also changed later:
 * 1. For a variable called "foo", create ImodState members and methods:
 *    - A foo variable
 *    - A get or is function for foo.
 *    - A set function for foo.
 *    - An initialFoo variable, set to a default value
 *    - An initialFooSet boolean variable, set to false
 *    - A get or is function for initialFoo
 *    - A set function for initialFoo:
 *        Do nothing if initialFooSet is true.
 *        Set foo = initialFoo.
 *        Set initialFooSet to true.
 * 2. If the state can change ImodProcess.open(), create a variable for it in
 *    ImodProcess.  Pass the state variable's value to ImodProcess either when
 *    set or in open().
 * 3. Change ImodState.reset():
 *    Set foo = initialFoo.
 * 4. Decide whether to create a "using" variable.  See Rules for upgrading 5-6.
 * 
 * Adding a state variable that is only changed during opening and modeling:
 * 1. For a variable called "foo", create ImodState members and methods:
 *    - A foo variable
 *    - A get or is function for foo.
 *    - A set function for foo.
 *    - A static final defaultFoo variable, set to a default value
 *    - A get or is function for defaultFoo
 * 2. If the state can change ImodProcess.open(), create a variable for it in
 *    ImodProcess.  Pass the state variable's value to ImodProcess either when
 *    set or in open().
 * 3. Change ImodState.reset():
 *    Set foo = defaultFoo.
 * 4. Decide whether to create a "using" variable.  See Rules for upgrading 5-6.
 * 
 * Adding a state variable that holds the last value the user gave:
 * 1. If the state can only be passed only on 3dmod command line then ImodState 
 *     doesn't need to remember it.
 * 2. If the state can change ImodProcess.open(), create a variable for it in
 *    ImodProcess.  Pass the state variable's value to ImodProcess either when
 *    set or in open().
 * 3. If ImodState has to remember it, don't change it in reset().
 * </p>
 * 
 * <p>Rules for upgrading:
 * 1. Unchanging variables are set only once.  They should not be changed in
 *    reset().
 * 2. Variables with an initial value set during construction and initialization
 *    should be set back to their initial value in reset().
 * 3. Variables that should always hold the last value set by the user don't
 *    need to be in ImodState.  If they are, reset() shouldn't change them.
 * 4. Variables that are only changed during opening and modelling should be set
 *    to their default value in reset().
 * 5. Default variables and the default value of Initial variables should match
 *    3dmod's default state.
 * 6. If 3dmod can be changed back to the default version of a state variable
 *    with a message, consider creating a "using" variable.  When a using
 *    variable is on, the message based on the corresponding state variable is
 *    always sent.  "Using" variables should be turned on whenever the
 *    corresponding state variable is changed.  "Using" variables should never
 *    be turned off.  See usingMode.
 * </p>
 * 
 * <p>Current types of state information as of 6/9/04:
 * Unchanging state information:
 * - datasetName
 * - modelView
 * - useModv
 * States information that changes during opening and modeling:
 *   Settings that are set during construction and initialization:
 *   - modelName
 *   - mode
 *   - swapYZ
 *   State information that is not set during construction and initialization:
 *   - preserveContrast
 *   - openContours
 *   - binning
 * State information that is controlled by the user
 * - whether 3dmod running
 * 
 * ImodState's relationship with ImodProcess:
 * State variable will also exist in ImodProcess because they cause the
 * 3dmod command line to change:
 * - If preserveContrast is true, ImodProcess.openWithModel should be false.
 *     This is because there is no preserve contrast option to pass to 3dmod
 *     when it is opened.
 * - modelName corresponds to ImodProcess.modelName.
 * - swapYZ corresponds to ImodProcess.swapYZ.
 * - modelView corresponds to ImodProcess.modelView.
 * - useModv corresponds to ImodProcess.useModv.
 * ImodState has set functions for this but does not keep a state variable
 * because the user controls its setting:
 * - ImodProcess.workingDirectory
 * ImodState has set function for this but does not keep a state variable
 * because it can only go on the command line:
 * - ImodProcess.binning
 * 
 * </p>
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
 * <p> $Revision 1.22  2004/12/14 01:36:21  sueh
 * <p> $bug# 373 Putting a list of dataset names in a string array.
 * <p> $
 * <p> $Revision 1.21  2004/11/24 18:10:45  sueh
 * <p> $bug# 520 Added binning in XY.
 * <p> $
 * <p> $Revision 1.20  2004/11/19 23:21:49  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
 * <p> $Revision 1.19.2.2  2004/09/22 22:07:52  sueh
 * <p> $bug# 520 Added getSlicerAngles().
 * <p> $
 * <p> $Revision 1.19.2.1  2004/09/21 17:56:28  sueh
 * <p> $bug# 520 Added ImodState(File), which sets the absolute path of the file
 * <p> $to ImodProcess.datasetName.
 * <p> $
 * <p> $Revision 1.19  2004/08/31 01:09:03  sueh
 * <p> $bug# 541 reset():  resetting each time 3dmod runs
 * <p> $
 * <p> $Revision 1.18  2004/06/22 23:39:39  sueh
 * <p> $bug# 455 Removing usingOpenContours because it automatcally
 * <p> $gets reset to default when a new model is opened.
 * <p> $
 * <p> $Revision 1.17  2004/06/22 22:57:45  sueh
 * <p> $bug# 455 Added openContours.  Changed useMode to usingMode.
 * <p> $Clarified "using" functionality.  Updated documentation.
 * <p> $
 * <p> $Revision 1.16  2004/06/17 01:31:04  sueh
 * <p> $bug# 471 fixing how model name is set in ImodProcess
 * <p> $
 * <p> $Revision 1.15  2004/06/10 18:28:59  sueh
 * <p> $bug# 463 openBeadFixer - a current state variable with no initial
 * <p> $value.  Add message to open bead fixer in open().
 * <p> $
 * <p> $Revision 1.14  2004/06/10 17:27:17  sueh
 * <p> $bug# 462 update comments, clarify how each state variable
 * <p> $is being used, add equals() functions for testing, update sets
 * <p> $and gets to distinguish between initial state and current state,
 * <p> $add some unchanging state variables to the constructor.
 * <p> $
 * <p> $Revision 1.13  2004/06/07 18:33:29  sueh
 * <p> $bug# 457 In open(), collect all messages to be sent to 3dmod in process.
 * <p> $Send all messages to 3dmod before exiting open().
 * <p> $
 * <p> $Revision 1.12  2004/06/07 00:19:21  sueh
 * <p> $bug# 452 Remove the model() functions.  No longer take
 * <p> $responsibility for knowing when to set openWithModel.
 * <p> $Use the open() functions to do opening and modeling.
 * <p> $
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

  //public constants
  //mode
  public static final int MODEL_MODE = -1;
  public static final int MOVIE_MODE = -2;
  //modelView
  public static final int MODEL_VIEW = -3;
  //useModv
  public static final int MODV = -4;
  
  //unchanging state information
  private String datasetName = "";
  private boolean modelView = false;
  private boolean useModv = false;
  
  //current state information
  //reset to initial state
  private String modelName;
  private int mode;
  private boolean swapYZ;
  //reset to default state
  private boolean preserveContrast;
  private boolean openBeadFixer;
  private boolean openContours;
  
  //signals that a state variable has been changed at least once, so the
  //corrosponding message must always be sent
  private boolean usingMode = false;
  
  //reset values
  //initial state information
  private String initialModelName = "";
  private int initialMode = MOVIE_MODE;
  private boolean initialSwapYZ = false;
  //default state information
  private static final boolean defaultOpenWithModel = false;
  private static final boolean defaultPreserveContrast = false;
  private static final boolean defaultOpenBeadFixer = false;
  private static final boolean defaultOpenContours = false;
  private static final boolean defaultFrames = false;
  private static final int defaultBinning = 1;
    
  //internal state information
  private ImodProcess process = null;
  private boolean warnedStaleFile = false;
  //initial state information
  boolean initialModeSet = false;
  boolean initialSwapYZSet = false;

  //constructors
  //they can set final state variables
  //they can also set initialModelName
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess().
   */
  public ImodState() {
    process = new ImodProcess();
    reset();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess() and set either model view or imodv.
   */
  public ImodState(int modelViewType) {
    process = new ImodProcess();
    setModelViewType(modelViewType);
    reset();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset).
   */
  public ImodState(String datasetName) {
    this.datasetName = datasetName;
    process = new ImodProcess(datasetName);
    reset();
  }
  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset) and set either model view or imodv.
   */
  public ImodState(String datasetName, int modelViewType) {
    this.datasetName = datasetName;
    process = new ImodProcess(datasetName);
    setModelViewType(modelViewType);
    reset();
  }

  
  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset, String model).
   */
  public ImodState(String datasetName, String modelName) {
    this.datasetName = datasetName;
    initialModelName = modelName;
    process = new ImodProcess(datasetName, modelName);
    reset();
  }
  
  public ImodState(File file) {
    process = new ImodProcess(file.getAbsolutePath());
    reset();
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
    reset();
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
    String[] datasetNameArray = { datasetName1 + axisExtension + datasetExt,
        datasetName2 + axisExtension + datasetExt,
        datasetName3 + axisExtension + datasetExt };
    datasetName = datasetNameArray[0] + " " + datasetNameArray[1] + " "
        + datasetNameArray[2];
    initialModelName = modelName + axisExtension + modelExt;
    process = new ImodProcess(datasetNameArray, modelName);
    reset();
  }

  /**
   * Opens a process, opens a model.
   * 
   * @throws SystemProcessException
   */
  public void open() throws SystemProcessException, NullPointerException{
    //process is not running
    if (!process.isRunning()) {
      //open
      process.open();
      warnedStaleFile = false;
      //open bead fixer
      if (openBeadFixer) {
        process.setOpenBeadFixerMessage();
      }
      //model will be opened
      if (modelName != null && modelName.matches("\\S+") && preserveContrast) {
        process.setOpenModelPreserveContrastMessage(modelName);
      }
      //This message can only be sent after opening the model
      if (openContours) {
        process.setNewContoursMessage(true);
      }
    }
    else {
      //process is running
      //raise 3dmod
      if (!modelView && !useModv) {
        process.setOpenZapWindowMessage();
      }
      else {
        process.setRaise3dmodMessage();
      }
      //open bead fixer
      if (openBeadFixer) {
        process.setOpenBeadFixerMessage();
      }
      //reopen model
      if (!useModv && modelName != null && modelName.matches("\\S+")) {
        if (preserveContrast) {
          process.setOpenModelPreserveContrastMessage(modelName);
        }
        else {
          process.setOpenModelMessage(modelName);
        }
      }
      //This message can only be sent after opening the model
      if (openContours) {
        process.setNewContoursMessage(true);
      }
    }
    //set mode
    if (usingMode) {
      if (mode == MODEL_MODE) {
        process.setModelModeMessage();
      }
      else {
        process.setMovieModeMessage();
      }
    }
    process.sendMessages();
    reset();
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
    setModelName(modelName);
    open();
  } 
  
  public void open(String modelName, boolean modelMode) throws SystemProcessException {
    setModelName(modelName);
    setModelMode(modelMode);
    open();
  } 

  public Vector getRubberbandCoordinates() throws SystemProcessException {
    return process.getRubberbandCoordinates();
  }
  
  public Vector getSlicerAngles() throws SystemProcessException {
    return process.getSlicerAngles();
  }

  
  /**
   * Tells process to quit.
   * @throws SystemProcessException
   */
  public void quit() throws SystemProcessException {
    process.quit();
  }


  /**
   * @param modelViewType either MODEL_VIEW or MODV
   */
  protected void setModelViewType(int modelViewType) {
    if (modelViewType == MODEL_VIEW) {
      modelView = true;
    }
    else if (modelViewType == MODV) {
      useModv = true;
    }
    else {
      modelView = false;
      useModv = false;
    }
    process.setModelView(modelView);
    process.setUseModv(useModv);
  }
  
  protected void reset() {
    //reset to initial state
    setModelName(initialModelName);
    mode = initialMode;
    swapYZ = initialSwapYZ;
    //reset to default state
    preserveContrast = defaultPreserveContrast;
    process.setOpenWithModel(!preserveContrast);
    openBeadFixer = defaultOpenBeadFixer;
    openContours = defaultOpenContours;
    process.setBinning(defaultBinning);
    process.setFrames(defaultFrames);
  }

  protected String getModeString(int mode) {
    if (mode == MOVIE_MODE) {
      return "MOVIE_MODE";
    }
    else if (mode == MODEL_MODE) {
      return "MODEL_MODE";
    }
    else {
      return "ERROR:" + Integer.toString(mode);
    }
  }
 
 
  //unchanging state information
  /**
   * @return datasetName
   */
  public String getDatasetName() {
    return datasetName;
  }
  
  /**
   * 
   * @return modelView
   */
  public boolean isModelView() {
    return modelView;
  }
  
  /**
   * @return useModv
   */
  public boolean isUseModv() {
    return useModv;
  }
  
  //current state information
  /**
   * @return modelName
   */
  public String getModelName() {
    return modelName;
  }
  public void setModelName(String modelName) {
    this.modelName = modelName;
    process.setModelName(modelName);
  }
  
  /**
   * @return usingMode
   */
  public boolean isUsingMode() {
    return usingMode;
  }
  /**
   * @param usingMode
   */
  public void setUsingMode(boolean usingMode){
    this.usingMode = usingMode;
  }
  
  /**
   * @return openContours
   */
  public boolean isOpenContours() {
    return openContours;
  }
  /**
   * @param usingOpenContour
   */
  public void setOpenContours(boolean openContours){
    this.openContours = openContours;
  }

  /**
   * @return
   */
  public int getMode() {
    return mode;
  }
  /**
   * @return mode
   */
  public String getModeString() {
    return getModeString(mode);
  }
  /**
   * set the mode to model or movie.
   */
  public void setMode(int mode) {
    usingMode = true;
    this.mode = mode;
  }
  /**
   * Sets the mode (model or movie)
   * @param modelMode
   */
  private void setModelMode(boolean modelMode) {
    usingMode = true;
    if (modelMode) {
      mode = MODEL_MODE;
    }
    else {
      mode = MOVIE_MODE;
    }
  }

  /**
   * @return swapZY
   */
  public boolean isSwapYZ() {
    return swapYZ;
  }
  /**
   * @param swapYZ
   */
  public void setSwapYZ(boolean swapYZ) {
    this.swapYZ = swapYZ;
    process.setSwapYZ(swapYZ);
  }

  /**
   * @return preserveContrast
   */
  public boolean isPreserveContrast() {
    return preserveContrast;
  }
  /**
   * @param preserveContrast
   */
  public void setPreserveContrast(boolean preserveContrast) {
    this.preserveContrast = preserveContrast;
    process.setOpenWithModel(!preserveContrast);
  }

  public void setFrames(boolean frames) {
    process.setFrames(frames);
  }
  
  /**
   * @return openBeadFixer
   */
  public boolean isOpenBeadFixer() {
    return openBeadFixer;
  }
  /**
   * @param openBeadFixer
   */
  public void setOpenBeadFixer(boolean openBeadFixer) {
    this.openBeadFixer = openBeadFixer;
  }


  //initial state information
  /**
   * @return initialModelName
   */
  public String getInitialModelName() {
    return initialModelName;
  }
  
  /**
   * @return
   */
  public int getInitialMode() {
    return initialMode;
  }
  /**
   * @return
   */
  public String getInitialModeString() {
    return getModeString(initialMode);
  }
  /**
   * @param initialMode
   */
  public void setInitialMode(int initialMode) {
    if (initialModeSet) {
      return;
    }
    this.initialMode = initialMode;
    setMode(initialMode);
    initialModeSet = true;
  }
  
  public boolean isInitialSwapYZ() {
    return initialSwapYZ;
  }
  public void setInitialSwapYZ(boolean initialSwapYZ) {
    if (initialSwapYZSet) {
      return;
    }
    this.initialSwapYZ = initialSwapYZ;
    setSwapYZ(initialSwapYZ);
    initialSwapYZSet = true;
  }
  
  //default state information
  
  public boolean isDefaultOpenWithModel() {
    return defaultOpenWithModel;
  }
  
  public boolean isDefaultPreserveContrast() {
    return defaultPreserveContrast;
  }
  
  //user controlled state information - pass through to ImodProcess
  /**
   * @return true if process is running
   */
  public boolean isOpen() {
    return process.isRunning();
  }
  
  /**
   * @param binning
   */
  public void setBinning(int binning) {
    process.setBinning(binning);
  }
  
  public void setBinningXY(int binning) {
    process.setBinningXY(binning);
  }
  
  /**
   * @param workingDirectory
   */
  public void setWorkingDirectory(File workingDirectory) {
    process.setWorkingDirectory(workingDirectory);
  }

  //internal state sets and gets
  /**
   * @returns warnedStaleFile
   */
  public boolean isWarnedStaleFile() {
    return warnedStaleFile;
  }
  /**
   * 
   * @param warnedStaleFile
   */
  public void setWarnedStaleFile(boolean warnedStaleFile) {
    this.warnedStaleFile = warnedStaleFile;
  }
  

  /**
   * @return string
   */
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  /**
   * @return string
   */
  protected String paramString() {
    Vector params = new Vector(17);
    params.add("datasetName=" + getDatasetName());
    params.add("modelView=" + isModelView());
    params.add("useModv=" + isUseModv());
    
    params.add("modelName=" + getModelName());
    params.add("usingMode=" + isUsingMode());
    params.add("mode=" + getModeString());
    
    params.add("swapYZ=" + isSwapYZ());
    params.add("preserveContrast=" + isPreserveContrast());
    params.add("openBeadFixer=" + isOpenBeadFixer());
    
    params.add("initialModelName=" + getInitialModelName());
    params.add("initialMode=" + getInitialModeString());
    params.add("initialSwapYZ=" + isInitialSwapYZ());
    
    params.add("defaultOpenWithModel=" + isDefaultOpenWithModel());
    params.add("defaultPreserveContrast=" + isDefaultPreserveContrast());
    params.add("process=" + process.toString());
    
    params.add("warnedStaleFile=" + isWarnedStaleFile());
    params.add("openContours=" + isOpenContours());
    
    return params.toString();
  }
  
  /**
   * 
   * @param imodState
   * @return true if unchanging and initial state information is the same
   */
  public boolean equalsInitialConfiguration(ImodState imodState) {
    if (datasetName.equals(imodState.getDatasetName())
        && modelView == imodState.isModelView()
        && useModv == imodState.isUseModv()
        && initialModelName.equals(imodState.getInitialModelName())
        && initialMode == imodState.getInitialMode()
        && initialSwapYZ == imodState.isInitialSwapYZ()) {
      return true;
    }
    return false;
  }
  
  /**
   * 
   * @param imodState
   * @return true if unchanging and current state information is the same
   */
  public boolean equalsCurrentConfiguration(ImodState imodState) {
    if (datasetName.equals(imodState.getDatasetName())
        && modelView == imodState.isModelView()
        && useModv == imodState.isUseModv()
        && modelName.equals(imodState.getModelName())
        && usingMode == imodState.isUsingMode()
        && openContours == imodState.isOpenContours()
        && mode == imodState.getMode()
        && swapYZ == imodState.isSwapYZ()
        && preserveContrast == imodState.isPreserveContrast()
        && openBeadFixer == imodState.isOpenBeadFixer()) {
      return true;
    }
    return false;
  }
  
  /**
   * 
   * @param imodState
   * @return true if unchanging, initial, and current state information is the
   * same
   */
  public boolean equals(ImodState imodState) {
    return equalsInitialConfiguration(imodState)
      && equalsCurrentConfiguration(imodState);
  }

}
