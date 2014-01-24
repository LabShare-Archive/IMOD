package etomo.process;

import java.util.List;
import java.util.Vector;
import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.type.FileType;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> $Revision 1.61  2011/04/04 16:51:26  sueh
 * <p> $bug# 1416 Added fileList and a constructor.
 * <p> $
 * <p> $Revision 1.60  2011/02/21 16:59:41  sueh
 * <p> $bug# 1437 Reformatting.
 * <p> $
 * <p> $Revision 1.59  2010/09/21 16:27:11  sueh
 * <p> $bug# 1395 In open, when the process is already running and bead fixer is
 * <p> $in use, always set skiplist.  Null skiplist is being handled by ImodProcess.
 * <p> $
 * <p> $Revision 1.58  2010/09/21 04:04:59  sueh
 * <p> $bug# 1395 In open set skipList when process is already running.
 * <p> $
 * <p> $Revision 1.57  2010/04/28 16:19:45  sueh
 * <p> $bug# 1344 Added getDatasetName.
 * <p> $
 * <p> $Revision 1.56  2010/03/11 06:01:46  sueh
 * <p> $bug# 1311 Added setOpenModelView.
 * <p> $
 * <p> $Revision 1.55  2010/02/17 04:49:20  sueh
 * <p> $bug# 1301 Using the manager instead of the manager key do pop up
 * <p> $messages.
 * <p> $
 * <p> $Revision 1.54  2009/09/01 03:17:56  sueh
 * <p> $bug# 1222
 * <p> $
 * <p> $Revision 1.53  2009/06/05 01:55:27  sueh
 * <p> $bug# 1219 Added startNewContoursAtNewZ and
 * <p> $setStartNewContoursAtNewZ.
 * <p> $
 * <p> $Revision 1.52  2009/03/23 17:08:21  sueh
 * <p> $bug# 1187 Added setContinuousTarget.
 * <p> $
 * <p> $Revision 1.51  2008/12/15 23:59:12  sueh
 * <p> $bug# 1160 Fixed the problem where 3dmod has the wrong diameter if
 * <p> $the button is pressed while it is running.
 * <p> $
 * <p> $Revision 1.50  2008/12/09 21:30:13  sueh
 * <p> $bug# 1160 Removing the contractor that has the beadfixerDiameter
 * <p> $parameter.  In open, setting beadfixerDiameter from ApplicationManager
 * <p> $if the manager is an instance of ApplicationManager and setAutoCenter is
 * <p> $true.
 * <p> $
 * <p> $Revision 1.49  2008/12/05 00:52:31  sueh
 * <p> $bug# 1156 Added skipList.
 * <p> $
 * <p> $Revision 1.48  2008/07/24 17:58:59  sueh
 * <p> $bug# 1128 Added pointLimit and setPointLimit.
 * <p> $
 * <p> $Revision 1.47  2008/06/19 23:35:05  sueh
 * <p> $bug# 1112 Added setTiltFile and resetTiltFile.
 * <p> $
 * <p> $Revision 1.46  2008/05/03 00:41:02  sueh
 * <p> $bug# 847 In Run3dmodMenuOptions renamed setOptions to
 * <p> $orGlobalOptions, which is a better description of its functionality.
 * <p> $
 * <p> $Revision 1.45  2008/05/01 22:56:03  sueh
 * <p> $bug# 1107 Added setOpenZap (-Z) and addWindowOpenOption (-E).
 * <p> $
 * <p> $Revision 1.44  2007/12/26 22:14:26  sueh
 * <p> $bug# 1052 In open(Run3dmodMenuOptions) make it possible to pass a null in
 * <p> $instead of having to create an empty Run3dmodMenuOptions instance.
 * <p> $
 * <p> $Revision 1.43  2007/11/06 19:23:03  sueh
 * <p> $bug# 1047 Added subdirName.
 * <p> $
 * <p> $Revision 1.42  2007/05/11 19:29:12  sueh
 * <p> $bug# 964 Save fileNameArray.  Added equalsFileNameArray(String[]).
 * <p> $
 * <p> $Revision 1.41  2007/05/11 15:43:57  sueh
 * <p> $bug# 964 Added ImodState(BaseManager, String[], AxisID) to open 3dmod
 * <p> $with multiple files and no model.
 * <p> $
 * <p> $Revision 1.40  2007/02/05 22:55:40  sueh
 * <p> $bug# 962 Added modeleled join and transformed model.
 * <p> $
 * <p> $Revision 1.39  2006/09/19 22:22:23  sueh
 * <p> $bug# 928 Open the imodv objects window when opening a patch vector model.
 * <p> $
 * <p> $Revision 1.38  2006/08/11 23:49:35  sueh
 * <p> $bug# 816 Added reopenLog().
 * <p> $
 * <p> $Revision 1.37  2006/08/11 21:46:59  sueh
 * <p> $bug# 816 Added setOpenLog() and setOpenLogOff()
 * <p> $
 * <p> $Revision 1.36  2006/07/17 21:17:52  sueh
 * <p> $bug# 900 Added imodSendEvent functionality back.  Uses the
 * <p> $SystemProcessException.
 * <p> $
 * <p> $Revision 1.35  2006/07/04 20:40:06  sueh
 * <p> $bug# 894 Changed seedMode to newContours.  Added setBeadfixerMode().
 * <p> $
 * <p> $Revision 1.34  2006/07/03 21:55:35  sueh
 * <p> $bug# 895 Added processRequest().
 * <p> $
 * <p> $Revision 1.33  2006/06/22 21:02:19  sueh
 * <p> $bug# 797 Catching io exception when sending messages to 3dmods.
 * <p> $
 * <p> $Revision 1.32  2006/06/07 22:24:34  sueh
 * <p> $bug# 862 Added setAutoCenter member variable.  Auto center should not be
 * <p> $modified unless setAutoCenter() is called.
 * <p> $
 * <p> $Revision 1.31  2006/04/11 13:47:58  sueh
 * <p> $bug# 809 Manage auto center and seed mode separately from
 * <p> $openBeadFixer so that seed mode doesn't always have to be managed.
 * <p> $Reset auto center and seed mode when setOpenBeadFixer is called.
 * <p> $
 * <p> $Revision 1.30  2006/03/30 21:24:09  sueh
 * <p> $bug# 809 Passing auto center and seed mode settings to ImodProcess.
 * <p> $
 * <p> $Revision 1.29  2005/10/18 22:10:46  sueh
 * <p> $bug# 727 Can't reproduce this bug so added some prints to the error log
 * <p> $to document it, if it appears again.
 * <p> $
 * <p> $Revision 1.28  2005/08/11 23:39:38  sueh
 * <p> $bug# 711  In ImodManager:  to be sure that the 3dmod -view
 * <p> $configuration won't use -O, -B, or -b; prevent Run3dmodMenuOptions
 * <p> $from being turned on by setting noOptions.  Pass
 * <p> $Run3dmodMenuOptions to ImodManager.open(), ImodState.open(), and
 * <p> $ImodProcess.open().  It should not be saved, because it needs to be
 * <p> $refreshed each time 3dmod is run.  In ImodState.open() add the menu
 * <p> $options from the pulldown menu to the existing menu options.
 * <p> $
 * <p> $Revision 1.27  2005/08/09 20:07:07  sueh
 * <p> $bug# 711  Added setRun3dmodMenuOption().
 * <p> $
 * <p> $Revision 1.26  2005/07/29 00:51:53  sueh
 * <p> $bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> $because the current manager changes when the user changes the tab.
 * <p> $Passing the manager where its needed.
 * <p> $
 * <p> $Revision 1.25  2005/04/25 20:47:08  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 1.24  2005/03/04 00:14:51  sueh
 * <p> $bug# 533 Added setPieceListFileName() to set the -p command line
 * <p> $option in the 3dmod call.
 * <p> $
 * <p> $Revision 1.23  2005/03/02 23:14:54  sueh
 * <p> $bug# 533 Adding -fr (frames) to ignore montaging information and
 * <p> $display the stack frame by frame.
 * <p> $
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

public final class ImodState {
  public static final String rcsid = "$$Id$$";

  // public constants
  // mode
  public static final int MODEL_MODE = -1;
  public static final int MOVIE_MODE = -2;
  // modelView
  public static final int MODEL_VIEW = -3;
  // useModv
  public static final int MODV = -4;

  // unchanging state information
  private boolean modelView = false;
  private boolean useModv = false;
  private AxisID axisID;

  // current state information
  // reset to initial state
  private String modelName = null;
  private int mode;
  private boolean swapYZ;
  // reset to default state
  private boolean preserveContrast;
  private boolean openBeadFixer;
  private boolean openContours;
  private boolean startNewContoursAtNewZ = false;
  private int pointLimit = -1;

  // sent with open bead fixer
  private boolean setAutoCenter = false;
  private boolean autoCenter = false;
  private boolean newContours = false;
  private boolean manageNewContours = false;
  private ImodProcess.BeadFixerMode beadfixerMode = null;
  private String skipList = null;

  // signals that a state variable has been changed at least once, so the
  // corrosponding message must always be sent
  private boolean usingMode = false;

  // don't reset
  private boolean allowMenuBinningInZ = false;
  private boolean noMenuOptions = false;

  // reset values
  // initial state information
  private String initialModelName = "";
  private int initialMode = MOVIE_MODE;
  private boolean initialSwapYZ = false;
  // default state information
  private static final boolean defaultOpenWithModel = false;
  private static final boolean defaultPreserveContrast = false;
  private static final boolean defaultOpenBeadFixer = false;
  private static final boolean defaultOpenContours = false;
  private static final boolean defaultFrames = false;
  private static final int defaultBinning = 1;

  // internal state information
  private final ImodProcess process;
  private String[] fileNameArray = null;
  private File[] fileList = null;
  private boolean warnedStaleFile = false;
  // initial state information
  boolean initialModeSet = false;
  boolean initialSwapYZSet = false;
  private final BaseManager manager;

  private String logName = null;
  private boolean debug = false;
  // Should be turned off after each use. This is because it is rarely used and
  // should not be on for most situations. This way I don't have to keep track
  // of when it is on.
  private EtomoBoolean2 deleteAllSections = null;
  private String fileName = null;
  private EtomoBoolean2 interpolation = null;
  private List<String> modelNameList = null;

  private boolean openSurfContPoint = false;

  // constructors
  // they can set final state variables
  // they can also set initialModelName

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess().
   */
  ImodState(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, axisID);
    reset();
  }

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess() and set either model view or imodv.
   */
  ImodState(BaseManager manager, int modelViewType, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, axisID);
    setModelViewType(modelViewType);
    reset();
  }

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset).
   */
  ImodState(BaseManager manager, String datasetName, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, datasetName, axisID);
    reset();
  }

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset) and set either model view or imodv.
   */
  ImodState(BaseManager manager, String datasetName, int modelViewType, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, datasetName, axisID);
    setModelViewType(modelViewType);
    reset();
  }

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset) and set either model view or imodv, and open a
   * 3dmod window.
   */
  ImodState(BaseManager manager, String datasetName, int modelViewType, AxisID axisID,
      ImodProcess.WindowOpenOption option) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, datasetName, axisID);
    setModelViewType(modelViewType);
    process.addWindowOpenOption(option);
    reset();
  }

  /**
   * Use this constructor to create an instance of ImodProcess using
   * ImodProcess(String dataset, String model).
   */
  ImodState(BaseManager manager, String datasetName, String modelName, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    initialModelName = modelName;
    process = new ImodProcess(manager, datasetName, modelName);
    reset();
  }

  ImodState(BaseManager manager, File file, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, file.getAbsolutePath(), axisID);
    reset();
  }

  ImodState(BaseManager manager, String[] fileNameArray, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.fileNameArray = fileNameArray;
    process = new ImodProcess(manager, fileNameArray);
    reset();
  }

  ImodState(BaseManager manager, String[] fileNameArray, AxisID axisID, String subdirName) {
    this.manager = manager;
    this.axisID = axisID;
    this.fileNameArray = fileNameArray;
    process = new ImodProcess(manager, fileNameArray);
    process.setSubdirName(subdirName);
    reset();
  }

  ImodState(final BaseManager manager, final File[] fileList, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.fileList = fileList;
    process = new ImodProcess(manager, fileList);
    reset();
  }

  /**
   * Build ImodState with a complete file name.
   * @param manager
   * @param axisID
   * @param fileName
   */
  ImodState(BaseManager manager, AxisID axisID, String fileName) {
    this.manager = manager;
    this.axisID = axisID;
    process = new ImodProcess(manager, fileName, axisID);
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
  ImodState(final BaseManager manager, final AxisID axisID, String datasetName,
      final String datasetExt) {
    this.manager = manager;
    this.axisID = axisID;
    String axisExtension = axisID.getExtension();
    if (axisExtension == "ERROR") {
      throw new IllegalArgumentException(axisID.toString());
    }
    datasetName = datasetName + axisExtension + datasetExt;
    process = new ImodProcess(manager, datasetName, axisID);
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
  ImodState(final BaseManager manager, final AxisID tempAxisID,
      final String datasetName1, final String datasetName2, final String datasetName3,
      final String datasetExt, final String modelName, final String modelExt) {
    this.manager = manager;
    axisID = tempAxisID;
    String axisExtension = tempAxisID.getExtension();
    if (axisExtension == "ERROR") {
      throw new IllegalArgumentException(tempAxisID.toString());
    }
    String[] datasetNameArray = { datasetName1 + axisExtension + datasetExt,
        datasetName2 + axisExtension + datasetExt,
        datasetName3 + axisExtension + datasetExt };
    String datasetName = datasetNameArray[0] + " " + datasetNameArray[1] + " "
        + datasetNameArray[2];
    initialModelName = modelName + axisExtension + modelExt;
    process = new ImodProcess(manager, datasetNameArray, modelName);
    reset();
  }

  void processRequest() {
    process.processRequest();
  }

  /**
   * Opens a process, opens a model.
   * 
   * @throws SystemProcessException
   */
  void open(Run3dmodMenuOptions menuOptions) throws SystemProcessException, IOException {
    if (menuOptions == null) {
      menuOptions = new Run3dmodMenuOptions();
    }
    menuOptions.setNoOptions(noMenuOptions);
    menuOptions.orGlobalOptions();
    menuOptions.setAllowBinningInZ(allowMenuBinningInZ);
    // process is not running
    if (!process.isRunning()) {
      // open
      process.open(menuOptions);
      warnedStaleFile = false;
      // model will be opened
      if (modelName != null && modelName.matches("\\S+") && preserveContrast) {
        process.setOpenModelPreserveContrastMessage(modelName);
      }
      // This message can only be sent after opening the model
      if (openContours) {
        process.setNewContoursMessage(true);
      }
      if (startNewContoursAtNewZ) {
        process.setStartNewContoursAtNewZ();
      }
      if (interpolation != null) {
        process.setInterpolation(interpolation.is());
        interpolation = null;
      }
      if (pointLimit != -1) {
        process.setPointLimitMessage(pointLimit);
      }
      // open bead fixer
      if (openBeadFixer) {
        process.setOpenBeadFixerMessage();
        if (setAutoCenter) {
          if (manager instanceof ApplicationManager) {
            process.setBeadfixerDiameter(((ApplicationManager) manager)
                .getBeadfixerDiameter(axisID));
          }
          process.setAutoCenter(autoCenter);
        }
        if (manageNewContours) {
          process.setNewContours(newContours);
        }
        if (beadfixerMode != null) {
          process.setBeadfixerMode(beadfixerMode);
        }
        if (logName != null) {
          process.setOpenLog(logName);
        }
        if (skipList != null) {
          process.setSkipList(skipList);
        }
        if (deleteAllSections != null) {
          process.setDeleteAllSections(deleteAllSections.is());
          deleteAllSections.set(false);
        }
      }
    }
    else {
      // process is running
      // raise 3dmod
      if (!modelView && !useModv) {
        process.setOpenZapWindowMessage();
      }
      if (interpolation != null) {
        process.setInterpolation(interpolation.is());
        interpolation = null;
      }
      else {
        process.setRaise3dmodMessage();
      }
      // reopen model
      if (!useModv && modelName != null && !modelName.matches("\\s+")) {
        if (preserveContrast) {
          process.setOpenModelPreserveContrastMessage(modelName);
        }
        else {
          process.setOpenModelMessage(modelName);
        }
      }
      // This message can only be sent after opening the model
      if (openContours) {
        process.setNewContoursMessage(true);
      }
      // open bead fixer
      if (openBeadFixer) {
        process.setOpenBeadFixerMessage();
        if (setAutoCenter) {
          if (manager instanceof ApplicationManager) {
            process.setBeadfixerDiameter(((ApplicationManager) manager)
                .getBeadfixerDiameter(axisID));
          }
          process.setAutoCenter(autoCenter);
        }
        if (manageNewContours) {
          process.setNewContours(newContours);
        }
        if (beadfixerMode != null) {
          process.setBeadfixerMode(beadfixerMode);
        }
        if (logName != null) {
          process.setOpenLog(logName);
        }
        if (deleteAllSections != null) {
          process.setDeleteAllSections(deleteAllSections.is());
          deleteAllSections.set(false);
        }
        process.setSkipList(skipList);
      }
    }
    if (openSurfContPoint) {
      process.openSurfContPoint();
      openSurfContPoint = false;
    }
    // set mode
    if (usingMode) {
      if (mode == MODEL_MODE) {
        process.setModelModeMessage();
      }
      else {
        process.setMovieModeMessage();
      }
    }
    if ((modelView || useModv) && interpolation == null && usingMode
        && mode != MODEL_MODE
        && EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
      System.err.println("ImodState:open: sendMessages");
      Thread.dumpStack();
    }
    process.sendMessages();
    reset();
  }

  void setOpenSurfContPoint(final boolean input) {
    openSurfContPoint = input;
  }

  void open(FileType modelName, Run3dmodMenuOptions menuOptions)
      throws SystemProcessException, IOException {
    setModel(modelName);
    open(menuOptions);
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
  void open(String modelName, Run3dmodMenuOptions menuOptions)
      throws SystemProcessException, IOException {
    setModelName(modelName);
    open(menuOptions);
  }

  void open(List<String> modelNameList, Run3dmodMenuOptions menuOptions)
      throws SystemProcessException, IOException {
    setModelNameList(modelNameList);
    open(menuOptions);
  }

  public void open(String modelName, boolean modelMode, Run3dmodMenuOptions menuOptions)
      throws SystemProcessException, IOException {
    setModelName(modelName);
    setModelMode(modelMode);
    open(menuOptions);
  }

  public Vector getRubberbandCoordinates() throws IOException, SystemProcessException {
    return process.getRubberbandCoordinates();
  }

  public Vector getSlicerAngles() throws IOException, SystemProcessException {
    return process.getSlicerAngles();
  }

  /**
   * Tells process to quit.
   */
  public void quit() throws IOException, SystemProcessException {
    process.quit();
  }

  public void disconnect() throws IOException {
    process.disconnect();
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

  /**
   * Zap opens by default.  OpenZap is only necessary when model view is used.
   */
  void setOpenZap() {
    process.setOpenZap();
  }

  void setTiltFile(String tiltFile) {
    process.setTiltFile(tiltFile);
  }

  void resetTiltFile() {
    process.resetTiltFile();
  }

  void addWindowOpenOption(ImodProcess.WindowOpenOption option) {
    process.addWindowOpenOption(option);
  }

  void reset() {
    // reset to initial state
    setModelName(initialModelName);
    mode = initialMode;
    swapYZ = initialSwapYZ;
    // reset to default state
    preserveContrast = defaultPreserveContrast;
    process.setOpenWithModel(!preserveContrast);
    openBeadFixer = defaultOpenBeadFixer;
    openContours = defaultOpenContours;
    process.setBinning(defaultBinning);
    process.setFrames(defaultFrames);
    process.setPieceListFileName(null);
    manageNewContours = false;
    pointLimit = -1;
    startNewContoursAtNewZ = false;
  }

  String getModeString(int mode) {
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

  void setDebug(boolean input) {
    debug = input;
    process.setDebug(debug);
  }

  boolean equalsSubdirName(String input) {
    return process.getSubdirName().equals(input);
  }

  boolean equalsFileNameArray(String[] input) {
    if (debug) {
      System.out.println("ImodState.equalsFileNameArray:input.length=" + input.length
          + ",fileNameArray.length=" + fileNameArray.length);
    }
    if (fileNameArray == null && input == null) {
      return true;
    }
    if (fileNameArray == null || input == null) {
      return false;
    }
    if (fileNameArray.length != input.length) {
      return false;
    }
    for (int i = 0; i < fileNameArray.length; i++) {
      if (!fileNameArray[i].equals(input[i])) {
        return false;
      }
    }
    return true;
  }

  // unchanging state information

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

  // current state information
  /**
   * @return modelName
   */
  public String getModelName() {
    return modelName;
  }

  private void setModel(FileType model) {
    setModelName(model.getFile(manager, axisID).getAbsolutePath());
  }

  private void setModelName(String modelName) {
    this.modelName = modelName;
    process.setModelName(modelName);
  }

  private void setModelNameList(List<String> modelNameList) {
    this.modelNameList = modelNameList;
    process.setModelNameList(modelNameList);
  }

  void setLoadAsIntegers() {
    process.setLoadAsIntegers();
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
  public void setUsingMode(boolean usingMode) {
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
  public void setOpenContours(boolean openContours) {
    this.openContours = openContours;
  }

  public void setStartNewContoursAtNewZ(boolean startNewContoursAtNewZ) {
    this.startNewContoursAtNewZ = startNewContoursAtNewZ;
  }

  void setPointLimit(int input) {
    pointLimit = input;
  }

  final AxisID getAxisID() {
    return axisID;
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

  public void setPieceListFileName(String pieceListFileName) {
    process.setPieceListFileName(pieceListFileName);
  }

  public void setMontageSeparation() {
    process.setMontageSeparation();
  }

  public void setInterpolation(final boolean input) {
    if (interpolation == null) {
      interpolation = new EtomoBoolean2();
    }
    interpolation.set(input);
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
    setAutoCenter = false;
    autoCenter = false;
    newContours = false;
    manageNewContours = false;
  }

  void setAutoCenter(boolean autoCenter) {
    setAutoCenter = true;
    this.autoCenter = autoCenter;
  }

  void setSkipList(String input) {
    skipList = input;
  }

  void setDeleteAllSections(boolean on) {
    if (deleteAllSections == null) {
      deleteAllSections = new EtomoBoolean2();
    }
    deleteAllSections.set(on);
  }

  void setBeadfixerMode(ImodProcess.BeadFixerMode mode) {
    beadfixerMode = mode;
  }

  void setOpenLogOff() {
    logName = null;

  }

  void setOpenLog(boolean openLog, String logName) {
    if (openLog) {
      this.logName = logName;
    }
    else {
      this.logName = null;
    }
  }

  void setNewContours(boolean newContours) {
    this.newContours = newContours;
    manageNewContours = true;
  }

  // initial state information
  /**
   * @return initialModelName
   */
  public String getInitialModelName() {
    return initialModelName;
  }

  public String getDatasetName() {
    return process.getDatasetName();
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

  // default state information

  public boolean isDefaultOpenWithModel() {
    return defaultOpenWithModel;
  }

  public boolean isDefaultPreserveContrast() {
    return defaultPreserveContrast;
  }

  // user controlled state information - pass through to ImodProcess
  /**
   * @return true if process is running
   */
  public boolean isOpen() {
    return process.isRunning();
  }

  final void setAllowMenuBinningInZ(boolean allowMenuBinningInZ) {
    this.allowMenuBinningInZ = allowMenuBinningInZ;
  }

  final void setNoMenuOptions(boolean noMenuOptions) {
    this.noMenuOptions = noMenuOptions;
  }

  public void reopenLog() throws SystemProcessException, IOException {
    process.reopenLog();
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

  public void setOpenModelView() throws IOException, SystemProcessException {
    process.setOpenModelView();
  }

  void setContinuousListenerTarget(ContinuousListenerTarget continuousListenerTarget) {
    process.setContinuousListenerTarget(continuousListenerTarget);
  }

  // internal state sets and gets
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
   
   public String toString() {
   return getClass().getName() + "[" + paramString() + "]";
   }*/

  /**
   * @return string
   */
  protected String paramString() {
    Vector params = new Vector(17);
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
    if (modelView == imodState.isModelView() && useModv == imodState.isUseModv()
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
    if (modelView == imodState.isModelView() && useModv == imodState.isUseModv()
        && modelName.equals(imodState.getModelName())
        && usingMode == imodState.isUsingMode()
        && openContours == imodState.isOpenContours() && mode == imodState.getMode()
        && swapYZ == imodState.isSwapYZ()
        && preserveContrast == imodState.isPreserveContrast()
        && openBeadFixer == imodState.isOpenBeadFixer()
        && startNewContoursAtNewZ == imodState.startNewContoursAtNewZ) {
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
    return equalsInitialConfiguration(imodState) && equalsCurrentConfiguration(imodState);
  }

}
