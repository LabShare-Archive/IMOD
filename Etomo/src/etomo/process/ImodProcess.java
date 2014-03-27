package etomo.process;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Vector;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.OSType;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.swing.UIHarness;
import etomo.util.Utilities;

/**
 * <p>
 * Description: ImodProcess opens an instance of imod with the specfied stack
 * projection stack(s) and possibly model files. Model files can also be loaded
 * and changed after the process has started.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.72  2011/06/22 02:22:05  sueh
 * bug# 1462 Added loadAsIntegers.
 *
 * Revision 3.71  2011/04/04 16:51:11  sueh
 * bug# 1416 Added fileList and a constructor.  Modified open.
 *
 * Revision 3.70  2011/02/21 16:58:25  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 3.69  2010/11/13 16:03:45  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 3.68  2010/09/21 16:25:42  sueh
 * bug# 1395 Added BF_MESSAGE_REMOVE_SKIP_LIST to handle a null skip
 * list.
 *
 * Revision 3.67  2010/05/16 17:41:24  sueh
 * bug# 1358 In imodSendEvent stop throwing an exception just because
 * messages was passed in as null.
 *
 * Revision 3.66  2010/05/12 17:25:26  sueh
 * bug# 1358 In Mac keep getting an exception in MessageSender.readResponse.  Added more
 information to the exception.  Increased the timeout of readResponse.
 *
 * Revision 3.65  2010/03/11 06:01:28  sueh
 * bug# 1311 Added setOpenModelView.  Added BeadFixerMode.
 *
 * Revision 3.64  2010/02/17 04:49:20  sueh
 * bug# 1301 Using the manager instead of the manager key do pop up
 * messages.
 *
 * Revision 3.63  2009/12/19 01:09:19  sueh
 * bug# 1294 Added WindowOpenOption.OBJECT_LIST.
 *
 * Revision 3.62  2009/10/06 23:11:00  sueh
 * bug# 1251 In disconnect, disconnect in all operating system, not just
 * windows.
 *
 * Revision 3.61  2009/09/30 19:12:19  sueh
 * iutest on salsa:  In readResponse changed timeout to 20 because timeout equals 10 was timing
 out, even when the file opened normally.
 *
 * Revision 3.60  2009/09/01 03:17:56  sueh
 * bug# 1222
 *
 * Revision 3.59  2009/06/10 22:13:54  sueh
 * bug# 1220 In sendMessages start the continuous listener thread if the
 * target is set and it is not already started.
 *
 * Revision 3.58  2009/06/05 01:54:43  sueh
 * bug# 1219 Added setStartNewContoursAtNewZ.
 *
 * Revision 3.57  2009/03/24 21:10:24  sueh
 * bug# 1187 In ContinuousListener.startThread create a new Thread instance when the
 * old one is not alive.
 *
 * Revision 3.56  2009/03/24 20:17:03  sueh
 * bug# 1187 Added some debug statements.
 *
 * Revision 3.55  2009/03/23 17:07:02  sueh
 * bug# 1187 Added classes ContinuousListener and Stderr, member variables
 * CONTINUOUS_TAG, continuousListener, continuousListenerTarget, stderr, and
 * listenToStdin.  Removing member variables requestQueue and stderrQueue, and
 * function getStderr.  In disconnect() only send the MESSAGE_STOP_LISTENING
 * command when the OS is Windows and the --listen parameter is used.  In
 * MessageSender changed responseRequired to readResponse.  Get all stderr
 * messages from Stderr.  Stop sleeping for 500 except in Stderr.  In open
 * (Run3dmodMenuOptions) update member variables stderr and continuousListener.
 * Use listenToStdin to decide if MessageSender or imodsendevent should be used.
 *
 * Revision 3.54  2009/03/17 00:36:13  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.53  2009/03/11 21:37:15  sueh
 * bug# 1195 In imodSendAndReceive added error pop ups.
 *
 * Revision 3.52  2009/03/09 21:07:39  sueh
 * bug# 1198 removed exceptionMessage - ignoring unrecognized
 * messages.
 *
 * Revision 3.51  2008/12/09 21:32:44  sueh
 * bug# 1160 Removing the constructor that has the beadfixerDiameter
 * parameter.  Added setBeadfixerDiameter.  Also setting
 * beadfixerDiameter to default in setAutoCenter when beadfixerDiameter
 * was not set.
 *
 * Revision 3.50  2008/12/05 00:52:03  sueh
 * bug# 1156 Added setSkipList.
 *
 * Revision 3.49  2008/07/24 17:58:25  sueh
 * bug# 1128 Added setMoreObjectPropertiesMessage and
 * setPointLimitMessage.
 *
 * Revision 3.48  2008/06/19 23:34:54  sueh
 * bug# 1112 Added tiltFile.
 *
 * Revision 3.47  2008/05/01 22:57:45  sueh
 * bug# 1107 Added openZap to add -Z to the command line.  Since the
 * Zap window opens automatically, this is only useful when using model
 * view.  Added WindowOpenOption.ISOSURFACE (U).
 *
 * Revision 3.46  2007/12/26 22:13:09  sueh
 * bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 *
 * Revision 3.45  2007/11/06 19:22:28  sueh
 * bug# 1047 Added flip and subdirName.
 *
 * Revision 3.44  2007/09/07 00:19:05  sueh
 * bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * instead of getInstance and createInstance.
 *
 * Revision 3.43  2007/05/11 15:41:20  sueh
 * bug# 964 Added ImodProcess(BaseManager, String[]) to handle multiple
 * files without a model.
 *
 * Revision 3.42  2006/09/19 22:21:19  sueh
 * bug# 928 Added WindowOpenOption, to use 3dmod's window open command-
 * line functionality.
 *
 * Revision 3.41  2006/08/11 23:49:25  sueh
 * bug# 816 Added reopenLog().
 *
 * Revision 3.40  2006/08/11 21:46:10  sueh
 * bug# 816 Added setOpenLog()
 *
 * Revision 3.39  2006/07/17 21:17:29  sueh
 * bug# 900 Added imodSendEvent functionality back for Windows.
 *
 * Revision 3.38  2006/07/04 20:39:57  sueh
 * bug# 894 Changed seedMode to newContours.  Added setBeadfixerMode().
 *
 * Revision 3.37  2006/07/03 23:33:46  sueh
 * bug# 895 Added responseRequired to MessageSender so that it doesn't have
 * to wait for a response for disconnect.
 *
 * <p>
 * Revision 3.36 2006/07/03 21:42:21 sueh
 * <p>
 * bug# 895 Added processRequest() to disconnect if a request is received from
 * <p>
 * 3dmod. Added requestQueue and stderrQueue to store output from
 * <p>
 * imod.getStderr() until it is needed. isRequestReceived() adds lines to
 * <p>
 * stderrQueue and returns true when it finds a request. GetStderr() adds lines
 * to
 * <p>
 * requestQueue and returns a line from stderr when it is not a request.
 * <p>
 * <p>
 * Revision 3.35 2006/06/28 23:28:51 sueh
 * <p>
 * Removed unnecessary print
 * <p>
 * <p>
 * Revision 3.34 2006/06/26 18:56:12 sueh
 * <p>
 * bug# 797 Want the send and receive message attempts to exclude other send
 * <p>
 * and receive attempts while they are working, without locking up the GUI.
 * <p>
 * Remove ResponseReader and add MessageSender. GUI will be locked when a
 * <p>
 * message requiring a reply is sent.
 * <p>
 * <p>
 * Revision 3.33 2006/06/22 21:01:43 sueh
 * <p>
 * bug# 797 Stop using imodSendEvent. Added sendCommand(s() and
 * <p>
 * sendRequest().
 * <p>
 * <p>
 * Revision 3.32 2006/05/22 22:47:22 sueh
 * <p>
 * bug# 577 Formatted
 * <p>
 * <p>
 * Revision 3.31 2006/04/11 13:47:20 sueh
 * <p>
 * bug# 809 Manage auto center and seed mode separately from
 * <p>
 * openBeadFixer so that seed mode doesn't always have to be managed.
 * <p>
 * <p>
 * Revision 3.30 2006/03/30 21:23:24 sueh
 * <p>
 * bug# 809 Sending seed mode, auto center, and diameter messages to
 * <p>
 * the bead fixer.
 * <p>
 * <p>
 * Revision 3.29 2005/11/02 21:57:48 sueh
 * <p>
 * bug# 754 Getting error and warning tags from ProcessMessages.
 * <p>
 * <p>
 * Revision 3.28 2005/08/15 18:21:26 sueh
 * <p>
 * bug# 532 commenting print statements
 * <p>
 * <p>
 * Revision 3.27 2005/08/11 23:38:42 sueh
 * <p>
 * bug# 711 Pass Run3dmodMenuOptions to ImodManager.open(),
 * <p>
 * ImodState.open(), and ImodProcess.open(). It should not be saved,
 * <p>
 * because it needs to be refreshed each time 3dmod is run. In
 * <p>
 * ImodState.open() add the menu options from the pulldown menu to the
 * <p>
 * existing menu options.
 * <p>
 * <p>
 * Revision 3.26 2005/08/09 19:58:15 sueh
 * <p>
 * bug# 711 Added Run3dmodMenuOption processing to open(). Added
 * <p>
 * calcCurrentBinning().
 * <p>
 * <p>
 * Revision 3.25 2005/07/29 00:51:48 sueh
 * <p>
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p>
 * because the current manager changes when the user changes the tab.
 * <p>
 * Passing the manager where its needed.
 * <p>
 * <p>
 * Revision 3.24 2005/04/25 20:46:30 sueh
 * <p>
 * bug# 615 Passing the axis where a command originates to the message
 * <p>
 * functions so that the message will be popped up in the correct window.
 * <p>
 * This requires adding AxisID to many objects.
 * <p>
 * <p>
 * Revision 3.23 2005/03/04 00:14:40 sueh
 * <p>
 * bug# 533 Added setPieceListFileName() to set the -p command line
 * <p>
 * option in the 3dmod call.
 * <p>
 * <p>
 * Revision 3.22 2005/03/02 23:14:19 sueh
 * <p>
 * bug# 533 Adding -fr (frames) to ignore montaging information and
 * <p>
 * display the stack frame by frame.
 * <p>
 * <p>
 * Revision 3.21 2004/12/14 01:35:09 sueh
 * <p>
 * bug# 373 Getting a list of dataset names with datasetNameArray. Do not
 * <p>
 * add the model name to the command if the model name is "".
 * <p>
 * <p>
 * Revision 3.20 2004/12/04 00:57:56 sueh
 * <p>
 * bug# 569 Handling directory paths with spaces: converting from a
 * <p>
 * command line to a command array to prevent the command line from
 * <p>
 * being split on white space.
 * <p>
 * <p>
 * Revision 3.19 2004/11/24 18:10:36 sueh
 * <p>
 * bug# 520 Added binning in XY.
 * <p>
 * <p>
 * Revision 3.18 2004/11/19 23:21:39 sueh
 * <p>
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p>
 * Revision 3.17.4.3 2004/10/08 15:57:43 sueh
 * <p>
 * bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p>
 * member variables non-static.
 * <p>
 * <p>
 * Revision 3.17.4.2 2004/09/22 22:07:25 sueh
 * <p>
 * bug# 520 Added get slicer angles functionality.
 * <p>
 * <p>
 * Revision 3.17.4.1 2004/09/03 21:11:24 sueh
 * <p>
 * bug# 520 calling from EtomoDirector.isDebug
 * <p>
 * <p>
 * Revision 3.17 2004/06/22 22:54:50 sueh
 * <p>
 * bug# 462 Removed fillCache. bug# 455 added openWithModel
 * <p>
 * functionality to handle opening a model while preserving contrast.
 * <p>
 * Added open contours functions
 * <p>
 * <p>
 * Revision 3.16 2004/06/17 01:29:52 sueh
 * <p>
 * added 3dmod command to err log because it is useful to see
 * <p>
 * <p>
 * Revision 3.15 2004/06/10 18:23:11 sueh
 * <p>
 * bug# 463 add setOpenBeadFixerMessage() to add the open
 * <p>
 * bead fixer message to the message list
 * <p>
 * <p>
 * Revision 3.14 2004/06/07 18:42:06 sueh
 * <p>
 * bug# 457 added functions to add messages to list.
 * <p>
 * Added a function to send the messages to 3dmod using
 * <p>
 * imodSendEvent.
 * <p>
 * <p>
 * Revision 3.13 2004/06/07 16:58:44 rickg
 * <p>
 * Bug #452 added debug output for imodsendevent since we have
 * <p>
 * been having diffuculty with it.
 * <p>
 * <p>
 * Revision 3.12 2004/05/13 20:11:21 sueh
 * <p>
 * bug# 33 allowing imodSendAndReceive() to receive any type
 * <p>
 * of result data
 * <p>
 * <p>
 * Revision 3.11 2004/05/07 19:43:57 sueh
 * <p>
 * bug# 33 adding getRubberbandCoordinates(),
 * <p>
 * imodSendAndReceive(), parseError().
 * <p>
 * Keeping InteractiveSystemProgram imod around for send
 * <p>
 * and receive.
 * <p>
 * <p>
 * Revision 3.10 2004/05/06 20:21:47 sueh
 * <p>
 * bug# 33 added getRubberbandCoordinates(), passing back the
 * <p>
 * InteractiveSystemProgram from imodSendEvent()
 * <p>
 * <p>
 * Revision 3.9 2004/05/03 22:22:25 sueh
 * <p>
 * bug# 416 added binning (-B)
 * <p>
 * <p>
 * Revision 3.8 2004/04/30 21:11:52 sueh
 * <p>
 * bug# 428 add open ZaP window message
 * <p>
 * <p>
 * Revision 3.7 2004/04/27 22:02:58 sueh
 * <p>
 * bug# 320 removing test
 * <p>
 * <p>
 * Revision 3.6 2004/04/26 17:05:05 sueh
 * <p>
 * bug# 320 Commented out code - no functional change.
 * <p>
 * Experimenting with a fix for this bug.
 * <p>
 * <p>
 * Revision 3.5 2004/04/22 23:26:11 rickg
 * <p>
 * Switched getIMODBinPath method
 * <p>
 * <p>
 * Revision 3.4 2004/02/07 03:04:59 sueh
 * <p>
 * bug# 169 Added setWorkingDirectory().
 * <p>
 * <p>
 * Revision 3.3 2003/11/21 23:54:49 sueh
 * <p>
 * bug242 Added toString() function
 * <p>
 * <p>
 * Revision 3.2 2003/11/12 17:14:36 sueh
 * <p>
 * removing debug prints
 * <p>
 * <p>
 * Revision 3.1 2003/11/11 00:23:59 sueh
 * <p>
 * Bug349 add useModv "-view" default false, add
 * <p>
 * outputWindowID "-W" default true, open(): -W is a default
 * <p>
 * option rather then a constant, multiple options allowed
 * <p>
 * <p>
 * Revision 3.0 2003/11/07 23:19:00 rickg
 * <p>
 * Version 1.0.0
 * <p>
 * <p>
 * Revision 2.18 2003/11/05 20:28:42 rickg
 * <p>
 * Bug #292 Added openPreserveContrast and openBeadfixer methods
 * <p>
 * <p>
 * Revision 2.17 2003/11/04 20:56:11 rickg
 * <p>
 * Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p>
 * Revision 2.16 2003/11/04 17:45:21 rickg
 * <p>
 * Bug #345 Explicitly set path to 3dmodusing IMOD_DIR
 * <p>
 * <p>
 * Revision 2.15 2003/11/04 01:03:37 rickg
 * <p>
 * Javadoc comment fix
 * <p>
 * <p>
 * Revision 2.14 2003/09/25 22:17:17 rickg
 * <p>
 * Corrected a sendevent comment
 * <p>
 * <p>
 * Revision 2.13 2003/08/25 22:18:39 rickg
 * <p>
 * Removed errant model opening for the tomogram where a matching
 * <p>
 * or patch region model had been previously opened
 * <p>
 * <p>
 * Revision 2.12 2003/08/05 21:20:45 rickg
 * <p>
 * Added movieMode
 * <p>
 * <p>
 * Revision 2.11 2003/07/25 23:00:33 rickg
 * <p>
 * openModel does not automatically switch 3dmod to model mode
 * <p>
 * now
 * <p>
 * <p>
 * Revision 2.10 2003/06/05 21:12:23 rickg
 * <p>
 * Added model mode and raise messages
 * <p>
 * fill cache flag is functional
 * <p>
 * <p>
 * Revision 2.9 2003/05/27 08:44:03 rickg
 * <p>
 * Removed TODO
 * <p>
 * <p>
 * Revision 2.8 2003/05/15 20:19:41 rickg
 * <p>
 * Removed extraneous debug printing
 * <p>
 * <p>
 * Revision 2.7 2003/05/12 23:26:29 rickg
 * <p>
 * imod -D -> 3dmod
 * <p>
 * commad line reporting (need to check debug state)
 * <p>
 * <p>
 * Revision 2.6 2003/05/07 22:28:30 rickg
 * <p>
 * Implemented fillCache mechanism, but not enabled
 * <p>
 * <p>
 * Revision 2.5 2003/04/28 23:25:26 rickg
 * <p>
 * Changed visible imod references to 3dmod
 * <p>
 * <p>
 * Revision 2.4 2003/03/19 00:23:22 rickg
 * <p>
 * Added model view option
 * <p>
 * <p>
 * Revision 2.3 2003/03/02 23:30:41 rickg
 * <p>
 * Combine layout in progress
 * <p>
 * <p>
 * Revision 2.2 2003/01/31 05:34:08 rickg
 * <p>
 * Support for foreground imod/qtimod through -W
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 21:09:05 rickg
 * <p>
 * Added sleep to wait for imod process to exit and then
 * <p>
 * some when. For some reason the windowID/processID
 * <p>
 * strings were not available
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.6 2002/10/16 17:36:24 rickg
 * <p>
 * reformat
 * <p>
 * <p>
 * Revision 1.5 2002/09/20 17:06:38 rickg
 * <p>
 * Added typed exceptions
 * <p>
 * Added a quit method
 * <p>
 * Check for ProcessID before running PS in isRunning
 * <p>
 * <p>
 * Revision 1.4 2002/09/19 22:47:45 rickg
 * <p>
 * More robust method to extract process and window ID from imod
 * <p>
 * <p>
 * Revision 1.3 2002/09/18 23:39:26 rickg
 * <p>
 * Moved opening to a separate method
 * <p>
 * Opening checks to see if the imod process already exists
 * <p>
 * <p>
 * Revision 1.2 2002/09/17 23:20:31 rickg
 * <p>
 * Complete basic operation
 * <p>
 * <p>
 * Revision 1.1 2002/09/13 21:28:44 rickg
 * <p>
 * initial entry
 * <p>
 * </p>
 */
public class ImodProcess {
  public static final String rcsid = "$Id$";

  public static final String MESSAGE_OPEN_MODEL = "1";
  public static final String MESSAGE_SAVE_MODEL = "2";
  public static final String MESSAGE_VIEW_MODEL = "3";
  public static final String MESSAGE_CLOSE = "4";
  public static final String MESSAGE_RAISE = "5";
  public static final String MESSAGE_OPEN_MODEL_VIEW = "3";
  public static final String MESSAGE_MODEL_MODE = "6";
  public static final String MESSAGE_OPEN_KEEP_BW = "7";
  public static final String MESSAGE_OPEN_BEADFIXER = "8";
  public static final String MESSAGE_ONE_ZAP_OPEN = "9";
  public static final String MESSAGE_RUBBERBAND = "10";
  public static final String MESSAGE_OBJ_PROPERTIES = "11";
  public static final String MESSAGE_NEWOBJ_PROPERTIES = "12";
  public static final String MESSAGE_SLICER_ANGLES = "13";
  public static final String MESSAGE_PLUGIN_MESSAGE = "14";
  public static final String MESSAGE_MORE_OBJ_PROPERTIES = "16";
  public static final String MESSAGE_INTERPOLATION = "18";
  public static final String BEAD_FIXER_PLUGIN = "Bead Fixer";
  public static final String BF_MESSAGE_OPEN_LOG = "1";
  public static final String BF_MESSAGE_REREAD_LOG = "2";
  public static final String BF_MESSAGE_NEW_CONTOURS = "3";
  public static final String BF_MESSAGE_AUTO_CENTER = "4";
  public static final String BF_MESSAGE_DIAMETER = "5";
  public static final String BF_MESSAGE_MODE = "6";
  public static final String BF_MESSAGE_SKIP_LIST = "7";
  public static final String BF_MESSAGE_DELETE_ALL_SECTIONS = "8";
  public static final String BF_MESSAGE_REMOVE_SKIP_LIST = "9";
  public static final String MESSAGE_ON = "1";
  public static final String MESSAGE_OFF = "0";
  public static final String MESSAGE_STOP_LISTENING = "\n";
  public static final String RUBBERBAND_RESULTS_STRING = "Rubberband:";
  public static final String SLICER_ANGLES_RESULTS_STRING1 = "Slicer";
  public static final String SLICER_ANGLES_RESULTS_STRING2 = "angles:";
  public static final String TRUE = "1";
  public static final String FALSE = "0";
  public static final int CIRCLE = 1;
  public static final String REQUEST_TAG = "REQUEST";
  public static final String STOP_LISTENING_REQUEST = "STOP LISTENING";
  private static final int defaultBinning = 1;
  public static final String IMOD_SEND_EVENT_STRING = "imodsendevent returned:";
  // static final String CONTINUOUS_TAG = "ETOMO INFO:";
  static final String CONTINUOUS_TAG = "ETOMO INFO:";
  private static final String MESSAGE_OPEN_DIALOG = "19";
  private static final String SURF_CONT_POINT_DIALOG = "s";

  // Get stderr messages only through this member variable.
  private final Stderr stderr = new Stderr();
  private final Integer messageSenderRegId = stderr.register();
  private final Integer stderrRegId = stderr.register();
  private final ContinuousListener continuousListener;

  private String datasetName = "";
  private String modelName = "";
  private String windowID = "";
  private boolean swapYZ = false;
  private boolean modelView = false;
  private boolean useModv = false;
  private boolean outputWindowID = true;
  private boolean openWithModel = true;
  private File workingDirectory = null;
  private int binning = defaultBinning;
  private int binningXY = defaultBinning;
  InteractiveSystemProgram imod = null;
  private Vector sendArguments = new Vector();
  private String[] datasetNameArray = null;
  private boolean frames = false;
  private String pieceListFileName = null;
  private AxisID axisID;
  private boolean flip = false;
  private Thread imodThread;

  private final BaseManager manager;

  private boolean beadfixerDiameterSet = false;
  private ArrayList windowOpenOptionList = null;
  private boolean debug = false;
  private String subdirName = null;
  // Zap opens by default. OpenZap is only necessary when model view is in use.
  private boolean openZap = false;
  private String tiltFile = null;
  private ContinuousListenerTarget continuousListenerTarget = null;
  private File[] fileList = null;
  private boolean loadAsIntegers = false;
  private boolean montageSeparation = false;
  private List<String> modelNameList = null;

  /**
   * If true, run 3dmod with -L.  This means that imodsentevent will not be used
   * - the MessageSender can be used instead.  In Windows the
   * Stderr.requestQueue will receive requests to send MESSAGE_STOP_LISTENING.
   * The thread should be checking this queue (see ImodManager.ImodManager()).
   */
  private final boolean listenToStdin = !Utilities.isWindowsOS()
      || EtomoDirector.INSTANCE.getArguments().isListen();

  /**
   * Constructor for using imodv
   * 
   */
  public ImodProcess(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    continuousListener = new ContinuousListener(stderr, axisID);

  }

  /**
   * Dataset only constructor
   * 
   * @param A
   *          string specifying the path to the projection stack file
   */
  public ImodProcess(BaseManager manager, String dataset, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    datasetName = dataset;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  public ImodProcess(BaseManager manager, String dataset, AxisID axisID, boolean flip) {
    this.manager = manager;
    this.axisID = axisID;
    this.flip = flip;
    datasetName = dataset;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  /**
   * Dataset and model file constructor
   * 
   * @param dataset
   *          A string specifying the path to the projection stack file
   * @param model
   *          A string specifying the path to the IMOD model file
   */
  public ImodProcess(BaseManager manager, String dataset, String model) {
    this.manager = manager;
    datasetName = dataset;
    modelName = model;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  /**
   * Dataset and model file constructor
   * 
   * @param datasetArray
   *          A string array specifying the path to the projection stack file
   * @param model
   *          A string specifying the path to the IMOD model file
   */
  public ImodProcess(BaseManager manager, String[] datasetArray, String model) {
    this.manager = manager;
    datasetNameArray = datasetArray;
    modelName = model;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  public ImodProcess(BaseManager manager, String[] datasetArray) {
    this.manager = manager;
    datasetNameArray = datasetArray;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  public ImodProcess(final BaseManager manager, final File[] fileList) {
    this.manager = manager;
    this.fileList = fileList;
    continuousListener = new ContinuousListener(stderr, axisID);
  }

  /**
   * Change the dataset name
   * 
   * @param datasetName
   */
  public void setDatasetName(String datasetName) {
    this.datasetName = datasetName;
  }

  public void setSubdirName(String input) {
    subdirName = input;
  }

  public String getSubdirName() {
    return subdirName;
  }

  /**
   * Sets the -f command line option
   * 
   * @param frames
   */
  public void setFrames(boolean frames) {
    this.frames = frames;
  }

  public void setPieceListFileName(String pieceListFileName) {
    this.pieceListFileName = pieceListFileName;
  }

  public void setMontageSeparation() {
    this.montageSeparation = true;
  }

  /**
   * Specify or change the model name
   * 
   * @param modelName
   */
  public void setModelName(String modelName) {
    this.modelName = modelName;
  }

  public void setModelNameList(List<String> modelNameList) {
    this.modelNameList = modelNameList;
  }

  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  void setLoadAsIntegers() {
    loadAsIntegers = true;
  }

  /**
   * When openWithModel is true 3dmod will open with a model, if a model is set.
   * The default for openWithModel is true. Some open model options cannot be
   * sent to 3dmod during open. Turn off this option to prevent opening the
   * model during open. Example: MESSAGE_OPEN_KEEP_BW
   * 
   * @param openWithoutModel
   */
  public void setOpenWithModel(boolean openWithModel) {
    this.openWithModel = openWithModel;
  }

  private final int calcCurrentBinning(int binning, Run3dmodMenuOptions menuOptions) {
    int currentBinning;
    if (binning == defaultBinning) {
      currentBinning = 0;
    }
    else {
      currentBinning = binning;
    }
    if (menuOptions.isBinBy2()) {
      currentBinning += 2;
    }
    return currentBinning;
  }

  /**
   * Open the 3dmod process if is not already open.
   */
  public void open(Run3dmodMenuOptions menuOptions) throws SystemProcessException,
      IOException {
    if (isRunning()) {
      raise3dmod();
      return;
    }

    // Reset the window string
    windowID = "";
    if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
      System.err.println("open 1:windowID:" + windowID);
    }
    ArrayList commandOptions = new ArrayList();
    commandOptions.add(ApplicationManager.getIMODBinPath() + "3dmod");
    // Collect the command line options

    // 3/22/09
    // On Mac never run with -D, -W, and not -L. This will crash 3dmod by
    // copying the clipboard onto the message area. 3dmod will crash if there is
    // something big in the clipboard.
    if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
      commandOptions.add("-D");
      if (OSType.getInstance() == OSType.MAC && outputWindowID && !listenToStdin) {
        commandOptions.add("-L");
      }
    }

    if (outputWindowID) {
      commandOptions.add("-W");
    }
    if (listenToStdin) {
      commandOptions.add("-L");
    }

    if (swapYZ) {
      commandOptions.add("-Y");
    }
    if (frames) {
      commandOptions.add("-f");
    }

    if (montageSeparation) {
      commandOptions.add("-o");
      commandOptions.add(etomo.comscript.Utilities.MONTAGE_SEPARATION + ","
          + etomo.comscript.Utilities.MONTAGE_SEPARATION);
    }

    if (pieceListFileName != null && pieceListFileName.matches("\\S+")) {
      commandOptions.add("-p");
      commandOptions.add(pieceListFileName);
    }

    if (modelView) {
      commandOptions.add("-V");
    }

    if (openZap) {
      commandOptions.add("-Z");
    }

    if (loadAsIntegers) {
      commandOptions.add("-I");
      commandOptions.add("1");
    }

    if (tiltFile != null) {
      commandOptions.add("-a");
      commandOptions.add(tiltFile);
    }

    if (useModv) {
      commandOptions.add("-view");
    }
    /* if (debug) { commandOptions.add("-DC"); } */
    if (binning > defaultBinning
        || (menuOptions.isBinBy2() && menuOptions.isAllowBinningInZ())) {
      commandOptions.add("-B");
      commandOptions.add(Integer.toString(calcCurrentBinning(binning, menuOptions)));
    }

    if (binningXY > defaultBinning
        || (menuOptions.isBinBy2() && !menuOptions.isAllowBinningInZ())) {
      commandOptions.add("-b");
      commandOptions.add(Integer.toString(calcCurrentBinning(binningXY, menuOptions)));
    }

    if (menuOptions.isStartupWindow()) {
      commandOptions.add("-O");
    }

    if (windowOpenOptionList != null) {
      commandOptions.add(WindowOpenOption.OPTION);
      StringBuffer buffer = new StringBuffer(
          ((WindowOpenOption) windowOpenOptionList.get(0)).toString());
      for (int i = 1; i < windowOpenOptionList.size(); i++) {
        buffer.append(((WindowOpenOption) windowOpenOptionList.get(i)).toString());
      }
      commandOptions.add(buffer.toString());
    }

    if (!datasetName.equals("")) {
      commandOptions.add(datasetName);
    }

    if (datasetNameArray != null) {
      for (int i = 0; i < datasetNameArray.length; i++) {
        if (subdirName == null) {
          commandOptions.add(datasetNameArray[i]);
        }
        else {
          commandOptions.add(new File(subdirName, datasetNameArray[i]).getPath());
        }
      }
    }

    if (fileList != null) {
      for (int i = 0; i < fileList.length; i++) {
        if (subdirName == null) {
          commandOptions.add(fileList[i].getName());
        }
        else {
          commandOptions.add(new File(subdirName, fileList[i].getName()).getPath());
        }
      }
    }

    if (openWithModel) {
      if (!modelName.equals("")) {
        commandOptions.add(modelName);
      }
      if (modelNameList != null) {
        Iterator<String> i = modelNameList.iterator();
        while (i.hasNext()) {
          commandOptions.add(i.next());
        }
      }
    }
    String[] commandArray = new String[commandOptions.size()];
    for (int i = 0; i < commandOptions.size(); i++) {
      commandArray[i] = (String) commandOptions.get(i);
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.print(commandArray[i] + " ");
      }
      else if (debug) {
        System.err.print(commandArray[i] + " ");
      }
    }
    if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
      System.err.println();
    }
    else if (debug) {
      System.err.println();
    }
    imod = new InteractiveSystemProgram(manager, commandArray, axisID);
    stderr.setImod(imod);

    if (workingDirectory != null) {
      imod.setWorkingDirectory(workingDirectory);
    }
    // Start the 3dmod program thread and wait for it to finish
    imodThread = new Thread(imod);
    imodThread.start();
    if (continuousListenerTarget != null) {
      continuousListener.startThread(imodThread, continuousListenerTarget);
    }
    // Synchronized on stderr.quickListenerQueue to keep other threads from
    // from causing a response to appear on this queue before start up messages
    // are processed.
    synchronized (stderr.quickListenerQueue) {

      // Check the stderr of the 3dmod process for the windowID and the
      String line;
      if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isVerbose()) {
        System.err.println("ImodProcess:open " + Utilities.getDateTimeStamp(true));
      }
      while (imodThread.isAlive() && windowID.equals("")) {
        while ((line = stderr.getQuickMessage(stderrRegId)) != null) {
          if (line.indexOf("Window id = ") != -1) {
            String[] words = line.split("\\s+");
            if (words.length < 4) {
              throw (new SystemProcessException("Could not parse window ID from imod\n"));
            }
            windowID = words[3];
            if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isOn()) {
              System.err.println("Found windowID:" + windowID);
            }
          }
        }
      }
      // If imod exited before getting the window report the problem to the user
      if (windowID.equals("") && outputWindowID) {
        String message = "Missing windowID.  3dmod returned: "
            + String.valueOf(imod.getExitValue()) + "\n";

        while ((line = stderr.getQuickMessage(stderrRegId)) != null) {
          System.err.println(line);
          message = message + "stderr: " + line + "\n";
        }

        while ((line = imod.readStdout()) != null) {
          message = message + "stdout: " + line + "\n";
          line = imod.readStdout();
        }

        throw (new SystemProcessException(message));
      }
    }
  }

  /**
   * Send the quit messsage to imod
   */
  public void quit() throws IOException, SystemProcessException {
    if (isRunning()) {
      String[] messages = new String[1];
      messages[0] = MESSAGE_CLOSE;
      send(messages);
    }
  }

  /**
   * When 3dmod is listening to stdin, it can't quit properly, so send
   * it a command to stop listening to stdin.
   * @throws IOException
   */
  public void disconnect() throws IOException {
    if (listenToStdin) {
      if (isRunning()) {
        String[] messages = new String[1];
        messages[0] = MESSAGE_STOP_LISTENING;
        sendCommandsNoWait(messages);
        System.err.println("Telling 3dmod " + datasetName + " to stop listening.");
      }
    }
  }

  /**
   * Check to see if this 3dmod process is running
   */
  public boolean isRunning() {
    if (imodThread == null) {
      return false;
    }
    return imodThread.isAlive();
  }

  /**
   * Places arguments to open a model on the argument list.
   * 
   * @param newModelName
   */
  public void setOpenModelMessage(String newModelName) {
    modelName = newModelName;
    sendArguments.add(MESSAGE_OPEN_MODEL);
    sendArguments.add(newModelName);
  }

  /**
   * Open a new model file
   */
  public void openModel(String newModelName) throws IOException, SystemProcessException {
    modelName = newModelName;
    String[] args = new String[2];
    args[0] = MESSAGE_OPEN_MODEL;
    args[1] = newModelName;
    send(args);
  }

  /**
   * Places arguments to open a model and preserve contrast on the argument
   * list.
   * 
   * @param newModelName
   */
  public void setOpenModelPreserveContrastMessage(String newModelName) {
    sendArguments.add(MESSAGE_OPEN_KEEP_BW);
    sendArguments.add(newModelName);
  }

  /**
   * Open a new model file, Preserve the constrast settings
   */
  public void openModelPreserveContrast(String newModelName) throws IOException,
      SystemProcessException {
    String[] args = new String[2];
    args[0] = MESSAGE_OPEN_KEEP_BW;
    args[1] = newModelName;
    send(args);
  }

  /**
   * Save the current model file
   */
  public void saveModel() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_SAVE_MODEL;
    send(args);
  }

  /**
   * View the current model file
   */
  public void viewModel() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_VIEW_MODEL;
    send(args);
  }

  /**
   * Adds a message which sets new contours to be open Message description: 12 0
   * 1 1 7 0 12 says to do it to a new (empty) contour only (11 would be
   * unconditional) 0 is for object 1 1 sets it to open 1 sets it to display
   * circles 7 makes circle size be 7 0 keeps 3D size at 0
   */
  public void setNewContoursMessage(boolean open) {
    setNewObjectMessage(0, open, CIRCLE, 7, 0);
  }

  public void setPointLimitMessage(int pointLimit) {
    setMoreObjectPropertiesMessage(1, pointLimit, -1, -1);
  }

  public void setStartNewContoursAtNewZ() {
    setMoreObjectPropertiesMessage(1, -1, 1, -1);
  }

  public void setInterpolation(final boolean input) {
    sendArguments.add(MESSAGE_INTERPOLATION);
    sendArguments.add(input ? TRUE : FALSE);
  }

  public void openSurfContPoint() {
    sendArguments.add(MESSAGE_OPEN_DIALOG);
    sendArguments.add(SURF_CONT_POINT_DIALOG);
  }

  /**
   * 
   * @param object
   * @param open
   * @param symbol
   * @param size
   * @param size3D
   */
  public void setNewObjectMessage(int object, boolean open, int symbol, int size,
      int size3D) {
    sendArguments.add(MESSAGE_NEWOBJ_PROPERTIES);
    sendArguments.add(String.valueOf(object));
    sendArguments.add(open ? TRUE : FALSE);
    sendArguments.add(String.valueOf(symbol));
    sendArguments.add(String.valueOf(size));
    sendArguments.add(String.valueOf(size3D));
  }

  public void setMoreObjectPropertiesMessage(int object, int pointLimit,
      int newContourInNewZ, int sphereInCentralOnly) {
    sendArguments.add(MESSAGE_MORE_OBJ_PROPERTIES);
    sendArguments.add(String.valueOf(object));
    sendArguments.add(String.valueOf(pointLimit));
    sendArguments.add(String.valueOf(newContourInNewZ));
    sendArguments.add(String.valueOf(sphereInCentralOnly));
  }

  /**
   * Places arguments to set model mode on the argument list.
   */
  public void setModelModeMessage() {
    sendArguments.add(MESSAGE_MODEL_MODE);
    sendArguments.add("1");
  }

  /**
   * Switch the 3dmod process to model mode
   */
  public void modelMode() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_MODEL_MODE;
    send(args);
  }

  /**
   * Places arguments to set movie mode on the argument list.
   */
  public void setMovieModeMessage() {
    sendArguments.add(MESSAGE_MODEL_MODE);
    sendArguments.add("0");
  }

  /**
   * Switch the 3dmod process to movie mode
   */
  public void movieMode() throws IOException, SystemProcessException {
    String[] args = new String[2];
    args[0] = MESSAGE_MODEL_MODE;
    args[1] = "0";
    send(args);
  }

  /**
   * Places arguments to raise 3dmod on the argument list.
   */
  public void setRaise3dmodMessage() {
    sendArguments.add(MESSAGE_RAISE);
  }

  /**
   * Raise the 3dmod window
   * 
   * @throws IOException
   */
  public void raise3dmod() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_RAISE;
    send(args);
  }

  /**
   * Places arguments to open one zap window and raise 3dmod on the argument
   * list.
   */
  public void setOpenZapWindowMessage() {
    sendArguments.add(MESSAGE_ONE_ZAP_OPEN);
  }

  /**
   * Open one zap window and raise 3dmod.
   */
  public void openZapWindow() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_ONE_ZAP_OPEN;
    send(args);
  }

  /**
   * Places arguments to open the beadfixer dialog on the argument list.
   */
  public void setOpenBeadFixerMessage() {
    sendArguments.add(MESSAGE_OPEN_BEADFIXER);
  }

  public void setOpenModelView() {
    sendArguments.add(MESSAGE_OPEN_MODEL_VIEW);
  }

  public void setSkipList(String skipList) {
    if (skipList != null) {
      addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_SKIP_LIST, skipList);
    }
    else {
      addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_REMOVE_SKIP_LIST);
    }
  }

  public void setDeleteAllSections(boolean on) {
    if (on) {
      addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_DELETE_ALL_SECTIONS, TRUE);
    }
    else {
      addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_DELETE_ALL_SECTIONS, FALSE);
    }
  }

  public void setBeadfixerDiameter(int beadfixerDiameter) {
    beadfixerDiameterSet = true;
    addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_DIAMETER,
        String.valueOf(beadfixerDiameter));
  }

  public void setAutoCenter(boolean autoCenter) {
    addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_AUTO_CENTER, autoCenter ? MESSAGE_ON
        : MESSAGE_OFF);
    if (!beadfixerDiameterSet) {
      addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_DIAMETER,
          String.valueOf(ImodManager.DEFAULT_BEADFIXER_DIAMETER));
    }
  }

  public void setNewContours(boolean newContours) {
    addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_NEW_CONTOURS, newContours ? MESSAGE_ON
        : MESSAGE_OFF);
  }

  public void setBeadfixerMode(BeadFixerMode beadfixerMode) {
    addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_MODE, beadfixerMode.getValue());
  }

  public void reopenLog() throws IOException, SystemProcessException {
    sendPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_REREAD_LOG);
  }

  public void setOpenLog(String logName) {
    addPluginMessage(BEAD_FIXER_PLUGIN, BF_MESSAGE_OPEN_LOG, logName);
  }

  /**
   * Open the beadfixer dialog
   * 
   * @throws IOException
   */
  public void openBeadFixer() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_OPEN_BEADFIXER;
    send(args);
  }

  /**
   * Sends message requesting rubberband coordinates. Should not be used with
   * sendMessages().
   * 
   * @return rubberband coordinates and error messages
   * @throws IOException
   */
  public Vector getRubberbandCoordinates() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_RUBBERBAND;
    return request(args);
  }

  public Vector getSlicerAngles() throws IOException, SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_SLICER_ANGLES;
    return request(args);
  }

  private void sendPluginMessage(String plugin, String message) throws IOException,
      SystemProcessException {
    send(new String[] { MESSAGE_PLUGIN_MESSAGE, plugin, message });
  }

  private void addPluginMessage(String plugin, String message, String value) {
    sendArguments.add(MESSAGE_PLUGIN_MESSAGE);
    sendArguments.add(plugin);
    sendArguments.add(message);
    sendArguments.add(value);
  }

  private void addPluginMessage(String plugin, String message) {
    sendArguments.add(MESSAGE_PLUGIN_MESSAGE);
    sendArguments.add(plugin);
    sendArguments.add(message);
  }

  AxisID getAxisID() {
    return axisID;
  }

  /**
   * Sends all messages collected in the argument list via imodSendEvent().
   * Clears the argument list.
   * 
   * @throws IOException
   */
  public void sendMessages() throws IOException, SystemProcessException {
    if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
      System.err.println("sendMessages");
    }
    if (sendArguments.size() == 0) {
      return;
    }
    if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
      System.err.print("sendArguments: ");
      for (int i = 0; i < sendArguments.size(); i++) {
        System.err.print(sendArguments.get(i) + " ");
      }
      System.err.println();
    }
    String[] argArray = (String[]) sendArguments
        .toArray(new String[sendArguments.size()]);
    if (!listenToStdin) {
      imodSendEvent(argArray);
    }
    else {
      sendCommands(argArray);
    }
    sendArguments.clear();

    // The 3dmod process may have started without a continuous listener target.
    // If a target has been added and the continuous listener thread is not
    // running, start the continuous listener thread.
    if (isRunning() && continuousListenerTarget != null && !continuousListener.isAlive()) {
      continuousListener.startThread(imodThread, continuousListenerTarget);
    }
  }

  private void send(String[] args) throws IOException, SystemProcessException {
    if (!listenToStdin) {
      imodSendEvent(args);
    }
    else {
      sendCommands(args);
    }
  }

  private Vector request(String[] args) throws IOException, SystemProcessException {
    if (!listenToStdin) {
      return imodSendAndReceive(args);
    }
    else {
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("using stdin");
      }
      return sendRequest(args);
    }
  }

  /**
   * Sends a message and then records the results found in the error stream.
   * @param args
   * @return
   * @throws SystemProcessException
   */
  protected Vector imodSendAndReceive(String[] args) throws SystemProcessException {
    Vector results = new Vector();
    if (!isRunning()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
          "3dmod Warning", axisID);
      return null;
    }
    imodSendEvent(args, results);
    // 3dmod sends the results before it returns
    // the exit value to imodSendEvent - no waiting
    if (imod == null) {
      return results;
    }
    String line;
    line = imod.readStderr();
    if (line == null) {
      return results;
    }
    // Currently assuming results can only be on one line.
    boolean foundError = false;
    do {
      if (!parseError(line, results)) {
        String[] words = line.split("\\s+");
        for (int i = 0; i < words.length; i++) {
          results.add(words[i]);
        }
      }
      else {
        foundError = true;
      }
    } while ((line = imod.readStderr()) != null);
    if (foundError) {
      UIHarness.INSTANCE.openMessageDialog(manager, results.toString(), "3dmod Message",
          getAxisID());
    }
    return results;
  }

  protected boolean parseError(String line, Vector errorMessage) {
    // Currently assuming that an error or warning message will be only one
    // line and contain ERROR_STRING or WARNING_STRING.
    int index = ProcessMessages.getErrorIndex(line);
    if (index != -1) {
      errorMessage.add(line.substring(index));
      return true;
    }
    index = line.indexOf(ProcessMessages.WARNING_TAG);
    if (index != -1) {
      errorMessage.add(line.substring(index));
      return true;
    }
    return false;
  }

  private void imodSendEvent(String[] args) throws SystemProcessException {
    imodSendEvent(args, null);
  }

  /**
   * Send an event to 3dmod using the imodsendevent command.
   * Synchronized on stderr.quickListenerQueue to keep other threads from
   * from causing a response to appear on this queue before this thread can read
   * the response it generates.
   */
  private void imodSendEvent(String[] args, Vector messages)
      throws SystemProcessException {
    if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
      System.err.println("using imodsendevent");
    }
    synchronized (stderr.quickListenerQueue) {
      if (windowID.equals("")) {
        throw (new SystemProcessException("No window ID available for imod"));
      }
      String[] command = new String[2 + args.length];
      command[0] = ApplicationManager.getIMODBinPath() + "imodsendevent";
      if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
        System.err.println("imodSendEvent:windowID:" + windowID);
      }
      command[1] = windowID;
      // String command = ApplicationManager.getIMODBinPath() + "imodsendevent "
      // + windowID + " ";
      for (int i = 0; i < args.length; i++) {
        command[i + 2] = args[i];
      }
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.print(command);
      }
      InteractiveSystemProgram imodSendEvent = new InteractiveSystemProgram(manager,
          command, axisID);

      // Start the imodSendEvent program thread and wait for it to finish
      Thread sendEventThread = new Thread(imodSendEvent);
      sendEventThread.start();
      try {
        sendEventThread.join();
      }
      catch (Exception except) {
        except.printStackTrace();
      }
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("...done");
      }

      // Check imodSendEvent's exit code, if it is not zero read in the
      // stderr/stdout stream and throw an exception describing why the file
      // was not loaded
      if (imodSendEvent.getExitValue() != 0) {

        String message = IMOD_SEND_EVENT_STRING + " "
            + String.valueOf(imodSendEvent.getExitValue()) + "\n";

        String line = imodSendEvent.readStderr();
        while (line != null) {
          message = message + "stderr: " + line + "\n";
          line = imodSendEvent.readStderr();
        }
        line = imodSendEvent.readStdout();
        while (line != null) {
          message = message + "stdout: " + line + "\n";
          line = imodSendEvent.readStdout();
        }

        if (messages == null) {
          System.err.println(message);
        }
        else {
          messages.add(message);
        }
      }
    }
  }

  /**
   * Sends a request to 3dmod's stdin and returns the results. Pops up error and
   * warning messages from 3dmod that are directed at the user.  Synchronized on
   * stderr.quickListenerQueue to keep other threads from
   * from causing a response to appear on this queue before this thread can read
   * the response it generates.
   * 
   * @param args -
   *          commands.
   * @return - vector with values received from 3dmod.
   * @throws IOException
   */
  private Vector sendRequest(String[] args) throws IOException {
    synchronized (stderr.quickListenerQueue) {
      Vector imodReturnValues = new Vector();
      sendCommands(args, imodReturnValues, true);
      return imodReturnValues;
    }
  }

  /**
   * Sends commands to 3dmod's stdin and process the results. Pops up error and
   * warning messages from 3dmod that are directed at the user.  Synchronized on
   * stderr.quickListenerQueue to keep other threads from
   * from causing a response to appear on this queue before this thread can read
   * the response it generates.
   * @param args -
   *          commands.
   * @throws IOException
   *           messages are received.
   */
  private void sendCommands(String[] args) throws IOException {
    synchronized (stderr.quickListenerQueue) {
      sendCommands(args, null, true);
    }
  }

  private void sendCommandsNoWait(String[] args) throws IOException {
    sendCommands(args, null, false);
  }

  /**
   * Sends commands to 3dmod's stdin and process the results. Pops up error and
   * warning messages from 3dmod that are directed at the user.
   * 
   * @param args -
   *          commands.
   * @param imodReturnValues -
   *          optional return value vector to be used when expecting return
   *          values from 3dmod.
   * @throws IOException
   *           messages are received and imodReturnValues is null.
   */
  private void sendCommands(String[] args, Vector imodReturnValues, boolean readResponse)
      throws IOException {
    MessageSender messageSender = new MessageSender(args, imodReturnValues, readResponse);
    /* //patch for quicklistener shared queue problem try { Thread.sleep(200); } catch
     * (InterruptedException e) { } */
    if (imodReturnValues == null) {
      new Thread(messageSender).start();
    }
    else {
      // get return values
      messageSender.run();
    }
  }

  void processRequest() {
    if (isRequestReceived()) {
      try {
        disconnect();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  private boolean isRequestReceived() {
    if (stderr == null) {
      return false;
    }
    if (stderr.getRequestMessage() != null) {
      return true;
    }
    return false;
  }

  /**
   * Returns the datasetName.
   * 
   * @return String
   */
  public String getDatasetName() {
    return datasetName;
  }

  /**
   * Returns the modelName.
   * 
   * @return String
   */
  public String getModelName() {
    return modelName;
  }

  /**
   * Returns the windowID.
   * 
   * @return String
   */
  public String getWindowID() {
    if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
      System.err.println("getWindowID:windowID:" + windowID);
    }
    return windowID;
  }

  /**
   * Returns the swapYZ.
   * 
   * @return String
   */
  public boolean getSwapYZ() {
    return swapYZ;
  }

  /**
   * Returns the windowID.
   * 
   * @return String
   */
  public void setSwapYZ(boolean state) {
    swapYZ = state;
  }

  /**
   * @return boolean
   */
  public boolean isModelView() {
    return modelView;
  }

  /**
   * Sets the modelView.
   * 
   * @param modelView
   *          The modelView to set
   */
  public void setModelView(boolean modelView) {
    this.modelView = modelView;
  }

  public void setOpenZap() {
    openZap = true;
  }

  void setTiltFile(String input) {
    tiltFile = input;
  }

  void resetTiltFile() {
    tiltFile = null;
  }

  /**
   * @return
   */
  public boolean isUseModv() {
    return useModv;
  }

  /**
   * @param b
   */
  public void setUseModv(boolean b) {
    useModv = b;
  }

  /**
   * @return
   */
  public boolean isOutputWindowID() {
    return outputWindowID;
  }

  /**
   * @param b
   */
  public void setOutputWindowID(boolean b) {
    outputWindowID = b;
  }

  public void setDebug(boolean input) {
    debug = input;
  }

  public void setBinning(int binning) {
    if (binning < defaultBinning) {
      this.binning = defaultBinning;
    }
    else {
      this.binning = binning;
    }
  }

  public void setBinningXY(int binningXY) {
    if (binningXY < defaultBinning) {
      this.binningXY = defaultBinning;
    }
    else {
      this.binningXY = binningXY;
    }
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",datasetName=" + datasetName + ", modelName=" + modelName + ", windowID="
        + windowID + ", swapYZ=" + swapYZ + ", modelView=" + modelView + ", useModv="
        + useModv + ", outputWindowID=" + outputWindowID + ", binning=" + binning;
  }

  void addWindowOpenOption(WindowOpenOption option) {
    if (option.isImodv() && !modelView && !useModv) {
      System.err.println("WARNING:  Can't use 3dmod " + WindowOpenOption.OPTION
          + " with " + option.toString() + " because the Model View is not open.");
    }
    if (windowOpenOptionList == null) {
      windowOpenOptionList = new ArrayList();
    }
    windowOpenOptionList.add(option);
  }

  void setContinuousListenerTarget(ContinuousListenerTarget continuousListenerTarget) {
    this.continuousListenerTarget = continuousListenerTarget;
  }

  /**
   * Class to allow testing of the quick listener queue functionality in Stderr.
   * @author sueh
   *
   */
  static final class QuickListenerQueueTestWrapper {
    private final Stderr stderr = new Stderr();

    QuickListenerQueueTestWrapper() {
    }

    int getExpectedRegistrants() {
      return Stderr.EXPECTED_REGISTRANTS;
    }

    Integer register() {
      return stderr.register();
    }

    int getPurgeSize() {
      return Stderr.PURGE_SIZE;
    }

    void add(final String input) {
      stderr.quickListenerQueue.add(input);
    }

    public String toString() {
      return stderr.quickListenerQueue.toString();
    }

    String getQuickMessage(final Integer regId) {
      return stderr.getQuickMessage(regId);
    }

    void purge() {
      stderr.purgeQuickListenerQueue();
    }
  }

  /**
   * Class to get messages from the stderr and place them in queues.  This is
   * only way that imod.stderr should be accessed.
   * @author sueh
   *
   */
  private static final class Stderr {
    private static final int EXPECTED_REGISTRANTS = 2;
    private static final int PURGE_SIZE = 10;

    /**
     * Contains an id and the last index used to read quickListenerQueue.
     */
    private final Map<Integer, EtomoNumber> registration = new HashMap<Integer, EtomoNumber>();
    /**
     * Queue to hold returned data that was requested by etomo, and also error
     * messages.  Also contains miscellaneous messages that can be ignored.
     */
    private final List<String> quickListenerQueue = new ArrayList<String>();

    /**
     * Queue to hold information from 3dmod.  These messages are requested by
     * etomo but do not arrive instantly.
     */
    private final Queue continuousListenerQueue = new LinkedList();

    /**
     * Queue to hold requests from 3dmod.  3dmod chooses when to send these
     * messages.
     */
    private final Queue requestQueue = new LinkedList();

    private InteractiveSystemProgram imod = null;
    private boolean receivedInterruptedException = false;
    private int regId = -1;

    private Stderr() {
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("Stderr:" + this);
      }
    }

    private void setImod(InteractiveSystemProgram imod) {
      this.imod = imod;
    }

    /**
    * Creates a new id and adds it to registration.  Returns the id.
    * @return
    */
    private synchronized Integer register() {
      Integer id = new Integer(++regId);
      EtomoNumber index = new EtomoNumber();
      index.set(-1);
      registration.put(id, index);
      return id;
    }

    /**
     * Returns one line from the quickListenerQueue, or null if
     * the queue is empty.
     * @return
     */
    private synchronized String getQuickMessage(final Integer regId) {
      int queueSize = quickListenerQueue.size();
      readStderr();
      EtomoNumber index = registration.get(regId);
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("regId:" + regId + ",index:" + index);
      }
      // Read a string from the queue, if there is anything left to read.
      if (index.lt(quickListenerQueue.size() - 1)) {
        // Increment the index.
        index.add(1);
        return quickListenerQueue.get(index.getInt());
      }
      return null;
    }

    /**
     * Don't let the quick listener queue grow too big.  Make sure that all interested
     * parties are registered before doing any purging.  This function is not efficient at
     * all but it isn't likely to be used very much.  To make it efficient, make in the
     * quick listener queue a real link list.
     * much.
     */
    private synchronized void purgeQuickListenerQueue() {
      if (registration.size() < EXPECTED_REGISTRANTS
          || quickListenerQueue.size() < PURGE_SIZE) {
        return;
      }
      Iterator<Map.Entry<Integer, EtomoNumber>> i = registration.entrySet().iterator();
      // Get the lowest index, which is the last string that all the registrants have
      // read.
      int readByAllIndex = -1;
      if (i.hasNext()) {
        readByAllIndex = i.next().getValue().getInt();
        while (i.hasNext()) {
          readByAllIndex = Math.min(readByAllIndex, i.next().getValue().getInt());
        }
      }
      // Purging is expensive - decide if its worth purging.
      if (readByAllIndex >= PURGE_SIZE / 2) {
        for (int j = 0; j <= readByAllIndex; j++) {
          quickListenerQueue.remove(0);
        }
        // Now all the indexes are wrong - fix them.
        i = registration.entrySet().iterator();
        // Get the lowest index, which is the last string that all the registrants have
        // read.
        while (i.hasNext()) {
          EtomoNumber index = i.next().getValue();
          // Reduce the saved indices by the number of elements that where removed.
          index.set(index.getInt() - readByAllIndex - 1);
        }
      }
    }

    /**
     * Removes and returns one message from the quickListenerQueue, or null if
     * the queue is empty.  Assuming that only one continuous listener exists
     * per 3dmod instance.
     * @return
     */
    private String getContinuousMessage() {
      readStderr();
      return (String) continuousListenerQueue.poll();
    }

    /**
     * Removes and returns one message from the requestQueue, or null if
     * the queue is empty.
     * @return
     */
    private String getRequestMessage() {
      readStderr();
      return (String) requestQueue.poll();
    }

    /**
     * Sleeps and then moves all stderr messages found into a queue.  Messages
     * that start with REQUEST_TAG go to the requeueQueue.  Messages that start
     * with CONTINOUS_TAG go to the continuousListenerQueue.  All other messages
     * go to the quickListenerQueue.
     * This function should only be called by Stderr functions.
     */
    private synchronized void readStderr() {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
        receivedInterruptedException = true;
      }
      if (imod == null) {
        return;
      }
      String message;
      while ((message = imod.readStderr()) != null) {
        if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
          System.err.println("stderr:" + message);
        }
        if (message.startsWith(REQUEST_TAG)
            && message.indexOf(STOP_LISTENING_REQUEST) != -1) {
          requestQueue.add(message);
        }
        else if (message.startsWith(CONTINUOUS_TAG)) {
          continuousListenerQueue.add(message);
        }
        else {
          quickListenerQueue.add(message);
          purgeQuickListenerQueue();
        }
      }
    }
  }

  private static class ContinuousListener implements Runnable {
    private final Stderr stderr;

    private final AxisID axisID;

    private Thread imodThread = null;

    private Thread continuousListenerThread = null;

    private ContinuousListenerTarget target = null;

    private ContinuousListener(Stderr stderr, AxisID axisID) {
      this.stderr = stderr;
      this.axisID = axisID;
    }

    /**
     * Set imodThread and target and run the run() function on a separate
     * thread.
     * @param imodThread
     */
    private synchronized void startThread(Thread imodThread,
        ContinuousListenerTarget continuousListenerTarget) {
      this.imodThread = imodThread;
      target = continuousListenerTarget;
      // If thread has ended create and start a new thread
      if (continuousListenerThread == null || !continuousListenerThread.isAlive()) {
        continuousListenerThread = new Thread(this);
        continuousListenerThread.start();
      }
    }

    private boolean isAlive() {
      return continuousListenerThread != null && continuousListenerThread.isAlive();
    }

    /**
     * Check stderr.continuousListenerQueue until imodThread is no longer alive
     * or an interrupted exception is received.
     */
    public synchronized void run() {
      try {
        if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
          System.err
              .println("ContinuousListener:run " + Utilities.getDateTimeStamp(true));
        }
        do {
          Thread.sleep(500);
          String message = stderr.getContinuousMessage();
          if (message != null && target != null) {
            target.getContinuousMessage(message, axisID);
          }
        } while (imodThread != null && imodThread.isAlive());
      }
      catch (InterruptedException e) {
      }
    }
  }

  /**
   * Class to send a message to 3dmod. Can be run on a separate thread to avoid
   * locking up the GUI.
   */
  private class MessageSender implements Runnable {
    private final String[] args;

    private final Vector imodReturnValues;

    private boolean readResponse = true;

    private MessageSender(String[] args, Vector imodReturnValues, boolean readResponse) {
      this.imodReturnValues = imodReturnValues;
      this.args = args;
      this.readResponse = readResponse;
    }

    /**
     * Send the message and wait for a response.
     */
    public void run() {
      // make sure that 3dmod is running
      if (imod == null) {
        if (imodReturnValues != null) {
          // unable to get return values
          UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
              "3dmod Warning", getAxisID());
        }
        return;
      }
      // boolean responseReceived = false;
      // build a string to send
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < args.length; i++) {
        buffer.append(args[i] + " ");
      }
      if (buffer.length() > 0) {
        try {
          if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
            System.err.println("MessageSender:" + this + "," + buffer.toString());

          }
          // send the string to 3dmod's stdin
          if (!isRunning()) {
            if (imodReturnValues != null) {
              // unable to get return values
              UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
                  "3dmod Warning", getAxisID());
            }
            return;
          }
          if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
            System.err.println("ImodProcess:MessageSender:run:Setting stdin "
                + Utilities.getDateTimeStamp(true));
          }
          imod.setCurrentStdInput(buffer.toString());
        }
        catch (IOException exception) {
          // make sure that 3dmod is running
          if (exception.getMessage().toLowerCase().indexOf("broken pipe") != -1) {
            if (imodReturnValues != null) {
              // unable to get return values
              UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
                  "3dmod Warning", getAxisID());
            }
            return;
          }
          else {
            exception.printStackTrace();
            UIHarness.INSTANCE.openMessageDialog(manager, exception.getMessage(),
                "3dmod Exception", getAxisID());
          }
        }
      }
      if (readResponse) {
        // read the response from 3dmod
        readResponse();
      }
    }

    /**
     * Wait for a response to the message and pop up a message if there is a
     * problem.
     */
    public void readResponse() {
      if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
        System.err.println("MessageSender:" + this + ",readResponse");
      }
      boolean responseReceived = false;
      String response = null;
      StringBuffer userMessage = new StringBuffer();
      // wait for the response for at most 5 seconds
      if (EtomoDirector.INSTANCE.getArguments().getDebugLevel().isExtraVerbose()) {
        System.err.println("ImodProcess:MessageSender:readResponse "
            + Utilities.getDateTimeStamp(true));
      }
      for (int timeout = 0; timeout < 30; timeout++) {
        if (responseReceived) {
          if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
            System.err.println("MessageSender:" + this + ",responseReceived");
          }
          break;
        }
        // process response
        boolean failure = false;
        while ((response = stderr.getQuickMessage(messageSenderRegId)) != null) {
          responseReceived = true;
          if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
            System.err.println("MessageSender:" + this + "," + response);
          }
          response = response.trim();
          if (response.equals("OK")) {
            // OK is sent last, so this is done
            break;
          }
          // if the response is not OK or an error message meant for the user
          // then it may be a requested return string. Otherwise it is some
          // 3dmod output that etomo can ignore.
          if (!parseUserMessages(response, userMessage) && imodReturnValues != null
              && !failure && !response.startsWith("imodExecuteMessage:")) {
            String[] words = response.split("\\s+");
            for (int i = 0; i < words.length; i++) {
              imodReturnValues.add(words[i]);
            }
          }
        }
      }
      // pop up error and warning messages for the user
      if (userMessage.length() > 0) {
        UIHarness.INSTANCE.openMessageDialog(manager, userMessage.toString(),
            "3dmod Message", getAxisID());
      }
      if (!responseReceived) {
        if (isRunning()) {
          if (EtomoDirector.INSTANCE.getArguments().isDebug()
              && stderr.receivedInterruptedException) {
            System.err.println("\nsleep interrupted");
          }
          // no response received and 3dmod is running - "throw" exception
          SystemProcessException exception = new SystemProcessException("MessageSender:"
              + this + ",No response received from 3dmod.  datasetName=" + datasetName
              + ",modelName=" + modelName + ",workingDirectory=" + workingDirectory
              + ",axisID=" + axisID);
          exception.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog(manager, exception.getMessage(),
              "3dmod Exception", getAxisID());
        }
        else if (imodReturnValues != null) {
          // unable to get return values
          UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
              "3dmod Warning", getAxisID());
        }
      }
    }

    /**
     * Parse messages that are directed at the user - messages that contain
     * ERROR_TAG or WARNING_TAG.
     * 
     * @param line
     * @param userMessages
     * @return true if an error or warning is found
     */
    private boolean parseUserMessages(String line, StringBuffer userMessages) {
      // Currently assuming that each user error or warning messages will be
      // only one
      // line and contain ERROR_STRING or WARNING_STRING.
      int index = ProcessMessages.getErrorIndex(line);
      if (index != -1) {
        userMessages.append(line + "\n");
        return true;
      }
      index = line.indexOf(ProcessMessages.WARNING_TAG);
      if (index != -1) {
        userMessages.append(line + "\n");
        return true;
      }
      return false;
    }
  }

  static class WindowOpenOption {
    static final String OPTION = "-E";

    static final WindowOpenOption IMODV_OBJECTS = new WindowOpenOption("O", true);
    static final WindowOpenOption ISOSURFACE = new WindowOpenOption("U", true);
    static final WindowOpenOption OBJECT_LIST = new WindowOpenOption("L", true);
    static final WindowOpenOption MODEL_EDIT = new WindowOpenOption("M", true);

    private final String windowKey;

    private final boolean imodv;

    private WindowOpenOption(String windowKey, boolean imodv) {
      this.windowKey = windowKey;
      this.imodv = imodv;
    }

    public String toString() {
      return windowKey;
    }

    boolean isImodv() {
      return imodv;
    }
  }

  public static final class BeadFixerMode {

    public static final BeadFixerMode SEED_MODE = new BeadFixerMode("0");

    public static final BeadFixerMode GAP_MODE = new BeadFixerMode("1");

    public static final BeadFixerMode RESIDUAL_MODE = new BeadFixerMode("2");

    public static final BeadFixerMode PATCH_TRACKING_RESIDUAL_MODE = new BeadFixerMode(
        "3");

    private final String value;

    private BeadFixerMode(String value) {
      this.value = value;
    }

    private String getValue() {
      return value;
    }
  }
}