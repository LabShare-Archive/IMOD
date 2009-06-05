package etomo.comscript;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JOptionPane;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

/**
 * <p>Description: This class provides a high level manager for loading and
 * saving particlar com scripts and extracting the parameter sets for the 
 * commands within those scripts.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.54  2009/03/17 00:31:16  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.53  2008/10/27 17:43:58  sueh
 * <p> bug# 1141 Added ctfCorrection and ctfPlotter scripts.
 * <p>
 * <p> Revision 3.52  2007/12/10 21:54:28  sueh
 * <p> bug# 1041 Formatted.
 * <p>
 * <p> Revision 3.51  2007/09/07 00:17:34  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.50  2007/03/07 20:58:48  sueh
 * <p> bug# 981 Passing manager to TiltalignParam().
 * <p>
 * <p> Revision 3.49  2007/02/05 21:36:13  sueh
 * <p> bug# 962 Put comscript mode info into an inner class.
 * <p>
 * <p> Revision 3.48  2006/10/24 21:16:48  sueh
 * <p> bug# 947 Changed ProcessName.fromString() to getInstance().
 * <p>
 * <p> Revision 3.47  2006/10/13 22:22:39  sueh
 * <p> bug# 927 Added parameters String and int to getSetParamFromVolcombine().
 * <p> Added getSetParamFromVolcombine(String,int,String) to get the second set
 * <p> command in volcombine.  Added the ability to initialize an optional command
 * <p> using the previous command.  Added a new modifyCommand(), which can
 * <p> modify a command based on a previous command.
 * <p>
 * <p> Revision 3.46  2006/09/19 21:55:05  sueh
 * <p> bug# 920 Added a global key for saving param values in data files.
 * <p>
 * <p> Revision 3.45  2006/09/05 17:33:45  sueh
 * <p> bug# 917 Added scriptMatchvol1, getMatchvolParam(), loadMatchvol1(), and
 * <p> saveMatchvol().
 * <p>
 * <p> Revision 3.44  2006/08/25 22:42:30  sueh
 * <p> bug# 918 Changed updateComScript functions to modifyCommand and addModifyCommand to reduce the number of boolean parameters.  Added
 * <p> deleteCommand and modifyOptionalCommand.  Deleting the old patchcrawl
 * <p> script and added the new one.
 * <p>
 * <p> Revision 3.43  2006/08/14 22:22:14  sueh
 * <p> Improved the pop up error messages.
 * <p>
 * <p> Revision 3.42  2006/06/05 16:05:12  sueh
 * <p> bug# 766 In ProcessName:  Changed getCommand() and getCommandArray() to
 * <p> getComscript... because the fuctions are specialized for comscripts.
 * <p>
 * <p> Revision 3.41  2006/05/16 21:21:41  sueh
 * <p> bug# 856 Passing the BaseManager to SolvematchParam so that it can get the
 * <p> fiducial model.
 * <p>
 * <p> Revision 3.40  2006/01/26 21:47:52  sueh
 * <p> Added spaces to an error message.
 * <p>
 * <p> Revision 3.39  2005/09/01 17:45:45  sueh
 * <p> bug# 688 putting temporary prints (for finding cause of undistort
 * <p> parameters being set in xcorr) into the error log
 * <p>
 * <p> Revision 3.38  2005/08/27 22:17:01  sueh
 * <p> bug# 532 changed Utilities.timestamp to take a string status instead of
 * <p> integer
 * <p>
 * <p> Revision 3.37  2005/08/25 01:45:18  sueh
 * <p> removed print statement
 * <p>
 * <p> Revision 3.36  2005/08/24 22:32:12  sueh
 * <p> bug# 715 Added loadComScript(ProcessName...) to work with
 * <p> BlendmontParam, which uses getProcessName() instead of
 * <p> getCommandFileName().
 * <p>
 * <p> Revision 3.35  2005/07/29 00:43:19  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.34  2005/06/17 20:03:06  sueh
 * <p> bug# 685 Added timestamp functions for ComScript and File types.
 * <p> Added code to the main timestamp function to strip the path from a file
 * <p> name.  These changes reduces the amount of timestamp related code
 * <p> being executed when debug is off.
 * <p>
 * <p> Revision 3.33  2005/06/17 00:32:14  sueh
 * <p> bug# 685 Added timestamp to deleteCommand(), initialize(),
 * <p> loadComScript(), updateComScript(), and useTemplate().
 * <p>
 * <p> Revision 3.32  2005/05/17 19:14:57  sueh
 * <p> bug# 658 Passing axisID to BeadtrackParam constructor.
 * <p>
 * <p> Revision 3.31  2005/04/26 17:36:13  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 3.30  2005/04/25 20:37:49  sueh
 * <p> bug# 615 Passing the axis where the command originated to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 3.29  2005/04/13 20:32:13  sueh
 * <p> bug# 633 put updateComScript call back into savePrenewst().
 * <p>
 * <p> Revision 3.28  2005/04/07 21:50:48  sueh
 * <p> bug# 626 Added undistort script.  This script is contains a blendmont
 * <p> command which is loaded from xcorr, updated, and then written into
 * <p> undistort.com.
 * <p>
 * <p> Revision 3.27  2005/03/11 01:33:03  sueh
 * <p> bug# 533 printing a stack trace for an error in updateComScript.
 * <p>
 * <p> Revision 3.26  2005/03/09 18:00:35  sueh
 * <p> bug# 533 Added the blend script.
 * <p>
 * <p> Revision 3.25  2005/03/08 01:53:37  sueh
 * <p> bug# 533 Added preblend script.
 * <p>
 * <p> Revision 3.24  2005/03/04 00:08:37  sueh
 * <p> bug# 533 Added getBlendmontParamFromTiltxcorr(),
 * <p> getGotoParamFromTiltxcorr(), saveXcorr(BlendmontParam),
 * <p> saveXcorr(GotoParam).
 * <p>
 * <p> Revision 3.23  2005/02/23 18:48:09  sueh
 * <p> bug# 607 Fix problem: Etomo unable to exit because of null pointer
 * <p> exception.
 * <p>
 * <p> Revision 3.22  2005/01/29 00:17:58  sueh
 * <p> Checking for null values in initialize() to prevent null value exception on
 * <p> exit.
 * <p>
 * <p> Revision 3.21  2005/01/08 01:28:20  sueh
 * <p> bug# 578 Passing axisID to NewstParam constructor.  Passing dataset
 * <p> name and axisID to TiltParam constructor.
 * <p>
 * <p> Revision 3.20  2005/01/06 17:55:40  sueh
 * <p> bug# 567 Passing dataset name to TiltalignParam constructor to build the
 * <p> zFactorFile name without making TiltalignParam depend on one type of
 * <p> manager.
 * <p>
 * <p> Revision 3.19  2005/01/05 18:54:51  sueh
 * <p> bug# 578 Create tiltalignParam with axisID.
 * <p>
 * <p> Revision 3.18  2004/12/03 20:19:45  sueh
 * <p> bug# 556 SetupParam may be missing in volcombine.com in older .com
 * <p> scripts.
 * <p>
 * <p> Revision 3.17  2004/11/30 00:33:11  sueh
 * <p> bug# 556 Adding functions to parse volcombine.com.
 * <p>
 * <p> Revision 3.16  2004/11/19 22:42:19  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.15.2.3  2004/10/11 02:00:21  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p>
 * <p> Revision 3.15.2.2  2004/10/08 15:45:42  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.15.2.1  2004/09/15 22:34:51  sueh
 * <p> bug# 520 call openMessageDialog in mainPanel instead of mainFrame
 * <p>
 * <p> Revision 3.15  2004/08/19 01:25:44  sueh
 * <p> Added functions to get a CombineComscriptState.  Added ComScript
 * <p> combine.  Added functions for echo, exit, and goto in the combine
 * <p> script.  Added new general functions to add a command to a script
 * <p> based on the location of the previous command or command index.
 * <p> Added a general function to delete a command based on the location of
 * <p> the previous command.  Added general initialization functions for
 * <p> optional command and for commands that must be located by
 * <p> specifying the previous command.  Added a second useTemplate
 * <p> command for the simpler case where the .com file did not change.
 * <p> Added:
 * <p> ComScript scriptCombine
 * <p> deleteCommand(ComScript script, String command, AxisID axisID,
 * <p>     String previousCommand)
 * <p> deleteFromCombine(String command, String previousCommand)
 * <p> getCombineComscript()
 * <p> getEchoParamFromCombine(String previousCommand)
 * <p> getGotoParamFromCombine()
 * <p> initialize(CommandParam param, ComScript comScript,
 * <p>     String command, AxisID axisID, boolean optionalCommand)
 * <p> initialize(CommandParam param, ComScript comScript,
 * <p>     String command, AxisID axisID, String previousCommand)
 * <p> loadCombine()
 * <p> saveCombine(EchoParam echoParam, String previousCommand)
 * <p> saveCombine(ExitParam exitParam, int previousCommandIndex)
 * <p> saveCombine(GotoParam gotoParam, int previousCommandIndex)
 * <p> saveCombine(GotoParam gotoParam)
 * <p> updateComScript(ComScript script, CommandParam params,
 * <p>     String command, AxisID axisID, boolean addNew,
 * <p>    int previousCommandIndex)
 * <p> updateComScript(ComScript script, CommandParam params,
 * <p>     String command, AxisID axisID, boolean addNew,
 * <p>     String previousCommand)
 * <p> useTemplate(String scriptName, AxisType axisType, AxisID axisID,
 * <p>     boolean rename)
 * <p> Changed:
 * <p> initialize(CommandParam param, ComScript comScript,
 * <p>     String command, AxisID axisID)
 * <p>
 * <p> Revision 3.14  2004/06/24 18:36:20  sueh
 * <p> bug# 482 Removing proof-of-concept test code.  Add functions
 * <p> to retrieve matchshifts from solvematchshift and add it to
 * <p> solvematch.
 * <p>
 * <p> Revision 3.13  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.12  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.11  2004/05/05 19:15:05  sueh
 * <p> param test - proof of concept
 * <p>
 * <p> Revision 3.10  2004/05/03 17:59:36  sueh
 * <p> param testing proof of concept
 * <p>
 * <p> Revision 3.9  2004/04/27 00:50:16  sueh
 * <p> bug# 427 parse comments for tomopitch
 * <p>
 * <p> Revision 3.8  2004/04/26 21:09:50  sueh
 * <p> bug# 427 added tomopitch
 * <p>
 * <p> Revision 3.7  2004/04/19 19:24:46  sueh
 * <p> bug# 409 putting text back to pre-409, handling changes in
 * <p> ComScript
 * <p>
 * <p> Revision 3.6  2004/04/16 01:45:25  sueh
 * <p> bug# 409 changes for mtffilter where not working for newst - fixed
 * <p>
 * <p> Revision 3.5  2004/04/12 17:11:25  sueh
 * <p> bug# 409  In initialize() allow the param to initialize itself if necessary.  In update
 * <p> ComScript, get the commandIndex after running
 * <p> script.getScriptCommand(command) to make sure that the command exists in
 * <p> the ComScript object.
 * <p>
 * <p> Revision 3.4  2004/03/29 20:46:57  sueh
 * <p> bug# 409 add MTF Filter
 * <p>
 * <p> Revision 3.3  2004/03/13 00:30:49  rickg
 * <p> Bug# 390 Add prenewst and xfproduct management
 * <p>
 * <p> Revision 3.2  2004/03/12 00:04:10  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Handle newst or newstack commands the same way
 * <p>
 * <p> Revision 3.1  2004/03/04 00:46:54  rickg
 * <p> Bug# 406 Correctly write out command when it isn't the first in the
 * <p> script
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.9  2003/07/25 22:57:30  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.8  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.7  2003/06/23 23:28:32  rickg
 * <p> Return exception class name in error dialog
 * <p>
 * <p> Revision 2.6  2003/05/07 22:32:42  rickg
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.5  2003/03/27 00:27:49  rickg
 * <p> Fixed but in loading tilt with with respect to parsing comments.
 * <p>
 * <p> Revision 2.4  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.3  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/09 00:00:34  rickg
 * <p> Fixed formatting due to emacs
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ComScriptManager {
  public static final String rcsid = "$Id$";

  static final String PARAM_KEY = "Param";

  private final ApplicationManager appManager;
  UIHarness uiHarness = UIHarness.INSTANCE;

  public static final String MTFFILTER_COMMAND = "mtffilter";

  private ComScript scriptEraserA;
  private ComScript scriptEraserB;
  private ComScript scriptXcorrA;
  private ComScript scriptXcorrB;
  private ComScript scriptPrenewstA;
  private ComScript scriptPrenewstB;
  private ComScript scriptTrackA;
  private ComScript scriptTrackB;
  private ComScript scriptAlignA;
  private ComScript scriptAlignB;
  private ComScript scriptNewstA;
  private ComScript scriptNewstB;
  private ComScript scriptTiltA;
  private ComScript scriptTiltB;
  private ComScript scriptMTFFilterA;
  private ComScript scriptMTFFilterB;
  private ComScript scriptPreblendA;
  private ComScript scriptPreblendB;
  private ComScript scriptBlendA;
  private ComScript scriptBlendB;
  private ComScript scriptUndistortA;
  private ComScript scriptUndistortB;
  private ComScript scriptMatchvol1;
  // The solvematch com script replaces the functionality of the
  // solvematchshift and solvematchmod com scripts
  private ComScript scriptSolvematch;
  private ComScript scriptSolvematchshift;
  private ComScript scriptSolvematchmod;
  private ComScript scriptPatchcorr;
  private ComScript scriptMatchorwarp;
  private ComScript scriptTomopitchA;
  private ComScript scriptTomopitchB;
  private ComScript scriptCombine;
  private ComScript scriptVolcombine;
  private ComScript scriptCtfCorrectionA;
  private ComScript scriptCtfCorrectionB;
  private ComScript scriptCtfPlotterA;
  private ComScript scriptCtfPlotterB;
  private ComScript scriptFlatten;

  public ComScriptManager(ApplicationManager appManager) {
    this.appManager = appManager;
  }

  /**
   * Load the specified eraser com script
   * @param axisID the AxisID to load.
   */
  public void loadEraser(AxisID axisID) {
    //  Assign the new ComScript object object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptEraserB = loadComScript("eraser", axisID, true);
    }
    else {
      scriptEraserA = loadComScript("eraser", axisID, true);
    }
  }

  /**
   * Get the CCD eraser parameters from the specified eraser script object
   * @param axisID the AxisID to read.
   * @return a CCDEraserParam object that will be created and initialized
   * with the input arguments from eraser in the com script.
   */
  public CCDEraserParam getCCDEraserParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript eraser;
    if (axisID == AxisID.SECOND) {
      eraser = scriptEraserB;
    }
    else {
      eraser = scriptEraserA;
    }

    // Initialize a CCDEraserParam object from the com script command object
    CCDEraserParam ccdEraserParam = new CCDEraserParam();
    initialize(ccdEraserParam, eraser, "ccderaser", axisID);
    return ccdEraserParam;
  }

  /**
   * Save the specified eraser com script updating the ccderaser parmaeters
   * @param axisID the AxisID to load.
   * @param ccdEraserParam a CCDEraserParam object containing the new input
   * parameters for the ccderaser command.
   */
  public void saveEraser(CCDEraserParam ccdEraserParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptEraser;
    if (axisID == AxisID.SECOND) {
      scriptEraser = scriptEraserB;
    }
    else {
      scriptEraser = scriptEraserA;
    }

    // update the ccderaser parameters
    modifyCommand(scriptEraser, ccdEraserParam, "ccderaser", axisID);
  }

  /**
   * Load the specified xcorr com script and initialize the TiltXcorrParam
   * object
   * @param axisID the AxisID to load.
   */
  public void loadXcorr(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptXcorrB = loadComScript("xcorr", axisID, true);
    }
    else {
      scriptXcorrA = loadComScript("xcorr", axisID, true);
    }
  }

  /**
   * load or create undistort.com
   * @param axisID
   */
  public void loadUndistort(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      scriptUndistortB = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.UNDISTORT), axisID, true);
    }
    else {
      scriptUndistortA = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.UNDISTORT), axisID, true);
    }
  }

  /**
   * Get the tiltxcorr parameters from the specified xcorr script object
   * @param axisID the AxisID to read.
   * @return a TiltxcorrParam object that will be created and initialized
   * with the input arguments from xcorr in the com script.
   */
  public TiltxcorrParam getTiltxcorrParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript xcorr;
    if (axisID == AxisID.SECOND) {
      xcorr = scriptXcorrB;
    }
    else {
      xcorr = scriptXcorrA;
    }

    // Initialize a TiltxcorrParam object from the com script command object
    TiltxcorrParam tiltXcorrParam = new TiltxcorrParam(appManager, axisID);
    initialize(tiltXcorrParam, xcorr, "tiltxcorr", axisID);
    return tiltXcorrParam;
  }

  /**
   * Save the specified xcorr com script updating the tiltxcorr parameters
   * @param axisID the AxisID to load.
   * @param tiltXcorrParam a TiltxcorrParam object that will be used to update
   * the xcorr com script
   */
  public void saveXcorr(TiltxcorrParam tiltXcorrParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptXcorr;
    if (axisID == AxisID.SECOND) {
      scriptXcorr = scriptXcorrB;
    }
    else {
      scriptXcorr = scriptXcorrA;
    }
    modifyCommand(scriptXcorr, tiltXcorrParam, "tiltxcorr", axisID);
  }

  /**
   * Save the blendmont param object to xcorr.com.
   * @param blendmontParam
   * @param axisID
   */
  public void saveXcorr(BlendmontParam blendmontParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptXcorr;
    if (axisID == AxisID.SECOND) {
      scriptXcorr = scriptXcorrB;
    }
    else {
      scriptXcorr = scriptXcorrA;
    }
    modifyCommand(scriptXcorr, blendmontParam, BlendmontParam.COMMAND_NAME,
        axisID);
  }

  public void saveXcorrToUndistort(BlendmontParam blendmontParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptUndistort;
    ComScript scriptXcorr;
    if (axisID == AxisID.SECOND) {
      scriptUndistort = scriptUndistortB;
      scriptXcorr = scriptXcorrB;
    }
    else {
      scriptUndistort = scriptUndistortA;
      scriptXcorr = scriptXcorrA;
    }
    addModifyCommand(scriptXcorr, scriptUndistort, blendmontParam,
        BlendmontParam.COMMAND_NAME, axisID);
  }

  /**
   * Save the goto param object to xcorr.com.
   * Saves to the first instance of the goto command in xcorr.com.
   * @param gotoParam
   * @param axisID
   */
  public void saveXcorr(GotoParam gotoParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptXcorr;
    if (axisID == AxisID.SECOND) {
      scriptXcorr = scriptXcorrB;
    }
    else {
      scriptXcorr = scriptXcorrA;
    }
    modifyCommand(scriptXcorr, gotoParam, GotoParam.COMMAND_NAME, axisID);
  }

  /**
   * Load the specified prenewst com script and initialize the NewstParam
   * object
   * @param axisID the AxisID to load.
   */
  public void loadPrenewst(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptPrenewstB = loadComScript("prenewst", axisID, true);
    }
    else {
      scriptPrenewstA = loadComScript("prenewst", axisID, true);
    }
  }

  public void loadPreblend(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptPreblendB = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.PREBLEND), axisID, true);
    }
    else {
      scriptPreblendA = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.PREBLEND), axisID, true);
    }
  }

  public void loadBlend(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptBlendB = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.BLEND), axisID, true);
    }
    else {
      scriptBlendA = loadComScript(BlendmontParam
          .getProcessName(BlendmontParam.Mode.BLEND), axisID, true);
    }
  }

  /**
   * @param axisID
   * @param required
   * @return true if script has loaded
   */
  public boolean loadCtfPlotter(AxisID axisID, boolean required) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptCtfPlotterB = loadComScript(ProcessName.CTF_PLOTTER, axisID, true,
          required);
      return scriptCtfPlotterB != null;
    }
    scriptCtfPlotterA = loadComScript(ProcessName.CTF_PLOTTER, axisID, true,
        required);
    return scriptCtfPlotterA != null;
  }

  public boolean loadCtfCorrection(AxisID axisID, boolean required) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptCtfCorrectionB = loadComScript(ProcessName.CTF_CORRECTION, axisID,
          true, required);
      return scriptCtfCorrectionB != null;
    }
    scriptCtfCorrectionA = loadComScript(ProcessName.CTF_CORRECTION, axisID,
        true, required);
    return scriptCtfCorrectionA != null;
  }

  /**
   * Get the newstack parameters from the specified prenewst script object
   * @param axisID the AxisID to read.
   * @return a NewstParam object that will be created and initialized
   * with the input arguments from prenewst in the com script.
   */
  public NewstParam getPrenewstParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptPrenewst;
    if (axisID == AxisID.SECOND) {
      scriptPrenewst = scriptPrenewstB;
    }
    else {
      scriptPrenewst = scriptPrenewstA;
    }

    // Initialize a NewstParam object from the com script command object
    NewstParam prenewstParam = new NewstParam(axisID);

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptPrenewst);
    initialize(prenewstParam, scriptPrenewst, cmdName, axisID);
    return prenewstParam;
  }

  public BlendmontParam getPreblendParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptPreblend;
    if (axisID == AxisID.SECOND) {
      scriptPreblend = scriptPreblendB;
    }
    else {
      scriptPreblend = scriptPreblendA;
    }

    // Initialize a BlendmontParam object from the com script command object
    BlendmontParam preblendParam = new BlendmontParam(appManager, appManager
        .getMetaData().getDatasetName(), axisID, BlendmontParam.Mode.PREBLEND);
    initialize(preblendParam, scriptPreblend, BlendmontParam.COMMAND_NAME,
        axisID);
    return preblendParam;
  }

  public BlendmontParam getBlendParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptBlend;
    if (axisID == AxisID.SECOND) {
      scriptBlend = scriptBlendB;
    }
    else {
      scriptBlend = scriptBlendA;
    }

    // Initialize a BlendmontParam object from the com script command object
    BlendmontParam blendParam = new BlendmontParam(appManager, appManager
        .getMetaData().getDatasetName(), axisID, BlendmontParam.Mode.BLEND);
    initialize(blendParam, scriptBlend, BlendmontParam.COMMAND_NAME, axisID);
    return blendParam;
  }

  /**
   * Save the specified prenewst com script updating the newst parameters
   * @param axisID the AxisID to load.
   * @param tiltXcorrParam a TiltxcorrParam object that will be used to update
   * the xcorr com script
   */
  public void savePrenewst(NewstParam prenewstParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptPrenewst;
    if (axisID == AxisID.SECOND) {
      scriptPrenewst = scriptPrenewstB;
    }
    else {
      scriptPrenewst = scriptPrenewstA;
    }

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptPrenewst);
    modifyCommand(scriptPrenewst, prenewstParam, cmdName, axisID);
  }

  public void savePreblend(BlendmontParam blendmontParam, AxisID axisID) {
    //TEMP
    System.err.println("savePreblend:mode=" + blendmontParam.getMode());
    //  Get a reference to the appropriate script object
    ComScript scriptPreblend;
    if (axisID == AxisID.SECOND) {
      scriptPreblend = scriptPreblendB;
    }
    else {
      scriptPreblend = scriptPreblendA;
    }
    modifyCommand(scriptPreblend, blendmontParam, BlendmontParam.COMMAND_NAME,
        axisID);
  }

  public void saveBlend(BlendmontParam blendmontParam, AxisID axisID) {
    //TEMP
    System.err.println("saveBlend:mode=" + blendmontParam.getMode());
    //  Get a reference to the appropriate script object
    ComScript scriptBlend;
    if (axisID == AxisID.SECOND) {
      scriptBlend = scriptBlendB;
    }
    else {
      scriptBlend = scriptBlendA;
    }
    modifyCommand(scriptBlend, blendmontParam, BlendmontParam.COMMAND_NAME,
        axisID);
  }

  public void saveCtfPlotter(CtfPlotterParam param, AxisID axisID) {
    ComScript script;
    if (axisID == AxisID.SECOND) {
      script = scriptCtfPlotterB;
    }
    else {
      script = scriptCtfPlotterA;
    }
    modifyCommand(script, param, ProcessName.CTF_PLOTTER.toString(), axisID);
  }

  public void saveCtfPhaseFlip(CtfPhaseFlipParam param, AxisID axisID) {
    ComScript script;
    if (axisID == AxisID.SECOND) {
      script = scriptCtfCorrectionB;
    }
    else {
      script = scriptCtfCorrectionA;
    }
    modifyCommand(script, param, CtfPhaseFlipParam.COMMAND, axisID);
  }

  /**
   * Load the specified track com script
   * @param axisID the AxisID to load.
   */
  public void loadTrack(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTrackB = loadComScript("track", axisID, true);
    }
    else {
      scriptTrackA = loadComScript("track", axisID, true);
    }
  }

  /**
   * Get the beadtrack parameters from the specified track script object
   * @param axisID the AxisID to read.
   * @return a BeadtrackParam object that will be created and initialized
   * with the input arguments from beadtrack in the com script.
   */
  public BeadtrackParam getBeadtrackParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript track;
    if (axisID == AxisID.SECOND) {
      track = scriptTrackB;
    }
    else {
      track = scriptTrackA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    BeadtrackParam beadtrackParam = new BeadtrackParam(axisID, appManager);
    initialize(beadtrackParam, track, "beadtrack", axisID);
    return beadtrackParam;
  }

  /**
   * Save the specified track com script updating the beadtrack parameters
   * @param axisID the AxisID to load.
   * @param beadtrackParam a BeadtrackParam object that will be used to update
   * the track com script
   */
  public void saveTrack(BeadtrackParam beadtrackParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptTrack;
    if (axisID == AxisID.SECOND) {
      scriptTrack = scriptTrackB;
    }
    else {
      scriptTrack = scriptTrackA;
    }
    // update the beadtrack parameters
    modifyCommand(scriptTrack, beadtrackParam, "beadtrack", axisID);
  }

  /**
   * Load the specified align com script object
   * @param axisID the AxisID to load.
   */
  public void loadAlign(AxisID axisID) {

    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptAlignB = loadComScript("align", axisID, true);
    }
    else {
      scriptAlignA = loadComScript("align", axisID, true);
    }
  }

  /**
   * Get the tiltalign parameters from the specified align script object
   * @param axisID the AxisID to read.
   * @return a TiltalignParam object that will be created and initialized
   * with the input arguments from tiltalign in the com script.
   */
  public TiltalignParam getTiltalignParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript align;
    if (axisID == AxisID.SECOND) {
      align = scriptAlignB;
    }
    else {
      align = scriptAlignA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    TiltalignParam tiltalignParam = new TiltalignParam(appManager, appManager
        .getMetaData().getDatasetName(), axisID);
    initialize(tiltalignParam, align, "tiltalign", axisID);
    return tiltalignParam;
  }

  /**
   * Get the xfproduct parameter from the align script
   * @param axisID
   * @return
   */
  public XfproductParam getXfproductInAlign(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript align;
    if (axisID == AxisID.SECOND) {
      align = scriptAlignB;
    }
    else {
      align = scriptAlignA;
    }

    // Initialize a BeadtrckParam object from the com script command object
    XfproductParam xfproductParam = new XfproductParam();
    initialize(xfproductParam, align, "xfproduct", axisID);
    return xfproductParam;
  }

  /**
   * Save the specified align com script updating the tiltalign parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveAlign(TiltalignParam tiltalignParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptAlign;
    if (axisID == AxisID.SECOND) {
      scriptAlign = scriptAlignB;
    }
    else {
      scriptAlign = scriptAlignA;
    }

    //  update the tiltalign parameters
    modifyCommand(scriptAlign, tiltalignParam, "tiltalign", axisID);
  }

  /**
   * Save the xfproduct command to the specified align com script
   * @param xfproductParam
   * @param axisID
   */
  public void saveXfproductInAlign(XfproductParam xfproductParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptAlign;
    if (axisID == AxisID.SECOND) {
      scriptAlign = scriptAlignB;
    }
    else {
      scriptAlign = scriptAlignA;
    }

    //  update the tiltalign parameters
    modifyCommand(scriptAlign, xfproductParam, "xfproduct", axisID);
  }

  /**
   * Load the specified newst com script
   * @param axisID the AxisID to load.
   */
  public void loadNewst(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptNewstB = loadComScript("newst", axisID, true);
    }
    else {
      scriptNewstA = loadComScript("newst", axisID, true);
    }
  }

  /**
   * Get the newst parameters from the specified newst script object
   * @param axisID the AxisID to read.
   * @return a NewstParam object that will be created and initialized
   * with the input arguments from newst in the com script.
   */
  public NewstParam getNewstComNewstParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptNewst;
    if (axisID == AxisID.SECOND) {
      scriptNewst = scriptNewstB;
    }
    else {
      scriptNewst = scriptNewstA;
    }

    // Initialize a NewstParam object from the com script command object
    NewstParam newstParam = new NewstParam(axisID);

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptNewst);
    initialize(newstParam, scriptNewst, cmdName, axisID);
    return newstParam;
  }

  /**
   * Save the specified newst com script updating the newst parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveNewst(NewstParam newstParam, AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript scriptNewst;
    if (axisID == AxisID.SECOND) {
      scriptNewst = scriptNewstB;
    }
    else {
      scriptNewst = scriptNewstA;
    }

    // Implementation note: since the name of the command newst was changed to
    // newstack we need to figure out which one it is before calling initialize.
    String cmdName = newstOrNewstack(scriptNewst);

    // update the newst parameters
    modifyCommand(scriptNewst, newstParam, cmdName, axisID);
  }

  /**
   * Load the specified tilt com script
   * @param axisID the AxisID to load.
   */
  public void loadTilt(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTiltB = loadComScript("tilt", axisID, false);
    }
    else {
      scriptTiltA = loadComScript("tilt", axisID, false);
    }
  }

  /**
   * Get the tilt parameters from the specified tilt script object
   * @param axisID the AxisID to read.
   * @return a TiltParam object that will be created and initialized
   * with the input arguments from tilt in the com script.
   */
  public TiltParam getTiltParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript tilt;
    if (axisID == AxisID.SECOND) {
      tilt = scriptTiltB;
    }
    else {
      tilt = scriptTiltA;
    }

    // Initialize a TiltParam object from the com script command object
    TiltParam tiltParam = new TiltParam(appManager, appManager.getMetaData()
        .getDatasetName(), axisID);
    initialize(tiltParam, tilt, "tilt", axisID);
    return tiltParam;
  }

  /**
   * Save the specified tilt com script updating the tilt parameters
   * @param axisID the AxisID to load.
   * @param tiltalignParam a TiltalignParam object that will be used to update
   * tiltalign command in the align com script
   */
  public void saveTilt(TiltParam tiltParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptTilt;
    if (axisID == AxisID.SECOND) {
      scriptTilt = scriptTiltB;
    }
    else {
      scriptTilt = scriptTiltA;
    }
    modifyCommand(scriptTilt, tiltParam, "tilt", axisID);
  }

  /**
   * Load the specified tomopitch com script
   * @param axisID the AxisID to load.
   */
  public void loadTomopitch(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptTomopitchB = loadComScript("tomopitch", axisID, true);
    }
    else {
      scriptTomopitchA = loadComScript("tomopitch", axisID, true);
    }
  }

  /**
   * Get the tomopitch parameters from the specified tomopitch script object
   * @param axisID the AxisID to read.
   * @return a TomopitchParam object that will be created and initialized
   * with the input arguments from tomopitch in the com script.
   */
  public TomopitchParam getTomopitchParam(AxisID axisID) {

    //  Get a reference to the appropriate script object
    ComScript tomopitch;
    if (axisID == AxisID.SECOND) {
      tomopitch = scriptTomopitchB;
    }
    else {
      tomopitch = scriptTomopitchA;
    }

    // Initialize a TomopitchParam object from the com script command object
    TomopitchParam tomopitchParam = new TomopitchParam();
    initialize(tomopitchParam, tomopitch, "tomopitch", axisID);
    return tomopitchParam;
  }

  /**
   * Save the specified tomopitch com script updating the tomopitch parameters
   * @param axisID the AxisID to load.
   * @param tomopitchParam a TomopitchParam object that will be used to update
   * tomopitch command in the tomopitch com script
   */
  public void saveTomopitch(TomopitchParam tomopitchParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptTomopitch;
    if (axisID == AxisID.SECOND) {
      scriptTomopitch = scriptTomopitchB;
    }
    else {
      scriptTomopitch = scriptTomopitchA;
    }
    modifyCommand(scriptTomopitch, tomopitchParam, "tomopitch", axisID);
  }

  public void loadMTFFilter(AxisID axisID) {
    //  Assign the new ComScriptObject object to the appropriate reference
    if (axisID == AxisID.SECOND) {
      scriptMTFFilterB = loadComScript(MTFFILTER_COMMAND, axisID, false);
    }
    else {
      scriptMTFFilterA = loadComScript(MTFFILTER_COMMAND, axisID, false);
    }
  }

  public CtfPhaseFlipParam getCtfPhaseFlipParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript ctfPhaseflip;
    if (axisID == AxisID.SECOND) {
      ctfPhaseflip = scriptCtfCorrectionB;
    }
    else {
      ctfPhaseflip = scriptCtfCorrectionA;
    }

    // Initialize from the com script command object
    CtfPhaseFlipParam ctfPhaseFlipParam = new CtfPhaseFlipParam();
    initialize(ctfPhaseFlipParam, ctfPhaseflip, CtfPhaseFlipParam.COMMAND,
        axisID);
    return ctfPhaseFlipParam;
  }

  public CtfPlotterParam getCtfPlotterParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript ctfPlotter;
    if (axisID == AxisID.SECOND) {
      ctfPlotter = scriptCtfPlotterB;
    }
    else {
      ctfPlotter = scriptCtfPlotterA;
    }

    // Initialize from the com script command object
    CtfPlotterParam ctfPlotterParam = new CtfPlotterParam();
    initialize(ctfPlotterParam, ctfPlotter, CtfPlotterParam.COMMAND, axisID);
    return ctfPlotterParam;
  }

  public MTFFilterParam getMTFFilterParam(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript mtfFilter;
    if (axisID == AxisID.SECOND) {
      mtfFilter = scriptMTFFilterB;
    }
    else {
      mtfFilter = scriptMTFFilterA;
    }

    // Initialize a TiltParam object from the com script command object
    MTFFilterParam mtfFilterParam = new MTFFilterParam();
    initialize(mtfFilterParam, mtfFilter, MTFFILTER_COMMAND, axisID);
    return mtfFilterParam;
  }

  public void saveMTFFilter(MTFFilterParam mtfFilterParam, AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript scriptMTFFilter;
    if (axisID == AxisID.SECOND) {
      scriptMTFFilter = scriptMTFFilterB;
    }
    else {
      scriptMTFFilter = scriptMTFFilterA;
    }
    modifyCommand(scriptMTFFilter, mtfFilterParam, MTFFILTER_COMMAND, axisID);
  }

  /**
   * Load in the solvematch com script.
   */
  public void loadSolvematch() {
    scriptSolvematch = loadComScript("solvematch", AxisID.ONLY, true);
  }

  /**
   * Return the solvematch parameter object from the currently loaded 
   * solvematch com script.
   * @return
   */
  public SolvematchParam getSolvematch() {

    // Initialize a SolvematchParam object from the com script command
    // object
    SolvematchParam solveMatchParam = new SolvematchParam(appManager);
    initialize(solveMatchParam, scriptSolvematch, "solvematch", AxisID.ONLY);
    return solveMatchParam;
  }

  /**
   * Replace the solvematch command in the solvematch com script with the info
   * in the specified SolvematchParam object
   * @param solveMatchParam
   */
  public void saveSolvematch(SolvematchParam solveMatchParam) {

    modifyCommand(scriptSolvematch, solveMatchParam, "solvematch", AxisID.ONLY);
  }

  /**
   * Save the matchshifts command to the solvematch com script
   * @param matchshiftsParam
   */
  public void saveSolvematch(MatchshiftsParam matchshiftsParam) {
    modifyCommand(scriptSolvematch, matchshiftsParam, matchshiftsParam
        .getCommand(), AxisID.ONLY, true, false);
  }

  /**
   * Load the solvematchshift com script
   */
  public void loadSolvematchshift() {
    scriptSolvematchshift = loadComScript("solvematchshift", AxisID.ONLY, true);
  }

  /**
   * Parse the solvematch command from the solvematchshift script
   * @return MatchorwarpParam
   */
  public SolvematchshiftParam getSolvematchshift() {

    // Initialize a SolvematchshiftParam object from the com script command
    // object
    SolvematchshiftParam solveMatchshiftParam = new SolvematchshiftParam();
    initialize(solveMatchshiftParam, scriptSolvematchshift, "solvematch",
        AxisID.ONLY);
    return solveMatchshiftParam;
  }

  public MatchshiftsParam getMatchshiftsFromSolvematchshifts() {

    // Initialize a MatchshiftsParam object from the com script command
    // object
    MatchshiftsParam matchshiftsParam = new MatchshiftsParam();
    initialize(matchshiftsParam, scriptSolvematchshift, "matchshifts",
        AxisID.ONLY);
    return matchshiftsParam;
  }

  /**
   * Get the blendmont command from xcorr.com
   * @param axisID
   * @return
   */
  public BlendmontParam getBlendmontParamFromTiltxcorr(AxisID axisID) {
    //  Get a reference to the appropriate script object
    ComScript xcorr;
    if (axisID == AxisID.SECOND) {
      xcorr = scriptXcorrB;
    }
    else {
      xcorr = scriptXcorrA;
    }

    // Initialize a TiltxcorrParam object from the com script command object
    BlendmontParam blendmontParam = new BlendmontParam(appManager, appManager
        .getMetaData().getDatasetName(), axisID);
    initialize(blendmontParam, xcorr, BlendmontParam.COMMAND_NAME, axisID);
    return blendmontParam;
  }

  /**
   * Get the first goto command from xcorr.com
   * @param axisID
   * @return
   */
  public GotoParam getGotoParamFromTiltxcorr(AxisID axisID) {
    ComScript xcorr;
    if (axisID == AxisID.SECOND) {
      xcorr = scriptXcorrB;
    }
    else {
      xcorr = scriptXcorrA;
    }
    // Initialize a GotoParam object from the com script command
    // object
    GotoParam gotoParam = new GotoParam();
    if (!initialize(gotoParam, xcorr, GotoParam.COMMAND_NAME, axisID)) {
      return null;
    }
    return gotoParam;
  }

  /**
   * Save the solvematchshift com script updating the solveMatchshiftParam
   * parameters
   * @param solveMatchshiftParam
   */
  public void saveSolvematchshift(SolvematchshiftParam solveMatchshiftParam) {
    Exception except = new Exception();
    except.printStackTrace();
    System.err.println("WARNING: call to saveSolvematchshift");
    modifyCommand(scriptSolvematchshift, solveMatchshiftParam, "solvematch",
        AxisID.ONLY);
  }

  /**
   * Load the solvematchmod com script
   */
  public void loadSolvematchmod() {
    scriptSolvematchmod = loadComScript("solvematchmod", AxisID.ONLY, true);
  }

  /**
   * Parse the solvematch command from the solvematchmod script
   * @return MatchorwarpParam
   */
  public SolvematchmodParam getSolvematchmod() {

    // Initialize a SolvematchmodParam object from the com script command
    // object
    SolvematchmodParam solveMatchmodParam = new SolvematchmodParam();
    initialize(solveMatchmodParam, scriptSolvematchmod, "solvematch",
        AxisID.ONLY);
    return solveMatchmodParam;
  }

  /**
   * Save the solvematchmod com script updating the solveMatchmodParam
   * parameters
   * @param solveMatchmodParam
   */
  public void saveSolvematchmod(SolvematchmodParam solveMatchmodParam) {

    Exception except = new Exception();
    except.printStackTrace();
    System.err.println("WARNING: call to saveSolvematchshift");
    modifyCommand(scriptSolvematchmod, solveMatchmodParam, "solvematch",
        AxisID.ONLY);
  }

  /**
   * Load the patchcorr com script
   */
  public void loadPatchcorr() {
    scriptPatchcorr = loadComScript("patchcorr", AxisID.ONLY, true);
  }

  /**
   * Parse the patchrawl3D command from the patchcorr script
   * @return MatchorwarpParam
   */
  public Patchcrawl3DParam getPatchcrawl3D() {

    // Initialize a Patchcrawl3DParam object from the com script command object
    Patchcrawl3DParam patchcrawl3DParam = new Patchcrawl3DParam();
    if (!initialize(patchcrawl3DParam, scriptPatchcorr,
        Patchcrawl3DParam.COMMAND, AxisID.ONLY, true)) {
      initialize(patchcrawl3DParam, scriptPatchcorr,
          Patchcrawl3DPrePIPParam.COMMAND, AxisID.ONLY);
    }
    return patchcrawl3DParam;
  }

  /**
   * Save the patchcorr com script updating the patchcrawl3d parameters
   * @param patchcrawl3DParam
   */
  public void savePatchcorr(Patchcrawl3DParam patchcrawl3DParam) {

    if (!modifyOptionalCommand(scriptPatchcorr, patchcrawl3DParam,
        Patchcrawl3DParam.COMMAND, AxisID.ONLY)) {
      deleteCommand(scriptPatchcorr, Patchcrawl3DPrePIPParam.COMMAND,
          AxisID.ONLY);
      addModifyCommand(scriptPatchcorr, patchcrawl3DParam,
          Patchcrawl3DParam.COMMAND, AxisID.ONLY, -1, false);
    }
  }

  /**
   * Load the matchvol1 com script
   */
  public void loadMatchvol1() {
    scriptMatchvol1 = loadComScript(MatchvolParam.COMMAND + '1', AxisID.ONLY,
        true);
  }

  /**
   * Parse the matchvol command from the matchvol1 script
   * @return MatchvolParam
   */
  public MatchvolParam getMatchvolParam() {

    // Initialize a MatchvolParam object from the com script command object
    MatchvolParam param = new MatchvolParam();
    initialize(param, scriptMatchvol1, MatchvolParam.COMMAND, AxisID.ONLY);
    return param;
  }

  /**
   * Save the matchvol1 com script updating the matchvol parameters
   * @param matchvolParam
   */
  public void saveMatchvol(MatchvolParam matchvolParam) {

    modifyCommand(scriptMatchvol1, matchvolParam, MatchvolParam.COMMAND,
        AxisID.ONLY);
  }

  /**
   * Load the matchorwarp com script
   */
  public void loadMatchorwarp() {
    scriptMatchorwarp = loadComScript("matchorwarp", AxisID.ONLY, true);
  }

  /**
   * Parse the matchorwarp command from the matchorwarp script
   * @return MatchorwarpParam
   */
  public MatchorwarpParam getMatchorwarParam() {

    // Initialize a MatchorwarpParam object from the com script command object
    MatchorwarpParam matchorwarpParam = new MatchorwarpParam();
    initialize(matchorwarpParam, scriptMatchorwarp, "matchorwarp", AxisID.ONLY);
    return matchorwarpParam;
  }

  /**
   * Save the matchorwarp com script updating the matchorwarp parameters
   * @param matchorwarpParam
   */
  public void saveMatchorwarp(MatchorwarpParam matchorwarpParam) {

    modifyCommand(scriptMatchorwarp, matchorwarpParam, "matchorwarp",
        AxisID.ONLY);
  }

  /**
   * 
   * @return
   */
  public CombineComscriptState getCombineComscript() {
    // Initialize a CombineComscript object from the com script command object
    CombineComscriptState combineComscriptState = new CombineComscriptState();
    if (!combineComscriptState.initialize(this)) {
      return null;
    }
    return combineComscriptState;
  }

  /**
   * Load the combine com script
   */
  public void loadCombine() {
    scriptCombine = loadComScript(CombineComscriptState.COMSCRIPT_NAME,
        AxisID.ONLY, true);
  }

  public void loadVolcombine() {
    scriptVolcombine = loadComScript("volcombine", AxisID.ONLY, true);
  }

  /**
   * 
   * @param gotoParam
   */
  public void saveCombine(GotoParam gotoParam) {
    modifyCommand(scriptCombine, gotoParam, GotoParam.COMMAND_NAME, AxisID.ONLY);
  }

  /**
   * returns index of saved command
   * @param echoParam
   * @param previousCommand
   */
  public int saveCombine(EchoParam echoParam, String previousCommand) {
    return addModifyCommand(scriptCombine, echoParam, EchoParam.COMMAND_NAME,
        AxisID.ONLY, previousCommand);
  }

  /**
   * 
   * @param gotoParam
   */
  public void saveCombine(ExitParam exitParam, int previousCommandIndex) {
    addModifyCommand(scriptCombine, exitParam, ExitParam.COMMAND_NAME,
        AxisID.ONLY, previousCommandIndex, false);
  }

  /**
   * 
   * @param gotoParam
   * @param previousCommandIndex
   */
  public void saveCombine(GotoParam gotoParam, int previousCommandIndex) {
    addModifyCommand(scriptCombine, gotoParam, GotoParam.COMMAND_NAME,
        AxisID.ONLY, previousCommandIndex, false);
  }

  public void saveVolcombine(SetParam setParam) {
    modifyCommand(scriptVolcombine, setParam, SetParam.COMMAND_NAME,
        AxisID.ONLY);
  }

  public void saveVolcombine(SetParam setParam, String previousCommand) {
    modifyCommand(scriptVolcombine, setParam, SetParam.COMMAND_NAME,
        AxisID.ONLY, false, false, previousCommand);
  }

  /**
   * returns index of saved command
   * @param echoParam
   * @param previousCommand
   */
  public void deleteFromCombine(String command, String previousCommand) {
    deleteCommand(scriptCombine, command, AxisID.ONLY, previousCommand);
  }

  /**
   * 
   * @return
   */
  public GotoParam getGotoParamFromCombine() {
    // Initialize a GotoParam object from the com script command
    // object
    GotoParam gotoParam = new GotoParam();
    if (!initialize(gotoParam, scriptCombine, GotoParam.COMMAND_NAME,
        AxisID.ONLY, true)) {
      return null;
    }
    return gotoParam;
  }

  public SetParam getSetParamFromVolcombine(String name, EtomoNumber.Type type) {
    SetParam setParam = new SetParam(name, type);
    if (!initialize(setParam, scriptVolcombine, SetParam.COMMAND_NAME,
        AxisID.ONLY, true)) {
      return null;
    }
    return setParam;
  }

  public SetParam getSetParamFromVolcombine(String name, EtomoNumber.Type type,
      String previousCommand) {
    SetParam setParam = new SetParam(name, type);
    if (!initialize(setParam, scriptVolcombine, SetParam.COMMAND_NAME,
        AxisID.ONLY, true, previousCommand, true)) {
      return null;
    }
    return setParam;
  }

  /**
   * 
   * @param previousCommand
   * @return
   */
  public EchoParam getEchoParamFromCombine(String previousCommand) {
    // Initialize an EchoParam object from a location after previousCommand
    //in the com script command
    // object
    EchoParam echoParam = new EchoParam();
    if (!initialize(echoParam, scriptCombine, EchoParam.COMMAND_NAME,
        AxisID.ONLY, false, previousCommand, false)) {
      return null;
    }
    return echoParam;
  }

  public void loadFlatten(AxisID axisID) {
    scriptFlatten = loadComScript(ProcessName.FLATTEN.toString(), axisID, true);
  }

  public boolean isWarpVolParamInFlatten(AxisID axisID) {
    return loadComScript(ProcessName.FLATTEN.toString(), axisID, true)
        .isCommandLoaded();
  }

  public WarpVolParam getWarpVolParamFromFlatten(AxisID axisID) {
    // Initialize a WarpVolParam object from the com script command
    // object
    WarpVolParam param = new WarpVolParam(appManager,axisID);
    initialize(param, scriptFlatten, WarpVolParam.COMMAND, axisID);
    return param;
  }

  /**
   * Save the WarpVolParam command to the flatten com script
   * @param warpVolParam
   */
  public void saveFlatten(WarpVolParam param, AxisID axisID) {
    modifyCommand(scriptFlatten, param, WarpVolParam.COMMAND, axisID, true,
        false);
  }

  /**
   * Use a template script from the $IMOD_DIR/com directory.  This is useful if
   * we encounter an old data set that does not have a current complete set of
   * com scripts
   * @param scriptName the basename of the template file
   * @param axisType axisType Specify whether this is a single or dual axis
   * tomogram. 
   * @param axisID the AxisID to create.  This is ignored for single axis 
   * tomogram.  For a dual axis tomogram AxisID.FIRST will create an a.com
   * script, AxisID.SECOND will create a b.com script.  AxisID.ONLY will
   * create a .com script replacing the g5a and g5b tags.
   *  
   */
  public void useTemplate(String scriptName, String datasetName,
      AxisType axisType, AxisID axisID) throws BadComScriptException,
      IOException {
    Utilities.timestamp("copy from template", scriptName,
        Utilities.STARTED_STATUS);
    // Read in the template file from the IMOD_DIR/com directory replacing all
    // instances of the tag g5a and g5b with the appropriate dataset name
    String comDirectory = EtomoDirector.INSTANCE.getIMODDirectory()
        .getAbsolutePath()
        + File.separator + "com";

    File template = new File(comDirectory, scriptName + ".com");
    if (!template.exists()) {
      String message = "Unknown template: " + scriptName;
      Utilities.timestamp("copy from template", scriptName,
          Utilities.FAILED_STATUS);
      throw new BadComScriptException(message);
    }
    BufferedReader templateReader = new BufferedReader(new FileReader(template));

    // The ouput script
    File script;
    BufferedWriter scriptWriter;

    // Open the appropriate output script and change the dataset name if
    // necessary.
    if (axisType == AxisType.SINGLE_AXIS) {
      script = new File(appManager.getPropertyUserDir(), scriptName + ".com");
      scriptWriter = new BufferedWriter(new FileWriter(script));
    }
    else {
      script = new File(appManager.getPropertyUserDir(), scriptName
          + axisID.getExtension() + ".com");
      scriptWriter = new BufferedWriter(new FileWriter(script));
      datasetName = datasetName + axisID.getExtension();
    }

    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      String line;
      while ((line = templateReader.readLine()) != null) {
        line = line.replaceAll("g5a", datasetName + "a");
        line = line.replaceAll("g5b", datasetName + "b");
        scriptWriter.write(line);
        scriptWriter.newLine();
      }
    }
    else {
      String line;
      while ((line = templateReader.readLine()) != null) {
        line = line.replaceAll("g5a", datasetName);
        scriptWriter.write(line);
        scriptWriter.newLine();
      }
    }
    templateReader.close();
    scriptWriter.close();
    Utilities.timestamp("copy from template", scriptName,
        Utilities.FINISHED_STATUS);
  }

  /**
   * Use a template script from the $IMOD_DIR/com directory.  This is useful if
   * we encounter an old data set that does not have a current complete set of
   * com scripts
   * @param scriptName the basename of the template file
   * @param axisType axisType Specify whether this is a single or dual axis
   * tomogram. 
   * @param axisID the AxisID to create.  This is ignored for single axis 
   * tomogram.  For a dual axis tomogram AxisID.FIRST will create an a.com
   * script, AxisID.SECOND will create a b.com script.  AxisID.ONLY will
   * create a .com script.
   * @param rename rename the original script file
   * @throws BadComScriptException
   * @throws IOException
   */
  public void useTemplate(String scriptName, AxisType axisType, AxisID axisID,
      boolean rename) throws BadComScriptException, IOException {
    Utilities.timestamp("copy from template", scriptName,
        Utilities.STARTED_STATUS);
    // Copy the template file from the IMOD_DIR/com directory to the script
    String comDirectory = EtomoDirector.INSTANCE.getIMODDirectory()
        .getAbsolutePath()
        + File.separator + "com";

    File template = new File(comDirectory, scriptName + ".com");
    if (!template.exists()) {
      String message = "Unknown template: " + scriptName;
      Utilities.timestamp("copy from template", scriptName,
          Utilities.FAILED_STATUS);
      throw new BadComScriptException(message);
    }

    // The ouput script
    File script;
    if (axisType == AxisType.SINGLE_AXIS) {
      script = new File(appManager.getPropertyUserDir(), scriptName + ".com");
    }
    else {
      script = new File(appManager.getPropertyUserDir(), scriptName
          + axisID.getExtension() + ".com");
    }

    if (rename) {
      Utilities.renameFile(script, new File(script.getAbsolutePath() + "~"));
    }
    Utilities.copyFile(template, script);
    Utilities.timestamp("copy from template", scriptName,
        Utilities.FINISHED_STATUS);
  }

  private ComScript loadComScript(String scriptName, AxisID axisID,
      boolean parseComments) {
    return loadComScript(ProcessName.getInstance(scriptName, axisID), axisID,
        parseComments, true);
  }

  private ComScript loadComScript(ProcessName processName, AxisID axisID,
      boolean parseComments) {
    return loadComScript(processName, axisID, parseComments, true);
  }

  /**
   * Load the specified Com script 
   * @param scriptName
   * @param axisID
   * @return ComScript
   */
  private ComScript loadComScript(ProcessName processName, AxisID axisID,
      boolean parseComments, boolean required) {
    Utilities.timestamp("load", processName, Utilities.STARTED_STATUS);
    String command = processName.getComscript(axisID);
    File comFile = new File(appManager.getPropertyUserDir(), command);
    //If the file isn't there and its not required then just return null without
    //any error messages.
    if (!required && !comFile.exists()) {
      return null;
    }
    ComScript comScript = new ComScript(comFile);
    try {
      comScript.setParseComments(parseComments);
      comScript.readComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Com file: " + comScript.getComFileName();
      errorMessage[1] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't parse "
          + processName + axisID.getExtension() + ".com file: "
          + comScript.getComFileName(), JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("load", processName, Utilities.FAILED_STATUS);
      return null;
    }
    Utilities.timestamp("load", processName, Utilities.FINISHED_STATUS);
    return comScript;
  }

  /**
   * Update the specified comscript with 
   * @param script
   * @param params
   * @param command
   * @param axisID
   */
  private void modifyCommand(ComScript script, CommandParam params,
      String command, AxisID axisID) {
    modifyCommand(script, params, command, axisID, false, false);
  }

  private boolean modifyOptionalCommand(ComScript script, CommandParam params,
      String command, AxisID axisID) {
    return modifyCommand(script, params, command, axisID, false, true);
  }

  /**
   * Modify and/or add command (depending on addNew boolean)
   * May treate command as optional, depending on optional boolean
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param optional
   * @return
   */
  private boolean modifyCommand(ComScript script, CommandParam params,
      String command, AxisID axisID, boolean addNew, boolean optional) {
    if (script == null) {
      (new IllegalStateException()).printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.";
      errorMessage[1] = "\nscript=" + script + "\ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.STARTED_STATUS);

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command, addNew);
    //optional return false if failed
    if (optional && commandIndex == -1) {
      Utilities.timestamp("updateComScript", command, script,
          Utilities.FAILED_STATUS);
      return false;
    }
    try {
      comScriptCommand = script.getScriptCommand(command);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't update "
          + command + " in " + script.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.FINISHED_STATUS);
    return true;
  }

  private boolean modifyCommand(ComScript script, CommandParam params,
      String command, AxisID axisID, boolean addNew, boolean optional,
      String previousCommand) {
    if (script == null) {
      (new IllegalStateException()).printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.";
      errorMessage[1] = "\nscript=" + script + "\ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.STARTED_STATUS);

    //locate previous command
    ComScriptCommand previousComScriptCommand = null;
    int previousCommandIndex = script.getScriptCommandIndex(previousCommand);

    //previous command must exist
    if (previousCommandIndex == -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update " + script.getName() + ".  ";
      errorMessage[1] = "Unable to update " + command
          + " because the previous command, " + previousCommand
          + ", is missing.";
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }
    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command,
        previousCommandIndex + 1, addNew);
    //optional return false if failed
    if (optional && commandIndex == -1) {
      Utilities.timestamp("updateComScript", command, script,
          Utilities.FAILED_STATUS);
      return false;
    }
    try {
      comScriptCommand = script.getScriptCommand(commandIndex);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't update "
          + command + " in " + script.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return false;
    }
    Utilities.timestamp("update", command, script, Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * Find the command in fromScript, update it, and write it to toScript.
   * it back to toScript.  AddNew is only used with the toScript.  The
   * fromScript should not be changed.  If the fromScript is not null, the
   * command must be in it.
   * @param fromScript
   * @param toScript
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   */
  private void addModifyCommand(ComScript fromScript, ComScript toScript,
      CommandParam params, String command, AxisID axisID) {
    if (toScript == null) {
      (new IllegalStateException()).printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.  ";
      errorMessage[1] = "\ntoScript=" + toScript + "ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return;
    }
    Utilities.timestamp("update", command, toScript, Utilities.STARTED_STATUS);
    //If no fromScript, then default to reading from and writing to the
    //toScript.
    if (fromScript == null) {
      modifyCommand(toScript, params, command, axisID, true, false);
      Utilities.timestamp("update", command, toScript,
          Utilities.FINISHED_STATUS);
      return;
    }
    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int toScriptCommandIndex = toScript.getScriptCommandIndex(command, true);
    try {
      //Get comScriptCommand from fromScript
      comScriptCommand = fromScript.getScriptCommand(command);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + fromScript.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't read " + command
          + " in " + fromScript.getComFileName(), JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, toScript, Utilities.FAILED_STATUS);
      return;
    }
    try {
      //Update comScriptCommand
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + toScript.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't update "
          + command + " in " + toScript.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, toScript, Utilities.FAILED_STATUS);
      return;
    }

    //Replace the specified command by the updated comScriptCommand in toScript
    toScript.setScriptComand(toScriptCommandIndex, comScriptCommand);

    //Write toScript back out to disk
    try {
      toScript.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + toScript.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, toScript, Utilities.FAILED_STATUS);
      return;
    }
    Utilities.timestamp("update", command, toScript, Utilities.FINISHED_STATUS);
  }

  /**
   * Modify and/or add command
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param previousCommand
   * @return index of updated command
   */
  private int addModifyCommand(ComScript script, CommandParam params,
      String command, AxisID axisID, String previousCommand) {
    if (script == null) {
      String[] errorMessage = new String[1];
      errorMessage[0] = "Unable to update comscript.\nscript=" + script
          + "\ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return -1;
    }
    Utilities.timestamp("update", command, script, Utilities.STARTED_STATUS);
    //locate previous command
    ComScriptCommand previousComScriptCommand = null;
    int previousCommandIndex = script.getScriptCommandIndex(previousCommand);

    if (previousCommandIndex == -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update " + script.getName() + ".  ";
      errorMessage[1] = "Unable to update " + command
          + " because the previous command, " + previousCommand
          + ", is missing.";
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return -1;
    }

    return addModifyCommand(script, params, command, axisID,
        previousCommandIndex, true);
  }

  /**
   * Modify and/or add command
   * @param script
   * @param params
   * @param command
   * @param axisID
   * @param addNew
   * @param previousCommandIndex
   * @return
   */
  private int addModifyCommand(ComScript script, CommandParam params,
      String command, AxisID axisID, int previousCommandIndex,
      boolean updateStarted) {
    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.";
      errorMessage[1] = "\nscript=" + script + "\ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return -1;
    }
    if (!updateStarted) {
      Utilities.timestamp("update", command, script, Utilities.STARTED_STATUS);
    }
    if (previousCommandIndex < -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update " + script.getName() + ".  ";
      errorMessage[1] = "Can't find " + command
          + " because the previous command index, " + previousCommandIndex
          + ", is invalid.\npreviousCommandIndex=" + previousCommandIndex;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return -1;
    }

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = previousCommandIndex + 1;

    try {
      comScriptCommand = script.getScriptCommand(command, commandIndex, true);
      params.updateComScriptCommand(comScriptCommand);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't update "
          + command + " in " + script.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return commandIndex;
    }

    // Replace the specified command by the updated comScriptCommand
    script.setScriptComand(commandIndex, comScriptCommand);
    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("update", command, script, Utilities.FAILED_STATUS);
      return commandIndex;
    }
    Utilities.timestamp("update", command, script, Utilities.FINISHED_STATUS);
    return commandIndex;
  }

  private void deleteCommand(ComScript script, String command, AxisID axisID,
      String previousCommand) {
    if (script == null) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.  ";
      errorMessage[1] = "Cannot delete" + command + ".\nscript=" + script;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return;
    }
    Utilities.timestamp("delete", command, script, Utilities.STARTED_STATUS);

    //locate previous command
    ComScriptCommand previousComScriptCommand = null;
    int previousCommandIndex = script.getScriptCommandIndex(previousCommand);

    if (previousCommandIndex == -1) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update " + script.getName() + ".  ";
      errorMessage[1] = "Unable to delete " + command
          + " because the previous command, " + previousCommand
          + ", is missing.";
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      Utilities.timestamp("delete", command, script, Utilities.FAILED_STATUS);
      return;
    }

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command,
        previousCommandIndex + 1);

    if (commandIndex == -1) {
      Utilities.timestamp("delete", command, script, Utilities.FAILED_STATUS);
      return;
    }
    script.deleteCommand(commandIndex);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("delete", command, script, Utilities.FAILED_STATUS);
    }
    Utilities.timestamp("delete", command, script, Utilities.FINISHED_STATUS);
  }

  private void deleteCommand(ComScript script, String command, AxisID axisID) {
    if (script == null) {
      (new IllegalStateException()).printStackTrace();
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to update comscript.";
      errorMessage[1] = "\nscript=" + script + "\ncommand=" + command;
      uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
          axisID, appManager.getManagerKey());
      return;
    }
    Utilities.timestamp("delete", command, script, Utilities.STARTED_STATUS);

    //  Update the specified com script command from the CommandParam object
    ComScriptCommand comScriptCommand = null;
    int commandIndex = script.getScriptCommandIndex(command);

    try {
      comScriptCommand = script.getScriptCommand(command);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane.showMessageDialog(null, errorMessage, "Can't delete "
          + command + " in " + script.getComFileName(),
          JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("delete", command, script, Utilities.FAILED_STATUS);
      return;
    }

    // Delete the specified command
    script.deleteCommand(commandIndex);

    //  Write the script back out to disk
    try {
      script.writeComFile();
    }
    catch (Exception except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Com file: " + script.getComFileName();
      errorMessage[1] = "Command: " + command;
      errorMessage[2] = except.getMessage();
      JOptionPane
          .showMessageDialog(null, except.getMessage(), "Can't write "
              + command + axisID.getExtension() + ".com",
              JOptionPane.ERROR_MESSAGE);
      Utilities.timestamp("delete", command, script, Utilities.FAILED_STATUS);
      return;
    }
    Utilities.timestamp("delete", command, script, Utilities.FINISHED_STATUS);
  }

  /**
   * Initialize the CommandParam object from the specified command in the
   * comscript.  True is returned if the initialization is successful, false if
   * the initialization fails.
   * 
   * @param param
   * @param comScript
   * @param command
   * @param axisID
   * @return boolean
   */
  private boolean initialize(CommandParam param, ComScript comScript,
      String command, AxisID axisID) {
    return initialize(param, comScript, command, axisID, false);
  }

  private boolean initialize(CommandParam param, ComScript comScript,
      String command, AxisID axisID, boolean optionalCommand) {
    if (comScript == null) {
      return false;
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.STARTED_STATUS);
    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
      if (optionalCommand) {
        if (comScript.getScriptCommandIndex(command) == -1) {
          Utilities.timestamp("initialize", command, comScript,
              Utilities.FAILED_STATUS);
          return false;
        }
      }
      try {
        param.parseComScriptCommand(comScript.getScriptCommand(command));
      }
      catch (Exception except) {
        except.printStackTrace();
        String[] errorMessage = new String[4];
        errorMessage[0] = "Com file: " + comScript.getComFileName();
        errorMessage[1] = "Command: " + command;
        errorMessage[2] = except.getClass().getName();
        errorMessage[3] = except.getMessage();
        JOptionPane.showMessageDialog(null, errorMessage,
            "Com Script Command Parse Error", JOptionPane.ERROR_MESSAGE);
        Utilities.timestamp("initialize", command, comScript,
            Utilities.FAILED_STATUS);
        return false;
      }
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * Initialize the CommandParam object from the specified command in the
   * comscript.  True is returned if the initialization is successful, false if
   * the initialization fails.
   * 
   * @param param
   * @param comScript
   * @param command
   * @param axisID
   * @param optionalCommand - missing command returns false, no exception thrown
   * @param optionalPreviousCommand - missing previous command returns false, no exception thrown
   * @return boolean
   */
  private boolean initialize(CommandParam param, ComScript comScript,
      String command, AxisID axisID, boolean optionalCommand,
      String previousCommand, boolean optionalPreviousCommand) {
    if (previousCommand == null) {
      return initialize(param, comScript, command, axisID);
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.STARTED_STATUS);
    if (!comScript.isCommandLoaded()) {
      param.initializeDefaults();
    }
    else {
      //locate previous command
      ComScriptCommand previousComScriptCommand = null;
      int previousCommandIndex = comScript
          .getScriptCommandIndex(previousCommand);

      if (previousCommandIndex == -1) {
        if (optionalPreviousCommand) {
          return false;
        }
        String[] errorMessage = new String[2];
        errorMessage[0] = "Unable to read " + comScript.getName() + ".  ";
        errorMessage[1] = "Unable to read " + command
            + " because the previous command, " + previousCommand
            + ", is missing.";
        uiHarness.openMessageDialog(errorMessage, "ComScriptManager Error",
            axisID, appManager.getManagerKey());
        Utilities.timestamp("initialize", command, comScript,
            Utilities.FAILED_STATUS);
        return false;
      }

      ComScriptCommand comScriptCommand = null;
      int commandIndex = comScript.getScriptCommandIndex(command,
          previousCommandIndex + 1);

      if (commandIndex == -1) {
        Utilities.timestamp("initialize", command, comScript,
            Utilities.FAILED_STATUS);
        return false;
      }
      if (optionalCommand) {
        if (comScript.getScriptCommandIndex(command, previousCommandIndex + 1) == -1) {
          Utilities.timestamp("initialize", command, comScript,
              Utilities.FAILED_STATUS);
          return false;
        }
      }
      try {
        param.parseComScriptCommand(comScript.getScriptCommand(command,
            commandIndex, false));
      }
      catch (Exception except) {
        except.printStackTrace();
        String[] errorMessage = new String[4];
        errorMessage[0] = "Com file: " + comScript.getComFileName();
        errorMessage[1] = "Command: " + command + " after " + previousCommand;
        errorMessage[2] = except.getClass().getName();
        errorMessage[3] = except.getMessage();
        JOptionPane.showMessageDialog(null, errorMessage,
            "Com Script Command Parse Error", JOptionPane.ERROR_MESSAGE);
        Utilities.timestamp("initialize", command, comScript,
            Utilities.FAILED_STATUS);
        return false;
      }
    }
    Utilities.timestamp("initialize", command, comScript,
        Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * Examine the com script to see whether it contains newst or newstack
   * commands.
   * @param comScript  The com script object to examine
   * @return The 
   */
  private String newstOrNewstack(ComScript comScript) {
    if (comScript == null) {
      return "";
    }
    String[] commands = comScript.getCommandArray();
    for (int i = 0; i < commands.length; i++) {
      if (commands[i].equals("newst")) {
        return "newst";
      }
      if (commands[i].equals("newstack")) {
        return "newstack";
      }
    }
    return "";
  }
}