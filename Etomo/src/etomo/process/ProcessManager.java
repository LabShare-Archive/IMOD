package etomo.process;

import etomo.type.AxisID;
import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.ui.TextPageWindow;
import etomo.comscript.CopyTomoComs;
import etomo.comscript.BadComScriptException;
import etomo.comscript.SetupCombine;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * <p>
 * Description: This object manages the execution of com scripts in the
 * background and the opening and sending messages to imod.
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
 * Revision 3.3  2004/03/11 00:00:33  sueh
 * bug# 61
 *
 * Revision 3.2  2003/11/26 23:39:14  rickg
 * Debug flag and getter changed to static in AppManager.
 *
 * Revision 3.1  2003/11/10 07:33:06  rickg
 * Bug# 353 Transferfid log window is now a standard log window
 *
 * Revision 3.0  2003/11/07 23:19:00  rickg
 * Version 1.0.0
 *
 * Revision 2.33  2003/11/04 20:56:11  rickg
 * Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 *
 * Revision 2.32  2003/11/04 01:02:41  rickg
 * Bug #345 Explicitly set path to script using IMOD_DIR
 *
 * <p>
 * Revision 2.31 2003/10/27 23:56:33 rickg
 * <p>
 * Bug# 283 Open tomopitch log file in done message when
 * <p>
 * appropriate
 * <p>
 * <p>
 * Revision 2.30 2003/10/24 19:07:55 rickg
 * <p>
 * Bug# 333 Fixed null reference to stderr array
 * <p>
 * <p>
 * Revision 2.29 2003/10/09 23:11:12 sueh
 * <p>
 * bug251 fixed Kill Process and fixed prevention of two processes
 * <p>
 * on the same axis by shifting transferfid process to the correct
 * <p>
 * axis (the destination axis where the button was pushed).
 * <p>
 * <p>
 * Revision 2.28 2003/10/09 05:58:41 rickg
 * <p>
 * Forget to get transferfid command line from BackgroundProcess
 * <p>
 * <p>
 * Revision 2.27 2003/10/06 21:59:52 rickg
 * <p>
 * split mapAxisThread and mapAxisProcessMonitor
 * <p>
 * <p>
 * Revision 2.26 2003/10/05 21:55:08 rickg
 * <p>
 * Changed method order to clarify structure
 * <p>
 * <p>
 * Revision 2.25 2003/10/05 21:32:49 rickg
 * <p>
 * Bug# 256
 * <p>
 * Generalized BackgroundProcess starting
 * <p>
 * <p>
 * Revision 2.24 2003/10/01 18:16:54 rickg
 * <p>
 * Update demo mode to work with process monitor structure
 * <p>
 * <p>
 * Revision 2.23 2003/10/01 04:22:47 rickg
 * <p>
 * Moved the order of processing in msgComScriptDone to
 * <p>
 * keep monitor around till after the processDone method is called
 * <p>
 * <p>
 * Revision 2.22 2003/09/08 22:20:35 rickg
 * <p>
 * Throw an exception if a process is already running for the current
 * <p>
 * axis
 * <p>
 * <p>
 * Revision 2.21 2003/08/20 22:01:34 rickg
 * <p>
 * Explicitly wait for the comScriptProcess to start before starting the
 * <p>
 * process monitor thread.
 * <p>
 * <p>
 * Revision 2.20 2003/08/05 21:40:53 rickg
 * <p>
 * Implemented CCDEraserProcessMonitor
 * <p>
 * LogFileProcessMonitors now are passed to startComScriptProcess
 * <p>
 * comScript => comScriptProcess
 * <p>
 * <p>
 * Revision 2.19 2003/07/01 22:52:58 rickg
 * <p>
 * Start process monitor thread before comscript thread
 * <p>
 * <p>
 * Revision 2.18 2003/07/01 19:28:27 rickg
 * <p>
 * startComScript and mapThreadAxis now manage the
 * <p>
 * processMonitor
 * <p>
 * prenewst, newst and tilt switched fileSizeProcessMonitors
 * <p>
 * <p>
 * Revision 2.17 2003/06/27 20:17:19 rickg
 * <p>
 * Added process monitor class members and management
 * <p>
 * <p>
 * Revision 2.16 2003/06/05 21:11:34 rickg
 * <p>
 * Parse ERROR: from stdout
 * <p>
 * <p>
 * Revision 2.15 2003/06/05 16:52:37 rickg
 * <p>
 * Removed stdout messages
 * <p>
 * <p>
 * Revision 2.14 2003/06/05 04:40:15 rickg
 * <p>
 * thread references changed to SystemProcessInterface
 * <p>
 * mapped threads for transferfid and trimvol
 * <p>
 * <p>
 * Revision 2.13 2003/05/27 08:45:59 rickg
 * <p>
 * Added new method to generate the align logs
 * <p>
 * Added a section to the comscript done section for scipt
 * <p>
 * dependent post processing
 * <p>
 * <p>
 * Revision 2.12 2003/05/23 14:26:35 rickg
 * <p>
 * RunComscript renamed to ComScriptProcess
 * <p>
 * StartComScript method returns ComScriptProcess instead
 * <p>
 * of string name
 * <p>
 * Starting to implement determinant progress bar functions
 * <p>
 * <p>
 * Revision 2.11 2003/05/22 23:34:53 rickg
 * <p>
 * Kill process now kills by walking the process tree.
 * <p>
 * <p>
 * Revision 2.10 2003/05/21 22:56:23 rickg
 * <p>
 * Initial kill implementation
 * <p>
 * <p>
 * Revision 2.9 2003/05/13 16:56:04 rickg
 * <p>
 * Use whole command line for transferfid command ID, since
 * <p>
 * windows/cygwin needs a preceeding tcsh
 * <p>
 * <p>
 * Revision 2.8 2003/05/08 23:19:03 rickg
 * <p>
 * Standardized debug setting
 * <p>
 * <p>
 * Revision 2.7 2003/05/07 22:27:07 rickg
 * <p>
 * System property user.dir now defines the working directory
 * <p>
 * <p>
 * Revision 2.6 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 2.5 2003/04/16 00:14:26 rickg
 * <p>
 * Trimvol in progress
 * <p>
 * <p>
 * Revision 2.4 2003/03/26 00:52:42 rickg
 * <p>
 * Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p>
 * Revision 2.3 2003/03/18 00:32:33 rickg
 * <p>
 * combine development in progress
 * <p>
 * <p>
 * Revision 2.2 2003/02/24 23:35:24 rickg
 * <p>
 * Added management of threads for each axis
 * <p>
 * Added interrupt method (doesn't work well with C shell scripts)
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 20:46:40 rickg
 * <p>
 * Check debug mode for messages
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.14.2.1 2003/01/24 18:36:17 rickg
 * <p>
 * Single window GUI layout initial revision
 * <p>
 * <p>
 * Revision 1.14 2003/01/08 05:00:13 rickg
 * <p>
 * Wrote transferfid log file and create stdout dialog box
 * <p>
 * <p>
 * Revision 1.13 2003/01/08 03:56:17 rickg
 * <p>
 * Mods in progress
 * <p>
 * <p>
 * Revision 1.12 2003/01/04 00:23:36 rickg
 * <p>
 * Added a test method.
 * <p>
 * TransferFiducials method reports command name
 * <p>
 * instead of thread name.
 * <p>
 * <p>
 * Revision 1.11 2003/01/03 00:56:00 rickg
 * <p>
 * Added msgBackgroundProcessDone method
 * <p>
 * <p>
 * Revision 1.10 2002/12/31 00:57:38 rickg
 * <p>
 * Added transferFiducials method
 * <p>
 * <p>
 * Revision 1.9 2002/12/30 20:38:11 rickg
 * <p>
 * msgComScriptDone now invokes appropriate error
 * <p>
 * or warning dialogs
 * <p>
 * <p>
 * Revision 1.8 2002/10/22 21:38:33 rickg
 * <p>
 * ApplicationManager now controls both demo and debug
 * <p>
 * modes
 * <p>
 * <p>
 * Revision 1.7 2002/10/14 22:45:07 rickg
 * <p>
 * Enabled debug output for com scripts
 * <p>
 * <p>
 * Revision 1.6 2002/10/14 19:02:37 rickg
 * <p>
 * Opens a dialog through the app manager if a process returns
 * <p>
 * an error
 * <p>
 * <p>
 * Revision 1.5 2002/10/10 23:39:56 rickg
 * <p>
 * refactored createCombineScripts to setupCombineScripts
 * <p>
 * <p>
 * Revision 1.4 2002/10/10 18:47:16 rickg
 * <p>
 * Enabled debugging output from the SystemProgram object
 * <p>
 * <p>
 * Revision 1.3 2002/10/07 22:26:31 rickg
 * <p>
 * removed unused imports
 * <p>
 * reformat after emacs messed it up
 * <p>
 * <p>
 * Revision 1.2 2002/09/19 21:43:30 rickg
 * <p>
 * Moved imod control to ImodManager/ImodProcess objects
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */

public class ProcessManager {
  public static final String rcsid =
    "$Id$";

  ApplicationManager appManager;
  SystemProcessInterface threadAxisA = null;
  SystemProcessInterface threadAxisB = null;
  Thread processMonitorA = null;
  Thread processMonitorB = null;

  // save the transferfid command line so that we can identify when process is
  // complete.
  String transferfidCommandLine;

  public ProcessManager(ApplicationManager appMgr) {
    appManager = appMgr;
  }

  /**
   * Run the copytomocoms script
   * 
   * @param metaData
   *          a read-only MetaData object containing the information to run the
   *          copytomocoms script
   */
  public void setupComScripts(ConstMetaData metaData)
    throws BadComScriptException, IOException {

    CopyTomoComs copyTomoComs = new CopyTomoComs(metaData);

    if (ApplicationManager.isDebug()) {
      System.err.println(
        "copytomocoms command line: " + copyTomoComs.getCommandLine());
    }

    int exitValue = copyTomoComs.run();

    if (exitValue != 0) {
      System.err.println("Exit value: " + String.valueOf(exitValue));

      //  Compile the exception message from the stderr stream
      String[] stdError = copyTomoComs.getStdError();
      if (stdError.length < 1) {
        stdError = new String[1];
        stdError[0] =
          "Get David to add some std error reporting to copytomocoms";
      }
      StringBuffer buffer = new StringBuffer();
      buffer.append("Copytomocoms Error\n");
      buffer.append("Standard error output:\n");
      for (int i = 0; i < stdError.length; i++) {
        buffer.append(stdError[i]);
        buffer.append("\n");
      }

      throw (new BadComScriptException(buffer.toString()));
    }
  }

  /**
   * Erase the specified pixels
   * 
   * @param axisID
   *          the AxisID to erase.
   */
  public String eraser(AxisID axisID) throws SystemProcessException {

    // Create the process monitor
    CCDEraserProcessMonitor ccdEraserProcessMonitor =
      new CCDEraserProcessMonitor(appManager, axisID);

    //  Create the required command string
    String command = "eraser" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, ccdEraserProcessMonitor, axisID);

    return comScriptProcess.getName();
  }

  /**
   * Calculate the cross-correlation for the specified axis
   * 
   * @param axisID
   *          the AxisID to cross-correlate.
   */
  public String crossCorrelate(AxisID axisID) throws SystemProcessException {

    //  Create the process monitor
    XcorrProcessWatcher xcorrProcessWatcher =
      new XcorrProcessWatcher(appManager, axisID);

    //  Create the required command string
    String command = "xcorr" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, xcorrProcessWatcher, axisID);

    return comScriptProcess.getName();
  }

  /**
   * Calculate the coarse alignment for the specified axis
   * 
   * @param axisID
   *          the identifyer of the axis to coarse align.
   */
  public String coarseAlign(AxisID axisID) throws SystemProcessException {

    //  Create the required tiltalign command
    String command = "prenewst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    PrenewstProcessMonitor prenewstProcessMonitor =
      new PrenewstProcessMonitor(appManager, axisID);

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, prenewstProcessMonitor, axisID);
    return comScriptProcess.getName();
  }

  /**
   * Run midas on the specified raw stack
   * 
   * @param axisID
   *          the AxisID to run midas on.
   */
  public void midasRawStack(AxisID axisID) {
    String stack = "";
    String xform = "";

    //  Create the required midas command
    if (axisID == AxisID.ONLY) {
      stack = appManager.getDatasetName() + ".st ";
      xform = appManager.getDatasetName() + ".prexf ";
    }
    if (axisID == AxisID.FIRST) {
      stack = appManager.getDatasetName() + "a.st ";
      xform = appManager.getDatasetName() + "a.prexf ";
    }
    if (axisID == AxisID.SECOND) {
      stack = appManager.getDatasetName() + "b.st ";
      xform = appManager.getDatasetName() + "b.prexf ";
    }

    String imodBinPath =
      ApplicationManager.getIMODDirectory().getAbsolutePath()
        + File.separator
        + "bin"
        + File.separator;
    String commandLine = imodBinPath + "midas " + stack + xform;
    //  Start the system program thread
    startSystemProgramThread(commandLine);
  }

  /**
   * Run the appropriate track com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run track.com on.
   */
  public String fiducialModelTrack(AxisID axisID)
    throws SystemProcessException {
    //
    //  Create the required beadtrack command
    //
    String command = "track" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null, axisID);
    return comScriptProcess.getName();
  }

  /**
   * Run the appropriate align com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run align.com on.
   */
  public String fineAlignment(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required tiltalign command
    //
    String command = "align" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null, axisID);
    return comScriptProcess.getName();

  }

  /**
   * Generate the split align log file for the given axis ID
   * 
   * @param axisID
   */
  public void generateAlignLogs(AxisID axisID) {
    AlignLogGenerator alignLogGenerator = new AlignLogGenerator(axisID);

    try {
      alignLogGenerator.run();
    }
    catch (IOException except) {
      appManager.openMessageDialog(
        "Unable to create alignlog files",
        "Alignlog Error");
    }
  }

  /**
   * Run the transferfid script
   */
  public String transferFiducials(TransferfidParam transferfidParam)
    throws SystemProcessException {
    AxisID axisID = AxisID.SECOND;
    //Run transferfid on the destination axis.
    if (transferfidParam.isBToA()) {
      axisID = AxisID.FIRST;
    }

    BackgroundProcess backgroundProcess =
      startBackgroundProcess(transferfidParam.getCommandString(), axisID);
    transferfidCommandLine = backgroundProcess.getCommandLine();
    return backgroundProcess.getName();
  }

  /**
   * Run the appropriate sample com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run sample.com on.
   */
  public String createSample(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required tiltalign command
    //
    String command = "sample" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null, axisID);
    return comScriptProcess.getName();

  }

  /**
   * Run the appropriate tomopitch com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run tomoptich on.
   */
  public String tomopitch(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required tiltalign command
    //
    String command = "tomopitch" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null, axisID);
    return comScriptProcess.getName();

  }

  /**
   * Run the appropriate newst com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run newst on.
   */
  public String newst(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required newst command
    //
    String command = "newst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    NewstProcessMonitor newstProcessMonitor =
      new NewstProcessMonitor(appManager, axisID);
    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, newstProcessMonitor, axisID);
    return comScriptProcess.getName();

  }

  /**
   * Run the appropriate tilt com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run tilt on.
   */
  public String tilt(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required tilt command
    //
    String command = "tilt" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    TiltProcessMonitor tiltProcessMonitor =
      new TiltProcessMonitor(appManager, axisID);
    ComScriptProcess comScriptProcess =
      startComScript(command, tiltProcessMonitor, axisID);

    return comScriptProcess.getName();

  }

  /**
   * Execute the setupcombine script
   * 
   * @param combineParam
   *          A read-only object containing the parameters for setupcombine
   *          script
   */
  public void setupCombineScripts(ConstMetaData metaData)
    throws BadComScriptException, IOException {

    SetupCombine setupCombine = new SetupCombine(metaData);

    int exitValue = setupCombine.run();

    if (exitValue != 0) {
      System.err.println("Exit value: " + String.valueOf(exitValue));

      //  Compile the exception message from the stderr stream
      String[] stdError = setupCombine.getStdError();
      if (stdError.length < 1) {
        stdError = new String[1];
        stdError[0] =
          "Get David to add some std error reporting to setupCombine";
      }
      StringBuffer buffer = new StringBuffer();
      buffer.append("SetupCombine Error\n");
      buffer.append("Standard error output:\n");
      for (int i = 0; i < stdError.length; i++) {
        buffer.append(stdError[i]);
        buffer.append("\n");
      }

      throw (new BadComScriptException(buffer.toString()));
    }
  }

  /**
   * Run the imod2patch command
   */
  public void modelToPatch() throws SystemProcessException {
    //  Copy the old patch.out to patch.out~
    SystemProgram savePatchOut =
      new SystemProgram("mv -f patch.out patch.out~");
    savePatchOut.setWorkingDirectory(new File(System.getProperty("user.dir")));
    savePatchOut.setDebug(ApplicationManager.isDebug());

    savePatchOut.run();
    if (savePatchOut.getExitValue() != 0) {
      String message = "";
      String[] stderr = savePatchOut.getStdError();

      for (int i = 0; i < stderr.length; i++) {
        message = message + stderr[i] + "\n";
      }
      throw new SystemProcessException(message);
    }

    // Convert the new patchvector.mod
    String imodBinPath =
      ApplicationManager.getIMODDirectory().getAbsolutePath()
        + File.separator
        + "bin"
        + File.separator;
    String commandLine = imodBinPath + "imod2patch patch_vector.mod patch.out";
    SystemProgram patch2imod = new SystemProgram(commandLine);
    patch2imod.setWorkingDirectory(new File(System.getProperty("user.dir")));
    patch2imod.setDebug(ApplicationManager.isDebug());

    patch2imod.run();
    if (patch2imod.getExitValue() != 0) {
      String message = "";
      String[] stderr = patch2imod.getStdError();

      for (int i = 0; i < stderr.length; i++) {
        message = message + stderr[i] + "\n";
      }
      throw new SystemProcessException(message);
    }
  }

  /**
   * Run the combine com file
   * 
   * @param axisID
   *          the AxisID to run tilt on.
   */
  public String combine() throws SystemProcessException {
    //  Create the required combine command
    String command = "combine.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the solvematchshift com file
   * 
   * @return String
   */
  public String solvematchshift() throws SystemProcessException {
    //  Create the required combine command
    String command = "solvematchshift.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the solvematchmod com file
   * 
   * @return String
   */
  public String solvematchmod() throws SystemProcessException {
    //  Create the required combine command
    String command = "solvematchmod.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the matchvol1 com file
   * 
   * @return String
   */
  public String matchvol1() throws SystemProcessException {
    //  Create the required combine command
    String command = "matchvol1.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the patchcorr com file
   * 
   * @return String
   */
  public String patchcorr() throws SystemProcessException {
    //  Create the required combine command
    String command = "patchcorr.com";
    //  Create the process monitor
    PatchcorrProcessWatcher patchcorrProcessWatcher =
      new PatchcorrProcessWatcher(appManager, AxisID.FIRST);

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, patchcorrProcessWatcher, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the matchorwarp com file
   * 
   * @return String
   */
  public String matchorwarp() throws SystemProcessException {
    //  Create the required combine command
    String command = "matchorwarp.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the volcombine com file
   * 
   * @return String
   */
  public String volcombine() throws SystemProcessException {
    //  Create the required combine command
    String command = "volcombine.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startComScript(command, null, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run trimvol
   */
  public String trimVolume(TrimvolParam trimvolParam)
    throws SystemProcessException {
    BackgroundProcess backgroundProcess =
      startBackgroundProcess(trimvolParam.getCommandString(), AxisID.ONLY);
    return backgroundProcess.getName();
  }

  /**
   * Run the comand specified by the argument string
   */
  public String test(String commandLine) {
    BackgroundProcess command = new BackgroundProcess(commandLine, this);
    command.setWorkingDirectory(new File(System.getProperty("user.dir")));
    command.setDebug(ApplicationManager.isDebug());
    command.start();

    if (ApplicationManager.isDebug()) {
      System.err.println("Started " + commandLine);
      System.err.println("  Name: " + command.getName());
    }
    return command.getName();
  }

  /**
   * Start an arbtrary command as an unmanaged background thread
   */
  private void startSystemProgramThread(String command) {

    // Initialize the SystemProgram object
    SystemProgram sysProgram = new SystemProgram(command);
    sysProgram.setWorkingDirectory(new File(System.getProperty("user.dir")));
    sysProgram.setDebug(ApplicationManager.isDebug());

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    sysProgThread.start();
    if (ApplicationManager.isDebug()) {
      System.err.println("Started " + command);
      System.err.println(
        "  working directory: " + System.getProperty("user.dir"));
    }
  }

  /**
   * Start a managed command script for the specified axis
   * 
   * @param command
   * @param axisID
   * @return
   */
  private ComScriptProcess startComScript(
    String command,
    Runnable processMonitor,
    AxisID axisID)
    throws SystemProcessException {

    isAxisBusy(axisID);

    //  Run the script as a thread in the background
    ComScriptProcess comScriptProcess = new ComScriptProcess(command, this);
    comScriptProcess.setWorkingDirectory(
      new File(System.getProperty("user.dir")));
    comScriptProcess.setDebug(ApplicationManager.isDebug());
    comScriptProcess.setDemoMode(appManager.isDemo());
    comScriptProcess.start();

    // Map the thread to the correct axis
    mapAxisThread(comScriptProcess, axisID);

    if (ApplicationManager.isDebug()) {
      System.err.println("Started " + command);
      System.err.println("  Name: " + comScriptProcess.getName());
    }

    Thread processMonitorThread = null;
    // Replace the process monitor with a DemoProcessMonitor if demo mode is on
    if (appManager.isDemo()) {
      processMonitor =
        new DemoProcessMonitor(
          appManager,
          axisID,
          command,
          comScriptProcess.getDemoTime());
    }

    //	Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the comScriptProcess, this ensures
      // that log file has already been moved
      while (!comScriptProcess.isStarted()) {
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
          break;
        }
      }
      processMonitorThread = new Thread(processMonitor);
      processMonitorThread.start();
      mapAxisProcessMonitor(processMonitorThread, axisID);
    }

    return comScriptProcess;
  }

  /**
   * A message specifying that a com script has finished execution
   * 
   * @param script
   *          the ComScriptProcess execution object that finished
   * @param exitValue
   *          the exit value for the com script
   */
  public void msgComScriptDone(ComScriptProcess script, int exitValue) {
    if (exitValue != 0) {
      String[] stdError = script.getStdError();
      String[] combined;
      //    Is the last string "Killed"
      if ((stdError.length > 0)
        && (stdError[stdError.length - 1].trim().equals("Killed"))) {
        combined = new String[1];
        combined[0] = "<html>Terminated: " + script.getScriptName();
      }
      else {
        String[] message = script.getErrorMessage();
        combined = new String[message.length + stdError.length + 5];
        int j = 0;
        combined[j++] = "<html>Com script failed: " + script.getScriptName();
        combined[j++] = "  ";
        combined[j++] = "<html><U>Log file errors:</U>";

        for (int i = 0; i < message.length; i++, j++) {
          combined[j] = message[i];
        }
        combined[j++] = "  ";
        combined[j++] = "<html><U>Standard error output:</U>";
        for (int i = 0; i < stdError.length; i++, j++) {
          combined[j] = stdError[i];
        }
      }
      appManager.openMessageDialog(
        combined,
        script.getScriptName() + " terminated");
    }
    else {
      // Script specific post processing

      if (script.getScriptName().equals("aligna.com")) {
        generateAlignLogs(AxisID.FIRST);
      }
      if (script.getScriptName().equals("alignb.com")) {
        generateAlignLogs(AxisID.SECOND);
      }
      if (script.getScriptName().equals("align.com")) {
        generateAlignLogs(AxisID.ONLY);
      }

      if (script.getScriptName().equals("tomopitcha.com")) {
        appManager.openTomopitchLog(AxisID.FIRST);
      }
      if (script.getScriptName().equals("tomopitchb.com")) {
        appManager.openTomopitchLog(AxisID.SECOND);
      }
      if (script.getScriptName().equals("tomopitch.com")) {
        appManager.openTomopitchLog(AxisID.ONLY);
      }

      String[] warningMessages = script.getWarningMessage();
      String[] dialogMessage;
      if (warningMessages != null && warningMessages.length > 0) {
        dialogMessage = new String[warningMessages.length + 2];
        dialogMessage[0] = "Com script: " + script.getScriptName();
        dialogMessage[1] = "<html><U>Warnings:</U>";
        int j = 2;
        for (int i = 0; i < warningMessages.length; i++) {
          dialogMessage[j++] = warningMessages[i];
        }
        appManager.openMessageDialog(
          dialogMessage,
          script.getScriptName() + " warnings");
      }

    }

    //  Null out the correct thread
    // Interrupt the process monitor and nulll out the appropriate references
    if (threadAxisA == script) {
      if (processMonitorA != null) {
        processMonitorA.interrupt();
        processMonitorA = null;
      }
      threadAxisA = null;
    }
    if (threadAxisB == script) {
      if (processMonitorB != null) {
        processMonitorB.interrupt();
        processMonitorB = null;
      }
      threadAxisB = null;
    }

    //  Inform the app manager that this process is complete
    appManager.processDone(script.getName(), exitValue);
  }

  /**
   * Start a managed background process
   * 
   * @param command
   * @param axisID
   * @throws SystemProcessException
   */
  private BackgroundProcess startBackgroundProcess(
    String command,
    AxisID axisID)
    throws SystemProcessException {

    isAxisBusy(axisID);

    BackgroundProcess backgroundProcess = new BackgroundProcess(command, this);
    backgroundProcess.setWorkingDirectory(
      new File(System.getProperty("user.dir")));
    backgroundProcess.setDemoMode(appManager.isDemo());
    backgroundProcess.setDebug(ApplicationManager.isDebug());
    backgroundProcess.start();
    if (ApplicationManager.isDebug()) {
      System.err.println("Started " + command);
      System.err.println("  Name: " + backgroundProcess.getName());
    }

    mapAxisThread(backgroundProcess, axisID);
    return backgroundProcess;
  }

  /**
   * A message specifying that a background process has finished execution
   * 
   * @param script
   *          the BackgroundProcess execution object that finished
   * @param exitValue
   *          the exit value for the process
   */
  public void msgBackgroundProcessDone(
    BackgroundProcess process,
    int exitValue) {

    //  Check to see if the exit value is non-zero
    if (exitValue != 0) {
      String[] stdError = process.getStdError();
      String[] message;

      // Is the last string "Killed"
      if ((stdError.length > 0)
        && (stdError[stdError.length - 1].trim().equals("Killed"))) {
        message = new String[1];
        message[0] = "<html>Terminated: " + process.getCommandLine();
      }
      else {
        int j = 0;
        message = new String[stdError.length + 3];
        message[j++] = "<html>Command failed: " + process.getCommandLine();
        message[j++] = "  ";
        message[j++] = "<html><U>Standard error output:</U>";
        for (int i = 0; i < stdError.length; i++, j++) {
          message[j] = stdError[i];
        }
      }
      appManager.openMessageDialog(
        message,
        process.getCommand() + " terminated");
    }

    // Another possible error message source is ERROR: in the stdout stream
    String[] stdOutput = process.getStdOutput();
    ArrayList errors = new ArrayList();
    boolean foundError = false;
    for (int i = 0; i < stdOutput.length; i++) {
      if (!foundError) {
        int index = stdOutput[i].indexOf("ERROR:");
        if (index != -1) {
          foundError = true;
          errors.add(stdOutput[i]);
        }
      }
      else {
        errors.add(stdOutput[i]);
      }
    }
    String[] errorMessage =
      (String[]) errors.toArray(new String[errors.size()]);

    if (errorMessage.length > 0) {
      appManager.openMessageDialog(errorMessage, "Background Process Error");
    }

    // Command succeeded, check to see if we need to show any application
    // specific info
    else {
      if (process.getCommandLine().equals(transferfidCommandLine)) {
        handleTransferfidMessage(process);
      }
    }

    // Null the reference to the appropriate thread
    if (process == threadAxisA) {
      threadAxisA = null;
    }
    if (process == threadAxisB) {
      threadAxisB = null;
    }

    //	Inform the app manager that this process is complete
    appManager.processDone(process.getName(), exitValue);
  }

  /**
   * Unique case to parse the output of transferfid and save it to a file
   * 
   * @param process
   */
  private void handleTransferfidMessage(BackgroundProcess process) {
    try {

      //  Write the standard output to a the log file
      String[] stdOutput = process.getStdOutput();
      BufferedWriter fileBuffer =
        new BufferedWriter(
          new FileWriter(System.getProperty("user.dir") + "/transferfid.log"));

      for (int i = 0; i < stdOutput.length; i++) {
        fileBuffer.write(stdOutput[i]);
        fileBuffer.newLine();
      }
      fileBuffer.close();

      //  Show a log file window to the user
      TextPageWindow logFileWindow = new TextPageWindow();
      logFileWindow.setVisible(
        logFileWindow.setFile(
          System.getProperty("user.dir") + File.separator + "transferfid.log"));
    }
    catch (IOException except) {
      appManager.openMessageDialog(
        except.getMessage(),
        "Transferfid log error");
    }
  }
  /**
   * Save the process thread reference for the appropriate axis
   * 
   * @param thread
   * @param axisID
   */
  private void mapAxisThread(SystemProcessInterface thread, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadAxisB = thread;
    }
    else {
      threadAxisA = thread;
    }
  }

  /**
   * Check to see if specified axis is busy, throw a system a
   * ProcessProcessException if it is.
   * 
   * @param axisID
   * @throws SystemProcessException
   */
  private void isAxisBusy(AxisID axisID) throws SystemProcessException {
    // Check to make sure there is not another process already running on this
    // axis.
    if (axisID == AxisID.SECOND) {
      if (threadAxisB != null) {
        throw new SystemProcessException("A process is already executing in the current axis");
      }
    }
    else {
      if (threadAxisA != null) {
        throw new SystemProcessException("A process is already executing in the current axis");
      }
    }
  }

  /**
   * Save the process monitor thread reference for the appropriate axis
   * 
   * @param processMonitor
   * @param axisID
   */
  private void mapAxisProcessMonitor(Thread processMonitor, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      processMonitorB = processMonitor;
    }
    else {
      processMonitorA = processMonitor;
    }
  }

  /**
   * Kill the thread for the specified axis
   */
  public void kill(AxisID axisID) {
    String processID = "";
    if (axisID == AxisID.SECOND) {
      if (threadAxisB != null) {
        processID = threadAxisB.getShellProcessID();
      }
    }
    else {
      System.out.print(threadAxisA);
      if (threadAxisA != null) {
        processID = threadAxisA.getShellProcessID();
      }
    }

    //  Loop over killing the children until there are none left
    if (!processID.equals("")) {
      String[] children;
      while ((children = getChildProcessList(processID)) != null) {
        String killCommand = "kill ";
        for (int i = 0; i < children.length; i++) {
          killCommand = killCommand + children[i] + " ";
        }

        SystemProgram kill = new SystemProgram(killCommand);
        kill.run();
      }

      SystemProgram killShell = new SystemProgram("kill " + processID);
      killShell.run();
    }
  }

  /**
   * Return a string array of the PIDs of children processes for the specified
   * process.
   * 
   * @param processID
   * @return A string array of the child processes or null if they don't exist
   */
  private String[] getChildProcessList(String processID) {
//    System.out.println("in getChildProcessList: processID=" + processID);
    //  Run the appropriate version of ps
    SystemProgram ps = new SystemProgram("ps -l");
    ps.run();

    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
      }
    }
    //  Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    ArrayList childrenPID = new ArrayList();
    String[] fields;
    for (int i = 1; i < stdout.length; i++) {
      fields = stdout[i].trim().split("\\s+");
//      System.out.println("fields: PID=" + fields[idxPID] + ",PPID=" + fields[idxPPID] + ",name=" + fields[13]);
      if (fields[idxPPID].equals(processID)) {
        childrenPID.add(fields[idxPID]);
//        System.out.println("added : PID=" + fields[idxPID] + ",PPID=" + fields[idxPPID] + ",name=" + fields[13]);
      }
    }

    // If there are no children return null
    if (childrenPID.size() == 0) {
      return null;
    }

    // Connvert the ArrayList into a String[]
    String[] children =
      (String[]) childrenPID.toArray(new String[childrenPID.size()]);
    return children;
  }

}
