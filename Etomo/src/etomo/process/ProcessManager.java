/**
 * <p>
 * Description: This object manages the execution of com scripts in the
 * background and the opening and sending messages to imod.  It also provides
 * an interface to executing some simple command sequences.
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
 * Revision 3.35  2004/08/25 21:00:03  sueh
 * bug# 508 removing diagnostic prints and adding kill by
 * group (killProcessGroup)
 *
 * Revision 3.34  2004/08/25 18:35:53  sueh
 * bug# 508 print diagnostics during kill
 *
 * Revision 3.33  2004/08/24 20:44:09  sueh
 * bug# 508 change BackgroundProcessMonitor.kill() to
 * killMonitor()
 *
 * Revision 3.32  2004/08/24 20:41:55  sueh
 * bug# 508 running killProcessAndDescendants() causes a delay.  If
 * BackgroundComScriptProcess.kill() is called before
 * killProcessAndDescendents(), it pops up an message box immediately
 * and then pauses for 4 to 8 seconds before it displays the message and
 * OK button.  Calling kill() after killProcessAndDescendants() puts the
 * delay before the message box is popped up, which is more like the
 * behavior of other dialogs.  It also makes more sense to kill the monitor
 * after killing the processes.
 *
 * Revision 3.31  2004/08/23 23:41:48  sueh
 * bug# 508 passed watched file into BackgroundComScriptPRocess
 * the same way it is passed to ComScriptPRocess.  remove 
 * unnecessary watchedFileNAme parameter from the startComScript(
 * ComScriptProcess, String, Runnable, AxisID, String watchedFileNAme)
 *
 * Revision 3.30  2004/08/19 02:39:18  sueh
 * bug# 508 Passing CombineComscriptState to combine() so it can be
 * passed to BackgroundComScriptProcess.  In kill(), when the thread is
 * an instance of BackgroundComScriptProcess, call
 * BackgroundComScriptProcess.setKilled() to stop the process monitor.
 * Added:
 * combine(CombineComscriptState combineComscriptState)
 * startBackgroundComScript(String command,
 *     Runnable processMonitor, AxisID axisID,
 *     ComscriptState comscriptState)
 * Changed:
 * kill(AxisID axisID)
 * Deleted:
 * combine()
 * startBackgroundComScript(
 *     String command,
 *     Runnable processMonitor,
 *     AxisID axisID)
 *
 * Revision 3.29  2004/08/06 23:08:55  sueh
 * bug# 508 modified combine() to use BackgroundComScriptProcess.
 * Modified startComScript to accept a ComScriptProcess parameter,
 * so that it can be used with either ComScriptProcess or
 * BackgroundComScriptProcess.
 *
 * Revision 3.28  2004/08/02 23:50:18  sueh
 * bug# 519 improving error handling in
 * setupNonFiducialAlign
 *
 * Revision 3.27  2004/08/02 23:02:37  sueh
 * bug# 519 call ApplicationManager.makeRawtltFile() if .rawtlt
 * doesn't exist
 *
 * Revision 3.26  2004/07/20 20:06:20  sueh
 * bug# 405 changing to ps axl because mac requires ps x when
 * terminal has been exited and windows doesn't understand
 * ps -x, but it does understand ps x
 *
 * Revision 3.25  2004/07/14 18:43:27  sueh
 * bug# 405 go back to ps -l:  its faster and works for all the
 * ways to run etomo that we have tested
 *
 * Revision 3.24  2004/07/13 23:06:09  sueh
 * bug# 405 get a list of child processes instead of one.  This
 * will speed things up when there are sibling processes.
 *
 * Revision 3.23  2004/07/12 22:35:39  sueh
 * bug# 405 get the command name correctly for Linux and
 * Windows
 *
 * Revision 3.22  2004/07/07 00:22:38  sueh
 * bug# 405 in kill() recursively killing the descendants of a process
 * before killing the process
 *
 * Revision 3.21  2004/07/02 22:01:32  rickg
 * Bug #491 Fixed xf detection logic
 *
 * Revision 3.20  2004/07/02 16:50:39  sueh
 * bug# 490 in startComScript() constucting ComScriptProcess with
 * watchedFileName.  In patchcorr() sending the watched file
 * name to startComScript()
 *
 * Revision 3.19  2004/06/28 22:10:29  rickg
 * Bug #470 Moved the fiducial mode file copying to the same sections
 * where the fiducialless is handled.
 *
 * Revision 3.18  2004/06/25 23:13:46  rickg
 * Bug #470 Added generatePreXG method.
 *
 * Revision 3.17  2004/06/18 05:46:27  rickg
 * Shortened sleep while waited com script to start, this should
 * help but not totally solve the extremely short run com problem
 *
 * Revision 3.16  2004/06/17 23:55:51  rickg
 * Bug #460 moved getting of current time into FileSizeProcessMonitor on
 * instantiation
 *
 * Revision 3.15  2004/06/17 23:34:17  rickg
 * Bug #460 added script starting time to differentiate old data files
 *
 * Revision 3.14  2004/06/15 20:07:09  rickg
 * Bug #469 Rotation.xf was not getting the axisID inserted into the
 * filename.
 *
 * Revision 3.13  2004/06/02 23:45:56  rickg
 * Bug #391 added logic for backward compatibility in manage .xf
 * and .tlt files
 *
 * Revision 3.12  2004/06/02 20:41:55  rickg
 * Bug #187 catch ERROR: in stdout and added to error message
 *
 * Revision 3.11  2004/05/27 22:51:04  rickg
 * Bug #391 setupFiducialAlign will now remove .xf and .tlt if its
 * source files don't exist
 *
 * Revision 3.10  2004/05/25 23:21:55  rickg
 * Bug #391 midas now opens the image rotated by the tilt axis
 * angle
 *
 * Revision 3.9  2004/05/21 02:16:26  sueh
 * bug# 83 adding VolcombineProcessMonitor to volcombine()
 *
 * Revision 3.8  2004/04/26 00:21:35  rickg
 * Comment change
 *
 * Revision 3.7  2004/04/22 23:32:24  rickg
 * bug #391 Added processing for non fid aligne
 * Added runCommand private method
 *
 * Revision 3.6  2004/04/16 01:56:26  sueh
 * bug# 409 In msgComScriptDone: passed the script's processName and axisID to
 * ApplicationManager.processDone, simplified log generation.
 *
 * Revision 3.5  2004/03/29 20:57:58  sueh
 * bug# 409 added mtffilter()
 *
 * Revision 3.4  2004/03/22 23:47:31  sueh
 * bug# 83 Use PatchcorrProcessWatcher when running patchcorr.
 *
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

package etomo.process;

import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.ui.TextPageWindow;
import etomo.util.InvalidParameterException;
import etomo.util.Utilities;
import etomo.comscript.CombineComscriptState;
import etomo.comscript.ComscriptState;
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
import java.util.HashMap;

public class ProcessManager {
  public static final String rcsid = "$Id$";

  ApplicationManager appManager;
  SystemProcessInterface threadAxisA = null;
  SystemProcessInterface threadAxisB = null;
  Thread processMonitorA = null;
  Thread processMonitorB = null;
  private HashMap killedList = new HashMap();

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
      System.err.println("copytomocoms command line: "
        + copyTomoComs.getCommandLine());
    }

    int exitValue = copyTomoComs.run();

    if (exitValue != 0) {
      System.err.println("Exit value: " + String.valueOf(exitValue));

      //  Compile the exception message from the stderr stream
      String[] stdError = copyTomoComs.getStdError();
      if (stdError.length < 1) {
        stdError = new String[1];
        stdError[0] = "Get David to add some std error reporting to copytomocoms";
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
    CCDEraserProcessMonitor ccdEraserProcessMonitor = new CCDEraserProcessMonitor(
      appManager, axisID);

    //  Create the required command string
    String command = "eraser" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      ccdEraserProcessMonitor, axisID);

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
    XcorrProcessWatcher xcorrProcessWatcher = new XcorrProcessWatcher(
      appManager, axisID);

    //  Create the required command string
    String command = "xcorr" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      xcorrProcessWatcher, axisID);

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
    PrenewstProcessMonitor prenewstProcessMonitor = new PrenewstProcessMonitor(
      appManager, axisID);

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      prenewstProcessMonitor, axisID);
    return comScriptProcess.getName();
  }

  /**
   * Generate the XG transform file for  
   * @param axisID
   * @throws SystemProcessException
   */
  public void generatePreXG(AxisID axisID) throws SystemProcessException {
    String[] xftoxg = new String[5];
    xftoxg[0] = ApplicationManager.getIMODBinPath() + "xftoxg";
    xftoxg[1] = "-NumberToFit";
    xftoxg[2] = "0";
    xftoxg[3] = appManager.getDatasetName() + axisID.getExtension() + ".prexf";
    xftoxg[4] = appManager.getDatasetName() + axisID.getExtension() + ".prexg";
    runCommand(xftoxg);
  }
  
  /**
   * Run both xftoxg and xproduct to create the _nonfid.xf for specified axis.
   * 
   * @param axisID
   */
  public void generateNonFidXF(AxisID axisID) throws SystemProcessException {
    String[] xfproduct = new String[4];
    xfproduct[0] = ApplicationManager.getIMODBinPath() + "xfproduct";
    xfproduct[1] = appManager.getDatasetName() + axisID.getExtension()
      + ".prexg";
    xfproduct[2] = "rotation" + axisID.getExtension() + ".xf";
    xfproduct[3] = appManager.getDatasetName() + axisID.getExtension()
      + "_nonfid.xf";

    runCommand(xfproduct);
  }

  /**
   * Setup the non fiducial alignment files
   * Copy the _nonfid.xf to .xf file
   * Copy the .rawtlt to the .tlt file
   * @param axisID
   */
  public void setupNonFiducialAlign(AxisID axisID) throws IOException,
    InvalidParameterException {
    String workingDirectory = System.getProperty("user.dir");
    String axisDataset = appManager.getDatasetName() + axisID.getExtension();

    File nonfidXF = new File(workingDirectory, axisDataset + "_nonfid.xf");
    File xf = new File(workingDirectory, axisDataset + ".xf");
    Utilities.copyFile(nonfidXF, xf);

    File rawtlt = new File(workingDirectory, axisDataset + ".rawtlt");
    File tlt = new File(workingDirectory, axisDataset + ".tlt");
    if (!rawtlt.exists()) {
      appManager.makeRawtltFile(axisID);
    }
    Utilities.copyFile(rawtlt, tlt);
  }

  /**
   * Setup the fiducial alignment files
   * If they exist copy the _fid.xf to .xf
   * Copy _fid.tlt to .tlt
   * @param axisID
   */
  public void setupFiducialAlign(AxisID axisID) throws IOException {
    String workingDirectory = System.getProperty("user.dir");
    String axisDataset = appManager.getDatasetName() + axisID.getExtension();
    // Files to be managed
    File xf = new File(workingDirectory, axisDataset + ".xf");
    File fidXF = new File(workingDirectory, axisDataset + "_fid.xf");
    File nonfidXF = new File(workingDirectory, axisDataset + "_nonfid.xf");
    File tlt = new File(workingDirectory, axisDataset + ".tlt");
    File fidTlt = new File(workingDirectory, axisDataset + "_fid.tlt");
    File tltxf = new File(workingDirectory, axisDataset + ".tltxf");
    if (tltxf.exists()) {
      // Align{|a|b}.com shows evidence of being run
      if (Utilities.fileExists(appManager.getMetaData(), "_fid.xf", axisID)) {
        // A recent align.com (or equivalent) has created the _fid.xf and
        // _fid.tlt (protected) transform and tilt files 
        Utilities.copyFile(fidXF, xf);
        Utilities.copyFile(fidTlt, tlt);
      }
      else {
        // An older align.com that just wrote out an .xf and .tlt was run
        // if the nonfid.xf was run it overwrote the the original data
        // delete the xf and tlt so that an error occurs
        if (nonfidXF.exists()) {
            xf.delete();
            tlt.delete();
        }
        else {
          // No nonfid{|a|b}.xf exists so the .xf and .tlt came from align
          // create the protected copies
          Utilities.copyFile(xf, fidXF);
          Utilities.copyFile(tlt, fidTlt);
        }
      }
    }
    else {
      // Align has not been run, delete any .xf and .tlt file so that they
      // are not accidentally used
      xf.delete();
      tlt.delete();
    }
  }

  /**
   * Run midas on the specified raw stack
   * 
   * @param axisID
   *          the AxisID to run midas on.
   */
  public void midasRawStack(AxisID axisID, float imageRotation) {

    //  Construct the command line strings
    String[] commandArray = new String[3];

    String options = "-a " + String.valueOf(-1 * imageRotation) + " ";
    String stack = appManager.getDatasetName() + axisID.getExtension() + ".st ";
    String xform = appManager.getDatasetName() + axisID.getExtension()
      + ".prexf ";

    String commandLine = ApplicationManager.getIMODBinPath() + "midas "
      + options + stack + xform;

    //  Start the system program thread
    startSystemProgramThread(commandLine);
  }

  /**
   * Run the appropriate track com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run track.com on.
   */
  public String fiducialModelTrack(AxisID axisID) throws SystemProcessException {
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
      appManager.openMessageDialog("Unable to create alignlog files",
        "Alignlog Error");
    }
  }

  /**
   * Copy the fiducial align files to the new protected names.  This is a
   * redundancy to handle existing com scripts, this functionality is also
   * present at the end of new align com scripts. 
   * @param axisID
   */
  public void copyFiducialAlignFiles(AxisID axisID) {
    String workingDirectory = System.getProperty("user.dir");
    String axisDataset = appManager.getDatasetName() + axisID.getExtension();

    try {
      if (Utilities.fileExists(appManager.getMetaData(), ".xf", axisID)) {
        File xf = new File(workingDirectory, axisDataset + ".xf");
        File fidXF = new File(workingDirectory, axisDataset + "_fid.xf");
        Utilities.copyFile(xf, fidXF);
      }
      if (Utilities.fileExists(appManager.getMetaData(), ".tlt", axisID)) {
        File tlt = new File(workingDirectory, axisDataset + ".tlt");
        File fidTlt = new File(workingDirectory, axisDataset + "_fid.tlt");
        Utilities.copyFile(tlt, fidTlt);
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      appManager.openMessageDialog("Unable to copy protected align files:",
        "Align Error");
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

    BackgroundProcess backgroundProcess = startBackgroundProcess(
      transferfidParam.getCommandString(), axisID);
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
    //  Create the required sample command
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
    NewstProcessMonitor newstProcessMonitor = new NewstProcessMonitor(
      appManager, axisID);
    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      newstProcessMonitor, axisID);
    return comScriptProcess.getName();

  }

  /**
   * Run the appropriate mtffilter com file for the given axis ID
   * 
   * @param axisID
   *          the AxisID to run newst on.
   */
  public String mtffilter(AxisID axisID) throws SystemProcessException {
    //
    //  Create the required newst command
    //
    String command = "mtffilter" + axisID.getExtension() + ".com";
    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null, axisID);
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

    //  Instantiate the process monitor
    TiltProcessMonitor tiltProcessMonitor = new TiltProcessMonitor(appManager,
      axisID);

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      tiltProcessMonitor, axisID);

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
        stdError[0] = "Get David to add some std error reporting to setupCombine";
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
    String[] mv = {"mv", "-f", "patch.out", "patch.out~"};
    runCommand(mv);

    // Convert the new patchvector.mod
    String[] imod2patch = {"imod2patch", "patch_vector.mod", "patch.out"};
    runCommand(imod2patch);
  }

  /**
   * Run the combine com file
   * 
   * @param axisID
   *          the AxisID to run tilt on.
   */
  public String combine(CombineComscriptState combineComscriptState) throws SystemProcessException {
    //  Create the required combine command
    String command = CombineComscriptState.COMSCRIPT_NAME + ".com";
    
    CombineProcessMonitor combineProcessMonitor = new CombineProcessMonitor(
      appManager, AxisID.ONLY, combineComscriptState);
      
    //  Start the com script in the background
    ComScriptProcess comScriptProcess =
      startBackgroundComScript(command, combineProcessMonitor, AxisID.ONLY, 
        combineComscriptState, CombineComscriptState.COMSCRIPT_WATCHED_FILE);
    return comScriptProcess.getName();

  }

  /**
   * Run the solvematch com file
   * 
   * @return String
   */
  public String solvematch() throws SystemProcessException {
    //  Create the required solvematch command
    String command = "solvematch.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null,
      AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the matchvol1 com file
   * 
   * @return String
   */
  public String matchvol1() throws SystemProcessException {
    //  Create the required matchvol1 command
    String command = "matchvol1.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null,
      AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the patchcorr com file
   * 
   * @return String
   */
  public String patchcorr() throws SystemProcessException {
    //  Create the required patchcorr command
    String command = "patchcorr.com";
    //  Create the process monitor
    PatchcorrProcessWatcher patchcorrProcessWatcher = new PatchcorrProcessWatcher(
      appManager, AxisID.FIRST);

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      patchcorrProcessWatcher, AxisID.ONLY, "patch.out");
    return comScriptProcess.getName();

  }

  /**
   * Run the matchorwarp com file
   * 
   * @return String
   */
  public String matchorwarp() throws SystemProcessException {
    //  Create the required matchorwarp command
    String command = "matchorwarp.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, null,
      AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run the volcombine com file
   * 
   * @return String
   */
  public String volcombine() throws SystemProcessException {
    VolcombineProcessMonitor volcombineProcessMonitor = new VolcombineProcessMonitor(
      appManager, AxisID.ONLY);
    //  Create the required volcombine command
    String command = "volcombine.com";

    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command,
      volcombineProcessMonitor, AxisID.ONLY);
    return comScriptProcess.getName();

  }

  /**
   * Run trimvol
   */
  public String trimVolume(TrimvolParam trimvolParam)
    throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(trimvolParam
      .getCommandString(), AxisID.ONLY);
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
   * Start an arbitrary command as an unmanaged background thread
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
      System.err.println("  working directory: "
        + System.getProperty("user.dir"));
    }
  }


  
  /**
   * Start a managed background command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  private ComScriptProcess startBackgroundComScript(String command, 
    Runnable processMonitor, AxisID axisID, 
    ComscriptState comscriptState, String watchedFileName)
    throws SystemProcessException {
    return startComScript(new BackgroundComScriptProcess(command, this, axisID,
      watchedFileName, (BackgroundProcessMonitor) processMonitor, comscriptState),
      command, processMonitor, axisID);
  }
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  private ComScriptProcess startComScript(
    String command,
    Runnable processMonitor,
    AxisID axisID)
    throws SystemProcessException {
    return startComScript(
      new ComScriptProcess(command, this, axisID, null),
      command,
      processMonitor,
      axisID);
  }
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  private ComScriptProcess startComScript(
    String command,
    Runnable processMonitor,
    AxisID axisID,
    String watchedFileName)
    throws SystemProcessException {
    return startComScript(
      new ComScriptProcess(command, this, axisID, watchedFileName),
      command,
      processMonitor,
      axisID);
  }
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  private ComScriptProcess startComScript(ComScriptProcess comScriptProcess, 
    String command,
    Runnable processMonitor,
    AxisID axisID)
    throws SystemProcessException {
    // Make sure there isn't something going on in the current axis
    isAxisBusy(axisID);

    // Run the script as a thread in the background
    comScriptProcess.setWorkingDirectory(new File(System
      .getProperty("user.dir")));
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
      processMonitor = new DemoProcessMonitor(appManager, axisID, command,
        comScriptProcess.getDemoTime());
    }

    //	Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the comScriptProcess, this ensures
      // that log file has already been moved
      while (!comScriptProcess.isStarted()) {
        try {
          Thread.sleep(100);
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
    System.err.println("msgComScriptDone:scriptName=" + script.getScriptName()
      + ",processName=" + script.getProcessName());
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
      appManager.openMessageDialog(combined, script.getScriptName()
        + " terminated");
    }
    else {
      // TODO: Should this script specific processing be handled by the
      // nextProcess attribute of the application manager.
      // Script specific post processing
      if (script.getProcessName() == ProcessName.ALIGN) {
        generateAlignLogs(script.getAxisID());
        copyFiducialAlignFiles(script.getAxisID());
      }
      if (script.getProcessName() == ProcessName.TOMOPITCH) {
        appManager.openTomopitchLog(script.getAxisID());
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
        appManager.openMessageDialog(dialogMessage, script.getScriptName()
          + " warnings");
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
    appManager.processDone(script.getName(), exitValue,
      script.getProcessName(), script.getAxisID());
  }

  /**
   * Start a managed background process
   * 
   * @param command
   * @param axisID
   * @throws SystemProcessException
   */
  private BackgroundProcess startBackgroundProcess(String command, AxisID axisID)
    throws SystemProcessException {

    isAxisBusy(axisID);

    BackgroundProcess backgroundProcess = new BackgroundProcess(command, this);
    backgroundProcess.setWorkingDirectory(new File(System
      .getProperty("user.dir")));
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
  public void msgBackgroundProcessDone(BackgroundProcess process, int exitValue) {

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
      appManager.openMessageDialog(message, process.getCommand()
        + " terminated");
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
    String[] errorMessage = (String[]) errors
      .toArray(new String[errors.size()]);

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
    appManager.processDone(process.getName(), exitValue, null, null);
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
      BufferedWriter fileBuffer = new BufferedWriter(new FileWriter(System
        .getProperty("user.dir")
        + "/transferfid.log"));

      for (int i = 0; i < stdOutput.length; i++) {
        fileBuffer.write(stdOutput[i]);
        fileBuffer.newLine();
      }
      fileBuffer.close();

      //  Show a log file window to the user
      TextPageWindow logFileWindow = new TextPageWindow();
      logFileWindow.setVisible(logFileWindow.setFile(System
        .getProperty("user.dir")
        + File.separator + "transferfid.log"));
    }
    catch (IOException except) {
      appManager
        .openMessageDialog(except.getMessage(), "Transferfid log error");
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
        throw new SystemProcessException(
          "A process is already executing in the current axis");
      }
    }
    else {
      if (threadAxisA != null) {
        throw new SystemProcessException(
          "A process is already executing in the current axis");
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
    SystemProcessInterface thread = null;
    if (axisID == AxisID.SECOND) {
      thread = threadAxisB;
    }
    else {
      thread = threadAxisA;

    }
    if (thread != null) {
      processID = thread.getShellProcessID();
    }
    
    killProcessGroup(processID);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
      
    }
    killProcessAndDescendants(processID);
    
    if (thread instanceof BackgroundComScriptProcess) {
      ((BackgroundComScriptProcess) thread).killMonitor();
    }

    /*
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
    }*/
  }
  
  /**
   * Recursively kill all the descendents of a process and then kill the
   * process.  Function assumes that the process will continue spawning while
   * the descendant processes are being killed.  Function attempts to stop
   * spawning with a Stop signal.  The Stop signal may not work in all cases and
   * OS's, so the function refreshes the list of child processes until there are
   * no more child processes.  The function avoids getting stuck on an
   * unkillable process by recording each PID it sent a "kill -9" to.
   * 
   * The algorithm:
   * 1. Stop the root process.
   * 2. Go down to a leaf, stopping each process encountered.
   * 3. Kill the leaf.
   * 4. Go up to the parent of the killed leaf.
   * 5. If the parent is now a leaf, kill it and continue from step 4.
   * 6. If the parent is not a leaf, continue from step 2.
   * 
   * @param processID
   */
  protected void killProcessAndDescendants(String processID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    //try to prevent process from spawning with a SIGSTOP signal
    kill("-19", processID);

    //kill all decendents of process before killing process
    String[] childProcessIDList = null;
    do {
      //get unkilled child processes
      childProcessIDList = getChildProcessList(processID);
      if (childProcessIDList != null) {
        for (int i = 0; i < childProcessIDList.length; i++) {
          killProcessAndDescendants(childProcessIDList[i]);
        }
      }
    } while (childProcessIDList != null);
    //there are no more unkilled child processes so kill process with a SIGKILL
    //signal
    kill("-9", processID);
    //record killed process
    killedList.put(processID, "");
  }
  
  private void kill(String signal, String processID) {
    SystemProgram killShell = new SystemProgram("kill " + signal + " " + processID);
    killShell.run();
    System.out.println("kill " + signal + " " + processID + " at " + killShell.getRunTimestamp());
    Utilities.debugPrint("kill " + signal + " " + processID + " at " + killShell.getRunTimestamp());
  }
  
  protected void killProcessGroup(String processID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    long pid = Long.parseLong(processID);
    if (pid == 0 || pid == 1) {
      return;
    }
    long groupPid = pid * -1;
    String groupProcessID = Long.toString(groupPid);
    kill("-19", groupProcessID);
    kill("-9", groupProcessID);
  }


  /**
   * Return a PID of a child process for the specified parent process.  A new
   * ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used.  Only processes the have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  protected String getChildProcess(String processID) {
    Utilities.debugPrint("in getChildProcess: processID=" + processID);
    //ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram("ps axl");
    ps.run();

    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (found >= 3) {
        break;
      }
    }
    //  Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    String[] fields;
    for (int i = 1; i < stdout.length; i++) {
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
        && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint(
            "child found:PID="
              + fields[idxPID]
              + ",PPID="
              + fields[idxPPID]
              + ",name="
              + fields[idxCMD]);
        }
        return fields[idxPID];
      }
    }
    return null;
  }


  /**
   * Return a the PIDs of child processes for the specified parent process.  A
   * new ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used.  Only processes the have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  private String[] getChildProcessList(String processID) {
    Utilities.debugPrint("in getChildProcessList: processID=" + processID);
    //ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram("ps axl");
    ps.run();
    //System.out.println("ps axl date=" +  ps.getRunTimestamp());
    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int idxPGID = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (labels[i].equals("PGID")) {
        idxPGID = i;
      }
      if (found >= 3) {
        break;
      }
    }
    //  Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    ArrayList childrenPID = new ArrayList();
    String[] fields;
    //System.out.println(stdout[0]);
    for (int i = 1; i < stdout.length; i++) {
      //System.out.println(stdout[i]);
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
        && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint(
          "child found:PID="
            + fields[idxPID]
            + ",PPID="
            + fields[idxPPID]
            + ",name="
            + fields[idxCMD]);
        }
        childrenPID.add(fields[idxPID]);
      }
    }

    // If there are no children return null
    if (childrenPID.size() == 0) {
      return null;
    }

    // Connvert the ArrayList into a String[]
    String[] children = (String[]) childrenPID.toArray(new String[childrenPID
      .size()]);
    return children;
  }


  private void printPsOutput() {
    SystemProgram ps = new SystemProgram("ps axl");
    ps.run();
    System.out.println("ps axl date=" +  ps.getRunTimestamp());
    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();

    System.out.println(stdout[0]);
    for (int i = 1; i < stdout.length; i++) {
      System.out.println(stdout[i]);
    }
  }

  /**
   * Execute the command and arguments in commandAarray immediately.
   * @param commandArray
   * @throws SystemProcessException
   */
  private void runCommand(String[] commandArray) throws SystemProcessException {
    SystemProgram systemProgram = new SystemProgram(commandArray);
    systemProgram.setWorkingDirectory(new File(System.getProperty("user.dir")));
    systemProgram.setDebug(ApplicationManager.isDebug());

    systemProgram.run();
    if (systemProgram.getExitValue() != 0) {
      String message = "";
      // Copy any stderr output to the message
      String[] stderr = systemProgram.getStdError();
      for (int i = 0; i < stderr.length; i++) {
        message = message + stderr[i] + "\n";
      }

      // Also scan stdout for ERROR: lines
      String[] stdOutput = systemProgram.getStdOutput();
      boolean foundError = false;
      for (int i = 0; i < stdOutput.length; i++) {
        if (!foundError) {
          int index = stdOutput[i].indexOf("ERROR:");
          if (index != -1) {
            foundError = true;
            message = message + stdOutput[i];
          }
        }
        else {
          message = message + stdOutput[i];
        }
      }
      throw new SystemProcessException(message);
    }
  }
}