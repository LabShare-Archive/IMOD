package etomo.process;

import etomo.type.AxisID;
import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.comscript.CopyTomoComs;
import etomo.comscript.BadComScriptException;
import etomo.comscript.SetupCombine;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.Calendar;

/**
 * <p>Description: This object manages the execution of com scripts in the
 * background and the opening and sending messages to imod.</p>
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
 * <p> Revision 2.5  2003/04/16 00:14:26  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.4  2003/03/26 00:52:42  rickg
 * <p> Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p> Revision 2.3  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.2  2003/02/24 23:35:24  rickg
 * <p> Added management of threads for each axis
 * <p> Added interrupt method  (doesn't work well with C shell scripts)
 * <p>
 * <p> Revision 2.1  2003/01/29 20:46:40  rickg
 * <p> Check debug mode for messages
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.14.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.14  2003/01/08 05:00:13  rickg
 * <p> Wrote transferfid log file and create stdout dialog box
 * <p>
 * <p> Revision 1.13  2003/01/08 03:56:17  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.12  2003/01/04 00:23:36  rickg
 * <p> Added a test method.
 * <p> TransferFiducials method reports command name
 * <p> instead of thread name.
 * <p>
 * <p> Revision 1.11  2003/01/03 00:56:00  rickg
 * <p> Added msgBackgroundProcessDone method
 * <p>
 * <p> Revision 1.10  2002/12/31 00:57:38  rickg
 * <p> Added transferFiducials method
 * <p>
 * <p> Revision 1.9  2002/12/30 20:38:11  rickg
 * <p> msgComScriptDone now invokes appropriate error
 * <p> or warning dialogs
 * <p>
 * <p> Revision 1.8  2002/10/22 21:38:33  rickg
 * <p> ApplicationManager now controls both demo and debug
 * <p> modes
 * <p>
 * <p> Revision 1.7  2002/10/14 22:45:07  rickg
 * <p> Enabled debug output for com scripts
 * <p>
 * <p> Revision 1.6  2002/10/14 19:02:37  rickg
 * <p> Opens a dialog through the app manager if a process returns
 * <p> an error
 * <p>
 * <p> Revision 1.5  2002/10/10 23:39:56  rickg
 * <p> refactored createCombineScripts to setupCombineScripts
 * <p>
 * <p> Revision 1.4  2002/10/10 18:47:16  rickg
 * <p> Enabled debugging output from the SystemProgram object
 * <p>
 * <p> Revision 1.3  2002/10/07 22:26:31  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.2  2002/09/19 21:43:30  rickg
 * <p> Moved imod control to ImodManager/ImodProcess objects
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ProcessManager {
  public static final String rcsid =
    "$Id$";

  ApplicationManager appManager;
  Thread threadAxisA = null;
  Thread threadAxisB = null;

  public ProcessManager(ApplicationManager appMgr) {
    appManager = appMgr;
  }

  /**
   * Run the copytomocoms script
   * @param metaData a read-only MetaData object containing the information
   * to run the copytomocoms script
   */
  public void setupComScripts(ConstMetaData metaData)
    throws BadComScriptException, IOException {

    CopyTomoComs copyTomoComs = new CopyTomoComs(metaData);

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
   * @param axisID the AxisID to cross-correlate.
   */
  public String eraser(AxisID axisID) {

    //  Create the required command string
    String command = "eraser" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Calculate the cross-correlation for the specified axis
   * @param axisID the AxisID to cross-correlate.
   */
  public String crossCorrelate(AxisID axisID) {

    //  Create the required command string
    String command = "xcorr" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Calculate the coarse alignment for the specified axis
   * @param axisID the identifyer of the axis to coarse align.
   */
  public String coarseAlign(AxisID axisID) {

    //  Create the required tiltalign command
    String command = "prenewst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run midas on the specified raw stack
   * @param axisID the AxisID to run midas on.
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

    String command = "midas " + stack + xform;

    //  Start the system program thread
    startSystemProgramThread(command);
  }

  /**
  * Run the appropriate track com file for the given axis ID
  * @param axisID the AxisID to run track.com on.
  */
  public String fiducialModelTrack(AxisID axisID) {
    //
    //  Create the required beadtrack command
    //
    String command = "track" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run the appropriate align com file for the given axis ID
   * @param axisID the AxisID to run align.com on.
   */
  public String fineAlignment(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "align" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run the transferfid script
   */
  public String transferFiducials(TransferfidParam transferfidParam) {
    BackgroundProcess transferfid =
      new BackgroundProcess(transferfidParam.getCommandString(), this);
    transferfid.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    transferfid.setDemoMode(appManager.isDemo());
    transferfid.setDebug(appManager.isDebug());
    transferfid.start();
    return transferfid.getName();
  }

  /**
   * Run the appropriate sample com file for the given axis ID
   * @param axisID the AxisID to run sample.com on.
   */
  public String createSample(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "sample" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run the appropriate tomopitch com file for the given axis ID
   * @param axisID the AxisID to run tomoptich on.
   */
  public String tomopitch(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "tomopitch" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run the appropriate newst com file for the given axis ID
   * @param axisID the AxisID to run newst on.
   */
  public String newst(AxisID axisID) {
    //
    //  Create the required newst command
    //
    String command = "newst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Run the appropriate tilt com file for the given axis ID
   * @param axisID the AxisID to run tilt on.
   */
  public String tilt(AxisID axisID) {
    //
    //  Create the required tilt command
    //
    String command = "tilt" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    return startComScript(command, axisID);
  }

  /**
   * Execute the setupcombine script
   * @param combineParam A read-only object containing the parameters for
   * setupcombine script
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
    savePatchOut.setWorkingDirectory(
      new File(appManager.getWorkingDirectory()));
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
    SystemProgram patch2imod =
      new SystemProgram("imod2patch patch_vector.mod patch.out");
    patch2imod.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
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
   * @param axisID the AxisID to run tilt on.
   */
  public String combine() {
    //  Create the required combine command
    String command = "combine.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the solvematchshift com file 
   * @return String
   */
  public String solvematchshift() {
    //  Create the required combine command
    String command = "solvematchshift.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the solvematchmod com file 
   * @return String
   */
  public String solvematchmod() {
    //  Create the required combine command
    String command = "solvematchmod.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the matchvol1 com file 
   * @return String
   */
  public String matchvol1() {
    //  Create the required combine command
    String command = "matchvol1.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the patchcorr com file 
   * @return String
   */
  public String patchcorr() {
    //  Create the required combine command
    String command = "patchcorr.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the matchorwarp com file 
   * @return String
   */
  public String matchorwarp() {
    //  Create the required combine command
    String command = "matchorwarp.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run the volcombine com file 
   * @return String
   */
  public String volcombine() {
    //  Create the required combine command
    String command = "volcombine.com";

    //  Start the com script in the background
    return startComScript(command, AxisID.ONLY);
  }

  /**
   * Run trimvol
   */
  public String trimVolume(TrimvolParam trimvolParam) {
    BackgroundProcess trimvol =
      new BackgroundProcess(trimvolParam.getCommandString(), this);
    System.out.println(trimvolParam.getCommandString());
    trimvol.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    trimvol.setDemoMode(appManager.isDemo());
    trimvol.setDebug(appManager.isDebug());
    trimvol.start();
    return trimvol.getName();
  }

  /**
   * Interrupt the thread for the specified axis
   * @param commandLine
   * @return String
   */
  public void interrupt(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (threadAxisB != null) {
        threadAxisB.interrupt();
        threadAxisB = null;
      }
    }
    else {
      if (threadAxisA != null) {
        threadAxisA.interrupt();
        threadAxisA = null;
      }
    }
  }
  /**
   * Run the comand specified by the argument string
   */
  public String test(String commandLine) {
    BackgroundProcess command = new BackgroundProcess(commandLine, this);
    command.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    command.setDebug(appManager.isDebug());
    command.start();

    System.err.println("Started " + commandLine);
    System.err.println("  Name: " + command.getName());

    return command.getName();
  }

  /**
   * A message specifying that a background process has finished execution
   * @param script the BackgroundProcess execution object that finished
   * @param exitValue the exit value for the process
   */
  public void msgBackgroundProcessDone(
    BackgroundProcess process,
    int exitValue) {

    if (exitValue != 0) {
      String[] stdError = process.getStdError();
      String[] message = new String[stdError.length + 3];

      int j = 0;
      message[j++] = "<html>Command failed: " + process.getCommandLine();
      message[j++] = "  ";
      message[j++] = "<html><U>Standard error output:</U>";
      for (int i = 0; i < stdError.length; i++, j++) {
        message[j] = stdError[i];
      }

      appManager.openMessageDialog(message, process.getCommand() + " failed");
    }

    // Command succeeded, check to see if we need to show any application
    // specific info
    else {
      if (process.getCommand().equals("transferfid")) {
        handleTransferfidMessage(process);
      }
    }
    appManager.processDone(process.getName(), exitValue);
  }

  /**
   * A message specifying that a com script has finished execution
   * @param script the RunComScript execution object that finished
   * @param exitValue the exit value for the com script
   */
  public void msgComScriptDone(RunComScript script, int exitValue) {

    if (exitValue != 0) {
      String[] message = script.getErrorMessage();
      String[] stdError = script.getStdError();
      String[] combined = new String[message.length + stdError.length + 5];

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

      appManager.openMessageDialog(
        combined,
        script.getScriptName() + " failed");
    }
    else {
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

    // Null out the appropriate thread reference
    if (threadAxisA == script) {
      threadAxisA = null;
    }
    if (threadAxisB == script) {
      threadAxisB = null;
    }
    appManager.processDone(script.getName(), exitValue);
  }

  //  Internal utility functions
  private void startSystemProgramThread(String command) {
    SystemProgram sysProgram = new SystemProgram(command);
    sysProgram.setWorkingDirectory(new File(appManager.getWorkingDirectory()));

    sysProgram.enableDebug(appManager.isDebug());

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    sysProgThread.start();
    System.err.println("Started " + command);
    System.err.println(
      "  working directory: " + appManager.getWorkingDirectory());
  }

  private String startComScript(String command, AxisID axisID) {
    //  Run the script as a thread in the background
    RunComScript comScript = new RunComScript(command, this);
    comScript.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    comScript.setEnableDebug(appManager.isDebug());
    comScript.setDemoMode(appManager.isDemo());
    comScript.start();

    if (appManager.isDebug()) {
      Calendar calendar = Calendar.getInstance();
      System.err.println("Started " + command);
      System.err.println("  Name: " + comScript.getName());
    }

    mapThreadAxis(comScript, axisID);
    return comScript.getName();
  }

  private void handleTransferfidMessage(BackgroundProcess process) {
    try {
      String[] stdOutput = process.getStdOutput();
      //  Write the standard output to a the log file

      BufferedWriter fileBuffer =
        new BufferedWriter(
          new FileWriter(
            appManager.getWorkingDirectory() + "/transferfid.log"));

      for (int i = 0; i < stdOutput.length; i++) {
        fileBuffer.write(stdOutput[i]);
        fileBuffer.newLine();
      }
      fileBuffer.close();

      //  Show a dialog box to the user
      String[] message = new String[stdOutput.length + 1];
      int j = 0;
      message[j++] = "<html><U>" + process.getCommand() + ": output</U>";
      for (int i = 0; i < stdOutput.length; i++, j++) {
        message[j] = stdOutput[i];
      }

      appManager.openMessageDialog(message, "Transferfid output");
    }
    catch (IOException except) {
      appManager.openMessageDialog(
        except.getMessage(),
        "Transferfid log error");
    }
  }

  private void mapThreadAxis(Thread thread, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadAxisB = thread;
    }
    else {
      threadAxisA = thread;
    }
  }
}
