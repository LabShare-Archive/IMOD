package etomo.process;

import etomo.type.AxisID;
import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.comscript.CopyTomoComs;
import etomo.comscript.BadComScriptException;
import etomo.comscript.SetupCombine;
import etomo.comscript.TransferfidParam;

import java.io.File;
import java.io.IOException;

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
      System.out.println("Exit value: " + String.valueOf(exitValue));

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
  public void eraser(AxisID axisID) {

    //  Create the required command string
    String command = "eraser" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Calculate the cross-correlation for the specified axis
   * @param axisID the AxisID to cross-correlate.
   */
  public void crossCorrelate(AxisID axisID) {

    //  Create the required command string
    String command = "xcorr" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Calculate the coarse alignment for the specified axis
   * @param axisID the identifyer of the axis to coarse align.
   */
  public void coarseAlign(AxisID axisID) {

    //  Create the required tiltalign command
    String command = "prenewst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
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
      stack = appManager.getFilesetName() + ".st ";
      xform = appManager.getFilesetName() + ".prexf ";
    }
    if (axisID == AxisID.FIRST) {
      stack = appManager.getFilesetName() + "a.st ";
      xform = appManager.getFilesetName() + "a.prexf ";
    }
    if (axisID == AxisID.SECOND) {
      stack = appManager.getFilesetName() + "b.st ";
      xform = appManager.getFilesetName() + "b.prexf ";
    }

    String command = "midas " + stack + xform;

    //  Start the system program thread
    startSystemProgramThread(command);
  }

  /**
  * Run the appropriate track com file for the given axis ID
  * @param axisID the AxisID to run track.com on.
  */
  public void fiducialModelTrack(AxisID axisID) {
    //
    //  Create the required beadtrack command
    //
    String command = "track" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Run the appropriate align com file for the given axis ID
   * @param axisID the AxisID to run align.com on.
   */
  public void fineAlignment(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "align" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Run the transferfid script
   */
  public void transferFiducials(TransferfidParam transferfidParam) {
    BackgroundProcess transferfid =
      new BackgroundProcess(transferfidParam.getCommandString(), this);
    System.out.println(transferfidParam.getCommandString());
    transferfid.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    transferfid.setDebug(appManager.isDebug());
    transferfid.start();
  }

  /**
   * Run the appropriate sample com file for the given axis ID
   * @param axisID the AxisID to run sample.com on.
   */
  public void createSample(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "sample" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Run the appropriate tomopitch com file for the given axis ID
   * @param axisID the AxisID to run tomoptich on.
   */
  public void tomopitch(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "tomopitch" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Run the appropriate newst com file for the given axis ID
   * @param axisID the AxisID to run newst on.
   */
  public void newst(AxisID axisID) {
    //
    //  Create the required newst command
    //
    String command = "newst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Run the appropriate tilt com file for the given axis ID
   * @param axisID the AxisID to run tilt on.
   */
  public void tilt(AxisID axisID) {
    //
    //  Create the required tilt command
    //
    String command = "tilt" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
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
      System.out.println("Exit value: " + String.valueOf(exitValue));

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
   * Run the combine com file
   * @param axisID the AxisID to run tilt on.
   */
  public void combine() {
    //
    //  Create the required tilt command
    //
    String command = "combine.com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Run the comand specified by the argument string
   */
  public void test(String commandLine) {
    BackgroundProcess command = new BackgroundProcess(commandLine, this);
    command.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    command.setDebug(appManager.isDebug());
    command.start();

    System.out.println("Started " + commandLine);
    System.out.println("  Name: " + command.getName());
  }
  
  /**
   * A message specifying that a com script has finished execution
   * @param script the RunComScript execution object that finished
   * @param exitValue the exit value for the com script
   */
  public void msgBackgroundProcessDone(BackgroundProcess process,
    int exitValue) {

    if (exitValue != 0) {
      String[] stdError = process.getStdError();
      String[] message = new String[stdError.length + 5];

      int j = 0;
      message[j++] = "<html>Command failed: " + process.getCommandLine();
      message[j++] = "  ";
      message[j++] = "<html><U>Standard error output:</U>";
      for (int i = 0; i < stdError.length; i++, j++) {
        message[j] = stdError[i];
      }

      appManager.openMessageDialog(message, process.getCommand() + " failed");
    }

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
  }

  //  Internal utility functions
  private void startSystemProgramThread(String command) {
    SystemProgram sysProgram = new SystemProgram(command);
    sysProgram.setWorkingDirectory(new File(appManager.getWorkingDirectory()));

    sysProgram.enableDebug(appManager.isDebug());

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    sysProgThread.start();
    System.out.println("Started " + command);
    System.out.println(
      "  working directory: " + appManager.getWorkingDirectory());
  }

  private void startComScript(String command) {
    //  Run the script as a thread in the background
    RunComScript comScript = new RunComScript(command, this);
    comScript.setWorkingDirectory(new File(appManager.getWorkingDirectory()));
    comScript.setEnableDebug(appManager.isDebug());
    comScript.setDemoMode(appManager.isDemo());
    comScript.start();

    System.out.println("Started " + command);
    System.out.println("  Name: " + comScript.getName());
  }

}
