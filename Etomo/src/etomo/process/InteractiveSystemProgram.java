package etomo.process;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.comscript.Command;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.Utilities;

/*
 * <p>Description: InteractiveSystemProgram implements a Runable class that can
 * execute an arbitrary program under the host operating system.</p>
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
 * <p> Revision 3.12  2010/05/10 14:43:40  sueh
 * <p> In run catching a null pointer exception if process is null.  This is a situation that is unlikely to happen.  Found it during test on Windows 7-64.
 * <p>
 * <p> Revision 3.11  2010/04/28 16:20:29  sueh
 * <p> bug# 1344 Added closeOutputImageFile.
 * <p>
 * <p> Revision 3.10  2007/02/05 22:58:38  sueh
 * <p> bug# 962 Made EtomoNumber type info an inner class.
 * <p>
 * <p> Revision 3.9  2006/06/21 15:48:39  sueh
 * <p> bug# 581 Added setCurrentStdInput() so that etomo can communicate with
 * <p> imodqtassist via stdin.
 * <p>
 * <p> Revision 3.8  2006/05/22 22:48:26  sueh
 * <p> bug# 577 Removed constructors which accepted a String command.
 * <p>
 * <p> Revision 3.7  2006/05/11 19:52:19  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.6  2005/11/19 02:27:15  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetachedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 3.5  2005/07/29 00:52:00  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.4  2005/04/25 20:47:39  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.3  2005/01/08 01:48:02  sueh
 * <p> bug# 578 Command interface has changed - update calls.
 * <p>
 * <p> Revision 3.2  2004/12/04 00:58:22  sueh
 * <p> bug# 569 Handling directory paths with spaces:  converting from a
 * <p> command line to a command array to prevent the command line from
 * <p> being split on white space.
 * <p>
 * <p> Revision 3.1  2004/11/19 23:22:01  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.5  2004/11/16 02:21:59  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 3.0.6.4  2004/11/12 22:53:53  sueh
 * <p> bug# 520 Removed pass-through commands to Command.  Added
 * <p> Command getCommand().
 * <p>
 * <p> Revision 3.0.6.3  2004/10/28 17:05:51  sueh
 * <p> bug# 520 Rename oldOutputFileTime to outputFileLastModified.
 * <p>
 * <p> Revision 3.0.6.2  2004/10/21 02:43:00  sueh
 * <p> bug# 520 Created a constructor which takes a Command interface.  This
 * <p> lets ProcessManager and JoinProcessManager do non-generic post
 * <p> processing for this program type.  Added functions to be used during post
 * <p> processing.
 * <p>
 * <p> Revision 3.0.6.1  2004/10/11 02:03:07  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/03/20 17:26:48  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/01/30 04:54:24  rickg
 * <p> Comments
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/07 22:25:48  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/17 23:22:58  rickg
 * <p> Complete basic operation
 * <p>
 * <p> </p>
 */
final class InteractiveSystemProgram implements Runnable {
  public static final String rcsid = "$Id$";

  private static final List<String> STD_OUTPUT = new ArrayList<String>();
  private static final List<String> STD_ERROR = new ArrayList<String>();

  private final EtomoNumber outputFileLastModified = new EtomoNumber(
      EtomoNumber.Type.LONG, "");

  private final BaseManager manager;
  private final AxisID axisID;

  private BaseProcessManager processManager = null;
  private String threadName = null;
  /**
   * The exit value of the command
   */
  private int exitValue = Integer.MIN_VALUE;
  /**
   * The command to run
   */
  private String commandLine = null;
  private String[] commandArray = null;
  private Command command = null;
  /**
   * The buffered IO streams connecting to the commmand.
   */
  private BufferedWriter inputBuffer = null;
  private BufferedReader outputBuffer = null;
  private BufferedReader errorBuffer = null;
  private File workingDirectory = null;
  private String exceptionMessage = "";
  private OutputStream cmdIn = null;
  private String commandAction = null;

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i>
   * @param command The string containng the command to run
   */
  InteractiveSystemProgram(final BaseManager manager, final String[] commandArray,
      final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArray = commandArray;
  }

  InteractiveSystemProgram(final BaseManager manager, final Command command,
      final BaseProcessManager processManager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.processManager = processManager;
    this.command = command;

    if (command == null) {
      throw new IllegalArgumentException("CommandParam is null.");
    }
    commandArray = command.getCommandArray();
    commandLine = command.getCommandLine();
  }

  void closeOutputImageFile() {
    if (command == null) {
      return;
    }
    manager.closeStaleFile(command.getOutputImageFileType(), axisID);
    manager.closeStaleFile(command.getOutputImageFileType2(), axisID);
  }

  void setCurrentStdInput(final String input) throws IOException {
    if (inputBuffer != null) {
      inputBuffer.write(input);
      inputBuffer.newLine();
      inputBuffer.flush();
    }
    if (cmdIn != null) {
      cmdIn.flush();
    }
  }

  AxisID getAxisID() {
    return axisID;
  }

  void setName(final String threadName) {
    this.threadName = threadName;
  }

  /**
   * Specify the directory in which the program should be run.  If the working
   * directory is not specified then current user.dir is the default.
   * @param workingDirectory a File object specifying the working directory
   */
  void setWorkingDirectory(final File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  String getCommandLine() {
    return commandLine;
  }

  String getCommandName() {
    if (command == null) {
      return null;
    }
    return command.getCommandName();
  }

  String getCommandAction() {
    if (commandAction == null) {
      return getCommandName();
    }
    return commandAction;
  }

  /**
   * Execute the command.
   */
  public void run() {
    // Setup the Process object and run the command
    Process process = null;
    File outputFile = null;
    if (command != null) {
      outputFile = command.getCommandOutputFile();
      outputFileLastModified.set(outputFile.lastModified());
    }
    try {
      if (workingDirectory == null) {
        File currentUserDirectory = new File(manager.getPropertyUserDir());
        if (commandArray != null) {
          commandAction = Utilities.getCommandAction(commandArray, null);
        }
        else {
          commandAction = Utilities.getCommandAction(commandLine);
        }
        if (commandArray != null) {
          process = Runtime.getRuntime().exec(commandArray, null, currentUserDirectory);
        }
        else {
          process = Runtime.getRuntime().exec(commandLine, null, currentUserDirectory);
        }
      }
      else {
        if (commandArray != null) {
          process = Runtime.getRuntime().exec(commandArray, null, workingDirectory);
        }
        else {
          process = Runtime.getRuntime().exec(commandLine, null, workingDirectory);
        }
      }

      // Create a buffered writer to handle the stdin, stdout and stderr
      // streams of the process
      cmdIn = process.getOutputStream();
      OutputStreamWriter cmdInWriter = new OutputStreamWriter(cmdIn);
      inputBuffer = new BufferedWriter(cmdInWriter);

      InputStream cmdOut = process.getInputStream();
      InputStreamReader cmdOutReader = new InputStreamReader(cmdOut);
      outputBuffer = new BufferedReader(cmdOutReader);

      InputStream cmdErr = process.getErrorStream();
      InputStreamReader cmdErrReader = new InputStreamReader(cmdErr);
      errorBuffer = new BufferedReader(cmdErrReader);
    }

    // TOD need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue
    // we can't rethrow the exception because it is a runnable implementation
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }

    try {
      process.waitFor();
      exitValue = process.exitValue();
      String msg = null;
      if (exitValue == 0
          && (msg = Utilities.getCommandActionMessage(commandAction)) != null) {
        System.err.println(msg);
      }
    }
    catch (InterruptedException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
    catch (NullPointerException e) {
      e.printStackTrace();
      exceptionMessage = e.getMessage();
    }

    if (processManager != null) {
      processManager.msgInteractiveSystemProgramDone(this, exitValue);
    }
  }

  /**
   * Send text to the program's standard input
   * @param line the string to send to the program's standard input
   */
  void writeStdin(final String line) {
    try {
      if (inputBuffer != null) {
        inputBuffer.write(line);
        inputBuffer.newLine();
        inputBuffer.flush();
      }
    }
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
  }

  /**
   * Read one line from the stdout buffer if available, if one isn't available
   * then null is returned
   */
  String readStdout() {
    String line = null;
    try {
      if (outputBuffer != null && outputBuffer.ready()) {
        line = outputBuffer.readLine();
        if (line != null) {
          STD_OUTPUT.add(line);
        }
      }
    }
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
    return line;
  }

  /**
   * Put all of stdout into stdOutput and then return stdOutput
   * @return
   */
  String[] getStdOutput() {
    while (readStdout() != null) {
    }
    return getStringArray(STD_OUTPUT);
  }

  /**
   * Put all of stderr into stdError and then return stdError
   * @return
   */
  String[] getStdError() {
    while (readStderr() != null) {
    }
    return getStringArray(STD_ERROR);
  }

  private String[] getStringArray(final List<String> list) {
    if (list == null || list.size() == 0) {
      return null;
    }
    if (list.size() == 1) {
      return new String[] { list.get(0) };
    }
    return list.toArray(new String[list.size()]);
  }

  Command getCommand() {
    return command;
  }

  ConstEtomoNumber getOutputFileLastModified() {
    return outputFileLastModified;
  }

  /**
   * Read one line from the stderr buffer if available, if one isn't available
   * then null is returned
   */
  String readStderr() {
    String line = null;
    try {
      if (errorBuffer != null && errorBuffer.ready()) {
        line = errorBuffer.readLine();
        if (line != null) {
          STD_ERROR.add(line);
        }
      }
    }
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
    return line;
  }

  int getExitValue() {
    return exitValue;
  }

  String getName() {
    return threadName;
  }

  String getExceptionMessage() {
    return exceptionMessage;
  }
}
