/**
 * <p>Description: SystemProgram provides a class to execute programs under the
 * host operating system.  The class provides access to stdin, stdout and
 * stderr streams and implements the Runnable interface so that it can be
 *  threaded.</p>
 *
 * <p> If the working directory is not explicitly set then the current working
 * directory for the command is set to the system property "user.dir"
 * 
 * <p>Copyright: Copyright (c) 2002 - 2005</p
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.43  2011/03/02 18:16:08  sueh
 * <p> bug# 1457 Popping up a warning when python is not available.
 * <p>
 * <p> Revision 3.42  2011/02/22 04:10:36  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.41  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.40  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.39  2009/03/31 22:33:31  sueh
 * <p> bug# 1204 Porting from 3-13.
 * <p>
 * <p> Revision 3.38  2009/03/23 17:09:52  sueh
 * <p> bug# 1204 Handling a null commandArray.
 * <p>
 * <p> Revision 3.37  2009/03/17 00:44:03  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.36  2009/01/13 22:29:19  sueh
 * <p> bug# 1171 popping up an error message when tcsh is missing.
 * <p>
 * <p> Revision 3.35  2008/02/16 01:55:09  sueh
 * <p> bug# 1080 Added collectionOutput, which is used to set
 * <p> OutputBufferManager.collectionOutput.  This allows intermittent
 * <p> processes to throw away their output and avoid growing without bounds.
 * <p>
 * <p> Revision 3.34  2007/05/25 00:24:39  sueh
 * <p> bug# 994 Added function destroy.
 * <p>
 * <p> Revision 3.33  2006/06/07 17:47:03  sueh
 * <p> bug# 766 Avoiding null pointer exceptions in run() by checking stdout and
 * <p> stderr.
 * <p>
 * <p> Revision 3.32  2006/06/05 16:10:46  sueh
 * <p> bug# 766 Removed the constructor with the multi line messages boolean
 * <p> because all the processes that are created with an ArrayList are already using
 * <p> multi line messages.
 * <p>
 * <p> Revision 3.31  2006/05/22 22:50:56  sueh
 * <p> bug# 577 Removed constructors which accepted a String command.
 * <p>
 * <p> Revision 3.30  2005/11/19 02:41:13  sueh
 * <p> bug# 744 Added another option to ProcessMessage constructor.  Made the
 * <p> constructor private and used getInstance functions to simply its use.
 * <p>
 * <p> Revision 3.29  2005/11/10 18:06:58  sueh
 * <p> bug# 758 Correcting a line that prints in the error log.  propertyUserDir
 * <p> may not always be the same as user.dir.
 * <p>
 * <p> Revision 3.28  2005/11/02 22:13:01  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.
 * <p> Removed errorList, infoList, multiLineMessages, and warningList.  Added
 * <p> processMessages.  Removed functions append, getErrors, getWarnings,
 * <p> and parse.  Added getProcessMessages.
 * <p>
 * <p> Revision 3.27  2005/10/28 18:53:36  sueh
 * <p> bug# 747 Rewrote parseWarnings() and renamed it parse().  Always parse
 * <p> all types of messages:  errors, warnings, and info (new).  Call parse in
 * <p> run().  Save messages in warningList, errorList, and infoList.  Pass
 * <p> multiLineMessages to the constructor so that multi-line messages can be
 * <p> handled.
 * <p>
 * <p> Revision 3.26  2005/09/10 02:13:27  sueh
 * <p> bug# 532 putting the creation of stdout and stderr in different functions so
 * <p> they can be handled differently in child classes.
 * <p>
 * <p> Revision 3.25  2005/09/10 01:53:00  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed,
 * <p> keeping separate lists of standard output for separate monitors, and
 * <p> setting a key phrase in OutputBufferManager so that only useful lines from
 * <p> standard output will be saved.
 * <p>
 * <p> Revision 3.24  2005/09/09 21:44:36  sueh
 * <p> bug# 532 Encaplulating the output array inside of OutputBufferManager.
 * <p>
 * <p> Revision 3.23  2005/08/27 22:33:40  sueh
 * <p> bug# 532 In setCurrentStdInput, throwing IOException instead of handling
 * <p> it.  This allows IntermittentSystemProgram to halt a failed intermittent
 * <p> command.
 * <p>
 * <p> Revision 3.22  2005/08/24 00:24:03  sueh
 * <p> bug# 532 added commented out print statements
 * <p>
 * <p> Revision 3.21  2005/08/22 22:07:33  sueh
 * <p> bug# 714 fixed the code that checks to see if anything is in
 * <p> propertyUserDir.
 * <p>
 * <p> Revision 3.20  2005/08/22 17:07:29  sueh
 * <p> bug# 532 Added boolean acceptInputWhileRunning to prevent the closing
 * <p> of the standard in too early.  AcceptInputWhileRunning defaults to false
 * <p> because it prevents several types of processes from running.
 * <p>
 * <p> Revision 3.19  2005/08/16 00:19:36  sueh
 * <p> bug# 532 standard input most be closed for com scripts to run.
 * <p>
 * <p> Revision 3.18  2005/08/15 18:27:07  sueh
 * <p> bug# 532 Added setCurrentStdInput() to allow a string to be sent to the
 * <p> process while it is running.  Changes run() to close the standard input
 * <p> stream after the process is done.
 * <p>
 * <p> Revision 3.17  2005/07/29 00:52:55  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.16  2005/06/21 00:45:52  sueh
 * <p> bug# 522 Fixed debug message which didn't show the space between
 * <p> each element of the commandArray.
 * <p>
 * <p> Revision 3.15  2005/05/10 17:36:04  sueh
 * <p> bug# 660 Added comment.
 * <p>
 * <p> Revision 3.14  2005/05/10 17:34:03  sueh
 * <p> bug# 660 Added a parseWarning() function that does handle multi-line
 * <p> warnings.
 * <p>
 * <p> Revision 3.13  2005/05/10 16:59:31  sueh
 * <p> bug# 660 Added parseWarning() to create an ArrayList of warnings from
 * <p> a String array.  Allows multi-line warnings, separated by blank lines.
 * <p>
 * <p> Revision 3.12  2005/04/25 20:49:46  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.11  2005/01/21 22:56:05  sueh
 * <p> bug# 509 bug# 591  Fixed a typo in a print statement.
 * <p>
 * <p> Revision 3.10  2004/11/19 23:25:59  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.9.2.4  2004/10/18 19:10:42  sueh
 * <p> bug# 520 Added getCommandLine().
 * <p>
 * <p> Revision 3.9.2.3  2004/10/11 02:04:39  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.9.2.2  2004/10/06 01:51:00  sueh
 * <p> bug# 520 Removed unnecessary import.
 * <p>
 * <p> Revision 3.9.2.1  2004/10/01 19:49:15  sueh
 * <p> bug# 520 Add a static parseError(String[]) function that can be used to
 * <p> parse stdOutput and stdError.
 * <p>
 * <p> Revision 3.9  2004/08/25 23:04:58  sueh
 * <p> bug# 508 fixed null pointer bug in getRunTimestamp()
 * <p>
 * <p> Revision 3.8  2004/08/25 18:36:40  sueh
 * <p> bug# 508 adding a timestamp that is set just before the process
 * <p> is run.
 * <p>
 * <p> Revision 3.7  2004/08/19 02:45:11  sueh
 * <p> bug# 508 Took the get process exit value functionality out of run() and
 * <p> placed it into a function so that it can be overridden.  Did the same
 * <p> with the sleep call that comes after exec'ing the process - called it
 * <p> waitForProcess.  WaitForProcess() doesn't mean much in this class,
 * <p> but it is really useful in BackgroundSystemProgram.
 * <p> Added:
 * <p> getProcessExitValue(Process process)
 * <p> waitForProcess()
 * <p> Changed:
 * <p> run()
 * <p>
 * <p> Revision 3.6  2004/05/25 23:23:03  rickg
 * <p> Bug #391 ignore IOExceptions in the stream monitor under the
 * <p> assumption that the program has ended
 * <p>
 * <p> Revision 3.5  2004/04/06 02:46:59  rickg
 * <p> Added method to get the standard error as a single string
 * <p>
 * <p> Revision 3.4  2004/03/11 02:25:00  sueh
 * <p> bug# 61 hiding print statements
 * <p>
 * <p> Revision 3.3  2004/03/11 00:01:53  sueh
 * <p> bug# 61 more to do
 * <p>
 * <p> Revision 3.2  2004/01/13 22:43:02  rickg
 * <p> Bug #376 Created a new Constructor that accepts a String[] for
 * <p> the command and arguments.  This also required restructing the
 * <p> exec call and splitting the exsiting constructor command strings.
 * <p>
 * <p> Revision 3.1  2003/12/01 19:57:08  rickg
 * <p> Close cleanup
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.9  2003/05/21 21:19:21  rickg
 * <p> Added started and done flags
 * <p> Shorted thread sleep times
 * <p>
 * <p> Revision 2.8  2003/05/15 20:20:03  rickg
 * <p> Removed extraneous debug printing
 * <p>
 * <p> Revision 2.7  2003/05/14 21:48:04  rickg
 * <p> Added Threaded reader for output (stdout and error) of command to
 * <p> keep command from hanging on full buffers.
 * <p>
 * <p> Revision 2.6  2003/05/14 14:37:44  rickg
 * <p> Workaround attempts for IO buffer bugs
 * <p>
 * <p> Revision 2.5  2003/05/09 06:07:00  rickg
 * <p> Work around for Java 1.4.1, yuk
 * <p>
 * <p> Revision 2.4  2003/05/08 23:17:33  rickg
 * <p> Set working directory for all cases.
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.3  2003/03/20 17:27:23  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.2  2003/02/24 23:46:37  rickg
 * <p> Handle interrupted exception to destroy the process
 * <p>
 * <p> Revision 2.1  2003/01/29 20:44:01  rickg
 * <p> Only write in debug mode if strings are available
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2002/12/28 00:18:23  rickg
 * <p> Standand error output identifies itself
 * <p>
 * <p> Revision 1.5  2002/10/10 23:39:00  rickg
 * <p> call println with the correct object for stderror output
 * <p>
 * <p> Revision 1.4  2002/10/10 18:52:30  rickg
 * <p> Added enableDebug method and functionality
 * <p> Closed IO stream of the process when done
 * <p> Swapped order of waitFor and process output reading
 * <p>
 * <p> Revision 1.3  2002/09/19 21:45:19  rickg
 * <p> Removed starting process stdout message
 * <p>
 * <p> Revision 1.2  2002/09/17 23:32:07  rickg
 * <p> Description update
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

package etomo.process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import etomo.Arguments;
import etomo.Arguments.DebugLevel;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.ui.swing.UIHarness;
import etomo.util.Utilities;

public class SystemProgram implements Runnable {
  public static final String rcsid = "$Id$";

  private final String propertyUserDir;
  private final BaseManager manager;
  private final String[] commandArray;
  private final AxisID axisID;
  private final ProcessMessages processMessages;

  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  private int exitValue = Integer.MIN_VALUE;
  private String[] stdInput = null;
  private OutputBufferManager stdout = null;
  private OutputBufferManager stderr = null;
  private File workingDirectory = null;
  private String exceptionMessage = "";
  private boolean started = false;
  private boolean done = false;
  private Date runTimestamp = null;
  private OutputStream cmdInputStream = null;
  private BufferedWriter cmdInBuffer = null;
  private boolean acceptInputWhileRunning = false;
  private StringBuffer commandLine = null;
  private Process process = null;
  private boolean collectOutput = true;
  private String commandAction = null;

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i> 
   * @param command The string containng the command and arguments to run.
   * 
   * It is suggested
   * that SystemProgram(String[] arg) for be used so that spaces are not
   * accidentally lost in path or arguments. 
   */
  public SystemProgram(final BaseManager manager, final String propertyUserDir,
      final List<String> command, final AxisID axisID) {
    this.manager = manager;
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
    processMessages = ProcessMessages.getMultiLineInstance(manager);
    commandArray = new String[command.size()];
    for (int i = 0; i < command.size(); i++) {
      commandArray[i] = command.get(i);
    }
  }

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>cmdArray</i> 
   * @param cmdArray The string array containng the command and arguments to
   *  run.
   * 	
   */
  public SystemProgram(final BaseManager manager, final String propertyUserDir,
      final String[] cmdArray, final AxisID axisID) {
    this.manager = manager;
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
    processMessages = ProcessMessages.getInstance(manager);
    commandArray = cmdArray;
  }

  public SystemProgram(final BaseManager manager, final String propertyUserDir,
      final String[] cmdArray, final AxisID axisID, final boolean multilineWarning,
      final boolean multilineInfo) {
    this.manager = manager;
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
    processMessages = ProcessMessages.getMultiLineInstance(manager, multilineWarning,
        multilineInfo);
    commandArray = cmdArray;
  }

  public void setDebug(final DebugLevel debugLevel) {
    debug = debugLevel;
  }

  /**
   * Specify the standard input to the program
   * @param programInput A string array containing the standard input to the
   * program each string should contain one line of input for the program.
   */
  public void setStdInput(final String[] programInput) {
    stdInput = programInput;
  }

  void clearStdError() {
    if (stderr != null) {
      stderr.clear();
    }
  }

  String[] getStdError(final Object listenerKey) {
    if (stderr == null) {
      return null;
    }
    return stderr.get(listenerKey);
  }

  String[] getStdOutput(final Object listenerKey) {
    if (stdout == null) {
      return null;
    }
    return stdout.get(listenerKey);
  }

  void dropStdOutputListener(final Object listenerKey) {
    if (stdout != null) {
      stdout.dropListener(listenerKey);
    }
  }

  void setCurrentStdInput(final String input) throws IOException {
    if (cmdInBuffer != null) {
      cmdInBuffer.write(input);
      cmdInBuffer.newLine();
      cmdInBuffer.flush();
    }
    if (cmdInputStream != null) {
      cmdInputStream.flush();
    }
  }

  /**
   * Specify the directory in which the program should be run.  If the working
   * directory is not specified then current user.dir is the default.
   * @param workingDirectory a File object specifying the working directory
   */
  public void setWorkingDirectory(final File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Execute the command.
   */
  public void run() {
    // Don't print background processes
    if (!debug.isVerbose()
        && (commandArray.length == 1
            && (commandArray[0].equals("env") || commandArray[0].endsWith("imodinfo") || commandArray[0]
                .equals("b3dhostname"))
            || (commandArray.length > 0 && (commandArray[0].equals("ssh") || commandArray[0]
                .equals("ps"))) || (commandArray.length > 1 && commandArray[0]
            .equals("b3dwinps")))) {
      debug = Arguments.DebugLevel.OFF;
    }
    started = true;
    if (debug.isOn()) {
      System.err.println("");
      System.err.println("SystemProgram: command array: ");
      for (int i = 0; i < commandArray.length; i++) {
        System.err.println("  " + commandArray[i]);
      }
      if (debug.isStandard()) {
        System.err.print("SystemProgram: working directory: ");
        if (workingDirectory == null) {
          System.err.println("null");
          System.err.println("SystemProgram: using:  " + propertyUserDir);
        }
        else {
          System.err.println(workingDirectory.getAbsoluteFile());
        }
      }
    }

    // Setup the Process object and run the command
    process = null;
    try {
      if (debug.isExtra())
        System.err.print("SystemProgram: Exec'ing process...");

      if (workingDirectory == null && propertyUserDir != null
          && !propertyUserDir.matches("\\s*+")) {
        workingDirectory = new File(propertyUserDir);
      }
      // timestamp
      StringBuffer timestampString = new StringBuffer(3);
      if (commandArray == null) {
        exitValue = 1204;// bug# 1204
        return;
      }
      for (int i = 0; i < Math.min(2, commandArray.length); i++) {
        timestampString.append(commandArray[i] + " ");
      }
      Utilities.timestamp(timestampString.toString(), Utilities.STARTED_STATUS);
      runTimestamp = new Date();

      commandAction = Utilities.getCommandAction(commandArray, stdInput);
      if (workingDirectory == null) {
        process = Runtime.getRuntime().exec(commandArray, null);
      }
      else if (commandArray != null) {
        process = Runtime.getRuntime().exec(commandArray, null, workingDirectory);
      }
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException except) {
      }
      if (debug.isExtra())
        System.err.println("returned, process started");

      // Create a buffered writer to handle the stdin, stdout and stderr
      // streams of the process
      cmdInputStream = process.getOutputStream();
      cmdInBuffer = new BufferedWriter(new OutputStreamWriter(cmdInputStream));

      InputStream cmdOutputStream = process.getInputStream();
      BufferedReader cmdOutputBuffer = new BufferedReader(new InputStreamReader(
          cmdOutputStream));

      // Set up a reader thread to keep the stdout buffers of the process empty
      stdout = newOutputBufferManager(cmdOutputBuffer);
      Thread stdoutReaderThread = new Thread(stdout);
      stdoutReaderThread.start();

      InputStream cmdErrorStream = process.getErrorStream();
      BufferedReader cmdErrorBuffer = new BufferedReader(new InputStreamReader(
          cmdErrorStream));

      // Set up a reader thread to keep the stdout buffers of the process empty
      stderr = newErrorBufferManager(cmdErrorBuffer);
      Thread stderrReaderThread = new Thread(stderr);
      stderrReaderThread.start();

      // Write out to the program's stdin pipe each line of the
      // stdInput array if it is not null
      if (stdInput != null) {
        for (int i = 0; i < stdInput.length; i++) {
          cmdInBuffer.write(stdInput[i]);
          cmdInBuffer.newLine();
          cmdInBuffer.flush();
          cmdInputStream.flush();
        }
      }
      if (!acceptInputWhileRunning) {
        cmdInputStream.close();
      }

      if (debug.isStandard()) {
        System.err.println("Done writing to process stdin");
        if (stdInput != null && stdInput.length > 0) {
          System.err.println("SystemProgram: Wrote to process stdin:");
          System.err
              .println("------------------------------------------------------------");
          for (int i = 0; i < stdInput.length; i++) {
            System.err.println(stdInput[i]);
          }
          System.err.println("");
        }
        else {
          System.err.println("SystemProgram: No input for process stdin");
        }
      }

      // Wait for the process to exit
      // why can we read the stdout and stderr above before this completes
      if (debug.isExtra())
        System.err.print("SystemProgram: Waiting for process to end...");
      waitForProcess();
      try {
        process.waitFor();
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("SystemProgram:: interrupted waiting for process to finish!");
      }
      Utilities.timestamp(timestampString.toString(), Utilities.FINISHED_STATUS);
      // Inform the output manager threads that the process is done
      stdout.setProcessDone(true);
      stderr.setProcessDone(true);

      if (debug.isExtra())
        System.err.println("done");

      exitValue = getProcessExitValue(process);
      String msg = null;
      if (exitValue == 0
          && (msg = Utilities.getCommandActionMessage(commandAction)) != null) {
        System.err.println(msg);
      }

      if (debug.isStandard())
        System.err.println("SystemProgram: Exit value: " + String.valueOf(exitValue));

      // Wait for the manager threads to complete
      try {
        stderrReaderThread.join(1000);
        stdoutReaderThread.join(1000);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("SystemProgram:: interrupted waiting for reader threads!");
      }
      if (debug.isStandard())
        System.err.print("SystemProgram: Reading from process stdout: ");
      cmdOutputStream.close();

      int cntStdOutput = stdout.size();
      if (debug.isStandard())
        System.err.println(String.valueOf(cntStdOutput) + " lines");

      if (debug.isStandard())
        System.err.print("SystemProgram: Reading from process stderr: ");

      cmdErrorStream.close();

      int cntStdError = stderr.size();
      if (debug.isStandard())
        System.err.println(String.valueOf(cntStdError) + " lines");

    }
    // TODO need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue

    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
      int errorIndex = -1;
      String error_tag = "error=";
      if (exceptionMessage.indexOf("Cannot run program \"tcsh\"") != -1) {
        UIHarness.INSTANCE.openMessageDialog(manager, exceptionMessage, "System Error");
      }
      else if (exceptionMessage.indexOf("Cannot run program \"python\"") != -1) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to run python.  Please see the IMOD Users Guide.\n"
                + exceptionMessage, "System Error");
      }
      else if (exceptionMessage.indexOf("not found") != -1) {
        // Unable to pop up an error message. This exception may cause dialog.setVisible
        // to lock up.
        System.err.println("ERROR: Unable to run command.\n" + exceptionMessage);
        exitValue = -3;
        done = true;
        return;
      }
      else if ((errorIndex = exceptionMessage.indexOf(error_tag)) != -1) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Unable to run command.\n"
            + exceptionMessage, "System Error");
        exitValue = 1;
        // Get the error number from the exception message
        String[] array = exceptionMessage.split("\\s+");
        if (array != null) {
          for (int i = array.length - 1; i >= 0; i--) {
            if (array[i].indexOf(error_tag) != -1) {
              String[] errorArray = array[i].split("\\s*=\\s*");
              if (errorArray != null && errorArray.length > 1) {
                EtomoNumber n = new EtomoNumber();
                n.set(errorArray[1]);
                if (n.isValid()) {
                  exitValue = n.getInt();
                }
              }
            }
          }
        }
      }
    }
    processMessages.addProcessOutput(stdout);
    processMessages.addProcessOutput(stderr);

    if (debug.isStandard()) {
      if (stdout != null && stdout.size() > 0) {
        System.err.println("SystemProgram: Read from process stdout:");
        System.err
            .println("------------------------------------------------------------");
        for (int i = 0; i < stdout.size(); i++) {
          System.err.println(stdout.get(i));
        }

        System.err.println("");
      }
      if (stderr != null && stderr.size() > 0) {
        System.err.println("SystemProgram: Read from process stderr:");
        System.err
            .println("------------------------------------------------------------");
        for (int i = 0; i < stderr.size(); i++) {
          System.err.println(stderr.get(i));
        }
        System.err.println("");
      }
    }
    else {
      processMessages.print();
    }

    // close standard input if it wasn't closed before
    if (acceptInputWhileRunning) {
      try {
        if (cmdInputStream != null) {
          cmdInputStream.close();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    // Set the done flag for the thread
    done = true;
  }

  void destroy() {
    if (process == null) {
      return;
    }
    process.destroy();
  }

  private OutputBufferManager newOutputBufferManager(final BufferedReader cmdBuffer) {
    OutputBufferManager bufferManager = new OutputBufferManager(cmdBuffer);
    bufferManager.setDebug(debug.isOn());
    bufferManager.setCollectOutput(collectOutput);
    return bufferManager;
  }

  private OutputBufferManager newErrorBufferManager(final BufferedReader cmdBuffer) {
    OutputBufferManager bufferManager = new OutputBufferManager(cmdBuffer);
    bufferManager.setDebug(debug.isOn());
    bufferManager.setCollectOutput(collectOutput);
    return bufferManager;
  }

  void setCollectOutput(final boolean input) {
    collectOutput = input;
  }

  /**
   *
   */
  void waitForProcess() {
  }

  /**
   * 
   * @param process
   * @return
   */
  int getProcessExitValue(final Process process) {
    return process.exitValue();
  }

  /**
   * Get the standard output from the execution of the program.
   * @return String[] An array of strings containing the standard output from
   * the program.  Each line of standard out is stored in a String.
   */
  public String[] getStdOutput() {
    if (stdout == null) {
      return null;
    }
    String[] stdOutputArray = stdout.get();
    return stdOutputArray;
  }

  public String[] getStdError() {
    if (stderr == null) {
      return null;
    }
    return stderr.get();
  }

  public String getStdErrorString() {
    String[] stdErrorArray = getStdError();
    if (stdErrorArray == null) {
      return null;
    }
    String stdErrorString = null;
    for (int i = 0; i < stdErrorArray.length; i++) {
      stdErrorString = stdErrorArray[i] + "\n";
    }
    return stdErrorString;
  }

  public String getStdOutputString() {
    String[] array = getStdOutput();
    if (array == null) {
      return null;
    }
    String string = null;
    for (int i = 0; i < array.length; i++) {
      string = array[i] + "\n";
    }
    return string;
  }

  public int getExitValue() {
    return exitValue;
  }

  public void setExitValue(final int value) {
    exitValue = value;
  }

  public String getCommandLine() {
    if (commandLine == null) {
      commandLine = new StringBuffer();
      for (int i = 0; i < commandArray.length; i++) {
        commandLine.append(commandArray[i] + " ");
      }
    }
    return commandLine.toString();
  }

  public String getCommandAction() {
    if (commandAction == null) {
      return getCommandLine();
    }
    return commandAction;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public void setMessagePrependTag(final String tag) {
    processMessages.setMessagePrependTag(tag);
  }

  /**
   * Return true if this thread has ever been started
   * @return
   */
  public boolean isStarted() {
    return started;
  }

  /**
   * Return true if run() has been excuted
   * @return
   */
  public boolean isDone() {
    return done;
  }

  public Timestamp getRunTimestamp() {
    if (runTimestamp == null) {
      return null;
    }
    return new Timestamp(runTimestamp.getTime());
  }

  public final ProcessMessages getProcessMessages() {
    return processMessages;
  }

  /**
   * 
   * @param acceptInputWhileRunning
   */
  void setAcceptInputWhileRunning(final boolean acceptInputWhileRunning) {
    this.acceptInputWhileRunning = acceptInputWhileRunning;
  }
}
