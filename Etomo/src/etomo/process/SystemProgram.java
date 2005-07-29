/**
 * <p>Description: SystemProgram provides a class to execute programs under the
 * host operating system.  The class provides access to stdin, stdout and
 * stderr streams and implements the Runnable interface so that it can be
 *  threaded.</p>
 *
 * <p> If the working directory is not explicitly set then the current working
 * directory for the command is set to the system property "user.dir"
 * 
 * <p>Copyright: Copyright (c) 2002</p
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
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
import java.util.ArrayList;
import java.util.Date;

import etomo.type.AxisID;


public class SystemProgram implements Runnable {
  public static final String rcsid =
    "$Id$";

  private boolean debug = false;
  private int exitValue = Integer.MIN_VALUE;
  private String[] commandArray;
  private String[] stdInput = null;
  private ArrayList stdOutput = new ArrayList();
  private ArrayList stdError = new ArrayList();
  private File workingDirectory = null;
  private String exceptionMessage = "";
  private boolean started = false;
  private boolean done = false;
  private ArrayList warningList = new ArrayList();
  private Date runTimestamp = null;
  private AxisID axisID;
  private final String propertyUserDir;

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i> 
   * @param command The string containng the command and arguments to run.
   * 
   * WARNING: The comand string is split at whitespace to create command and
   * arguments.  This behavior is for backward compatibility.  It is suggested
   * that SystemProgram(String[] arg) for be used so that spaces are not
   * accidentally lost in path or arguments. 
   */
  public SystemProgram(String propertyUserDir, String command, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
    commandArray = command.split("\\s+");
  }

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>cmdArray</i> 
   * @param cmdArray The string array containng the command and arguments to
   *  run.
   * 	
   */
  public SystemProgram(String propertyUserDir, String[] cmdArray, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
    commandArray = cmdArray;
  }

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i> with input sequence specified in
   * <i>programInput</n>.
   * @param command The string containng the command to run.
   * @param programInput A string array containing the standard input to the
   * program each string should contain one line of input for the program.
   * 
   * WARNING: The comand string is split at whitespace to create command and
   * arguments.  This behavior is for backward compatibility.  It is suggested
   * that SystemProgram(String[] arg) for be used so that spaces are not
   * accidentally lost in path or arguments. 
   */
  public SystemProgram(String propertyUserDir, String command,
      String[] programInput, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
		commandArray = command.split("\\s+");
    stdInput = programInput;
  }

  /**
   * Specify the standard input to the program
   * @param programInput A string array containing the standard input to the
   * program each string should contain one line of input for the program.
   */
  public void setStdInput(String[] programInput) {
    stdInput = programInput;
  }

  /**
   * Specify the directory in which the program should be run.  If the working
   * directory is not specified then current user.dir is the default.
   * @param workingDirectory a File object specifying the working directory
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Execute the command.
   */
  public void run() {
    started = true;
    if (debug) {
      System.err.println("");
      System.err.println("SystemProgram: command array: ");
      for (int i = 0; i < commandArray.length; i++) {
        System.err.println("  " + commandArray[i]);
      }
      System.err.print("SystemProgram: working directory: ");
      if (workingDirectory == null) {
        System.err.println("null");
        System.err.println("SystemProgram: using current user.dir "
            + propertyUserDir);
      }
      else {
        System.err.println(workingDirectory.getAbsoluteFile());
      }
    }

    //  Setup the Process object and run the command
    Process process = null;
    try {
      if (debug)
        System.err.print("SystemProgram: Exec'ing process...");

      if (workingDirectory == null && propertyUserDir != null && propertyUserDir.matches("\\S+")) {
        workingDirectory = new File(propertyUserDir);
      }
      runTimestamp = new Date();
      if (workingDirectory == null) {
        process = Runtime.getRuntime().exec(commandArray, null);
      }
      else {
        process = Runtime.getRuntime().exec(commandArray, null, workingDirectory);
      }
      waitForProcess();
      if (debug)
        System.err.println("returned, process started");

      //  Create a buffered writer to handle the stdin, stdout and stderr
      //  streams of the process
      OutputStream cmdInputStream = process.getOutputStream();
      BufferedWriter cmdInBuffer =
        new BufferedWriter(new OutputStreamWriter(cmdInputStream));

      InputStream cmdOutputStream = process.getInputStream();
      BufferedReader cmdOutputBuffer =
        new BufferedReader(new InputStreamReader(cmdOutputStream));

      // Set up a reader thread to keep the stdout buffers of the process empty
      OutputBufferManager stdoutReaderMgr =
        new OutputBufferManager(cmdOutputBuffer, stdOutput);
      Thread stdoutReaderThread = new Thread(stdoutReaderMgr);
      stdoutReaderThread.start();

      InputStream cmdErrorStream = process.getErrorStream();
      BufferedReader cmdErrorBuffer =
        new BufferedReader(new InputStreamReader(cmdErrorStream));

      // Set up a reader thread to keep the stdout buffers of the process empty
      OutputBufferManager stderrReaderMgr =
        new OutputBufferManager(cmdErrorBuffer, stdError);
      Thread stderrReaderThread = new Thread(stderrReaderMgr);
      stderrReaderThread.start();

      //  Write out to the program's stdin pipe each line of the
      //  stdInput array if it is not null
      if (stdInput != null) {
        for (int i = 0; i < stdInput.length; i++) {
          cmdInBuffer.write(stdInput[i]);
          cmdInBuffer.newLine();
          cmdInBuffer.flush();
          cmdInputStream.flush();
        }
      }
      cmdInputStream.close();

      if (debug) {
        System.err.println("Done writing to process stdin");
        if (stdInput != null && stdInput.length > 0) {
          System.err.println("SystemProgram: Wrote to process stdin:");
          System.err.println(
            "------------------------------------------------------------");
          for (int i = 0; i < stdInput.length; i++) {
            System.err.println(stdInput[i]);
          }
          System.err.println("");
        }
        else {
          System.err.println("SystemProgram: No input for process stdin");
        }
      }

      //  Wait for the process to exit
      //  why can we read the stdout and stderr above before this completes
      if (debug)
        System.err.print("SystemProgram: Waiting for process to end...");

      try {
        process.waitFor();
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println(
          "SystemProgram:: interrupted waiting for process to finish!");
      }
      // Inform the output manager threads that the process is done
      stdoutReaderMgr.setProcessDone(true);
      stderrReaderMgr.setProcessDone(true);

      if (debug)
        System.err.println("done");

      exitValue = getProcessExitValue(process);

      if (debug)
        System.err.println(
          "SystemProgram: Exit value: " + String.valueOf(exitValue));

      //  Wait for the manager threads to complete
      try {
        stderrReaderThread.join(1000);
        stdoutReaderThread.join(1000);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println(
          "SystemProgram:: interrupted waiting for reader threads!");
      }
      if (debug)
        System.err.print("SystemProgram: Reading from process stdout: ");
      cmdOutputStream.close();

      int cntStdOutput = stdOutput.size();
      if (debug)
        System.err.println(String.valueOf(cntStdOutput) + " lines");

      if (debug)
        System.err.print("SystemProgram: Reading from process stderr: ");

      cmdErrorStream.close();

      int cntStdError = stdError.size();
      if (debug)
        System.err.println(String.valueOf(cntStdError) + " lines");

    }
    // TODO need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue

    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }

    if (stdOutput.size() > 0) {
      for (int i = 0; i < stdOutput.size(); i++) {
        String output = (String) stdOutput.get(i);
        if (output.startsWith("WARNING:")) {
          warningList.add(output);
          //System.out.println("warning=" + warningList.get(warningList.size() - 1));
        }
      }
    }
    if (stdError.size() > 0) {
      //System.out.println("stderr");
      for (int i = 0; i < stdError.size(); i++) {
        String error = (String) stdError.get(i);
        //System.out.println(error);
        if (error.startsWith("WARNING:")) {
          warningList.add(error);
          //System.out.println("warning=" + warningList.get(warningList.size() - 1));
        }
      }
    }

    if (debug) {
      if (stdOutput.size() > 0) {
        System.err.println("SystemProgram: Read from process stdout:");
        System.err.println(
          "------------------------------------------------------------");
        for (int i = 0; i < stdOutput.size(); i++) {
          System.err.println(stdOutput.get(i));
        }

        System.err.println("");
      }
      if (stdError.size() > 0) {
        System.err.println("SystemProgram: Read from process stderr:");
        System.err.println(
          "------------------------------------------------------------");
        for (int i = 0; i < stdError.size(); i++) {
          System.err.println(stdError.get(i));
        }
        System.err.println("");
      }
    }

    //  Set the done flag for the thread
    done = true;
  }

  /**
   *
   */
  protected void waitForProcess() {
    try {
      Thread.sleep(100);
    }
    catch (InterruptedException except) {

    }
  }
  
  /**
   * 
   * @param process
   * @return
   */
  protected int getProcessExitValue(Process process) {
    return process.exitValue();
  }
  
  /**
   * Get the standard output from the execution of the program.
   * @return String[] An array of strings containing the standard output from
   * the program.  Each line of standard out is stored in a String.
   */
  public String[] getStdOutput() {
    return (String[]) stdOutput.toArray(new String[stdOutput.size()]);
  }

  public String[] getStdError() {
    return (String[]) stdError.toArray(new String[stdError.size()]);
  }

  public String getStdErrorString() {
    String[] stdErrorArray = (String[]) stdError.toArray(new String[stdError.size()]);
    String stdErrorString = null;
    for (int i = 0; i < stdErrorArray.length; i++) {
      stdErrorString = stdErrorArray[i] + "\n";
    }
    return stdErrorString;
  }

  public int getExitValue() {
    return exitValue;
  }

  public void setExitValue(int value) {
    exitValue = value;
  }

  String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  public String getExceptionMessage() {
    return exceptionMessage;
  }
  
  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * @return
   */
  public boolean isDebug() {
    return debug;
  }

  /**
   * @param b
   */
  public void setDebug(boolean state) {
    debug = state;
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
 
 public static ArrayList parseError(String[] output) {  
   ArrayList errors = new ArrayList();
   for (int i = 0; i < output.length; i++) {
     int index = output[i].indexOf("ERROR:");
     if (index != -1) {
       errors.add(output[i].substring(index));
     }
   }
   return errors;
 }
 
 /**
  * Finds warnings in output and returns them in an array list.
  * @param output
  * @return
  */
 public static ArrayList parseWarning(String[] output) {
   return SystemProgram.parseWarning(output, false);
 }
 
 /**
  * Finds warnings in output and returns them in an array list, one element per
  * warning.  Handles multi-line warnings when multiLine is true.  Puts "\n"
  * between the lines.  WARNING:  When multiLine is true, this function will not
  * work with warnings that are not separated at the end by a blank line.
  * @param output
  * @param multiLine
  * @return
  */
 public static ArrayList parseWarning(String[] output, boolean multiLine) {
   ArrayList warnings = new ArrayList();
   boolean found = false;
   StringBuffer warning = null;
   boolean done = false;
   int index = 0;
   do {
     int labelIndex = -1;
     boolean emptyLine = true;
     if (!(done = index >= output.length)) {
       labelIndex = output[index].indexOf("WARNING:");
       emptyLine = output[index].trim().length() == 0;
     }
     //If end of current warning, save it in warnings
     if (found && (!multiLine || (labelIndex != -1 || (labelIndex == -1 && emptyLine)))) {
       if (warning == null) {
         throw new IllegalStateException("buffer is null when found is true");
       }
       warnings.add(warning.toString());
       found = false;
       warning = null;
     }
     //build warning string
     if (labelIndex != -1) {
       if (found) {
         throw new IllegalStateException("found is true when a new warning buffer is about to be created");
       }
       found = true;
       warning = new StringBuffer(output[index].substring(labelIndex));
     }
     else if (found && multiLine && !emptyLine) {
       warning.append("\n" + output[index].trim());
     }
     index++;
   } while (!done);
   return warnings;
 }

  /**
   * Runnable class to keep the output buffers of the child process from filling
   * up and locking up the process.
   * See Java bugs #: 4750978, 4098442, etc
   */
  class OutputBufferManager implements Runnable {
    BufferedReader outputReader;
    ArrayList outputList;
    boolean processDone = false;

    public OutputBufferManager(BufferedReader reader, ArrayList output) {
      outputReader = reader;
      outputList = output;
    }

    public void run() {
      String line;
      try {
        while (!processDone) {
          while ((line = outputReader.readLine()) != null) {
            outputList.add(line);
          }
          Thread.sleep(100);
        }
        while ((line = outputReader.readLine()) != null) {
          outputList.add(line);
        }
      }
      catch (IOException except) {
        //  Assume the stream is closed by the program exiting.  
        processDone = true;
        return;
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("SystemProgram::OuputBufferManager interrupted!");
      }
    }

    public void setProcessDone(boolean state) {
      processDone = state;
    }
  }
}
