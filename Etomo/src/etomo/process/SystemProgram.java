package etomo.process;

import java.io.*;
import java.util.*;

/**
 * <p>Description: SystemProgram provides a class to execute programs under the
 * host operating system.  The class provides access to stdin, stdout and stderr
 * streams and implements the Runnable interface so that it can be threaded.</p>
 *
 * <p> If the working directory is not explicitly set then the current working
 * directory for the command is set to the system property "user.dir"
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
 * <p> Revision 1.2  2002/09/17 23:32:07  rickg
 * <p> Description update
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class SystemProgram implements Runnable {
  public static final String rcsid =
    "$Id$";

  /**
   * The exit value of the command
   */
  private int exitValue = Integer.MIN_VALUE;
  /**
   * The command to run
   */
  private String command = null;
  /**
   * The array containing the standard input for the program.  Each element of
   * the is submitted followed by a newline.
   */
  private String[] stdInput = null;
  /**
   * An ArrayList containing the standard out from the excution of the command.
   */
  private ArrayList stdOutput = new ArrayList();
  /**
   * An ArrayList containing the standard error from the excution of the
   * command.
   */
  private ArrayList stdError = new ArrayList();
  /**
   * The directory in which the command will be run.
   */
  private File workingDirectory = null;

  private String exceptionMessage = "";

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i>
   * @param command The string containng the command to run
   */
  public SystemProgram(String command) {
    this.command = command;
  }

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i> with input sequence specified in
   * <i>programInput</n>.
   * @param command The string containng the command to run.
   * @param programInput A string array containing the standard input to the
   * program each string should contain one line of input for the program.
   */
  public SystemProgram(String command, String[] programInput) {
    this.command = command;
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

    //  Setup the Process object and run the command
    Process process = null;
    try {
      if (workingDirectory == null) {
        File currentUserDirectory = new File(System.getProperty("user.dir"));
        process =
          Runtime.getRuntime().exec(command, null, currentUserDirectory);
      }
      else {
        process = Runtime.getRuntime().exec(command, null, workingDirectory);
      }

      //  Create a buffered writer to handle the stdin, stdout and stderr
      //  streams of the process
      OutputStream cmdIn = process.getOutputStream();
      OutputStreamWriter cmdInWriter = new OutputStreamWriter(cmdIn);
      BufferedWriter cmdInBuffer = new BufferedWriter(cmdInWriter);

      InputStream cmdOut = process.getInputStream();
      InputStreamReader cmdOutReader = new InputStreamReader(cmdOut);
      BufferedReader cmdOutBuffer = new BufferedReader(cmdOutReader);

      InputStream cmdErr = process.getErrorStream();
      InputStreamReader cmdErrReader = new InputStreamReader(cmdErr);
      BufferedReader cmdErrBuffer = new BufferedReader(cmdErrReader);

      //  Write out to the program's stdin pipe each line of the
      //  stdInput array if it is not null
      if (stdInput != null) {
        for (int i = 0; i < stdInput.length; i++) {
          cmdInBuffer.write(stdInput[i]);
          cmdInBuffer.newLine();
          cmdInBuffer.flush();
        }
      }

      //  Read in the command's stdout and stderr
      String line;
      while ((line = cmdOutBuffer.readLine()) != null)
        stdOutput.add(line);

      while ((line = cmdErrBuffer.readLine()) != null)
        stdError.add(line);
    }

    //  FIXME need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }

    try {
      //  Wait for the process to exit
      //  why can we read the stdout and stderr above before this completes
      process.waitFor();
      exitValue = process.exitValue();
    }
    catch (InterruptedException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
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

  public int getExitValue() {
    return exitValue;
  }

  public String getExceptionMessage() {
    return exceptionMessage;
  }
}
