package etomo.process;
import java.io.*;

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
 * <p> Revision 1.1  2002/09/17 23:22:58  rickg
 * <p> Complete basic operation
 * <p>
 * <p> </p>
 */
public class InteractiveSystemProgram implements Runnable {
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
   * The buffered IO streams connecting to the commmand.
   */
  BufferedWriter inputBuffer;
  BufferedReader outputBuffer;
  BufferedReader errorBuffer;

  private File workingDirectory = null;

  private String exceptionMessage = "";

  /**
   * Creates a SystemProgram object to execute the program specified by the
   * argument <i>command</i>
   * @param command The string containng the command to run
   */
  public InteractiveSystemProgram(String command) {
    this.command = command;
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
      inputBuffer = new BufferedWriter(cmdInWriter);

      InputStream cmdOut = process.getInputStream();
      InputStreamReader cmdOutReader = new InputStreamReader(cmdOut);
      outputBuffer = new BufferedReader(cmdOutReader);

      InputStream cmdErr = process.getErrorStream();
      InputStreamReader cmdErrReader = new InputStreamReader(cmdErr);
      errorBuffer = new BufferedReader(cmdErrReader);

    }

    //  FIXME need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }

    try {
      process.waitFor();
      exitValue = process.exitValue();
    }
    catch (InterruptedException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
  }

  /**
   * Send text to the program's standard input
   * @param line the string to send to the program's standard input
   */
  public void writeStdin(String line) {
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

  public String readStdout() {
    String line = null;
    try {
      if (outputBuffer != null && outputBuffer.ready()) {
        line = outputBuffer.readLine();
      }
    }
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
    return line;
  }

  public String readStderr() {
    String line = null;
    try {
      if (errorBuffer != null && errorBuffer.ready()) {
        line = errorBuffer.readLine();
      }
    }
    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }
    return line;
  }

  public int getExitValue() {
    return exitValue;
  }

  public String getExceptionMessage() {
    return exceptionMessage;
  }
}
