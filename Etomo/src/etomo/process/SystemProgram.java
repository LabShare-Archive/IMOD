package etomo.process;

import java.io.*;
import java.util.*;

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
public class SystemProgram implements Runnable {
  public static final String rcsid =
    "$Id$";

  private boolean debug = false;

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

    if (debug) {
      System.err.println("");
      System.err.println("SystemProgram: command string: " + command);
      System.err.print("SystemProgram: working directory: ");
      if (workingDirectory == null) {
        System.err.println("null");
        System.err.println(
          "SystemProgram: using current user.dir "
            + System.getProperty("user.dir"));
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

      if (workingDirectory == null) {
        File currentUserDirectory = new File(System.getProperty("user.dir"));
        process =
          Runtime.getRuntime().exec(command, null, currentUserDirectory);
      }
      else {
        process = Runtime.getRuntime().exec(command, null, workingDirectory);
      }

      if (debug)
        System.err.println("done");

      //  Create a buffered writer to handle the stdin, stdout and stderr
      //  streams of the process
      OutputStream cmdInputStream = process.getOutputStream();
      BufferedWriter cmdInBuffer =
        new BufferedWriter(new OutputStreamWriter(cmdInputStream));

      InputStream cmdOutputStream = process.getInputStream();
      BufferedReader cmdOutputBuffer =
        new BufferedReader(new InputStreamReader(cmdOutputStream));

      InputStream cmdErrorStream = process.getErrorStream();
      BufferedReader cmdErrorBuffer =
        new BufferedReader(new InputStreamReader(cmdErrorStream));

      //  Write out to the program's stdin pipe each line of the
      //  stdInput array if it is not null
      if (stdInput != null) {
        for (int i = 0; i < stdInput.length; i++) {
          cmdInBuffer.write(stdInput[i]);
          cmdInBuffer.newLine();
          cmdInBuffer.flush();
        }
      }
      cmdInputStream.close();

      if (debug) {
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

      process.waitFor();

      if (debug)
        System.err.println("done");

      exitValue = process.exitValue();

      if (debug)
        System.err.println(
          "SystemProgram: Exit value: " + String.valueOf(exitValue));

      //  Read in the command's stdout and stderr
      String line;
      int count = 0;

      if (debug)
        System.err.print("SystemProgram: Reading from process stdout: ");

      while ((line = cmdOutputBuffer.readLine()) != null) {
        stdOutput.add(line);
        count++;
      }
      cmdOutputStream.close();

      if (debug)
        System.err.println(String.valueOf(count) + " lines");

      if (debug)
        System.err.print("SystemProgram: Reading from process stderr: ");

      count = 0;
      while ((line = cmdErrorBuffer.readLine()) != null) {
        stdError.add(line);
        count++;
      }
      cmdErrorStream.close();

      if (debug)
        System.err.println(String.valueOf(count) + " lines");

    }
    // TODO need better error handling, what should be the state if an
    // exception is thrown i.e. the exitValue

    catch (IOException except) {
      except.printStackTrace();
      exceptionMessage = except.getMessage();
    }

    // Kill the underlying system command if we receive an interrupt exception
    catch (InterruptedException except) {
      if (debug) {
        System.err.println(
          "Received InterruptedException...destroying process");
      }
      process.destroy();
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

  public void setExitValue(int value) {
    exitValue = value;
  }

  public String getExceptionMessage() {
    return exceptionMessage;
  }

  public void enableDebug(boolean state) {
    debug = state;
  }
}
