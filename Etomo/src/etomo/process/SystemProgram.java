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
public class SystemProgram implements Runnable {
  public static final String rcsid =
    "$Id$";

  private boolean debug = false;
  private int exitValue = Integer.MIN_VALUE;
  private String command = null;
  private String[] stdInput = null;
  private ArrayList stdOutput = new ArrayList();
  private ArrayList stdError = new ArrayList();
  private File workingDirectory = null;
  private String exceptionMessage = "";

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
        workingDirectory = new File(System.getProperty("user.dir"));
      }
      process = Runtime.getRuntime().exec(command, null, workingDirectory);
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException except) {

      }
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
			if(debug){
				System.err.println("Done writting to process stdin");
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

      exitValue = process.exitValue();

      if (debug)
        System.err.println(
          "SystemProgram: Exit value: " + String.valueOf(exitValue));

      //  Wait for the manager threads to complete
      try {
        stderrReaderThread.join(10000);
        stdoutReaderThread.join(10000);
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
        except.printStackTrace();
        System.err.println(
          "IO Exception while reading SystemProgram process output");
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
