package etomo.process;

import java.io.File;
/**
 * <p>Description: </p>
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
 * <p> Revision 1.3.2.1  2003/01/24 18:28:09  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2003/01/08 04:00:21  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.2  2003/01/04 00:21:29  rickg
 * <p> Methods to get the command and command line.
 * <p>
 * <p> Revision 1.1  2003/01/03 00:56:19  rickg
 * <p> Initial revision
 * <p>
 * <p> </p>
 */
public class BackgroundProcess extends Thread {
  public static final String rcsid =
    "$Id$";
  private String commandLine = null;
  private File workingDirectory = null;
  private ProcessManager processManager;
  private boolean demoMode = false;
  private boolean debug = false;
  private String[] stdOutput;
  private String[] stdError;

  private String stdoutLogFile = "";
  private String stderrLogFile = "";

  public BackgroundProcess(String commandLine, ProcessManager processManager) {
    this.commandLine = commandLine.trim();
    this.processManager = processManager;
  }

  /**
   * Returns the demoMode.
   * @return boolean
   */
  public boolean isDemoMode() {
    return demoMode;
  }

  /**
   * Returns the enableDebug.
   * @return boolean
   */
  public boolean isEnableDebug() {
    return debug;
  }

  /**
   * Returns the workingDirectory.
   * @return File
   */
  public File getWorkingDirectory() {
    return workingDirectory;
  }

  /**
   * Returns the workingDirectory.
   * @return File
   */
  public String getCommandLine() {
    return commandLine;
  }

  /**
   * Returns the workingDirectory.
   * @return File
   */
  public String getCommand() {
    String[] words = commandLine.split("\\s");
    return words[0];
  }
  /**
   * Set the working directory in which the com script is to be run.
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Sets the demoMode.
   * @param demoMode The demoMode to set
   */
  public void setDemoMode(boolean demoMode) {
    this.demoMode = demoMode;
  }

  /**
   * Sets the enableDebug.
   * @param enableDebug The enableDebug to set
   */
  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * Execute the command and notify the ProcessManager when it is done
   */
  public void run() {
    SystemProgram command = new SystemProgram(commandLine);
    command.setWorkingDirectory(workingDirectory);

    if (demoMode) {
      try {
        sleep(3000);
        command.setExitValue(0);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.out.println("Sleep interrupted");
      }
    }
    else {
      // Execute the command
      command.run();
    }

    //  Get any output from the command
    stdError = command.getStdError();
    stdOutput = command.getStdOutput();

    // Send a message back to the ProcessManager that this thread is done.
    processManager.msgBackgroundProcessDone(this, command.getExitValue());
  }

  /**
   * Returns the stdError.
   * @return String[]
   */
  public String[] getStdError() {
    return stdError;
  }

  /**
   * Returns the stdOutput.
   * @return String[]
   */
  public String[] getStdOutput() {
    return stdOutput;
  }

}
