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
 * <p> </p>
 */
public class BackgroundProcess extends Thread {
  public static final String rcsid = "$Id$";
  private String commandLine = null;
  private File workingDirectory = null;
  private ProcessManager processManager;
  private boolean demoMode = false;
  private boolean debug = false;
  private String[] stdOutput;
  private String[] stdError;


  public BackgroundProcess(String commandLine, ProcessManager processManager) {
    this.commandLine = commandLine;
    this.processManager = processManager;
  }

  /**
   * Set the working directory in which the com script is to be run.
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
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

    //  Start a progress bar to keep the user informed
    ProcessProgressDialog progress =
      new ProcessProgressDialog(commandLine, this);
    progress.stepTimeMS = 200;
    progress.progressBar.setIndeterminate(true);
    Thread progressBarThread = new Thread(progress);
    progressBarThread.start();

    if (demoMode) {
      try {
        sleep(3000);
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
    stdError = command.getStdError();
    stdOutput = command.getStdOutput();
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
