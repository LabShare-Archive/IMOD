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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/06/05 04:23:10  rickg
 * <p> method name change to getShellProcessID
 * <p>
 * <p> Revision 2.5  2003/05/23 22:48:44  rickg
 * <p> Implemented getting the PID of tcsh shell scripts that emmit it
 * <p> to stderr
 * <p>
 * <p> Revision 2.4  2003/05/23 14:27:36  rickg
 * <p> Implements SystemProcessInterface
 * <p>
 * <p> Revision 2.3  2003/05/12 23:24:54  rickg
 * <p> Comment fixes
 * <p>
 * <p> Revision 2.2  2003/05/08 23:19:03  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.1  2003/01/29 20:45:11  rickg
 * <p> Debug messages to stderr instead of stdout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
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
public class BackgroundProcess
  extends Thread
  implements SystemProcessInterface {
    
  public static final String rcsid =
    "$Id$";
  private String commandLine = null;
  private File workingDirectory = null;
  private ProcessManager processManager;
  private boolean demoMode = false;
  private boolean debug = false;
  private String[] stdOutput;
  private String[] stdError;
  private StringBuffer commandProcessID;
  
  private String stdoutLogFile = "";
  private String stderrLogFile = "";

  private boolean started = false;
  private boolean done = false;

  public BackgroundProcess(String commandLine, ProcessManager processManager) {
    this.commandLine = commandLine.trim();
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
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
  public boolean isDebug() {
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
   * Returns the full command line.
   * @return File
   */
  public String getCommandLine() {
    return commandLine;
  }

  /**
   * Returns command name of the process
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
    started = true;
    SystemProgram command = new SystemProgram(commandLine);
    command.setWorkingDirectory(workingDirectory);
    command.setDebug(debug);

    if (demoMode) {
      try {
        sleep(3000);
        command.setExitValue(0);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("Sleep interrupted");
      }
    }
    else {
      // Execute the command
      ParsePID parsePID = new ParsePID(command, commandProcessID);
      Thread parsePIDThread = new Thread(parsePID);
      parsePIDThread.start();
      command.run();
    }

    //  Get any output from the command
    stdError = command.getStdError();
    stdOutput = command.getStdOutput();

    // Send a message back to the ProcessManager that this thread is done.
    processManager.msgBackgroundProcessDone(this, command.getExitValue());
    done = true;
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
  
  public boolean isStarted() {
    return started;    
  }

  public boolean isDone() {
    return done;
  }
    
  /**
   * Get the shell process ID if it is available
   * @return
   */
  public String getShellProcessID() {
    if (commandProcessID == null) {
      return "";
    }
    return commandProcessID.toString();
  }
  
  /**
   * nothing to do
   */
  public void notifyKill() {
    
  }

}
