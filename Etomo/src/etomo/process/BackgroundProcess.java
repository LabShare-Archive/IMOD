package etomo.process;

import java.io.File;

import etomo.comscript.Command;
import etomo.type.AxisID;
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
 * <p> Revision 3.5  2005/01/08 01:47:14  sueh
 * <p> bug# 578 Removed getMode().  Use getCommand().getCommandMode().
 * <p>
 * <p> Revision 3.4  2004/12/16 02:24:32  sueh
 * <p> bug# 564 In run():  avoid using commandLine.  Use commandArray if it is
 * <p> available.
 * <p>
 * <p> Revision 3.3  2004/12/04 00:39:14  sueh
 * <p> bug# 569 Handling directory paths with spaces:  converting from a
 * <p> command line to a command array to prevent the command line from
 * <p> being split on white space.
 * <p>
 * <p> Revision 3.2  2004/11/19 23:17:18  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.1.2.5  2004/11/12 22:50:28  sueh
 * <p> bug# 520 Removed pass-through commands to Command.  Removed
 * <p> String getCommand() - using getCommandName instead.  Added
 * <p> Command getCommand().
 * <p>
 * <p> Revision 3.1.2.4  2004/11/08 22:18:17  sueh
 * <p> bug# 520 Added getMode(), which gets the mode the from Command.
 * <p>
 * <p> Revision 3.1.2.3  2004/10/18 17:46:22  sueh
 * <p> bug# 520 Fixed getCommandLine():  when the command is in an array,
 * <p> getCommandLine() should return the complete command in a string.
 * <p>
 * <p> Revision 3.1.2.2  2004/10/08 15:52:29  sueh
 * <p> bug# 520 Addded a command array option to the constructor.  Integrated
 * <p> the new Command option into the existing code (run, getCommand and
 * <p> getCommandLine).
 * <p>
 * <p> Revision 3.1.2.1  2004/10/06 01:34:35  sueh
 * <p> bug# 520 Using BaseProcessManager in BackgroundProcess.  Created a
 * <p>  constructor that constructs a BackgroundProcess with a Command.
 * <p> Added functions to get information from the Command.
 * <p>
 * <p> Revision 3.1  2004/08/30 18:42:02  sueh
 * <p> bug# 508 adding notifyKill()
 * <p>
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
  private String[] commandArray = null;
  private File workingDirectory = null;
  private BaseProcessManager processManager;
  private boolean demoMode = false;
  private boolean debug = false;
  private String[] stdOutput;
  private String[] stdError;
  private StringBuffer commandProcessID;
  private File outputFile = null;
  private Command command = null;
  private AxisID axisID;
  
  private String stdoutLogFile = "";
  private String stderrLogFile = "";

  private boolean started = false;
  private boolean done = false;

  public BackgroundProcess(String commandLine, BaseProcessManager processManager, AxisID axisID) {
    this.axisID = axisID;
    this.commandLine = commandLine.trim();
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }
  
  public BackgroundProcess(Command command, BaseProcessManager processManager, AxisID axisID) {
    this.axisID = axisID;
    this.command = command;
    this.commandArray = command.getCommandArray();
    this.commandLine = command.getCommandLine().trim();
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }
  
  public BackgroundProcess(String[] commandArray, BaseProcessManager processManager, AxisID axisID) {
    this.axisID = axisID;
    this.commandArray = commandArray;
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }
  
  public AxisID getAxisID() {
    return axisID;
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
    if (commandLine != null) {
      return commandLine;
    }
    else if (commandArray != null) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < commandArray.length; i++) {
        buffer.append(commandArray[i] + " ");
      }
      return buffer.toString();
    }
    else if (command != null) {
      return command.getCommandLine();
    }
    return null;
  }
  
  public Command getCommand() {
    return command;
  }

  /**
   * Returns command name of the process
   * @return File
   */
  public String getCommandName() {
    if (command != null) {
      return command.getCommandName();
    }
    if (commandLine != null) {
      String[] words = commandLine.split("\\s");
      return words[0];
    }
    if (commandArray != null) {
      return commandArray[0];
    }
    return null;
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
    SystemProgram program;
    if (commandArray != null) {
      program = new SystemProgram(commandArray, axisID);
    }
    else if (command != null) {
      program = new SystemProgram(command.getCommandArray(), axisID);
    }
    else if (commandLine != null) {
      program = new SystemProgram(commandLine, axisID);
    }
    else {
      processManager.msgBackgroundProcessDone(this, 1);
      return;
    }
    program.setWorkingDirectory(workingDirectory);
    program.setDebug(debug);

    if (demoMode) {
      try {
        sleep(3000);
        program.setExitValue(0);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("Sleep interrupted");
      }
    }
    else {
      // Execute the command
      ParsePID parsePID = new ParsePID(program, commandProcessID);
      Thread parsePIDThread = new Thread(parsePID);
      parsePIDThread.start();
      program.run();
    }

    //  Get any output from the command
    stdError = program.getStdError();
    stdOutput = program.getStdOutput();

    // Send a message back to the ProcessManager that this thread is done.
    processManager.msgBackgroundProcessDone(this, program.getExitValue());
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
