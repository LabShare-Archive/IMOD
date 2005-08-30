
/**
 * <p>
 * Description: Provides a threadable class to execute IMOD com scripts.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.27  2005/08/15 18:19:34  sueh
 * bug# 532 Added kill, pause, setCurrentStdInput, signalInterrupt, and
 * signalKill to implement SystemProcessInterface.  Only kill and signalKill
 * are valid to use in ComScriptProcess.  They allow processchunks (a
 * background process) to choose to signal interrupt instead of kill.
 *
 * Revision 3.26  2005/08/04 19:44:27  sueh
 * bug# 532 Added getCurrentStdOutput to implement interface.
 *
 * Revision 3.25  2005/07/29 00:51:29  sueh
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * because the current manager changes when the user changes the tab.
 * Passing the manager where its needed.
 *
 * Revision 3.24  2005/07/26 18:42:07  sueh
 * bug# 701 Added a ProcessMonitor member variable.  Added a
 * ProcessendState member variable for when the ProcessMonitor variable
 * is null.
 *
 * Revision 3.23  2005/07/11 22:44:20  sueh
 * bug#  619 Added isKilled() so Etomo can respond differently to a
 * process that was killed by the user then it does to a process that ended
 * withed an error.
 *
 * Revision 3.22  2005/04/25 20:45:18  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.
 *
 * Revision 3.21  2005/01/06 18:10:09  sueh
 * bug# 578 Added getCommand().
 *
 * Revision 3.20  2005/01/05 19:54:00  sueh
 * bug# 578 Added a constructor which accepts
 * Command comScriptCommand instead of String comScript.
 *
 * Revision 3.19  2004/11/24 00:59:50  sueh
 * bug# 520 Add getComScriptName to identify which comscript was run.
 *
 * Revision 3.18  2004/11/19 23:19:18  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.17.2.1  2004/09/29 17:49:24  sueh
 * bug# 520 changing to BaseProcessManager.
 *
 * Revision 3.17  2004/08/30 18:42:14  sueh
 * bug# 508 adding notifyKill()
 *
 * Revision 3.16  2004/08/28 00:53:40  sueh
 * bug# 508 gave a descendent class access to watchedFileNAme and
 * errorMessage.
 * Added isComScriptBusy().  It always returns true in this class.  It is
 * needed for background comscript execution.
 * Allowed renameFiles() to cause the comscript execution to fail if an
 * error is returned.  This never happens in this class.  It is for background
 * comscripts running on Windows.
 * Fixed a problem with failures that happen before the started variable is
 * set.  ProcessManager.startComScript() has a loop which doesn't end
 * until started is true.  Added error and treated it like started.
 *
 * Revision 3.15  2004/08/20 21:43:59  sueh
 * bug# 508 split parseWarning() into two functions so that parseWarning
 * can be used for all the .log files parsed by
 * BackgroundComsScripProcess.
 *
 * Revision 3.14  2004/08/19 02:06:34  sueh
 * bug# 508 Put the rename files functionality into a separate function,
 * so it can be overriden.  Placed the rename files code into a separate
 * function that could handle different command names.  Placed the
 * parseError() code into a separate function that could handle different
 * log file names and missing log files.  Made parseBaseName() static.
 * Added:
 * parseError(String name, boolean mustExist)
 * renameFiles()
 * renameFiles(String name, String watchedFileName,
 *     File workingDirectory)
 * Changed:
 * parseBaseName(String filename, String extension)
 * parseError()
 * run()
 *
 * Revision 3.13  2004/08/06 23:00:44  sueh
 * bug# 508 Base class for BackgroundComScriptProcess.
 * Changing functions and variables needed by
 * BackgroundComScriptProcess from private to protected.
 *
 * Revision 3.12  2004/07/13 17:26:11  sueh
 * bug# 429 moving fix to Utilities
 *
 * Revision 3.11  2004/07/08 00:06:50  sueh
 * bug# 429 rename .log to .log~ when .log exists
 *
 * Revision 3.10  2004/07/07 21:42:22  sueh
 * bug# 490 only rename watched file if it exists
 *
 * Revision 3.9  2004/07/02 16:47:32  sueh
 * bug# 490 added watchFileName.  Moving watchFile to backup
 * in run().
 *
 * Revision 3.8  2004/04/28 19:58:55  rickg
 * bug #429 logfile rename functionality moved to Utilities
 *
 * Revision 3.7  2004/04/22 23:29:40  rickg
 * Switched getIMODBinPath method
 *
 * Revision 3.6  2004/04/21 20:27:17  sueh
 * reformatting
 *
 * Revision 3.5  2004/04/19 22:01:12  rickg
 * bug# 115 Remove all text before ERROR: string
 *
 * Revision 3.4  2004/04/16 01:51:41  sueh
 * bug# 409 added ProcessName
 *
 * Revision 3.3  2004/04/10 00:52:01  sueh
 * bug# 409 fixed a possible bug in parseWarning()
 *
 * Revision 3.2  2004/04/09 17:03:39  sueh
 * bug# 409 parsing multi-line "PIP WARNING:" message
 *
 * Revision 3.1  2003/11/26 23:49:28  rickg
 * Bug# 366 Restructured renameLogFile to work on windows
 *  Added necessary (for windows) closes on the fileBuffer
 *
 * Revision 3.0  2003/11/07 23:19:00  rickg
 * Version 1.0.0
 *
 * Revision 1.13  2003/11/06 16:50:27  rickg
 * Removed -e flag for tcsh execution for all but the com scripts
 *
 * Revision 1.12  2003/11/04 20:56:11  rickg
 * Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 *
 * Revision 1.11  2003/11/04 00:55:10  rickg
 * Bug #345 Explicitly set path to script using IMOD_DIR
 *
 * <p>
 * Revision 1.10 2003/10/06 19:05:31 rickg
 * <p>
 * Move renameLogFile to a method
 * <p>
 * <p>
 * Revision 1.9 2003/10/01 18:15:48 rickg
 * <p>
 * Store demo time as a parameter and make it avaiable
 * <p>
 * <p>
 * Revision 1.8 2003/08/20 22:02:21 rickg
 * <p>
 * Added log message when unable to rename log file
 * <p>
 * <p>
 * Revision 1.7 2003/08/05 21:18:18 rickg
 * <p>
 * Moved log file renaming to the beginning of the process
 * <p>
 * <p>
 * Revision 1.6 2003/06/05 20:48:58 rickg
 * <p>
 * Added multiline error messages
 * <p>
 * <p>
 * Revision 1.5 2003/06/05 04:22:36 rickg
 * <p>
 * method name change to getShellProcessID
 * <p>
 * <p>
 * Revision 1.4 2003/06/04 23:48:33 rickg
 * <p>
 * Rename log file if it exists
 * <p>
 * <p>
 * Revision 1.3 2003/05/27 08:43:16 rickg
 * <p>
 * Set started flag in com script execution
 * <p>
 * <p>
 * Revision 1.2 2003/05/23 14:27:24 rickg
 * <p>
 * Implements SystemProcessInterface
 * <p>
 * <p>
 * Revision 1.1 2003/05/23 02:34:56 rickg
 * <p>
 * Change RunComScript to ComScriptProcess
 * <p>
 * <p>
 * Revision 2.7 2003/05/21 22:55:14 rickg
 * <p>
 * Moved ParsePID to its own class so that BackgroundProcess
 * <p>
 * can also use it.
 * <p>
 * <p>
 * Revision 2.6 2003/05/21 21:20:42 rickg
 * <p>
 * Parse the csh process ID from stderr
 * <p>
 * <p>
 * Revision 2.5 2003/05/13 20:00:12 rickg
 * <p>
 * Added -f option to tcsh call
 * <p>
 * <p>
 * Revision 2.4 2003/05/12 23:28:13 rickg
 * <p>
 * removed -f from tcsh executation (allows for user modifications to be
 * <p>
 * used within etomo execution)
 * <p>
 * <p>
 * Revision 2.3 2003/05/12 01:21:33 rickg
 * <p>
 * Added explici tcsh call to copytomocoms
 * <p>
 * <p>
 * Revision 2.2 2003/05/08 23:19:03 rickg
 * <p>
 * Standardized debug setting
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 20:44:48 rickg
 * <p>
 * Debug messages to stderr instead of stdout
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.8.2.1 2003/01/24 18:36:17 rickg
 * <p>
 * Single window GUI layout initial revision
 * <p>
 * <p>
 * Revision 1.8 2002/12/31 23:14:25 rickg
 * <p>
 * Removed unnecessary string IMODDIR
 * <p>
 * <p>
 * Revision 1.7 2002/12/30 20:31:59 rickg
 * <p>
 * Added interface to get process output
 * <p>
 * Program execution correctly calls done message
 * <p>
 * of process manager
 * <p>
 * <p>
 * Revision 1.6 2002/10/22 21:38:40 rickg
 * <p>
 * ApplicationManager now controls both demo and debug
 * <p>
 * modes
 * <p>
 * <p>
 * Revision 1.5 2002/10/14 19:03:59 rickg
 * <p>
 * vmstocsh and csh -ef are executed directly and separately now.
 * <p>
 * <p>
 * Revision 1.4 2002/10/11 23:32:30 rickg
 * <p>
 * Implementing individual execution of vmstocsh and csh
 * <p>
 * <p>
 * Revision 1.3 2002/10/10 18:53:29 rickg
 * <p>
 * Enabled SystemProgram debugging and remove local
 * <p>
 * writing to stdout.
 * <p>
 * <p>
 * Revision 1.2 2002/10/03 01:47:53 rickg
 * <p>
 * Reformat after emacs whitespace trashed it
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */

package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.comscript.Command;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.util.Utilities;

public class ComScriptProcess
  extends Thread
  implements SystemProcessInterface {

  public static final String rcsid =
    "$Id$";

  protected String name = null;
  protected File workingDirectory = null;
  private BaseProcessManager processManager;

  private boolean demoMode = false;
  private int demoTime = 5000;
  protected boolean debug = false;
  protected String[] errorMessage;
  private String[] warningMessage;
  private SystemProgram vmstocsh;
  protected SystemProgram csh;
  protected StringBuffer cshProcessID;
  protected AxisID axisID;
  protected String watchedFileName;
  private Command command = null;

  private boolean started = false;
  private boolean done = false;
  private boolean error = false;
  protected final ProcessMonitor processMonitor;
  private ProcessEndState endState = null;//used when processMonitor is null
  protected final BaseManager manager;

  public ComScriptProcess(BaseManager manager, String comScript,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor) {
    this.manager = manager;
    this.name = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    this.processMonitor = processMonitor;
  }

  public ComScriptProcess(BaseManager manager, Command comScriptCommand,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor) {
    this.manager = manager;
    this.name = comScriptCommand.getCommandLine();
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    command = comScriptCommand;
    this.processMonitor = processMonitor;
  }

  /**
   * Set the working directory in which the com script is to be run.
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Execute the specified com script. This can be initiated by the start()
   * function for the thread.
   */
  public void run() {

    if (demoMode) {
      try {
        started = true;
        csh = new SystemProgram(manager.getPropertyUserDir(), "nothing", axisID);
        csh.setExitValue(0);
        sleep(demoTime);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("Sleep interrupted");
      }
    }
    else {
      if (isComScriptBusy()) {
        error = true;
        errorMessage = new String[1];
        errorMessage[0] = name + " is already running";
        processManager.msgComScriptDone(this, 1);
        return;
      }

      if (!renameFiles()) {
        error = true;
        processManager.msgComScriptDone(this, 1);
      }
      //  Covert the com script to a sequence of csh commands
      String[] commands;
      try {
        commands = vmsToCsh();
      }
      catch (SystemProcessException except) {
        error = true;
        processManager.msgComScriptDone(this, vmstocsh.getExitValue());
        return;
      }
      catch (IOException except) {
        error = true;
        errorMessage = new String[1];
        errorMessage[0] = except.getMessage();
        processManager.msgComScriptDone(this, vmstocsh.getExitValue());
        return;
      }

      // Execute the csh commands
      started = true;
      try {
        execCsh(commands);
      }
      catch (SystemProcessException except) {
        try {
          errorMessage = parseError();
        }
        catch (IOException except2) {
          errorMessage = new String[1];
          errorMessage[0] = except2.getMessage();
        }
      }
      catch (IOException except) {
        errorMessage = new String[1];
        errorMessage[0] = except.getMessage();
      }

      // Get any warnings from the log file
      try {
        warningMessage = parseWarning();
      }
      catch (IOException except2) {
        errorMessage = new String[1];
        errorMessage[0] = except2.getMessage();
      }
    }
    
    // Send a message back to the ProcessManager that this thread is done.
    //  FIXME this modifies swing element within this thread!!!
    processManager.msgComScriptDone(this, csh.getExitValue());
    done = true;
  }
  
  protected boolean renameFiles() {
    try {
      renameFiles(name, watchedFileName, workingDirectory);
    }
    catch (IOException except) {
      except.printStackTrace();
      System.err.println(except.getMessage());
    }
    return true;
  }
  
  static protected void renameFiles(String name, String watchedFileName, 
      File workingDirectory) throws IOException {
    // Rename the logfile so that any log file monitor does not get confused
    // by an existing log file
    String logFileName = parseBaseName(name, ".com") + ".log";
    File logFile = new File(workingDirectory, logFileName);
    File oldLog = new File(workingDirectory, logFileName + "~");
    Utilities.renameFile(logFile, oldLog);
    
    if (watchedFileName != null) {
      File watchedFile = new File(workingDirectory, watchedFileName);
      File oldWatchedFile = new File(workingDirectory, watchedFileName + "~");
      Utilities.renameFile(watchedFile, oldWatchedFile);
    }
  }

  /**
   * Get the name of the com script
   */
  public String getScriptName() {
    return name;
  }
  
  public Command getCommand() {
    return command;
  }

  public ProcessName getProcessName() {
    return ProcessName.fromFileName(name, axisID, ".com");
  }

  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * Get the error message, if any from the run method.
   */
  public String[] getErrorMessage() {
    return errorMessage;
  }

  /**
   * Get the warning message, if any from the run method.
   */
  public String[] getWarningMessage() {
    return warningMessage;
  }

  /**
   * Get the standard output
   */
  public String[] getStdOutput() {
    if (csh != null) {
      return csh.getStdOutput();
    }
    else {
      return null;
    }
  }
  
  /**
   * Get the standard output while the process is running
   * getStdOutput() is already doing this
   */
  public String[] getCurrentStdOutput() {
    return getStdOutput();
  }

  /**
   * Get the standard error output
   */
  public String[] getStdError() {
    if (csh != null) {
      return csh.getStdError();
    }
    else {
      return null;
    }
  }

  /**
   * Get the csh process ID if it is available
   * 
   * @return
   */
  public String getShellProcessID() {
    if (cshProcessID == null) {
      return "";
    }
    return cshProcessID.toString();
  }

  /**
   * Get the debug state.
   */
  public boolean isDebug() {
    return debug;
  }

  /**
   * Set the debug state.
   */
  public void setDebug(boolean state) {
    debug = state;
  }

  /**
   * Extract the basename from a filename given the filename and the expected
   * extension.
   */
  static protected String parseBaseName(String filename, String extension) {
    int idxExtension = filename.indexOf(extension);
    String base = null;
    if (idxExtension > 0)
      base = filename.substring(0, idxExtension);
    return base;
  }

  /**
   * Execute the csh commands.
   */
  protected void execCsh(String[] commands) throws IOException,
      SystemProcessException {

    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    csh = new SystemProgram(manager.getPropertyUserDir(), "tcsh -ef", axisID);
    csh.setWorkingDirectory(workingDirectory);
    csh.setStdInput(commands);
    csh.setDebug(debug);
    ParsePID parsePID = new ParsePID(csh, cshProcessID);
    Thread parsePIDThread = new Thread(parsePID);
    parsePIDThread.start();

    csh.run();

    // Check the exit value, if it is non zero, parse the warnings and errors
    // from the log file.
    if (csh.getExitValue() != 0) {
      throw new SystemProcessException("");
    }
  }

  /**
   * Convert the com script to a sequence of csh command commands
   * 
   * @return A string array containing the csh command sequence
   */
  private String[] vmsToCsh() throws IOException, SystemProcessException {
    // Redirecting stdin for the command does not work even when a shell is
    // called, need to pump the com file directly into the stdin of vmstocsh
    String[] comSequence = loadFile();
    String commandLine = ApplicationManager.getIMODBinPath() + "vmstocsh "
        + parseBaseName(name, ".com") + ".log";
    vmstocsh = new SystemProgram(manager.getPropertyUserDir(), commandLine,
        axisID);
    vmstocsh.setWorkingDirectory(workingDirectory);
    vmstocsh.setStdInput(comSequence);
    vmstocsh.setDebug(debug);
    vmstocsh.run();

    if (vmstocsh.getExitValue() != 0) {
      errorMessage = new String[1];
      errorMessage[0] = "Running vmstocsh against " + name + " failed";
      throw new SystemProcessException("");
    }

    return vmstocsh.getStdOutput();
  }
  
  String getComScriptName() {
    return name;
  }

  /**
   * Load the comscript into a String[] for feeding into vmstocsh since we do
   * not have access to piping or standard input when running commands.
   * 
   * @return a String[] with each element containing a line of the com script
   */
  private String[] loadFile() throws IOException {

    //  Open the file as a stream
    InputStream fileStream = new FileInputStream(workingDirectory
      .getAbsolutePath()
        + "/" + name);

    BufferedReader fileReader = new BufferedReader(new InputStreamReader(
      fileStream));

    ArrayList lines = new ArrayList();
    String line;
    while ((line = fileReader.readLine()) != null) {
      lines.add(line);
    }
    return (String[]) lines.toArray(new String[lines.size()]);
  }

  /**
   * call parseError with the name member variable
   * @return
   * @throws IOException
   */
  protected String[] parseError() throws IOException {
    ArrayList errors = parseError(name, true);
    return (String[]) errors.toArray(new String[errors.size()]);
  }
  
  /**
    * Parse the log file for errors. Since the fortran code is not smart enough
    * handle formatted output we need find ERROR: in the middle of the output
    * stream. The error report starts with the ERROR: text instead of the whole
    * line.
    */
  protected ArrayList parseError(String name, boolean mustExist) 
      throws IOException {
        
    ArrayList errors = new ArrayList();
    
    File file = 
        new File(workingDirectory, parseBaseName(name, ".com") + ".log");
    if (!file.exists() && !mustExist) {
      return errors;
    }
    
    //  Open the file as a stream
    InputStream fileStream = new FileInputStream(file);

    BufferedReader fileReader = new BufferedReader(new InputStreamReader(
      fileStream));

    String line;
    boolean foundError = false;
    while ((line = fileReader.readLine()) != null) {
      if (!foundError) {
        int index = line.indexOf("ERROR:");
        if (index != -1) {
          foundError = true;
          errors.add(line.substring(index));
        }
      }
      else {
        errors.add(line);
      }
    }
    fileReader.close();
    return errors;
  }

  /**
   * 
   * @return
   * @throws IOException
   */
  protected String[] parseWarning() throws IOException {
    ArrayList errors = parseWarning(name, true);
    return (String[]) errors.toArray(new String[errors.size()]);
  }
  
  /**
   * Parse the log file for warnings. Since the fortran code is no smart enough
   * handle formatted output we need find WARNING: in the middle of the output
   * stream. The error report starts with the WARNING: text instead of the
   * whole line.
   * 
   * @return A ArrayList containing any errors. If the com script has not been
   *         run then null is returned. If the com script ran with no warnings
   *         then zero length array will be returned.
   */
  protected ArrayList parseWarning(String name, boolean mustExist) throws IOException {
    boolean nextLineIsWarning = false;
    
    ArrayList errors = new ArrayList();
    
    File file = 
        new File(workingDirectory, parseBaseName(name, ".com") + ".log");
    if (!file.exists() && !mustExist) {
      return errors;
    }
    //  Open the file as a stream
    InputStream fileStream = new FileInputStream(file);

    BufferedReader fileBuffer = new BufferedReader(new InputStreamReader(
      fileStream));

    String line;
    while ((line = fileBuffer.readLine()) != null) {
      int index = line.indexOf("WARNING:");
      if (index != -1) {
        nextLineIsWarning = false;
        int trimIndex = line.trim().indexOf("PIP WARNING:");
        if (trimIndex != -1
            && line.trim().length() 
              <= trimIndex + 1 + "PIP WARNING:".length()) {
          nextLineIsWarning = true;
        }
        errors.add(line.substring(index));
      }
      else if (nextLineIsWarning) {
        if (!line.matches("\\s+")) {
          errors.add(line.trim());
          if (line.indexOf("Using fallback options in Fortran code") != -1) {
            nextLineIsWarning = false;
          }
        }
        else {
          nextLineIsWarning = false;
        }
      }
    }
    fileBuffer.close();
    return errors;
  }

  /**
   * Returns true if the com script process is running, this does not include
   * vmstocsh process.
   */
  public boolean isStarted() {
    return started;
  }
  
  /**
   * Returns true if an error was found before the com script process started
   * running.
   */
  public boolean isError() {
    return error;
  }


  /**
   * Returns true if the com script process has completed and the process
   * manager has been notifified.
   */
  public boolean isDone() {
    return done;
  }

  /**
   * Returns the demoMode.
   * 
   * @return boolean
   */
  public boolean isDemoMode() {
    return demoMode;
  }

  /**
   * Sets the demoMode.
   * 
   * @param demoMode
   *          The demoMode to set
   */
  public void setDemoMode(boolean demoMode) {
    this.demoMode = demoMode;
  }

  /**
   * Get the number of milliseconds the demo process will run
   * @return
   */
  public int getDemoTime() {
    return demoTime;
  }
  
  /**
   * Used by BackgroundComScriptProcess.
   * Always returns false in ComScriptProcess because two regular comscript
   * processes cannot be run on the same axis.
   * @return
   */
  protected boolean isComScriptBusy() {
    return false;
  }
  
  /**
   * nothing to do
   */
  public void notifyKilled() {
  }
  
  public final void setProcessEndState(ProcessEndState endState) {
    if (processMonitor == null) {
      this.endState = endState;
    }
    else {
      processMonitor.setProcessEndState(endState);
    }
  }
  
  final ProcessEndState getProcessEndState() {
    if (processMonitor == null) {
      return endState;
    }
    return processMonitor.getProcessEndState();
  }
  
  public void kill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }
  
  public void pause(AxisID axisID) {
    throw new IllegalStateException("pause is not used by any ComScriptProcess");
  }
  
  public void signalKill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }
  
  public void signalInterrupt(AxisID axisID) {
    throw new IllegalStateException("signalInterrupt is not used by any ComScriptProcess");
  }
  
  public void setCurrentStdInput(String input) {
    throw new IllegalStateException("no ComScriptProcess writes to standard input while the process is running");
  }
}