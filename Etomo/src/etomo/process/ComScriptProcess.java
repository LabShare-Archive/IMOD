/**
 * <p>
 * Description: Provides a threadable class to execute IMOD com scripts.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002 - 2006
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
 * Revision 3.41  2006/09/25 16:34:55  sueh
 * bug# 931 renameFiles():  Tell the processMonitor when the log file has been
 * renamed.
 *
 * Revision 3.40  2006/07/27 19:36:36  sueh
 * bug# 908 Run():  parsing warning whether or not the script finishes
 *
 * Revision 3.39  2006/06/15 16:16:30  sueh
 * bug# 871 Added isNohup().
 *
 * Revision 3.38  2006/06/05 16:22:29  sueh
 * bug# 766 Added ProcessData.
 *
 * Revision 3.37  2006/05/22 22:46:41  sueh
 * bug# 577 Removed constructors which accepted a String command.
 *
 * Revision 3.36  2006/05/11 19:52:08  sueh
 * bug# 838 Add CommandDetails, which extends Command and
 * ProcessDetails.  Changed ProcessDetails to only contain generic get
 * functions.  Command contains all the command oriented functions.
 *
 * Revision 3.35  2006/01/31 20:43:00  sueh
 * bug# 521 Added setProcessResultDisplay to SystemProcessInterface.
 * This allows the last ProcessResultDisplay used by the combine monitor
 * to be assigned to the process.
 *
 * Revision 3.34  2006/01/26 21:54:17  sueh
 * bug# 401 Added a ProcessResultDisplay member variable
 *
 * Revision 3.33  2006/01/20 20:52:54  sueh
 * updated copyright year
 *
 * Revision 3.32  2005/11/19 02:20:23  sueh
 * bug# 744 Made protected fields in ComScriptProcess private.  Getting
 * them with get functions.
 *
 * Revision 3.31  2005/11/02 21:57:21  sueh
 * bug# 754 Parsing errors and warnings inside ProcessMessages.
 * Removed errorMessage and warningMessage.  Added processMessages.
 * Removed functions getErrorMessage, parseError, and parseWarning.
 * Added functions getProcessMessages, and parse.
 *
 * Revision 3.30  2005/10/29 00:04:54  sueh
 * bug# 747 Sending error and warning messages to the error log.
 *
 * Revision 3.29  2005/10/21 19:55:08  sueh
 * bug# 742 Added getCurrentStdError().
 *
 * Revision 3.28  2005/08/30 18:41:15  sueh
 * bug# 532 Made processMonitor protected so it can be used by the child
 * class.
 *
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
import etomo.comscript.CommandDetails;
import etomo.comscript.ProcessDetails;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;

public class ComScriptProcess extends Thread implements SystemProcessInterface {

  public static final String rcsid = "$Id$";

  private String comScriptName = null;
  private File workingDirectory = null;
  private BaseProcessManager processManager;

  private boolean demoMode = false;
  private int demoTime = 5000;
  private boolean debug = false;
  private SystemProgram vmstocsh;
  private SystemProgram csh;
  private StringBuffer cshProcessID;
  private AxisID axisID;
  private String watchedFileName;
  private Command command = null;
  private ProcessDetails processDetails = null;
  private CommandDetails commandDetails = null;
  private final ProcessData processData;

  private boolean started = false;
  private boolean error = false;
  private final ProcessMonitor processMonitor;
  private ProcessEndState endState = null;//used when processMonitor is null
  private final BaseManager manager;
  private final ProcessMessages processMessages = ProcessMessages.getInstance();
  private ProcessResultDisplay processResultDisplay = null;
  protected final LogFile logFile;

  public ComScriptProcess(BaseManager manager, String comScript,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor, ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.comScriptName = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    this.processMonitor = processMonitor;
    this.processResultDisplay = processResultDisplay;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  public ComScriptProcess(BaseManager manager, String comScript,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor, ProcessResultDisplay processResultDisplay,
      ProcessDetails processDetails) {
    this.manager = manager;
    this.comScriptName = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    this.processMonitor = processMonitor;
    this.processResultDisplay = processResultDisplay;
    this.processDetails = processDetails;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  public ComScriptProcess(BaseManager manager, String comScript,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor) {
    this.manager = manager;
    this.comScriptName = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    this.processMonitor = processMonitor;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  public ComScriptProcess(BaseManager manager, CommandDetails commandDetails,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor) {
    this.manager = manager;
    this.comScriptName = commandDetails.getCommand();
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    processDetails = commandDetails;
    command = commandDetails;
    this.commandDetails = commandDetails;
    this.processMonitor = processMonitor;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  public ComScriptProcess(BaseManager manager, CommandDetails commandDetails,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor, ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.comScriptName = commandDetails.getCommand();
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    command = commandDetails;
    processDetails = commandDetails;
    this.commandDetails = commandDetails;
    this.processMonitor = processMonitor;
    this.processResultDisplay = processResultDisplay;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  public ComScriptProcess(BaseManager manager, Command command,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      ProcessMonitor processMonitor, ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.comScriptName = command.getCommand();
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
    this.watchedFileName = watchedFileName;
    this.command = command;
    this.processMonitor = processMonitor;
    this.processResultDisplay = processResultDisplay;
    processData = ProcessData.getManagedInstance(axisID, manager,
        getProcessName());
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        getProcessName());
  }

  /**
   * Set the working directory in which the com script is to be run.
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  final ProcessResultDisplay getProcessResultDisplay() {
    return processResultDisplay;
  }

  public final void setProcessResultDisplay(
      ProcessResultDisplay processResultDisplay) {
    this.processResultDisplay = processResultDisplay;
  }

  public final ProcessData getProcessData() {
    return processData;
  }

  /**
   * Execute the specified com script. This can be initiated by the start()
   * function for the thread.
   */
  public void run() {
    processData.reset();
    if (demoMode) {
      try {
        started = true;
        csh = new SystemProgram(manager.getPropertyUserDir(),
            new String[] { "nothing" }, axisID);
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
        processMessages.addError(comScriptName + " is already running");
        processManager.msgComScriptDone(this, 1);
        return;
      }

      try {
        if (!renameFiles()) {
          error = true;
          processManager.msgComScriptDone(this, 1);
        }
      }
      catch (LogFile.BackupException except) {
        processManager.msgComScriptDone(this, vmstocsh.getExitValue());
        return;
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
        processMessages.addError(except.getMessage());
        processManager.msgComScriptDone(this, vmstocsh.getExitValue());
        return;
      }

      // Execute the csh commands
      started = true;
      try {
        execCsh(commands);
      }
      catch (SystemProcessException except) {
      }
      catch (IOException except) {
        processMessages.addError(except.getMessage());
      }
      try {
        parse();
      }
      catch (LogFile.ReadException except2) {
        processMessages.addError(except2.getMessage());
      }
    }

    // Send a message back to the ProcessManager that this thread is done.
    //  FIXME this modifies swing element within this thread!!!
    processManager.msgComScriptDone(this, csh.getExitValue());
  }

  protected boolean renameFiles() throws LogFile.BackupException{
      renameFiles(comScriptName, watchedFileName, workingDirectory, logFile);
    if (processMonitor != null) {
      processMonitor.msgLogFileRenamed();
    }
    return true;
  }

  static protected void renameFiles(String name, String watchedFileName,
      File workingDirectory, LogFile logFile) throws LogFile.BackupException{
    // Rename the logfile so that any log file monitor does not get confused
    // by an existing log file
    //String logFileName = parseBaseName(name, ".com") + ".log";
    //File logFile = new File(workingDirectory, logFileName);
    //File oldLog = new File(workingDirectory, logFileName + "~");
    logFile.backup();
    //Utilities.renameFile(logFile, oldLog);
    if (watchedFileName != null) {
      LogFile watchedFile = LogFile.getInstance(workingDirectory
          .getAbsolutePath(), watchedFileName);
      watchedFile.backup();
      /*File watchedFile = new File(workingDirectory, watchedFileName);
       File oldWatchedFile = new File(workingDirectory, watchedFileName + "~");
       Utilities.renameFile(watchedFile, oldWatchedFile);*/
    }
  }

  public ProcessDetails getProcessDetails() {
    return processDetails;
  }

  public Command getCommand() {
    return command;
  }

  public CommandDetails getCommandDetails() {
    return commandDetails;
  }

  public ProcessName getProcessName() {
    return ProcessName.fromFileName(comScriptName, axisID, ".com");
  }

  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * Get the messages, if any from the run method.
   */
  public ProcessMessages getProcessMessages() {
    return processMessages;
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
    csh = new SystemProgram(manager.getPropertyUserDir(), new String[] {
        "tcsh", "-ef" }, axisID);
    csh.setWorkingDirectory(workingDirectory);
    csh.setStdInput(commands);
    csh.setDebug(debug);
    ParsePID parsePID = new ParsePID(csh, cshProcessID, processData);
    Thread parsePIDThread = new Thread(parsePID);
    parsePIDThread.start();
    //make sure nothing else is writing or backing up the log file
    long logWriteId = logFile.openForWriting();

    csh.run();

    //release the log file
    logFile.closeForWriting(logWriteId);
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
    String[] command = new String[] {
        ApplicationManager.getIMODBinPath() + "vmstocsh", logFile.getName() };
    //parseBaseName(comScriptName, ".com") + ".log" };
    vmstocsh = new SystemProgram(manager.getPropertyUserDir(), command, axisID);
    vmstocsh.setWorkingDirectory(workingDirectory);
    vmstocsh.setStdInput(comSequence);
    vmstocsh.setDebug(debug);
    vmstocsh.run();

    if (vmstocsh.getExitValue() != 0) {
      processMessages.addError("Running vmstocsh against " + comScriptName
          + " failed");
      throw new SystemProcessException("");
    }

    return vmstocsh.getStdOutput();
  }

  String getComScriptName() {
    return comScriptName;
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
        + "/" + comScriptName);

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
   * 
   * @return
   * @throws IOException
   */
  protected void parse() throws LogFile.ReadException {
    parse(comScriptName, true);
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
  protected final void parse(String name, boolean mustExist)
      throws LogFile.ReadException {
    ArrayList errors = new ArrayList();
    //File file = new File(workingDirectory, parseBaseName(name, ".com") + ".log");
    if (!logFile.exists() && !mustExist) {
      return;
    }
    processMessages.addProcessOutput(logFile);
    processMessages.print();
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
   * Always returns true because all comscripts a piped to tcsh and do not
   * disconnect when etomo exits.
   */
  public boolean isNohup() {
    return true;
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
    setProcessEndState(ProcessEndState.KILLED);
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

  public final void kill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  public void pause(AxisID axisID) {
    throw new IllegalStateException("pause is not used by any ComScriptProcess");
  }

  public void signalKill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  protected final ProcessMonitor getMonitor() {
    return processMonitor;
  }

  protected final File getWorkingDirectory() {
    return workingDirectory;
  }

  protected final String getWatchedFileName() {
    return watchedFileName;
  }

  protected final void setSystemProgram(SystemProgram systemProgram) {
    csh = systemProgram;
  }
}