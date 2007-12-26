package etomo.process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.ComscriptState;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.util.Utilities;

/**
 * <p>
 * Description: Provides a threadable class to execute IMOD com scripts in the
 * background.
 * </p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $Log$
 * <p> Revision 1.27  2007/09/07 00:18:22  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.26  2007/06/12 00:23:41  sueh
 * <p> bug# 1017 In parse() restored functionality which went to each log.  The
 * <p> grep in combine.com will be removed.  It is more portable to place this
 * <p> functionality in Etomo.  For compatibility with ComScriptProcesses with a
 * <p> non-standard log file, ComScriptProcess.parseLogFile has been added.
 * <p>
 * <p> Revision 1.25  2007/06/11 21:20:16  sueh
 * <p> bug# 1017 In parse() removed functionality which went to each log.  This
 * <p> is unnecessary because combine.com is grepping from error messages
 * <p> in the child log.
 * <p>
 * <p> Revision 1.24  2006/10/11 10:05:50  sueh
 * <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> FileException.
 * <p>
 * <p> Revision 1.23  2006/10/10 05:05:05  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.22  2006/06/05 16:17:46  sueh
 * <p> bug# 766 Removed an unused constructor.  Added ProcessData to the base
 * <p> class.
 * <p>
 * <p> Revision 1.21  2006/05/22 22:44:21  sueh
 * <p> bug# 577 Placed the command in a String[] rather then a String.
 * <p>
 * <p> Revision 1.20  2006/01/26 21:49:58  sueh
 * <p> bug# 401 Added a ProcessResultDisplay member variable
 * <p>
 * <p> Revision 1.19  2005/11/19 02:04:01  sueh
 * <p> bug# 744 Removing BackgroundComScriptMonitor.  Using
 * <p> DetachedProcessMonitor with both DetachedProcess and
 * <p> BackgroundComScriptProcess.  BackgroundComScriptProcess is also
 * <p> detached.  Detached monitors don't wait for interrupts so they need
 * <p> isProcessRunning().  The other functions in
 * <p> BackgroundComScriptMonitor are unnecessary.  Added
 * <p> setOutputFileName() to DetachedProcessMonitor.  Made protected fields
 * <p> in ComScriptMonitor private.  Getting them with get functions.
 * <p>
 * <p> Revision 1.18  2005/11/02 21:40:24  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.
 * <p> Replaced functions parseWarning() and parseError() with parse.  Put
 * <p> error messages created in this function directly into processMessages.
 * <p>
 * <p> Revision 1.17  2005/08/30 18:30:55  sueh
 * <p> bug# 532 Changed BackgroundProcessMonitor to
 * <p> BackgroundComScriptMonitor.  Using BackgroundProcessMonitor for
 * <p> BackgroundProcess's that need monitors.
 * <p>
 * <p> Revision 1.16  2005/07/29 00:50:48  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.15  2005/07/26 17:33:24  sueh
 * <p> bug# 701 Passing the monitor to ComScriptProcess so that
 * <p> ProcessEndState can be recorded
 * <p>
 * <p> Revision 1.14  2005/04/25 20:42:44  sueh
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 1.13  2005/02/09 18:40:00  sueh
 * <p> Removing print statements.
 * <p>
 * <p> Revision 1.12  2005/02/09 01:27:55  sueh
 * <p> bug# 598 isComScriptBusy(): Using the index of NAME to find the
 * <p> directory path and file name, rather then splitting.  There could be spaces
 * <p> in the directory path and each output line has an unknown number of
 * <p> columns.  However the columns are spaced to always start under their
 * <p> column header and NAME is always the last column.
 * <p>
 * <p> Revision 1.11  2005/02/07 23:48:40  sueh
 * <p> bug# 595 isComScriptBusy(): Checking the os and only running lsof on
 * <p> linux and mac.
 * <p>
 * <p> Revision 1.10  2005/01/05 19:49:27  sueh
 * <p> bug# 578 Passing BaseProcessManager to constructor instead of
 * <p> ProcessManager.
 * <p>
 * <p> Revision 1.9  2004/12/04 00:34:49  sueh
 * <p> bug# 569 Handling directory paths with spaces:  converting from a
 * <p> command line to a command array to prevent the command line from
 * <p> being split on white space.
 * <p>
 * <p> Revision 1.8  2004/08/30 18:41:29  sueh
 * <p> bug# 508 ProcessManager.kill() is using notifyKill() to tell this object that
 * <p> a kill has been requested.  KillMonitor() can be private
 * <p>
 * <p> Revision 1.7  2004/08/28 02:41:39  sueh
 * <p> bug# 508 isComScriptBusy: The output from Mac lsof is a little different.
 * <p> Stopped checking the type since the value is different on Mac; it wasn't
 * <p> necessary anyway.  Since more then one field can be empty and name is
 * <p> always the last field, setting name index to the last field index when
 * <p> idxNAME is incorrect.
 * <p>
 * <p> Revision 1.6  2004/08/28 00:45:08  sueh
 * <p> bug# 508 adding isComScriptBusy() - uses lsof to check for an open
 * <p> combine.log.  This will only work on Mac and Linux.
 * <p> Changed renameFiles so it can report an error if the rename fails.
 * <p> In Windows this could mean that combine.com is already running.
 * <p> Adding code to parse combine.out because ParseBackgroundPID runs
 * <p> on a separate thread and watches a system program.  Tried to put the
 * <p> code I needed from ParseBackgroundPID.parsePIDString() into a static
 * <p> function in ParseBackgroundPID, but it gave a file not found error, so
 * <p> I created a function parsePIDString(File) in this object.
 * <p>
 * <p> Revision 1.5  2004/08/24 20:43:35  sueh
 * <p> bug# 508 change kill() to killMonitor()
 * <p>
 * <p> Revision 1.4  2004/08/23 23:31:49  sueh
 * <p> bug# 508 moved rename combine.out, removed unecessary
 * <p> throws from parseWarning and parseError, changed setKilled(boolean)
 * <p> to kill()
 * <p>
 * <p> Revision 1.3  2004/08/20 21:39:45  sueh
 * <p> bug# 508 added parseWarning()
 * <p>
 * <p> $Revision 1.2  2004/08/19 01:45:03  sueh
 * <p> $bug# 508 Removed the -e when running combine.csh, because it
 * <p> $wasn't returning errors.  Added file renaming based the state of
 * <p> $combine.com.  Added parsed errors from combine.log and child .log
 * <p> $files based on the state of combine.com.  Added a function to kill the
 * <p> $process monitor.
 * <p> $Added:
 * <p> $BackgroundProcessMonitor backgroundProcessMonitor
 * <p> $ComscriptState comscriptState
 * <p> $BackgroundComScriptProcess(String comScript,
 * <p> $    ProcessManager processManager, AxisID axisID,
 * <p> $    String watchedFileName,
 * <p> $    BackgroundProcessMonitor backgroundProcessMonitor,
 * <p> $    ComscriptState comscriptState)
 * <p> $parseError()
 * <p> $renameFiles()
 * <p> $setKilled(boolean killed)
 * <p> $Changed:
 * <p> $execCsh(String[] commands)
 * <p> $makeRunCshFile(File runCshFile, String cshFileName,
 * <p> $    String outFileName)
 * <p> $Deleted:
 * <p> $BackgroundComScriptProcess(
 * <p> $    String comScript,
 * <p> $    ProcessManager processManager,
 * <p> $    AxisID axisID,
 * <p> $    String watchedFileName)
 * <p> $
 * <p> $Revision 1.1  2004/08/06 22:58:19  sueh
 * <p> $bug# 508 Runs comscripts in the background by placing
 * <p> $vmstocsh output in  .csh file, and running the .csh file with an
 * <p> $"&".  Also need to send output to a file.  In order to run the
 * <p> $.csh file with an "&", runing it from another .csh file, which is
 * <p> $generated by this object.
 * <p> </p>
 */
public class BackgroundComScriptProcess extends ComScriptProcess {
  public static final String rcsid = "$$Id$$";

  private final AxisID axisID;
  private final ComscriptState comscriptState;
  private final BaseManager manager;
  private StringBuffer processID = new StringBuffer();

  /**
   * @param comScript
   * @param processManager
   * @param axisID
   * @param watchedFileName
   */
  public BackgroundComScriptProcess(BaseManager manager, String comScript,
      BaseProcessManager processManager, AxisID axisID, String watchedFileName,
      DetachedProcessMonitor monitor, ComscriptState comscriptState) {
    super(manager, comScript, processManager, axisID, watchedFileName, monitor);
    this.comscriptState = comscriptState;
    this.axisID = axisID;
    this.manager = manager;
    setParseLogFile(false);
  }

  /**
   * Since background processes can run after etomo has exited, it would be
   * easy to start a second combine that would interfer and cause
   * file corruption.
   * Check to see if comscript log is open by using lsof (list open files)
   * If it is, stop the monitor and return false
   * If the monitor isn't stopped it reattaches to the existing combine.log
   */
  protected boolean isComScriptBusy() {
    String osName = System.getProperty("os.name").toLowerCase();
    //lsof does not exist in Windows.  In Windows, a busy log file will be
    //detected when the rename fails.
    if (osName.indexOf("linux") == -1 && osName.indexOf("mac os") == -1) {
      return false;
    }
    File pidFile = new File(getWorkingDirectory(), getWatchedFileName());
    String groupPid = null;
    SystemProgram lsof = null;
    if (pidFile.exists()) {
      groupPid = parsePIDString(pidFile);
    }
    if (groupPid == null) {
      String[] command = new String[] { "/usr/sbin/lsof", "-w", "-S", "-l",
          "-M", "-L" };
      lsof = new SystemProgram(manager.getPropertyUserDir(), command, axisID);
    }
    else {
      String[] command = new String[] { "/usr/sbin/lsof", "-w", "-S", "-l",
          "-M", "-L", "-g", groupPid };
      lsof = new SystemProgram(manager.getPropertyUserDir(), command, axisID);
    }
    lsof.run();
    String[] stdout = lsof.getStdOutput();
    if (stdout == null || stdout.length == 0) {
      return false;
    }
    String header = stdout[0].trim();
    int nameIndex = header.indexOf(" NAME") + 1;
    //Return false if the NAME field is not found
    if (nameIndex == 0) {
      return false;
    }
    String[] fields;
    //File comscriptLog = new File(getWorkingDirectory(), comscriptState
    //    .getComscriptName()
    //    + ".log");
    LogFile comscriptLog = LogFile.getInstance(getWorkingDirectory()
        .getAbsolutePath(), axisID, comscriptState.getComscriptName());
    for (int i = 1; i < stdout.length; i++) {
      //check for missing size entry - assume name is last
      if (stdout[i].substring(nameIndex).trim().equals(
          comscriptLog.getAbsolutePath())) {
        if (getMonitor() != null) {
          getMonitor().kill(this, axisID);
        }
        return true;
      }
    }
    return false;
  }

  protected boolean renameFiles() {
    try {
      renameFiles(getComScriptName(), getWatchedFileName(),
          getWorkingDirectory(), logFile);
    }
    catch (LogFile.FileException e) {
      getProcessMessages().addError(e.getMessage());
      getProcessMessages().addError(
          getComScriptName() + " may already be running.  Check the log file.");
      e.printStackTrace();
      return false;
    }
    int startCommand = comscriptState.getStartCommand();
    int endCommand = comscriptState.getEndCommand();
    int index = startCommand;
    while (index <= endCommand) {
      try {
        renameFiles(comscriptState.getCommand(index) + ".com", comscriptState
            .getWatchedFile(index), getWorkingDirectory(), LogFile.getInstance(
            manager.getPropertyUserDir(), getAxisID(), comscriptState
                .getCommand(index)));
      }
      catch (LogFile.FileException e) {
        getProcessMessages().addError(e.getMessage());
        getProcessMessages().addError(
            getComScriptName()
                + " may already be running.  Check the log file.");
        e.printStackTrace();
        return false;
      }
      index++;
    }
    return true;
  }

  /**
   * Places commmands in the .csh file.  Creates and runs a file containing
   * commands to execute the .csh file in the background.  
   */
  protected void execCsh(String[] commands) throws IOException,
      SystemProcessException {
    File workingDirectory = getWorkingDirectory();
    String runName = parseBaseName(getComScriptName(), ".com");
    String cshFileName = runName + ".csh";
    File cshFile = new File(workingDirectory, cshFileName);

    String runCshFileName = "run" + runName + ".csh";
    File runCshFile = new File(workingDirectory, runCshFileName);

    File outFile = new File(workingDirectory, getWatchedFileName());

    Utilities.writeFile(cshFile, commands, true);
    makeRunCshFile(runCshFile, cshFileName, getWatchedFileName());

    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06 
    String[] command = { "tcsh", "-f", runCshFile.getAbsolutePath() };
    BackgroundSystemProgram program = new BackgroundSystemProgram(manager,
        command, getDetachedMonitor(), getAxisID());
    setSystemProgram(program);
    program.setWorkingDirectory(workingDirectory);
    program.setDebug(EtomoDirector.INSTANCE.getArguments().isDebug());

    ParseBackgroundPID parsePID = new ParseBackgroundPID(program, processID,
        outFile, getProcessData());
    Thread parsePIDThread = new Thread(parsePID);
    parsePIDThread.start();
    //make sure nothing else is writing or backing up the log files
    long logWriteId = logFile.openForWriting();

    program.run();

    //release the log files
    logFile.closeForWriting(logWriteId);
    // Check the exit value, if it is non zero, parse the warnings and errors
    // from the log file.
    if (program.getExitValue() != 0) {
      throw new SystemProcessException("");
    }
  }

  private final DetachedProcessMonitor getDetachedMonitor() {
    return (DetachedProcessMonitor) super.getMonitor();
  }

  /**
   * create a csh file to run commandname.csh (created from commandname.com).
   * To avoid hangups when quitting Etomo or logging out, put nohup on the first
   * line and send the output to a file.
   * @param runCshFile
   * @param cshFileName
   * @param runName
   * @throws IOException
   */
  private void makeRunCshFile(File runCshFile, String cshFileName,
      String outFileName) throws IOException {
    if (runCshFile == null) {
      throw new IOException("unable to create " + runCshFile.getAbsolutePath());
    }
    if (runCshFile.exists()) {
      return;
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(
        runCshFile));
    if (bufferedWriter == null) {
      throw new IOException("unable to write to "
          + runCshFile.getAbsolutePath());
    }
    bufferedWriter.write("nohup");
    bufferedWriter.newLine();
    bufferedWriter.write("tcsh -f " + cshFileName + ">&" + outFileName + "&");
    bufferedWriter.newLine();
    bufferedWriter.close();
  }

  /**
   * kill monitor when notified that a kill was done
   */
  public void notifyKilled() {
    if (getMonitor() != null) {
      getMonitor().kill(this, axisID);
    }
    super.notifyKilled();
  }

  /**
   * Parses errors and warnings from log files.
   * Parses errors and warnings from the comscript and all child comscripts found in
   * comscriptState that may have been executed.
   */
  protected void parse() throws LogFile.ReadException {
    parse(getComScriptName(), true);
    int startCommand = comscriptState.getStartCommand();
    int endCommand = comscriptState.getEndCommand();
    int index = startCommand;
    while (index <= endCommand) {
      parse(comscriptState.getCommand(index) + ".com", false);
      index++;
    }
  }

  /** 
   * want to parse the pid file on this thread, without access to the system
   * program thread so I can't use ParseBackgroundPID.
   * @param outFile
   * @return
   */
  private String parsePIDString(File outFile) {
    StringBuffer PID = new StringBuffer();
    BufferedReader bufferedReader = null;
    try {
      bufferedReader = new BufferedReader(new FileReader(outFile));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      closeFile(bufferedReader);
      return null;
    }
    String line;
    try {
      if ((line = bufferedReader.readLine()) != null) {
        if (line.startsWith("Shell PID:")) {
          String[] tokens = line.split("\\s+");
          if (tokens.length > 2) {
            PID.append(tokens[2]);
          }
        }
      }
      getProcessData().setPid(PID.toString());
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    closeFile(bufferedReader);
    return PID.toString();
  }

  private void closeFile(BufferedReader bufferedReader) {
    try {
      if (bufferedReader != null) {
        bufferedReader.close();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get the csh process ID if it is available
   * 
   * @return
   */
  public final String getShellProcessID() {
    if (processID == null) {
      return "";
    }
    return processID.toString();
  }
}