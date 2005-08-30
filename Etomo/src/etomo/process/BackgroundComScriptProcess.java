package etomo.process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.comscript.ComscriptState;
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
  
  private ComscriptState comscriptState;

  /**
   * @param comScript
   * @param processManager
   * @param axisID
   * @param watchedFileName
   */
  public BackgroundComScriptProcess(BaseManager manager, String comScript,
    BaseProcessManager processManager, AxisID axisID, String watchedFileName,
    BackgroundComScriptMonitor monitor, 
    ComscriptState comscriptState) {
    super(manager, comScript, processManager, axisID, watchedFileName, monitor);
    this.comscriptState = comscriptState;
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
    File pidFile = new File(workingDirectory, watchedFileName);
    String groupPid = null;
    SystemProgram lsof = null;
    if (pidFile.exists()) {
      groupPid = parsePIDString(pidFile);
    }
    if (groupPid == null) {
      lsof = new SystemProgram(manager.getPropertyUserDir(),
          "/usr/sbin/lsof -w -S -l -M -L", axisID);     
    }
    else {
      lsof =
          new SystemProgram(manager.getPropertyUserDir(),
          "/usr/sbin/lsof -w -S -l -M -L -g " + groupPid, axisID);
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
    File comscriptLog = new File(workingDirectory, comscriptState.getComscriptName() + ".log");
    for (int i = 1; i < stdout.length; i++) {
      //check for missing size entry - assume name is last
      if (stdout[i].substring(nameIndex).trim().equals(comscriptLog.getAbsolutePath())){
        killMonitor();
        return true;
      }
    }
    return false;
  }
  
  protected boolean renameFiles() {
    try {
      renameFiles(name, watchedFileName, workingDirectory);
    }
    catch (IOException e) {
      errorMessage = new String[2];
      errorMessage[0] = e.getMessage();
      errorMessage[1] = name + " may already be running.  Check the log file.";
      e.printStackTrace();
      return false;
    }
    int startCommand = comscriptState.getStartCommand();
    int endCommand = comscriptState.getEndCommand();
    int index = startCommand;
    while (index <= endCommand) {
      try {
        renameFiles(comscriptState.getCommand(index) + ".com", 
          comscriptState.getWatchedFile(index), workingDirectory);
      }
      catch (IOException e) {
        errorMessage = new String[2];
        errorMessage[0] = e.getMessage();
        errorMessage[1] = 
          name + " may already be running.  Check the log file.";
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
    String runName = parseBaseName(name, ".com");
    String cshFileName = runName + ".csh";
    File cshFile = new File(workingDirectory, cshFileName);
    
    String runCshFileName = "run" + runName + ".csh";
    File runCshFile = new File(workingDirectory, runCshFileName);

    File outFile = new File(workingDirectory, watchedFileName);
    
    Utilities.writeFile(cshFile, commands, true);
    makeRunCshFile(runCshFile, cshFileName, watchedFileName);
    
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06 
    String[] command = { "tcsh", "-f", runCshFile.getAbsolutePath() };
    csh = new BackgroundSystemProgram(manager, command, (BackgroundComScriptMonitor) processMonitor);
    csh.setWorkingDirectory(workingDirectory);
    csh.setDebug(debug);
    
    ParseBackgroundPID parsePID = 
        new ParseBackgroundPID(csh, cshProcessID, outFile);
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
   * create a csh file to run commandname.csh (created from commandname.com).
   * To avoid hangups when quitting Etomo or logging out, put nohup on the first
   * line and send the output to a file.
   * @param runCshFile
   * @param cshFileName
   * @param runName
   * @throws IOException
   */
  private void makeRunCshFile(File runCshFile, String cshFileName, String outFileName)
    throws IOException {
    if (runCshFile == null) {
      throw new IOException("unable to create " + runCshFile.getAbsolutePath());
    }
    if (runCshFile.exists()) {
      return;
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(runCshFile));
    if (bufferedWriter == null) {
      throw new IOException("unable to write to " + runCshFile.getAbsolutePath());
    }
    bufferedWriter.write("nohup");
    bufferedWriter.newLine();
    bufferedWriter.write("tcsh -f " + cshFileName + ">&" + outFileName + "&");
    bufferedWriter.newLine();
    bufferedWriter.close();
  }
  
  /**
   * set killed in the process monitor
   * @param killed
   */
  private void killMonitor() {
    if (processMonitor != null) {
      ((BackgroundComScriptMonitor) processMonitor).kill();
    }
  }
  
  /**
   * kill monitor when notified that a kill was done
   */
  public void notifyKilled() {
    killMonitor();
  }
  
  /**
   * Parses errors from log files.
   * Parses errors from the comscript and all child comscripts found in
   * comscriptState that may have been executed.
   */
  protected String[] parseError() throws IOException {
    ArrayList errors = parseError(name, true);
    int startCommand = comscriptState.getStartCommand();
    int endCommand = comscriptState.getEndCommand();
    int index = startCommand;
    while (index <= endCommand) {
      errors.addAll(
          parseError(comscriptState.getCommand(index) + ".com", false));
      index++;
    }
    return (String[]) errors.toArray(new String[errors.size()]);
  }
 
  /**
   * Parses warnings from log files.
   * Parses warnings from the comscript and all child comscripts found in
   * comscriptState that may have been executed.
   */
  protected String[] parseWarning() throws IOException {
    ArrayList errors = parseWarning(name, true);
    int startCommand = comscriptState.getStartCommand();
    int endCommand = comscriptState.getEndCommand();
    int index = startCommand;
    while (index <= endCommand) {
      errors.addAll(
          parseWarning(comscriptState.getCommand(index) + ".com", false));
      index++;
    }
    return (String[]) errors.toArray(new String[errors.size()]);
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
}
