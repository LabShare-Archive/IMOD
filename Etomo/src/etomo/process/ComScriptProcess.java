package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.Utilities;

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
public class ComScriptProcess
  extends Thread
  implements SystemProcessInterface {

  public static final String rcsid =
    "$Id$";

  private String name = null;
  private File workingDirectory = null;
  private ProcessManager processManager;

  private boolean demoMode = false;
  private int demoTime = 5000;
  private boolean debug = false;
  private String[] errorMessage;
  private String[] warningMessage;
  private SystemProgram vmstocsh;
  private SystemProgram csh;
  private StringBuffer cshProcessID;
  private AxisID axisID;

  private boolean started = false;
  private boolean done = false;

  public ComScriptProcess(
    String comScript,
    ProcessManager processManager,
    AxisID axisID) {
    this.name = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
    this.axisID = axisID;
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
        csh = new SystemProgram("nothing");
        csh.setExitValue(0);
        sleep(demoTime);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("Sleep interrupted");
      }
    }
    else {

      // Rename the logfile so that any log file monitor does not get confused
      // by an existing log file
      renameLogFile();

      //  Covert the com script to a sequence of csh commands
      String[] commands;
      try {
        commands = vmsToCsh();
      }
      catch (SystemProcessException except) {
        processManager.msgComScriptDone(this, vmstocsh.getExitValue());
        return;
      }
      catch (IOException except) {
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

  /**
   * Rename the log to backup file (tilde)
   * This need serious work arounds because of the random failure bugs on
   * windows.  See sun java bugs: 4017593, 4017593, 4042592
   */
  private void renameLogFile() {
    try {
      Utilities.debugPrint("In renameLogFile");
      String logFileName = parseBaseName(name, ".com") + ".log";

      // Delete the existing backup file if it exists, otherwise the call will
      // fail on windows 
      File oldLog = new File(workingDirectory, logFileName + "~");
      if (oldLog.exists()) {
        Utilities.debugPrint(oldLog.getAbsolutePath() + " exists, deleting");
        if (!oldLog.delete()) {
          System.err.println(
            "Unable to delete backup log file: " + oldLog.getAbsolutePath());
          if (oldLog.exists()) {
            System.err.println(oldLog.getAbsolutePath() + " still exists!");
          }
          else {
            System.err.println(oldLog.getAbsolutePath() + " does not exist!");
          }
        }
      }

      // Rename the existing log file
      File logFile = new File(workingDirectory, logFileName);
      if (logFile.exists()) {
        Utilities.debugPrint(logFile.getAbsolutePath() + " exists");

        if (!logFile.renameTo(oldLog)) {
          if (logFile.exists()) {
            System.err.println(logFile.getAbsolutePath() + " still exists");
          }
          else {
            System.err.println(logFile.getAbsolutePath() + " does not exist!");
          }

          if (oldLog.exists()) {
            System.err.println(oldLog.getAbsolutePath() + " still exists!");
          }
          else {
            System.err.println(oldLog.getAbsolutePath() + " does not exist");
          }
          System.err.println(
            "Unable to rename log file to: " + oldLog.getAbsolutePath());
        }
      }
    }
    catch (Exception except) {
      except.printStackTrace();
    }
  }

  /**
   * Get the name of the com script
   */
  public String getScriptName() {
    return name;
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
  private String parseBaseName(String filename, String extension) {
    int idxExtension = filename.indexOf(extension);
    String base = null;
    if (idxExtension > 0)
      base = filename.substring(0, idxExtension);
    return base;
  }

  /**
   * Execute the csh commands.
   */
  private void execCsh(String[] commands)
    throws IOException, SystemProcessException {

    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    csh = new SystemProgram("tcsh -ef");
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
    String imodBinPath =
      ApplicationManager.getIMODDirectory().getAbsolutePath()
        + File.separator
        + "bin"
        + File.separator;
    String commandLine =
      imodBinPath + "vmstocsh " + parseBaseName(name, ".com") + ".log";
    vmstocsh = new SystemProgram(commandLine);
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

  /**
   * Load the comscript into a String[] for feeding into vmstocsh since we do
   * not have access to piping or standard input when running commands.
   * 
   * @return a String[] with each element containing a line of the com script
   */
  private String[] loadFile() throws IOException {

    //  Open the file as a stream
    InputStream fileStream =
      new FileInputStream(workingDirectory.getAbsolutePath() + "/" + name);

    BufferedReader fileReader =
      new BufferedReader(new InputStreamReader(fileStream));

    ArrayList lines = new ArrayList();
    String line;
    while ((line = fileReader.readLine()) != null) {
      lines.add(line);
    }
    return (String[]) lines.toArray(new String[lines.size()]);
  }

  /**
   * Parse the log file for errors. Since the fortran code is not smart enough
   * handle formatted output we need find ERROR: in the middle of the output
   * stream. The error report starts with the ERROR: text instead of the whole
   * line.
   */
  private String[] parseError() throws IOException {
    //  Open the file as a stream
    InputStream fileStream =
      new FileInputStream(
        workingDirectory.getAbsolutePath()
          + "/"
          + parseBaseName(name, ".com")
          + ".log");

    BufferedReader fileReader =
      new BufferedReader(new InputStreamReader(fileStream));

    ArrayList errors = new ArrayList();
    String line;
    boolean foundError = false;
    while ((line = fileReader.readLine()) != null) {
      if (!foundError) {
        int index = line.indexOf("ERROR:");
        if (index != -1) {
          foundError = true;
          errors.add(line);
        }
      }
      else {
        errors.add(line);
      }
    }
    fileReader.close();
    return (String[]) errors.toArray(new String[errors.size()]);
  }

  /**
   * Parse the log file for warnings. Since the fortran code is no smart enough
   * handle formatted output we need find WARNING: in the middle of the output
   * stream. The error report starts with the WARNING: text instead of the
   * whole line.
   * 
   * @return A String[] containing any errors. If the com script has not been
   *         run then null is returned. If the com script ran with no warnings
   *         then zero length array will be returned.
   */
  private String[] parseWarning() throws IOException {
    boolean nextLineIsWarning = false;
    //  Open the file as a stream
    InputStream fileStream =
      new FileInputStream(
        workingDirectory.getAbsolutePath()
          + "/"
          + parseBaseName(name, ".com")
          + ".log");

    BufferedReader fileBuffer =
      new BufferedReader(new InputStreamReader(fileStream));

    ArrayList errors = new ArrayList();
    String line;
    while ((line = fileBuffer.readLine()) != null) {
      int index = line.indexOf("WARNING:");
      if (index != -1) {
        nextLineIsWarning = false;
        int trimIndex = line.trim().indexOf("PIP WARNING:");
        if (trimIndex != -1 && line.trim().length() <= trimIndex + 1 + "PIP WARNING:".length()) {
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
    return (String[]) errors.toArray(new String[errors.size()]);
  }

  /**
   * Returns true if the com script process is running, this does not include
   * vmstocsh process.
   */
  public boolean isStarted() {
    return started;
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
}
