package etomo.process;

import java.io.*;
import java.util.ArrayList;

/*
 * <p>Description: Provides a threadable class to execute IMOD com scripts.</p>
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
 * <p> Revision 1.8  2003/08/20 22:02:21  rickg
 * <p> Added log message when unable to rename log file
 * <p>
 * <p> Revision 1.7  2003/08/05 21:18:18  rickg
 * <p> Moved log file renaming to the beginning of the process
 * <p>
 * <p> Revision 1.6  2003/06/05 20:48:58  rickg
 * <p> Added multiline error messages
 * <p>
 * <p> Revision 1.5  2003/06/05 04:22:36  rickg
 * <p> method name change to getShellProcessID
 * <p>
 * <p> Revision 1.4  2003/06/04 23:48:33  rickg
 * <p> Rename log file if it exists
 * <p>
 * <p> Revision 1.3  2003/05/27 08:43:16  rickg
 * <p> Set started flag in com script execution
 * <p>
 * <p> Revision 1.2  2003/05/23 14:27:24  rickg
 * <p> Implements SystemProcessInterface
 * <p>
 * <p> Revision 1.1  2003/05/23 02:34:56  rickg
 * <p> Change RunComScript to ComScriptProcess
 * <p>
 * <p> Revision 2.7  2003/05/21 22:55:14  rickg
 * <p> Moved ParsePID to its own class so that BackgroundProcess
 * <p> can also use it.
 * <p>
 * <p> Revision 2.6  2003/05/21 21:20:42  rickg
 * <p> Parse the csh process ID from stderr
 * <p>
 * <p> Revision 2.5  2003/05/13 20:00:12  rickg
 * <p> Added -f option to tcsh call
 * <p>
 * <p> Revision 2.4  2003/05/12 23:28:13  rickg
 * <p> removed -f from tcsh executation (allows for user modifications to be
 * <p> used within etomo execution)
 * <p>
 * <p> Revision 2.3  2003/05/12 01:21:33  rickg
 * <p> Added explici tcsh call to copytomocoms
 * <p>
 * <p> Revision 2.2  2003/05/08 23:19:03  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.1  2003/01/29 20:44:48  rickg
 * <p> Debug messages to stderr instead of stdout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.8.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.8  2002/12/31 23:14:25  rickg
 * <p> Removed unnecessary string IMODDIR
 * <p>
 * <p> Revision 1.7  2002/12/30 20:31:59  rickg
 * <p> Added interface to get process output
 * <p> Program execution correctly calls done message
 * <p> of process manager
 * <p>
 * <p> Revision 1.6  2002/10/22 21:38:40  rickg
 * <p> ApplicationManager now controls both demo and debug
 * <p> modes
 * <p>
 * <p> Revision 1.5  2002/10/14 19:03:59  rickg
 * <p> vmstocsh and csh -ef are executed directly and separately now.
 * <p>
 * <p> Revision 1.4  2002/10/11 23:32:30  rickg
 * <p> Implementing individual execution of vmstocsh and csh
 * <p>
 * <p> Revision 1.3  2002/10/10 18:53:29  rickg
 * <p> Enabled SystemProgram debugging and remove local
 * <p> writing to stdout.
 * <p>
 * <p> Revision 1.2  2002/10/03 01:47:53  rickg
 * <p> Reformat after emacs whitespace trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
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

  private boolean started = false;
  private boolean done = false;

  /**
   * @return
   */
  public int getDemoTime() {
    return demoTime;
  }

  public ComScriptProcess(String comScript, ProcessManager processManager) {
    this.name = comScript;
    this.processManager = processManager;
    cshProcessID = new StringBuffer("");
  }

  /**
   * Set the working directory in which the com script is to be run.
   */
  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Execute the specified com script.  This can be initiated by the start()
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
      // Rename the existing log file
      String logFileName = parseBaseName(name, ".com") + ".log";
      File logFile = new File(workingDirectory, logFileName);
      if (logFile.exists()) {
        File oldLog = new File(workingDirectory, logFileName + "~");
        if (!logFile.renameTo(oldLog)) {
          System.err.println(
            "Unable to rename log file: "
              + workingDirectory
              + logFileName
              + "~");
          }
      }

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
   * Get the name of the com script
   */
  public String getScriptName() {
    return name;
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
   * @return A string array containing the csh command sequence
   */
  private String[] vmsToCsh() throws IOException, SystemProcessException {
    // Redirecting stdin for the command does not work even when a shell is
    // called, need to pump the com file directly into the stdin of vmstocsh
    String[] comSequence = loadFile();

    vmstocsh =
      new SystemProgram("vmstocsh " + parseBaseName(name, ".com") + ".log");
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
   * Load the comscript into a String[]
   * @return a String[] with each element containing a line of the com script
   */
  private String[] loadFile() throws IOException {

    //  Open the file as a stream
    InputStream fileStream =
      new FileInputStream(workingDirectory.getAbsolutePath() + "/" + name);

    BufferedReader fileBuffer =
      new BufferedReader(new InputStreamReader(fileStream));

    ArrayList lines = new ArrayList();
    String line;
    while ((line = fileBuffer.readLine()) != null) {
      lines.add(line);
    }
    return (String[]) lines.toArray(new String[lines.size()]);
  }
  /**
   * Returns the demoMode.
   * @return boolean
   */
  public boolean isDemoMode() {
    return demoMode;
  }

  /**
   * Sets the demoMode.
   * @param demoMode The demoMode to set
   */
  public void setDemoMode(boolean demoMode) {
    this.demoMode = demoMode;
  }

  /**
   * Parse the log file for errors.  Since the fortran code is not smart enough
   * handle formatted output we need find ERROR: in the middle of the output
   * stream.  The error report starts with the ERROR: text instead of the whole
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

    BufferedReader fileBuffer =
      new BufferedReader(new InputStreamReader(fileStream));

    ArrayList errors = new ArrayList();
    String line;
    boolean foundError = false;
    while ((line = fileBuffer.readLine()) != null) {
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
    return (String[]) errors.toArray(new String[errors.size()]);
  }

  /**
   * Parse the log file for warnings.  Since the fortran code is no smart enough
   * handle formatted output we need find WARNING: in the middle of the output
   * stream.  The error report starts with the WARNING: text instead of the
   * whole line.
   * 
   * @return A String[] containing any errors.  If the com script has not been
   * run then null is returned.  If the com script ran with no warnings then
   * zero length array will be returned.
   */
  private String[] parseWarning() throws IOException {
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
        errors.add(line.substring(index));
      }
    }
    return (String[]) errors.toArray(new String[errors.size()]);
  }

  public boolean isStarted() {
    return started;
  }

  public boolean isDone() {
    return done;
  }
}
