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
public class RunComScript extends Thread {
  public static final String rcsid =
    "$Id$";

  private String comScript = null;
  private File workingDirectory = null;
  private ProcessManager processManager;
  private boolean demoMode = false;
  private boolean enableDebug = false;
  private String message = "";
  

  public RunComScript(String comScript, ProcessManager processManager) {
    this.comScript = comScript;
    this.processManager = processManager;
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

    String IMODDIR = processManager.getIMODDirectory();

    //  Start a progress bar to keep the user informed
    ProcessProgressDialog progress =
      new ProcessProgressDialog(parseBaseName(comScript, ".com"), this);
    progress.stepTimeMS = 200;
    progress.progressBar.setIndeterminate(true);
    Thread progressBarThread = new Thread(progress);
    progressBarThread.start();


    int exitValue = 0;
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
      try {
        //  Covert the com script to a sequence of csh command
        String[] commands = vmsToCsh();
        execCsh(commands);
      }
      catch (Exception excep) {
        excep.printStackTrace();
        message = excep.getMessage();        
      }

    }

    //
    // Stop the progress bar and send a message back to the ProcessManager
    // that this thread is done.
    //
    progressBarThread.interrupt();
    processManager.msgComScriptDone(this, exitValue, comScript);
  }


  /**
   * Get the error message, if any from the run method.
   */
  public String getMessage() {
    return message;
  }
  
  /**
   * Get the debug state.
   */
  public boolean getEnableDebug() {
    return enableDebug;
  }
  
  /**
   * Set the debug state.
   */
  public void setEnableDebug(boolean state) {
    enableDebug = state;
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
  private void execCsh(String[] commands) throws SystemProcessException {
    SystemProgram csh = new SystemProgram("csh -ef");
    csh.setWorkingDirectory(workingDirectory);
    csh.setStdInput(commands);
    csh.enableDebug(enableDebug);
    csh.run();

    if (csh.getExitValue() != 0) {
      String[] stderr = csh.getStdError();
      String message = "";
      for (int i = 0; i < stderr.length; i++) {
        message += stderr[i] + "\n";
      }
      throw new SystemProcessException(message);
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


    SystemProgram vmstocsh = new SystemProgram("vmstocsh "  + 
      parseBaseName(comScript, ".com") + ".log");
    vmstocsh.setWorkingDirectory(workingDirectory);
    vmstocsh.setStdInput(comSequence);
    vmstocsh.enableDebug(enableDebug);
    vmstocsh.run();

    if (vmstocsh.getExitValue() != 0) {
      String[] stderr = vmstocsh.getStdError();
      String message = "";
      for (int i = 0; i < stderr.length; i++) {
        message += stderr[i] + "\n";
      }
      throw new SystemProcessException(message);
    }

    return vmstocsh.getStdOutput();
  }

  /**
   * Load the comscript into a String[]
   * @return a String[] with each element containing a line of the com script
   */
  private String[] loadFile() throws IOException {

    //  Open the file as a 
    InputStream fileStream =
      new FileInputStream(workingDirectory.getAbsolutePath() + 
        "/" + comScript);

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

}
