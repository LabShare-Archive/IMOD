package etomo.process;

import java.io.*;

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
 * <p> $Log$ </p>
 */
public class RunComScript extends Thread {
  public static final String rcsid = "$Id$";

  private String comBaseName = null;
  private String command = null;
  private File  workingDirectory = null;
  private ProcessManager processManager;
  private boolean isDemo = false;

  public RunComScript(String comScript, ProcessManager processManager) {
    comBaseName = parseBaseName(comScript, ".com");
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
    //
    // FIXME need to include the script below in the code or find a work around
    // for the exisisting scripts
    command = "bash " + IMODDIR + "/bin/vmstocsh.sh " + comBaseName + ".com "
      + comBaseName + ".log ";

    System.out.println(command);
    SystemProgram systemProgram = new SystemProgram(command);
    systemProgram.setWorkingDirectory(workingDirectory);

    ProcessProgressDialog progress =
      new ProcessProgressDialog(comBaseName, this);
    progress.stepTimeMS = 200;
    progress.progressBar.setIndeterminate(true);
    Thread progressBarThread = new Thread(progress);
    progressBarThread.start();

    int exitValue = 0;
    if(isDemo) {
      try {
	sleep(5000);
      }
      catch(InterruptedException except) {
	System.out.println("Sleep interrupted");
      }
    }
    else {
      try {
	systemProgram.run();
	exitValue = systemProgram.getExitValue();
      }
      catch(Exception excep) {
	excep.printStackTrace();
	System.exit(-1);
      }

      System.out.println("Exit value:" + String.valueOf(exitValue));

      System.out.println("Stdout:");
      System.out.println(
	"------------------------------------------------------------");
      String[] stdout = systemProgram.getStdOutput();
      for(int i=0; i < stdout.length; i++) {
	System.out.println(stdout[i]);
      }
      System.out.println("");
      System.out.println("Stderr:");
      System.out.println(
	"------------------------------------------------------------");
      String[] stderr = systemProgram.getStdError();
      for(int i=0; i < stderr.length; i++) {
	System.out.println(stderr[i]);
      }
      System.out.println("");
      System.out.println("");
    }
    progressBarThread.interrupt();

    //
    //  Send a message back to the ProcessManager that this thread is done.
    //
    processManager.msgComScriptDone(exitValue, command);
  }


  /**
   * Extract the basename from a filename given the filename and the expected
   * extension.
   */
  private String parseBaseName(String filename, String extension) {
    int idxExtension = filename.indexOf(extension);
    String base = null;
    if(idxExtension > 0)
      base = filename.substring(0, idxExtension);
    return base;
  }
}
