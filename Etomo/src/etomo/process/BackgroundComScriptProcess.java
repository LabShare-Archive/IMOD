package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

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
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$$ </p>
 */
public class BackgroundComScriptProcess extends ComScriptProcess {
  public static final String rcsid = "$$Id$$";
  /**
   * @param comScript
   * @param processManager
   * @param axisID
   * @param watchedFileName
   */
  public BackgroundComScriptProcess(
    String comScript,
    ProcessManager processManager,
    AxisID axisID,
    String watchedFileName) {
    super(comScript, processManager, axisID, watchedFileName);
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
    
    String outFileName = runName + ".out";
    File outFile = new File(workingDirectory, outFileName);
    
    Utilities.writeFile(cshFile, commands, true);
    Utilities.renameFile(outFile, new File(outFile.getAbsolutePath() + "~"));
    makeRunCshFile(runCshFile, cshFileName, outFileName);
    
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06 

    csh = new SystemProgram("tcsh -f " + runCshFile.getAbsolutePath());
    /*
    String runCommand =
      "tcsh -fc 'tcsh -fe "
        + cshFile.getAbsolutePath()
        + " >&"
        + outFile.getName()
        + "'";
    System.out.println("runCommand=" + runCommand);
    csh = new SystemProgram(runCommand);*/
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
    bufferedWriter.write("tcsh -ef " + cshFileName + ">&" + outFileName + "&");
    bufferedWriter.newLine();
    bufferedWriter.close();
  }
}
