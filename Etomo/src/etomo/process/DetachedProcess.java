package etomo.process;
/*
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.comscript.Command;
import etomo.type.AxisID;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;*/

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class DetachedProcess/* extends BackgroundProcess */{
  public static final String rcsid = "$Id$";
/*
  private File outFile = null;
  
  private final DetachedProcessMonitor monitor;
  private final AxisID axisID;
  private final BaseManager manager;

  public DetachedProcess(BaseManager manager, Command command,
      BaseProcessManager processManager, AxisID axisID,
      DetachedProcessMonitor monitor) {
    super(manager, command, processManager, axisID);
    this.monitor = monitor;
    this.axisID = axisID;
    this.manager = manager;
  }

  protected final boolean newProgram() {
    String[] runCommand;
    try {
      runCommand = makeRunFile();
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Can't Run "
          + getCommandName());
      return false;
    }
    setProgram(new SystemProgram(manager.getPropertyUserDir(), runCommand,
        getAxisID()));
    //program.setAcceptInputWhileRunning(true);
    return true;
  }
  
  private final String[] makeRunFile() throws IOException {
    String commandName = getCommandName();
    AxisID axisID = getAxisID();
    File runFile = DatasetFiles.getShellScript(manager, commandName, axisID);
    if (runFile.exists()) {
      runFile.delete();
    }
    outFile = DatasetFiles.getOutFile(manager, commandName, axisID);
    if (runFile.exists()) {
      runFile.delete();
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(runFile));
    if (bufferedWriter == null) {
      throw new IOException("unable to write to " + runFile.getAbsolutePath());
    }
    bufferedWriter.write("nohup");
    bufferedWriter.newLine();
    bufferedWriter.write(command.getCommandLine() + " >& " + outFile.getName()
        + "&");
    bufferedWriter.newLine();
    bufferedWriter.close();
    if (workingDirectory == null) {
      workingDirectory = new File(manager.getPropertyUserDir());
    }
    String[] runCommand = new String[3];
    runCommand[0] = "tcsh";
    runCommand[1] = "-f";
    runCommand[2] = runFile.getName();
    return runCommand;
  }
  
  final BufferedReader getOutputFileReader() {
    if (outFile == null || !outFile.exists()) {
      return null;
    }
    try {
      return new BufferedReader(new FileReader(outFile));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      return null;
    }
  }*/
}
/**
 * <p> $Log$ </p>
 */