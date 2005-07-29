/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ 
 * </p>
 */

package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.type.AxisID;

public class AlignLogGenerator {

  AxisID axisID;
  String alignLogCommand;
  private final BaseManager manager;

  public AlignLogGenerator(BaseManager manager, AxisID id) {
    this.manager = manager;
    axisID = id;
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    alignLogCommand = "tcsh -f " + ApplicationManager.getIMODBinPath()
        + "alignlog ";
  }

  public void run() throws IOException {
    runArgument("-a", "taAngles");
    runArgument("-c", "taCoordinates");
    runArgument("-e", "taError");
    runArgument("-l", "taLocals");
    runArgument("-m", "taMappings");
    runArgument("-r", "taResiduals");
    runArgument("-s", "taSolution");
  }

  private void runArgument(String argument, String logFile) throws IOException {
    SystemProgram alignlog = new SystemProgram(manager.getPropertyUserDir(),
        alignLogCommand + argument + " " + axisID.getExtension(), axisID);

    alignlog.run();

    String[] stdOutput = alignlog.getStdOutput();
    String[] stdError = alignlog.getStdError();

    BufferedWriter fileBuffer;
    fileBuffer = new BufferedWriter(new FileWriter(manager.getPropertyUserDir()
        + File.separator + logFile + axisID.getExtension() + ".log"));

    if (stdOutput == null) {
      if (stdError == null) {
        fileBuffer.write("alignlog produced no output");
      }
      else {
        for (int i = 0; i < stdError.length; i++) {
          fileBuffer.write(stdError[i]);
          fileBuffer.newLine();
        }
      }
    }
    else {
      for (int i = 0; i < stdOutput.length; i++) {
        fileBuffer.write(stdOutput[i]);
        fileBuffer.newLine();
      }
    }
    fileBuffer.close();
  }
}