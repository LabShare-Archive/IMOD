/*
 * Created on May 26, 2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.type.AxisID;

/**
 * @author rickg
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class AlignLogGenerator {

  String IMODPath;
  AxisID axisID;
  String alignLogCommand;

  public AlignLogGenerator(String path, AxisID id) {
    IMODPath = path;
    axisID = id;
    String IMODBinPath = IMODPath + File.separator + "bin" + File.separator;
    alignLogCommand = "tcsh -ef " + IMODBinPath + "alignlog ";
  }

  public void run() throws IOException {
    runArgument("-a", "alignAngles");
    runArgument("-c", "alignCoordinates");
    runArgument("-e", "alignError");
    runArgument("-m", "alignMappings");
    runArgument("-r", "alignResiduals");
    runArgument("-s", "alignSolution");
  }

  private void runArgument(String argument, String logFile)
    throws IOException {
    SystemProgram alignlog =
      new SystemProgram(
        alignLogCommand + argument + " " + axisID.getExtension());

    alignlog.run();

    String[] stdOutput = alignlog.getStdOutput();
    String[] stdError = alignlog.getStdError();

    BufferedWriter fileBuffer;
    fileBuffer =
      new BufferedWriter(
        new FileWriter(
          System.getProperty("user.dir")
            + File.separator
            + logFile
            + axisID.getExtension()
            + ".log"));

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
