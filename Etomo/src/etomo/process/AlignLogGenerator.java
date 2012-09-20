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
 * <p> Revision 3.13  2011/07/19 20:25:32  sueh
 * <p> Bug# 1492 In AlignLogGenerator, switched call to run with python.
 * <p>
 * <p> Revision 3.12  2011/02/22 03:58:04  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.11  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.10  2009/09/01 03:17:56  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.9  2009/03/17 00:33:49  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.8  2009/02/04 23:15:50  sueh
 * <p> bug# 1158 Sharing the name of the error log.
 * <p>
 * <p> Revision 3.7  2007/11/14 23:46:20  sueh
 * <p> bug# 1048 Added beam tilt log.
 * <p>
 * <p> Revision 3.6  2006/06/27 23:09:46  sueh
 * <p> bug# 886 Fixed problem:  Was placing an empty string in alignLogCommand
 * <p> when the axisID is ONLY.  This caused alignlog to fail.
 * <p>
 * <p> Revision 3.5  2006/05/22 22:43:44  sueh
 * <p> bug# 577 Placed the command in a String[] rather then a String.
 * <p>
 * <p> Revision 3.4  2005/07/29 00:50:40  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p> 
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
  public static final String ERROR_LOG_NAME = "taError";
  public static final String ANGLES_LOG_NAME = "taAngles";
  public static final String ROBUST_LOG_NAME = "taRobust";

  AxisID axisID;
  String[] alignLogCommand = null;
  private final BaseManager manager;

  public AlignLogGenerator(BaseManager manager, AxisID id) {
    this.manager = manager;
    axisID = id;
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    if (id == AxisID.ONLY) {
      alignLogCommand = new String[4];
    }
    else {
      alignLogCommand = new String[5];
    }
    alignLogCommand[0] = "python";
    alignLogCommand[1] = "-u";
    alignLogCommand[2] = ApplicationManager.getIMODBinPath() + "alignlog";
    if (id != AxisID.ONLY) {
      alignLogCommand[4] = axisID.getExtension();
    }
  }

  public void run() throws IOException {
    runArgument("-a", ANGLES_LOG_NAME);
    runArgument("-c", "taCoordinates");
    runArgument("-e", ERROR_LOG_NAME);
    runArgument("-l", "taLocals");
    runArgument("-m", "taMappings");
    runArgument("-r", "taResiduals");
    runArgument("-s", "taSolution");
    runArgument("-b", "taBeamtilt");
    runArgument("-w", "taRobust");
  }

  private void runArgument(String argument, String logFile) throws IOException {
    alignLogCommand[3] = argument;
    SystemProgram alignlog = new SystemProgram(manager, manager.getPropertyUserDir(),
        alignLogCommand, axisID);

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