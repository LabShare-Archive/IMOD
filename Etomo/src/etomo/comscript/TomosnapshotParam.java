package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 */
public final class TomosnapshotParam implements Command {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.TOMOSNAPSHOT;
  public static final String OUTPUT_LINE = "Snapshot done";
  private static final String COMMAND_NAME = PROCESS_NAME
      .toString();

  private final AxisID axisID;

  private String[] commandArray = null;
  private boolean debug = true;

  public TomosnapshotParam(AxisID axisID) {
    this.axisID = axisID;
  }

  public final String getCommand() {
    return COMMAND_NAME;
  }

  public CommandMode getCommandMode() {
    return null;
  }
  public boolean isMessageReporter() {
    return false;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add("tcsh");
    command.add("-f");
    command.add(BaseManager.getIMODBinPath() + COMMAND_NAME);
    //    command.add("-e");
    //    command.add(manager.getBaseMetaData().getMetaDataFileName());
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
    if (debug) {
      System.err.println("Running tomosnapshot in "
          + System.getProperty("user.dir"));
      for (int i = 0; i < commandArray.length; i++) {
        System.err.print(commandArray[i] + " ");
      }
      System.err.println();
    }
  }

  public final String getCommandName() {
    return COMMAND_NAME;
  }
  
  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public final String getCommandLine() {
    getCommandArray();
    if (commandArray == null) {
      return null;
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + ' ');
    }
    return buffer.toString();
  }

  public String[] getCommandArray() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }
  
  public ProcessName getSubcommandProcessName() {
    return null;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.10  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.9  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.8  2009/04/02 19:14:51  sueh
 * <p> bug# 1206 Removed the manager member variable.  Calling tomosnapshot without
 * <p> options or parameters.
 * <p>
 * <p> Revision 1.7  2007/11/06 19:17:32  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.6  2007/02/05 22:48:08  sueh
 * <p> bug# 962 Changed getCommandMode to return CommandMode.
 * <p>
 * <p> Revision 1.5  2006/05/22 22:41:07  sueh
 * <p> bug# 577 Moved the call to buildCommand to getCommandArray().  Made
 * <p> getCommand() conform to the Command interface.
 * <p>
 * <p> Revision 1.4  2006/05/11 19:50:20  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.3  2006/02/06 21:03:24  sueh
 * <p> bug# 776 Call script with tcsh -f, so it can be run on WIndows.
 * <p>
 * <p> Revision 1.2  2006/01/20 20:48:13  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.1  2005/12/09 20:25:02  sueh
 * <p> bug# 776 A param for the tomosnapshot command
 * <p> </p>
 */
