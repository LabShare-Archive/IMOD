package etomo.comscript;

import java.io.File;

import etomo.BaseManager;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class ArchiveorigParam implements Command {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.ARCHIVEORIG;
  public static final String COMMAND_NAME = PROCESS_NAME.toString();

  private String[] commandArray;
  private Mode mode = Mode.AXIS_ONLY;
  private File outputFile;
  private final BaseManager manager;

  public ArchiveorigParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    if (axisID == AxisID.FIRST) {
      mode = Mode.AXIS_A;
    }
    else if (axisID == AxisID.SECOND) {
      mode = Mode.AXIS_B;
    }
    File stack = Utilities.getFile(manager, false, axisID,
        DatasetTool.STANDARD_DATASET_EXT, "");
    commandArray = new String[] { "python", "-u",
        BaseManager.getIMODBinPath() + COMMAND_NAME, "-PID", stack.getName() };
    outputFile = Utilities.getFile(manager, false, axisID, "_xray.st.gz", "");
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public String getCommand() {
    return COMMAND_NAME;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getCommandLine() {
    if (commandArray == null) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public final static class Mode implements CommandMode {
    public static final Mode AXIS_A = new Mode("AxisA");
    public static final Mode AXIS_B = new Mode("AxisB");
    public static final Mode AXIS_ONLY = new Mode("AxisOnly");

    private final String string;

    private Mode(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.16  2011/05/10 16:48:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 1.15  2010/04/28 15:43:12  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.14  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.13  2009/12/11 17:25:18  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.12  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.11  2007/11/06 19:05:10  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.10  2007/02/05 21:31:03  sueh
 * <p> bug# 962 Put mode info into an inner class.
 * <p>
 * <p> Revision 1.9  2006/06/07 17:45:09  sueh
 * <p> bug# 766 Running archiveorig with tcsh for windows
 * <p>
 * <p> Revision 1.8  2006/05/22 22:34:45  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.7  2006/05/11 19:33:59  sueh
 * <p> bug# 838 Implement Command instead of CommandDetails
 * <p>
 * <p> Revision 1.6  2006/04/06 18:48:12  sueh
 * <p> bug# 808 Implementing ProcessDetails.
 * <p>
 * <p> Revision 1.5  2006/01/20 20:45:00  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.4  2005/11/19 01:45:53  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.3  2005/07/29 00:42:44  sueh
 * <p> bug# 709 Adding a EtomoDirector test harness so that unit test functions
 * <p> can use package level EtomoDirector functions getCurrentManager and
 * <p> setCurrentPropertyUserDir.  As long as the unit test doesn't open multiple
 * <p> windows and switch to another tab, it is OK for it to get the current
 * <p> manager from EtomoDirector.
 * <p>
 * <p> Revision 1.2  2005/07/26 17:08:27  sueh
 * <p> bug# 701 Get the PID from archiveorig
 * <p>
 * <p> Revision 1.1  2005/05/18 22:31:38  sueh
 * <p> bug# 662 A param object for archiveorig.
 * <p> </p>
 */
