package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstJoinState;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.5  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.4  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.3  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2007/11/06 19:18:38  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.1  2007/02/05 22:51:16  sueh
 * <p> bug# 962 Xftoxg parameter object.
 * <p> </p>
 */
public final class XftoxgParam implements Command {
  public static final String rcsid = "$Id$";

  public static final ProcessName PROCESS_NAME = ProcessName.XFTOXG;
  public static final String COMMAND_NAME = PROCESS_NAME.toString();

  private static final boolean debug = true;
  private static final int COMMAND_SIZE = 1;
  private final String[] commandArray;
  private final JoinManager manager;

  public XftoxgParam(JoinManager manager) {
    this.manager = manager;
    ArrayList options = genOptions();
    commandArray = new String[options.size() + COMMAND_SIZE];
    commandArray[0] = BaseManager.getIMODBinPath() + COMMAND_NAME;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + COMMAND_SIZE] = (String) options.get(i);
    }
    if (debug) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < commandArray.length; i++) {
        buffer.append(commandArray[i]);
        if (i < commandArray.length - 1) {
          buffer.append(' ');
        }
      }
      System.err.println(buffer.toString());
    }
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-NumberToFit");
    options.add("0");
    ConstJoinState state = manager.getState();
    boolean trial = state.getRefineTrial().is();
    if (!state.getJoinAlignmentRefSection(trial).isNull()) {
      options.add("-ReferenceSection");
      options.add(state.getJoinAlignmentRefSection(trial).toString());
    }
    options.add(DatasetFiles.getRefineXfFileName(manager));
    options.add(DatasetFiles.getRefineXgFileName(manager));
    return options;
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    return COMMAND_NAME;
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public String getCommandLine() {
    if (commandArray.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(commandArray[0]);
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(' ' + commandArray[i]);
    }
    return buffer.toString();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandOutputFile() {
    return DatasetFiles.getRefineXgFile(manager);
  }

  public FileType getOutputImageFileType() {
    return FileType.TRANSFORMED_REFINING_MODEL;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }
}
