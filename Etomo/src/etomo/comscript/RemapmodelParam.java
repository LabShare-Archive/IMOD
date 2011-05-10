package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstJoinState;
import etomo.type.FileType;
import etomo.type.IntKeyList;
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
 * <p> Revision 1.7  2010/04/28 16:05:50  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.6  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.5  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.4  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.3  2007/11/06 19:16:14  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.2  2007/03/01 01:13:07  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.1  2007/02/05 22:42:41  sueh
 * <p> bug# 962 Remapmodel parameter object.
 * <p> </p>
 */
public final class RemapmodelParam implements Command {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.REMAPMODEL;
  public static final String COMMAND_NAME = PROCESS_NAME.toString();

  private static final int COMMAND_SIZE = 1;
  private static final boolean debug = true;
  private final String[] commandArray;
  private final JoinManager manager;

  public RemapmodelParam(JoinManager manager) {
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
    ConstJoinState state = manager.getState();
    boolean trial = state.getRefineTrial().is();
    options.add("-FromChunkLimits");
    options.add(buildStartEndString(state.getJoinStartListWalker(trial), state
        .getJoinEndListWalker(trial)));
    options.add("-ToChunkLimits");
    options.add(buildStartEndString(state.getRefineStartListWalker(), state
        .getRefineEndListWalker()));
    String outputFile = state.getXfModelOutputFile();
    if (outputFile == null) {
      options.add(DatasetFiles.getRefineAlignedModelFileName(manager));
      options.add(DatasetFiles.getRefineAlignedModelFileName(manager));
    }
    else {
      options.add(outputFile);
      options.add(outputFile);
    }
    return options;
  }

  private String buildStartEndString(IntKeyList.Walker startListWalker,
      IntKeyList.Walker endListWalker) {
    if (startListWalker.size() != endListWalker.size()) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    while (startListWalker.hasNext()) {
      buffer.append(startListWalker.nextEtomoNumber() + ","
          + endListWalker.nextEtomoNumber());
      if (startListWalker.hasNext()) {
        buffer.append(',');
      }
    }
    return buffer.toString();
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

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandOutputFile() {
    return DatasetFiles.getRefineAlignedModelFile(manager);
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
}
