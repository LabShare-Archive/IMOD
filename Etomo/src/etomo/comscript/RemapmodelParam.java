package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstJoinState;
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
 * <p> Revision 1.1  2007/02/05 22:42:41  sueh
 * <p> bug# 962 Remapmodel parameter object.
 * <p> </p>
 */
public final class RemapmodelParam implements Command {
  public static final String rcsid = "$Id$";
  public static final String COMMAND_NAME = ProcessName.REMAPMODEL.toString();

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

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public File getCommandOutputFile() {
    return DatasetFiles.getRefineAlignedModelFile(manager);
  }
}
