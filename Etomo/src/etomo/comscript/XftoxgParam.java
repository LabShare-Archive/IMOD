package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstJoinState;
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
* <p> $Log$ </p>
*/
public final class XftoxgParam implements Command{
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = ProcessName.XFTOXG.toString();
  
  private static final boolean debug = true;
  private static final int COMMAND_SIZE = 1;
  private final String[] commandArray;
  private final JoinManager manager;
  
  public XftoxgParam(JoinManager manager){
    this.manager=manager;
    ArrayList options =genOptions();
    commandArray = new String[options.size() + COMMAND_SIZE];
    commandArray[0] = BaseManager.getIMODBinPath() + COMMAND_NAME;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + COMMAND_SIZE] = (String) options.get(i);
    }
    if (debug) {
      StringBuffer buffer = new StringBuffer();
      for (int i=0;i< commandArray.length;i++) {
        buffer.append(commandArray[i]);
        if (i<commandArray.length-1) {
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
    if (commandArray.length==0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(commandArray[0]);
    for (int i=0;i<commandArray.length;i++) {
      buffer.append(' '+commandArray[i]);
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
    return DatasetFiles.getRefineXgFile(manager);
  }
}
