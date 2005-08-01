package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;

/**
* <p>Description: Command line for processchunks.  Assumes that it will be run
* once per instance (no reset function).</p>
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
public final class ProcesschunksParam {
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = "processchunks";
  public static final int NICE_DEFAULT = 15;
  public static final int NICE_FLOOR = 0;
  public static final int NICE_CEILING = 19;
  
  private String[] commandArray = null;
  private final AxisID axisID;
  private EtomoBoolean2 resume = new EtomoBoolean2();
  private EtomoNumber nice = new EtomoNumber();
  private ArrayList machineNames = new ArrayList();
  private String rootName;
  
  public ProcesschunksParam(AxisID axisID) {
    this.axisID = axisID;
    nice.set(NICE_DEFAULT);
    nice.setFloor(NICE_FLOOR);
    nice.setCeiling(NICE_CEILING);
  }
  
  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }
  
  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add("tcsh");
    command.add("-f");
    command.add(BaseManager.getIMODBinPath() + COMMAND_NAME);
    if (resume.is()) {
      command.add("-r");
    }
    command.add("-g");
    command.add("-n");
    command.add(nice.toString());
    command.add("-d");
    command.add(String.valueOf(5));
    command.add("-P");
    //add machine names
    int size = machineNames.size();
    if (size > 0) {
      StringBuffer list = new StringBuffer((String) machineNames.get(0));
      for (int i = 1; i < size; i++) {
        list.append(',');
        list.append(machineNames.get(i));
      }
      command.add(list.toString());
    }
    
    command.add(rootName + axisID.getExtension());
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
  
  public final void setResume(boolean resume) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    this.resume.set(resume);
  }
  
  public final void setNice(Number nice) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    this.nice.set(nice);
  }
  
  public final void setRootName(String rootName) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    this.rootName = rootName;
  }
  
  public final void addMachineName(String machineName) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    machineNames.add(machineName);
  }
}
/**
* <p> $Log$ </p>
*/
