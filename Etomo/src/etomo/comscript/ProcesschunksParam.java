package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;

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
public final class ProcesschunksParam implements ParallelParam {
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = "processchunks";
  public static final int NICE_DEFAULT = 15;
  public static final int NICE_FLOOR = 0;
  public static final int NICE_CEILING = 19;
  public static final int DROP_VALUE = 5;
  
  private final EtomoBoolean2 resume = new EtomoBoolean2();
  private final EtomoNumber nice = new EtomoNumber();
  private final ArrayList machineNames = new ArrayList();
  
  private final AxisID axisID;
  
  private String[] commandArray = null;
  private String rootName = null;
  private StringBuffer machineList = null;
  
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
    command.add(String.valueOf(DROP_VALUE));
    command.add("-c");
    command.add(DatasetFiles.getCommandsFileName(rootName));
    command.add("-P");
    //add machine names
    buildMachineList();
    if (machineList != null) {
      command.add(machineList.toString());
    }
    
    command.add(rootName);
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
  
  private final void buildMachineList() {
    int size = machineNames.size();
    if (size > 0) {
      machineList = new StringBuffer((String) machineNames.get(0));
      for (int i = 1; i < size; i++) {
        machineList.append(',');
        machineList.append(machineNames.get(i));
      }
    }
  }
  
  /**
   * Set resume.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   * @param resume
   */
  public final void setResume(boolean resume) {
    if (this.resume.equals(resume)) {
      return;
    }
    commandArray = null;
    this.resume.set(resume);
  }
  
  /**
   * Set nice.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   * @param nice
   */
  public final void setNice(Number nice) {
    if (this.nice.equals(nice)) {
      return;
    }
    commandArray = null;
    this.nice.set(nice);
  }
  
  public final void setRootName(String rootName) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    this.rootName = rootName + axisID.getExtension();
  }
  
  public final String getRootName() {
    return rootName;
  }
  
  public final ConstEtomoNumber getResume() {
    return resume;
  }
  
  public final String getMachineList() {
    if (machineList == null) {
      buildMachineList();
    }
    if (machineList != null) {
      return machineList.toString();
    }
    return "";
  }
  
  /**
   * Clears macinesNames.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   */
  public final void resetMachineName() {
    if (machineNames.size() == 0) {
      return;
    }
    commandArray = null;
    machineNames.clear();
  }
  
  public final void addMachineName(String machineName) {
    if (commandArray != null) {
      throw new IllegalStateException("can't change parameter values after command is built");
    }
    machineNames.add(machineName);
  }
}
/**
* <p> $Log$
* <p> Revision 1.7  2005/09/16 20:52:37  sueh
* <p> bug# 532 Added getResume().
* <p>
* <p> Revision 1.6  2005/09/16 17:19:29  sueh
* <p> bug# 532 Implementing ParallelParam.
* <p>
* <p> Revision 1.5  2005/09/07 20:31:15  sueh
* <p> bug# 532 Adding commands file option.
* <p>
* <p> Revision 1.4  2005/09/01 17:46:56  sueh
* <p> bug# 532 Make the drop value available publicly.  Change
* <p> getComputerList() to getMachineList and fix it.
* <p>
* <p> Revision 1.3  2005/08/30 18:29:09  sueh
* <p> bug# 532 Added getComputerList() so ProcesschunksProcessMonitor
* <p> can figure out which computers it is monitoring.
* <p>
* <p> Revision 1.2  2005/08/22 16:05:56  sueh
* <p> bug# 532 Added the extension to the rootName and getRootName().
* <p>
* <p> Revision 1.1  2005/08/01 17:58:22  sueh
* <p> Class to create a processchunks command line.
* <p> </p>
*/
