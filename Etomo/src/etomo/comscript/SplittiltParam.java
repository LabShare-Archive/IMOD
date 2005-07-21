package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

/**
* <p>Description: </p>
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
public class SplittiltParam {
  public static  final String  rcsid =  "$Id$";

  public static final String COMMAND_NAME = "splittilt";
  
  private String[] commandArray = null;
  private EtomoNumber numMachines;
  private AxisID axisID;
  
  public SplittiltParam(AxisID axisID) {
    this.axisID = axisID;
    numMachines = new EtomoNumber();
    numMachines.setNullIsValid(false);
    numMachines.setValidFloor(1);
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
    command.add("-n");
    command.add(numMachines.toString());
    command.add("tilt" + axisID.getExtension());
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
  
  public ConstEtomoNumber setNumMachines(String numMachines) {
    return this.numMachines.set(numMachines);
  }
}
/**
* <p> $Log$ </p>
*/