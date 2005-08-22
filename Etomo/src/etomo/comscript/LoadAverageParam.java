package etomo.comscript;

import java.util.ArrayList;

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
public class LoadAverageParam implements IntermittentCommand {
  public static  final String  rcsid =  "$Id$";
  
  private final String computer;

  private String[] commandArray = null;
  private String intermittentCommand = null;
  
  public LoadAverageParam(String computer) {
    this.computer = computer;
  }
  
  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }
  
  public String getIntermittentCommand() {
    if (intermittentCommand == null) {
      buildIntermittentCommand();
    }
    return intermittentCommand;
  }
  
  public int getInterval() {
    return 5000;
  }
  
  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add("ssh");
    command.add(computer);
    
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
  
  private final void buildIntermittentCommand() {
    intermittentCommand = "w";
  }

  public final String getKey() {
    return computer;
  }
}
/**
* <p> $Log$ </p>
*/