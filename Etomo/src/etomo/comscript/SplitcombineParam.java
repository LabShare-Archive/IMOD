package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;

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
public class SplitcombineParam {
  public static  final String  rcsid =  "$Id$";
  public static final String COMMAND_NAME = "splitcombine";
  
  private String[] commandArray = null;
  
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
    command.add("volcombine");
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/09/16 17:48:46  sueh
* <p> bug# 532 A param for splitcombine.
* <p> </p>
*/