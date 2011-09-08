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
    command.add("python");
    command.add("-u");
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
* <p> Revision 1.3  2011/02/24 19:49:14  sueh
* <p> bug# 1451 Splitcombine is a python script now; calling with "python -u".
* <p>
* <p> Revision 1.2  2005/10/28 19:34:03  sueh
* <p> bug# 746 Removed temp directory.
* <p>
* <p> Revision 1.1  2005/09/16 17:48:46  sueh
* <p> bug# 532 A param for splitcombine.
* <p> </p>
*/