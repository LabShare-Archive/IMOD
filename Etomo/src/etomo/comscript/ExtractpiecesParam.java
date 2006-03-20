package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.util.DatasetFiles;

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
public class ExtractpiecesParam {
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = "extractpieces";
  
  private final AxisID axisID;
  private final BaseManager manager;
  
  private String[] commandArray = null;
  
  public ExtractpiecesParam(BaseManager manager, AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
  }
  
  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }
  
  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add(BaseManager.getIMODBinPath() + COMMAND_NAME);
    String dataset = manager.getName();
    command.add(DatasetFiles.getStackName(manager, axisID));
    command.add(DatasetFiles.getPieceListFileName(manager, axisID));
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }

}
/**
* <p> $Log$
* <p> Revision 1.1  2005/10/27 00:12:41  sueh
* <p> bug# 725 Param to create a extractpieces command line.
* <p> </p>
*/