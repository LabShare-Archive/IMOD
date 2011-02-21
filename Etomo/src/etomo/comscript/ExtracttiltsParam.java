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
public class ExtracttiltsParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND_NAME = "extracttilts";

  private final AxisID axisID;
  private final BaseManager manager;

  private String[] commandArray = null;

  public ExtracttiltsParam(BaseManager manager, AxisID axisID) {
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
    command.add(DatasetFiles.getRawTiltName(manager, axisID));
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/03/20 17:51:30  sueh
* <p> bug# 835 Added getName (a convenience function) to managers.
* <p>
* <p> Revision 1.1  2005/10/27 00:12:56  sueh
* <p> bug# 725 Param to create a extracttilts command line.
* <p> </p>
*/
