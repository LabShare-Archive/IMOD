package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.FileType;
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
  public static final String rcsid = "$Id$";

  public static final String COMMAND_NAME = "extractpieces";

  private final AxisID axisID;
  private final BaseManager manager;
  private final String rawStackFileName;// use when the manager setup is incomplete
  private final String rootName;// use when the manager setu is incomplete
  private final AxisType axisType;// use when the manager setup is incomplete

  private String[] commandArray = null;

  public ExtractpiecesParam(final BaseManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    rawStackFileName = null;
    rootName = null;
    axisType = null;
  }

  /**
   * Use this constructor when the manager setup is incomplete.
   * @param manager
   * @param rawStackFileName
   * @param axisType
   * @param axisID
   */
  public ExtractpiecesParam(final String rawStackFileName, final String rootName,
      final AxisType axisType, final BaseManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    this.rawStackFileName = rawStackFileName;
    this.rootName = rootName;
    this.axisType = axisType;
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
    if (rawStackFileName == null) {
      command.add(DatasetFiles.getStackName(manager, axisID));
      command.add(DatasetFiles.getPieceListFileName(manager, axisID));
    }
    else {
      command.add(rawStackFileName);
      command
          .add(FileType.PIECE_LIST.deriveFileName(rootName, axisType, manager, axisID));
    }

    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }

}
/**
* <p> $Log$
* <p> Revision 1.2  2006/03/20 17:51:21  sueh
* <p> bug# 835 Added getName (a convenience function) to managers.
* <p>
* <p> Revision 1.1  2005/10/27 00:12:41  sueh
* <p> bug# 725 Param to create a extractpieces command line.
* <p> </p>
*/
