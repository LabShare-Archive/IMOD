package etomo.comscript;

import etomo.ToolsManager;
import etomo.type.AxisID;
import etomo.type.FileType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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

public final class ToolsComScriptManager extends BaseComScriptManager {
  public static final String rcsid = "$Id$";

  private final ToolsManager manager;

  private ComScript scriptFlatten;

  public ToolsComScriptManager(ToolsManager manager) {
    super(manager);
    this.manager = manager;
  }

  public void loadFlatten(AxisID axisID) {
    scriptFlatten = loadComScript(FileType.FLATTEN_TOOL_COMSCRIPT, axisID,
        true, false, false);
  }

  public boolean isWarpVolParamInFlatten(AxisID axisID) {
    return loadComScript(FileType.FLATTEN_TOOL_COMSCRIPT, axisID, true, false,
        false).isCommandLoaded();
  }

  public WarpVolParam getWarpVolParamFromFlatten(AxisID axisID) {
    // Initialize a WarpVolParam object from the com script command
    // object
    WarpVolParam param = new WarpVolParam(manager, axisID);
    initialize(param, scriptFlatten, WarpVolParam.COMMAND, axisID, false, false);
    return param;
  }
  
  /**
   * Save the WarpVolParam command to the flatten com script
   * @param warpVolParam
   */
  public void saveFlatten(WarpVolParam param, AxisID axisID) {
    modifyCommand(scriptFlatten, param, WarpVolParam.COMMAND, axisID, true,
        false);
  }
}
