package etomo.comscript;

import etomo.SerialSectionsManager;
import etomo.type.AxisID;
import etomo.type.FileType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsComScriptManager extends BaseComScriptManager {
  public static final String rcsid = "$Id:$";

  private final SerialSectionsManager manager;

  private ComScript scriptPreblend = null;
  private ComScript scriptBlend = null;
  private ComScript scriptNewst = null;

  public SerialSectionsComScriptManager(final SerialSectionsManager manager) {
    super(manager);
    this.manager = manager;
  }

  public void loadPreblend(final AxisID axisID) {
    scriptPreblend = loadComScript(FileType.PREBLEND_COMSCRIPT, axisID, true, false,
        false);
  }

  public BlendmontParam getBlendmontParamFromPreblend(final AxisID axisID,
      final String rootName) {
    BlendmontParam param = new BlendmontParam(manager, rootName, axisID,
        BlendmontParam.Mode.SERIAL_SECTION_PREBLEND);
    initialize(param, scriptPreblend, BlendmontParam.COMMAND_NAME, axisID, false, false);
    return param;
  }

  public void savePreblend(final BlendmontParam param, final AxisID axisID) {
    modifyCommand(scriptPreblend, param, BlendmontParam.COMMAND_NAME, axisID, true, false);
  }

  public void loadBlend(final AxisID axisID) {
    scriptBlend = loadComScript(FileType.BLEND_COMSCRIPT, axisID, true, false, false);
  }

  public BlendmontParam getBlendmontParamFromBlend(final AxisID axisID,
      final String rootName) {
    BlendmontParam param = new BlendmontParam(manager, rootName, axisID,
        BlendmontParam.Mode.SERIAL_SECTION_BLEND);
    initialize(param, scriptBlend, BlendmontParam.COMMAND_NAME, axisID, false, false);
    return param;
  }

  public void saveBlend(final BlendmontParam param, final AxisID axisID) {
    modifyCommand(scriptBlend, param, BlendmontParam.COMMAND_NAME, axisID, true, false);
  }

  public void loadNewst(final AxisID axisID) {
    scriptNewst = loadComScript(FileType.NEWST_COMSCRIPT, axisID, true, false, false);
  }

  public NewstParam getNewstackParam(final AxisID axisID, final String rootName) {
    NewstParam param = NewstParam.getColorInstance(manager, axisID);
    initialize(param, scriptNewst, param.getCommandName(), axisID, false, false);
    return param;
  }

  public void saveNewst(final NewstParam param, final AxisID axisID) {
    modifyCommand(scriptNewst, param, param.getCommandName(), axisID, true, false);
  }
}
