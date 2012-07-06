package etomo.process;

import etomo.BaseManager;
import etomo.comscript.ExtractpiecesParam;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;

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
public final class SerialSectionsProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id:$";

  private final BaseManager manager;

  public SerialSectionsProcessManager(final BaseManager manager) {
    super(manager);
    this.manager = manager;
  }

  /**
   * Run extractpieces
   */
  public String extractpieces(final ExtractpiecesParam param, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param.getCommand(),
        axisID, false, processResultDisplay, processSeries, ProcessName.EXTRACTPIECES);
    return backgroundProcess.getName();
  }
}
