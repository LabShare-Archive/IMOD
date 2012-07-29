package etomo.process;

import etomo.BaseManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.MidasParam;
import etomo.comscript.XftoxgParam;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ProcessName;

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
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param.getCommand(),
        axisID, false, null, processSeries, ProcessName.EXTRACTPIECES);
    return backgroundProcess.getName();
  }

  /**
   * run blend comscript
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  public String blend(final BlendmontParam blendmontParam, final AxisID axisID,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    // Start the com script in the background
    BlendmontProcessMonitor blendmontProcessMonitor = new BlendmontProcessMonitor(
        manager, axisID, blendmontParam.getMode());
    // Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(blendmontParam,
        blendmontProcessMonitor, axisID, null, processSeries);
    return comScriptProcess.getName();
  }

  /**
   * Run midas.
   */
  public String midas(final MidasParam midasParam) throws SystemProcessException {
    InteractiveSystemProgram program = startInteractiveSystemProgram(midasParam);
    return program.getName();
  }

  /**
   * Run xftoxg
   * @param param
   * @param processSeries
   * @return
   * @throws SystemProcessException
   */
  public String xftoxg(final XftoxgParam param, final AxisID axisID,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, axisID,
        ProcessName.XFTOXG, processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run newst.com
   */
  public String newst(final ConstNewstParam newstParam, final AxisID axisID,
      final ConstProcessSeries processSeries) throws SystemProcessException {
    // Start the com script in the background
    NewstProcessMonitor newstProcessMonitor = new NewstProcessMonitor(manager, axisID,
        ProcessName.NEWST, newstParam);
    // Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(newstParam, newstProcessMonitor,
        axisID, null, processSeries);
    return comScriptProcess.getName();
  }
}
