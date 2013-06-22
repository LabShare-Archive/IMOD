package etomo.process;

import etomo.ProcessSeries;
import etomo.SerialSectionsManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.Command;
import etomo.comscript.CommandDetails;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.MidasParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.XftoxgParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.type.SerialSectionsState;
import etomo.type.ViewType;

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

  private final SerialSectionsManager manager;

  public SerialSectionsProcessManager(final SerialSectionsManager manager) {
    super(manager);
    this.manager = manager;
  }

  /**
   * Run extractpieces
   */
  public String extractpieces(final ExtractpiecesParam param, final AxisID axisID,
      final ProcessSeries processSeries) throws SystemProcessException {
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
      final ProcessSeries processSeries) throws SystemProcessException {
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
      final ProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, axisID,
        ProcessName.XFTOXG, processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run newst.com
   */
  public String newst(final ConstNewstParam newstParam, final AxisID axisID,
      final ProcessSeries processSeries) throws SystemProcessException {
    // Start the com script in the background
    NewstProcessMonitor newstProcessMonitor = new NewstProcessMonitor(manager, axisID,
        ProcessName.NEWST, newstParam);
    // Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(newstParam, newstProcessMonitor,
        axisID, null, processSeries);
    return comScriptProcess.getName();
  }

  private void setInvalidEdgeFunctions(Command command, boolean succeeded) {
    if (manager.getViewType() == ViewType.MONTAGE
        && command.getCommandName().equals(BlendmontParam.COMMAND_NAME)
        && (command.getCommandMode() == BlendmontParam.Mode.SERIAL_SECTION_PREBLEND || command
            .getCommandMode() == BlendmontParam.Mode.SERIAL_SECTION_BLEND)) {
      manager.getState().setInvalidEdgeFunctions(!succeeded);
    }
  }

  void postProcess(ComScriptProcess script) {
    try {
      // Script specific post processing
      ProcessName processName = script.getProcessName();
      ProcessDetails processDetails = script.getProcessDetails();
      CommandDetails commandDetails = script.getCommandDetails();
      Command command = script.getCommand();
      SerialSectionsState state = manager.getState();
      AxisID axisID = script.getAxisID();
      if (processName == ProcessName.PREBLEND) {
        setInvalidEdgeFunctions(script.getCommand(), true);
      }
      else if (processName == ProcessName.BLEND) {
        setInvalidEdgeFunctions(script.getCommand(), true);
      }
    }
    catch (Exception e) {
      e.printStackTrace();
      System.err.println("ERROR:  Unable to record state.");
    }
  }
  void errorProcess(ComScriptProcess script) {
    try {
      ProcessName processName = script.getProcessName();
      if (processName == ProcessName.BLEND) {
        setInvalidEdgeFunctions(script.getCommand(), false);
      }
      else if (processName == ProcessName.PREBLEND) {
        setInvalidEdgeFunctions(script.getCommand(), false);
      }
    }
    catch (Exception e) {
      e.printStackTrace();
      System.err.println("ERROR:  Unable to record state.");
    }
  }
}
