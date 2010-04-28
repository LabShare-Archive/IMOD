package etomo.process;

import etomo.BaseManager;
import etomo.ToolsManager;
import etomo.comscript.FlattenWarpParam;
import etomo.comscript.WarpVolParam;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.FileType;
import etomo.type.ProcessResultDisplay;

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
 * <p> $Log$
 * <p> Revision 1.1  2010/02/17 04:49:00  sueh
 * <p> bug# 1301 Process manager for tools processes.
 * <p> </p>
 */
public final class ToolsProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id$";

  private final ToolsManager manager;

  public ToolsProcessManager(final ToolsManager manager) {
    super(manager);
    this.manager = manager;
  }

  /**
   * Run the appropriate flatten com file for the given axis ID
   */
  public String flatten(final WarpVolParam param, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries, final FileType fileType)
      throws SystemProcessException {
    //  Create the required tilt command
    String command = fileType.getFileName(manager);
    //  Instantiate the process monitor
    Matchvol1ProcessMonitor monitor = Matchvol1ProcessMonitor
        .getFlattenInstance(manager, axisID, fileType);
    //  Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, monitor,
        axisID, processResultDisplay, param, processSeries, fileType);
    return comScriptProcess.getName();
  }

  public String flattenWarp(final FlattenWarpParam param,
      final ProcessResultDisplay processResultDisplay,
      final ConstProcessSeries processSeries, final AxisID axisID)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param
        .getCommandArray(), axisID, processResultDisplay, param
        .getProcessName(), processSeries);
    return backgroundProcess.getName();
  }

  void postProcess(final BackgroundProcess process) {
  }

  void errorProcess(final BackgroundProcess process) {
  }

  void errorProcess(final ComScriptProcess process) {
  }

  BaseManager getManager() {
    return manager;
  }

  void postProcess(final ComScriptProcess script) {
  }

  void postProcess(final InteractiveSystemProgram program) {
  }

  void errorProcess(final ReconnectProcess script) {
  }

  void postProcess(final ReconnectProcess script) {
  }

  void postProcess(final DetachedProcess process) {
  }
}
