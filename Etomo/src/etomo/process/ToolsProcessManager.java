package etomo.process;

import etomo.BaseManager;
import etomo.ProcessSeries;
import etomo.ToolsManager;
import etomo.comscript.FlattenWarpParam;
import etomo.comscript.GpuTiltTestParam;
import etomo.comscript.WarpVolParam;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ProcessName;
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
 * <p> Revision 1.3  2011/02/22 04:12:40  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/04/28 16:22:57  sueh
 * <p> bug# 1344 Passing params whose processes output image files to the
 * <p> start process functions.
 * <p>
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
   * Run gpuTiltTest
   */
  public String gpuTiltTest(GpuTiltTestParam param, AxisID axisID)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param.getCommand(),
        axisID, ProcessName.GPU_TILT_TEST, null);
    return backgroundProcess.getName();
  }

  /**
   * Run the appropriate flatten com file for the given axis ID
   */
  public String flatten(final WarpVolParam param, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final FileType fileType)
      throws SystemProcessException {
    // Create the required tilt command
    String command = fileType.getFileName(manager, AxisID.ONLY);
    // Instantiate the process monitor
    Matchvol1ProcessMonitor monitor = Matchvol1ProcessMonitor.getFlattenInstance(manager,
        axisID, fileType);
    // Start the com script in the background
    ComScriptProcess comScriptProcess = startComScript(command, monitor, axisID,
        processResultDisplay, param, processSeries, fileType);
    return comScriptProcess.getName();
  }

  public String flattenWarp(final FlattenWarpParam param,
      final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final AxisID axisID)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param.getCommandArray(),
        axisID, processResultDisplay, param.getProcessName(), processSeries);
    return backgroundProcess.getName();
  }

  void postProcess(final BackgroundProcess process) {
    super.postProcess(process);
    if (process.getProcessName() == ProcessName.GPU_TILT_TEST) {
      String[] output = process.getStdOutput();
      manager.logMessageWithKeyword(output, GpuTiltTestParam.OUTPUT_KEYWORD,
          ProcessName.GPU_TILT_TEST.toString() + ":", process.getAxisID());
      manager.logMessageWithKeyword(process.getStdError(),
          GpuTiltTestParam.OUTPUT_KEYWORD, ProcessName.GPU_TILT_TEST.toString() + ":",
          process.getAxisID());
      manager.gpuTiltTestSuceeded(output, process.getAxisID());
    }
  }

  void errorProcess(final BackgroundProcess process) {
    if (process.getProcessName() == ProcessName.GPU_TILT_TEST) {
      manager.logMessageWithKeyword(process.getStdOutput(),
          GpuTiltTestParam.OUTPUT_KEYWORD, ProcessName.GPU_TILT_TEST.toString() + ":",
          process.getAxisID());
      manager.logMessageWithKeyword(process.getStdError(),
          GpuTiltTestParam.OUTPUT_KEYWORD, ProcessName.GPU_TILT_TEST.toString() + ":",
          process.getAxisID());
      manager.logMessageWithKeyword(FileType.GPU_TEST_LOG,
          GpuTiltTestParam.OUTPUT_KEYWORD, ProcessName.GPU_TILT_TEST.toString() + ":",
          process.getAxisID());
    }
  }

  BaseManager getManager() {
    return manager;
  }

  void postProcess(final DetachedProcess process) {
  }
}
