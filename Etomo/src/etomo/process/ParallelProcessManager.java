package etomo.process;

import etomo.BaseManager;
import etomo.ParallelManager;
import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.comscript.Command;
import etomo.comscript.CommandDetails;
import etomo.comscript.TrimvolParam;
import etomo.type.AxisID;
import etomo.type.ParallelState;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 */
public final class ParallelProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id$";

  private final ParallelManager manager;

  public ParallelProcessManager(final ParallelManager manager) {
    super(manager);
    this.manager = manager;
  }

  /**
   * Run trimvol
   */
  public String trimVolume(TrimvolParam trimvolParam)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(trimvolParam,
        AxisID.ONLY, ProcessName.TRIMVOL);
    return backgroundProcess.getName();
  }

  public String anisotropicDiffusion(AnisotropicDiffusionParam param)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param,
        AxisID.ONLY, ProcessName.ANISOTROPIC_DIFFUSION);
    return backgroundProcess.getName();
  }

  public String chunksetup(ChunksetupParam param) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param
        .getCommandArray(), AxisID.ONLY, ProcessName.CHUNKSETUP);
    return backgroundProcess.getName();
  }

  void postProcess(final BackgroundProcess process) {
    CommandDetails commandDetails = process.getCommandDetails();
    if (commandDetails == null) {
      return;
    }
    if (commandDetails.getCommandName().equals(
        ProcessName.ANISOTROPIC_DIFFUSION.toString())) {
      ParallelState state = manager.getState();
      state.setTestKValue(commandDetails
          .getFloatValue(AnisotropicDiffusionParam.Fields.K_VALUE));
      state.setTestIterationList(commandDetails
          .getString(AnisotropicDiffusionParam.Fields.ITERATION_LIST));
    }
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
    Command command = process.getCommand();
    if (command == null) {
      return;
    }
    if (command.getCommandName().equals(ProcessName.PROCESSCHUNKS.toString())) {
      CommandDetails subcommandDetails = command.getSubcommandDetails();
      if (subcommandDetails != null
          && subcommandDetails.getCommandName().equals(
              ProcessName.ANISOTROPIC_DIFFUSION.toString())
          && subcommandDetails.getCommandMode() == AnisotropicDiffusionParam.Mode.VARYING_K) {
        ParallelState state = manager.getState();
        state.setTestKValueList(subcommandDetails
            .getString(AnisotropicDiffusionParam.Fields.K_VALUE_LIST));
        state.setTestIteration(subcommandDetails
            .getIntValue(AnisotropicDiffusionParam.Fields.ITERATION));
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2007/11/06 19:24:21  sueh
 * <p> bug# 1047 Added post processing for anisotropic diffusion processes.
 * <p>
 * <p> Revision 1.3  2006/08/02 22:24:10  sueh
 * <p> bug# 769 Added empty functions errorProcess and postProcess.
 * <p>
 * <p> Revision 1.2  2006/06/05 16:27:25  sueh
 * <p> bug# 766 Added manager to the base class.
 * <p>
 * <p> Revision 1.1  2006/03/20 17:52:03  sueh
 * <p> bug# 835 A process manager for ParallelManager.
 * <p> </p>
 */
