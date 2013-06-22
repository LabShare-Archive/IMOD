package etomo.process;

import etomo.BaseManager;
import etomo.ParallelManager;
import etomo.ProcessSeries;
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
  public String trimVolume(TrimvolParam trimvolParam, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(trimvolParam,
        AxisID.ONLY, ProcessName.TRIMVOL, processSeries);
    return backgroundProcess.getName();
  }

  public String anisotropicDiffusion(AnisotropicDiffusionParam param,
      final ProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION, processSeries);
    return backgroundProcess.getName();
  }

  public String chunksetup(ChunksetupParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param.getCommandArray(),
        AxisID.ONLY, ProcessName.CHUNKSETUP, processSeries);
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
          .getDoubleValue(AnisotropicDiffusionParam.Field.K_VALUE));
      state.setTestIterationList(commandDetails
          .getIteratorElementList(AnisotropicDiffusionParam.Field.ITERATION_LIST));
    }
  }

  BaseManager getManager() {
    return manager;
  }

  void postProcess(final DetachedProcess process) {
    super.postProcess(process);
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
            .getString(AnisotropicDiffusionParam.Field.K_VALUE_LIST));
        state.setTestIteration(subcommandDetails
            .getIntValue(AnisotropicDiffusionParam.Field.ITERATION));
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2009/09/05 00:38:55  sueh
 * <p> bug# 1256 Changed testIterationList to IteratorElementList type.
 * <p>
 * <p> Revision 1.7  2008/05/03 00:41:51  sueh
 * <p> bug# 847 Passing a ProcessSeries instance to all processes that use
 * <p> process objects.  The goal is to pass then back to process done functions.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:20:28  sueh
 * <p> bug# 1041 Call the base postProcess function for detached processes to handle
 * <p> saving data for resume processchunks.
 * <p>
 * <p> Revision 1.5  2007/11/09 17:44:31  sueh
 * <p> bug# 1047 Changed the names of NAD fields for understandability.
 * <p>
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
