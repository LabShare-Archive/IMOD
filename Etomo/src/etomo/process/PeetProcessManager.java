package etomo.process;

import etomo.PeetManager;
import etomo.comscript.PeetParserParam;
import etomo.comscript.ProcessDetails;
import etomo.type.AxisID;
import etomo.type.PeetState;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.3  2007/04/27 23:39:20  sueh
 * <p> bug# 964 Changed prmParser() to peetParser.
 * <p>
 * <p> Revision 1.2  2007/04/26 02:43:34  sueh
 * <p> bug# 964 Added prmParser.
 * <p>
 * <p> Revision 1.1  2007/02/19 21:53:04  sueh
 * <p> bug# 964 Process manager for PEET interface.
 * <p> </p>
 */
public final class PeetProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id$";

  private final PeetManager manager;

  public PeetProcessManager(PeetManager manager) {
    super(manager);
    this.manager = manager;
  }

  public String peetParser(PeetParserParam param) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param,
        AxisID.ONLY, ProcessName.PEET_PARSER);
    return backgroundProcess.getName();
  }

   void errorProcess(BackgroundProcess process) {
  }

   void errorProcess(ComScriptProcess process) {
  }

   void errorProcess(ReconnectProcess script) {
  }

   void postProcess(ComScriptProcess script) {
  }

   void postProcess(InteractiveSystemProgram program) {
  }

   void postProcess(ReconnectProcess script) {
  }

   void postProcess(BackgroundProcess process) {
    super.postProcess(process);
    ProcessName processName = process.getProcessName();
    ProcessDetails processDetails = process.getProcessDetails();
    PeetState state = manager.getState();
    System.out.println("peet.postProcess.BackgroundProcess:processName=" + processName
        + ",processDetails=" + processDetails);
    if (processName == null) {
      return;
    }
    if (processName == ProcessName.PEET_PARSER) {
      if (processDetails == null) {
        return;
      }
      state.setParserIterationListSize(processDetails
          .getIntValue(PeetParserParam.Fields.ITERATION_LIST_SIZE));
      state.setParserLstThresholdsArray(processDetails
          .getStringArray(PeetParserParam.Fields.LST_THRESHOLDS_ARRAY));
    }
  }

   void postProcess(DetachedProcess process) {
    System.out.println("peet.postProcess.DetachedProcess");
    PeetState state = manager.getState();
    state.setIterationListSize(state.getParserIterationListSize());
    state.setLstThresholdsArray(state.getParserLstThresholdsArray());
  }
}
