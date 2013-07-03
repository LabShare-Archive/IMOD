package etomo.process;

import etomo.PeetManager;
import etomo.ProcessSeries;
import etomo.comscript.AverageAllParam;
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
 * <p> Revision 1.12  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.11  2010/01/13 21:53:05  sueh
 * <p> bug# 1298 No longer saving lstThresholdArray.
 * <p>
 * <p> Revision 1.10  2009/12/08 02:41:47  sueh
 * <p> bug# 1286 Added averageAll.  Removed parserLstThresholds from PeetState; saving lstThresholds after prmParser is run.
 * <p>
 * <p> Revision 1.9  2009/12/01 00:23:06  sueh
 * <p> bug# 1285 In postProcess(BackgroundProcess).  Reset lstThresholds when
 * <p> running the parser.  This prevents out of date lstThreshold data if the
 * <p> process completes when Etomo is not running.
 * <p>
 * <p> Revision 1.8  2009/10/30 20:52:44  sueh
 * <p> bug# 1285 Put a comment in postProcess(DetachedProcess).
 * <p>
 * <p> Revision 1.7  2008/05/03 00:42:20  sueh
 * <p> bug# 847 Passing a ProcessSeries instance to all processes that use
 * <p> process objects.  The goal is to pass then back to process done functions.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:28:52  sueh
 * <p> bug# 1041 Call the base postProcess function for detached processes to handle
 * <p> saving data for resume processchunks.
 * <p>
 * <p> Revision 1.5  2007/06/06 20:40:48  sueh
 * <p> Removed print statement.
 * <p>
 * <p> Revision 1.4  2007/05/11 15:46:01  sueh
 * <p> bug# 964 Added postProcess(BackgroundProcess) to handle mtbParser.
 * <p> Added postProcess(DetachedProcess) to handle processchunks.  These
 * <p> functions will save the information needed to find the AvgVol files.
 * <p>
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

  public String peetParser(PeetParserParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, AxisID.ONLY,
        ProcessName.PEET_PARSER, processSeries);
    return backgroundProcess.getName();
  }

  public String averageAll(AverageAllParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, AxisID.ONLY,
        ProcessName.AVERAGE_ALL, processSeries, false);
    return backgroundProcess.getName();
  }

  void postProcess(BackgroundProcess process) {
    super.postProcess(process);
    ProcessName processName = process.getProcessName();
    ProcessDetails processDetails = process.getProcessDetails();
    PeetState state = manager.getState();
    if (processName == null) {
      return;
    }
    if (processName == ProcessName.PEET_PARSER) {
      if (processDetails == null) {
        return;
      }
      state.setIterationListSize(processDetails
          .getIntValue(PeetParserParam.Fields.ITERATION_LIST_SIZE));
      manager.logMessage(processDetails, AxisID.ONLY);
    }
    else if (processName == ProcessName.AVERAGE_ALL) {
      state.setIterationListSize(processDetails
          .getIntValue(AverageAllParam.Fields.ITERATION_LIST_SIZE));
      manager.logMessage(processDetails, AxisID.ONLY);
    }
  }
}
