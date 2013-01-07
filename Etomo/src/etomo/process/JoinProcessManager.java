package etomo.process;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.ProcessSeries;
import etomo.comscript.ClipParam;
import etomo.comscript.Command;
import etomo.comscript.ProcessDetails;
import etomo.comscript.FinishjoinParam;
import etomo.comscript.MakejoincomParam;
import etomo.comscript.RemapmodelParam;
import etomo.comscript.StartJoinParam;
import etomo.comscript.XfjointomoParam;
import etomo.comscript.XfmodelParam;
import etomo.comscript.XftoxgParam;
import etomo.storage.LogFile;
import etomo.storage.XfjointomoLog;
import etomo.type.AxisID;
import etomo.type.JoinState;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.32  2010/09/09 17:07:16  sueh
 * <p> bug# 1402 In postProcess(BackgroundProcess) compare command name to ClipParam.PROCESS_NAME.toString().
 * <p>
 * <p> Revision 1.31  2010/04/28 16:21:14  sueh
 * <p> bug# 1344 Removing FlipyzParam because it is no longer in use.
 * <p>
 * <p> Revision 1.30  2009/12/08 02:41:26  sueh
 * <p> bug# 1286 Implemented Loggable in parameter classes.
 * <p>
 * <p> Revision 1.29  2009/04/01 20:07:15  sueh
 * <p> bug# 1208 Changed flipyz to rotx.  Substituting ClipParam for FlipyzParam in post
 * <p> processing, except when deleting a file resulting from an error; in that case adding
 * <p> coverage for ClipParam.
 * <p>
 * <p> Revision 1.28  2009/02/04 23:25:27  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.27  2008/11/20 01:32:53  sueh
 * <p> bug# 1149 Moved xfmodel from JoinProcessManager to BaseProcessManager.
 * <p>
 * <p> Revision 1.26  2008/08/18 22:36:52  sueh
 * <p> bug# 1130 In postProcess(BackgroundProcess) setting joinLocalFits and joinTrialLocalFits when the mode is TRIAL or FINISH_JOIN.
 * <p>
 * <p> Revision 1.25  2008/05/03 00:41:41  sueh
 * <p> bug# 847 Passing a ProcessSeries instance to all processes that use
 * <p> process objects.  The goal is to pass then back to process done functions.
 * <p>
 * <p> Revision 1.24  2008/01/31 20:18:33  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.23  2007/06/08 23:57:11  sueh
 * <p> bug# 995 In postProcess(BackgroundProcess) save
 * <p> RefineTrialUseEveryNSlices when running FinishJoinParam in
 * <p> REJOIN_TRIAL mode.
 * <p>
 * <p> Revision 1.22  2007/02/05 22:59:21  sueh
 * <p> bug# 962 Handling remapmodel, xfmodel, and xftoxg.
 * <p>
 * <p> Revision 1.21  2006/10/16 22:42:15  sueh
 * <p> bug# 933  PostProcessing(BackgroundProcess):  moved makejoincom post
 * <p> processing to JoinManager.
 * <p>
 * <p> Revision 1.20  2006/08/02 22:24:00  sueh
 * <p> bug# 769 Added empty functions errorProcess and postProcess.
 * <p>
 * <p> Revision 1.19  2006/06/05 16:26:15  sueh
 * <p> bug# 766 Added manager to the base class.  Passing the process name to the
 * <p> processes.
 * <p>
 * <p> Revision 1.18  2006/05/11 19:54:31  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.17  2006/04/06 19:41:18  sueh
 * <p> bug# 808 Added post processing for makejoincom and startjoin.  Added
 * <p> StartJoinParam to startjoin().
 * <p>
 * <p> Revision 1.16  2006/01/20 20:56:09  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.15  2005/12/09 20:27:59  sueh
 * <p> bug# 776 Added non-abstract super.postProcessing to handle
 * <p> tomosnapshot
 * <p>
 * <p> Revision 1.14  2005/11/19 02:29:04  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetachedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.13  2005/09/09 21:36:56  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
 * <p> Revision 1.12  2005/06/21 00:45:12  sueh
 * <p> bug# 522 Moved touch() from JoinProcessManager to
 * <p> BaseProcessManager for MRCHeaderTest.
 * <p>
 * <p> Revision 1.11  2005/04/25 20:48:05  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.10  2005/01/08 01:49:22  sueh
 * <p> bug# 578 Command interface has changed - update calls.  Put GET_
 * <p> in from of statics passed to Command.getIntegerValue() and
 * <p> Command.getBooleanValue().
 * <p>
 * <p> Revision 1.9  2004/12/14 21:38:05  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 1.8  2004/12/09 04:54:45  sueh
 * <p> bug# 565 Removed save meta data from postPRocess and errorProcess
 * <p> functions.  It will be done more centrally.
 * <p>
 * <p> Revision 1.7  2004/12/08 21:23:33  sueh
 * <p> bug# 564 Changed FinishjoinParam statics SHIFT_IN_X_VALUE_NAME,
 * <p> etc to SHIFT_IN_X.
 * <p>
 * <p> Revision 1.6  2004/12/06 23:35:17  sueh
 * <p> Removed print statement.
 * <p>
 * <p> Revision 1.5  2004/11/24 01:02:08  sueh
 * <p> bug# 520 Added errorProcess(ComScriptProcess): turn off sample
 * <p> produced in meta data when startjoin is killed.  Moved kill background
 * <p> process handling to errorProcess().  Do not setMode in JoinManager until
 * <p> after startjoin has completed successfully.
 * <p>
 * <p> Revision 1.4  2004/11/23 22:31:17  sueh
 * <p> bug# 520 changing postProcess(BackgroundProcess) to delete output
 * <p> files when the Flipyx was killed
 * <p>
 * <p> Revision 1.3  2004/11/20 01:58:33  sueh
 * <p> bug# 520 Passing exitValue to postProcess(BackgroundProcess).
 * <p>
 * <p> Revision 1.2  2004/11/19 23:22:15  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.14  2004/11/19 00:01:55  sueh
 * <p> bug# 520 Saving meta data after post processing and error processing
 * <p> that changes the meta data.
 * <p>
 * <p> Revision 1.1.2.13  2004/11/16 23:30:06  sueh
 * <p> bug# 520 In errorProcess():  setting join meta data sample produced to
 * <p> false when makejoincom fails.
 * <p>
 * <p> Revision 1.1.2.12  2004/11/16 02:22:45  sueh
 * <p> EtomoNumber used getLong() instead of get().
 * <p>
 * <p> Revision 1.1.2.11  2004/11/15 22:18:58  sueh
 * <p> bug# 520 Changing postProcess(BackgoundProcess):  setting meta data
 * <p> sample produced = true when makejoincom finishes successfully.
 * <p>
 * <p> Revision 1.1.2.10  2004/11/12 22:54:57  sueh
 * <p> bug# 520 Added code to save binning, size, and shift after finishjoin in
 * <p> trial mode ends.
 * <p>
 * <p> Revision 1.1.2.9  2004/11/08 22:20:55  sueh
 * <p> bug# 520 Get the size in and X and Y and the offsetr in X and Y from
 * <p> FinishjoinParam when it is in Max Size mode.  Use FinishjoinParam to
 * <p> find the values and convert offset to shift.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/28 17:07:39  sueh
 * <p> bug# 520 Copy output file after xfalign.  Copy output file after midas, if
 * <p> it was changed.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/25 23:11:52  sueh
 * <p> bug# 520 Added to backgroundErrorProcess() for post processing when
 * <p> BackgroundProcess fails (enabling Midas when xfalign fails).
 * <p>
 * <p> Revision 1.1.2.6  2004/10/21 02:44:29  sueh
 * <p> bug# 520 Added finishJoin and midasSample.  Added post processing for
 * <p> midas and xfalign.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/18 19:08:50  sueh
 * <p> bug# 520 Added misdasSample.  Added getManager().
 * <p>
 * <p> Revision 1.1.2.4  2004/10/18 17:58:26  sueh
 * <p> bug# 520 Added xfalign.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/08 15:59:59  sueh
 * <p> bug# 520 Fixed makejoincom() to that it used BackgroundProcess.
 * <p> Added startjoin.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/06 01:40:27  sueh
 * <p> bug# 520 Added flipyz().  Added backgroundPostProcess() to handle non-
 * <p> generic processing after BackgroundProcess is done.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 17:54:52  sueh
 * <p> bug# 520 Process manager for serial sections.
 * <p> </p>
 */
public final class JoinProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id$";

  private static final String startjoinComscriptName = "startjoin.com";

  private final JoinState state;
  private final JoinManager manager;

  public JoinProcessManager(JoinManager joinMgr, JoinState state) {
    super(joinMgr);
    manager = joinMgr;
    this.state = state;
  }

  /**
   * Run makejoincom
   */
  public String makejoincom(MakejoincomParam makejoincomParam,
      final ProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(makejoincomParam,
        AxisID.ONLY, ProcessName.MAKEJOINCOM, processSeries);
    return backgroundProcess.getName();
  }

  public String remapmodel(RemapmodelParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, AxisID.ONLY,
        ProcessName.REMAPMODEL, processSeries);
    return backgroundProcess.getName();
  }

  public String xftoxg(XftoxgParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(param, AxisID.ONLY,
        ProcessName.XFTOXG, processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run the post process functionality for finishjoin without running the
   * process.
   * @param finishjoinParam
   */
  public void saveFinishjoinState(FinishjoinParam finishjoinParam,
      final ProcessSeries processSeries) {
    postProcess(new BackgroundProcess(manager, finishjoinParam, this, AxisID.ONLY,
        ProcessName.FINISHJOIN, processSeries));
  }

  /**
   * Run finishjoin
   */
  public String finishjoin(FinishjoinParam finishjoinParam,
      final ProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(finishjoinParam,
        AxisID.ONLY, ProcessName.FINISHJOIN, processSeries);
    return backgroundProcess.getName();
  }

  public String xfjointomo(XfjointomoParam xfjointomoParam,
      final ProcessSeries processSeries) throws SystemProcessException {
    XfjointomoLog.getInstance(manager).reset();
    BackgroundProcess backgroundProcess = startBackgroundProcess(
        xfjointomoParam.getCommandArray(), AxisID.ONLY, null, ProcessName.XFJOINTOMO,
        processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run clip rotx
   */
  public String rotx(ClipParam clipyzParam, final ProcessSeries processSeries)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(clipyzParam,
        AxisID.ONLY, ProcessName.CLIP, processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run the startjoin com file
   */
  public String startjoin(StartJoinParam param, final ProcessSeries processSeries)
      throws SystemProcessException {
    ComScriptProcess comScriptProcess = startComScript(param, null, AxisID.ONLY,
        processSeries);
    return comScriptProcess.getName();
  }

  void postProcess(ComScriptProcess process) {
    String commandName = process.getComScriptName();
    if (commandName == null) {
      return;
    }
    ProcessDetails processDetails = process.getProcessDetails();
    if (commandName.equals(startjoinComscriptName)) {
      state.setSampleProduced(true);
      manager.setMode();
      if (processDetails.getBooleanValue(StartJoinParam.Fields.ROTATE)) {
        state.setTotalRows(processDetails.getIntValue(StartJoinParam.Fields.TOTAL_ROWS));
        state.setRotationAnglesList(processDetails
            .getHashtable(StartJoinParam.Fields.ROTATION_ANGLES_LIST));
      }
    }
  }

  /**
   * non-generic post processing for a successful BackgroundProcess.
   */
  void postProcess(BackgroundProcess process) {
    super.postProcess(process);
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    ProcessDetails processDetails = process.getProcessDetails();
    Command command = process.getCommand();
    if (commandName.equals(ClipParam.PROCESS_NAME.toString())) {
      if (command == null) {
        return;
      }
      manager.addSection(command.getCommandOutputFile());
    }
    else if (commandName.equals(FinishjoinParam.COMMAND_NAME)) {
      if (command == null) {
        return;
      }
      FinishjoinParam.Mode mode = (FinishjoinParam.Mode) command.getCommandMode();
      if (mode == FinishjoinParam.Mode.MAX_SIZE) {
        String[] stdOutput = process.getStdOutput();
        if (stdOutput != null) {
          for (int i = 0; i < stdOutput.length; i++) {
            String line = stdOutput[i];
            String[] lineArray;
            if (line.indexOf(FinishjoinParam.SIZE_TAG) != -1) {
              lineArray = line.split("\\s+");
              manager.setSize(lineArray[FinishjoinParam.SIZE_IN_X_INDEX],
                  lineArray[FinishjoinParam.SIZE_IN_Y_INDEX]);
            }
            else if (line.indexOf(FinishjoinParam.OFFSET_TAG) != -1) {
              lineArray = line.split("\\s+");
              manager.setShift(
                  FinishjoinParam.getShift(lineArray[FinishjoinParam.OFFSET_IN_X_INDEX]),
                  FinishjoinParam.getShift(lineArray[FinishjoinParam.OFFSET_IN_Y_INDEX]));
            }
          }
        }
        return;
      }
      else if (mode == FinishjoinParam.Mode.TRIAL
          || mode == FinishjoinParam.Mode.FINISH_JOIN) {
        if (processDetails != null) {
          boolean trial = mode == FinishjoinParam.Mode.TRIAL;
          state
              .setJoinAlignmentRefSection(trial, processDetails
                  .getEtomoNumber(FinishjoinParam.Fields.ALIGNMENT_REF_SECTION));
          state.setJoinSizeInX(trial,
              processDetails.getEtomoNumber(FinishjoinParam.Fields.SIZE_IN_X));
          state.setJoinSizeInY(trial,
              processDetails.getEtomoNumber(FinishjoinParam.Fields.SIZE_IN_Y));
          state.setJoinShiftInX(trial,
              processDetails.getEtomoNumber(FinishjoinParam.Fields.SHIFT_IN_X));
          state.setJoinShiftInY(trial,
              processDetails.getEtomoNumber(FinishjoinParam.Fields.SHIFT_IN_Y));
          state.setJoinLocalFits(trial,
              processDetails.getEtomoNumber(FinishjoinParam.Fields.LOCAL_FITS));
          state.setJoinStartList(trial,
              processDetails.getIntKeyList(FinishjoinParam.Fields.JOIN_START_LIST));
          state.setJoinEndList(trial,
              processDetails.getIntKeyList(FinishjoinParam.Fields.JOIN_END_LIST));
          state.setCurrentJoinVersion(trial);
          if (mode == FinishjoinParam.Mode.TRIAL) {
            state.setJoinTrialBinning(processDetails
                .getEtomoNumber(FinishjoinParam.Fields.BINNING));
            state.setJoinTrialUseEveryNSlices(processDetails
                .getEtomoNumber(FinishjoinParam.Fields.USE_EVERY_N_SLICES));
          }
        }
        manager.updateJoinDialogDisplay();
      }
      else if (mode == FinishjoinParam.Mode.REJOIN
          || mode == FinishjoinParam.Mode.SUPPRESS_EXECUTION) {
        state.setRefineStartList(processDetails
            .getIntKeyList(FinishjoinParam.Fields.REFINE_START_LIST));
        state.setRefineEndList(processDetails
            .getIntKeyList(FinishjoinParam.Fields.REFINE_END_LIST));
        manager.updateJoinDialogDisplay();
      }
      else if (mode == FinishjoinParam.Mode.TRIAL_REJOIN) {
        state.setRefineTrialUseEveryNSlices(processDetails
            .getEtomoNumber(FinishjoinParam.Fields.USE_EVERY_N_SLICES));
      }
    }
    else if (commandName.equals(MakejoincomParam.commandName)) {
      manager.postProcess(commandName, processDetails);
    }
    else if (commandName.equals(ProcessName.XFJOINTOMO.toString())) {
      writeLogFile(process, process.getAxisID(),
          DatasetFiles.getLogName(manager, process.getAxisID(), process.getProcessName()));
      try {
        state.setGapsExist(XfjointomoLog.getInstance(manager).gapsExist());
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        // if not sure whether gaps exist, run remapmodel
        state.setGapsExist(true);
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
        // if not sure whether gaps exist, run remapmodel
        state.setGapsExist(true);
      }
      catch (IOException e) {
        e.printStackTrace();
        // if not sure whether gaps exist, run remapmodel
        state.setGapsExist(true);
      }
      manager.postProcess(commandName, process.getProcessDetails());
      manager.updateJoinDialogDisplay();
    }
    else if (commandName.equals(XfmodelParam.COMMAND_NAME)) {
      state.setXfModelOutputFile(processDetails
          .getString(XfmodelParam.Fields.OUTPUT_FILE));
    }
  }

  void errorProcess(BackgroundProcess process) {
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    else if (commandName.equals(MakejoincomParam.commandName)) {
      state.setSampleProduced(false);
      manager.setMode();
    }
    else if (commandName.equals(ClipParam.PROCESS_NAME)) {
      Command command = process.getCommand();
      if (command == null) {
        return;
      }
      File outputFile = command.getCommandOutputFile();
      // A partially created flip file can cause an error when it is opened.
      if (outputFile != null) {
        outputFile.delete();
      }
      manager.abortAddSection();
    }
    else if (commandName.equals(ProcessName.XFJOINTOMO.toString())) {
      writeLogFile(process, process.getAxisID(),
          DatasetFiles.getLogName(manager, process.getAxisID(), process.getProcessName()));
    }
  }

  void errorProcess(ComScriptProcess process) {
    String commandName = process.getComScriptName();
    if (commandName == null) {
      return;
    }
    if (commandName.equals(startjoinComscriptName)) {
      state.setSampleProduced(false);
      manager.setMode();
    }
  }

  BaseManager getManager() {
    return manager;
  }
}