package etomo.process;

import java.io.File;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.comscript.Command;
import etomo.comscript.FinishjoinParam;
import etomo.comscript.FlipyzParam;
import etomo.comscript.MakejoincomParam;
import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.type.AxisID;
import etomo.type.JoinState;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
public class JoinProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id$";

  private static final String startjoinComscriptName = "startjoin.com";
  JoinManager joinManager;
  
  public JoinProcessManager(JoinManager joinMgr) {
    super();
    joinManager = joinMgr;
  }
  
  /**
   * Run makejoincom
   */
  public String makejoincom(MakejoincomParam makejoincomParam)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(
        makejoincomParam, AxisID.ONLY);
    return backgroundProcess.getName();
  }
  
  /**
   * Run finishjoin
   */
  public String finishjoin(FinishjoinParam finishjoinParam)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(
        finishjoinParam, AxisID.ONLY);
    return backgroundProcess.getName();
  }
  
  /**
   * Run xfalign
   */
  public String xfalign(XfalignParam xfalignParam)
      throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(xfalignParam,
        AxisID.ONLY);
    return backgroundProcess.getName();
  }
  
  /**
   * Run flip
   */
  public String flipyz(FlipyzParam flipyzParam)
    throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(flipyzParam, AxisID.ONLY);
    return backgroundProcess.getName();
  }
  
  /**
   * Run the startjoin com file
   */
  public String startjoin() throws SystemProcessException {
    ComScriptProcess comScriptProcess = startComScript(startjoinComscriptName,
      null, AxisID.ONLY);
    return comScriptProcess.getName();
  }
  
  /**
   * Run midas on the sample file.
   */
  public String midasSample(MidasParam midasParam) throws SystemProcessException {
    InteractiveSystemProgram program = startInteractiveSystemProgram(midasParam);
    return program.getName();
  }
  
  public void touch(File file) {
    String[] commandArray = { "touch", file.getAbsolutePath() };
    startSystemProgramThread(commandArray);
  }

  protected void postProcess(ComScriptProcess process) {
    String commandName = process.getComScriptName();
    if (commandName == null) {
      return;
    }
    if (commandName.equals(startjoinComscriptName)) {
      joinManager.getState().setSampleProduced(true);
      joinManager.setMode();
    }
  }
  
  /**
   * non-generic post processing for a successful BackgroundProcess.
   */
  protected void postProcess(BackgroundProcess process) {
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    Command command = process.getCommand();
    if (command == null) {
      return;
    }
    if (commandName.equals(FlipyzParam.getName())) {
      joinManager.addSection(command.getOutputFile());
    }
    else if (commandName.equals(XfalignParam.getName())) {
      joinManager.copyXfFile(command.getOutputFile());
      joinManager.enableMidas();
    }
    else if (commandName.equals(FinishjoinParam.getName())) {
      int mode = process.getMode();
      if (mode == FinishjoinParam.MAX_SIZE_MODE) {
        String[] stdOutput = process.getStdOutput();
        for (int i = 0; i < stdOutput.length; i++) {
          String line = stdOutput[i];
          String[] lineArray;
          if (line.indexOf(FinishjoinParam.SIZE_TAG) != -1) {
            lineArray = line.split("\\s+");
            joinManager.setSize(lineArray[FinishjoinParam.SIZE_IN_X_INDEX],
                lineArray[FinishjoinParam.SIZE_IN_Y_INDEX]);
          }
          else if (line.indexOf(FinishjoinParam.OFFSET_TAG) != -1) {
            lineArray = line.split("\\s+");
            joinManager.setShift(FinishjoinParam
                .getShift(lineArray[FinishjoinParam.OFFSET_IN_X_INDEX]),
                FinishjoinParam
                    .getShift(lineArray[FinishjoinParam.OFFSET_IN_Y_INDEX]));
          }
        }
        return;
      }
      if (mode == FinishjoinParam.TRIAL_MODE) {
        JoinState state = joinManager.getState();
        state.setTrialBinning(command.getBinning());
        state.setTrialSizeInX(command.getIntegerValue(FinishjoinParam.SIZE_IN_X));
        state.setTrialSizeInY(command.getIntegerValue(FinishjoinParam.SIZE_IN_Y));
        state.setTrialShiftInX(command.getIntegerValue(FinishjoinParam.SHIFT_IN_X));
        state.setTrialShiftInY(command.getIntegerValue(FinishjoinParam.SHIFT_IN_Y));
      }
    }
  }
  
  protected void errorProcess(BackgroundProcess process) {
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    Command command = process.getCommand();
    if (command == null) {
      return;
    }
    if (commandName.equals(XfalignParam.getName())) {
      joinManager.enableMidas();
    }
    else if (commandName.equals(MakejoincomParam.getName())) {
      joinManager.getState().setSampleProduced(false);
      joinManager.setMode();
    }
    else if (commandName.equals(FlipyzParam.getName())) {
      File outputFile = command.getOutputFile();
      //A partially created flip file can cause an error when it is opened.
      if (outputFile != null) {
        outputFile.delete();
      }
      joinManager.abortAddSection();
    }
  }
  
  protected void errorProcess(ComScriptProcess process) {
    String commandName = process.getComScriptName();
    if (commandName == null) {
      return;
    }
    if (commandName.equals(startjoinComscriptName)) {
      joinManager.getState().setSampleProduced(false);
      joinManager.setMode();
    }
  }

  protected void postProcess(
      InteractiveSystemProgram program) {
    String commandName = program.getCommandName();
    if (commandName == null) {
      return;
    }
    Command command = program.getCommand();
    if (command == null) {
      return;
    }
    if (commandName.equals(MidasParam.getName())) {
      File outputFile = command.getOutputFile();
      if (outputFile != null
          && outputFile.exists()
          && outputFile.lastModified() > program.getOutputFileLastModified()
              .getLong()) {
        joinManager.copyXfFile(outputFile);
      }
    }
  }
  
  protected BaseManager getManager() {
    return joinManager;
  }
}
