package etomo.process;

import java.io.File;

import etomo.AutoAlignmentController;
import etomo.BaseManager;
import etomo.ProcessSeries;
import etomo.comscript.Command;
import etomo.comscript.MidasParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.XfalignParam;
import etomo.type.AxisID;
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
public final class AutoAlignmentProcessManager extends BaseProcessManager {
  public static final String rcsid = "$Id:$";

  private final BaseManager manager;
  private final AutoAlignmentController controller;

  public AutoAlignmentProcessManager(final BaseManager manager,
      final AutoAlignmentController controller) {
    super(manager);
    this.manager = manager;
    this.controller = controller;
  }

  /**
   * Run xfalign
   */
  public String xfalign(final XfalignParam xfalignParam, final AxisID axisID,
      final ProcessSeries processSeries) throws SystemProcessException {
    BackgroundProcess backgroundProcess = startBackgroundProcess(xfalignParam, axisID,
        ProcessName.XFALIGN, processSeries);
    return backgroundProcess.getName();
  }

  /**
   * Run midas on the sample file.
   */
  public String midasSample(MidasParam midasParam) throws SystemProcessException {
    InteractiveSystemProgram program = startInteractiveSystemProgram(midasParam);
    return program.getName();
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
    if (commandName.equals(XfalignParam.getName())) {
      if (command == null) {
        return;
      }
      writeLogFile(process, process.getAxisID(), XfalignParam.getName() + ".log");
      controller.copyXfFile(command.getCommandOutputFile());
      controller.msgProcessEnded();
    }
  }

  void errorProcess(BackgroundProcess process) {
    String commandName = process.getCommandName();
    if (commandName == null) {
      return;
    }
    if (commandName.equals(XfalignParam.getName())) {
      writeLogFile(process, process.getAxisID(), XfalignParam.getName() + ".log");
      controller.msgProcessEnded();
    }
  }

  void postProcess(InteractiveSystemProgram program) {
    String commandName = program.getCommandName();
    if (commandName == null) {
      return;
    }
    Command command = program.getCommand();
    if (command == null) {
      return;
    }
    if (commandName.equals(MidasParam.getName())) {
      File outputFile = command.getCommandOutputFile();
      if (outputFile != null && outputFile.exists()
          && outputFile.lastModified() > program.getOutputFileLastModified().getLong()) {
        controller.copyXfFile(outputFile);
      }
    }
  }
}
