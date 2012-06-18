package etomo;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.process.AutoAlignmentProcessManager;
import etomo.process.BaseProcessManager;
import etomo.process.SystemProcessException;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.ui.AutoAlignmentDisplay;
import etomo.ui.swing.UIHarness;
import etomo.util.Utilities;

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
public final class AutoAlignmentController {
  public static final String rcsid = "$Id:$";

  private static final FileType[] LOCAL_TRANSFORMATION_LIST_FILES = new FileType[] {
      FileType.LOCAL_TRANSFORMATION_LIST, FileType.AUTO_LOCAL_TRANSFORMATION_LIST,
      FileType.EMPTY_LOCAL_TRANSFORMATION_LIST, FileType.MIDAS_LOCAL_TRANSFORMATION_LIST };

  private final BaseManager manager;
  private final AutoAlignmentDisplay display;
  private final AxisID axisID;
  private final AutoAlignmentProcessManager processManager;

  public AutoAlignmentController(final BaseManager manager,
      final AutoAlignmentDisplay display) {
    this.manager = manager;
    this.display = display;
    axisID = display.getAxisID();
    processManager = new AutoAlignmentProcessManager(manager, this);
  }

  public void xfalignInitial(final ConstProcessSeries processSeries,
      final boolean tomogramAverages) {
    if (!updateMetaData()) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(manager.getName(),
        manager.getPropertyUserDir(), manager.getAutoAlignmentMetaData(),
        XfalignParam.Mode.INITIAL, tomogramAverages);
    try {
      manager.setThreadName(processManager.xfalign(xfalignParam, axisID, processSeries),
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Can't run initial xfalign\n"
          + except.getMessage(), "SystemProcessException", axisID);
      display.enableMidas();
      return;
    }
    manager.getMainPanel().startProgressBar("Initial xfalign", axisID,
        ProcessName.XFALIGN);
  }

  public void xfalignRefine(ConstProcessSeries processSeries,
      final boolean tomogramAverages, final String description) {
    if (!updateMetaData()) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(manager.getName(),
        manager.getPropertyUserDir(), manager.getAutoAlignmentMetaData(),
        XfalignParam.Mode.REFINE, tomogramAverages);
    if (!copyMostRecentXfFile(description)) {
      return;
    }
    try {
      manager.setThreadName(processManager.xfalign(xfalignParam, axisID, processSeries),
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Can't run " + description + "\n"
          + except.getMessage(), "SystemProcessException", axisID);
      display.enableMidas();
      return;
    }
    manager.getMainPanel()
        .startProgressBar("Refine xfalign", axisID, ProcessName.XFALIGN);
  }

  public void revertXfFileToMidas() {
    File midasOutputFile = FileType.MIDAS_LOCAL_TRANSFORMATION_LIST.getFile(manager,
        axisID);
    BaseProcessManager.touch(midasOutputFile.getAbsolutePath(), manager);
    copyXfFile(midasOutputFile);
  }

  public void revertXfFileToEmpty() {
    File emptyFile = FileType.EMPTY_LOCAL_TRANSFORMATION_LIST.getFile(manager, axisID);
    BaseProcessManager.touch(emptyFile.getAbsolutePath(), manager);
    copyXfFile(emptyFile);
  }

  public void enableMidas() {
    display.enableMidas();
  }

  /**
   * Run midas on the sample
   */
  public void midasSample(final String description) {
    if (!updateMetaData()) {
      return;
    }
    MidasParam midasParam = new MidasParam(manager, axisID);
    if (!copyMostRecentXfFile(description)) {
      return;
    }
    try {
      processManager.midasSample(midasParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Can't run" + description + "\n"
          + except.getMessage(), "SystemProcessException", axisID);
      return;
    }
  }

  /**
   * Create the empty xf file and copy it to root.xf.  This function may before the dataset name is set.
   * @param rootName
   */
  public void createEmptyXfFile() {
    File emptyXfFile = FileType.EMPTY_LOCAL_TRANSFORMATION_LIST.getFile(manager, axisID);
    if (!emptyXfFile.exists()) {
      String emptyLine = "   1.0000000   0.0000000   0.0000000   1.0000000       0.000       0.000";
      try {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(emptyXfFile));
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.close();
      }
      catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }
    File xfFile = FileType.LOCAL_TRANSFORMATION_LIST.getFile(manager, axisID);
    if (!xfFile.exists()) {
      try {
        Utilities.copyFile(emptyXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public void copyXfFile(File xfOutputFile) {
    File xfFile = FileType.LOCAL_TRANSFORMATION_LIST.getFile(manager, axisID);
    if (xfOutputFile != null && xfOutputFile.exists()) {
      try {
        Utilities.copyFile(xfOutputFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + xfOutputFile.getAbsolutePath() + " to "
                + xfFile.getName() + ".",
            "Copy " + xfOutputFile.getName() + " to " + xfFile.getName() + "." };
        UIHarness.INSTANCE
            .openMessageDialog(manager, message, "Cannot Copy File", axisID);
      }
    }
  }

  private boolean updateMetaData() {
    if (!manager.updateMetaData(display.getDialogType(), axisID)) {
      return false;
    }
    if (!manager.getBaseMetaData().isValid()) {
      UIHarness.INSTANCE.openMessageDialog(manager, manager.getBaseMetaData()
          .getInvalidReason(), "Invalid Data", axisID);
      return false;
    }
    if (!manager.saveMetaDataToParameterStore()) {
      return false;
    }
    return true;
  }

  private boolean copyMostRecentXfFile(final String commandDescription) {
    FileType newestXfFileType = Utilities.mostRecentFile(manager, axisID,
        LOCAL_TRANSFORMATION_LIST_FILES, 2/* EMPTY_LOCAL_TRANSFORMATION_LIST */);
    // If the most recent .xf file is not root.xf, copy it to root.xf
    if (newestXfFileType != FileType.LOCAL_TRANSFORMATION_LIST) {
      try {
        Utilities.copyFile(newestXfFileType, FileType.LOCAL_TRANSFORMATION_LIST, manager,
            axisID);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy "
                + newestXfFileType.getFile(manager, axisID).getAbsolutePath() + " to "
                + FileType.LOCAL_TRANSFORMATION_LIST.getFileName(manager, axisID) + ".",
            "Copy " + newestXfFileType.getFileName(manager, axisID) + " to "
                + FileType.LOCAL_TRANSFORMATION_LIST.getFileName(manager, axisID),
            " and then rerun " + commandDescription + "." };
        UIHarness.INSTANCE.openMessageDialog(manager, message, "Cannot Run Command",
            axisID);
        return false;
      }
    }
    return true;
  }
}
