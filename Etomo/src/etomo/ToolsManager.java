package etomo;

import java.io.File;
import java.io.IOException;

import etomo.comscript.FlattenWarpParam;
import etomo.comscript.GpuTiltTestParam;
import etomo.comscript.ToolsComScriptManager;
import etomo.comscript.WarpVolParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.SystemProcessException;
import etomo.process.ToolsProcessManager;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.ParallelState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ToolType;
import etomo.type.ToolsMetaData;
import etomo.ui.swing.Deferred3dmodButton;
import etomo.ui.swing.FlattenWarpDisplay;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogWindow;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainToolsPanel;
import etomo.ui.swing.ToolsDialog;
import etomo.ui.swing.WarpVolDisplay;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

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
 * <p> Revision 1.7  2011/02/21 21:08:13  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.6  2011/02/03 05:55:11  sueh
 * <p> bug# 1422 Using ProcessingMethod to keep track of which type of
 * <p> processing method is in use.  The decisions about when to display the
 * <p> parallel processing table have been centralized in
 * <p> ProcessingMethodMediator.
 * <p>
 * <p> Revision 1.5  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2010/04/28 15:39:23  sueh
 * <p> bug# 1344 Added getFileSubdirectoryName.  Passing params to process
 * <p> manager functions, standardizing "3dmod is open" messages to always
 * <p> use closeImod.  Using ProcessSeries.Process to hold process information.
 * <p>
 * <p> Revision 1.3  2010/03/27 04:45:53  sueh
 * <p> bug# 1316 Checking for conflicting dataset files.
 * <p>
 * <p> Revision 1.2  2010/02/26 20:37:31  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.1  2010/02/17 04:44:11  sueh
 * <p> bug# 1301 Manager for all tools menu choices.
 * <p> </p>
 */
public final class ToolsManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;
  private static final DialogType DIALOG_TYPE = DialogType.TOOLS;
  private static final int STATUS_BAR_SIZE = 65;

  private final ToolsMetaData metaData;
  private final ToolType toolType;

  private MainToolsPanel mainPanel;
  private ToolsProcessManager processMgr;

  private ToolsDialog toolsDialog = null;
  private ToolsComScriptManager comScriptMgr;

  public ToolsManager(final ToolType toolType) {
    this.toolType = toolType;
    this.metaData = new ToolsMetaData(DIALOG_TYPE, toolType, getLogProperties());
    createState();
    processMgr = new ToolsProcessManager(this);
    initializeUIParameters(null, AXIS_ID);
    // Frame hasn't been created yet so stop here.
  }

  /**
   * Open panel and add dialog.
   */
  public void initialize() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      String location = null;
      int size = STATUS_BAR_SIZE;
      if (toolType == ToolType.GPU_TILT_TEST) {
        location = propertyUserDir;
        size = 40;
      }
      mainPanel.setStatusBarText(location, size);
      openToolsDialog();
      uiHarness.toFront(this);
    }
  }

  public boolean closeFrame() {
    return true;
  }

  /**
   * Checks for .edf, .ejf, or .epe files with the same dataset name (left side)
   * as file.getName().  Conflicting dataset names can cause file name
   * collisions.
   * @param file
   * @return true if there was a conflict 
   */
  public boolean isConflictingDatasetName(AxisID axisID, File file) {
    File dir = file.getParentFile();
    File[] conflictFileList = dir.listFiles(new ConflictFileFilter(file.getName()));
    if (conflictFileList.length > 0) {
      uiHarness.openMessageDialog(this, "This file, " + file.getAbsolutePath()
          + ", cannot be opened by the Tools interface in the this "
          + "directory because the name conflicts with the dataset file "
          + conflictFileList[0].getName() + ".  Please copy " + file.getName()
          + " to another directory and work on it there, or change its name.",
          "Entry Error", axisID);
      return true;
    }
    return false;
  }

  /**
   * Sets the dataset name.
   * @param file
   */
  public void setName(File inputFile) {
    propertyUserDir = inputFile.getParent();
    metaData.setRootName(inputFile);
    mainPanel.setStatusBarText(propertyUserDir, STATUS_BAR_SIZE);
    uiHarness.setTitle(this, toolType.toString() + " - " + getName());
    // Open dialog tasks for for Flatten Volume
    if (toolType == ToolType.FLATTEN_VOLUME) {
      comScriptMgr.loadFlatten(AxisID.ONLY);
      // Set from dataset_flatten.com. Dataset_flatten.com would only exist if
      // the Tools flatten functionality was used before on the same file in the
      // same directory.
      if (comScriptMgr.isWarpVolParamInFlatten(AxisID.ONLY)) {
        toolsDialog.setParameters(comScriptMgr.getWarpVolParamFromFlatten(AxisID.ONLY));
      }
    }
  }

  public void openToolsDialog() {
    if (toolsDialog == null) {
      toolsDialog = ToolsDialog.getInstance(this, AXIS_ID, DIALOG_TYPE, toolType);
    }
    mainPanel.showProcess(toolsDialog.getContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(DialogType.TOOLS,
        AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  public void gpuTiltTestSuceeded(final String[] output, final AxisID axisID) {
    if (output != null && output.length > 0) {
      uiHarness.openInfoMessageDialog(this, output[output.length - 1],
          "Gputilttest Succeeded", axisID);
    }
  }

  public void gpuTiltTest(final AxisID axisID) {
    GpuTiltTestParam param = updateGpuTiltTest(axisID, true);
    if (param == null) {
      return;
    }
    String threadName;
    try {
      threadName = processMgr.gpuTiltTest(param, axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.GPU_TILT_TEST;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
      return;
    }
    mainPanel.startProgressBar("Running gputilttest", axisID, ProcessName.GPU_TILT_TEST);
    setThreadName(threadName, axisID);
  }

  /**
   * Execute flatten.com
   */
  public void flatten(final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType,
      final AxisID axisID, WarpVolDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    WarpVolParam param = updateWarpVolParam(display, axisID, false);
    if (param == null) {
      return;
    }
    // Run process
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.flatten(param, axisID, processResultDisplay, processSeries,
          FileType.FLATTEN_TOOL_COMSCRIPT);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.FLATTEN;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  private GpuTiltTestParam updateGpuTiltTest(final AxisID axisID,
      final boolean doValidation) {
    GpuTiltTestParam param = new GpuTiltTestParam();
    if (toolsDialog == null) {
      uiHarness.openMessageDialog(this, "Unable to get information from the dialog.",
          "Etomo Error", axisID);
      return null;
    }
    if (!toolsDialog.getParameters(param, doValidation)) {
      return null;
    }
    return param;
  }

  private WarpVolParam updateWarpVolParam(final WarpVolDisplay display,
      final AxisID axisID, final boolean doValidation) {
    WarpVolParam param = comScriptMgr.getWarpVolParamFromFlatten(axisID);
    if (display == null) {
      uiHarness.openMessageDialog(this, "Unable to get information from the display.",
          "Etomo Error", axisID);
      return null;
    }
    if (!display.getParameters(param, doValidation)) {
      return null;
    }
    comScriptMgr.saveFlatten(param, axisID);
    return param;
  }

  /**
   * Open flatten.com output
   * @param menuOptions
   * @param axisID
   */
  public void imodFlatten(Run3dmodMenuOptions menuOptions, AxisID axisID) {
    String key = ImodManager.FLATTEN_TOOL_OUTPUT_KEY;
    try {
      imodManager.open(key, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage()
          + "\nCan't open 3dmod on the " + key, "Cannot Open 3dmod", axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  public void imodMakeSurfaceModel(Run3dmodMenuOptions menuOptions, AxisID axisID,
      int binning, File file) {
    // Pick ImodManager key
    // Need to look at tomogram edge on. Use -Y, unless using squeezevol and it
    // is not flipped.
    String key = ImodManager.FLATTEN_INPUT_KEY;
    boolean useSwapYZ = isFlipped(file);
    try {
      imodManager.setSwapYZ(key, file, useSwapYZ);
      imodManager.setOpenContours(key, axisID, true);
      imodManager.setStartNewContoursAtNewZ(key, axisID, true);
      imodManager.setBinningXY(key, binning);
      imodManager
          .open(key, file, FileType.FLATTEN_WARP_INPUT_MODEL.getFileName(this, AXIS_ID),
              true, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage()
          + "\nCan't open 3dmod on the " + key, "Cannot Open 3dmod", axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  /**
   * Backward compatibility
   * function to decide whether file is flipped based on the header
   * @return
   */
  private boolean isFlipped(File mrcFile) {
    MRCHeader header = MRCHeader.getInstance(getPropertyUserDir(),
        mrcFile.getAbsolutePath(), AXIS_ID);
    try {
      if (!header.read(this)) {
        return false;
      }
    }
    catch (IOException e) {
      return false;
    }
    catch (Exception e) {
      e.printStackTrace();
      return false;
    }
    if (header.getNRows() < header.getNSections()) {
      System.err.println("Assuming that " + mrcFile.getName() + " has not been flipped\n"
          + "because the Y is less then Z in the header.");
      return false;
    }
    System.err.println("Assuming that " + mrcFile.getName() + " has been flipped\n"
        + "because the Y is greater or equal to Z in the header.");
    return true;
  }

  /**
   * Execute flattenwarp
   */
  public void flattenWarp(final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType,
      final AxisID axisID, FlattenWarpDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    FlattenWarpParam param = updateFlattenWarpParam(display, axisID, true);
    if (param == null) {
      return;
    }
    // Run process
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.flattenWarp(param, processResultDisplay, processSeries,
          axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + param.getProcessName();
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + param.getProcessName(), axisID,
        param.getProcessName());
  }

  private FlattenWarpParam updateFlattenWarpParam(FlattenWarpDisplay display,
      final AxisID axisID, final boolean doValidation) {
    FlattenWarpParam param = new FlattenWarpParam(this);
    if (display == null) {
      uiHarness.openMessageDialog(this, "Unable to get information from the display.",
          "Etomo Error", axisID);
      return null;
    }
    if (!display.getParameters(param, doValidation)) {
      return null;
    }
    return param;
  }

  /**
   * Open 3dmodv with a model
   */
  public void imodViewModel(AxisID axisID, FileType modelFileType) {
    try {
      imodManager.open(modelFileType.getImodManagerKey(this), axisID,
          modelFileType.getFileName(this, axisID));
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "Can't open 3dmod on "
          + modelFileType.getFileName(this, axisID), axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  public ParallelState getState() {
    return null;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.TOOLS;
  }

  public LogInterface getLogInterface() {
    return toolsDialog;
  }

  void createComScriptManager() {
    comScriptMgr = new ToolsComScriptManager(this);
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainToolsPanel(this);
    }
  }

  private void createState() {
  }

  LogWindow createLogWindow() {
    return null;
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  Storable[] getStorables(final int offset) {
    return null;
  }

  public boolean isInManagerFrame() {
    return true;
  }

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    return true;
  }

  public boolean exitProgram(final AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        endThreads();
        saveParamFile();
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  public String getName() {
    return metaData.getName();
  }

  /**
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(axisProcessData.getSavedProcessData(AxisID.ONLY), AxisID.ONLY, false);
  }

  /**
   * Class identifies dataset files that conflict with the member variable
   * compareFileName.  Used to avoid file name collisions between tools projects
   * and exclusive datasets.  Ignores parallel processing datasets because these
   * are file oriented datasets like tools projects.
   * @author sueh
   *
   */
  private static final class ConflictFileFilter extends
      javax.swing.filechooser.FileFilter implements java.io.FileFilter {
    private final String compareFileName;

    private ConflictFileFilter(final String compareFileName) {
      this.compareFileName = compareFileName;
    }

    /**
     * @return true if file is in conflict with compareFileName
     */
    public boolean accept(final File file) {
      // If this file has one of the five exclusive dataset extensions and the
      // left side of the file name is equal to compareFileName, then
      // compareFileName is in conflict with the dataset in this directory and
      // may cause file name collisions.
      if (file.isFile()) {
        String fileName = file.getName();
        if (fileName.endsWith(DataFileType.RECON.extension)
            || fileName.endsWith(DataFileType.JOIN.extension)
            || fileName.endsWith(DataFileType.PEET.extension)
            || fileName.endsWith(DataFileType.SERIAL_SECTIONS.extension)) {
          return fileName.substring(0, fileName.lastIndexOf('.')).equals(compareFileName);
        }
      }
      return false;
    }

    /**
     * @see javax.swing.filechooser.FileFilter#getDescription()
     */
    public String getDescription() {
      return "Dataset file that conflicts with " + compareFileName;
    }
  }
}
