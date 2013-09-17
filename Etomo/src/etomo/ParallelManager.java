package etomo;

import java.io.File;
import java.io.IOException;
import java.util.List;

import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TrimvolParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ParallelProcessManager;
import etomo.process.SystemProcessException;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.ParallelMetaData;
import etomo.type.ParallelState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.UIComponent;
import etomo.ui.swing.AnisotropicDiffusionDialog;
import etomo.ui.swing.Deferred3dmodButton;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainParallelPanel;
import etomo.ui.swing.ParallelChooser;
import etomo.ui.swing.ParallelDialog;
import etomo.ui.swing.ParallelPanel;
import etomo.ui.swing.ProcessDisplay;
import etomo.ui.swing.UIHarness;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

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
 */

public final class ParallelManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;

  private final BaseScreenState screenState = new BaseScreenState(AXIS_ID,
      AxisType.SINGLE_AXIS);
  private final ParallelState state = new ParallelState(this, AXIS_ID);

  private final ParallelMetaData metaData;

  private ParallelDialog parallelDialog = null;
  private AnisotropicDiffusionDialog anisotropicDiffusionDialog = null;

  private MainParallelPanel mainPanel;
  private ParallelProcessManager processMgr;

  public ParallelManager() {
    this("", null);
  }

  public ParallelManager(final DialogType dialogType) {
    this("", dialogType);
  }

  public ParallelManager(final String paramFileName) {
    this(paramFileName, null);
  }

  public ParallelManager(final String paramFileName, final DialogType dialogType) {
    super();
    this.metaData = new ParallelMetaData(getLogProperties());
    createState();
    processMgr = new ParallelProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData, logWindow);
      if (paramFile == null) {
        if (dialogType == DialogType.PARALLEL) {
          openParallelDialog();
        }
        else if (dialogType == DialogType.ANISOTROPIC_DIFFUSION) {
          openAnisotropicDiffusionDialog();
        }
        else {
          openParallelChooser();
        }
      }
      else {
        if (metaData.getDialogType() == DialogType.PARALLEL) {
          openParallelDialog();
        }
        else if (metaData.getDialogType() == DialogType.ANISOTROPIC_DIFFUSION) {
          openAnisotropicDiffusionDialog();
        }
      }
    }
  }

  public ParallelState getState() {
    return state;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.PP;
  }

  public boolean canSnapshot() {
    return false;
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainParallelPanel(this);
    }
  }

  private void createState() {
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public BaseScreenState getBaseScreenState(final AxisID axisID) {
    return screenState;
  }

  public BaseState getBaseState() {
    return state;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  public String getFileSubdirectoryName() {
    if (anisotropicDiffusionDialog == null) {
      return null;
    }
    return anisotropicDiffusionDialog.getSubdirectory();
  }

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[3 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    storables[index++] = state;
    return storables;
  }

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    saveDialog();
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

  boolean startNextProcess(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final DialogType dialogType, ProcessDisplay display) {
    if (super.startNextProcess(uiComponent, axisID, process, processResultDisplay,
        processSeries, dialogType, display)) {
      return true;
    }
    if (process.equals(ProcessName.ANISOTROPIC_DIFFUSION.toString())) {
      anisotropicDiffusion(processSeries, process.getProcessingMethod(), dialogType);
      return true;
    }
    return false;
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

  private void openParallelChooser() {
    ParallelChooser parallelChooser = ParallelChooser.getInstance(this);
    mainPanel.showProcess(parallelChooser.getContainer(), AXIS_ID);
  }

  public void openParallelDialog() {
    if (parallelDialog == null) {
      parallelDialog = ParallelDialog.getInstance(this, AXIS_ID);
    }
    parallelDialog.setParameters(screenState);
    if (paramFile != null && metaData.isValid()) {
      parallelDialog.setParameters(metaData);
      parallelDialog.updateDisplay(false);
    }
    mainPanel.showProcess(parallelDialog.getContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(DialogType.PARALLEL,
        AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  public void openAnisotropicDiffusionDialog() {
    metaData.setDialogType(DialogType.ANISOTROPIC_DIFFUSION);
    if (anisotropicDiffusionDialog == null) {
      anisotropicDiffusionDialog = AnisotropicDiffusionDialog.getInstance(this, AXIS_ID);
    }
    if (paramFile != null && metaData.isValid()) {
      anisotropicDiffusionDialog.setParameters(metaData);
    }
    mainPanel.showProcess(anisotropicDiffusionDialog.getContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.ANISOTROPIC_DIFFUSION, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  private void saveParallelDialog() {
    if (parallelDialog == null) {
      return;
    }
    parallelDialog.getParameters(screenState);
    saveStorables(AXIS_ID);
  }

  private void saveAnisotropicDiffusionDialog() {
    if (anisotropicDiffusionDialog == null) {
      return;
    }
    anisotropicDiffusionDialog.getParameters(metaData);
    anisotropicDiffusionDialog.getParametersForTrimvol(metaData);
    saveStorables(AXIS_ID);
  }

  /**
   * set the paramFile and change the state, if necessary
   * run BaseManager.processchunks
   */
  public final void processchunks(final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final String rootName,
      final FileType outputImageFileType, final ProcessingMethod processingMethod,
      final DialogType dialogType) {
    if (parallelDialog == null) {
      return;
    }
    if (paramFile == null) {
      if (!setNewParamFile()) {
        return;
      }
      parallelDialog.updateDisplay(false);
    }
    sendMsgProcessStarting(processResultDisplay);
    if (parallelDialog == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY, rootName,
        outputImageFileType);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    parallelDialog.getParameters(param);
    if (!parallelPanel.getParameters(param, true)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(AxisID.ONLY, param, processResultDisplay, processSeries, true,
        processingMethod, false, dialogType);
  }

  public boolean setNewParamFile(final File file) {
    if (loadedParamFile) {
      return true;
    }
    // set paramFile and propertyUserDir
    String parentDirPath = file.getParentFile().getAbsolutePath();
    if (parentDirPath.endsWith(" ")) {
      uiHarness.openMessageDialog(this, "The directory, " + parentDirPath
          + ", cannot be used because it ends with a space.", "Unusable Directory Name",
          AxisID.ONLY);
      return false;
    }
    propertyUserDir = parentDirPath;
    System.err.println("propertyUserDir: " + propertyUserDir);
    anisotropicDiffusionDialog.getInitialParameters(metaData);
    String errorMessage = metaData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(this, errorMessage,
          "Anisotropic Diffusion Dialog error", AXIS_ID);
      return false;
    }
    imodManager.setMetaData(metaData);
    if (!setParamFile(new File(propertyUserDir, metaData.getMetaDataFileName()))) {
      return false;
    }
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    return true;
  }

  /**
   * Asks to close the files to be cleaned up, and then cleans up if the files
   * are closed.
   * @param subdirName
   * @return
   */
  public boolean deleteSubdir(String subdirName) {
    if (closeImods(ImodManager.TEST_VOLUME_KEY, ImodManager.VARYING_K_TEST_KEY,
        ImodManager.VARYING_ITERATION_TEST_KEY, AxisID.ONLY,
        "Temporary files must be closed before cleaning up.  Should files be closed?")) {
      File subdir = new File(getPropertyUserDir(), subdirName);
      File[] fileList = subdir.listFiles();
      for (int i = 0; i < fileList.length; i++) {
        fileList[i].delete();
      }
      subdir.delete();
      return true;
    }
    return false;
  }

  public void imod(FileType fileType, final Run3dmodMenuOptions menuOptions,
      final boolean flip) {
    File file = fileType.getFile(this, AxisID.ONLY);
    if (!file.exists()) {
      uiHarness.openMessageDialog(this, "No file to open", "Entry Error");
      return;
    }
    try {
      imodManager.open(fileType.getImodManagerKey(this), file, menuOptions, flip);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
  }

  public void imod(final String key, final File file,
      final Run3dmodMenuOptions menuOptions, final boolean flip) {
    if (file == null) {
      uiHarness.openMessageDialog(this, "No file to open", "Entry Error");
      return;
    }
    try {
      imodManager.open(key, file, menuOptions, flip);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
  }

  public void imodVaryingKValue(final String key, final Run3dmodMenuOptions menuOptions,
      final String subdirName, final String testVolumeName, final boolean flip) {
    List fileNameList = AnisotropicDiffusionParam.getTestFileNameList(this,
        state.getTestKValueList(), state.getTestIteration(), testVolumeName);
    imod(key, menuOptions, subdirName, fileNameList, flip);
  }

  public void imodVaryingIteration(final String key,
      final Run3dmodMenuOptions menuOptions, final String subdirName,
      final String testVolumeName, final boolean flip) {
    List fileNameList = AnisotropicDiffusionParam.getTestFileNameList(
        state.getTestKValue(), state.getTestIterationList(), testVolumeName);
    imod(key, menuOptions, subdirName, fileNameList, flip);
  }

  private void imod(String key, Run3dmodMenuOptions menuOptions, String subdirName,
      List fileNameList, final boolean flip) {
    String[] fileNameArray;
    if (fileNameList.size() == 0) {
      fileNameArray = new String[0];
    }
    else if (fileNameList.size() == 1) {
      fileNameArray = new String[1];
      fileNameArray[0] = (String) fileNameList.get(0);
    }
    else {
      fileNameArray = (String[]) fileNameList.toArray(new String[fileNameList.size()]);
    }
    try {
      imodManager.open(key, fileNameArray, menuOptions, subdirName, flip);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
  }

  public boolean makeSubdir(final String subdirName) {
    if (paramFile == null) {
      uiHarness.openMessageDialog(this, "Must pick a volume.", "Entry Error");
      return false;
    }
    File subdir = new File(propertyUserDir, subdirName);
    if (!subdir.exists()) {
      subdir.mkdir();
    }
    return true;
  }

  private TrimvolParam updateTrimvolParam(final boolean doValidation) {
    // Get trimvol param data from dialog.
    TrimvolParam param = new TrimvolParam(this, TrimvolParam.Mode.NAD);
    if (!anisotropicDiffusionDialog.getParameters(param, doValidation)) {
      return null;
    }
    anisotropicDiffusionDialog.getParametersForTrimvol(metaData);
    param.setOldFlippedCoordinates(metaData.isNewStyleZ());
    return param;
  }

  private AnisotropicDiffusionParam updateAnisotropicDiffusionParamForVaryingK(
      final String subdirName, final boolean doValidation) throws LogFile.LockException,
      IOException {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this,
        AnisotropicDiffusionParam.Mode.VARYING_K);
    if (!anisotropicDiffusionDialog.getParametersForVaryingK(param, doValidation)) {
      return null;
    }
    param.deleteTestFiles();
    param.createTestFiles();
    return param;
  }

  private AnisotropicDiffusionParam updateAnisotropicDiffusionParam(
      final boolean doValidation) throws LogFile.LockException, IOException {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this,
        AnisotropicDiffusionParam.Mode.FULL);
    if (!anisotropicDiffusionDialog.getParameters(param, doValidation)) {
      return null;
    }
    param.createFilterFullFile();
    return param;
  }

  private ChunksetupParam updateChunksetupParam() {
    ChunksetupParam param = new ChunksetupParam();
    anisotropicDiffusionDialog.getParameters(param);
    return param;
  }

  public boolean setupAnisotropicDiffusion(final boolean doValidation) {
    try {
      AnisotropicDiffusionParam param = updateAnisotropicDiffusionParam(doValidation);
      if (param == null) {
        return false;
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return false;
    }
    return true;
  }

  private AnisotropicDiffusionParam updateAnisotropicDiffusionParamForVaryingIteration(
      final String subdirName, final boolean doValidation) {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this,
        AnisotropicDiffusionParam.Mode.VARYING_ITERATIONS);
    if (!anisotropicDiffusionDialog.getParametersForVaryingIteration(param, doValidation)) {
      return null;
    }
    return param;
  }

  public void chunksetup(ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton, Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType, final ProcessingMethod processingMethod) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog(this, "Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    if (!setupAnisotropicDiffusion(true)) {
      return;
    }
    ChunksetupParam param = updateChunksetupParam();
    if (param == null) {
      return;
    }
    processSeries.setNextProcess(ProcessName.ANISOTROPIC_DIFFUSION.toString(),
        processingMethod);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.chunksetup(param, processSeries);
    }
    catch (SystemProcessException e) {
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.CHUNKSETUP + "command";
      message[1] = e.getMessage();
      uiHarness
          .openMessageDialog(this, message, "Unable to execute command", AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar(ProcessName.CHUNKSETUP.toString(), AxisID.ONLY,
        ProcessName.CHUNKSETUP);
  }

  public void anisotropicDiffusionVaryingIteration(final String subdirName,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog(this, "Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    AnisotropicDiffusionParam param = updateAnisotropicDiffusionParamForVaryingIteration(
        subdirName, true);
    if (param == null) {
      return;
    }
    if (!validateTestVolume(param)) {
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    // Start the trimvol process
    String threadName;
    try {
      threadName = processMgr.anisotropicDiffusion(param, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.ANISOTROPIC_DIFFUSION + "command";
      message[1] = e.getMessage();
      uiHarness
          .openMessageDialog(this, message, "Unable to execute command", AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar(ProcessName.ANISOTROPIC_DIFFUSION.toString(), AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION);
  }

  public void anisotropicDiffusion(ProcessSeries processSeries,
      final ProcessingMethod processingMethod, final DialogType dialogType) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog(this, "Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION, FileType.ANISOTROPIC_DIFFUSION_OUTPUT);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    anisotropicDiffusionDialog.getParameters(param);
    if (!parallelPanel.getParameters(param, true)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    parallelPanel.resetResults();
    processchunks(AxisID.ONLY, param, null, processSeries, true, processingMethod, false,
        dialogType);
  }

  /**
   * Attempts to validate the size of the test volume.  Returns false if the
   * test volume is too big.  Returns true if the test volume is not too big or
   * if the validation cannot be done.
   * The test volume is too big if it is larger then the current
   * "Memory per chunk" spinner setting in AnisotripicDiffusionDialog divided by
   * 36 (ChunksetupParam.MEMORY_TO_VOXEL).
   * @param param
   * @return False if test volume is too big.
   */
  private boolean validateTestVolume(AnisotropicDiffusionParam param) {
    MRCHeader testVolumeHeader = MRCHeader.getInstance(
        new File(propertyUserDir, param.getSubdirName()).getAbsolutePath(),
        param.getInputFileName(), AxisID.ONLY);
    try {
      testVolumeHeader.read(this);
      long size = Math.round(testVolumeHeader.getNColumns() / 1024.0
          * testVolumeHeader.getNRows() * testVolumeHeader.getNSections()
          * ChunksetupParam.MEMORY_TO_VOXEL / 1024.0);
      if (size > 0 && size > anisotropicDiffusionDialog.getMemoryPerChunk().intValue()) {
        UIHarness.INSTANCE.openMessageDialog(
            this,
            "Processing this test volume would require " + size
                + " MB of memory, more than the limit of "
                + anisotropicDiffusionDialog.getMemoryPerChunk()
                + " MB that you have set for chunks when processing the "
                + "whole volume.  Make a smaller test volume or increase the "
                + "memory limit for chunks in the \""
                + AnisotropicDiffusionDialog.MEMORY_PER_CHUNK_LABEL
                + "\" spinner in the "
                + AnisotropicDiffusionDialog.FILTER_FULL_VOLUME_LABEL + " box.",
            "Entry Error", AxisID.ONLY);
        return false;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      System.err.println("Unable to validate test volume.");
    }
    catch (IOException e) {
      e.printStackTrace();
      System.err.println("Unable to validate test volume.");
    }
    return true;
  }

  public void anisotropicDiffusionVaryingK(final String subdirName,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType,
      final ProcessingMethod processingMethod) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog(this, "Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    AnisotropicDiffusionParam anisotropicDiffusionParam;
    try {
      anisotropicDiffusionParam = updateAnisotropicDiffusionParamForVaryingK(subdirName,
          true);
      if (anisotropicDiffusionParam == null) {
        return;
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return;
    }
    if (!validateTestVolume(anisotropicDiffusionParam)) {
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION, null);
    param.setSubcommandDetails(anisotropicDiffusionParam);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    anisotropicDiffusionDialog.getParameters(param);
    if (!parallelPanel.getParameters(param, true)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    parallelPanel.resetResults();
    processchunks(AxisID.ONLY, param, null, processSeries, true, processingMethod, false,
        dialogType);
  }

  /**
   * Execute trimvol
   */
  public void trimVolume(final ProcessSeries processSeries) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog(this, "Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    TrimvolParam param = updateTrimvolParam(true);
    if (param == null) {
      return;
    }
    // Start the trimvol process
    String threadName;
    try {
      threadName = processMgr.trimVolume(param, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute trimvol command";
      message[1] = e.getMessage();
      uiHarness
          .openMessageDialog(this, message, "Unable to execute command", AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Trimming volume", AxisID.ONLY, ProcessName.TRIMVOL);
  }

  private boolean setNewParamFile() {
    if (loadedParamFile) {
      return true;
    }
    // set paramFile and propertyUserDir
    String workingDir = parallelDialog.getWorkingDir().getAbsolutePath();
    if (workingDir.endsWith(" ")) {
      uiHarness.openMessageDialog(this, "The directory, " + workingDir
          + ", cannot be used because it ends with a space.", "Unusable Directory Name",
          AxisID.ONLY);
      return false;
    }
    propertyUserDir = workingDir;
    System.err.println("propertyUserDir: " + propertyUserDir);
    parallelDialog.getParameters(metaData);
    String errorMessage = metaData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(this, errorMessage,
          "Anisotropic Diffusion Error", AXIS_ID);
      return false;
    }
    if (!setParamFile(new File(propertyUserDir, metaData.getMetaDataFileName()))) {
      return false;
    }
    System.err.println("paramFile: " + paramFile);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    return true;
  }

  private void saveDialog() {
    saveParallelDialog();
    saveAnisotropicDiffusionDialog();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.49  2011/04/09 06:21:06  sueh
 * <p> bug# 1416 Need to pass the manager to most FileType functions so that TILT_OUTPUT can distinguish
 * <p> between single and dual axis type.
 * <p>
 * <p> Revision 1.48  2011/02/21 21:07:05  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.47  2011/02/03 05:52:47  sueh
 * <p> bug# 1422 Using ProcessingMethod to keep track of which type of
 * <p> processing method is in use.  The decisions about when to display the
 * <p> parallel processing table have been centralized in
 * <p> ProcessingMethodMediator.
 * <p>
 * <p> Revision 1.46  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.45  2010/07/02 03:13:56  sueh
 * <p> bug# 1388 Calling processchunks with popupChunkWarnings equals to true.
 * <p>
 * <p> Revision 1.44  2010/04/28 15:37:05  sueh
 * <p> bug# 1344 Added getFileSubdirectoryName.  Passing params to process
 * <p> manager functions, standardizing "3dmod is open" messages to always
 * <p> use closeImod.  Using ProcessSeries.Process to hold process information.
 * <p>
 * <p> Revision 1.43  2010/02/17 04:42:26  sueh
 * <p> bug# 1301 Moved comScriptMgr and logPanel from BaseManager to child
 * <p> class.
 * <p>
 * <p> Revision 1.42  2010/01/21 21:26:26  sueh
 * <p> bug# 1305 Passing FileType to imod instead of key and file.
 * <p>
 * <p> Revision 1.41  2009/10/27 20:39:05  sueh
 * <p> bug# 1275 Moving the resposibility for creating the log panel to the child
 * <p> classes.  That way the Front Page manager doesn't have to have a log
 * <p> panel.  Handling a null process manager.
 * <p>
 * <p> Revision 1.40  2009/10/23 22:22:24  sueh
 * <p> bug# 1275 Made touch() a start function in BaseProcessManager.
 * <p>
 * <p> Revision 1.39  2009/10/23 19:43:04  sueh
 * <p> bug# 1275 Separate generic parallel process and NAD.  Only use the
 * <p> openParallelChooser if there is no way to tell which dialog is requested.
 * <p>
 * <p> Revision 1.38  2009/09/05 00:39:22  sueh
 * <p> bug# 1256 Passed manager and axisID to state constructor.
 * <p>
 * <p> Revision 1.37  2009/09/01 03:17:35  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.36  2009/06/11 16:46:07  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Changed
 * <p> startNextProcess.
 * <p>
 * <p> Revision 1.35  2009/04/14 23:01:28  sueh
 * <p> bug# 1207  In reconnect:  handling some situations where process data is not running.
 * <p>
 * <p> Revision 1.34  2009/04/13 22:22:09  sueh
 * <p> bug# 1207 Implemented doAutomation in BaseManager.
 * <p>
 * <p> Revision 1.33  2009/03/17 00:24:18  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.32  2009/03/06 23:36:07  sueh
 * <p> bug# 1196 Added validateTestVolume.
 * <p>
 * <p> Revision 1.31  2009/03/02 18:56:04  sueh
 * <p> bug# 1193 Commented openProcessingPanel().
 * <p>
 * <p> Revision 1.30  2009/02/04 23:04:11  sueh
 * <p> bug# 1158 passing logPanel to mainPanel.setStatusBarText so its title can
 * <p> be updated.
 * <p>
 * <p> Revision 1.29  2008/08/26 20:35:26  sueh
 * <p> bug# 1122 Moved deleteSubdir from dialog to manager so that ImodManager can be queried about open 3dmods.
 * <p>
 * <p> Revision 1.28  2008/05/28 02:47:55  sueh
 * <p> bug# 1111 Removed processDialogTypeA and B from BaseManager.
 * <p> The dialogType for processes should be handled by ProcessSeries.
 * <p> Passing a DialogType parameter to startNextProcess.
 * <p>
 * <p> Revision 1.27  2008/05/13 20:59:03  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.26  2008/05/06 23:55:04  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 1.25  2008/05/03 00:33:48  sueh
 * <p> bug# 847 Passing ProcessSeries to the process manager,
 * <p> startNextProcess, and to all process functions.  To avoid having to decide
 * <p> which processes are next processes, pass it everywhere, even to
 * <p> processes that don't use ProcessResultDisplay.  The UI should not create
 * <p> any ProcessSeries and should pass them as null (since they don't know
 * <p> about processes).  Before adding to a process series, create it if it doesn't
 * <p> exist.  Before checking a process series, make sure it exists.  Since a
 * <p> series is only created when it doesn't exist and is needed, I don't have to
 * <p> keep track of which process comes first in a series.
 * <p>
 * <p> Revision 1.24  2008/01/14 20:22:48  sueh
 * <p> bug# 1050 Added an empty getProcessResultDisplayFactoryInterface.  Calling
 * <p> BaseManager.reconnect from openProcessingPanel to reconnect to parallel
 * <p> processes.
 * <p>
 * <p> Revision 1.23  2007/12/26 21:57:34  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.22  2007/12/10 21:51:10  sueh
 * <p> bug# 1041 Passing rootName to processchunks because it is now required for
 * <p> the ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.21  2007/11/12 14:50:59  sueh
 * <p> bug# 1047 Adding a swapYZ option to all the imods in the anisotropic diffusion
 * <p> interface.
 * <p>
 * <p> Revision 1.20  2007/11/09 17:17:21  sueh
 * <p> bug# 1047 Add the test volume to the imods that display varying k and varying
 * <p> iteration.
 * <p>
 * <p> Revision 1.19  2007/11/07 14:53:16  sueh
 * <p> bug# 1047 Fixed getBaseState.
 * <p>
 * <p> Revision 1.18  2007/11/06 18:57:48  sueh
 * <p> bug# 1047 Added anisotropic diffusion dialog.
 * <p>
 * <p> Revision 1.17  2007/09/27 19:21:11  sueh
 * <p> bug# 1044 Made ProcessorTable the ParallelProgress display instead of
 * <p> ParallelPanel.
 * <p>
 * <p> Revision 1.16  2007/09/07 00:16:30  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.
 * <p>
 * <p> Revision 1.15  2007/08/29 21:33:21  sueh
 * <p> bug# 1041 Made getBaseState public.
 * <p>
 * <p> Revision 1.14  2007/06/08 21:50:42  sueh
 * <p> bug# 1014 Removed setMetaData(ImodManager).
 * <p>
 * <p> Revision 1.13  2007/05/21 22:28:23  sueh
 * <p> bug# 964 Added getInterfaceType().
 * <p>
 * <p> Revision 1.12  2007/04/09 19:30:19  sueh
 * <p> bug# 964 Added setParamFile(), which just returns loadedParamFile
 * <p>
 * <p> Revision 1.11  2007/02/19 21:50:10  sueh
 * <p> bug# 964 Removed isNewManager() because it is only used by Application
 * <p> Manager.
 * <p>
 * <p> Revision 1.10  2007/02/05 21:27:59  sueh
 * <p> bug# 962 Creating process manager later.
 * <p>
 * <p> Revision 1.9  2006/11/28 22:48:48  sueh
 * <p> bug# 934 Changed BaseManager.stop() to endThreads().
 * <p>
 * <p> Revision 1.8  2006/11/15 18:49:00  sueh
 * <p> bug# 872 Changed getParamFileStorableArray to getStorables.  Letting the base
 * <p> save param file function call save().  getStorables always gets all the storables
 * <p> (including meta data) each time, to make it simpler.
 * <p>
 * <p> Revision 1.7  2006/10/16 22:36:32  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p> Revision 1.6  2006/09/13 23:07:23  sueh
 * <p> bug# 920 Moving BaseManager.createState() call to child classes, so it can be
 * <p> done after meta data is created.
 * <p>
 * <p> Revision 1.5  2006/07/28 19:43:45  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing.
 * <p>
 * <p> Revision 1.4  2006/07/26 16:33:55  sueh
 * <p> bug# 868 Temporarily moving part of processchunks to the specific manager.
 * <p> Eventually the process side of processchunks will be in UIExpert.
 * <p>
 * <p> Revision 1.3  2006/07/19 20:05:39  sueh
 * <p> bug# 902 Added processSucceeded().
 * <p>
 * <p> Revision 1.2  2006/06/05 16:01:30  sueh
 * <p> bug# 766 getParamFileStorableArray():  Add the option have elements in the storable array that aer set by the base manager.
 * <p>
 * <p> Revision 1.1  2006/03/20 17:50:19  sueh
 * <p> bug# 835 Manager (at the same level as ApplicationManager and
 * <p> JoinManager) to manage generic parallel processes.
 * <p> </p>
 */
