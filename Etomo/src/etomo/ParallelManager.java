package etomo;

import java.io.File;
import java.io.IOException;
import java.util.List;

import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TrimvolParam;
import etomo.process.BaseProcessManager;
import etomo.process.ParallelProcessManager;
import etomo.process.ProcessResultDisplayFactoryBlank;
import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.process.SystemProcessException;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogType;
import etomo.type.InterfaceType;
import etomo.type.ParallelMetaData;
import etomo.type.ParallelState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.AnisotropicDiffusionDialog;
import etomo.ui.MainPanel;
import etomo.ui.MainParallelPanel;
import etomo.ui.ParallelChooser;
import etomo.ui.ParallelDialog;
import etomo.ui.ParallelPanel;
import etomo.ui.UIHarness;

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
  private final ParallelState state = new ParallelState();
  private final ProcessResultDisplayFactoryBlank processResultDisplayFactory = new ProcessResultDisplayFactoryBlank();

  private final ParallelMetaData metaData;

  private ParallelDialog parallelDialog = null;
  private AnisotropicDiffusionDialog anisotropicDiffusionDialog = null;

  private MainParallelPanel mainPanel;
  private ParallelProcessManager processMgr;

  public ParallelManager() {
    this("");
  }

  public ParallelManager(final String paramFileName) {
    super();
    this.metaData = new ParallelMetaData();
    createState();
    processMgr = new ParallelProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData);
      if (paramFile == null) {
        openParallelChooser();
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

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      AxisID axisID) {
    return processResultDisplayFactory;
  }

  public void doAutomation() {
  }

  public ParallelState getState() {
    return state;
  }

  public boolean setParamFile() {
    return loadedParamFile;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.PP;
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public boolean canSnapshot() {
    return false;
  }

  protected void createComScriptManager() {
  }

  protected void processSucceeded(final AxisID axisID,
      final ProcessName processName) {
  }

  protected void createMainPanel() {
    mainPanel = new MainParallelPanel(this);
  }

  protected void createProcessTrack() {
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

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[3 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    storables[index++] = state;
    return storables;
  }

  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }

  protected BaseProcessTrack getProcessTrack() {
    return null;
  }

  protected void getProcessTrack(final Storable[] storable, final int index) {
  }

  public void kill(final AxisID axisID) {
    processMgr.kill(axisID);
  }

  public void pause(final AxisID axisID) {
    processMgr.pause(axisID);
  }

  public void save() throws LogFile.FileException, LogFile.WriteException {
    super.save();
    mainPanel.done();
    saveDialog();
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

  public void setParamFile(final File paramFile) {
    this.paramFile = paramFile;
  }

  void startNextProcess(final AxisID axisID, final String nextProcess,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries) {
    if (nextProcess.equals(ProcessName.ANISOTROPIC_DIFFUSION.toString())) {
      anisotropicDiffusion(processSeries);
    }
  }

  public void touch(final String absolutePath) {
    processMgr.touch(absolutePath);
  }

  public String getName() {
    return metaData.getName();
  }

  void updateDialog(final ProcessName processName, final AxisID axisID) {
  }

  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(AxisID.ONLY);
  }

  private void openParallelChooser() {
    ParallelChooser parallelChooser = ParallelChooser.getInstance(this);
    mainPanel.showProcess(parallelChooser.getContainer(), AXIS_ID);
  }

  public void openParallelDialog() {
    if (parallelDialog == null) {
      parallelDialog = ParallelDialog.getInstance(this, AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, parallelDialog
        .usingParallelProcessing());
    parallelDialog.setParameters(screenState);
    if (paramFile != null && metaData.isValid()) {
      parallelDialog.setParameters(metaData);
      parallelDialog.updateDisplay(false);
    }
    mainPanel.showProcess(parallelDialog.getContainer(), AXIS_ID);
  }

  public void openAnisotropicDiffusionDialog() {
    metaData.setDialogType(DialogType.ANISOTROPIC_DIFFUSION);
    if (anisotropicDiffusionDialog == null) {
      anisotropicDiffusionDialog = AnisotropicDiffusionDialog.getInstance(this,
          AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, anisotropicDiffusionDialog
        .usingParallelProcessing());
    if (paramFile != null && metaData.isValid()) {
      anisotropicDiffusionDialog.setParameters(metaData);
    }
    mainPanel.showProcess(anisotropicDiffusionDialog.getContainer(), AXIS_ID);
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
    saveStorables(AXIS_ID);
  }

  /**
   * set the paramFile and change the state, if necessary
   * run BaseManager.processchunks
   */
  public final void processchunks(
      final ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, String rootName) {
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
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        rootName);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    parallelDialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(AxisID.ONLY, param, processResultDisplay, processSeries);
  }

  public boolean setNewParamFile(final File file) {
    if (loadedParamFile) {
      return true;
    }
    //set paramFile and propertyUserDir
    propertyUserDir = file.getParentFile().getAbsolutePath();
    System.err.println("propertyUserDir: " + propertyUserDir);
    anisotropicDiffusionDialog.getInitialParameters(metaData);
    String errorMessage = metaData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Anisotropic Diffusion Dialog error", AXIS_ID);
      return false;
    }
    imodManager.setMetaData(metaData);
    setParamFile(new File(propertyUserDir, metaData.getMetaDataFileName()));
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData);
    return true;
  }

  public void imod(final String key, final File file,
      final Run3dmodMenuOptions menuOptions, final boolean flip) {
    if (file == null) {
      uiHarness.openMessageDialog("No file to open", "Entry Error");
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

  public void imodVaryingKValue(final String key,
      final Run3dmodMenuOptions menuOptions, final String subdirName,
      final String testVolumeName, final boolean flip) {
    List fileNameList = AnisotropicDiffusionParam.getTestFileNameList(this,
        state.getTestKValueList(), state.getTestIteration(), testVolumeName);
    imod(key, menuOptions, subdirName, fileNameList, flip);
  }

  public void imodVaryingIteration(final String key,
      final Run3dmodMenuOptions menuOptions, final String subdirName,
      final String testVolumeName, final boolean flip) {
    List fileNameList = AnisotropicDiffusionParam.getTestFileNameList(this,
        state.getTestKValue(), state.getTestIterationList(), testVolumeName);
    imod(key, menuOptions, subdirName, fileNameList, flip);
  }

  private void imod(String key, Run3dmodMenuOptions menuOptions,
      String subdirName, List fileNameList, final boolean flip) {
    String[] fileNameArray;
    if (fileNameList.size() == 0) {
      fileNameArray = new String[0];
    }
    else if (fileNameList.size() == 1) {
      fileNameArray = new String[1];
      fileNameArray[0] = (String) fileNameList.get(0);
    }
    else {
      fileNameArray = (String[]) fileNameList.toArray(new String[fileNameList
          .size()]);
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

  public void makeSubdir(final String subdirName) {
    if (paramFile == null) {
      uiHarness.openMessageDialog("Must pick a volume.", "Entry Error");
      return;
    }
    File subdir = new File(propertyUserDir, subdirName);
    if (!subdir.exists()) {
      subdir.mkdir();
    }
  }

  private TrimvolParam updateTrimvolParam() {
    // Get trimvol param data from dialog.
    TrimvolParam param = new TrimvolParam(this);
    anisotropicDiffusionDialog.getParameters(param);
    return param;
  }

  private AnisotropicDiffusionParam updateAnisotropicDiffusionParamForVaryingK(
      final String subdirName) throws LogFile.WriteException,
      LogFile.FileException {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this);
    if (!anisotropicDiffusionDialog.getParametersForVaryingK(param)) {
      return null;
    }
    param.deleteTestFiles();
    param.createTestFiles();
    return param;
  }

  private void updateAnisotropicDiffusionParam() throws LogFile.FileException,
      LogFile.WriteException {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this);
    anisotropicDiffusionDialog.getParameters(param);
    param.createFilterFullFile();
  }

  private ChunksetupParam updateChunksetupParam() {
    ChunksetupParam param = new ChunksetupParam();
    anisotropicDiffusionDialog.getParameters(param);
    return param;
  }

  public boolean setupAnisotropicDiffusion() {
    try {
      updateAnisotropicDiffusionParam();
    }
    catch (LogFile.WriteException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return false;
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return false;
    }
    return true;
  }

  private AnisotropicDiffusionParam updateAnisotropicDiffusionParamForVaryingIteration(
      final String subdirName) {
    AnisotropicDiffusionParam param = new AnisotropicDiffusionParam(this);
    if (!anisotropicDiffusionDialog.getParametersForVaryingIteration(param)) {
      return null;
    }
    return param;
  }

  public void chunksetup(ProcessSeries processSeries) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this);
    }
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog("Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    ChunksetupParam param = updateChunksetupParam();
    if (param == null) {
      return;
    }
    processSeries.setNextProcess(ProcessName.ANISOTROPIC_DIFFUSION.toString());
    String threadName;
    try {
      threadName = processMgr.chunksetup(param, processSeries);
    }
    catch (SystemProcessException e) {
      String[] message = new String[2];
      message[0] = "Can not execute" + ProcessName.CHUNKSETUP + "command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar(ProcessName.CHUNKSETUP.toString(), AxisID.ONLY,
        ProcessName.CHUNKSETUP);
  }

  public void anisotropicDiffusionVaryingIteration(final String subdirName,
      ConstProcessSeries processSeries) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog("Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    AnisotropicDiffusionParam param = updateAnisotropicDiffusionParamForVaryingIteration(subdirName);
    if (param == null) {
      return;
    }
    // Start the trimvol process
    String threadName;
    try {
      threadName = processMgr.anisotropicDiffusion(param, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute" + ProcessName.ANISOTROPIC_DIFFUSION
          + "command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar(ProcessName.ANISOTROPIC_DIFFUSION.toString(),
        AxisID.ONLY, ProcessName.ANISOTROPIC_DIFFUSION);
  }

  public void anisotropicDiffusion(ConstProcessSeries processSeries) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog("Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    anisotropicDiffusionDialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(AxisID.ONLY, param, null, processSeries);
  }

  public void anisotropicDiffusionVaryingK(final String subdirName,
      ConstProcessSeries processSeries) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog("Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    AnisotropicDiffusionParam anisotropicDiffusionParam;
    try {
      anisotropicDiffusionParam = updateAnisotropicDiffusionParamForVaryingK(subdirName);
      if (anisotropicDiffusionParam == null) {
        return;
      }
    }
    catch (LogFile.WriteException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return;
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          "Anisotropic diffusion comscripts could not be created",
          "Write Comscript Error", AxisID.ONLY);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        ProcessName.ANISOTROPIC_DIFFUSION);
    param.setSubcommandDetails(anisotropicDiffusionParam);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    anisotropicDiffusionDialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(AxisID.ONLY, param, null, processSeries);
  }

  /**
   * Execute trimvol
   */
  public void trimVolume(ConstProcessSeries processSeries) {
    if (anisotropicDiffusionDialog == null) {
      uiHarness.openMessageDialog("Anisotropic diffusion dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    TrimvolParam param = updateTrimvolParam();
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
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Trimming volume", AxisID.ONLY,
        ProcessName.TRIMVOL);
  }

  private boolean setNewParamFile() {
    if (loadedParamFile) {
      return true;
    }
    //set paramFile and propertyUserDir
    propertyUserDir = parallelDialog.getWorkingDir().getAbsolutePath();
    System.err.println("propertyUserDir: " + propertyUserDir);
    parallelDialog.getParameters(metaData);
    String errorMessage = metaData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Anisotropic Diffusion Error", AXIS_ID);
      return false;
    }
    setParamFile(new File(propertyUserDir, metaData.getMetaDataFileName()));
    System.err.println("paramFile: " + paramFile);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData);
    return true;
  }

  private void saveDialog() {
    saveParallelDialog();
    saveAnisotropicDiffusionDialog();
  }
}
/**
 * <p> $Log$
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
