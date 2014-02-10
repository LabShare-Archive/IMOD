package etomo.ui.swing;

import java.awt.Container;
import java.io.File;

import etomo.ApplicationManager;
import etomo.Arguments;
import etomo.EtomoDirector;
import etomo.logic.DatasetTool;
import etomo.logic.UserEnv;
import etomo.storage.DirectiveFileCollection;
import etomo.storage.EtomoFileFilter;
import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TiltAngleSpec;
import etomo.type.UserConfiguration;
import etomo.type.ViewType;
import etomo.ui.SetupReconInterface;
import etomo.ui.SetupReconUIHarness;
import etomo.util.SharedConstants;

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
 * <p> Revision 1.3  2011/02/24 23:37:49  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.2  2011/02/22 19:29:29  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.18  2010/09/08 19:20:13  sueh
 * <p> bug# 1401 In doAutomation handle dir equals ".".
 * <p>
 * <p> Revision 1.17  2010/04/10 05:32:14  sueh
 * <p> bug# 1349 Pressing Scan Header button without a dataset name pops up
 * <p> too many error messages.
 * <p>
 * <p> Revision 1.16  2010/04/08 18:04:01  sueh
 * <p> bug# 1348 In scanHeaderAction, getting imageRotation as an
 * <p> EtomoNumber.
 * <p>
 * <p> Revision 1.15  2010/03/27 05:09:30  sueh
 * <p> bug# 1333 Added GPU checkbox.
 * <p>
 * <p> Revision 1.14  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.13  2010/01/11 23:59:01  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.12  2009/10/13 17:45:07  sueh
 * <p> bug# 1278 Changed MetaData.setExcludeProjectionsA and B to
 * <p> setExcludeProjections.
 * <p>
 * <p> Revision 1.11  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.10  2009/08/24 20:23:54  sueh
 * <p> bug# 1254 Made setViewType package private.
 * <p>
 * <p> Revision 1.9  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.8  2009/02/13 02:36:47  sueh
 * <p> bug# 1152 Adding frame validation to isValid.  Factoring MRC header
 * <p> reading functionality to readMRCHeader and getStackFileName.
 * <p>
 * <p> Revision 1.7  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.6  2008/12/02 21:24:46  sueh
 * <p> bug# 1157 Simplify rounding method used with pixel.
 * <p>
 * <p> Revision 1.5  2008/12/01 22:35:05  sueh
 * <p> bug# 1131 Setting montage from userConfiguration.
 * <p>
 * <p> Revision 1.4  2008/07/19 01:12:15  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.3  2008/01/31 20:31:12  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.2  2008/01/25 22:29:24  sueh
 * <p> bug# 1070 Don't use parallel processing unless the cpu.adoc or
 * <p> IMOD_PROCESSORS has been set by the user.
 * <p>
 * <p> Revision 1.1  2007/12/26 22:31:39  sueh
 * <p> bug# 1052 Turned SetupDialog into an extremely thin GUI.  Moved decisions and
 * <p> knowledge to SetupDialogExpert.  Added doAutomation() to handle user
 * <p> specified automated functionality to be done before control is handed over to the
 * <p> user.
 * <p> </p>
 */
public final class SetupDialogExpert {
  public static final String rcsid = "$Id$";

  private final TiltAnglePanelExpert tiltAnglePanelExpertA;
  private final TiltAnglePanelExpert tiltAnglePanelExpertB;
  private final SetupDialog dialog;
  private final ApplicationManager manager;
  private final SetupReconUIHarness setupUIHarness;

  private File dir = null;

  private SetupDialogExpert(final ApplicationManager manager,
      final SetupReconUIHarness setupUIHarness, final boolean calibrationAvailable) {
    this.manager = manager;
    this.setupUIHarness = setupUIHarness;
    tiltAnglePanelExpertA = new TiltAnglePanelExpert(manager, AxisID.FIRST);
    tiltAnglePanelExpertB = new TiltAnglePanelExpert(manager, AxisID.SECOND);
    dialog = SetupDialog.getInstance(this, manager, AxisID.ONLY, DialogType.SETUP_RECON,
        calibrationAvailable);
  }

  public static SetupDialogExpert getInstance(ApplicationManager manager,
      final SetupReconUIHarness setupUIHarness, boolean calibrationAvailable) {
    SetupDialogExpert instance = new SetupDialogExpert(manager, setupUIHarness,
        calibrationAvailable);
    instance.setTooltips();
    return instance;
  }

  /**
   * Process command line arguments that pertain to Setup Dialog.  May can
   * functions in ApplicationManager.
   * @return true the automation does not generate errors
   */
  public void doAutomation() {
    Arguments arguments = EtomoDirector.INSTANCE.getArguments();
    // build and set dataset
    dir = arguments.getDir();
    String dataset = arguments.getDataset();
    if (dataset != null) {
      // If the directory was set and dataset is a file (not the dataset name), pass the
      // absolute path to the dialog.
      if (dir != null && dataset.endsWith(FileType.RAW_STACK.getExtension(manager))) {
        dialog.setDataset(new File(dir, dataset).getAbsolutePath());
      }
      else {
        dialog.setDataset(arguments.getDataset());
      }
    }
    // check radio buttons
    AxisType axis = arguments.getAxis();
    if (axis == AxisType.SINGLE_AXIS) {
      dialog.setSingleAxis(true);
    }
    else if (axis == AxisType.DUAL_AXIS) {
      dialog.setDualAxis(true);
    }
    ViewType frame = arguments.getFrame();
    if (frame != null) {
      setViewType(frame);
    }
    // fill in fields
    ConstEtomoNumber fiducial = arguments.getFiducial();
    if (!fiducial.isNull()) {
      dialog.setFiducialDiameter(fiducial.getDouble());
    }
    // scan header
    if (arguments.isScan()) {
      setupUIHarness.scanHeaderAction(dialog);
    }
    if (arguments.isCpus()) {
      dialog.setParallelProcess(true);
    }
    if (arguments.isGpus()) {
      dialog.setGpuProcessing(true);
    }
    // complete the dialog
    if (arguments.isCreate()) {
      dialog.buttonExecuteAction();
    }
  }

  public DirectiveFileCollection getDirectiveFileCollection() {
    return dialog.getDirectiveFileCollection();
  }

  public File getDir() {
    return dir;
  }

  public String getDataset() {
    return dialog.getDataset();
  }

  public SetupReconInterface getSetupReconInterface() {
    return dialog;
  }

  /**
   * Checks for an existing reconstruction on a different stack in the current
   * directory. Assumes that the new .edf file for this instance has not been
   * created yet. Since .com file names are not stack specific, it is necessary
   * to prevent interference by doing only one reconstruction per directory. A
   * secondary goal is to have only one tilt series per directory. Multiple .edf
   * files accessing the same stacks are allowed so that the user can back up
   * their .edf file or start a fresh .edf file. The user may also have one
   * single and one dual reconstruction in a directory, as long as they have a
   * stack in common.
   * @return True if there is already an .edf file in the propertyUserDir and it
   *         is referencing a stack other then the one(s) specified in the setup
   *         dialog. True if the new .edf file and the existing .edf file are
   *         both single axis, even if one file is accessing the A stack and the
   *         other is accessing the B stack. False if no existing .edf file is
   *         found. False if the new .edf file and the existing .edf file are
   *         single and dual axis, as long as they have a stack in common. False
   *         if there is a conflict, but the stacks that one of .edf files
   *         references don't exist.
   */
  public boolean checkForSharedDirectory() {
    String propertyUserDir = getPropertyUserDir();
    File directory = new File(propertyUserDir);
    // Get all the edf files in propertyUserDir.
    File[] edfFiles = directory.listFiles(new EtomoFileFilter());
    if (edfFiles == null) {
      return false;
    }
    BaseMetaData metaData = manager.getBaseMetaData();
    String datasetName = metaData.getDatasetName();
    AxisType axisType = metaData.getAxisType();
    String extension = metaData.getFileExtension();
    File firstStack = null;
    File secondStack = null;
    String firstStackName = null;
    String secondStackName = null;
    // Create File instances based on the stacks specified in the setup dialog
    if (axisType == AxisType.DUAL_AXIS) {
      firstStack = new File(propertyUserDir, datasetName + AxisID.FIRST.getExtension()
          + DatasetTool.STANDARD_DATASET_EXT);
      firstStackName = firstStack.getName();
      secondStack = new File(propertyUserDir, datasetName + AxisID.SECOND.getExtension()
          + DatasetTool.STANDARD_DATASET_EXT);
      secondStackName = secondStack.getName();
    }
    else if (axisType == AxisType.SINGLE_AXIS) {
      firstStack = new File(propertyUserDir, datasetName + AxisID.ONLY.getExtension()
          + DatasetTool.STANDARD_DATASET_EXT);
      firstStackName = firstStack.getName();
    }
    // open any .edf files in propertyUserDir - assuming the .edf file for this
    // instance hasn't been created yet.
    // If there is at least one .edf file that references existing stacks that
    // are not the stacks the will be used in this instance, then the directory
    // is already in use.
    // Doing a dual and single axis on the same stack is not sharing a
    // directory.
    // However doing two single axis reconstructions on the same tilt series,
    // where one is done on A and the other is done on B, would be considered
    // sharing a directory.
    for (int i = 0; i < edfFiles.length; i++) {
      MetaData savedMetaData = new MetaData(manager, manager.getLogProperties());
      try {
        ParameterStore paramStore = ParameterStore.getInstance(edfFiles[i]);
        if (paramStore != null) {
          paramStore.load(savedMetaData);
        }
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read .edf files in "
            + propertyUserDir, "Etomo Error");
        continue;
      }
      // Create File instances based on the stacks specified in the edf file
      // found in propertyUserDir.
      AxisType savedAxisType = savedMetaData.getAxisType();
      String savedDatasetName = savedMetaData.getDatasetName();
      File savedFirstStack;
      File savedSecondStack;
      String savedFirstStackName;
      String savedSecondStackName;
      if (savedAxisType == AxisType.DUAL_AXIS) {
        savedFirstStack = new File(propertyUserDir, savedDatasetName
            + AxisID.FIRST.getExtension() + DatasetTool.STANDARD_DATASET_EXT);
        savedFirstStackName = savedFirstStack.getName();
        savedSecondStack = new File(propertyUserDir, savedDatasetName
            + AxisID.SECOND.getExtension() + DatasetTool.STANDARD_DATASET_EXT);
        savedSecondStackName = savedSecondStack.getName();
        if (axisType == AxisType.DUAL_AXIS) {
          // compare dual axis A against saved dual axis A
          if (savedFirstStack.exists() && !firstStackName.equals(savedFirstStackName)) {
            return true;
          }
          // compare dual axis B against saved dual axis B
          if (savedSecondStack.exists() && !secondStackName.equals(savedSecondStackName)) {
            return true;
          }
        }
        else if (axisType == AxisType.SINGLE_AXIS) {
          // compare single axis against saved dual axis A
          // compare single axis against saved dual axis B
          if (savedFirstStack.exists()
              && !firstStackName.equals(savedFirstStackName)
              && (!savedSecondStack.exists() || (savedSecondStack.exists() && !firstStackName
                  .equals(savedSecondStackName)))) {
            return true;
          }
        }
      }
      else if (savedAxisType == AxisType.SINGLE_AXIS) {
        savedFirstStack = new File(propertyUserDir, savedDatasetName
            + AxisID.ONLY.getExtension() + DatasetTool.STANDARD_DATASET_EXT);
        savedFirstStackName = savedFirstStack.getName();
        if (axisType == AxisType.DUAL_AXIS) {
          // compare dual axis A against saved single axis
          // compare dual axis B against saved single axis
          if (savedFirstStack.exists() && !firstStackName.equals(savedFirstStackName)
              && !secondStackName.equals(savedFirstStackName)) {
            return true;
          }
        }
        else if (axisType == AxisType.SINGLE_AXIS) {
          // compare single axis against saved single axis
          if (savedFirstStack.exists() && !firstStackName.equals(savedFirstStackName)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  String getPropertyUserDir() {
    return setupUIHarness.getPropertyUserDir();
  }

  public DialogExitState getExitState() {
    return dialog.getExitState();
  }

  /**
   * Return the working directory as a File object.
   * @return
   */
  public File getWorkingDirectory() {
    String datasetText = dialog.getDataset();
    File dataset = new File(datasetText);
    if (!dataset.isAbsolute()) {

      dataset = new File(getPropertyUserDir() + File.separator + datasetText);
    }
    return dataset.getParentFile();
  }

  public void initializeFields(ConstMetaData metaData, UserConfiguration userConfig) {
    if (!metaData.getDatasetName().equals("")) {
      String canonicalPath = getPropertyUserDir() + "/" + metaData.getDatasetName();
      dialog.setDataset(canonicalPath);
    }
    // Parallel processing is optional in tomogram reconstruction, so only use it
    // if the user set it up.
    dialog.setParallelProcess(UserEnv.isParallelProcessing(manager, AxisID.ONLY,
        getPropertyUserDir()));
    dialog.setParallelProcessEnabled(UserEnv.isParallelProcessingEnabled(manager,
        AxisID.ONLY, getPropertyUserDir()));
    dialog.setGpuProcessingEnabled(UserEnv.isGpuProcessingEnabled(manager, AxisID.ONLY,
        getPropertyUserDir()));
    dialog.setGpuProcessing(UserEnv.isGpuProcessing(manager, AxisID.ONLY,
        getPropertyUserDir()));
    dialog.setBackupDirectory(metaData.getBackupDirectory());
    dialog.setDistortionFile(metaData.getDistortionFile());
    dialog.setMagGradientFile(metaData.getMagGradientFile());
    dialog.setAdjustedFocus(AxisID.FIRST, metaData.getAdjustedFocusA().is());
    dialog.setAdjustedFocus(AxisID.SECOND, metaData.getAdjustedFocusB().is());
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS || userConfig.getSingleAxis()) {
      dialog.setSingleAxis(true);
    }
    else {
      dialog.setDualAxis(true);
    }
    if (userConfig.getMontage()) {
      setViewType(ViewType.MONTAGE);
    }
    else {
      setViewType(metaData.getViewType());
    }
    if (!Double.isNaN(metaData.getPixelSize())) {
      dialog.setPixelSize(metaData.getPixelSize());
    }
    if (!Double.isNaN(metaData.getFiducialDiameter())) {
      dialog.setFiducialDiameter(metaData.getFiducialDiameter());
    }
    if (!metaData.getImageRotation(AxisID.ONLY).isNull()) {
      dialog.setImageRotation(metaData.getImageRotation(AxisID.ONLY).toString());
    }
    dialog.setBinning(metaData.getBinning());

    tiltAnglePanelExpertA.setFields(metaData.getTiltAngleSpecA(), userConfig);
    dialog.setExcludeList(AxisID.FIRST, metaData.getExcludeProjectionsA());
    tiltAnglePanelExpertB.setFields(metaData.getTiltAngleSpecB(), userConfig);
    dialog.setExcludeList(AxisID.SECOND, metaData.getExcludeProjectionsB());
    dialog.setTwodir(AxisID.FIRST, metaData.isTwodir(AxisID.FIRST));
    dialog.setTwodir(AxisID.SECOND, metaData.isTwodir(AxisID.SECOND));
    dialog.setTwodir(AxisID.FIRST, metaData.getTwodir(AxisID.FIRST));
    dialog.setTwodir(AxisID.SECOND, metaData.getTwodir(AxisID.SECOND));
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      tiltAnglePanelExpertB.setEnabled(false);
      dialog.setExcludeListEnabled(AxisID.SECOND, false);
      dialog.setTwodirEnabled(AxisID.SECOND, false);
      dialog.setViewRawStackEnabled(AxisID.SECOND, false);
    }
    dialog.setParameters(userConfig);
    dialog.checkpoint();
    tiltAnglePanelExpertA.checkpoint();
    tiltAnglePanelExpertB.checkpoint();
  }

  boolean validateTiltAngle(final AxisID axisID, final String errorTitle) {
    return getTiltAnglesPanelExpert(axisID).validate(errorTitle);
  }

  TiltAnglePanelExpert getTiltAnglesPanelExpert(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tiltAnglePanelExpertB;
    }
    return tiltAnglePanelExpertA;
  }

  public Container getContainer() {
    return dialog.getContainer();
  }

  AxisType getAxisType() {
    return setupUIHarness.getAxisType();
  }

  String getDatasetDir() {
    if (dir != null) {
      return dir.getAbsolutePath();
    }
    return EtomoDirector.INSTANCE.getOriginalUserDir();
  }

  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    if (axisID == AxisID.SECOND) {
      return tiltAnglePanelExpertB.getFields(tiltAngleSpec, doValidation);
    }
    return tiltAnglePanelExpertA.getFields(tiltAngleSpec, doValidation);
  }

  String getCurrentBackupDirectory() {
    // Open up the file chooser in the working directory
    String currentBackupDirectory = dialog.getBackupDirectory();
    if (currentBackupDirectory.equals("")) {
      currentBackupDirectory = EtomoDirector.INSTANCE.getOriginalUserDir();
    }
    return currentBackupDirectory;
  }

  String getCurrentMagGradientDir() {
    // Open up the file chooser in the calibration directory, if available,
    // otherwise open in the working directory
    String currentMagGradientDir = dialog.getMagGradientFile();
    if (currentMagGradientDir.equals("")) {
      File calibrationDir = EtomoDirector.INSTANCE.getIMODCalibDirectory();
      File magGradientDir = new File(calibrationDir.getAbsolutePath(), "Distortion");
      if (magGradientDir.exists()) {
        currentMagGradientDir = magGradientDir.getAbsolutePath();
      }
      else {
        currentMagGradientDir = getPropertyUserDir();
      }
    }
    return currentMagGradientDir;
  }

  void action(final String actionCommand) {
    if (dialog.equalsSingleAxisActionCommand(actionCommand)) {
      tiltAnglePanelExpertB.setEnabled(false);
      dialog.setExcludeListEnabled(AxisID.SECOND, false);
      dialog.setTwodirEnabled(AxisID.SECOND, false);
      dialog.setViewRawStackEnabled(AxisID.SECOND, false);
    }
    else if (dialog.equalsDualAxisActionCommand(actionCommand)) {
      tiltAnglePanelExpertB.setEnabled(true);
      dialog.setExcludeListEnabled(AxisID.SECOND, true);
      dialog.setTwodirEnabled(AxisID.SECOND, true);
      dialog.setViewRawStackEnabled(AxisID.SECOND, true);
    }
    else if (dialog.equalsSingleViewActionCommand(actionCommand)) {
      dialog.setAdjustedFocusEnabled(AxisID.FIRST, false);
      dialog.setAdjustedFocusEnabled(AxisID.SECOND, false);
    }
    else if (dialog.equalsMontageActionCommand(actionCommand)) {
      dialog.setAdjustedFocusEnabled(AxisID.FIRST, true);
      dialog.setAdjustedFocusEnabled(AxisID.SECOND, true);
    }
    else if (dialog.equalsScanHeaderActionCommand(actionCommand)) {
      setupUIHarness.scanHeaderAction(dialog);
    }
    else if (dialog.equalsTemplateActionCommand(actionCommand)) {
      dialog.updateTemplateValues();
    }
  }

  void updateTiltAnglePanelTemplateValues(
      final DirectiveFileCollection directiveFileCollection) {
    tiltAnglePanelExpertA.updateTemplateValues(directiveFileCollection);
    tiltAnglePanelExpertB.updateTemplateValues(directiveFileCollection);
  }

  void viewRawStack(AxisID axisID, final Run3dmodMenuOptions menuOptions) {
    if (axisID == AxisID.SECOND) {
      manager.imodPreview(AxisID.SECOND, menuOptions);
    }
    else {
      if (getAxisType() == AxisType.SINGLE_AXIS) {
        manager.imodPreview(AxisID.ONLY, menuOptions);
      }
      else {
        manager.imodPreview(AxisID.FIRST, menuOptions);
      }
    }
  }

  private void setTooltips() {
    dialog.setDatasetTooltip(
        "Enter the name of view data file(s). You can also select the view "
            + "data file by pressing the folder button.",
        "This button will open a file chooser dialog box allowing you to "
            + "select the view data file.");
    dialog.setBackupDirectoryTooltip(
        "Enter the name of the directory where you want the small data "
            + "files .com and .log files to be backed up.  You can use the "
            + "folder button on the right to create a new directory to "
            + "store the backups.",
        "This button will open a file chooser dialog box allowing you to "
            + "select and/or create the backup directory.");
    dialog.setScanHeaderTooltip("Attempt to extract pixel size and tilt axis "
        + "rotation angle from data stack.");
    dialog.setAxisTypeTooltip("This radio button selector will choose whether the "
        + "data consists of one or two tilt axis.");
    dialog.setViewTypeTooltip(SharedConstants.VIEW_TYPE_TOOLTIP);

    dialog
        .setPixelSizeTooltip("Enter the view image pixel size in nanometers " + "here.");
    dialog.setFiducialDiameterTooltip("Enter the fiducial size in nanometers here.");
    dialog.setImageRotationTooltip("Enter the view image rotation in degrees. This "
        + "is the rotation (CCW positive) from the Y-axis (the tilt axis "
        + "after the views are aligned) to the suspected tilt axis in the "
        + "unaligned views.");
    dialog.setDistortionFileTooltip(SharedConstants.DISTORTION_FIELD_TOOLTIP);
    dialog.setBinningTooltip(SharedConstants.IMAGES_ARE_BINNED_TOOLTIP);

    tiltAnglePanelExpertA.setTooltips();
    tiltAnglePanelExpertB.setTooltips();

    dialog.setExcludeListTooltip("Enter the view images to <b>exclude</b> from the "
        + "processing of this axis.  Ranges are allowed, separate ranges by "
        + "commas.  For example to exclude the first four and last four "
        + "images of a 60 view stack enter 1-4,57-60.");
    dialog.setTwodirTooltip("Tilt series was acquired in two directions from the given "
        + "starting angle; the break in the series is assumed to be after the first "
        + "image in the stack at that angle.");
    dialog.setPostponeTooltip("This button will setup the processing for existing "
        + "command scripts.  <b>Be sure that parameters entered match the "
        + "existing command scripts.");
    dialog.setExecuteTooltip("This button will create a new set of command scripts "
        + "overwriting any of the same name in the specified working "
        + "directory.  Be sure to save the data file after creating the "
        + "command script if you wish to keep the results.");
    dialog.setParallelProcessTooltip("Sets the default for parallel processing "
        + "(distributing processes across multiple computers).");
    dialog.setGpuProcessingTooltip("Sets the default for GPU processing (sending "
        + "processes to the graphics card).");

    dialog.setMagGradientFileTooltip("OPTIONAL:  A file with magnification "
        + "gradients to be applied for each image.");
    dialog.setViewRawStackTooltip("View the current raw image stack.");
    dialog.setAdjustedFocusTooltip("Set this if \"Change focus with height\" was "
        + "selected when the montage was acquired in SerialEM.");
  }

  // View type radio button
  void setViewType(ViewType viewType) {
    if (viewType == ViewType.SINGLE_VIEW) {
      dialog.setSingleView(true);
    }
    else {
      dialog.setMontage(true);
    }
  }
}
