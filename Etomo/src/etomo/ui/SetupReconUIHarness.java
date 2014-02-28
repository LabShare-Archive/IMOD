package etomo.ui;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.Arguments;
import etomo.EtomoDirector;
import etomo.comscript.FortranInputSyntaxException;
import etomo.logic.DatasetTool;
import etomo.logic.SeedingMethod;
import etomo.logic.TrackingMethod;
import etomo.storage.DirectiveFile;
import etomo.storage.DirectiveFileCollection;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DataFileType;
import etomo.type.DialogExitState;
import etomo.type.DirectiveFileType;
import etomo.type.MetaData;
import etomo.type.UserConfiguration;
import etomo.type.ViewType;
import etomo.ui.swing.SetupDialogExpert;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

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
public final class SetupReconUIHarness {
  public static final String rcsid = "$Id:$";

  private static final String NO_GUI_ERROR_TITLE = "No GUI";
  private static final String NO_GUI_ERROR_MESSAGE = "GUI not found.  To run automation "
      + "without the GUI, use the " + Arguments.DIRECTIVE_TAG + " option.";

  private final ApplicationManager manager;
  private final AxisID axisID;

  private SetupDialogExpert expert = null;
  private DirectiveFileCollection directiveFileCollection = null;
  private boolean setFEIPixelSize = false;

  public SetupReconUIHarness(final ApplicationManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  /**
   * Runs the doAutomation function in the dialog expert, or handles an automation
   * directive file.
   */
  public void doAutomation() {
    if (!EtomoDirector.INSTANCE.getArguments().isDirective()) {
      if (expert != null) {
        expert.doAutomation();
      }
      else {
        UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
            NO_GUI_ERROR_TITLE);
      }
      return;
    }
    directiveFileCollection = new DirectiveFileCollection(manager, axisID);
    DirectiveFile batchDirectiveFile = DirectiveFile.getInstance(manager, axisID);
    directiveFileCollection.setBatchDirectiveFile(batchDirectiveFile);
    if (!doDirectiveAutomation()) {
      UIHarness.INSTANCE.exit(axisID, 1);
    }
    else {
      UIHarness.INSTANCE.exit(axisID, 0);
    }
  }

  private boolean doDirectiveAutomation() {
    if (directiveFileCollection == null) {
      return false;
    }
    AxisType axisType = AxisType.SINGLE_AXIS;
    if (directiveFileCollection.isDual()) {
      axisType = AxisType.DUAL_AXIS;
    }
    if (!DatasetTool.validateDatasetName(manager, null, axisID, new File(
        getPropertyUserDir()), directiveFileCollection.getName(), DataFileType.RECON,
        axisType, true)) {
      return false;
    }
    if (directiveFileCollection.isScanHeader()) {
      if (!scanHeaderAction(directiveFileCollection)) {
        return false;
      }
    }
    if (manager.doneSetupDialog(true)) {
      return true;
    }
    return false;
  }

  /**
   * Called by the manager when not headless.
   * @return
   */
  public SetupDialogExpert getSetupDialogExpert() {
    if (EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
          NO_GUI_ERROR_TITLE);
      return null;
    }
    if (expert == null) {
      File distortionDir = DatasetFiles.getDistortionDir(manager,
          manager.getPropertyUserDir(), axisID);
      expert = SetupDialogExpert.getInstance(manager, this, distortionDir != null
          && distortionDir.exists());
    }
    return expert;
  }

  public void freeDialog() {
    expert = null;
  }

  private SetupReconInterface getSetupReconInterface() {
    if (directiveFileCollection != null) {
      return directiveFileCollection;
    }
    if (expert != null) {
      return expert.getSetupReconInterface();
    }
    UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
        NO_GUI_ERROR_TITLE);
    return null;
  }

  public DialogExitState getExitState() {
    if (directiveFileCollection != null) {
      return DialogExitState.EXECUTE;
    }
    if (expert != null) {
      return expert.getExitState();
    }
    UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
        NO_GUI_ERROR_TITLE);
    return null;
  }

  /**
   * This is functionality is mostly duplicated by the validate dataset functions in the
   * logic package.  Not worth duplicating for headless automation.
   * @return
   */
  public boolean checkForSharedDirectory() {
    if (expert != null) {
      return expert.checkForSharedDirectory();
    }
    return false;
  }

  public File getWorkingDirectory() {
    if (directiveFileCollection != null) {
      return new File(getPropertyUserDir());
    }
    if (expert != null) {
      return expert.getWorkingDirectory();
    }
    UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
        NO_GUI_ERROR_TITLE);
    return null;
  }

  public MetaData getMetaData() {
    SetupReconInterface setupInterface = getSetupReconInterface();
    if (setupInterface == null) {
      return null;
    }
    MetaData metaData = new MetaData(manager, manager.getLogProperties());
    metaData.setAxisType(getAxisType());
    // The dataset name needs to be set after the axis type so the metadata object
    // modifies the file ending correctly (if a file name is used).
    metaData.setDatasetName(setupInterface.getDataset());
    return metaData;
  }

  public AxisType getAxisType() {
    SetupReconInterface setupInterface = getSetupReconInterface();
    if (setupInterface == null) {
      return null;
    }
    if (setupInterface.isSingleAxisSelected()) {
      return AxisType.SINGLE_AXIS;
    }
    else {
      return AxisType.DUAL_AXIS;
    }
  }

  private ViewType getViewType(final SetupReconInterface setupInterface) {
    if (setupInterface.isSingleViewSelected()) {
      return ViewType.SINGLE_VIEW;
    }
    else {
      return ViewType.MONTAGE;
    }
  }

  /**
   * Get the directory in which the user wants to create the dataset.
   * @return
   */
  public String getPropertyUserDir() {
    if (directiveFileCollection != null
        && directiveFileCollection.containsDatasetDirectory()) {
      return directiveFileCollection.getDatasetDirectory();
    }
    else if (expert != null) {
      File dir = expert.getDir();
      if (dir != null) {
        return dir.getAbsolutePath();
      }
    }
    return manager.getPropertyUserDir();
  }

  public boolean scanHeaderAction(final SetupReconInterface setupInterface) {
    MRCHeader header = readMRCHeader(getStackFileName(setupInterface));
    if (header == null) {
      return false;
    }
    // Set the image rotation if available
    ConstEtomoNumber imageRotation = header.getImageRotation();
    if (!imageRotation.isNull()) {
      setupInterface.setImageRotation(imageRotation.toString());
    }
    // set the pixel size if available
    double xPixelSize = header.getXPixelSize().getDouble();
    double yPixelSize = header.getYPixelSize().getDouble();
    if (Double.isNaN(xPixelSize) || Double.isNaN(yPixelSize)) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Pixel size is not defined in the image file header", "Pixel size is missing",
          AxisID.ONLY);
      return false;
    }
    if (xPixelSize != yPixelSize) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "X & Y pixels sizes are different, don't know what to do",
          "Pixel sizes are different", AxisID.ONLY);
      return false;
    }
    if (xPixelSize == 1.0) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Pixel size is not defined in the image file header", "Pixel size is missing",
          AxisID.ONLY);
      return false;
    }
    xPixelSize = xPixelSize / 10.0;
    setupInterface.setPixelSize(Math.round(xPixelSize * 1000000.0) / 1000000.0);
    // set binning
    int binning = header.getBinning();
    if (binning == Integer.MIN_VALUE) {
      binning = 1;
    }
    setupInterface.setBinning(binning);
    ConstEtomoNumber twodir = header.getTwodir();
    if (!twodir.isNull()) {
      setupInterface.setTwodir(AxisID.FIRST, twodir.getDouble());
    }
    // B stack
    String bStack = getBAxisStackFileName(setupInterface);
    if (bStack != null) {
      header = readMRCHeader(bStack);
      if (header != null) {
        twodir = header.getTwodir();
        if (!twodir.isNull()) {
          setupInterface.setTwodir(AxisID.SECOND, twodir.getDouble());
        }
      }
    }
    return true;
  }

  /**
   * Construction and read an MRCHeader object.
   * @return the MRCHeader object
   */
  private MRCHeader readMRCHeader(final String stackFileName) {
    // Run header on the dataset to the extract whatever information is
    // available
    if (stackFileName == null) {
      return null;
    }
    MRCHeader header = MRCHeader.getInstance(getPropertyUserDir(), stackFileName,
        AxisID.ONLY);
    try {
      if (!header.read(manager)) {
        UIHarness.INSTANCE.openMessageDialog(manager, "File does not exist.",
            "Entry Error", AxisID.ONLY);
        return null;
      }
    }
    catch (InvalidParameterException except) {
      UIHarness.INSTANCE.openMessageDialog(manager, except.getMessage(),
          "Invalid Parameter Exception", AxisID.ONLY);
    }
    catch (IOException except) {
      UIHarness.INSTANCE.openMessageDialog(manager, except.getMessage(), "IO Exception",
          AxisID.ONLY);
    }
    return header;
  }

  /**
   * Get the A or only stack name using dialog.getDataset()
   * @return
   */
  private String getStackFileName(final SetupReconInterface setupInterface) {
    // Get the dataset name from the UI object
    String datasetName = setupInterface.getDataset();
    if (datasetName == null || datasetName.equals("")) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Dataset name has not been entered",
          "Missing dataset name", AxisID.ONLY);
      return null;
    }
    // Add the appropriate extension onto the filename if necessary
    if (!datasetName.endsWith(DatasetTool.STANDARD_DATASET_EXT)
        && !datasetName.endsWith(DatasetTool.ALTERNATE_DATASET_EXT)) {
      String datasetNameSt;
      if (setupInterface.isDualAxisSelected()) {
        datasetNameSt = datasetName + "a" + DatasetTool.STANDARD_DATASET_EXT;
      }
      else {
        datasetNameSt = datasetName + DatasetTool.STANDARD_DATASET_EXT;
      }
      if (new File(datasetNameSt).exists()) {
        return datasetNameSt;
      }
      String datasetNameMrc;
      if (setupInterface.isDualAxisSelected()) {
        datasetNameMrc = datasetName + "a" + DatasetTool.ALTERNATE_DATASET_EXT;
      }
      else {
        datasetNameMrc = datasetName + DatasetTool.ALTERNATE_DATASET_EXT;
      }
      if (new File(datasetNameMrc).exists()) {
        return datasetNameMrc;
      }
      return datasetNameSt;
    }
    return datasetName;
  }

  /**
   * Get the B  stack name using dialog.getDataset()
   * @return
   */
  private String getBAxisStackFileName(final SetupReconInterface setupInterface) {
    if (!setupInterface.isDualAxisSelected()) {
      return null;
    }
    // Get the dataset name from the UI object
    String datasetName = setupInterface.getDataset();
    if (datasetName == null || datasetName.equals("")) {
      return null;
    }
    // Add the appropriate extension onto the filename if necessary
    if (!datasetName.endsWith(DatasetTool.STANDARD_DATASET_EXT)
        && !datasetName.endsWith(DatasetTool.ALTERNATE_DATASET_EXT)) {
      String datasetNameSt;
      datasetNameSt = datasetName + "b" + DatasetTool.STANDARD_DATASET_EXT;
      if (new File(datasetNameSt).exists()) {
        return datasetNameSt;
      }
      String datasetNameMrc;
      datasetNameMrc = datasetName + "b" + DatasetTool.ALTERNATE_DATASET_EXT;
      if (new File(datasetNameMrc).exists()) {
        return datasetNameMrc;
      }
      return datasetNameSt;
    }
    int index = datasetName.lastIndexOf("a");
    if (index == -1) {
      return null;
    }
    return datasetName.substring(0, index) + "b" + datasetName.substring(index + 1);
  }

  public boolean isValid() {
    SetupReconInterface setupInterface = getSetupReconInterface();
    if (setupInterface == null) {
      return false;
    }
    String errorMessageTitle = "Setup Dialog Error";
    String datasetText = setupInterface.getDataset();
    String panelErrorMessage;
    if (datasetText.equals("")) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Dataset name has not been entered.",
          errorMessageTitle, AxisID.ONLY);
      return false;
    }
    File dataset = new File(datasetText);
    String datasetFileName = dataset.getName();
    if (datasetFileName.equals("a" + DatasetTool.STANDARD_DATASET_EXT)
        || datasetFileName.equals("b" + DatasetTool.STANDARD_DATASET_EXT)
        || datasetFileName.equals(".")) {
      UIHarness.INSTANCE.openMessageDialog(manager, "The name " + datasetFileName
          + " cannot be used as a dataset name.", errorMessageTitle, AxisID.ONLY);
      return false;
    }
    // validate image distortion field file name
    // optional
    // file must exist
    String distortionFileText = setupInterface.getDistortionFile();
    if (distortionFileText != null && !distortionFileText.equals("")) {
      File distortionFile = new File(distortionFileText);
      if (!distortionFile.exists()) {
        String distortionFileName = distortionFile.getName();
        UIHarness.INSTANCE.openMessageDialog(manager, "The image distortion field file "
            + distortionFileName + " does not exist.", errorMessageTitle, AxisID.ONLY);
        return false;
      }
    }
    // validate mag gradient field file name
    // optional
    // file must exist
    String magGradientFileText = setupInterface.getMagGradientFile();
    if (magGradientFileText != null && !magGradientFileText.equals("")) {
      File magGradientFile = new File(magGradientFileText);
      if (!magGradientFile.exists()) {
        String magGradientFileName = magGradientFile.getName();
        UIHarness.INSTANCE.openMessageDialog(manager,
            "The mag gradients correction file " + magGradientFileName
                + " does not exist.", errorMessageTitle, AxisID.ONLY);
        return false;
      }
    }
    if (!setupInterface.validateTiltAngle(AxisID.FIRST, errorMessageTitle)) {
      return false;
    }
    if (!setupInterface.validateTiltAngle(AxisID.SECOND, errorMessageTitle)) {
      return false;
    }
    return DatasetTool.validateViewType(
        setupInterface.isSingleViewSelected() ? ViewType.SINGLE_VIEW : ViewType.MONTAGE,
        getPropertyUserDir(), getStackFileName(setupInterface), manager, null,
        AxisID.ONLY);
  }

  public boolean isDirectiveDrivenAutomation() {
    return directiveFileCollection != null;
  }

  public DirectiveFileCollection getDirectiveFileCollection() {
    if (directiveFileCollection != null) {
      return directiveFileCollection;
    }
    return expert.getDirectiveFileCollection();
  }

  public MetaData getFields(final boolean doValidation) {
    SetupReconInterface setupInterface = getSetupReconInterface();
    if (setupInterface == null) {
      return null;
    }
    try {
      MetaData metaData = getMetaData();
      AxisType axisType = getAxisType();
      metaData.setBackupDirectory(setupInterface.getBackupDirectory());
      metaData.setDistortionFile(setupInterface.getDistortionFile());
      metaData.setMagGradientFile(setupInterface.getMagGradientFile());
      metaData.setDefaultParallel(setupInterface
          .isParallelProcessSelected(getPropertyUserDir()));
      metaData.setDefaultGpuProcessing(setupInterface
          .isGpuProcessingSelected(getPropertyUserDir()));
      metaData.setAdjustedFocusA(setupInterface.isAdjustedFocusSelected(AxisID.FIRST));
      metaData.setAdjustedFocusB(setupInterface.isAdjustedFocusSelected(AxisID.SECOND));
      metaData.setViewType(getViewType(setupInterface));
      String currentField = "";
      currentField = "Image Rotation";
      metaData.setImageRotation(
          setupInterface.getImageRotation(AxisID.FIRST, doValidation), AxisID.FIRST);
      if (!metaData.getImageRotation(AxisID.FIRST).isValid()) {
        UIHarness.INSTANCE.openMessageDialog(manager, currentField + " must be numeric.",
            "Setup Dialog Error", AxisID.ONLY);
        return null;
      }
      try {
        currentField = "Pixel Size";
        metaData.setPixelSize(setupInterface.getPixelSize(doValidation));
        currentField = "Fiducial Diameter";
        metaData.setFiducialDiameter(setupInterface.getFiducialDiameter(doValidation));
        if (axisType == AxisType.DUAL_AXIS) {
          metaData
              .setImageRotation(
                  setupInterface.getImageRotation(AxisID.SECOND, doValidation),
                  AxisID.SECOND);
        }
        currentField = "Axis A starting and step angles";
        if (!setupInterface.getTiltAngleFields(AxisID.FIRST,
            metaData.getTiltAngleSpecA(), doValidation)) {
          return null;
        }
        currentField = "Axis B starting and step angles";
        if (!setupInterface.getTiltAngleFields(AxisID.SECOND,
            metaData.getTiltAngleSpecB(), doValidation)) {
          return null;
        }
      }
      catch (NumberFormatException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, currentField + " must be numeric.",
            "Setup Dialog Error", AxisID.ONLY);
        return null;
      }
      metaData.setBinning(setupInterface.getBinning());
      metaData.setExcludeProjections(
          setupInterface.getExcludeList(AxisID.FIRST, doValidation), AxisID.FIRST);
      metaData.setExcludeProjections(
          setupInterface.getExcludeList(AxisID.SECOND, doValidation), AxisID.SECOND);
      metaData.setIsTwodir(AxisID.FIRST, setupInterface.isTwodir(AxisID.FIRST));
      metaData.setIsTwodir(AxisID.SECOND, setupInterface.isTwodir(AxisID.SECOND));
      metaData.setTwodir(AxisID.FIRST,
          setupInterface.getTwodir(AxisID.FIRST, doValidation));
      metaData.setTwodir(AxisID.SECOND,
          setupInterface.getTwodir(AxisID.SECOND, doValidation));
      if (axisType == AxisType.DUAL_AXIS) {
        File bStack = DatasetFiles
            .getStack(getPropertyUserDir(), metaData, AxisID.SECOND);
        metaData.setBStackProcessed(bStack.exists());
      }
      metaData.setSetFEIPixelSize(setFEIPixelSize);
      DirectiveFileCollection directiveFileCollection = setupInterface
          .getDirectiveFileCollection();
      DirectiveFile directiveFile = directiveFileCollection
          .getDirectiveFile(DirectiveFileType.SCOPE);
      if (directiveFile != null) {
        metaData.setOrigScopeTemplate(directiveFile.getFile());
        saveDirectiveFile(directiveFile, metaData);
      }
      directiveFile = directiveFileCollection.getDirectiveFile(DirectiveFileType.SYSTEM);
      if (directiveFile != null) {
        metaData.setOrigSystemTemplate(directiveFile.getFile());
        saveDirectiveFile(directiveFile, metaData);
      }
      directiveFile = directiveFileCollection.getDirectiveFile(DirectiveFileType.USER);
      if (directiveFile != null) {
        metaData.setOrigUserTemplate(directiveFile.getFile());
        saveDirectiveFile(directiveFile, metaData);
      }
      if (directiveFileCollection != null) {
        saveDirectiveFile(
            directiveFileCollection.getDirectiveFile(DirectiveFileType.BATCH), metaData);
      }
      return metaData;
    }
    catch (FieldValidationFailedException e) {
      return null;
    }
  }

  private void saveDirectiveFile(final DirectiveFile directiveFile,
      final MetaData metaData) {
    if (directiveFile == null) {
      return;
    }
    AxisType axisType = getAxisType();
    if (directiveFile.containsRaptorUseAlignedStack(AxisID.FIRST)) {
      metaData.setTrackRaptorUseRawStack(directiveFile
          .isRaptorUseAlignedStack(AxisID.FIRST));
    }
    if (directiveFile.containsRaptorNumberOfMarkers(AxisID.FIRST)) {
      metaData.setTrackRaptorMark(directiveFile.getRaptorNumberOfMarkers(AxisID.FIRST));
    }
    saveDirectiveFile(directiveFile, metaData, AxisID.FIRST);
    saveDirectiveFile(directiveFile, metaData, AxisID.SECOND);
  }

  private void saveDirectiveFile(final DirectiveFile directiveFile,
      final MetaData metaData, final AxisID axisID) {
    if (directiveFile.containsFiducialsFiducialless(axisID)) {
      boolean value = directiveFile.isFiducialsFiducialless(axisID);
      metaData.setFiducialess(axisID, value);
      metaData.setFiducialessAlignment(axisID, value);
    }
    if (directiveFile.containsFiducialsSeedingMethod(axisID)) {
      SeedingMethod seedingMethod = directiveFile.getFiducialsSeedingMethod(axisID);
      if (seedingMethod == SeedingMethod.MANUAL) {
        metaData.setTrackSeedModelManual(true, axisID);
      }
      // If both is set, assume that autofidseed was done after manual.
      else if (seedingMethod == SeedingMethod.AUTO_FID_SEED
          || seedingMethod == SeedingMethod.BOTH) {
        metaData.setTrackSeedModelAuto(true, axisID);
      }
      else if (seedingMethod == SeedingMethod.TRANSFER_FID) {
        metaData.setTrackSeedModelTransfer(true, axisID);
      }
    }
    if (directiveFile.containsFiducialsTrackingMethod(axisID)) {
      metaData.setTrackMethod(axisID, TrackingMethod.toMetaDataValue(directiveFile
          .getFiducialsTrackingMethod(axisID)));
    }
    if (directiveFile.containsAlignedStackSizeInXandY(axisID)) {
      try {
        metaData.setSizeToOutputInXandY(axisID,
            directiveFile.getAlignedStackSizeInXandY(axisID));
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Invalid directive file: "
            + directiveFile.getFile().getAbsolutePath() + ".  Invalid directive: "
            + directiveFile.getAlignedStackSizeInXandYDescr() + ".  " + e.getMessage(),
            "Invalid Directive");
      }
    }
    if (directiveFile.containsAlignedStackBinByFactor(axisID)) {
      metaData.setStackBinning(axisID, directiveFile.getAlignedStackBinByFactor(axisID));
    }
    if (directiveFile.containsCTFplottingAutoFitRangeAndStep(axisID)) {
      try {
        metaData.setStackCtfAutoFitRangeAndStep(axisID,
            directiveFile.getCTFplottingAutoFitRangeAndStep(axisID));
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(
            manager,
            "Invalid directive file: " + directiveFile.getFile().getAbsolutePath()
                + ".  Invalid directive: "
                + directiveFile.getCTFplottingAutoFitRangeAndStepDescr() + ".  "
                + e.getMessage(), "Invalid Directive");
      }
    }
    if (directiveFile.containsGoldErasingBinning(axisID)) {
      metaData.setStack3dFindBinning(axisID, directiveFile.getGoldErasingBinning(axisID));
    }
    // GoldErasingThickness overrides the .com file
    if (directiveFile.containsGoldErasingThickness(axisID)) {
      metaData.setStack3dFindThickness(axisID,
          directiveFile.getGoldErasingThickness(axisID));
    }
    if (directiveFile.containsPositioningWholeTomogram(axisID)) {
      metaData.setWholeTomogramSample(axisID,
          directiveFile.isPositioningWholeTomogram(axisID));
    }
    if (directiveFile.containsReconstructionUseSirt(axisID)) {
      metaData.setGenBackProjection(axisID,
          !directiveFile.isReconstructionUseSirt(axisID));
    }
    if (directiveFile.containsPositioningThickness(axisID)) {
      metaData.setSampleThickness(axisID, directiveFile.getPositioningThickness(axisID));
    }
    if (directiveFile.containsPositioningBinByFactor(axisID)) {
      metaData.setPosBinning(axisID, directiveFile.getPositioningBinByFactor(axisID));
    }
  }

  public void initializeFields(ConstMetaData metaData, UserConfiguration userConfig) {
    if (expert != null) {
      expert.initializeFields(metaData, userConfig);
    }
    setFEIPixelSize = userConfig.isSetFEIPixelSize();
  }
}
