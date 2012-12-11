package etomo.ui;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.Arguments;
import etomo.EtomoDirector;
import etomo.logic.DatasetTool;
import etomo.storage.DirectiveFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.DataFileType;
import etomo.type.DialogExitState;
import etomo.type.MetaData;
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
  private DirectiveFile directiveFile = null;

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
    directiveFile = DirectiveFile.getInstance(manager, axisID);
    if (!doDirectiveAutomation()) {
      UIHarness.INSTANCE.exit(axisID, 1);
    }
    else {
      UIHarness.INSTANCE.exit(axisID, 0);
    }
  }

  private boolean doDirectiveAutomation() {
    if (directiveFile == null) {
      return false;
    }
    AxisType axisType = AxisType.SINGLE_AXIS;
    if (directiveFile.isDual()) {
      axisType = AxisType.DUAL_AXIS;
    }
    if (!DatasetTool.validateDatasetName(manager, null, axisID, new File(
        getPropertyUserDir()), directiveFile.getName(), DataFileType.RECON, axisType,
        true)) {
      return false;
    }
    if (directiveFile.isScanHeader()) {
      if (!scanHeaderAction(directiveFile)) {
        return false;
      }
    }
    if (manager.doneSetupDialog(true, directiveFile)) {
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
    if (directiveFile != null) {
      return directiveFile;
    }
    if (expert != null) {
      return expert.getSetupReconInterface();
    }
    UIHarness.INSTANCE.openMessageDialog(manager, NO_GUI_ERROR_MESSAGE,
        NO_GUI_ERROR_TITLE);
    return null;
  }

  public DialogExitState getExitState() {
    if (directiveFile != null) {
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
    if (directiveFile != null) {
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
    MetaData metaData = new MetaData(manager);
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
    if (directiveFile != null && directiveFile.isDatasetDirectory()) {
      return directiveFile.getDatasetDirectory();
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
    MRCHeader header = readMRCHeader(setupInterface);
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
    return true;
  }

  /**
   * Construction and read an MRCHeader object.
   * @return the MRCHeader object
   */
  private MRCHeader readMRCHeader(final SetupReconInterface setupInterface) {
    // Run header on the dataset to the extract whatever information is
    // available
    String stackFileName = getStackFileName(setupInterface);
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
    if (!datasetName.endsWith(".st")) {
      if (setupInterface.isDualAxisSelected()) {
        datasetName = datasetName + "a.st";
      }
      else {
        datasetName = datasetName + ".st";

      }
    }
    return datasetName;
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
    if (datasetFileName.equals("a.st") || datasetFileName.equals("b.st")
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
      metaData.setDefaultParallel(setupInterface.isParallelProcessSelected());
      metaData.setDefaultGpuProcessing(setupInterface.isGpuProcessingSelected());
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
      if (axisType == AxisType.DUAL_AXIS) {
        File bStack = DatasetFiles
            .getStack(getPropertyUserDir(), metaData, AxisID.SECOND);
        metaData.setBStackProcessed(bStack.exists());
      }
      if (directiveFile != null) {
        saveDirectives(metaData);
      }
      return metaData;
    }
    catch (FieldValidationFailedException e) {
      return null;
    }
  }

  private void saveDirectives(final MetaData metaData) {
    if (directiveFile == null) {
      return;
    }
    // Ignore Preprocessing removeXrays
    directiveFile.getFiducialsFiducialless(metaData);
    directiveFile.getFiducialsTrackingMethod(metaData);
    directiveFile.getFiducialsSeedingMethod(metaData);
    // Ignore BeadTracking numberOfRuns
    // Ignore SeedFinding rawBoundaryModel
    directiveFile.getRaptorUseAlignedStack(metaData);
    directiveFile.getRaptorNumberOfMarkers(metaData);
    // Ignore PatchTracking rawBoudaryModel
    // Ignore PatchTracking contourPieces
    // Ignore PatchTracking adjustTiltAngles
    // Ignore TiltAlignement enableStretching
    // Ignore AlignedStack correctCTF
    // Ignore AlignedStack eraseGold
    // Ignore AlignedStack filterStack
    // Ignore AlignedStack linearInterpolation
    directiveFile.getAlignedStackBinByFactor(metaData);
    directiveFile.getAlignedStackSizeInXandY(metaData);
    // Ignore CTFplotting autoFitRangeAndStep
    directiveFile.getGoldErasingBinning(metaData);
    // Ignore GoldErasing extraDiameter
    directiveFile.getGoldErasingThickness(metaData);// overrides the .com file
    //Ignore Reconstruction extraThickness
    directiveFile.getReconstructionUseSirt(metaData);
    // Ignore Reconstruction doBackprojAlso
  }
}
