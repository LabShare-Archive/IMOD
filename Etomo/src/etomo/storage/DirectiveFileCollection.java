package etomo.storage;

import java.io.File;

import etomo.ApplicationManager;
import etomo.logic.DatasetTool;
import etomo.logic.UserEnv;
import etomo.type.AxisID;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.ui.SetupReconInterface;

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
public class DirectiveFileCollection implements SetupReconInterface {
  public static final String rcsid = "$Id:$";

  private static final int BATCH_INDEX = 3;

  private final DirectiveFile[] directiveFileArray = new DirectiveFile[] { null, null,
      null, null };

  private final ApplicationManager manager;
  private final AxisID axisID;

  public DirectiveFileCollection(final ApplicationManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  /**
   * Returns true if an attribute called name is in any of the directive files.
   * @param parentName
   * @param name
   * @return
   */
  private boolean containsAttribute(final DirectiveFile.AttributeName parentName,
      final String name) {
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null) {
        if (directiveFileArray[i].containsAttribute(parentName, name)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Returns null if the attribute called name is not there.  Returns an empty string if
   * this attribute is there and it has no value.
   * @param parentName
   * @param name
   * @return
   */
  private String getValue(final DirectiveFile.AttributeName parentName, final String name) {
    String value = null;
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null
          && directiveFileArray[i].containsAttribute(parentName, name)) {
        value = directiveFileArray[i].getValue(parentName, name);
      }
    }
    return value;
  }

  public boolean containsDatasetDirectory() {
    return containsAttribute(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.DATASET_DIRECTORY_NAME);
  }

  public String getBackupDirectory() {
    return null;
  }

  public DirectiveFile getBatchDirectiveFile() {
    return directiveFileArray[BATCH_INDEX];
  }

  public String getBinning() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.BINNING_NAME);
  }

  public String getDataset() {
    return getName();
  }

  public String getDatasetDirectory() {
    return getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.DATASET_DIRECTORY_NAME);
  }

  public String getDistortionFile() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.DISTORT_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getExcludeList(final AxisID axisID, final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.SKIP_NAME));
  }

  /**
   * @param doValidation has no effect
   */
  public String getFiducialDiameter(final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.GOLD_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getImageRotation(final AxisID axisID, final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.ROTATION_NAME));
  }

  public String getMagGradientFile() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.GRADIENT_NAME);
  }

  public String getName() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.NAME_NAME);
  }

  public String getPixelSize(final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.PIXEL_NAME);
  }

  public File getScopeTemplateFile() {
    String value = getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.SCOPE_TEMPLATE_NAME);
    if (value != null) {
      return new File(value);
    }
    return null;
  }

  public File getSystemTemplateFile() {
    String value = getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.SYSTEM_TEMPLATE_NAME);
    if (value != null) {
      return new File(value);
    }
    return null;
  }

  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    tiltAngleSpec.reset();
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null
          && (directiveFileArray[i].containsAttribute(
              DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.FIRST_INC_NAME)
              || directiveFileArray[i].containsAttribute(
                  DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.USE_RAW_TLT_NAME) || directiveFileArray[i]
              .containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
                  DirectiveFile.EXTRACT_NAME))) {
        directiveFileArray[i].getTiltAngleFields(axisID, tiltAngleSpec, doValidation);
      }
    }
    return true;
  }

  public File getUserTemplateFile() {
    String value = getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.USER_TEMPLATE_NAME);
    if (value != null) {
      return new File(value);
    }
    return null;
  }

  public boolean isAdjustedFocusSelected(final AxisID axisID) {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.FOCUS_NAME)));
  }

  public boolean isDual() {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.DUAL_NAME));
  }

  public boolean isDualAxisSelected() {
    return isDual();
  }

  public boolean isGpuProcessingSelected(final String propertyUserDir) {
    return UserEnv.isGpuProcessing(manager, axisID, propertyUserDir);
  }

  public boolean isParallelProcessSelected(final String propertyUserDir) {
    return UserEnv.isParallelProcessing(manager, axisID, propertyUserDir);
  }

  public boolean isScanHeader() {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.SCAN_HEADER_NAME));
  }

  public boolean isSingleAxisSelected() {
    return !isDual();
  }

  public boolean isSingleViewSelected() {
    return !DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.MONTAGE_NAME));
  }

  public void setBatchDirectiveFile(final DirectiveFile directiveFile) {
    directiveFileArray[BATCH_INDEX] = directiveFile;
  }

  public void setBinning(final int input) {
    if (directiveFileArray[BATCH_INDEX] != null) {
      directiveFileArray[BATCH_INDEX].setBinning(input);
    }
  }

  public void setImageRotation(final String input) {
    if (directiveFileArray[BATCH_INDEX] != null) {
      directiveFileArray[BATCH_INDEX].setImageRotation(input);
    }
  }

  public void setPixelSize(final double input) {
    if (directiveFileArray[BATCH_INDEX] != null) {
      directiveFileArray[BATCH_INDEX].setPixelSize(input);
    }
  }

  public void setScopeTemplate(final String filePath) {
    if (filePath != null) {
      directiveFileArray[0] = DirectiveFile.getInstance(manager, axisID, new File(
          filePath));
    }
  }

  public void setSystemTemplate(final String filePath) {
    if (filePath != null) {
      directiveFileArray[1] = DirectiveFile.getInstance(manager, axisID, new File(
          filePath));
    }
  }

  public void setUserTemplate(final String filePath) {
    if (filePath != null) {
      directiveFileArray[2] = DirectiveFile.getInstance(manager, axisID, new File(
          filePath));
    }
  }

  public boolean validateTiltAngle(final AxisID axisID, final String errorTitle) {
    TiltAngleSpec tiltAngleSpec = new TiltAngleSpec();
    getTiltAngleFields(axisID, tiltAngleSpec, false);
    return DatasetTool.validateTiltAngle(manager, AxisID.ONLY, errorTitle, axisID,
        tiltAngleSpec.getType() == TiltAngleType.RANGE,
        String.valueOf(tiltAngleSpec.getRangeMin()),
        String.valueOf(tiltAngleSpec.getRangeStep()));
  }
}
