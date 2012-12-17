package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.FortranInputSyntaxException;
import etomo.logic.DatasetTool;
import etomo.logic.UserEnv;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAttributeIterator;
import etomo.storage.autodoc.ReadOnlyAttributeList;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.MetaData;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.ui.FieldType;
import etomo.ui.SetupReconInterface;
import etomo.ui.swing.FiducialModelDialog;
import etomo.ui.swing.UIHarness;

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
public final class DirectiveFile implements SetupReconInterface {
  public static final String rcsid = "$Id:$";

  private static final String A_AXIS_NAME = "a";
  private static final String ALIGNED_STACK_NAME = "AlignedStack";
  private static final String ANY_AXIS_NAME = "any";
  private static final String B_AXIS_NAME = "b";
  private static final String BIN_BY_FACTOR_NAME = "binByFactor";
  private static final String BINNING_NAME = "binning";
  private static final String DATASET_DIRECTORY_NAME = "datasetDirectory";
  private static final String DUAL_NAME = "dual";
  private static final String FIDUCIALLESS_NAME = "fiducialless";
  private static final String FIDUCIALS_NAME = "Fiducials";
  private static final String FIRST_INC_NAME = "firstinc";
  private static final String GOLD_ERASING_NAME = "GoldErasing";
  private static final String NUMBER_OF_MARKERS_NAME = "numberOfMarkers";
  private static final String PIXEL_NAME = "pixel";
  private static final String RAPTOR_NAME = "RAPTOR";
  private static final String ROTATION_NAME = "rotation";
  private static final String RUNTIME_NAME = "runtime";
  private static final String SEEDING_METHOD_NAME = "seedingMethod";
  private static final String SIZE_IN_X_AND_Y_NAME = "sizeInXandY";
  private static final String THICKNESS_NAME = "thickness";
  private static final String TRACKING_METHOD_NAME = "trackingMethod";
  private static final String USE_ALIGNED_STACK_NAME = "useAlignedStack";
  private static final String USE_SIRT_NAME = "useSirt";

  private final AxisID axisID;
  private final BaseManager manager;

  private ReadOnlyAttribute copyArg = null;
  private Map<String, String> copyArgOverrideMap = null;
  private List<Pair> copyArgExtraValues = null;
  private ReadOnlyAttribute runtime = null;
  private ReadOnlyAttribute setupSet = null;

  private DirectiveFile(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  /**
   * @return a valid DirectiveFile instance or null if it there was an initialization
   * failure.
   * @param manager
   * @param axisID
   * @return
   */
  public static DirectiveFile getInstance(final BaseManager manager, final AxisID axisID) {
    DirectiveFile instance = new DirectiveFile(manager, axisID);
    if (!instance.init()) {
      return null;
    }
    return instance;
  }

  /**
   * Called by getInstance.
   * @return false if failed
   */
  private boolean init() {
    try {
      ReadOnlyAutodoc autodoc = (ReadOnlyAutodoc) AutodocFactory.getInstance(manager,
          EtomoDirector.INSTANCE.getArguments().getDirective(), axisID);
      setupSet = autodoc.getAttribute("setupset");
      if (setupSet != null) {
        copyArg = setupSet.getAttribute("copyarg");
      }
      runtime = autodoc.getAttribute(RUNTIME_NAME);
    }
    catch (FileNotFoundException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(),
          "Directive File Not Found");
      return false;
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(),
          "Directive File Read Failure");
      return false;
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(),
          "Directive File Read Failure");
      return false;
    }
    return true;
  }

  /**
   * Converts the base name to either name or bname, depending on the axis ID.
   * @param axisID
   * @param name - the base name
   * @return Correct name for the axis ID.
   */
  private String convertAttributeName(final AxisID axisID, final String baseName) {
    return (axisID == AxisID.SECOND ? B_AXIS_NAME : "") + baseName;
  }

  private boolean convertBooleanValue(final String value) {
    return value == null || !value.equals("0");
  }

  private List<Pair> createCopyArgExtraValues() {
    if (copyArgExtraValues == null) {
      copyArgExtraValues = new ArrayList<Pair>();
    }
    return copyArgExtraValues;
  }

  private Map<String, String> createCopyArgOverrideMap() {
    if (copyArgOverrideMap == null) {
      copyArgOverrideMap = new HashMap<String, String>();
    }
    return copyArgOverrideMap;
  }

  private String[] getArrayValue(final ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return null;
    }
    String value = attribute.getValue();
    if (value == null) {
      return null;
    }
    return value.trim().split(FieldType.CollectionType.ARRAY.getSplitter());
  }

  private ReadOnlyAttribute getAttribute(final ReadOnlyAttribute parentAttribute,
      final String name) {
    if (parentAttribute != null) {
      return parentAttribute.getAttribute(name);
    }
    return null;
  }

  private String getAttributeValue(final ReadOnlyAttribute parentAttribute,
      final String name) {
    ReadOnlyAttribute attribute = getAttribute(parentAttribute, name);
    if (attribute != null) {
      return attribute.getValue();
    }
    return null;
  }

  private String getAttributeValue(final ReadOnlyAttribute grandparentAttribute,
      final String parentName, final String name) {
    ReadOnlyAttribute attribute = getAttribute(
        getAttribute(grandparentAttribute, parentName), name);
    if (attribute != null) {
      return attribute.getValue();
    }
    return null;
  }

  /**
   * Gets the value associated with name.  Looks in first in the override map, then in
   * copyArg, and then in the extra values list.
   * @param name
   * @return
   */
  private String getCopyArgValue(final String name) {
    // Check the override map first for copyArg
    if (copyArgOverrideMap != null && copyArgOverrideMap.containsKey(name)) {
      return copyArgOverrideMap.get(name);
    }
    // Look for the attribute
    ReadOnlyAttribute attribute = getAttribute(copyArg, name);
    if (attribute != null) {
      return attribute.getValue();
    }
    // Check the extra values for copyArg
    if (copyArgExtraValues != null) {
      for (int i = 0; i < copyArgExtraValues.size(); i++) {
        Pair pair = copyArgExtraValues.get(i);
        if (pair.equals(name)) {
          return pair.value;
        }
      }
    }
    return null;
  }

  /**
   * Puts the name/value pair into extra values if it isn't in copyArg.  Puts it in
   * override map if it is in copyArg.
   * @param name
   * @param value
   */
  private void setCopyArgValue(final String name, final String value) {
    if (copyArg.getAttribute(name) == null) {
      createCopyArgExtraValues().add(new Pair(name, value));
    }
    else {
      createCopyArgOverrideMap().put(name, value);
    }
  }

  /**
   * Convert tracking method from the directive value to the corresponding meta data value.
   * @param directiveValue
   * @return value that can be saved in MetaData
   */
  private String convertTrackingMethod(final String directiveValue) {
    if (directiveValue == null) {
      return null;
    }
    if (directiveValue.equals("0")) {
      return FiducialModelDialog.MethodEnumeratedType.SEED.getValue().toString();
    }
    if (directiveValue.equals("1")) {
      return FiducialModelDialog.MethodEnumeratedType.PATCH_TRACKING.getValue()
          .toString();
    }
    if (directiveValue.equals("2")) {
      return FiducialModelDialog.MethodEnumeratedType.RAPTOR.getValue().toString();
    }
    return null;
  }

  public void getAlignedStackBinByFactor(final MetaData metaData) {
    ReadOnlyAttribute alignedStack = getAttribute(runtime, ALIGNED_STACK_NAME);
    if (alignedStack != null) {
      String value = getAttributeValue(alignedStack, ANY_AXIS_NAME, BIN_BY_FACTOR_NAME);
      if (value != null) {
        metaData.setStackBinning(AxisID.FIRST, value);
        metaData.setStackBinning(AxisID.SECOND, value);
      }
      value = getAttributeValue(alignedStack, A_AXIS_NAME, BIN_BY_FACTOR_NAME);
      if (value != null) {
        metaData.setStackBinning(AxisID.FIRST, value);
      }
      value = getAttributeValue(alignedStack, B_AXIS_NAME, BIN_BY_FACTOR_NAME);
      if (value != null) {
        metaData.setStackBinning(AxisID.SECOND, value);
      }
    }
  }

  public void getAlignedStackSizeInXandY(final MetaData metaData) {
    ReadOnlyAttribute alignedStack = getAttribute(runtime, ALIGNED_STACK_NAME);
    if (alignedStack != null) {
      String value = getAttributeValue(alignedStack, ANY_AXIS_NAME, SIZE_IN_X_AND_Y_NAME);
      String errorLoc = ANY_AXIS_NAME;
      try {
        if (value != null) {
          metaData.setSizeToOutputInXandY(AxisID.FIRST, value);
          metaData.setSizeToOutputInXandY(AxisID.SECOND, value);
        }
        value = getAttributeValue(alignedStack, A_AXIS_NAME, SIZE_IN_X_AND_Y_NAME);
        if (value != null) {
          errorLoc = A_AXIS_NAME;
          metaData.setSizeToOutputInXandY(AxisID.FIRST, value);
        }
        value = getAttributeValue(alignedStack, B_AXIS_NAME, SIZE_IN_X_AND_Y_NAME);
        if (value != null) {
          errorLoc = B_AXIS_NAME;
          metaData.setSizeToOutputInXandY(AxisID.SECOND, value);
        }
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Syntax error in " + RUNTIME_NAME
            + "." + ALIGNED_STACK_NAME + "." + errorLoc + "." + SIZE_IN_X_AND_Y_NAME
            + ":" + e.getMessage(), "Syntax Error in Directive File");
      }
    }
  }

  public String getBackupDirectory() {
    return null;
  }

  public String getBinning() {
    return getCopyArgValue(BINNING_NAME);
  }

  public CopyArgIterator getCopyArgIterator() {
    return new CopyArgIterator(copyArg, copyArgOverrideMap, copyArgExtraValues);
  }

  public String getDataset() {
    return getName();
  }

  public String getDatasetDirectory() {
    return getAttributeValue(setupSet, DATASET_DIRECTORY_NAME);
  }

  public String getDistortionFile() {
    return getAttributeValue(copyArg, "distort");
  }

  /**
   * @param doValidation has no effect
   */
  public String getExcludeList(final AxisID axisID, final boolean doValidation) {
    return getAttributeValue(copyArg, convertAttributeName(axisID, "skip"));
  }

  /**
   * @param doValidation has no effect
   */
  public String getFiducialDiameter(final boolean doValidation) {
    return getAttributeValue(copyArg, "gold");
  }

  public void getFiducialsFiducialless(final MetaData metaData) {
    ReadOnlyAttribute fiducials = getAttribute(runtime, FIDUCIALS_NAME);
    if (fiducials != null) {
      ReadOnlyAttribute fiducialless = getAttribute(
          fiducials.getAttribute(ANY_AXIS_NAME), FIDUCIALLESS_NAME);
      if (fiducialless != null) {
        boolean value = convertBooleanValue(fiducialless.getValue());
        metaData.setFiducialess(AxisID.FIRST, value);
        metaData.setFiducialessAlignment(AxisID.FIRST, value);
        metaData.setFiducialess(AxisID.SECOND, value);
        metaData.setFiducialessAlignment(AxisID.SECOND, value);
      }
      fiducialless = getAttribute(fiducials.getAttribute(A_AXIS_NAME), FIDUCIALLESS_NAME);
      if (fiducialless != null) {
        boolean value = convertBooleanValue(fiducialless.getValue());
        metaData.setFiducialess(AxisID.FIRST, value);
        metaData.setFiducialessAlignment(AxisID.FIRST, value);
      }
      fiducialless = getAttribute(fiducials.getAttribute(B_AXIS_NAME), FIDUCIALLESS_NAME);
      if (fiducialless != null) {
        boolean value = convertBooleanValue(fiducialless.getValue());
        metaData.setFiducialess(AxisID.SECOND, value);
        metaData.setFiducialessAlignment(AxisID.SECOND, value);
      }
    }
  }

  public void getFiducialsSeedingMethod(final MetaData metaData) {
    ReadOnlyAttribute fiducials = getAttribute(runtime, FIDUCIALS_NAME);
    if (fiducials != null) {
      getFiducialsSeedingMethod(AxisID.FIRST,
          getAttributeValue(fiducials, ANY_AXIS_NAME, SEEDING_METHOD_NAME), metaData);
      getFiducialsSeedingMethod(AxisID.SECOND,
          getAttributeValue(fiducials, ANY_AXIS_NAME, SEEDING_METHOD_NAME), metaData);
      getFiducialsSeedingMethod(AxisID.FIRST,
          getAttributeValue(fiducials, A_AXIS_NAME, SEEDING_METHOD_NAME), metaData);
      getFiducialsSeedingMethod(AxisID.SECOND,
          getAttributeValue(fiducials, B_AXIS_NAME, SEEDING_METHOD_NAME), metaData);
    }
  }

  private void getFiducialsSeedingMethod(final AxisID axisID, final String value,
      final MetaData metaData) {
    if (value == null) {
      return;
    }
    if (value.equals("0")) {
      metaData.setTrackSeedModelManual(true, axisID);
    }
    // If both (3) is set, assume that autofidseed was done second.
    else if (value.equals("1") || value.equals("3")) {
      metaData.setTrackSeedModelAuto(true, axisID);
    }
    else if (value.equals("2")) {
      metaData.setTrackSeedModelTransfer(true, axisID);
    }
  }

  public void getFiducialsTrackingMethod(final MetaData metaData) {
    ReadOnlyAttribute fiducials = getAttribute(runtime, FIDUCIALS_NAME);
    if (fiducials != null) {
      String value = convertTrackingMethod(getAttributeValue(fiducials, ANY_AXIS_NAME,
          TRACKING_METHOD_NAME));
      if (value != null) {
        metaData.setTrackMethod(AxisID.FIRST, value);
        metaData.setTrackMethod(AxisID.SECOND, value);
      }
      value = convertTrackingMethod(getAttributeValue(fiducials, A_AXIS_NAME,
          TRACKING_METHOD_NAME));
      if (value != null) {
        metaData.setTrackMethod(AxisID.FIRST, value);
      }
      value = convertTrackingMethod(getAttributeValue(fiducials, B_AXIS_NAME,
          TRACKING_METHOD_NAME));
      if (value != null) {
        metaData.setTrackMethod(AxisID.SECOND, value);
      }
    }
  }

  public void getGoldErasingBinning(final MetaData metaData) {
    ReadOnlyAttribute goldErasing = getAttribute(runtime, GOLD_ERASING_NAME);
    if (goldErasing != null) {
      String value = getAttributeValue(goldErasing, ANY_AXIS_NAME, BINNING_NAME);
      if (value != null) {
        metaData.setStack3dFindBinning(AxisID.FIRST, value);
        metaData.setStack3dFindBinning(AxisID.SECOND, value);
      }
      value = getAttributeValue(goldErasing, A_AXIS_NAME, BINNING_NAME);
      if (value != null) {
        metaData.setStack3dFindBinning(AxisID.FIRST, value);
      }
      value = getAttributeValue(goldErasing, B_AXIS_NAME, BINNING_NAME);
      if (value != null) {
        metaData.setStack3dFindBinning(AxisID.SECOND, value);
      }
    }
  }

  public void getGoldErasingThickness(final MetaData metaData) {
    ReadOnlyAttribute goldErasing = getAttribute(runtime, GOLD_ERASING_NAME);
    if (goldErasing != null) {
      String value = getAttributeValue(goldErasing, ANY_AXIS_NAME, THICKNESS_NAME);
      if (value != null) {
        metaData.setStack3dFindThickness(AxisID.FIRST, value);
        metaData.setStack3dFindThickness(AxisID.SECOND, value);
      }
      value = getAttributeValue(goldErasing, A_AXIS_NAME, THICKNESS_NAME);
      if (value != null) {
        metaData.setStack3dFindThickness(AxisID.FIRST, value);
      }
      value = getAttributeValue(goldErasing, B_AXIS_NAME, THICKNESS_NAME);
      if (value != null) {
        metaData.setStack3dFindThickness(AxisID.SECOND, value);
      }
    }
  }

  /**
   * @param doValidation has no effect
   */
  public String getImageRotation(final AxisID axisID, final boolean doValidation) {
    return getCopyArgValue(convertAttributeName(axisID, ROTATION_NAME));
  }

  public String getMagGradientFile() {
    return getAttributeValue(copyArg, "gradient");
  }

  public String getName() {
    return getAttributeValue(copyArg, "name");
  }

  public String getPixelSize(final boolean doValidation) {
    return getCopyArgValue(PIXEL_NAME);
  }

  public void getRaptorNumberOfMarkers(final MetaData metaData) {
    ReadOnlyAttribute raptor = getAttribute(runtime, RAPTOR_NAME);
    if (raptor != null) {
      String value = getAttributeValue(raptor, ANY_AXIS_NAME, NUMBER_OF_MARKERS_NAME);
      if (value != null) {
        metaData.setTrackRaptorMark(value);
      }
      value = getAttributeValue(raptor, A_AXIS_NAME, NUMBER_OF_MARKERS_NAME);
      if (value != null) {
        metaData.setTrackRaptorMark(value);
      }
    }
  }

  public void getRaptorUseAlignedStack(final MetaData metaData) {
    ReadOnlyAttribute raptor = getAttribute(runtime, RAPTOR_NAME);
    if (raptor != null) {
      ReadOnlyAttribute useAlignedStack = getAttribute(
          raptor.getAttribute(ANY_AXIS_NAME), USE_ALIGNED_STACK_NAME);
      if (useAlignedStack != null) {
        boolean value = convertBooleanValue(useAlignedStack.getValue());
        metaData.setTrackRaptorUseRawStack(!value);
      }
      useAlignedStack = getAttribute(raptor.getAttribute(A_AXIS_NAME),
          USE_ALIGNED_STACK_NAME);
      if (useAlignedStack != null) {
        boolean value = convertBooleanValue(useAlignedStack.getValue());
        metaData.setTrackRaptorUseRawStack(!value);
      }
    }
  }

  public void getReconstructionUseSirt(final MetaData metaData) {
    ReadOnlyAttribute reconstruction = getAttribute(runtime, "Reconstruction");
    if (reconstruction != null) {
      ReadOnlyAttribute useSirt = getAttribute(
          reconstruction.getAttribute(ANY_AXIS_NAME), USE_SIRT_NAME);
      if (useSirt != null) {
        boolean value = convertBooleanValue(useSirt.getValue());
        metaData.setGenBackProjection(AxisID.FIRST, !value);
        metaData.setGenBackProjection(AxisID.SECOND, !value);
      }
      useSirt = getAttribute(reconstruction.getAttribute(A_AXIS_NAME), USE_SIRT_NAME);
      if (useSirt != null) {
        boolean value = convertBooleanValue(useSirt.getValue());
        metaData.setGenBackProjection(AxisID.FIRST, !value);
      }
      useSirt = getAttribute(reconstruction.getAttribute(B_AXIS_NAME), USE_SIRT_NAME);
      if (useSirt != null) {
        boolean value = convertBooleanValue(useSirt.getValue());
        metaData.setGenBackProjection(AxisID.SECOND, !value);
      }
    }
  }

  /**
   * @param doValidation has no effect.
   * @return true
   */
  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    if (copyArg == null) {
      return true;
    }
    ReadOnlyAttribute attribute = null;
    if ((attribute = copyArg.getAttribute(convertAttributeName(axisID, FIRST_INC_NAME))) != null) {
      tiltAngleSpec.setType(TiltAngleType.RANGE);
      String[] arrayValue = getArrayValue(attribute);
      if (arrayValue != null && arrayValue.length > 0) {
        tiltAngleSpec.setRangeMin(arrayValue[0]);
      }
      if (arrayValue.length > 1) {
        tiltAngleSpec.setRangeStep(arrayValue[1]);
      }
    }
    else if ((attribute = copyArg.getAttribute(convertAttributeName(axisID, "userawtlt"))) != null) {
      tiltAngleSpec.setType(TiltAngleType.FILE);
    }
    else if ((attribute = copyArg.getAttribute(convertAttributeName(axisID, "extract"))) != null) {
      tiltAngleSpec.setType(TiltAngleType.EXTRACT);
    }
    return true;
  }

  public boolean isAdjustedFocusSelected(final AxisID axisID) {
    return copyArg != null
        && copyArg.getAttribute(convertAttributeName(axisID, "focus")) != null;
  }

  public boolean isDatasetDirectory() {
    return setupSet != null && setupSet.getAttribute(DATASET_DIRECTORY_NAME) != null;
  }

  public boolean isDual() {
    return copyArg != null && copyArg.getAttribute(DUAL_NAME) != null;
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
    return setupSet != null && setupSet.getAttribute("scanHeader") != null;
  }

  public boolean isSingleAxisSelected() {
    return copyArg != null && copyArg.getAttribute(DUAL_NAME) == null;
  }

  public boolean isSingleViewSelected() {
    return copyArg != null && copyArg.getAttribute("montage") == null;
  }

  public void setBinning(final int input) {
    setCopyArgValue(BINNING_NAME, Integer.toString(input));
  }

  public void setImageRotation(final String input) {
    setCopyArgValue(convertAttributeName(AxisID.FIRST, ROTATION_NAME), input);
    if (isDual()) {
      setCopyArgValue(convertAttributeName(AxisID.SECOND, ROTATION_NAME), input);
    }
  }

  public void setPixelSize(final double input) {
    setCopyArgValue(PIXEL_NAME, Double.toString(input));
  }

  public boolean validateTiltAngle(final AxisID axisID, final String errorTitle) {
    ReadOnlyAttribute firstInc = null;
    if (copyArg != null) {
      firstInc = copyArg.getAttribute(convertAttributeName(axisID, FIRST_INC_NAME));
    }
    // Set first and inc.
    String[] firstIncArray = getArrayValue(firstInc);
    String first = null;
    String inc = null;
    if (firstIncArray != null && firstIncArray.length > 0) {
      first = firstIncArray[0];
      if (firstIncArray.length > 1) {
        inc = firstIncArray[1];
      }
    }
    return DatasetTool.validateTiltAngle(manager, AxisID.ONLY, errorTitle, axisID,
        firstInc != null, first, inc);
  }

  private static final class Pair {
    private final String name;
    private final String value;

    private Pair(final String name, final String value) {
      this.name = name;
      this.value = value;
    }

    private boolean equals(final String input) {
      if (name == null) {
        return input == null;
      }
      return name.equals(input);
    }
  }

  public static final class CopyArgIterator {
    private final ReadOnlyAttributeIterator iterator;
    private final Map<String, String> overrideMap;
    private final List<Pair> extraValues;

    private ReadOnlyAttribute curAttribute = null;
    private int index = -1;

    public CopyArgIterator(final ReadOnlyAttribute copyArg,
        final Map<String, String> copyArgOverrideMap, final List<Pair> copyArgExtraValues) {
      overrideMap = copyArgOverrideMap;
      extraValues = copyArgExtraValues;
      if (copyArg == null) {
        iterator = null;
      }
      else {
        ReadOnlyAttributeList list = copyArg.getChildren();
        if (list == null) {
          iterator = null;
        }
        else {
          iterator = list.iterator();
        }
      }
    }

    public boolean hasNext() {
      if (iterator != null && iterator.hasNext()) {
        return true;
      }
      return extraValues != null && index + 1 < extraValues.size();
    }

    /**
     * Moves iterator to the next element.  After calling next, use getName and getValue
     * to fetch the element that the iterator is pointing to.
     */
    public void next() {
      if (!hasNext()) {
        curAttribute = null;
        index = extraValues != null ? extraValues.size() : -1;
        return;
      }
      if (iterator != null && iterator.hasNext()) {
        curAttribute = iterator.next();
        return;
      }
      curAttribute = null;
      if (extraValues != null) {
        index++;
        return;
      }
    }

    /**
     * Call next before calling getName and getValue.
     * @return
     */
    public String getName() {
      if (curAttribute != null) {
        return curAttribute.getName();
      }
      if (index >= 0 && index < extraValues.size()) {
        return extraValues.get(index).name;
      }
      return null;
    }

    /**
     * Call next before calling getName and getValue.
     * @return
     */
    public String getValue() {
      if (curAttribute != null) {
        String name = curAttribute.getName();
        if (overrideMap != null && overrideMap.containsKey(name)) {
          return overrideMap.get(name);
        }
        return curAttribute.getValue();
      }
      if (index >= 0 && index < extraValues.size()) {
        return extraValues.get(index).value;
      }
      return null;
    }
  }
}
