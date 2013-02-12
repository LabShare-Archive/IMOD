package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAttributeIterator;
import etomo.storage.autodoc.ReadOnlyAttributeList;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.ui.FieldType;
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
public final class DirectiveFile {
  public static final String rcsid = "$Id:$";

  private static final String A_AXIS_NAME = "a";
  private static final String ALIGNED_STACK_NAME = "AlignedStack";
  private static final String ANY_AXIS_NAME = "any";
  private static final String AUTO_FIT_RANGE_AND_STEP_NAME = "autoFitRangeAndStep";
  private static final String B_AXIS_NAME = "b";
  private static final String BIN_BY_FACTOR_NAME = "binByFactor";
  static final String BINNING_NAME = "binning";
  private static final String CTF_PLOTTING_NAME = "CTFplotting";
  static final String DATASET_DIRECTORY_NAME = "datasetDirectory";
  static final String DISTORT_NAME = "distort";
  static final String DUAL_NAME = "dual";
  static final String EXTRACT_NAME = "extract";
  private static final String FIDUCIALLESS_NAME = "fiducialless";
  private static final String FIDUCIALS_NAME = "Fiducials";
  static final String FIRST_INC_NAME = "firstinc";
  static final String FOCUS_NAME = "focus";
  private static final String GOLD_ERASING_NAME = "GoldErasing";
  static final String GOLD_NAME = "gold";
  static final String GRADIENT_NAME = "gradient";
  static final String MONTAGE_NAME = "montage";
  static final String NAME_NAME = "name";
  private static final String NUMBER_OF_MARKERS_NAME = "numberOfMarkers";
  static final String PIXEL_NAME = "pixel";
  private static final String POSITIONING_NAME = "Positioning";
  private static final String RAPTOR_NAME = "RAPTOR";
  private static final String RECONSTRUCTION_NAME = "Reconstruction";
  static final String ROTATION_NAME = "rotation";
  private static final String RUNTIME_NAME = "runtime";
  static final String SCAN_HEADER_NAME = "scanHeader";
  static final String SCOPE_TEMPLATE_NAME = "scopeTemplate";
  private static final String SEEDING_METHOD_NAME = "seedingMethod";
  private static final String SIZE_IN_X_AND_Y_NAME = "sizeInXandY";
  static final String SKIP_NAME = "skip";
  static final String SYSTEM_TEMPLATE_NAME = "systemTemplate";
  private static final String THICKNESS_NAME = "thickness";
  private static final String TRACKING_METHOD_NAME = "trackingMethod";
  private static final String USE_ALIGNED_STACK_NAME = "useAlignedStack";
  static final String USE_RAW_TLT_NAME = "userawtlt";
  private static final String USE_SIRT_NAME = "useSirt";
  static final String USER_TEMPLATE_NAME = "userTemplate";
  private static final String WHOLE_TOMOGRAM_NAME = "wholeTomogram";

  private final AxisID axisID;
  private final BaseManager manager;
  private final File file;

  private ReadOnlyAttribute copyArg = null;
  private Map<String, String> copyArgExtraValues = null;
  private ReadOnlyAttribute runtime = null;
  private ReadOnlyAttribute setupSet = null;

  private DirectiveFile(final BaseManager manager, final AxisID axisID, final File file) {
    this.manager = manager;
    this.axisID = axisID;
    this.file = file;
  }

  /**
   * @return a valid DirectiveFile instance or null if it there was an initialization
   * failure.
   * @param manager
   * @param axisID
   * @return
   */
  public static DirectiveFile getInstance(final BaseManager manager, final AxisID axisID) {
    DirectiveFile instance = new DirectiveFile(manager, axisID, EtomoDirector.INSTANCE
        .getArguments().getDirective());
    if (!instance.init()) {
      return null;
    }
    return instance;
  }

  public static DirectiveFile getInstance(final BaseManager manager, final AxisID axisID,
      final File file) {
    DirectiveFile instance = new DirectiveFile(manager, axisID, file);
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
          file, axisID);
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
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(),
          "Directive File Read Failure");
      return false;
    }
    return true;
  }

  boolean containsAttribute(final AttributeName parentName, final String name) {
    return getAttribute(parentName, name) != null;
  }

  private boolean containsAttribute(final AttributeName parentName,
      final String sectionName, final AxisID axisID, final String name) {
    ReadOnlyAttribute parent = getAttribute(parentName, sectionName);
    if (parent == null) {
      return false;
    }
    ReadOnlyAttribute axis = parent.getAttribute(ANY_AXIS_NAME);
    if (axis != null && axis.getAttribute(name) != null) {
      return true;
    }
    String axisName = getAxisName(axisID);
    if (axisName != null) {
      axis = parent.getAttribute(axisName);
      if (axis != null && axis.getAttribute(name) != null) {
        return true;
      }
    }
    return false;
  }

  private boolean containsAttribute(final ReadOnlyAttribute parent,
      final String axisName, final String name) {
    if (parent == null) {
      return false;
    }
    ReadOnlyAttribute axis = parent.getAttribute(axisName);
    if (axis != null && axis.getAttribute(name) != null) {
      return true;
    }
    return false;
  }

  /**
   * Converts the base name to either name or bname, depending on the axis ID.
   * @param axisID
   * @param name - the base name
   * @return Correct name for the axis ID.
   */
  static String convertAttributeName(final AxisID axisID, final String baseName) {
    return (axisID == AxisID.SECOND ? B_AXIS_NAME : "") + baseName;
  }

  private ReadOnlyAttribute getAttribute(final AttributeName parentName, final String name) {
    ReadOnlyAttribute parent = getParentAttribute(parentName);
    if (parent == null) {
      return null;
    }
    return parent.getAttribute(name);
  }

  private String getAxisName(final AxisID axisID) {
    if (axisID == AxisID.ONLY || axisID == AxisID.FIRST) {
      return A_AXIS_NAME;
    }
    if (axisID == AxisID.SECOND) {
      return B_AXIS_NAME;
    }
    return null;
  }

  private ReadOnlyAttribute getParentAttribute(final AttributeName parentName) {
    if (parentName == AttributeName.COPY_ARG) {
      return copyArg;
    }
    if (parentName == AttributeName.SETUP_SET) {
      return setupSet;
    }
    if (parentName == AttributeName.RUN_TIME) {
      return runtime;
    }
    return null;
  }

  private boolean isValue(final AttributeName parentName, final String name) {
    ReadOnlyAttribute attribute = getAttribute(parentName, name);
    if (attribute == null) {
      return false;
    }
    return toBoolean(attribute.getValue());
  }

  private boolean isValue(final AttributeName parentName, final String sectionName,
      final AxisID axisID, final String name) {
    ReadOnlyAttribute section = getAttribute(parentName, sectionName);
    if (section == null) {
      return false;
    }
    String value = null;
    if (containsAttribute(section, ANY_AXIS_NAME, name)) {
      value = getValue(section, ANY_AXIS_NAME, name);
    }
    String axisName = getAxisName(axisID);
    if (axisName != null && containsAttribute(section, axisName, name)) {
      value = getValue(section, axisName, name);
    }
    return toBoolean(value);
  }

  /**
   * Puts the name/value pair into extra values if it isn't in copyArg.
   * @param name
   * @param value
   */
  private void setCopyArgValue(final String name, final String value) {
    if (copyArg.getAttribute(name) == null) {
      if (copyArgExtraValues == null) {
        copyArgExtraValues = new HashMap<String, String>();
      }
      if (copyArgExtraValues.containsKey(name)) {
        copyArgExtraValues.remove(name);
      }
      copyArgExtraValues.put(name, value);
    }
  }

  /**
   * Returns null if the attribute called name is not there.  Returns an empty string if
   * this attribute is there and it has no value.
   * @param parentName
   * @param name
   * @return
   */
  String getValue(final AttributeName parentName, final String name) {
    ReadOnlyAttribute parent = getParentAttribute(parentName);
    if (parent == null) {
      return null;
    }
    ReadOnlyAttribute attribute = parent.getAttribute(name);
    if (attribute == null) {
      return null;
    }
    String value = attribute.getValue();
    if (value == null) {
      return "";
    }
    return value;
  }

  /**
   * Returns null if the attribute called name is not there.  Returns an empty string if
   * this attribute is there and it has no value.
   * @param parentName
   * @param sectionName
   * @param axisID
   * @param name
   * @return
   */
  private String getValue(final AttributeName parentName, final String sectionName,
      final AxisID axisID, final String name) {
    ReadOnlyAttribute section = getAttribute(parentName, sectionName);
    if (section == null) {
      return null;
    }
    String value = null;
    if (containsAttribute(section, ANY_AXIS_NAME, name)) {
      value = getValue(section, ANY_AXIS_NAME, name);
      if (value == null) {
        value = "";
      }
    }
    String axisName = getAxisName(axisID);
    if (axisName != null && containsAttribute(section, axisName, name)) {
      value = getValue(section, axisName, name);
    }
    if (value == null) {
      value = "";
    }
    return value;
  }

  /**
   * Returns true unless value is null or 0.
   * @param value
   * @return
   */
  static boolean toBoolean(final String value) {
    if (value.equals("0")) {
      return false;
    }
    return true;
  }

  /**
   * Returns null if the attribute called name is not there.  Returns an empty string if
   * this attribute is there and it has no value.
   * @param parentName
   * @param name
   * @return
   */
  private String getValue(final ReadOnlyAttribute parent, final String axisName,
      final String name) {
    if (parent == null) {
      return null;
    }
    ReadOnlyAttribute axis = parent.getAttribute(axisName);
    if (axis == null) {
      return null;
    }
    ReadOnlyAttribute attribute = axis.getAttribute(name);
    if (attribute == null) {
      return null;
    }
    String value = attribute.getValue();
    if (value == null) {
      return "";
    }
    return value;
  }

  public boolean containsAlignedStackBinByFactor(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, ALIGNED_STACK_NAME, axisID,
        BIN_BY_FACTOR_NAME);
  }

  public boolean containsAlignedStackSizeInXandY(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, ALIGNED_STACK_NAME, axisID,
        SIZE_IN_X_AND_Y_NAME);
  }

  public boolean containsBinning() {
    return containsAttribute(AttributeName.COPY_ARG, BINNING_NAME);
  }

  public boolean containsCTFplottingAutoFitRangeAndStep(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, CTF_PLOTTING_NAME, axisID,
        AUTO_FIT_RANGE_AND_STEP_NAME);
  }

  public boolean containsDistort() {
    return containsAttribute(AttributeName.COPY_ARG, DISTORT_NAME);
  }

  public boolean containsFiducialsFiducialless(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, FIDUCIALS_NAME, axisID,
        FIDUCIALLESS_NAME);
  }

  public boolean containsFiducialsSeedingMethod(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, FIDUCIALS_NAME, axisID,
        SEEDING_METHOD_NAME);
  }

  public boolean containsFiducialsTrackingMethod(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, FIDUCIALS_NAME, axisID,
        TRACKING_METHOD_NAME);
  }

  public boolean containsGold() {
    return containsAttribute(AttributeName.COPY_ARG, GOLD_NAME);
  }

  public boolean containsGoldErasingBinning(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, GOLD_ERASING_NAME, axisID,
        BINNING_NAME);
  }

  public boolean containsGoldErasingThickness(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, GOLD_ERASING_NAME, axisID,
        THICKNESS_NAME);
  }

  public boolean containsGradient() {
    return containsAttribute(AttributeName.COPY_ARG, GRADIENT_NAME);
  }

  public boolean containsPixel() {
    return containsAttribute(AttributeName.COPY_ARG, PIXEL_NAME);
  }

  public boolean containsPositioningBinByFactor(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, POSITIONING_NAME, axisID,
        BIN_BY_FACTOR_NAME);
  }

  public boolean containsPositioningThickness(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, POSITIONING_NAME, axisID,
        THICKNESS_NAME);
  }

  public boolean containsPositioningWholeTomogram(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, POSITIONING_NAME, axisID,
        WHOLE_TOMOGRAM_NAME);
  }

  public boolean containsRaptorNumberOfMarkers(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, RAPTOR_NAME, axisID,
        NUMBER_OF_MARKERS_NAME);
  }

  public boolean containsRaptorUseAlignedStack(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, RAPTOR_NAME, axisID,
        USE_ALIGNED_STACK_NAME);
  }

  public boolean containsReconstructionUseSirt(final AxisID axisID) {
    return containsAttribute(AttributeName.RUN_TIME, RECONSTRUCTION_NAME, axisID,
        USE_SIRT_NAME);
  }

  public boolean containsRotation() {
    return containsAttribute(AttributeName.COPY_ARG, ROTATION_NAME);
  }

  public boolean containsTiltAngleSpec(final AxisID axisID) {
    return containsAttribute(AttributeName.COPY_ARG,
        convertAttributeName(axisID, FIRST_INC_NAME))
        || containsAttribute(AttributeName.COPY_ARG,
            convertAttributeName(axisID, USE_RAW_TLT_NAME))
        || containsAttribute(AttributeName.COPY_ARG,
            convertAttributeName(axisID, EXTRACT_NAME));
  }

  public String getAlignedStackBinByFactor(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, ALIGNED_STACK_NAME, axisID,
        BIN_BY_FACTOR_NAME);
  }

  public String getAlignedStackSizeInXandY(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, ALIGNED_STACK_NAME, axisID,
        SIZE_IN_X_AND_Y_NAME);
  }

  public String getAlignedStackSizeInXandYDescr() {
    return AttributeName.RUN_TIME + "." + ALIGNED_STACK_NAME + "..."
        + SIZE_IN_X_AND_Y_NAME;
  }

  Iterator<Entry<String, String>> getCopyArgExtraValuesIterator() {
    return copyArgExtraValues.entrySet().iterator();
  }

  ReadOnlyAttributeIterator getCopyArgIterator() {
    ReadOnlyAttributeList list = copyArg.getChildren();
    if (list != null) {
      return list.iterator();
    }
    return null;
  }

  public String getCTFplottingAutoFitRangeAndStep(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, CTF_PLOTTING_NAME, axisID,
        AUTO_FIT_RANGE_AND_STEP_NAME);
  }

  public String getCTFplottingAutoFitRangeAndStepDescr() {
    return AttributeName.RUN_TIME + "." + CTF_PLOTTING_NAME + "..."
        + AUTO_FIT_RANGE_AND_STEP_NAME;
  }

  public String getDistortionFile() {
    return getValue(AttributeName.COPY_ARG, DISTORT_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getFiducialDiameter(final boolean doValidation) {
    return getValue(AttributeName.COPY_ARG, GOLD_NAME);
  }

  public FiducialsSeedingMethod getFiducialsSeedingMethod(final AxisID axisID) {
    return FiducialsSeedingMethod.getInstance(getValue(AttributeName.RUN_TIME,
        FIDUCIALS_NAME, axisID, SEEDING_METHOD_NAME));
  }

  public FiducialsTrackingMethod getFiducialsTrackingMethod(final AxisID axisID) {
    return FiducialsTrackingMethod.getInstance(getValue(AttributeName.RUN_TIME,
        FIDUCIALS_NAME, axisID, TRACKING_METHOD_NAME));
  }

  public File getFile() {
    return file;
  }

  public String getGoldErasingBinning(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, GOLD_ERASING_NAME, axisID, BINNING_NAME);
  }

  public String getGoldErasingThickness(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, GOLD_ERASING_NAME, axisID, THICKNESS_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getImageRotation(final AxisID axisID, final boolean doValidation) {
    return getValue(AttributeName.COPY_ARG, convertAttributeName(axisID, ROTATION_NAME));
  }

  public int getIntBinning() {
    EtomoNumber binning = new EtomoNumber();
    binning.set(getValue(AttributeName.COPY_ARG, BINNING_NAME));
    if (binning.isValid()) {
      return binning.getInt();
    }
    UIHarness.INSTANCE.openMessageDialog(
        manager,
        "Invalid binning in " + file.getAbsolutePath() + ".  "
            + binning.getInvalidReason(), "Invalid Binning");
    return 1;
  }

  public String getMagGradientFile() {
    return getValue(AttributeName.COPY_ARG, GRADIENT_NAME);
  }

  public String getPixelSize(final boolean doValidation) {
    return getValue(AttributeName.COPY_ARG, PIXEL_NAME);
  }

  public String getPositioningBinByFactor(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, POSITIONING_NAME, axisID, BIN_BY_FACTOR_NAME);
  }

  public String getPositioningThickness(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, POSITIONING_NAME, axisID, THICKNESS_NAME);
  }

  public String getRaptorNumberOfMarkers(final AxisID axisID) {
    return getValue(AttributeName.RUN_TIME, RAPTOR_NAME, axisID, NUMBER_OF_MARKERS_NAME);
  }

  public String getScopeTemplate() {
    return getValue(AttributeName.SETUP_SET, SCOPE_TEMPLATE_NAME);
  }

  public String getSystemTemplate() {
    return getValue(AttributeName.SETUP_SET, SYSTEM_TEMPLATE_NAME);
  }

  public String getUserTemplate() {
    return getValue(AttributeName.SETUP_SET, USER_TEMPLATE_NAME);
  }

  /**
   * @param doValidation has no effect.
   * @return true
   */
  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    if (tiltAngleSpec == null) {
      return true;
    }
    ReadOnlyAttribute attribute = null;
    if ((attribute = getAttribute(AttributeName.COPY_ARG,
        convertAttributeName(axisID, FIRST_INC_NAME))) != null) {
      tiltAngleSpec.setType(TiltAngleType.RANGE);
      String[] arrayValue = null;
      if (attribute != null) {
        String value = attribute.getValue();
        if (value != null) {
          arrayValue = value.trim().split(FieldType.CollectionType.ARRAY.getSplitter());
        }
      }
      if (arrayValue != null && arrayValue.length > 0) {
        tiltAngleSpec.setRangeMin(arrayValue[0]);
      }
      if (arrayValue.length > 1) {
        tiltAngleSpec.setRangeStep(arrayValue[1]);
      }
    }
    else if ((attribute = getAttribute(AttributeName.COPY_ARG,
        convertAttributeName(axisID, EXTRACT_NAME))) != null) {
      tiltAngleSpec.setType(TiltAngleType.EXTRACT);
    }
    else if ((attribute = getAttribute(AttributeName.COPY_ARG,
        convertAttributeName(axisID, USE_RAW_TLT_NAME))) != null) {
      tiltAngleSpec.setType(TiltAngleType.FILE);
    }
    return true;
  }

  public boolean isAdjustedFocusSelected(final AxisID axisID) {
    return isValue(AttributeName.COPY_ARG, convertAttributeName(axisID, FOCUS_NAME));
  }

  public boolean isFiducialsFiducialless(final AxisID axisID) {
    return isValue(AttributeName.RUN_TIME, FIDUCIALS_NAME, axisID, FIDUCIALLESS_NAME);
  }

  public boolean isDual() {
    return isValue(AttributeName.COPY_ARG, DUAL_NAME);
  }

  public boolean isPositioningWholeTomogram(final AxisID axisID) {
    return isValue(AttributeName.RUN_TIME, POSITIONING_NAME, axisID, WHOLE_TOMOGRAM_NAME);
  }

  public boolean isRaptorUseAlignedStack(final AxisID axisID) {
    return isValue(AttributeName.RUN_TIME, RAPTOR_NAME, axisID, USE_ALIGNED_STACK_NAME);
  }

  public boolean isReconstructionUseSirt(final AxisID axisID) {
    return isValue(AttributeName.RUN_TIME, RECONSTRUCTION_NAME, axisID, USE_SIRT_NAME);
  }

  public boolean isSingleAxisSelected() {
    return !isDual();
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

  static final class AttributeName {
    static final AttributeName SETUP_SET = new AttributeName();
    static final AttributeName COPY_ARG = new AttributeName();
    static final AttributeName RUN_TIME = new AttributeName();

    private AttributeName() {
    }
  }

  public static final class FiducialsSeedingMethod {
    public static final FiducialsSeedingMethod MANUAL = new FiducialsSeedingMethod("0");
    public static final FiducialsSeedingMethod AUTO_FID_SEED = new FiducialsSeedingMethod(
        "1");
    public static final FiducialsSeedingMethod TRANSFER_FID = new FiducialsSeedingMethod(
        "2");
    public static final FiducialsSeedingMethod BOTH = new FiducialsSeedingMethod("3");

    private final String value;

    private FiducialsSeedingMethod(final String value) {
      this.value = value;
    }

    private static FiducialsSeedingMethod getInstance(final String value) {
      if (value == null) {
        return null;
      }
      if (value.equals(MANUAL.value)) {
        return MANUAL;
      }
      if (value.equals(AUTO_FID_SEED.value)) {
        return AUTO_FID_SEED;
      }
      if (value.equals(TRANSFER_FID.value)) {
        return TRANSFER_FID;
      }
      if (value.equals(BOTH.value)) {
        return BOTH;
      }
      return null;
    }
  }

  public static final class FiducialsTrackingMethod {
    public static final FiducialsTrackingMethod SEED_AND_TRACK = new FiducialsTrackingMethod(
        "0");
    public static final FiducialsTrackingMethod PATCH_TRACK = new FiducialsTrackingMethod(
        "1");
    public static final FiducialsTrackingMethod RAPTOR = new FiducialsTrackingMethod("2");

    private final String value;

    private FiducialsTrackingMethod(final String value) {
      this.value = value;
    }

    private static FiducialsTrackingMethod getInstance(final String value) {
      if (value == null) {
        return null;
      }
      if (value.equals(SEED_AND_TRACK.value)) {
        return SEED_AND_TRACK;
      }
      if (value.equals(PATCH_TRACK.value)) {
        return PATCH_TRACK;
      }
      if (value.equals(RAPTOR.value)) {
        return RAPTOR;
      }
      return null;
    }
  }

}
