package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.logic.SerialSectionsStartupData;
import etomo.ui.LogProperties;

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
public final class SerialSectionsMetaData extends BaseMetaData implements
    ConstSerialSectionsMetaData {
  public static final String rcsid = "$Id:$";

  public static final String NEW_TITLE = "Serial Sections";
  private static final EtomoVersion CURRENT_VERSION = EtomoVersion.getInstance(
      BaseMetaData.revisionNumberString, "1.0");

  private final StringProperty rootName = new StringProperty("RootName");
  private final AutoAlignmentMetaData autoAlignmentMetaData = new AutoAlignmentMetaData();
  private final StringProperty stack = new StringProperty("Stack");
  private final StringProperty viewType = new StringProperty("ViewType");
  private final StringProperty distortionField = new StringProperty("DistortionField");
  private final EtomoNumber imagesAreBinned = new EtomoNumber("ImagesAreBinned");
  private final EtomoNumber robustFitCriterion = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "RobustFitCriterion");
  private final EtomoNumber midasBinning = new EtomoNumber("MidasBinning");
  private EtomoBoolean2 useReferenceSection = new EtomoBoolean2("UseReferenceSection");
  private final EtomoNumber referenceSection = new EtomoNumber("ReferenceSection");
  private final EtomoBoolean2 hybridFitsTranslations = new EtomoBoolean2(
      "HybridFitsTranslations");
  private final EtomoBoolean2 hybridFitsTranslationsRotations = new EtomoBoolean2(
      "HybridFitsTranslationsRotations");
  private final EtomoBoolean2 noOptions = new EtomoBoolean2("NoOptions");
  private final EtomoBoolean2 numberToFitGlobalAlignment = new EtomoBoolean2(
      "NumberToFitGlobalAlignment");
  private final EtomoNumber shiftX = new EtomoNumber(EtomoNumber.Type.DOUBLE, "ShiftX");
  private final EtomoNumber shiftY = new EtomoNumber(EtomoNumber.Type.DOUBLE, "ShiftY");
  private final EtomoNumber sizeX = new EtomoNumber("SizeX");
  private final EtomoNumber sizeY = new EtomoNumber("SizeY");
  private final EtomoNumber tab = new EtomoNumber("Tab");

  public SerialSectionsMetaData(final LogProperties logProperties) {
    super(logProperties);
    fileExtension = DataFileType.SERIAL_SECTIONS.extension;
    axisType = AxisType.SINGLE_AXIS;
    noOptions.setDisplayValue(true);
    robustFitCriterion.setDisplayValue(1);
  }

  public void setName(final String rootName) {
    this.rootName.set(rootName);
  }

  String getGroupKey() {
    return "SerialSections";
  }

  public String getDatasetName() {
    return rootName.toString();
  }

  public String getMetaDataFileName() {
    if (rootName.equals("")) {
      return null;
    }
    return rootName + fileExtension;
  }

  public void load(final Properties props, String prepend) {
    super.load(props,prepend);
    // reset
    rootName.reset();
    stack.reset();
    viewType.reset();
    distortionField.reset();
    imagesAreBinned.reset();
    robustFitCriterion.reset();
    midasBinning.reset();
    useReferenceSection.reset();
    referenceSection.reset();
    hybridFitsTranslations.reset();
    hybridFitsTranslationsRotations.reset();
    noOptions.reset();
    numberToFitGlobalAlignment.reset();
    shiftX.reset();
    shiftY.reset();
    sizeX.reset();
    sizeY.reset();
    tab.reset();
    // load
    prepend = createPrepend(prepend);
    autoAlignmentMetaData.load(props, prepend);
    rootName.load(props, prepend);
    stack.load(props, prepend);
    viewType.load(props, prepend);
    distortionField.load(props, prepend);
    imagesAreBinned.load(props, prepend);
    robustFitCriterion.load(props, prepend);
    midasBinning.load(props, prepend);
    useReferenceSection.load(props, prepend);
    referenceSection.load(props, prepend);
    hybridFitsTranslations.load(props, prepend);
    hybridFitsTranslationsRotations.load(props, prepend);
    noOptions.load(props, prepend);
    numberToFitGlobalAlignment.load(props, prepend);
    shiftX.load(props, prepend);
    shiftY.load(props, prepend);
    sizeX.load(props, prepend);
    sizeY.load(props, prepend);
    tab.load(props, prepend);
  }

  public void store(final Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    CURRENT_VERSION.store(props, prepend);
    autoAlignmentMetaData.store(props, prepend);
    rootName.store(props, prepend);
    stack.store(props, prepend);
    viewType.store(props, prepend);
    distortionField.store(props, prepend);
    imagesAreBinned.store(props, prepend);
    robustFitCriterion.store(props, prepend);
    midasBinning.store(props, prepend);
    useReferenceSection.store(props, prepend);
    referenceSection.store(props, prepend);
    hybridFitsTranslations.store(props, prepend);
    hybridFitsTranslationsRotations.store(props, prepend);
    noOptions.store(props, prepend);
    numberToFitGlobalAlignment.store(props, prepend);
    shiftX.store(props, prepend);
    shiftY.store(props, prepend);
    sizeX.store(props, prepend);
    sizeY.store(props, prepend);
    tab.store(props, prepend);
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    setName(startupData.getRootName());
    stack.set(startupData.getStack().getName());
    viewType.set(startupData.getViewType().getParamValue());
    File file = startupData.getDistortionField();
    if (file == null) {
      distortionField.reset();
    }
    else {
      distortionField.set(file.getAbsolutePath());
    }
    imagesAreBinned.set(startupData.getImagesAreBinned());
  }

  public String getName() {
    if (rootName.toString() == null || rootName.toString().matches("\\s*")) {
      return NEW_TITLE;
    }
    return rootName.toString();
  }

  public boolean isValid() {
    return true;
  }

  public AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return autoAlignmentMetaData;
  }

  public String getStack() {
    return stack.toString();
  }

  public boolean isTabEmpty() {
    return tab.isNull();
  }

  public int getTab() {
    return tab.getInt();
  }

  public String getDistortionField() {
    return distortionField.toString();
  }

  public String getImagesAreBinned() {
    return imagesAreBinned.toString();
  }

  public ConstEtomoNumber getMidasBinning() {
    return midasBinning;
  }

  public void setMidasBinning(final Number input) {
    midasBinning.set(input);
  }

  public boolean isUseReferenceSection() {
    return useReferenceSection.is();
  }

  public ConstEtomoNumber getReferenceSection() {
    return referenceSection;
  }

  public void setUseReferenceSection(final boolean input) {
    useReferenceSection.set(input);
  }

  public void setReferenceSection(final Number input) {
    referenceSection.set(input);
  }

  public String getRobustFitCriterion() {
    return robustFitCriterion.toString();
  }

  public void setRobustFitCriterion(final String input) {
    robustFitCriterion.set(input);
  }

  public void setSizeX(final String input) {
    sizeX.set(input);
  }

  public void setSizeY(final String input) {
    sizeY.set(input);
  }

  public void setShiftX(final String input) {
    shiftX.set(input);
  }

  public void setShiftY(final String input) {
    shiftY.set(input);
  }

  public void setTab(final int input) {
    tab.set(input);
  }

  public ViewType getViewType() {
    return ViewType.fromString(viewType.toString());
  }

  public boolean isHybridFitsTranslations() {
    return hybridFitsTranslations.is();
  }

  public void setHybridFitsTranslations(final boolean input) {
    hybridFitsTranslations.set(input);
  }

  public boolean isHybridFitsTranslationsRotations() {
    return hybridFitsTranslationsRotations.is();
  }

  public void setHybridFitsTranslationsRotations(final boolean input) {
    hybridFitsTranslationsRotations.set(input);
  }

  public boolean isNoOptions() {
    return noOptions.is();
  }

  public void setNoOptions(final boolean input) {
    noOptions.set(input);
  }

  public boolean isNumberToFitGlobalAlignment() {
    return numberToFitGlobalAlignment.is();
  }

  public String getShiftX() {
    return shiftX.toString();
  }

  public String getShiftY() {
    return shiftY.toString();
  }

  public String getSizeX() {
    return sizeX.toString();
  }

  public String getSizeY() {
    return sizeY.toString();
  }

  public void setNumberToFitGlobalAlignment(final boolean input) {
    numberToFitGlobalAlignment.set(input);
  }
}
