package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.logic.SerialSectionsStartupData;

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
  private final StringProperty stackAbsolutePath = new StringProperty("StackAbsolutePath");
  private final StringProperty viewType = new StringProperty("ViewType");
  private final StringProperty distortionField = new StringProperty("DistortionField");
  private final EtomoNumber imagesAreBinned = new EtomoNumber("ImagesAreBinned");
  private final EtomoNumber robustFitCriterion = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "RobustFitCriterion");
  private final EtomoNumber midasBinning = new EtomoNumber("MidasBinning");
  private final EtomoNumber referenceSection = new EtomoNumber("ReferenceSection");
  private final EtomoBoolean2 hybridFitsTranslations = new EtomoBoolean2(
      "HybridFitsTranslations");
  private final EtomoBoolean2 hybridFitsTranslationsRotations = new EtomoBoolean2(
      "HybridFitsTranslationsRotations");
  private final EtomoBoolean2 noOptions = new EtomoBoolean2("NoOptions");
  private final EtomoBoolean2 numberToFitGlobalAlignment = new EtomoBoolean2(
      "NumberToFitGlobalAlignment");

  public SerialSectionsMetaData() {
    fileExtension = DataFileType.SERIAL_SECTIONS.extension;
    axisType = AxisType.SINGLE_AXIS;
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
    // reset
    rootName.reset();
    stackAbsolutePath.reset();
    viewType.reset();
    distortionField.reset();
    imagesAreBinned.reset();
    robustFitCriterion.reset();
    midasBinning.reset();
    referenceSection.reset();
    hybridFitsTranslations.reset();
    hybridFitsTranslationsRotations.reset();
    noOptions.reset();
    numberToFitGlobalAlignment.reset();
    // load
    prepend = createPrepend(prepend);
    autoAlignmentMetaData.load(props, prepend);
    rootName.load(props, prepend);
    stackAbsolutePath.load(props, prepend);
    viewType.load(props, prepend);
    distortionField.load(props, prepend);
    imagesAreBinned.load(props, prepend);
    robustFitCriterion.load(props, prepend);
    midasBinning.load(props, prepend);
    referenceSection.load(props, prepend);
    hybridFitsTranslations.load(props, prepend);
    hybridFitsTranslationsRotations.load(props, prepend);
    noOptions.load(props, prepend);
    numberToFitGlobalAlignment.load(props, prepend);
  }

  public void store(final Properties props, String prepend) {
    prepend = createPrepend(prepend);
    CURRENT_VERSION.store(props, prepend);
    autoAlignmentMetaData.store(props, prepend);
    rootName.store(props, prepend);
    stackAbsolutePath.store(props, prepend);
    viewType.store(props, prepend);
    distortionField.store(props, prepend);
    imagesAreBinned.store(props, prepend);
    robustFitCriterion.store(props, prepend);
    midasBinning.store(props, prepend);
    referenceSection.store(props, prepend);
    hybridFitsTranslations.store(props, prepend);
    hybridFitsTranslationsRotations.store(props, prepend);
    noOptions.store(props, prepend);
    numberToFitGlobalAlignment.store(props, prepend);
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    setName(startupData.getRootName());
    stackAbsolutePath.set(startupData.getStack().getAbsolutePath());
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

  public String getStackAbsolutePath() {
    return stackAbsolutePath.toString();
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

  public ConstEtomoNumber getReferenceSection() {
    return referenceSection;
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
  
  public void setNumberToFitGlobalAlignment(final boolean input) {
    numberToFitGlobalAlignment.set(input);
  }
}
