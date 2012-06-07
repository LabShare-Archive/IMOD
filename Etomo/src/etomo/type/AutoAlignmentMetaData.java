package etomo.type;

import java.util.Properties;

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
public final class AutoAlignmentMetaData {
  public static final String rcsid = "$Id:$";

  private static final String ALIGN_TRANFORM_KEY = "AlignTransform";
  // Version 1.0
  private static final String fullLinearTransformationString = "FullLinearTransformation";
  private static final String rotationTranslationMagnificationString = "RotationTranslationMagnification";
  private static final String rotationTranslationString = "RotationTranslation";

  private final ScriptParameter sigmaLowFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaLowFrequency");
  private final ScriptParameter cutoffHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "CutoffHighFrequency");
  private ScriptParameter sigmaHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaHighFrequency");
  private Transform alignTransform = Transform.DEFAULT;

  AutoAlignmentMetaData() {
    sigmaLowFrequency.setDefault(0).setDisplayValue(0.0);
    cutoffHighFrequency.setDefault(0).setDisplayValue(0.25);
    sigmaHighFrequency.setDefault(0).setDisplayValue(0.05);
  }

  void load(final Properties props, final String prependAndGroup,
      final EtomoVersion revisionNumber, final EtomoVersion latestRevisionNumber) {
    // reset
    sigmaLowFrequency.reset();
    cutoffHighFrequency.reset();
    sigmaHighFrequency.reset();
    alignTransform = Transform.DEFAULT;
    // load
    sigmaLowFrequency.load(props, prependAndGroup);
    cutoffHighFrequency.load(props, prependAndGroup);
    sigmaHighFrequency.load(props, prependAndGroup);
    if (revisionNumber.lt(latestRevisionNumber)) {
      // handling version 1.0
      loadVersion1_0(props, prependAndGroup);
    }
    else {
      alignTransform = Transform.load(props, prependAndGroup, ALIGN_TRANFORM_KEY,
          Transform.DEFAULT);
    }
  }

  private void loadVersion1_0(Properties props, String prependAndGroup) {
    String group = prependAndGroup + '.';
    if (Boolean.valueOf(
        props.getProperty(group + fullLinearTransformationString, "false"))
        .booleanValue()) {
      alignTransform = Transform.FULL_LINEAR_TRANSFORMATION;
    }
    else if (Boolean.valueOf(
        props.getProperty(group + rotationTranslationMagnificationString, "false"))
        .booleanValue()) {
      alignTransform = Transform.ROTATION_TRANSLATION_MAGNIFICATION;
    }
    else if (Boolean.valueOf(
        props.getProperty(group + rotationTranslationString, "false")).booleanValue()) {
      alignTransform = Transform.FULL_LINEAR_TRANSFORMATION;
    }
    else {
      alignTransform = Transform.DEFAULT;
    }
  }

  void store(Properties props, String prependAndGroup, final EtomoVersion revisionNumber,
      final EtomoVersion latestRevisionNumber) {
    // removing data used in old versions of join meta data
    // change this this when there are more then one old version
    if (revisionNumber.lt(latestRevisionNumber)) {
      removeVersion1_0(props, prependAndGroup);
    }
    sigmaLowFrequency.store(props, prependAndGroup);
    cutoffHighFrequency.store(props, prependAndGroup);
    sigmaHighFrequency.store(props, prependAndGroup);
    Transform.store(alignTransform, props, prependAndGroup, ALIGN_TRANFORM_KEY);
  }

  /**
   * Remove data not used after version 1.0 of join meta data.
   * Assumes the that data has been loaded (see loadVersion1_0()).
   * @param props
   * @param prepend
   */
  private void removeVersion1_0(Properties props, String prepend) {
    String group = prepend + '.';
    props.remove(group + fullLinearTransformationString);
    props.remove(group + rotationTranslationMagnificationString);
    props.remove(group + rotationTranslationString);
  }

  public ConstEtomoNumber setSigmaLowFrequency(String sigmaLowFrequency) {
    return this.sigmaLowFrequency.set(sigmaLowFrequency);
  }

  public ConstEtomoNumber getSigmaLowFrequency() {
    return sigmaLowFrequency;
  }

  public ScriptParameter getSigmaLowFrequencyParameter() {
    return sigmaLowFrequency;
  }

  public void setCutoffHighFrequency(String cutoffHighFrequency) {
    this.cutoffHighFrequency.set(cutoffHighFrequency);
  }

  public ConstEtomoNumber getCutoffHighFrequency() {
    return cutoffHighFrequency;
  }

  public ScriptParameter getCutoffHighFrequencyParameter() {
    return cutoffHighFrequency;
  }

  public void setSigmaHighFrequency(String sigmaHighFrequency) {
    this.sigmaHighFrequency.set(sigmaHighFrequency);
  }

  public ConstEtomoNumber getSigmaHighFrequency() {
    return sigmaHighFrequency;
  }

  public ScriptParameter getSigmaHighFrequencyParameter() {
    return sigmaHighFrequency;
  }

  public void setAlignTransform(Transform alignTransform) {
    this.alignTransform = alignTransform;
  }

  public Transform getAlignTransform() {
    return alignTransform;
  }
}
