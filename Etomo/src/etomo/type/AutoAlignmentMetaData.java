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

  private static final String GROUP_KEY = "AutoAlignment";
  private static final EtomoVersion CURRENT_VERSION = EtomoVersion.getInstance(
      BaseMetaData.revisionNumberString, "1.0");
  private static final String ALIGN_TRANFORM_KEY = "AlignTransform";

  private final ScriptParameter sigmaLowFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaLowFrequency");
  private final ScriptParameter cutoffHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "CutoffHighFrequency");
  private final ScriptParameter sigmaHighFrequency = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "SigmaHighFrequency");
  private final EtomoNumber reduceByBinning = new EtomoNumber("ReduceByBinning");
  private final EtomoNumber edgeToIgnore = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "EdgeToIgnore");
  private final EtomoNumber midasBinning = new EtomoNumber("MidasBinning");
  private final ScriptParameter skipSectionsFrom1 = new ScriptParameter(
      "SkipSectionsFrom1");
  private final EtomoBoolean2 preCrossCorrelation = new EtomoBoolean2(
      "PreCrossCorrelation");
  private final EtomoNumber shiftLimitsForWarpX = new EtomoNumber("ShiftLimitsForWarp.X");
  private final EtomoNumber shiftLimitsForWarpY = new EtomoNumber("ShiftLimitsForWarp.Y");
  private final EtomoNumber warpPatchSizeX = new EtomoNumber("WarpPatchSize.X");
  private final EtomoNumber warpPatchSizeY = new EtomoNumber("WarpPatchSize.Y");
  private final EtomoBoolean2 boundaryModel = new EtomoBoolean2("BoundaryModel");
  private final EtomoBoolean2 findWarping = new EtomoBoolean2("FindWarping");

  private Transform alignTransform = Transform.DEFAULT;

  AutoAlignmentMetaData() {
    sigmaLowFrequency.setDefault(0).setDisplayValue(0.0);
    cutoffHighFrequency.setDefault(0).setDisplayValue(0.25);
    sigmaHighFrequency.setDefault(0).setDisplayValue(0.05);
    reduceByBinning.setDisplayValue(2);
    midasBinning.setDisplayValue(1);
    edgeToIgnore.setDisplayValue(.05);
  }

  private String createPrepend(final String prepend) {
    if (prepend.equals("")) {
      return GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY;
  }

  void load(final Properties props, String prepend) {
    prepend = createPrepend(prepend);
    // reset
    sigmaLowFrequency.reset();
    cutoffHighFrequency.reset();
    sigmaHighFrequency.reset();
    alignTransform = Transform.DEFAULT;
    reduceByBinning.reset();
    edgeToIgnore.reset();
    midasBinning.reset();
    skipSectionsFrom1.reset();
    preCrossCorrelation.reset();
    shiftLimitsForWarpX.reset();
    shiftLimitsForWarpY.reset();
    warpPatchSizeX.reset();
    warpPatchSizeY.reset();
    boundaryModel.reset();
    findWarping.reset();
    // load
    sigmaLowFrequency.load(props, prepend);
    cutoffHighFrequency.load(props, prepend);
    sigmaHighFrequency.load(props, prepend);
    alignTransform = Transform
        .load(props, prepend, ALIGN_TRANFORM_KEY, Transform.DEFAULT);
    reduceByBinning.load(props, prepend);
    edgeToIgnore.load(props, prepend);
    midasBinning.load(props, prepend);
    skipSectionsFrom1.load(props, prepend);
    preCrossCorrelation.load(props, prepend);
    shiftLimitsForWarpX.load(props, prepend);
    shiftLimitsForWarpY.load(props, prepend);
    warpPatchSizeX.load(props, prepend);
    warpPatchSizeY.load(props, prepend);
    boundaryModel.load(props, prepend);
    findWarping.load(props, prepend);
  }

  void store(final Properties props, String prepend) {
    prepend = createPrepend(prepend);
    CURRENT_VERSION.store(props, prepend);
    sigmaLowFrequency.store(props, prepend);
    cutoffHighFrequency.store(props, prepend);
    sigmaHighFrequency.store(props, prepend);
    Transform.store(alignTransform, props, prepend, ALIGN_TRANFORM_KEY);
    reduceByBinning.store(props, prepend);
    edgeToIgnore.store(props, prepend);
    midasBinning.store(props, prepend);
    skipSectionsFrom1.store(props, prepend);
    preCrossCorrelation.store(props, prepend);
    shiftLimitsForWarpX.store(props, prepend);
    shiftLimitsForWarpY.store(props, prepend);
    warpPatchSizeX.store(props, prepend);
    warpPatchSizeY.store(props, prepend);
    boundaryModel.store(props, prepend);
    findWarping.store(props, prepend);
  }

  public ConstEtomoNumber setSigmaLowFrequency(String sigmaLowFrequency) {
    return this.sigmaLowFrequency.set(sigmaLowFrequency);
  }

  void setSigmaLowFrequency(final ConstEtomoNumber input) {
    sigmaLowFrequency.set(input);
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

  void setCutoffHighFrequency(final ConstEtomoNumber input) {
    cutoffHighFrequency.set(input);
  }

  public ConstEtomoNumber getReduceByBinning() {
    return reduceByBinning;
  }

  public void setReduceByBinning(final Number input) {
    reduceByBinning.set(input);
  }

  public void setShiftLimitsForWarpX(final String input) {
    shiftLimitsForWarpX.set(input);
  }

  public String getShiftLimitsForWarpX() {
    return shiftLimitsForWarpX.toString();
  }

  public void setShiftLimitsForWarpY(final String input) {
    shiftLimitsForWarpY.set(input);
  }

  public String getShiftLimitsForWarpY() {
    return shiftLimitsForWarpY.toString();
  }

  public void setWarpPatchSizeX(final String input) {
    warpPatchSizeX.set(input);
  }

  public String getWarpPatchSizeX() {
    return warpPatchSizeX.toString();
  }

  public void setWarpPatchSizeY(final String input) {
    warpPatchSizeY.set(input);
  }

  public String getWarpPatchSizeY() {
    return warpPatchSizeY.toString();
  }

  public void setBoundaryModel(final boolean input) {
    boundaryModel.set(input);
  }

  public boolean isBoundaryModel() {
    return boundaryModel.is();
  }

  public void setFindWarping(final boolean input) {
    findWarping.set(input);
  }

  public boolean isFindWarping() {
    return findWarping.is();
  }

  public double getEdgeToIgnore() {
    return edgeToIgnore.getDouble();
  }

  public void setEdgeToIgnore(final String input) {
    edgeToIgnore.set(input);
  }

  public ConstEtomoNumber getMidasBinning() {
    return midasBinning;
  }

  public void setMidasBinning(final Number input) {
    midasBinning.set(input);
  }

  public String getSkipSectionsFrom1() {
    return skipSectionsFrom1.toString();
  }

  public void setSkipSectionsFrom1(final String input) {
    skipSectionsFrom1.set(input);
  }

  public boolean isPreCrossCorrelation() {
    return preCrossCorrelation.is();
  }

  public void setPreCrossCorrelation(final boolean input) {
    preCrossCorrelation.set(input);
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

  void setSigmaHighFrequency(final ConstEtomoNumber input) {
    sigmaHighFrequency.set(input);
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
