package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;
import etomo.type.ConstEtomoBoolean;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean;
import etomo.type.EtomoNumber;
import etomo.type.TiltAngleSpec;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltalign program</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.9  2005/01/12 00:41:12  sueh
 * <p> bug# 579 Make localAlignments available to the Command interface.
 * <p>
 * <p> Revision 3.8  2005/01/11 20:15:25  sueh
 * <p> bug# 567 Added fixXYZCoordinates, localMagReferenceView, and
 * <p> localOutputOptions.
 * <p>
 * <p> Revision 3.7  2005/01/11 01:00:07  sueh
 * <p> bug# 567 Adding storage for outputModelAndResidual, in case we want to
 * <p> use it.
 * <p>
 * <p> Revision 3.6  2005/01/08 01:36:02  sueh
 * <p> bug# 578 Removed getBinning() since its not needed by Command.
 * <p> Removed Command access to skewOption and xStretchOption.
 * <p> Add Command access to useOutputZFactorFile().
 * <p>
 * <p> Revision 3.5  2005/01/06 17:59:54  sueh
 * <p> bug# 578 Changed getIntegerValue() to give access to skewOption and
 * <p> xStretchOption  to Command interface.
 * <p>
 * <p> Revision 3.4  2005/01/05 18:56:00  sueh
 * <p> bug# 578 Adding AxisID to constructor.  Implementing Command.
 * <p>
 * <p> Revision 3.3  2004/12/29 23:30:51  sueh
 * <p> bug# 567 Corrected a parameter name in the constructor.
 * <p>
 * <p> Revision 3.2  2004/12/29 01:51:57  sueh
 * <p> bug# 567 Adding reset value for rotDefaultGrouping.  Getting
 * <p> rotNondefaultGroup as a string.
 * <p>
 * <p> Revision 3.1  2004/12/28 23:59:58  sueh
 * <p> bug# 567 Placed the version of ConstTiltalignParam for the old-style comscript
 * <p> into OldTiltalignParam.  This version contains only the new parameters,
 * <p> get functions, reset(), and validate().
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/10/14 20:30:43  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/24 01:09:41  rickg
 * <p> Min local patch size changed to double
 * <p>
 * <p> Revision 1.4  2002/12/18 19:07:09  rickg
 * <p> Added getters for metro factor and cycle limit
 * <p>
 * <p> Revision 1.3  2002/12/10 21:37:21  rickg
 * <p> changed reportStddevThreshold to residualThreshold
 * <p>
 * <p> Revision 1.2  2002/12/03 05:22:56  rickg
 * <p> added getLocalRotationSolutionGroupSize
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ConstTiltalignParam implements Command {
  public static final String rcsid =
    "$Id$";

  public static final int FIXED_OPTION = 0;
  public static final int NONE_OPTION = FIXED_OPTION;
  public static final int AUTOMAPPED_OPTION = 3;
  public static final int TILT_AUTOMAPPED_OPTION = 5;
  
  public static final int GET_USE_OUTPUT_Z_FACTOR_FILE = -1;
  public static final int GET_LOCAL_ALIGNMENTS = -2;
  
  protected static final String modelFileString = "ModelFile";
  protected static final String imageFileString = "ImageFile";
  protected static final String outputModelFileString = "OutputModelFile";
  protected static final String outputResidualFileString = "OutputResidualFile";
  protected static final String outputModelAndResidualString = "OutputModelAndResidual";
  protected static final String outputFidXYZFileString = "OutputFidXYZFile";
  protected static final String outputTiltFileString = "OutputTiltFile";
  protected static final String outputTransformFileString = "OutputTransformFile";
  protected static final String outputZFactorFileString = "OutputZFactorFile";
  protected static final String includeStartEndIncString = "IncludeStartEndInc";
  protected static final String includeListString = "IncludeList";
  protected static final String excludeListString = "ExcludeList";
  protected static final String separateGroupString = "SeparateGroup";
  protected static final String firstTiltAngleShortString = "first";
  protected static final String tiltIncrementShortString = "increment";
  protected static final String tiltFileShortString = "tiltFile";
  protected static final String rotNondefaultGroupString = "RotNondefaultGroup";
  protected static final String localRotNondefaultGroupString = "LocalRotNondefaultGroup";
  protected static final String tiltNondefaultGroupString = "TiltNondefaultGroup";
  protected static final String localTiltNondefaultGroupString = "LocalTiltNondefaultGroup";
  protected static final String magNondefaultGroupString = "MagNondefaultGroup";
  protected static final String localMagNondefaultGroupString = "LocalMagNondefaultGroup";
  protected static final String xStretchNondefaultGroupString = "XStretchNondefaultGroup";
  protected static final String localXStretchNondefaultGroupString = "LocalXStretchNondefaultGroup";
  protected static final String skewNondefaultGroupString = "SkewNondefaultGroup";
  protected static final String localSkewNondefaultGroupString = "LocalSkewNondefaultGroup";
  protected static final String outputLocalFileString = "OutputLocalFile";
  protected static final String numberOfLocalPatchesXandYString = "NumberOfLocalPatchesXandY";
  protected static final String minSizeOrOverlapXandYString = "MinSizeOrOverlapXandY";
  protected static final String minFidsTotalAndEachSurfaceString = "MinFidsTotalAndEachSurface";
  protected static final String localOutputOptionsString = "LocalOutputOptions";
  
  protected static final String modelFileExtension = ".3dmod";
  protected static final String residualFileExtension = ".resid";
  protected static final String zFactorFileExtension = ".zfac";
  protected static final boolean[] nondefaultGroupIntegerType = { true, true, true };
  protected static final int nondefaultGroupSize = 3;
  
  private static final int tiltAllOption = 2;
  private static final int allOption = 1;
  private static final int[] optionValidValues = { FIXED_OPTION, allOption, AUTOMAPPED_OPTION };
  private static final int[] tiltOptionValidValues = { FIXED_OPTION, tiltAllOption, TILT_AUTOMAPPED_OPTION };
  private static final int[] distortionOptionValidValues = { FIXED_OPTION, AUTOMAPPED_OPTION };
  private static final int[] localOptionValidValues = { FIXED_OPTION, AUTOMAPPED_OPTION };
  private static final int[] localTiltOptionValidValues = { FIXED_OPTION, TILT_AUTOMAPPED_OPTION };
  private static final int[] surfacesToAnalyzeValidValues = { 0, 1, 2 };
  private static final String commandFileName = "align";
  private static final String commandFileExtension = ".com";
  
  protected String modelFile;
  protected String imageFile;
  protected String outputModelAndResidual;
  protected String outputModelFile;
  protected String outputResidualFile;
  protected String outputFidXYZFile;
  protected String outputTiltFile;
  protected String outputTransformFile;
  protected String outputZFactorFile;
  protected FortranInputString includeStartEndInc;
  protected StringList includeList;
  protected StringList excludeList;
  protected EtomoNumber rotationAngle;
  protected StringList separateGroup;
  protected TiltAngleSpec tiltAngleSpec;
  protected EtomoNumber angleOffset;
  protected EtomoBoolean projectionStretch;
  protected EtomoNumber rotOption;
  protected EtomoNumber rotDefaultGrouping;
  protected FortranInputString[] rotNondefaultGroup;
  protected EtomoNumber rotationFixedView;
  protected EtomoNumber localRotOption;
  protected EtomoNumber localRotDefaultGrouping;
  protected FortranInputString[] localRotNondefaultGroup;
  protected EtomoNumber tiltOption;
  protected EtomoNumber tiltDefaultGrouping;
  protected FortranInputString[] tiltNondefaultGroup;
  protected EtomoNumber localTiltOption;
  protected EtomoNumber localTiltDefaultGrouping;
  protected FortranInputString[] localTiltNondefaultGroup;
  protected EtomoNumber magReferenceView;
  protected EtomoNumber magOption;
  protected EtomoNumber magDefaultGrouping;
  protected FortranInputString[] magNondefaultGroup;
  protected EtomoNumber localMagReferenceView;
  protected EtomoNumber localMagOption;
  protected EtomoNumber localMagDefaultGrouping;
  protected FortranInputString[] localMagNondefaultGroup;
  protected EtomoNumber xStretchOption;
  protected EtomoNumber xStretchDefaultGrouping;
  protected FortranInputString[] xStretchNondefaultGroup;
  protected EtomoNumber localXStretchOption;
  protected EtomoNumber localXStretchDefaultGrouping;
  protected FortranInputString[] localXStretchNondefaultGroup;
  protected EtomoNumber skewOption;
  protected EtomoNumber skewDefaultGrouping;
  protected FortranInputString[] skewNondefaultGroup;
  protected EtomoNumber localSkewOption;
  protected EtomoNumber localSkewDefaultGrouping;
  protected FortranInputString[] localSkewNondefaultGroup;
  protected EtomoNumber residualReportCriterion;
  protected EtomoNumber surfacesToAnalyze;
  protected EtomoNumber metroFactor;
  protected EtomoNumber maximumCycles;
  protected EtomoNumber axisZShift;
  protected EtomoBoolean localAlignments;
  protected String outputLocalFile;
  protected FortranInputString numberOfLocalPatchesXandY;
  protected FortranInputString minSizeOrOverlapXandY;
  protected FortranInputString minFidsTotalAndEachSurface;
  protected EtomoBoolean fixXYZCoordinates;
  protected FortranInputString localOutputOptions;
  
  protected AxisID axisID;
  protected String datasetName;

  public ConstTiltalignParam(String datasetName, AxisID axisID) {
    this.axisID = axisID;
    this.datasetName = datasetName;
    rotationAngle = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "RotationAngle");
    tiltAngleSpec = new TiltAngleSpec("FirstTiltAngle", "TiltIncrement", "TiltFile");
    angleOffset = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "AngleOffset");
    projectionStretch = new EtomoBoolean("ProjectionStretch");
    projectionStretch.setDefault(false).setDisplayDefault(true);
    rotOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "RotOption");
    rotOption.setValidValues(optionValidValues).setResetValue(AUTOMAPPED_OPTION);
    rotDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "RotDefaultGrouping");
    rotDefaultGrouping.setResetValue(3);
    rotationFixedView = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "RotationFixedView");
    localRotOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalRotOption");
    localRotOption.setValidValues(localOptionValidValues).setResetValue(AUTOMAPPED_OPTION);
    localRotDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalRotDefaultGrouping");
    localRotDefaultGrouping.setResetValue(6);
    tiltOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "TiltOption");
    tiltOption.setValidValues(tiltOptionValidValues).setResetValue(tiltAllOption);
    tiltDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "TiltDefaultGrouping");
    tiltDefaultGrouping.setResetValue(5);
    localTiltOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalTiltOption");
    localTiltOption.setValidValues(localTiltOptionValidValues).setResetValue(TILT_AUTOMAPPED_OPTION);
    localTiltDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalTiltDefaultGrouping");
    localTiltDefaultGrouping.setResetValue(6);
    magReferenceView = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "MagReferenceView");
    magOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "MagOption");
    magOption.setValidValues(optionValidValues).setResetValue(allOption);
    magDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "MagDefaultGrouping");
    magDefaultGrouping.setResetValue(4);
    localMagReferenceView = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalMagReferenceView");
    localMagOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalMagOption");
    localMagOption.setValidValues(localOptionValidValues).setResetValue(AUTOMAPPED_OPTION);
    localMagDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalMagDefaultGrouping");
    localMagDefaultGrouping.setResetValue(7);
    xStretchOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "XStretchOption");
    xStretchOption.setValidValues(distortionOptionValidValues).setResetValue(NONE_OPTION);
    xStretchDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "XStretchDefaultGrouping");
    xStretchDefaultGrouping.setResetValue(7);
    localXStretchOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalXStretchOption");
    localXStretchOption.setValidValues(localOptionValidValues).setResetValue(AUTOMAPPED_OPTION);
    localXStretchDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalXStretchDefaultGrouping");
    localXStretchDefaultGrouping.setResetValue(7);
    skewOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SkewOption");
    skewOption.setValidValues(distortionOptionValidValues).setResetValue(NONE_OPTION);
    skewDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SkewDefaultGrouping");
    skewDefaultGrouping.setResetValue(11);
    localSkewOption = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalSkewOption");
    localSkewOption.setValidValues(optionValidValues).setResetValue(AUTOMAPPED_OPTION);
    localSkewDefaultGrouping = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "LocalSkewDefaultGrouping");
    localSkewDefaultGrouping.setResetValue(11);
    residualReportCriterion = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "ResidualReportCriterion");
    surfacesToAnalyze = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "SurfacesToAnalyze");
    surfacesToAnalyze.setValidValues(surfacesToAnalyzeValidValues);
    metroFactor = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "MetroFactor");
    maximumCycles = new EtomoNumber(EtomoNumber.INTEGER_TYPE, "MaximumCycles");
    maximumCycles.setDefault(500).setDisplayDefault(true);
    axisZShift = new EtomoNumber(EtomoNumber.DOUBLE_TYPE, "AxisZShift");
    localAlignments = new EtomoBoolean("LocalAlignments");
    localAlignments.setUpdateAs(EtomoBoolean.UPDATE_AS_INTEGER).setResetValue(false);
    fixXYZCoordinates = new EtomoBoolean("FixXYZCoordinates");
    reset();
  }
  
  protected void reset() {
    modelFile = "";
    imageFile = "";
    outputModelAndResidual = "";
    outputModelFile = "";
    outputResidualFile = "";
    outputFidXYZFile = "";
    outputTiltFile = "";
    outputTransformFile = "";
    outputZFactorFile = "";
    includeStartEndInc = new FortranInputString(3);
    includeStartEndInc.setIntegerType(new boolean[] {true, true, true});
    includeList = new StringList();
    excludeList = new StringList();
    rotationAngle.reset();
    separateGroup = new StringList();
    tiltAngleSpec.reset();
    angleOffset.reset();
    projectionStretch.reset();
    rotOption.reset();
    rotDefaultGrouping.reset();
    rotNondefaultGroup = null;
    rotationFixedView.reset();
    localRotOption.reset();
    localRotDefaultGrouping.reset();
    localRotNondefaultGroup = null;
    tiltOption.reset();
    tiltDefaultGrouping.reset();
    tiltNondefaultGroup = null;
    localTiltOption.reset();
    localTiltDefaultGrouping.reset();
    localTiltNondefaultGroup = null;
    magReferenceView.reset();
    magOption.reset();
    magDefaultGrouping.reset();
    magNondefaultGroup = null;
    localMagOption.reset();
    localMagDefaultGrouping.reset();
    localMagNondefaultGroup = null;
    xStretchOption.reset();
    xStretchDefaultGrouping.reset();
    xStretchNondefaultGroup = null;
    localXStretchOption.reset();
    localXStretchDefaultGrouping.reset();
    localXStretchNondefaultGroup = null;
    skewOption.reset();
    skewDefaultGrouping.reset();
    skewNondefaultGroup = null;
    localSkewOption.reset();
    localSkewDefaultGrouping.reset();
    localSkewNondefaultGroup = null;
    residualReportCriterion.reset();
    surfacesToAnalyze.reset();
    metroFactor.reset();
    maximumCycles.reset();
    axisZShift.reset();
    localAlignments.reset();
    outputLocalFile = "";
    numberOfLocalPatchesXandY = new FortranInputString(2);
    numberOfLocalPatchesXandY.setIntegerType(new boolean[] {true, true});
    minSizeOrOverlapXandY = new FortranInputString(2);
    minFidsTotalAndEachSurface = new FortranInputString(2);
    minFidsTotalAndEachSurface.setIntegerType(new boolean[] {true, true});
    fixXYZCoordinates.reset();
    localOutputOptions = new FortranInputString(3);
    localOutputOptions.setIntegerType(new boolean[] {true, true, true});
  }
  
  protected String validate() {
    StringBuffer invalidReason = new StringBuffer();
    if (!rotOption.isValid()) {
      invalidReason.append(rotOption.getInvalidReason() + "\n");
    }
    if (!localRotOption.isValid()) {
      invalidReason.append(localRotOption.getInvalidReason() + "\n");
    }
    if (!tiltOption.isValid()) {
      invalidReason.append(tiltOption.getInvalidReason() + "\n");
    }
    if (!localTiltOption.isValid()) {
      invalidReason.append(localTiltOption.getInvalidReason() + "\n");
    }
    if (!magOption.isValid()) {
      invalidReason.append(magOption.getInvalidReason() + "\n");
    }
    if (!localMagOption.isValid()) {
      invalidReason.append(localMagOption.getInvalidReason() + "\n");
    }
    if (!xStretchOption.isValid()) {
      invalidReason.append(xStretchOption.getInvalidReason() + "\n");
    }
    if (!localXStretchOption.isValid()) {
      invalidReason.append(localXStretchOption.getInvalidReason() + "\n");
    }
    if (!skewOption.isValid()) {
      invalidReason.append(skewOption.getInvalidReason() + "\n");
    }
    if (!localSkewOption.isValid()) {
      invalidReason.append(localSkewOption.getInvalidReason() + "\n");
    }
    if (!surfacesToAnalyze.isValid()) {
      invalidReason.append(surfacesToAnalyze.getInvalidReason() + "\n");
    }
    return invalidReason.toString();
  }

  public boolean isExcludeListAvailable() {
    return includeStartEndInc.isDefault() && includeList.getNElements() == 0;
  }

  
  public String getCommandLine() {
    return commandFileName + axisID.getExtension() + commandFileExtension;
  }
  public String getCommandName() {
    return commandFileName;
  }
  
  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }
  
  public int getCommandMode() {
    return 0;
  }
  
  public File getCommandOutputFile() {
    return null;
  }
  
  public int getIntegerValue(int name) {
    return EtomoNumber.INTEGER_NULL_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    switch (name) {
    case GET_USE_OUTPUT_Z_FACTOR_FILE:
      return useOutputZFactorFile();
    case GET_LOCAL_ALIGNMENTS:
      return localAlignments.is();
    }
    return false;
  }
  

  
  /**
   * @return Returns the angleOffset.
   */
  public ConstEtomoNumber getAngleOffset() {
    return angleOffset;
  }
  /**
   * @return Returns the axisZShift.
   */
  public ConstEtomoNumber getAxisZShift() {
    return axisZShift;
  }
  /**
   * @return Returns the excludeList.
   */
  public String getExcludeList() {
    return excludeList.toString();
  }
  /**
   * 
   * @return
   */
  public ConstEtomoBoolean getFixXYZCoordinates() {
    return fixXYZCoordinates;
  }
  /**
   * @return Returns the imageFile.
   */
  public String getImageFile() {
    return imageFile;
  }
  /**
   * @return Returns the localAlignments.
   */
  public ConstEtomoBoolean getLocalAlignments() {
    return localAlignments;
  }
  /**
   * @return Returns the localMagDefaultGrouping.
   */
  public ConstEtomoNumber getLocalMagDefaultGrouping() {
    return localMagDefaultGrouping;
  }
  /**
   * @return Returns the localMagNondefaultGroup.
   */
  public String getLocalMagNondefaultGroup() {
    return ParamUtilities.valueOf(localMagNondefaultGroup);
  }
  /**
   * @return Returns the localMagOption.
   */
  public ConstEtomoNumber getLocalMagOption() {
    return localMagOption;
  }
  /**
   * @return Returns the localRotDefaultGrouping.
   */
  public ConstEtomoNumber getLocalRotDefaultGrouping() {
    return localRotDefaultGrouping;
  }
  /**
   * @return Returns the localRotNondefaultGroup.
   */
  public String getLocalRotNondefaultGroup() {
    return ParamUtilities.valueOf(localRotNondefaultGroup);
  }
  /**
   * @return Returns the localRotOption.
   */
  public ConstEtomoNumber getLocalRotOption() {
    return localRotOption;
  }
  /**
   * @return Returns the localSkewDefaultGrouping.
   */
  public ConstEtomoNumber getLocalSkewDefaultGrouping() {
    return localSkewDefaultGrouping;
  }
  /**
   * @return Returns the localSkewNondefaultGroup.
   */
  public String getLocalSkewNondefaultGroup() {
    return ParamUtilities.valueOf(localSkewNondefaultGroup);
  }
  /**
   * @return Returns the localSkewOption.
   */
  public ConstEtomoNumber getLocalSkewOption() {
    return localSkewOption;
  }
  /**
   * @return Returns the localTiltDefaultGrouping.
   */
  public ConstEtomoNumber getLocalTiltDefaultGrouping() {
    return localTiltDefaultGrouping;
  }
  /**
   * @return Returns the localTiltNondefaultGroup.
   */
  public String getLocalTiltNondefaultGroup() {
    return ParamUtilities.valueOf(localTiltNondefaultGroup);
  }
  /**
   * @return Returns the localTiltOption.
   */
  public ConstEtomoNumber getLocalTiltOption() {
    return localTiltOption;
  }
  /**
   * @return Returns the localXStretchDefaultGrouping.
   */
  public ConstEtomoNumber getLocalXStretchDefaultGrouping() {
    return localXStretchDefaultGrouping;
  }
  /**
   * @return Returns the localXStretchNondefaultGroup.
   */
  public String getLocalXStretchNondefaultGroup() {
    return ParamUtilities.valueOf(localXStretchNondefaultGroup);
  }
  /**
   * @return Returns the localXStretchOption.
   */
  public ConstEtomoNumber getLocalXStretchOption() {
    return localXStretchOption;
  }
  /**
   * @return Returns the magDefaultGrouping.
   */
  public ConstEtomoNumber getMagDefaultGrouping() {
    return magDefaultGrouping;
  }
  /**
   * @return Returns the magNondefaultGroup.
   */
  public String getMagNondefaultGroup() {
    return ParamUtilities.valueOf(magNondefaultGroup);
  }
  /**
   * @return Returns the magOption.
   */
  public ConstEtomoNumber getMagOption() {
    return magOption;
  }
  /**
   * @return Returns the magReferenceView.
   */
  public ConstEtomoNumber getMagReferenceView() {
    return magReferenceView;
  }
  /**
   * @return Returns the maximumCycles.
   */
  public ConstEtomoNumber getMaximumCycles() {
    return maximumCycles;
  }
  /**
   * @return Returns the metroFactor.
   */
  public ConstEtomoNumber getMetroFactor() {
    return metroFactor;
  }
  /**
   * @return Returns the minFidsTotalAndEachSurface.
   */
  public String getMinFidsTotalAndEachSurface() {
    return minFidsTotalAndEachSurface.toString(true);
  }
  /**
   * @return Returns the minSizeOrOverlapXandY.
   */
  public String getMinSizeOrOverlapXandY() {
    return minSizeOrOverlapXandY.toString(true);
  }
  /**
   * @return Returns the modelFile.
   */
  public String getModelFile() {
    return modelFile;
  }
  /**
   * @return Returns the numberOfLocalPatchesXandY.
   */
  public String getNumberOfLocalPatchesXandY() {
    return numberOfLocalPatchesXandY.toString(true);
  }
  /**
   * @return Returns the outputFidXYZFile.
   */
  public String getOutputFidXYZFile() {
    return outputFidXYZFile;
  }
  /**
   * @return Returns the outputLocalFile.
   */
  public String getOutputLocalFile() {
    return outputLocalFile;
  }
  /**
   * @return Returns the outputModelFile.
   */
  public String getOutputModelFile() {
    return outputModelFile;
  }
  /**
   * @return Returns the outputResidualFile.
   */
  public String getOutputResidualFile() {
    return outputResidualFile;
  }
  /**
   * @return Returns the outputTiltFile.
   */
  public String getOutputTiltFile() {
    return outputTiltFile;
  }
  /**
   * @return Returns the outputTransformFile.
   */
  public String getOutputTransformFile() {
    return outputTransformFile;
  }
  
  
  /**
   * This must called after skewOption, or localAlignment, and localSkewOption
   * have been set.
   * @return
   */
  public boolean useOutputZFactorFile() {
    return !skewOption.equals(FIXED_OPTION)
        || (localAlignments.is() && !localSkewOption.equals(FIXED_OPTION));
  }
  
  /**
   * @return Returns the outputZFactorFile.
   */
  public String getOutputZFactorFile() {
    return outputZFactorFile;
  }
  
  /**
   * build an outputZFactorFile value from datasetName and axisID
   * @param datasetName
   * @param axisID
   * @return
   */
  public static String getOutputZFactorFileName(String datasetName, AxisID axisID) {
    return datasetName + axisID.getExtension() + zFactorFileExtension;
  }
  
  /**
   * @return Returns the projectionStretch.
   */
  public ConstEtomoBoolean getProjectionStretch() {
    return projectionStretch;
  }
  /**
   * @return Returns the residualReportCriterion.
   */
  public ConstEtomoNumber getResidualReportCriterion() {
    return residualReportCriterion;
  }
  /**
   * @return Returns the rotationAngle.
   */
  public ConstEtomoNumber getRotationAngle() {
    return rotationAngle;
  }
  /**
   * @return Returns the rotationFixedView.
   */
  public ConstEtomoNumber getRotationFixedView() {
    return rotationFixedView;
  }
  /**
   * @return Returns the rotDefaultGrouping.
   */
  public ConstEtomoNumber getRotDefaultGrouping() {
    return rotDefaultGrouping;
  }
  /**
   * @return Returns the rotNondefaultGroup.
   */
  public String getRotNondefaultGroup() {
    return ParamUtilities.valueOf(rotNondefaultGroup);
  }
  /**
   * @return Returns the rotOption.
   */
  public ConstEtomoNumber getRotOption() {
    return rotOption;
  }
  /**
   * @return Returns the separateGroup.
   */
  public String getSeparateGroup() {
    return separateGroup.toString();
  }
  /**
   * @return Returns the skewDefaultGrouping.
   */
  public ConstEtomoNumber getSkewDefaultGrouping() {
    return skewDefaultGrouping;
  }
  /**
   * @return Returns the skewNondefaultGroup.
   */
  public String getSkewNondefaultGroup() {
    return ParamUtilities.valueOf(skewNondefaultGroup);
  }
  /**
   * @return Returns the skewOption.
   */
  public ConstEtomoNumber getSkewOption() {
    return skewOption;
  }
  /**
   * @return Returns the surfacesToAnalyze.
   */
  public ConstEtomoNumber getSurfacesToAnalyze() {
    return surfacesToAnalyze;
  }
  /**
   * @return Returns the tiltAngleSpec.
   */
  public TiltAngleSpec getTiltAngleSpec() {
    return tiltAngleSpec;
  }
  /**
   * @return Returns the tiltDefaultGrouping.
   */
  public ConstEtomoNumber getTiltDefaultGrouping() {
    return tiltDefaultGrouping;
  }
  /**
   * @return Returns the tiltNondefaultGroup.
   */
  public String getTiltNondefaultGroup() {
    return ParamUtilities.valueOf(tiltNondefaultGroup);
  }
  /**
   * @return Returns the tiltOption.
   */
  public ConstEtomoNumber getTiltOption() {
    return tiltOption;
  }
  /**
   * @return Returns the xStretchDefaultGrouping.
   */
  public ConstEtomoNumber getXStretchDefaultGrouping() {
    return xStretchDefaultGrouping;
  }
  /**
   * @return Returns the xStretchNondefaultGroup.
   */
  public String getXStretchNondefaultGroup() {
    return ParamUtilities.valueOf(xStretchNondefaultGroup);
  }
  /**
   * @return Returns the xStretchOption.
   */
  public ConstEtomoNumber getXStretchOption() {
    return xStretchOption;
  }
  
}
