package etomo.comscript;

import java.io.File;

import etomo.type.AxisID;
import etomo.type.ConstEtomoBoolean;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;
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
 * <p> Revision 3.11  2005/01/13 19:01:28  sueh
 * <p> bug# 567 Changed isExcludeListAvailable():  taking the null value and the
 * <p> default value into account.
 * <p>
 * <p> Revision 3.10  2005/01/13 00:44:45  sueh
 * <p> bug# 576 Converted includeStartEndInc to FortranInputString.
 * <p>
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
  protected static final String localFileExtension = "local.xf";
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
  protected ScriptParameter rotationAngle;
  protected StringList separateGroup;
  protected TiltAngleSpec tiltAngleSpec;
  protected ScriptParameter angleOffset;
  protected EtomoBoolean2 projectionStretch;
  protected ScriptParameter rotOption;
  protected ScriptParameter rotDefaultGrouping;
  protected FortranInputString[] rotNondefaultGroup;
  protected ScriptParameter rotationFixedView;
  protected ScriptParameter localRotOption;
  protected ScriptParameter localRotDefaultGrouping;
  protected FortranInputString[] localRotNondefaultGroup;
  protected ScriptParameter tiltOption;
  protected ScriptParameter tiltDefaultGrouping;
  protected FortranInputString[] tiltNondefaultGroup;
  protected ScriptParameter localTiltOption;
  protected ScriptParameter localTiltDefaultGrouping;
  protected FortranInputString[] localTiltNondefaultGroup;
  protected ScriptParameter magReferenceView;
  protected ScriptParameter magOption;
  protected ScriptParameter magDefaultGrouping;
  protected FortranInputString[] magNondefaultGroup;
  protected ScriptParameter localMagReferenceView;
  protected ScriptParameter localMagOption;
  protected ScriptParameter localMagDefaultGrouping;
  protected FortranInputString[] localMagNondefaultGroup;
  protected ScriptParameter xStretchOption;
  protected ScriptParameter xStretchDefaultGrouping;
  protected FortranInputString[] xStretchNondefaultGroup;
  protected ScriptParameter localXStretchOption;
  protected ScriptParameter localXStretchDefaultGrouping;
  protected FortranInputString[] localXStretchNondefaultGroup;
  protected ScriptParameter skewOption;
  protected ScriptParameter skewDefaultGrouping;
  protected FortranInputString[] skewNondefaultGroup;
  protected ScriptParameter localSkewOption;
  protected ScriptParameter localSkewDefaultGrouping;
  protected FortranInputString[] localSkewNondefaultGroup;
  protected ScriptParameter residualReportCriterion;
  protected ScriptParameter surfacesToAnalyze;
  protected ScriptParameter metroFactor;
  protected ScriptParameter maximumCycles;
  protected ScriptParameter axisZShift;
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
    rotationAngle = new ScriptParameter(EtomoNumber.DOUBLE_TYPE, "RotationAngle");
    tiltAngleSpec = new TiltAngleSpec("FirstTiltAngle", "TiltIncrement", "TiltFile");
    angleOffset = new ScriptParameter(EtomoNumber.DOUBLE_TYPE, "AngleOffset");
    projectionStretch = new EtomoBoolean2("ProjectionStretch");
    projectionStretch.setDefault(false).setDisplayValue(false);
    rotOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "RotOption");
    rotOption.setValidValues(optionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    rotDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "RotDefaultGrouping");
    rotDefaultGrouping.setDisplayValue(3);
    rotationFixedView = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "RotationFixedView");
    localRotOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalRotOption");
    localRotOption.setValidValues(localOptionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    localRotDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalRotDefaultGrouping");
    localRotDefaultGrouping.setDisplayValue(6);
    tiltOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "TiltOption");
    tiltOption.setValidValues(tiltOptionValidValues).setDisplayValue(tiltAllOption);
    tiltDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "TiltDefaultGrouping");
    tiltDefaultGrouping.setDisplayValue(5);
    localTiltOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalTiltOption");
    localTiltOption.setValidValues(localTiltOptionValidValues).setDisplayValue(TILT_AUTOMAPPED_OPTION);
    localTiltDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalTiltDefaultGrouping");
    localTiltDefaultGrouping.setDisplayValue(6);
    magReferenceView = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "MagReferenceView");
    magOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "MagOption");
    magOption.setValidValues(optionValidValues).setDisplayValue(allOption);
    magDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "MagDefaultGrouping");
    magDefaultGrouping.setDisplayValue(4);
    localMagReferenceView = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalMagReferenceView");
    localMagOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalMagOption");
    localMagOption.setValidValues(localOptionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    localMagDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalMagDefaultGrouping");
    localMagDefaultGrouping.setDisplayValue(7);
    xStretchOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "XStretchOption");
    xStretchOption.setValidValues(distortionOptionValidValues).setDisplayValue(NONE_OPTION);
    xStretchDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "XStretchDefaultGrouping");
    xStretchDefaultGrouping.setDisplayValue(7);
    localXStretchOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalXStretchOption");
    localXStretchOption.setValidValues(localOptionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    localXStretchDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalXStretchDefaultGrouping");
    localXStretchDefaultGrouping.setDisplayValue(7);
    skewOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "SkewOption");
    skewOption.setValidValues(distortionOptionValidValues).setDisplayValue(NONE_OPTION);
    skewDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "SkewDefaultGrouping");
    skewDefaultGrouping.setDisplayValue(11);
    localSkewOption = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalSkewOption");
    localSkewOption.setValidValues(optionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    localSkewDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "LocalSkewDefaultGrouping");
    localSkewDefaultGrouping.setDisplayValue(11);
    residualReportCriterion = new ScriptParameter(EtomoNumber.DOUBLE_TYPE, "ResidualReportCriterion");
    surfacesToAnalyze = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "SurfacesToAnalyze");
    surfacesToAnalyze.setValidValues(surfacesToAnalyzeValidValues);
    metroFactor = new ScriptParameter(EtomoNumber.DOUBLE_TYPE, "MetroFactor");
    maximumCycles = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "MaximumCycles");
    maximumCycles.setDefault(500).setDisplayValue(500);
    axisZShift = new ScriptParameter(EtomoNumber.DOUBLE_TYPE, "AxisZShift");
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
    return (includeStartEndInc.isDefault() || !includeStartEndInc.valuesSet()) && includeList.getNElements() == 0;
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

  public static String getOutputLocalFileName(String datasetName, AxisID axisID) {
    return datasetName + axisID.getExtension() + localFileExtension;
  }
  
  /**
   * @return Returns the projectionStretch.
   */
  public ConstEtomoNumber getProjectionStretch() {
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
