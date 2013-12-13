package etomo.comscript;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
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
 */
public final class TiltalignParam extends ConstTiltalignParam implements CommandParam {
  public static final String rcsid = "$Id$";

  /**
   * Constructor for TiltalignParam.
   */
  public TiltalignParam(final BaseManager manager, final String datasetName,
      final AxisID axisID) {
    super(manager, datasetName, axisID);
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tiltalign command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    if (!scriptCommand.isKeywordValuePairs()) {
      OldTiltalignParam oldParam = new OldTiltalignParam();
      oldParam.parseComScriptCommand(scriptCommand);
      convertToPIP(oldParam);
    }
    else {
      imagesAreBinned.parse(scriptCommand);
      modelFile = scriptCommand.getValue(modelFileString);
      imageFile = scriptCommand.getValue(imageFileString);
      outputModelAndResidual = scriptCommand.getValue(outputModelAndResidualString);
      outputModelFile = scriptCommand.getValue(outputModelFileString);
      outputResidualFile = scriptCommand.getValue(outputResidualFileString);

      //Use OutputModelAndResidual if OutputModelFile or OutputResidualFile are blank
      //Convert OutputModelAndResidual to OutputModelFile and OutputResidualFile
      if (outputModelFile.matches("\\s*") || outputResidualFile.matches("\\s*")) {
        String outputModelAndResidual = scriptCommand
            .getValue(outputModelAndResidualString);
        if (outputModelFile.matches("\\s*")) {
          outputModelFile = outputModelAndResidual + modelFileExtension;
        }
        if (outputResidualFile.matches("\\s*")) {
          outputResidualFile = outputModelAndResidual + residualFileExtension;
        }
      }

      outputFidXYZFile = scriptCommand.getValue(outputFidXYZFileString);
      outputTiltFile = scriptCommand.getValue(outputTiltFileString);
      outputTransformFile = scriptCommand.getValue(outputTransformFileString);
      outputZFactorFile = scriptCommand.getValue(outputZFactorFileString);
      ParamUtilities.setParamIfPresent(scriptCommand, includeStartEndIncString,
          includeStartEndInc);
      includeList.parseString(scriptCommand.getValue(includeListString));
      excludeList.parseString(scriptCommand.getValue(EXCLUDE_LIST_KEY));
      rotationAngle.parse(scriptCommand);
      separateGroup.parseString(scriptCommand.getValues(SEPARATE_GROUP_KEY));
      tiltAngleSpec.parse(scriptCommand);
      angleOffset.parse(scriptCommand);
      projectionStretch.parse(scriptCommand);
      rotOption.parse(scriptCommand);
      rotDefaultGrouping.parse(scriptCommand);
      rotNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          ROT_NONDEFAULT_GROUP_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      rotationFixedView.parse(scriptCommand);
      localRotOption.parse(scriptCommand);
      localRotDefaultGrouping.parse(scriptCommand);
      localRotNondefaultGroup = ParamUtilities
          .setParamIfPresent(scriptCommand, LOCAL_ROT_NONDEFAULT_GROUP_KEY,
              nondefaultGroupSize, nondefaultGroupIntegerType);
      tiltOption.parse(scriptCommand);
      tiltDefaultGrouping.parse(scriptCommand);
      tiltNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          TILT_NONDEFAULT_GROUP_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      localTiltOption.parse(scriptCommand);
      localTiltDefaultGrouping.parse(scriptCommand);
      localTiltNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          LOCAL_TILT_NONDEFAULT_GROUP_KEY, nondefaultGroupSize,
          nondefaultGroupIntegerType);
      magReferenceView.parse(scriptCommand);
      magOption.parse(scriptCommand);
      magDefaultGrouping.parse(scriptCommand);
      magNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          MAG_NONDEFAULT_GROUP_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      localMagReferenceView.parse(scriptCommand);
      localMagOption.parse(scriptCommand);
      localMagDefaultGrouping.parse(scriptCommand);
      localMagNondefaultGroup = ParamUtilities
          .setParamIfPresent(scriptCommand, LOCAL_MAG_NONDEFAULT_GROUP_KEY,
              nondefaultGroupSize, nondefaultGroupIntegerType);
      xStretchOption.parse(scriptCommand);
      xStretchDefaultGrouping.parse(scriptCommand);
      xStretchNondefaultGroup = ParamUtilities
          .setParamIfPresent(scriptCommand, X_STRETCH_NONDEFAULT_GROUP_KEY,
              nondefaultGroupSize, nondefaultGroupIntegerType);
      localXStretchOption.parse(scriptCommand);
      localXStretchDefaultGrouping.parse(scriptCommand);
      localXStretchNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          LOCAL_X_STRETCH_NONDEFAULT_GROUP_KEY, nondefaultGroupSize,
          nondefaultGroupIntegerType);
      skewOption.parse(scriptCommand);
      skewDefaultGrouping.parse(scriptCommand);
      skewNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          SKEW_NONDEFAULT_GROUP_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      localSkewOption.parse(scriptCommand);
      localSkewDefaultGrouping.parse(scriptCommand);
      localSkewNondefaultGroup = ParamUtilities.setParamIfPresent(scriptCommand,
          LOCAL_SKEW_NONDEFAULT_GROUP_KEY, nondefaultGroupSize,
          nondefaultGroupIntegerType);
      residualReportCriterion.parse(scriptCommand);
      surfacesToAnalyze.parse(scriptCommand);
      metroFactor.parse(scriptCommand);
      maximumCycles.parse(scriptCommand);
      axisZShift.parse(scriptCommand);
      localAlignments.parse(scriptCommand);
      outputLocalFile = scriptCommand.getValue(outputLocalFileString);
      ParamUtilities.setParamIfPresent(scriptCommand, TARGET_PATCH_SIZE_X_AND_Y_KEY,
          targetPatchSizeXandY);
      ParamUtilities.setParamIfPresent(scriptCommand,
          NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY, numberOfLocalPatchesXandY);
      ParamUtilities.setParamIfPresent(scriptCommand, MIN_SIZE_OR_OVERLAP_X_AND_Y_KEY,
          minSizeOrOverlapXandY);
      ParamUtilities.setParamIfPresent(scriptCommand,
          MIN_FIDS_TOTAL_AND_EACH_SURFACE_KEY, minFidsTotalAndEachSurface);
      fixXYZCoordinates.parse(scriptCommand);
      ParamUtilities.setParamIfPresent(scriptCommand, localOutputOptionsString,
          localOutputOptions);
      beamTiltOption.parse(scriptCommand);
      fixedOrInitialBeamTilt.parse(scriptCommand);
      robustFitting.parse(scriptCommand);
      weightWholeTracks.parse(scriptCommand);
      kFactorScaling.parse(scriptCommand);
      String param = scriptCommand.getValue(OUTPUT_X_AXIS_TILT_FILE_KEY);
      if (param == null || param.matches("\\s*")) {
        outputXAxisTiltFile = DatasetFiles.getXTiltFileName(manager, axisID);
      }
      else
        outputXAxisTiltFile = param;
    }
    String invalidReason = validate();
    if (invalidReason != null && !invalidReason.matches("\\s*")) {
      throw new InvalidParameterException(invalidReason);
    }
    //set fields dependent on other fields
    setOutputLocalFile();
    loadedFromFile = true;
  }

  /**
   * Convert from the old style script to PIP:
   * modelFile = modelFile
   * imageFile = imageFile
   * Not using imageParameters
   * outputModelFile = imodFiducialPosFile + .3dmod
   * outputResidualFile = imodFiducialPosFile + .resid
   * outputFidXYZFile = asciiFiducialPosFile
   * outputTiltFile = tiltAngleSolutionFile
   * outputTransformFile = transformSolutionFile
   * outputZFactorFile new.
   * Not using solutionType.
   * Not saving includeExcludeType.
   * includeStartEndInc = includeExcludeList if includeExcludeType == 1
   * includeList = includeExcludeList if includeExcludeType == 2
   * excludeList = includeExcludeList if includeExcludeType == 3
   * rotationAngle = initialImageRotation
   * separateGroup = separateViewGroups
   * tiltAngleSpec = tiltAngleSpec;
   * angleOffset = tiltAngleOffset
   * projectionStretch new (default = false)
   * rotOption = 1 if rotationAngleSolutionType == 0
   * rotOption = 1 if rotationAngleSolutionType > 0
   * rotOption = 0 if rotationAngleSolutionType == -2
   * rotDefaultGrouping new
   * rotNondefaultGroup new
   * rotationFixedView = rotationAngleSolutionType if rotationAngleSolutionType > 0
   * localRotOption = localRotationSolution.type
   * localRotDefaultGrouping = localRotationSolution.params.getInt(0)
   * localRotNondefaultGroup = localRotationSolution.additionalGroups
   * tiltOption = tiltAngleSolution.type
   * tiltDefaultGrouping = tiltAngleSolution.params.getInt(0)
   * tiltNondefaultGroup = tiltAngleSolution.additionalGroups
   * localTiltOption = localTiltSolution.type
   * localTiltDefaultGrouping = localTiltSolution.params.getInt(0)
   * localTiltNondefaultGroup = localTiltSolution.additionalGroups
   * magReferenceView = magnificationSolution.referenceView
   * magOption = magnificationSolution.type
   * magDefaultGrouping = magnificationSolution.params.getInt(0)
   * magNondefaultGroup = magnificationSolution.additionalGroups
   * localMagOption = localMagnificationSolution.type
   * localMagDefaultGrouping = localMagnificationSolution.params.getInt(0)
   * localMagNondefaultGroup = localMagnificationSolution.additionalGroups
   * xStretchOption = xstretchSolution.type
   * xStretchDefaultGrouping = xstretchSolution.params.getInt(0)
   * localXStretchOption = localXstretchSolution.type
   * localXStretchDefaultGrouping = localXstretchSolution.params.getInt(0)
   * localXStretchNondefaultGroup = localXstretchSolution.additionalGroups
   * skewOption = skewSolution.type
   * skewDefaultGrouping = skewSolution.params.getInt(0)
   * skewNondefaultGroup = skewSolution.additionalGroups
   * localSkewOption = localSkewSolution.type
   * localSkewDefaultGrouping = localSkewSolution.params.getInt(0)
   * localSkewNondefaultGroup = localSkewSolution.additionalGroups
   * residualReportCriterion = residualThreshold
   * surfacesToAnalyze = nSurfaceAnalysis
   * metroFactor = metroFactor
   * maximumCycles = cycleLimit
   * axisZShift = tiltAxisZShift
   * localAlignments = localAlignments
   * outputLocalFile = localTransformFile
   * numberOfLocalPatchesXandY = nLocalPatches
   * minSizeOrOverlapXandY = minLocalPatchSize
   * minFidsTotalAndEachSurface = minLocalFiducials
   *   
   * @param oldParam
   */
  private void convertToPIP(OldTiltalignParam oldParam)
      throws FortranInputSyntaxException {
    modelFile = oldParam.getModelFile();
    imageFile = oldParam.getImageFile();
    //OldTiltParam only looks for IMODFiducialPosFile.  It does not check for
    //the model file or the residual file
    outputModelAndResidual = oldParam.getIMODFiducialPosFile();
    outputModelFile = oldParam.getIMODFiducialPosFile() + modelFileExtension;
    outputResidualFile = oldParam.getIMODFiducialPosFile() + residualFileExtension;
    outputFidXYZFile = oldParam.getAsciiFiducialPosFile();
    outputTiltFile = oldParam.getTiltAngleSolutionFile();
    outputTransformFile = oldParam.getTransformSolutionFile();
    //Set ExcludeList
    int includeExcludeType = oldParam.getIncludeExcludeType();
    if (includeExcludeType == 1) {
      excludeList = oldParam.getIncludeExcludeList();
    }
    else if (includeExcludeType == 2) {
      excludeList = oldParam.getIncludeExcludeList();
    }
    else if (includeExcludeType == 3) {
      excludeList = oldParam.getIncludeExcludeList();
    }

    rotationAngle.set(oldParam.getInitialImageRotation());
    separateGroup = oldParam.getSeparateViewGroups();
    separateGroup.setKey(SEPARATE_GROUP_KEY);
    separateGroup.setSuccessiveEntriesAccumulate();
    tiltAngleSpec.set(oldParam.getTiltAngleSpec());
    angleOffset.set(oldParam.getTiltAngleOffset());
    //Set RotationAngleSolutionType and RotationFixedView
    int rotationAngleSolutionType = oldParam.getRotationAngleSolutionType();
    if (rotationAngleSolutionType >= 0) {
      rotOption.set(1);
      if (rotationAngleSolutionType > 0) {
        rotationFixedView.set(rotationAngleSolutionType);
      }
    }
    else if (rotationAngleSolutionType == -2) {
      rotOption.set(0);
    }
    else {
      rotOption.set(rotationAngleSolutionType);
    }

    localRotNondefaultGroup = setSolution(localRotOption, localRotDefaultGrouping, null,
        oldParam.getLocalRotationSolution());
    tiltNondefaultGroup = setSolution(tiltOption, tiltDefaultGrouping, null, oldParam
        .getTiltAngleSolution());
    localTiltNondefaultGroup = setSolution(localTiltOption, localTiltDefaultGrouping,
        null, oldParam.getLocalTiltSolution());
    magNondefaultGroup = setSolution(magOption, magDefaultGrouping, magReferenceView,
        oldParam.getMagnificationSolution());
    localMagNondefaultGroup = setSolution(localMagOption, localMagDefaultGrouping,
        localMagReferenceView, oldParam.getLocalMagnificationSolution());
    xStretchNondefaultGroup = setSolution(xStretchOption, xStretchDefaultGrouping, null,
        oldParam.getXstretchSolution());
    localXStretchNondefaultGroup = setSolution(localXStretchOption,
        localXStretchDefaultGrouping, null, oldParam.getLocalXstretchSolution());
    skewNondefaultGroup = setSolution(skewOption, skewDefaultGrouping, null, oldParam
        .getSkewSolution());
    localSkewNondefaultGroup = setSolution(localSkewOption, localSkewDefaultGrouping,
        null, oldParam.getLocalSkewSolution());
    residualReportCriterion.set(oldParam.getResidualThreshold());
    surfacesToAnalyze.set(oldParam.getNSurfaceAnalysis());
    metroFactor.set(oldParam.getMetroFactor());
    maximumCycles.set(oldParam.getCycleLimit());
    axisZShift.set(oldParam.getTiltAxisZShift());
    localAlignments.set(oldParam.getLocalAlignments());
    outputLocalFile = oldParam.getLocalTransformFile();
    numberOfLocalPatchesXandY = oldParam.getNLocalPatches();
    minSizeOrOverlapXandY = oldParam.getMinLocalPatchSize();
    minFidsTotalAndEachSurface = oldParam.getMinLocalFiducials();
    fixXYZCoordinates.set(oldParam.getFixLocalFiducialCoodinates());
    localOutputOptions = oldParam.getLocalOutputSelection();
    //set state
    setOutputZFactorFile();
    loadedFromFile = true;
  }

  private FortranInputString[] setSolution(EtomoNumber option,
      EtomoNumber defaultGrouping, EtomoNumber referenceView, TiltalignSolution solution)
      throws FortranInputSyntaxException {
    if (solution == null) {
      return null;
    }
    if (option != null) {
      option.set(solution.type);
    }
    if (defaultGrouping != null && solution.params != null && solution.params.size() > 0
        && !solution.params.isDefault(0)) {
      defaultGrouping.set(solution.params.getInt(0));
    }
    if (referenceView != null && solution.referenceView != null
        && solution.referenceView.size() > 0 && !solution.referenceView.isDefault(0)) {
      referenceView.set(solution.referenceView.getInt(0));
    }
    return ParamUtilities.parse(solution.additionalGroups, nondefaultGroupIntegerType,
        nondefaultGroupSize);
  }

  /**
   * Update the script command with the
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    String invalidReason = validate();
    if (invalidReason != null && !invalidReason.matches("\\s*")) {
      throw new BadComScriptException(invalidReason);
    }
    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();

    imagesAreBinned.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, modelFileString, modelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, imageFileString, imageFile);
    scriptCommand.deleteKey(outputModelAndResidualString);
    ParamUtilities.updateScriptParameter(scriptCommand, outputModelFileString,
        outputModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, outputResidualFileString,
        outputResidualFile);
    ParamUtilities.updateScriptParameter(scriptCommand, outputFidXYZFileString,
        outputFidXYZFile);
    ParamUtilities.updateScriptParameter(scriptCommand, outputTiltFileString,
        outputTiltFile);
    ParamUtilities.updateScriptParameter(scriptCommand, outputTransformFileString,
        outputTransformFile);
    ParamUtilities.updateScriptParameter(scriptCommand, includeStartEndIncString,
        includeStartEndInc);
    ParamUtilities.updateScriptParameter(scriptCommand, includeListString, includeList);
    ParamUtilities.updateScriptParameter(scriptCommand, EXCLUDE_LIST_KEY, excludeList);
    rotationAngle.updateComScript(scriptCommand);
    separateGroup.updateComScript(scriptCommand);
    tiltAngleSpec.updateComScript(scriptCommand);
    angleOffset.updateComScript(scriptCommand);
    projectionStretch.updateComScript(scriptCommand);
    rotOption.updateComScript(scriptCommand);
    rotDefaultGrouping.updateComScript(scriptCommand);
    rotationFixedView.updateComScript(scriptCommand);
    tiltOption.updateComScript(scriptCommand);
    tiltDefaultGrouping.updateComScript(scriptCommand);
    magReferenceView.updateComScript(scriptCommand);
    magOption.updateComScript(scriptCommand);
    magDefaultGrouping.updateComScript(scriptCommand);
    xStretchOption.updateComScript(scriptCommand);
    skewOption.updateComScript(scriptCommand);
    xStretchDefaultGrouping.updateComScript(scriptCommand);
    skewDefaultGrouping.updateComScript(scriptCommand);
    residualReportCriterion.updateComScript(scriptCommand);
    surfacesToAnalyze.updateComScript(scriptCommand);
    metroFactor.updateComScript(scriptCommand);
    maximumCycles.updateComScript(scriptCommand);
    axisZShift.updateComScript(scriptCommand);
    //local alignment
    localAlignments.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, outputLocalFileString,
        outputLocalFile);
    ParamUtilities.updateScriptParameter(scriptCommand, TARGET_PATCH_SIZE_X_AND_Y_KEY,
        targetPatchSizeXandY);
    ParamUtilities.updateScriptParameter(scriptCommand,
        NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY, numberOfLocalPatchesXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, MIN_SIZE_OR_OVERLAP_X_AND_Y_KEY,
        minSizeOrOverlapXandY);
    ParamUtilities.updateScriptParameter(scriptCommand,
        MIN_FIDS_TOTAL_AND_EACH_SURFACE_KEY, minFidsTotalAndEachSurface);
    fixXYZCoordinates.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, localOutputOptionsString,
        localOutputOptions);
    localRotOption.updateComScript(scriptCommand);
    localRotDefaultGrouping.updateComScript(scriptCommand);
    localTiltOption.updateComScript(scriptCommand);
    localTiltDefaultGrouping.updateComScript(scriptCommand);
    localMagReferenceView.updateComScript(scriptCommand);
    localMagOption.updateComScript(scriptCommand);
    localMagDefaultGrouping.updateComScript(scriptCommand);
    localXStretchOption.updateComScript(scriptCommand);
    localXStretchDefaultGrouping.updateComScript(scriptCommand);
    localSkewOption.updateComScript(scriptCommand);
    localSkewDefaultGrouping.updateComScript(scriptCommand);
    //optional parameters
    ParamUtilities.updateScriptParameter(scriptCommand, outputZFactorFileString,
        outputZFactorFile);
    ParamUtilities.updateScriptParameter(scriptCommand, ROT_NONDEFAULT_GROUP_KEY,
        rotNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, TILT_NONDEFAULT_GROUP_KEY,
        tiltNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, MAG_NONDEFAULT_GROUP_KEY,
        magNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, X_STRETCH_NONDEFAULT_GROUP_KEY,
        xStretchNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, SKEW_NONDEFAULT_GROUP_KEY,
        skewNondefaultGroup);
    //local optional parameters
    ParamUtilities.updateScriptParameter(scriptCommand, LOCAL_ROT_NONDEFAULT_GROUP_KEY,
        localRotNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, LOCAL_TILT_NONDEFAULT_GROUP_KEY,
        localTiltNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, LOCAL_MAG_NONDEFAULT_GROUP_KEY,
        localMagNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand,
        LOCAL_X_STRETCH_NONDEFAULT_GROUP_KEY, localXStretchNondefaultGroup);
    ParamUtilities.updateScriptParameter(scriptCommand, LOCAL_SKEW_NONDEFAULT_GROUP_KEY,
        localSkewNondefaultGroup);
    beamTiltOption.updateComScript(scriptCommand, true);
    robustFitting.updateComScript(scriptCommand);
    weightWholeTracks.updateComScript(scriptCommand);
    kFactorScaling.updateComScript(scriptCommand);
    //Only using FixedOrInitialBeamTilt for fixed beam tilt
    if (beamTiltOption.equals(FIXED_OPTION) && !fixedOrInitialBeamTilt.isDefault()) {
      fixedOrInitialBeamTilt.updateComScript(scriptCommand);
    }
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_X_AXIS_TILT_FILE_KEY,
        outputXAxisTiltFile);
  }

  public void initializeDefaults() {
    reset();
  }

  /**
   * @param angleOffset The angleOffset to set.
   */
  public void setAngleOffset(String angleOffset) {
    this.angleOffset.set(angleOffset);
  }

  /**
   * @param axisZShift The axisZShift to set.
   */
  public void setAxisZShift(String axisZShift) {
    this.axisZShift.set(axisZShift);
  }

  /**
   * @param excludeList The excludeList to set.
   */
  public void setExcludeList(String excludeList) {
    this.excludeList.parseString(excludeList);
  }

  /**
   * @param imageFile The imageFile to set.
   */
  public void setImageFile(final String imageFile) {
    this.imageFile = imageFile;
  }

  public void setImagesAreBinned(final int imagesAreBinned) {
    this.imagesAreBinned.set(imagesAreBinned);
  }

  public void setBeamTiltOption(final int beamTiltOption) {
    this.beamTiltOption.set(beamTiltOption);
  }

  public void setFixedOrInitialBeamTilt(final String fixedOrInitialBeamTilt) {
    this.fixedOrInitialBeamTilt.set(fixedOrInitialBeamTilt);
  }

  public void resetFixedOrInitialBeamTilt() {
    fixedOrInitialBeamTilt.reset();
  }
  
  public void resetWeightWholeTracks() {
    weightWholeTracks.reset();
  }

  /**
   * @param localAlignments The localAlignments to set.
   */
  public void setLocalAlignments(final boolean localAlignments) {
    this.localAlignments.set(localAlignments);
  }

  /**
   * @param localMagDefaultGrouping The localMagDefaultGrouping to set.
   */
  public void setLocalMagDefaultGrouping(final String localMagDefaultGrouping) {
    this.localMagDefaultGrouping.set(localMagDefaultGrouping);
  }

  /**
   * @param localMagNondefaultGroup The localMagNondefaultGroup to set.
   */
  public void setLocalMagNondefaultGroup(final String localMagNondefaultGroup)
      throws FortranInputSyntaxException {
    this.localMagNondefaultGroup = ParamUtilities.parse(localMagNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param localMagOption The localMagOption to set.
   */
  public void setLocalMagOption(int localMagOption) {
    this.localMagOption.set(localMagOption);
  }

  /**
   * @param localRotDefaultGrouping The localRotDefaultGrouping to set.
   */
  public void setLocalRotDefaultGrouping(String localRotDefaultGrouping) {
    this.localRotDefaultGrouping.set(localRotDefaultGrouping);
  }

  /**
   * @param localRotNondefaultGroup The localRotNondefaultGroup to set.
   */
  public void setLocalRotNondefaultGroup(String localRotNondefaultGroup)
      throws FortranInputSyntaxException {
    this.localRotNondefaultGroup = ParamUtilities.parse(localRotNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param localRotOption The localRotOption to set.
   */
  public void setLocalRotOption(int localRotOption) {
    this.localRotOption.set(localRotOption);
  }

  /**
   * @param localSkewDefaultGrouping The localSkewDefaultGrouping to set.
   */
  public void setLocalSkewDefaultGrouping(String localSkewDefaultGrouping) {
    this.localSkewDefaultGrouping.set(localSkewDefaultGrouping);
  }

  /**
   * @param localSkewNondefaultGroup The localSkewNondefaultGroup to set.
   */
  public void setLocalSkewNondefaultGroup(String localSkewNondefaultGroup)
      throws FortranInputSyntaxException {
    this.localSkewNondefaultGroup = ParamUtilities.parse(localSkewNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param localSkewOption The localSkewOption to set.
   */
  public void setLocalSkewOption(int localSkewOption) {
    this.localSkewOption.set(localSkewOption);
  }

  /**
   * @param localTiltDefaultGrouping The localTiltDefaultGrouping to set.
   */
  public void setLocalTiltDefaultGrouping(String localTiltDefaultGrouping) {
    this.localTiltDefaultGrouping.set(localTiltDefaultGrouping);
  }

  /**
   * @param localTiltNondefaultGroup The localTiltNondefaultGroup to set.
   */
  public void setLocalTiltNondefaultGroup(String localTiltNondefaultGroup)
      throws FortranInputSyntaxException {
    this.localTiltNondefaultGroup = ParamUtilities.parse(localTiltNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param localTiltOption The localTiltOption to set.
   */
  public void setLocalTiltOption(int localTiltOption) {
    this.localTiltOption.set(localTiltOption);
  }

  /**
   * @param localXStretchDefaultGrouping The localXStretchDefaultGrouping to set.
   */
  public void setLocalXStretchDefaultGrouping(String localXStretchDefaultGrouping) {
    this.localXStretchDefaultGrouping.set(localXStretchDefaultGrouping);
  }

  /**
   * @param localXStretchNondefaultGroup The localXStretchNondefaultGroup to set.
   */
  public void setLocalXStretchNondefaultGroup(String localXStretchNondefaultGroup)
      throws FortranInputSyntaxException {
    this.localXStretchNondefaultGroup = ParamUtilities.parse(
        localXStretchNondefaultGroup, true, nondefaultGroupSize);
  }

  /**
   * @param localXStretchOption The localXStretchOption to set.
   */
  public void setLocalXStretchOption(int localXStretchOption) {
    this.localXStretchOption.set(localXStretchOption);
  }

  /**
   * @param magDefaultGrouping The magDefaultGrouping to set.
   */
  public void setMagDefaultGrouping(String magDefaultGrouping) {
    this.magDefaultGrouping.set(magDefaultGrouping);
  }

  /**
   * @param magNondefaultGroup The magNondefaultGroup to set.
   */
  public void setMagNondefaultGroup(String magNondefaultGroup)
      throws FortranInputSyntaxException {
    this.magNondefaultGroup = ParamUtilities.parse(magNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param magOption The magOption to set.
   */
  public void setMagOption(int magOption) {
    this.magOption.set(magOption);
  }

  /**
   * @param magReferenceView The magReferenceView to set.
   */
  public void setMagReferenceView(String magReferenceView) {
    this.magReferenceView.set(magReferenceView);
  }

  /**
   * @param maximumCycles The maximumCycles to set.
   */
  public void setMaximumCycles(String maximumCycles) {
    this.maximumCycles.set(maximumCycles);
  }

  /**
   * @param metroFactor The metroFactor to set.
   */
  public void setMetroFactor(String metroFactor) {
    this.metroFactor.set(metroFactor);
  }

  /**
   * @param minFidsTotalAndEachSurface The minFidsTotalAndEachSurface to set.
   */
  public void setMinFidsTotalAndEachSurface(String minFidsTotalAndEachSurface)
      throws FortranInputSyntaxException {
    this.minFidsTotalAndEachSurface.validateAndSet(minFidsTotalAndEachSurface);
  }

  public ConstEtomoNumber setFixXYZCoordinates(boolean fixXYZCoordinates) {
    return this.fixXYZCoordinates.set(fixXYZCoordinates);
  }

  /**
   * @param minSizeOrOverlapXandY The minSizeOrOverlapXandY to set.
   */
  public void setMinSizeOrOverlapXandY(String minSizeOrOverlapXandY)
      throws FortranInputSyntaxException {
    this.minSizeOrOverlapXandY.validateAndSet(minSizeOrOverlapXandY);
  }

  /**
   * @param modelFile The modelFile to set.
   */
  public void setModelFile(String modelFile) {
    this.modelFile = modelFile;
  }

  /**
   * @param numberOfLocalPatchesXandY The numberOfLocalPatchesXandY to set.
   */
  public void setNumberOfLocalPatchesXandY(String numberOfLocalPatchesXandY)
      throws FortranInputSyntaxException {
    this.numberOfLocalPatchesXandY.validateAndSet(numberOfLocalPatchesXandY);
  }

  public void setTargetPatchSizeXandY(String targetPatchSizeXandY)
      throws FortranInputSyntaxException {
    this.targetPatchSizeXandY.validateAndSet(targetPatchSizeXandY);
  }

  public void setNumberOfLocalPatchesXandYActive(boolean active) {
    this.numberOfLocalPatchesXandY.setActive(active);
  }

  public void setTargetPatchSizeXandYActive(boolean active) {
    this.targetPatchSizeXandY.setActive(active);
  }

  /**
   * @param outputFidXYZFile The outputFidXYZFile to set.
   */
  public void setOutputFidXYZFile(String outputFidXYZFile) {
    this.outputFidXYZFile = outputFidXYZFile;
  }

  /**
   * @param outputLocalFile The outputLocalFile to set.
   */
  public void setOutputLocalFile(String outputLocalFile) {
    this.outputLocalFile = outputLocalFile;
  }

  /**
   * @param outputModelFile The outputModelFile to set.
   */
  public void setOutputModelFile(String outputModelFile) {
    this.outputModelFile = outputModelFile;
  }

  /**
   * @param outputResidualFile The outputResidualFile to set.
   */
  public void setOutputResidualFile(String outputResidualFile) {
    this.outputResidualFile = outputResidualFile;
  }

  /**
   * @param outputTiltFile The outputTiltFile to set.
   */
  public void setOutputTiltFile(String outputTiltFile) {
    this.outputTiltFile = outputTiltFile;
  }

  /**
   * @param outputTransformFile The outputTransformFile to set.
   */
  public void setOutputTransformFile(String outputTransformFile) {
    this.outputTransformFile = outputTransformFile;
  }

  /**
   * This must called after skewOption, or localAlignment, and localSkewOption
   * have been set.
   * @param outputZFactorFile The outputZFactorFile to set.
   */
  public void setOutputZFactorFile() {
    if (useOutputZFactorFile()) {
      outputZFactorFile = getOutputZFactorFileName(datasetName, axisID);
    }
    else {
      outputZFactorFile = "";
    }
  }

  public void setOutputLocalFile() {
    if (outputLocalFile == null || outputLocalFile.matches("\\s*")) {
      outputLocalFile = getOutputLocalFileName(datasetName, axisID);
    }
  }

  /**
   * @param projectionStretch The projectionStretch to set.
   */
  public void setProjectionStretch(boolean projectionStretch) {
    this.projectionStretch.set(projectionStretch);
  }

  /**
   * @param residualReportCriterion The residualReportCriterion to set.
   */
  public void setResidualReportCriterion(double residualReportCriterion) {
    this.residualReportCriterion.set(residualReportCriterion);
  }

  /**
   * @param rotationAngle The rotationAngle to set.
   */
  public void setRotationAngle(String rotationAngle) {
    this.rotationAngle.set(rotationAngle);
  }

  /**
   * @param rotationFixedView The rotationFixedView to set.
   */
  public void setRotationFixedView(int rotationFixedView) {
    this.rotationFixedView.set(rotationFixedView);
  }

  /**
   * @param rotDefaultGrouping The rotDefaultGrouping to set.
   */
  public void setRotDefaultGrouping(String rotDefaultGrouping) {
    this.rotDefaultGrouping.set(rotDefaultGrouping);
  }

  /**
   * @param rotNondefaultGroup The rotNondefaultGroup to set.
   */
  public void setRotNondefaultGroup(String rotNondefaultGroup)
      throws FortranInputSyntaxException {
    this.rotNondefaultGroup = ParamUtilities.parse(rotNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param rotOption The rotOption to set.
   */
  public void setRotOption(int rotOption) {
    this.rotOption.set(rotOption);
  }

  /**
   * @param separateGroup The separateGroup to set.
   */
  public void setSeparateGroup(String separateGroup) {
    this.separateGroup.parseString(separateGroup);
  }

  /**
   * @param skewDefaultGrouping The skewDefaultGrouping to set.
   */
  public void setSkewDefaultGrouping(String skewDefaultGrouping) {
    this.skewDefaultGrouping.set(skewDefaultGrouping);
  }

  /**
   * @param skewNondefaultGroup The skewNondefaultGroup to set.
   */
  public void setSkewNondefaultGroup(String skewNondefaultGroup)
      throws FortranInputSyntaxException {
    this.skewNondefaultGroup = ParamUtilities.parse(skewNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param skewOption The skewOption to set.
   */
  public void setSkewOption(int skewOption) {
    this.skewOption.set(skewOption);
  }

  /**
   * @param surfacesToAnalyze The surfacesToAnalyze to set.
   */
  public void setSurfacesToAnalyze(int surfacesToAnalyze) {
    this.surfacesToAnalyze.set(surfacesToAnalyze);
  }
  
  public void setRobustFitting(boolean input) {
    robustFitting.set(input);
  }
  
  public void setWeightWholeTracks(boolean input) {
    weightWholeTracks.set(input);
  }
  
  public void setKFactorScaling(String input) {
    kFactorScaling.set(input);
  }

  /**
   * @param tiltDefaultGrouping The tiltDefaultGrouping to set.
   */
  public void setTiltDefaultGrouping(String tiltDefaultGrouping) {
    this.tiltDefaultGrouping.set(tiltDefaultGrouping);
  }

  /**
   * @param tiltNondefaultGroup The tiltNondefaultGroup to set.
   */
  public void setTiltNondefaultGroup(String tiltNondefaultGroup)
      throws FortranInputSyntaxException {
    this.tiltNondefaultGroup = ParamUtilities.parse(tiltNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param tiltOption The tiltOption to set.
   */
  public void setTiltOption(int tiltOption) {
    this.tiltOption.set(tiltOption);
  }

  /**
   * @param stretchDefaultGrouping The xStretchDefaultGrouping to set.
   */
  public void setXStretchDefaultGrouping(String stretchDefaultGrouping) {
    xStretchDefaultGrouping.set(stretchDefaultGrouping);
  }

  /**
   * @param stretchNondefaultGroup The xStretchNondefaultGroup to set.
   */
  public void setXStretchNondefaultGroup(String stretchNondefaultGroup)
      throws FortranInputSyntaxException {
    xStretchNondefaultGroup = ParamUtilities.parse(stretchNondefaultGroup, true,
        nondefaultGroupSize);
  }

  /**
   * @param stretchOption The xStretchOption to set.
   */
  public void setXStretchOption(int stretchOption) {
    xStretchOption.set(stretchOption);
  }

  /**
   * Backward compatibility fix.  Unbinned all the parameters which where binned
   * in the old version.  Ignore parameters with reset values.  Will throw an
   * IllegalStateException if it doesn't think that it is an old version.  The
   * param should be loaded from a com file before running this function.
   * @param binning
   * @return true if changes where made
   */
  public boolean upgradeOldVersion(int correctionBinning, int currentBinning) {
    if (!isOldVersion()) {
      return false;
    }
    //Set the binning to prevent this function from being called again
    imagesAreBinned.set(currentBinning);
    //Currently this function only multiplies by binning, so there is nothing to
    //do if binning is 1.
    if (correctionBinning != 1) {
      if (!axisZShift.isNull() && !axisZShift.equals(0)) {
        axisZShift.set(axisZShift.getDouble() * correctionBinning);
      }
    }
    StringBuffer buffer = new StringBuffer("\nUpgraded align" + axisID.getExtension()
        + ".com:\n");
    if (correctionBinning > 1) {
      buffer.append("Multiplied binned " + axisZShift.getName() + " by "
          + correctionBinning + ".\n");
    }
    buffer.append("Added " + imagesAreBinned.getName() + " " + currentBinning + ".\n");
    System.err.println(buffer.toString());
    return true;
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.25  2010/09/23 22:15:58  sueh
 * <p> bug# 1404 Allowing separateGroup to have multiple entries.
 * <p>
 * <p> Revision 3.24  2010/07/19 04:31:10  sueh
 * <p> bug# 1393 In parseComScriptCommand added parse of localTiltOption.
 * <p>
 * <p> Revision 3.23  2008/07/16 20:14:27  sueh
 * <p> bug# 1126 In setRotationAngle changed parameter to String.
 * <p>
 * <p> Revision 3.22  2007/03/07 21:02:20  sueh
 * <p> bug# 981 Added beamTiltOption and fixedOrInitialBeamTilt.
 * <p>
 * <p> Revision 3.21  2007/03/03 00:37:45  sueh
 * <p> bug# 973 Added targetPatchSizeXandY.  Added set/isActive functions for
 * <p> targetPatchSizeXandY and numberOfLocalPatchesXandY.
 * <p>
 * <p> Revision 3.20  2005/06/14 21:55:01  sueh
 * <p> bug# 681 Added setFixXYZCoordinates().
 * <p>
 * <p> Revision 3.19  2005/06/10 22:52:40  sueh
 * <p> bug# 583, bug# 682  Upgraded align.com to have all unbinned parameters
 * <p> and a binning value.  Added function:  updateOldVersion.
 * <p>
 * <p> Revision 3.18  2005/05/10 02:04:00  sueh
 * <p> bug# 658 Added parse(ComScriptCommand) to TiltAngleSpec.  Changed
 * <p> ScriptParameter.set(ComScriptCommand) to parse(ComScriptCommand).
 * <p> Changed ScriptParameter.update(ComScriptCommand) to
 * <p> updateComScript(ComScriptCommand).
 * <p>
 * <p> Revision 3.17  2005/02/24 00:50:35  sueh
 * <p> bug# 600 Fixed a bug that was saving a value to the wrong parameter.
 * <p>
 * <p> Revision 3.16  2005/02/21 22:57:18  sueh
 * <p> bug# 600 Making parameter name statics public.
 * <p>
 * <p> Revision 3.15  2005/02/18 01:27:38  sueh
 * <p> bug# 600 Moving parameter names to public statics, so they can be used
 * <p> for tooltips.
 * <p>
 * <p> Revision 3.14  2005/01/25 21:43:18  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 3.13  2005/01/21 22:46:11  sueh
 * <p> bug# 509 bug# 591  Changed ConstEtomoNumber.
 * <p> update(ComScriptCommand) to updateCommand(ComScriptCommand) to
 * <p> make its function clear.
 * <p>
 * <p> Revision 3.12  2005/01/14 23:01:52  sueh
 * <p> Changing the name of EtomoNumber.set(ComScriptCommand) to parse.
 * <p>
 * <p> Revision 3.11  2005/01/13 00:44:58  sueh
 * <p> bug# 576 Converted includeStartEndInc to FortranInputString.
 * <p>
 * <p> Revision 3.10  2005/01/11 20:18:15  sueh
 * <p> bug# 567 Added fixXYZCoordinates, localMagReferenceView, and
 * <p> localOutputOptions.  In updateComScriptCommand(), changed the order
 * <p> of updates to match the align.com file coming from copytomocoms.
 * <p>
 * <p> Revision 3.9  2005/01/11 01:00:36  sueh
 * <p> bug# 567 Getting outputModelAndResidual, in case we want to
 * <p> use it.
 * <p>
 * <p> Revision 3.8  2005/01/08 01:45:04  sueh
 * <p> bug# 578 Place the logic to create the z factor file into a static function.
 * <p>
 * <p> Revision 3.7  2005/01/06 18:09:28  sueh
 * <p> bug# 567 Changed setOutputZFactorFile() to set it based on member
 * <p> variable values.  Bug# 578 Made the logic of when to use outputZFileFile()
 * <p> public.
 * <p>
 * <p> Revision 3.6  2005/01/05 19:47:35  sueh
 * <p> bug# 567 Changed setProjectionStretch() to accept booleans.  Bug# 578
 * <p> Added AxisID to the constructor.
 * <p>
 * <p> Revision 3.5  2004/12/30 19:49:26  sueh
 * <p> bug# 567 Removed OutputModelAndResidual when writing command file.
 * <p> Already converting it to OutputModelFile and OutputResidualFile.
 * <p>
 * <p> Revision 3.4  2004/12/29 23:44:00  sueh
 * <p> bug# 567 In ParamUtilities, added the FortranInputString to parse(String...) and
 * <p> parse(StringList...).
 * <p>
 * <p> Revision 3.3  2004/12/29 01:53:12  sueh
 * <p> bug# 567 Passing ints, doubles, and strings to set functions, instead of
 * <p> EtomoNumber.
 * <p>
 * <p> Revision 3.2  2004/12/29 00:01:20  sueh
 * <p> bug# 567 Placed the version of TiltalignParam for the old-style comscript
 * <p> into OldTiltalignParam.  This version updates and parses only the new
 * <p> parameters and converts from the old-style comscript to the new
 * <p> parameters.
 * <p>
 * <p> Revision 3.1  2004/04/12 16:50:58  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/10/14 20:30:56  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.6  2003/10/09 23:24:10  rickg
 * <p> Bug#279  Added integer set method for nFiducials
 * <p>
 * <p> Revision 2.5  2003/08/07 17:59:06  rickg
 * <p> Merged in tilt angle fix from beta2a branch
 * <p>
 * <p> Revision 2.4  2003/07/25 22:55:04  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2.2.1  2003/08/07 16:15:44  rickg
 * <p> Fixed tiltanglespec handling to include start and step
 * <p>
 * <p> Revision 2.2  2003/03/20 17:24:45  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2002/12/18 19:13:57  rickg
 * <p> Added setters for metro factor and cycle limit
 * <p>
 * <p> Revision 1.5  2002/12/10 21:37:01  rickg
 * <p> changed reportStddevThreshold to residualThreshold
 * <p>
 * <p> Revision 1.4  2002/12/10 18:48:21  rickg
 * <p> changed names of comscript put and get methods to
 * <p> be more understandable
 * <p>
 * <p> Revision 1.3  2002/12/06 15:22:30  rickg
 * <p> Comment where to fix
 * <p>
 * <p> Revision 1.2  2002/10/07 22:24:17  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
