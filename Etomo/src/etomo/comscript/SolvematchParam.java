package etomo.comscript;

import etomo.type.FiducialMatch;

/**
 * <p>Description: A model of the solvematch com script.  The solvematch
 * com script supercedes the Solvematchshift and Solvematchmod com scripts.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.2  2004/06/14 23:36:47  rickg
 * <p> Bug #383  Initial revision
 * <p>
 * <p> Revision 3.1  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> </p>
 */

public class SolvematchParam extends ConstSolvematchParam
  implements CommandParam {

  /**
   * Set this objects default values if any
   */
  public void initializeDefaults() {
  }

  /**
   * Parse the parameter values from the specified comscript command object
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException, FortranInputSyntaxException,
    InvalidParameterException {
    //  Check to be sure that it is a solvematch command
    if (!scriptCommand.getCommand().equals("solvematch")) {
      throw (new BadComScriptException("Not a solvematch command"));
    }

    // Set any of this class's attributes that are present in the script command
    // object
    outputFile = ParamUtilities.setParamIfPresent(scriptCommand, OUTPUT_FILE,
      outputFile);
    toFiducialFile = ParamUtilities.setParamIfPresent(scriptCommand,
      TO_FIDUCIAL_FILE, toFiducialFile);
    fromFiducialFile = ParamUtilities.setParamIfPresent(scriptCommand,
      FROM_FIDUCIAL_FILE, fromFiducialFile);
    ParamUtilities.setParamIfPresent(scriptCommand, TO_CORRESPONDENCE_LIST,
      toCorrespondenceList);
    ParamUtilities.setParamIfPresent(scriptCommand, FROM_CORRESPONDENCE_LIST,
      fromCorrespondenceList);
    ParamUtilities.setParamIfPresent(scriptCommand, XAXIS_TILTS, xAxistTilt);
    surfacesOrModel = ParamUtilities.setParamIfPresent(scriptCommand,
      SURFACE_OR_USE_MODELS, surfacesOrModel);
    maximumResidual = ParamUtilities.setParamIfPresent(scriptCommand,
      MAXIMUM_RESIDUAL, maximumResidual);
    toMatchingModel = ParamUtilities.setParamIfPresent(scriptCommand,
      TO_MATCHING_MODEL, toMatchingModel);
    fromMatchingModel = ParamUtilities.setParamIfPresent(scriptCommand,
      FROM_MATCHING_MODEL, fromMatchingModel);
    toTomogramOrSizeXYZ = ParamUtilities.setParamIfPresent(scriptCommand,
      TO_TOMOGRAM_OR_SIZE_XYZ, toTomogramOrSizeXYZ);
    fromTomogramOrSizeXYZ = ParamUtilities.setParamIfPresent(scriptCommand,
      FROM_TOMOGRAM_OR_SIZE_XYZ, fromTomogramOrSizeXYZ);
  }

  /**
   * Update the values in the comscript command with the values from this object
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    if (!scriptCommand.getCommand().equals("solvematch")) {
      throw (new BadComScriptException("Not a solvematch command"));
    }

    //  Make sure the script is in keyword / value pairs
    scriptCommand.useKeywordValue();

    //  Update the values in the comscript command object
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_FILE, outputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_FIDUCIAL_FILE,
      toFiducialFile);
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_FIDUCIAL_FILE,
      fromFiducialFile);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_CORRESPONDENCE_LIST,
      toCorrespondenceList.toString());
    ParamUtilities.updateScriptParameter(scriptCommand,
      FROM_CORRESPONDENCE_LIST, fromCorrespondenceList.toString());
    ParamUtilities.updateScriptParameter(scriptCommand, XAXIS_TILTS, xAxistTilt);
    ParamUtilities.updateScriptParameter(scriptCommand, SURFACE_OR_USE_MODELS,
      surfacesOrModel);
    ParamUtilities.updateScriptParameter(scriptCommand, MAXIMUM_RESIDUAL,
      maximumResidual);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_MATCHING_MODEL,
      toMatchingModel);
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_MATCHING_MODEL,
      fromMatchingModel);
    ParamUtilities.updateScriptParameter(scriptCommand,
      TO_TOMOGRAM_OR_SIZE_XYZ, toTomogramOrSizeXYZ);
    ParamUtilities.updateScriptParameter(scriptCommand,
      FROM_TOMOGRAM_OR_SIZE_XYZ, fromTomogramOrSizeXYZ);
    ParamUtilities.updateScriptParameter(scriptCommand, SCALE_FACTORS,
      scaleFactors);
  }

  /**
   * Parse a solvematchshift param object into this object
   * @param solvematchshift
   */
  public void parseSolvematchshift(ConstSolvematchshiftParam solvematchshift,
    String datasetName) {
    toFiducialFile = solvematchshift.getToFiducialCoordinatesFile();
    setMatchBToA(toFiducialFile);
    fromFiducialFile = solvematchshift.getFromFiducialCoordinatesFile();
    toCorrespondenceList = solvematchshift.getFiducialMatchListA();
    fromCorrespondenceList = solvematchshift.getFiducialMatchListB();
    xAxistTilt = solvematchshift.getXAxistTilt();
    maximumResidual = solvematchshift.getResidualThreshold();
    surfacesOrModel = solvematchshift.getNSurfaces();
    outputFile = solvematchshift.getOutputTransformationFile();
    
    // Fill in the matching model and tomogram names since they will now be
    // ignored in non-model mode and are needed int model mode 
    if (matchBToA) {
      toMatchingModel = datasetName + "a.matmod";
      fromMatchingModel = datasetName + "b.matmod";
      toTomogramOrSizeXYZ = datasetName + "a.rec";
      fromTomogramOrSizeXYZ = datasetName + "b.rec";
    }
    else {
      toMatchingModel = datasetName + "b.matmod";
      fromMatchingModel = datasetName + "a.matmod";
      toTomogramOrSizeXYZ = datasetName + "b.rec";
      fromTomogramOrSizeXYZ = datasetName + "a.rec";
    }
  }

  /**
   * Parse a solvematchmod param object into this object
   * @param solvematchmod
   */
  public void parseSolvematchmod(ConstSolvematchmodParam solvematchmod) {
    toFiducialFile = solvematchmod.getToFiducialCoordinatesFile();
    setMatchBToA(toFiducialFile);
    fromFiducialFile = solvematchmod.getFromFiducialCoordinatesFile();
    toCorrespondenceList = solvematchmod.getFiducialMatchListA();
    fromCorrespondenceList = solvematchmod.getFiducialMatchListB();
    xAxistTilt = solvematchmod.getXAxistTilt();
    maximumResidual = solvematchmod.getResidualThreshold();
    surfacesOrModel = solvematchmod.getNSurfaces();
    //  As per David force output file to be solve.xf
    outputFile = "solvezero.xf";
    toMatchingModel = solvematchmod.getToMatchingModel();
    fromMatchingModel = solvematchmod.getFromMatchingModel();
    toTomogramOrSizeXYZ = solvematchmod.getToReconstructionFile();
    fromTomogramOrSizeXYZ = solvematchmod.getFromReconstructionFile();
  }

  /**
   * Detect the matching direction from name of the first fiducial file list
   * @param aFiducialFilename
   */
  protected void setMatchBToA(String aFiducialFilename) {
    if (aFiducialFilename == null || aFiducialFilename.matches("\\s*")) {
      return;
    }
    if (aFiducialFilename.matches("^\\s*\\S+?afid.xyz\\s*$")) {
      matchBToA = true;
    }
    else if (aFiducialFilename.matches("^\\s*\\S+?bfid.xyz\\s*$")) {
      matchBToA = false;
    }
    return;
  }

  /**
   * @param fromCorrespondenceList The fromCorrespondenceList to set.
   */
  public void setFromCorrespondenceList(String string) {
    fromCorrespondenceList.parseString(string);
  }

  /**
   * @param fromFiducialFile The fromFiducialFile to set.
   */
  public void setFromFiducialFile(String fromFiducialFile) {
    this.fromFiducialFile = fromFiducialFile;
  }

  /**
   * @param fromMatchingModel The fromMatchingModel to set.
   */
  public void setFromMatchingModel(String fromMatchingModel) {
    this.fromMatchingModel = fromMatchingModel;
  }

  /**
   * @param fromTomogramOrSizeXYZ The fromTomogramOrSizeXYZ to set.
   */
  public void setFromTomogramOrSizeXYZ(String fromTomogramOrSizeXYZ) {
    this.fromTomogramOrSizeXYZ = fromTomogramOrSizeXYZ;
  }

  /**
   * @param matchBToA The matchBToA to set.
   */
  public void setMatchBToA(boolean matchBToA) {
    this.matchBToA = matchBToA;
  }

  /**
   * @param maximumResidual The maximumResidual to set.
   */
  public void setMaximumResidual(float maximumResidual) {
    this.maximumResidual = maximumResidual;
  }

  public void setMaximumResidual(String value) {
    this.maximumResidual = ParamUtilities.parseFloat(value);
  }

  /**
   * @param surfaces The nSurfaces to set.
   */
  public void setSurfacesOrModel(FiducialMatch value) {
    if (value == FiducialMatch.USE_MODEL_ONLY) {
      surfacesOrModel = -2;
      return;
    }
    if (value == FiducialMatch.ONE_SIDE_INVERTED) {
      surfacesOrModel = -1;
      return;
    }
    if (value == FiducialMatch.USE_MODEL) {
      surfacesOrModel = 0;
      return;
    }
    if (value == FiducialMatch.ONE_SIDE) {
      surfacesOrModel = 1;
      return;
    }
    if (value == FiducialMatch.BOTH_SIDES) {
      surfacesOrModel = 2;
      return;
    }
  }

  /**
   * @param outputTransformationFile The outputTransformationFile to set.
   */
  public void setOutputFile(String outputTransformationFile) {
    this.outputFile = outputTransformationFile;
  }

  /**
   * @param scaleFactors The scaleFactors to set.
   */
  public void setScaleFactors(FortranInputString scaleFactors) {
    this.scaleFactors = scaleFactors;
  }

  /**
   * @param toCorrespondenceList The toCorrespondenceList to set.
   */
  public void setToCorrespondenceList(String string) {
    toCorrespondenceList.parseString(string);
  }

  /**
   * @param toFiducialFile The toFiducialFile to set.
   */
  public void setToFiducialFile(String toFiducialFile) {
    this.toFiducialFile = toFiducialFile;
  }

  /**
   * @param toMatchingModel The toMatchingModel to set.
   */
  public void setToMatchingModel(String toMatchingModel) {
    this.toMatchingModel = toMatchingModel;
  }

  /**
   * @param toTomogramOrSizeXYZ The toTomogramOrSizeXYZ to set.
   */
  public void setToTomogramOrSizeXYZ(String toTomogramOrSizeXYZ) {
    this.toTomogramOrSizeXYZ = toTomogramOrSizeXYZ;
  }

  /**
   * @param axistTilt The xAxistTilt to set.
   */
  public void setXAxistTilt(FortranInputString axistTilt) {
    xAxistTilt = axistTilt;
  }
}