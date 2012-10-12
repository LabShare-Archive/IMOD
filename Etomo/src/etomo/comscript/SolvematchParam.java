package etomo.comscript;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FiducialMatch;
import etomo.util.DatasetFiles;

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
 * <p> Revision 3.12  2007/03/07 21:02:06  sueh
 * <p> bug# 981 reformatted.
 * <p>
 * <p> Revision 3.11  2006/09/13 23:20:43  sueh
 * <p> bug# 921 Added centerShiftLimit.
 * <p>
 * <p> Revision 3.10  2006/05/23 21:02:30  sueh
 * <p> bug# 617 Changed DatasetFiles.getFiducialModel() to getFiducialModelName().
 * <p>
 * <p> Revision 3.9  2006/05/16 21:28:40  sueh
 * <p> bug# 856 Added a and bFiducialModel, usePoints, and transferCoordinateFile.
 * <p> Using matchBToA to add the atob parameter to the script.
 * <p>
 * <p> Revision 3.8  2005/02/23 01:40:33  sueh
 * <p> bug# 600 Making solvematch radio button options public static final ints.
 * <p>
 * <p> Revision 3.7  2004/08/02 23:06:30  rickg
 * <p> Bug #523 Added call to setMatchBToA to the parse.. method
 * <p>
 * <p> Revision 3.6  2004/06/24 21:40:07  sueh
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 3.5  2004/06/24 20:21:35  sueh
 * <p> bug# 482 removed commented out code
 * <p>
 * <p> Revision 3.4  2004/06/24 18:41:13  sueh
 * <p> bug# 482 modified parse functions to create functions the merge
 * <p> solvematchshift and solvematchmod together based on
 * <p> modelBased
 * <p>
 * <p> Revision 3.3  2004/06/18 15:50:56  rickg
 * <p> Bug #383 Forced outputFile to be solvezero.xf when converting
 * <p> from a solvematchmod.com script.
 * <p>
 * <p> Revision 3.2  2004/06/14 23:36:47  rickg
 * <p> Bug #383  Initial revision
 * <p>
 * <p> Revision 3.1  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> </p>
 */

public class SolvematchParam extends ConstSolvematchParam implements CommandParam {

  private static final double CENTER_SHIFT_LIMIT_DEFAULT = 10;

  private final BaseManager manager;

  public SolvematchParam(BaseManager manager) {
    this.manager = manager;
  }

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
    outputFile = ParamUtilities.setParamIfPresent(scriptCommand, OUTPUT_FILE, outputFile);
    toFiducialFile = ParamUtilities.setParamIfPresent(scriptCommand, TO_FIDUCIAL_FILE,
        toFiducialFile);
    fromFiducialFile = ParamUtilities.setParamIfPresent(scriptCommand,
        FROM_FIDUCIAL_FILE, fromFiducialFile);
    ParamUtilities.setParamIfPresent(scriptCommand, TO_CORRESPONDENCE_LIST,
        toCorrespondenceList);
    ParamUtilities.setParamIfPresent(scriptCommand, FROM_CORRESPONDENCE_LIST,
        fromCorrespondenceList);
    ParamUtilities.setParamIfPresent(scriptCommand, TRANSFER_COORDINATE_FILE,
        transferCoordinateFile);
    ParamUtilities.setParamIfPresent(scriptCommand, A_FIDUCIAL_MODEL, aFiducialModel);
    ParamUtilities.setParamIfPresent(scriptCommand, B_FIDUCIAL_MODEL, bFiducialModel);
    ParamUtilities.setParamIfPresent(scriptCommand, USE_POINTS, usePoints);
    ParamUtilities.setParamIfPresent(scriptCommand, XAXIS_TILTS, xAxistTilt);
    surfacesOrModel = ParamUtilities.setParamIfPresent(scriptCommand,
        SURFACE_OR_USE_MODELS, surfacesOrModel);
    maximumResidual = ParamUtilities.setParamIfPresent(scriptCommand, MAXIMUM_RESIDUAL,
        maximumResidual);
    centerShiftLimit.parse(scriptCommand);
    toMatchingModel = ParamUtilities.setParamIfPresent(scriptCommand, TO_MATCHING_MODEL,
        toMatchingModel);
    fromMatchingModel = ParamUtilities.setParamIfPresent(scriptCommand,
        FROM_MATCHING_MODEL, fromMatchingModel);
    toTomogramOrSizeXYZ = ParamUtilities.setParamIfPresent(scriptCommand,
        TO_TOMOGRAM_OR_SIZE_XYZ, toTomogramOrSizeXYZ);
    fromTomogramOrSizeXYZ = ParamUtilities.setParamIfPresent(scriptCommand,
        FROM_TOMOGRAM_OR_SIZE_XYZ, fromTomogramOrSizeXYZ);
    //set matchBToA
    //fiducial model parameters can be added regardless of the transfer coordinate file mode,
    //so they can be used to check the version of the script
    if (aFiducialModel == null) {
      //backwards compatibility - set transferCoordinateFile, aFiducialModel, bFiducialModel
      //  Set the matching state based on the toFiducialFile name
      setMatchBToA(toFiducialFile);
      transferCoordinateFile = null;
    }
    else {
      //scripts contains a parameter for A to B, not B to A
      matchBToA = !ParamUtilities.setParamIfPresent(scriptCommand, MATCHING_A_TO_B,
          !matchBToA);
    }
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
    ParamUtilities.updateScriptParameter(scriptCommand, TO_FIDUCIAL_FILE, toFiducialFile);
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_FIDUCIAL_FILE,
        fromFiducialFile);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_CORRESPONDENCE_LIST,
        toCorrespondenceList.toString());
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_CORRESPONDENCE_LIST,
        fromCorrespondenceList.toString());
    ParamUtilities.updateScriptParameter(scriptCommand, TRANSFER_COORDINATE_FILE,
        transferCoordinateFile);
    ParamUtilities.updateScriptParameter(scriptCommand, A_FIDUCIAL_MODEL, aFiducialModel);
    ParamUtilities.updateScriptParameter(scriptCommand, B_FIDUCIAL_MODEL, bFiducialModel);
    ParamUtilities.updateScriptParameter(scriptCommand, MATCHING_A_TO_B, !matchBToA);
    ParamUtilities.updateScriptParameter(scriptCommand, USE_POINTS, usePoints);
    ParamUtilities.updateScriptParameter(scriptCommand, XAXIS_TILTS, xAxistTilt);
    ParamUtilities.updateScriptParameter(scriptCommand, SURFACE_OR_USE_MODELS,
        surfacesOrModel);
    ParamUtilities
        .updateScriptParameter(scriptCommand, MAXIMUM_RESIDUAL, maximumResidual);
    centerShiftLimit.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_MATCHING_MODEL,
        toMatchingModel);
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_MATCHING_MODEL,
        fromMatchingModel);
    ParamUtilities.updateScriptParameter(scriptCommand, TO_TOMOGRAM_OR_SIZE_XYZ,
        toTomogramOrSizeXYZ);
    ParamUtilities.updateScriptParameter(scriptCommand, FROM_TOMOGRAM_OR_SIZE_XYZ,
        fromTomogramOrSizeXYZ);
    ParamUtilities.updateScriptParameter(scriptCommand, SCALE_FACTORS, scaleFactors);
  }

  /**
   * Merge a solvematchshift param object into this object
   * @param solvematchshift
   */
  public void mergeSolvematchshift(ConstSolvematchshiftParam solvematchshift,
      boolean modelBased) {
    if (!modelBased) {
      toFiducialFile = solvematchshift.getToFiducialCoordinatesFile();
      setMatchBToA(toFiducialFile);
      fromFiducialFile = solvematchshift.getFromFiducialCoordinatesFile();
      toCorrespondenceList = solvematchshift.getFiducialMatchListA();
      fromCorrespondenceList = solvematchshift.getFiducialMatchListB();
      xAxistTilt = solvematchshift.getXAxistTilt();
      surfacesOrModel = solvematchshift.getNSurfaces();
    }
    outputFile = solvematchshift.getOutputTransformationFile();
    maximumResidual = solvematchshift.getResidualThreshold();
    //older version, so coordinate file would not exist
    transferCoordinateFile = null;
    centerShiftLimit.set(CENTER_SHIFT_LIMIT_DEFAULT);
  }

  /**
   * Merge a solvematchmod param object into this object
   * @param solvematchmod
   */
  public void mergeSolvematchmod(ConstSolvematchmodParam solvematchmod, boolean modelBased) {
    if (modelBased) {
      toFiducialFile = solvematchmod.getToFiducialCoordinatesFile();
      setMatchBToA(toFiducialFile);
      fromFiducialFile = solvematchmod.getFromFiducialCoordinatesFile();
      toCorrespondenceList = solvematchmod.getFiducialMatchListA();
      fromCorrespondenceList = solvematchmod.getFiducialMatchListB();
      xAxistTilt = solvematchmod.getXAxistTilt();
      surfacesOrModel = solvematchmod.getNSurfaces();
    }
    toMatchingModel = solvematchmod.getToMatchingModel();
    fromMatchingModel = solvematchmod.getFromMatchingModel();
    toTomogramOrSizeXYZ = solvematchmod.getToReconstructionFile();
    fromTomogramOrSizeXYZ = solvematchmod.getFromReconstructionFile();
    //older version so coordinate file would not exist
    transferCoordinateFile = null;
    centerShiftLimit.set(CENTER_SHIFT_LIMIT_DEFAULT);
  }

  /**
   * Detect the matching direction from name of the first fiducial file list
   * Set all the fields that would change if the direction is changed, 
   * except the parameters that are handled outside of the param object.
   * @param aFiducialFilename
   */
  protected void setMatchBToA(String aFiducialFilename) {
    if (aFiducialFilename == null || aFiducialFilename.matches("\\s*")) {
      return;
    }
    aFiducialModel = null;
    bFiducialModel = null;
    if (aFiducialFilename.matches("^\\s*\\S+?afid.xyz\\s*$")) {
      matchBToA = true;
      aFiducialModel = DatasetFiles.getFiducialModelName(manager, AxisID.FIRST);
      bFiducialModel = DatasetFiles.getFiducialModelName(manager, AxisID.SECOND);
    }
    else if (aFiducialFilename.matches("^\\s*\\S+?bfid.xyz\\s*$")) {
      matchBToA = false;
      bFiducialModel = DatasetFiles.getFiducialModelName(manager, AxisID.FIRST);
      aFiducialModel = DatasetFiles.getFiducialModelName(manager, AxisID.SECOND);
    }
  }

  public void setUsePoints(String usePoints) {
    this.usePoints.parseString(usePoints);
  }

  /**
   * @param fromCorrespondenceList The fromCorrespondenceList to set.
   */
  public void setFromCorrespondenceList(String string) {
    fromCorrespondenceList.parseString(string);
  }

  /**
   * Set the transferCoordinateFile if useCorrespondenceList is false
   * @param useCorrespondenceList
   */
  public void setTransferCoordinateFile(boolean useCorrespondenceList) {
    if (useCorrespondenceList) {
      transferCoordinateFile = null;
    }
    else {
      transferCoordinateFile = DatasetFiles.getTransferFidCoordFileName();
    }
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
   * @param maximumResidual The maximumResidual to set.
   */
  public void setMaximumResidual(double maximumResidual) {
    this.maximumResidual = maximumResidual;
  }

  public void setMaximumResidual(String value) {
    this.maximumResidual = ParamUtilities.parseDouble(value);
  }

  public void setCenterShiftLimit(String centerShiftLimit) {
    this.centerShiftLimit.set(centerShiftLimit);
  }

  /**
   * @param surfaces The nSurfaces to set.
   */
  public void setSurfacesOrModel(FiducialMatch value) {
    if (value == FiducialMatch.USE_MODEL_ONLY) {
      surfacesOrModel = USE_MODEL_ONLY_OPTION;
      return;
    }
    if (value == FiducialMatch.ONE_SIDE_INVERTED) {
      surfacesOrModel = ONE_SIDE_INVERTED_OPTION;
      return;
    }
    if (value == FiducialMatch.USE_MODEL) {
      surfacesOrModel = USE_MODEL_OPTION;
      return;
    }
    if (value == FiducialMatch.ONE_SIDE) {
      surfacesOrModel = ONE_SIDE_OPTION;
      return;
    }
    if (value == FiducialMatch.BOTH_SIDES) {
      surfacesOrModel = BOTH_SIDES_OPTION;
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