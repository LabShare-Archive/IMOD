package etomo.comscript;

import java.util.ArrayList;
import javax.swing.JOptionPane;

import etomo.type.TiltAngleType;

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
 * <p> $Log$
 * <p> Revision 1.2  2002/10/07 22:22:11  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class BeadtrackParam extends ConstBeadtrackParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the beadtrack command
   * and parameters.
   */
  public void initialize(ComScriptCommand scriptCommand)
    throws BadComScriptException, FortranInputSyntaxException {

    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    int inputLine = 0;
    inputFile = inputArgs[inputLine++].getArgument();
    pieceListFile = inputArgs[inputLine++].getArgument();
    seedModelFile = inputArgs[inputLine++].getArgument();
    outputModelFile = inputArgs[inputLine++].getArgument();
    viewSkipList = inputArgs[inputLine++].getArgument();
    imageRotation = Double.parseDouble(inputArgs[inputLine++].getArgument());

    nAdditionalViewSets =
      Integer.parseInt(inputArgs[inputLine++].getArgument());
    if (nAdditionalViewSets > 0) {
      additionalViewGroups = new StringList(nAdditionalViewSets);
      for (int i = 0; i < nAdditionalViewSets; i++) {
        additionalViewGroups.set(i, inputArgs[inputLine++].getArgument());
      }
    }

    int typeSpec = Integer.parseInt(inputArgs[inputLine++].getArgument());
    tiltAngleSpec.setType(TiltAngleType.parseInt(typeSpec));
    // FIXME: what if the next argument is not a filename!
    tiltAngleSpec.setTiltAngleFilename(inputArgs[inputLine++].getArgument());

    try {
      tiltAngleGroupParams.validateAndSet(inputArgs[inputLine++].getArgument());
      int nGroups = tiltAngleGroupParams.getInt(1);
      if (nGroups > 0) {
        tiltAngleGroups = new StringList(nGroups);
        for (int i = 0; i < nGroups; i++) {
          tiltAngleGroups.set(i, inputArgs[inputLine++].getArgument());
        }
      }
      magnificationGroupParams.validateAndSet(
        inputArgs[inputLine++].getArgument());
      nGroups = magnificationGroupParams.getInt(1);
      if (nGroups > 0) {
        magnificationGroups = new StringList(nGroups);
        for (int i = 0; i < nGroups; i++) {
          magnificationGroups.set(i, inputArgs[inputLine++].getArgument());
        }
      }
      nMinViews = Integer.parseInt(inputArgs[inputLine++].getArgument());
      fiducialParams.validateAndSet(inputArgs[inputLine++].getArgument());
      if (Integer.parseInt(inputArgs[inputLine++].getArgument()) > 0) {
        fillGaps = true;
      }
      else {
        fillGaps = false;
      }
      maxGap = Integer.parseInt(inputArgs[inputLine++].getArgument());
      tiltAngleMinRange.validateAndSet(inputArgs[inputLine++].getArgument());
      searchBoxPixels.validateAndSet(inputArgs[inputLine++].getArgument());
      maxFiducialsAvg = Integer.parseInt(inputArgs[inputLine++].getArgument());
      fiducialExtrapolationParams.validateAndSet(
        inputArgs[inputLine++].getArgument());
      rescueAttemptParams.validateAndSet(inputArgs[inputLine++].getArgument());
      minRescueDistance =
        Integer.parseInt(inputArgs[inputLine++].getArgument());
      rescueRelaxationParams.validateAndSet(
        inputArgs[inputLine++].getArgument());
      residualDistanceLimit =
        Double.parseDouble(inputArgs[inputLine++].getArgument());
      secondPassParams.validateAndSet(inputArgs[inputLine++].getArgument());
      meanResidChangeLimits.validateAndSet(
        inputArgs[inputLine++].getArgument());
      deletionParams.validateAndSet(inputArgs[inputLine++].getArgument());
    }
    catch (FortranInputSyntaxException except) {
      String message =
        "Parse error in beadtrack command, standard input argument: "
          + String.valueOf(inputLine)
          + "\n"
          + except.getMessage();
      throw new FortranInputSyntaxException(message, except.getNewString());
    }

  }

  /**
   * Update the supplied ComScriptCommand with the parameters of this object.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command.
   * @throws BadComScriptException when the script command does not contain a
   * beadtrack command or the number of input arguments is incorrect.
   */
  public void updateComScript(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Get the existing input arguments from the command, this is so the
    //  comments are preserved.
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    inputArgs = updateInputArgs(inputArgs);
    scriptCommand.setInputArguments(inputArgs);
  }

  /**
   * Validate the parameters stored in the BeadtrackObject
   */
  public boolean isValid() {
    ArrayList errors = new ArrayList();
    //  Compare the number of additional view sets and the number of entries in
    //  in additionalViewGroups
    if (nAdditionalViewSets != additionalViewGroups.getNElements()) {
      errors.add(
        "The number of additional view groups does not equal the number specified");
      errors.add(
        "  number of additional views sets: "
          + String.valueOf(nAdditionalViewSets));
      errors.add(
        "  number of list entries: "
          + String.valueOf(additionalViewGroups.getNElements()));

    }
    return true;
  }

  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  public void setPieceListFile(String pieceListFile) {
    this.pieceListFile = pieceListFile;
  }

  public void setSeedModelFile(String seedModelFile) {
    this.seedModelFile = seedModelFile;
  }

  public void setOutputModelFile(String outputModelFile) {
    this.outputModelFile = outputModelFile;
  }

  public void setViewSkipList(String viewSkipList) {
    this.viewSkipList = viewSkipList;
  }

  public void setImageRotation(double imageRotation) {
    this.imageRotation = imageRotation;
  }

  public void setAdditionalViewGroups(String newAdditionalViewGroups) {
    additionalViewGroups.parseString(newAdditionalViewGroups);
    nAdditionalViewSets = additionalViewGroups.getNElements();
  }

  public void setTiltAngleGroupSize(int groupSize) {
    tiltAngleGroupParams.set(0, groupSize);
  }

  public void setTiltAngleGroups(String newTiltAngleGroups) {
    tiltAngleGroups.parseString(newTiltAngleGroups);
    tiltAngleGroupParams.set(1, tiltAngleGroups.getNElements());
  }

  public void setMagnificationGroupSize(int groupSize) {
    magnificationGroupParams.set(0, groupSize);
  }

  public void setMagnificationGroups(String newMagnificationGroups) {
    this.magnificationGroups.parseString(newMagnificationGroups);
    magnificationGroupParams.set(1, magnificationGroups.getNElements());
  }

  public void setNMinViews(int nMinViews) {
    this.nMinViews = nMinViews;
  }

  public void setFiducialParams(String fiducialParams)
    throws FortranInputSyntaxException {
    this.fiducialParams.validateAndSet(fiducialParams);
  }

  public void setFillGaps(boolean fillGaps) {
    this.fillGaps = fillGaps;
  }

  public void setMaxGap(int maxGap) {
    this.maxGap = maxGap;
  }

  public void setTiltAngleMinRange(String tiltAngleMinRange)
    throws FortranInputSyntaxException {
    this.tiltAngleMinRange.validateAndSet(tiltAngleMinRange);
  }

  public void setSearchBoxPixels(String searchBoxPixels)
    throws FortranInputSyntaxException {
    this.searchBoxPixels.validateAndSet(searchBoxPixels);
  }

  public void setMaxFiducialsAvg(int maxFiducialsAvg) {
    this.maxFiducialsAvg = maxFiducialsAvg;
  }

  public void setFiducialExtrapolationParams(String fiducialExtrapolationParams)
    throws FortranInputSyntaxException {
    this.fiducialExtrapolationParams.validateAndSet(
      fiducialExtrapolationParams);
  }

  public void setRescueAttemptParams(String rescueAttemptParams)
    throws FortranInputSyntaxException {
    this.rescueAttemptParams.validateAndSet(rescueAttemptParams);
  }

  public void setMinRescueDistance(int minRescueDistance) {
    this.minRescueDistance = minRescueDistance;
  }

  public void setRescueRelaxationParams(String rescueRelaxationParams)
    throws FortranInputSyntaxException {
    this.rescueRelaxationParams.validateAndSet(rescueRelaxationParams);
  }

  public void setResidualDistanceLimit(double residualDistanceLimit) {
    this.residualDistanceLimit = residualDistanceLimit;
  }

  public void setSecondPassParams(String secondPassParams)
    throws FortranInputSyntaxException {
    this.secondPassParams.validateAndSet(secondPassParams);
  }

  public void setMeanResidChangeLimits(String meanResidChangeLimits)
    throws FortranInputSyntaxException {
    this.meanResidChangeLimits.validateAndSet(meanResidChangeLimits);
  }

  public void setDeletionParams(String deletionParams)
    throws FortranInputSyntaxException {
    this.deletionParams.validateAndSet(deletionParams);
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a beadtrack command
    if (!scriptCommand.getCommand().equals("beadtrack")) {
      throw (new BadComScriptException("Not a beadtrack command"));
    }

    //  Extract the parameters
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length < 26) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to beadtrack command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected at least 26."));
    }

    return inputArgs;
  }

  /**
   * Update the inputArguments array with new parameters
   * @param inputArgs the array of existing input arguments, some of these
   * will be used for default values that BeadtrackParam currently does not
   * modify.
   * @return the new array of ComScriptInputArgs.  Note that this is a newly
   * allocated array since the number of elements may be different than the
   * input parameter.
   */
  private ComScriptInputArg[] updateInputArgs(ComScriptInputArg[] inputArgs) {
    ArrayList inputArgList = new ArrayList();

    //  Fill in the input argument sequence
    int srcListCount = 0;

    inputArgs[srcListCount].setArgument(inputFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(pieceListFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(seedModelFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(outputModelFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(viewSkipList);
    inputArgList.add(inputArgs[srcListCount++]);

    // tilt axis angle of rotation parameter [5],
    // this is a system/mag/camera variable
    inputArgList.add(inputArgs[srcListCount++]);

    // Increment the source list counter to skip the old view groups, the
    // comments for those entries are most likely not applicable
    int nSrcSets = Integer.parseInt(inputArgs[srcListCount].getArgument());
    inputArgs[srcListCount].setArgument(String.valueOf(nAdditionalViewSets));
    inputArgList.add(inputArgs[srcListCount++]);
    srcListCount = srcListCount + nSrcSets;
    for (int i = 0; i < nAdditionalViewSets; i++) {
      ComScriptInputArg viewSet = new ComScriptInputArg();
      viewSet.setArgument(additionalViewGroups.get(i));
      inputArgList.add(viewSet);
    }

    // Tilt angle source and filenames are not modified by this class
    inputArgList.add(inputArgs[srcListCount++]);
    inputArgList.add(inputArgs[srcListCount++]);

    // Tilt angle groups
    // The number of non-standard tilt angle sets is the second parameter
    // in the comma separated list.
    FortranInputString fis = new FortranInputString(2);
    fis.setIntegerType(0, true);
    fis.setIntegerType(1, true);
    try {
      nSrcSets = 0;
      fis.validateAndSet(inputArgs[srcListCount].getArgument());
      nSrcSets = fis.getInt(1);
    }
    catch (FortranInputSyntaxException except) {
      //  FIXME this should probably throw an excpetion or set a state so that
      //  some other code above a handles it
      String[] errorMessage = new String[5];
      errorMessage[0] = "BeadtrackParam Error";
      errorMessage[1] = "Existing beadtrack tilt angle parameter was incorrect";
      errorMessage[2] =
        "Don't know how many non-default tilt angle groups, assuming 0";
      errorMessage[3] =
        "Input string: " + inputArgs[srcListCount].getArgument();
      errorMessage[4] = except.getMessage();
      JOptionPane.showMessageDialog(
        null,
        errorMessage,
        "BeadtrackParam Error",
        JOptionPane.ERROR_MESSAGE);
    }

    inputArgs[srcListCount].setArgument(tiltAngleGroupParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    srcListCount = srcListCount + nSrcSets;
    for (int i = 0; i < tiltAngleGroupParams.getInt(1); i++) {
      ComScriptInputArg tiltAngleGroup = new ComScriptInputArg();
      tiltAngleGroup.setArgument(tiltAngleGroups.get(i));
      inputArgList.add(tiltAngleGroup);
    }

    // Magnification groups
    // The number of non-standard magnification sets is the second parameter
    // in the comma separated list.
    fis.setIntegerType(0, true);
    fis.setIntegerType(1, true);
    try {
      nSrcSets = 0;
      fis.validateAndSet(inputArgs[srcListCount].getArgument());
      nSrcSets = fis.getInt(1);
    }
    catch (FortranInputSyntaxException except) {
      //  FIXME this should probably throw an excpetion or set a state so that
      //  some other code above a handles it
      String[] errorMessage = new String[5];
      errorMessage[0] = "BeadtrackParam Error";
      errorMessage[1] =
        "Existing beadtrack magnification parameter was incorrect";
      errorMessage[2] =
        "Don't know how many non-default magnification groups, assuming 0";
      errorMessage[3] =
        "Input string: " + inputArgs[srcListCount].getArgument();
      errorMessage[4] = except.getMessage();
      JOptionPane.showMessageDialog(
        null,
        errorMessage,
        "BeadtrackParam Error",
        JOptionPane.ERROR_MESSAGE);
    }
    inputArgs[srcListCount].setArgument(magnificationGroupParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);
    srcListCount = srcListCount + nSrcSets;
    for (int i = 0; i < magnificationGroupParams.getInt(1); i++) {
      ComScriptInputArg magnificationGroup = new ComScriptInputArg();
      magnificationGroup.setArgument(magnificationGroups.get(i));
      inputArgList.add(magnificationGroup);
    }

    inputArgs[srcListCount].setArgument(String.valueOf(nMinViews));
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(fiducialParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    if (fillGaps) {
      inputArgs[srcListCount].setArgument("1");
    }
    else {
      inputArgs[srcListCount].setArgument("0");
    }
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(String.valueOf(maxGap));
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(tiltAngleMinRange.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(searchBoxPixels.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(String.valueOf(maxFiducialsAvg));
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(fiducialExtrapolationParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(rescueAttemptParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(String.valueOf(minRescueDistance));
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(rescueRelaxationParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(String.valueOf(residualDistanceLimit));
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(secondPassParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(meanResidChangeLimits.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(deletionParams.toString());
    inputArgList.add(inputArgs[srcListCount++]);

    return (ComScriptInputArg[]) inputArgList.toArray(
      new ComScriptInputArg[inputArgList.size()]);
  }
}
