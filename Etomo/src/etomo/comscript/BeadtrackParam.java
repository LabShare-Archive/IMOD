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
 * <p> Revision 3.1  2004/04/12 16:48:32  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/08/07 17:59:06  rickg
 * <p> Merged in tilt angle fix from beta2a branch
 * <p>
 * <p> Revision 2.4  2003/07/25 22:46:57  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2  2003/03/20 17:21:07  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/07 22:22:11  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class BeadtrackParam
  extends ConstBeadtrackParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the beadtrack command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
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
		if (tiltAngleSpec.getType() == TiltAngleType.FILE) {
			tiltAngleSpec.setTiltAngleFilename(inputArgs[inputLine++].getArgument());
		}
		else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
			String pair = inputArgs[inputLine++].getArgument();
			String values[] = pair.split(",");
			if (values.length != 2) {
				throw new BadComScriptException("Incorrect tilt angle specification type");
			}
			tiltAngleSpec.setRangeMin(Double.parseDouble(values[0]));
			tiltAngleSpec.setRangeStep(Double.parseDouble(values[1]));
		}
		else if (tiltAngleSpec.getType() == TiltAngleType.LIST) {
			throw new BadComScriptException("Unimplemented tilt angle specification type");
		}
		else {
			throw new BadComScriptException("Incorrect tilt angle specification type");
		}

    try {
      tiltAngleGroupParams.validateAndSet(inputArgs[inputLine++].getArgument());
      int nGroups = tiltAngleGroupParams.getInt(1);
      if (nGroups > 0) {
        StringList tiltAngleGroupsList = new StringList(nGroups);
        for (int i = 0; i < nGroups; i++) {
          tiltAngleGroupsList.set(i, inputArgs[inputLine++].getArgument());
        }
        tiltAngleGroups = ParamUtilities.parse(tiltAngleGroupsList,
            nondefaultGroupIntegerType, nondefaultGroupSize);
      }
      magnificationGroupParams.validateAndSet(
        inputArgs[inputLine++].getArgument());
      nGroups = magnificationGroupParams.getInt(1);
      if (nGroups > 0) {
        StringList magnificationGroupsList = new StringList(nGroups);
        for (int i = 0; i < nGroups; i++) {
          magnificationGroupsList.set(i, inputArgs[inputLine++].getArgument());
        }
        magnificationGroups = ParamUtilities.parse(magnificationGroupsList,
            nondefaultGroupIntegerType, nondefaultGroupSize);
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
  
  public void initializeDefaults() {
    tiltAngleGroups = null;
    magnificationGroups = null;
  }

  /**
   * Update the supplied ComScriptCommand with the parameters of this object.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command.
   * @throws BadComScriptException when the script command does not contain a
   * beadtrack command or the number of input arguments is incorrect.
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
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

  public void setTiltAngleGroups(String newTiltAngleGroups) throws FortranInputSyntaxException {
    tiltAngleGroups = ParamUtilities.parse(newTiltAngleGroups, true, nondefaultGroupSize);
    if (tiltAngleGroups == null) {
      tiltAngleGroupParams.setDefault(1);
    }
    else {
      tiltAngleGroupParams.set(1, tiltAngleGroups.length);
    }
  }

  public void setMagnificationGroupSize(int groupSize) {
    magnificationGroupParams.set(0, groupSize);
  }

  public void setMagnificationGroups(String newMagnificationGroups) throws FortranInputSyntaxException {
    magnificationGroups = ParamUtilities.parse(newMagnificationGroups, true, nondefaultGroupSize);
    if (tiltAngleGroups == null) {
      magnificationGroupParams.setDefault(1);
    }
    else {
      magnificationGroupParams.set(1, magnificationGroups.length);
    }
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

    // Tilt angle source and filenames/ranges are not modified by this class
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
      // TODO throw exception so calling object can catch and display a message 
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
      tiltAngleGroup.setArgument(tiltAngleGroups[i].toString());
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
      // TODO throw exception so calling object can catch and display a message 
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
      magnificationGroup.setArgument(magnificationGroups[i].toString());
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
