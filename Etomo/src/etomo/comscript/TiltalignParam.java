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
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
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
public class TiltalignParam
  extends ConstTiltalignParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Constructor for TiltalignParam.
   */
  public TiltalignParam() {
    super();
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tiltalign command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {

    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getComScriptArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    //  Read in inputArgs
    int inputLine = 0;
    modelFile = inputArgs[inputLine++].getArgument();
    imageFile = inputArgs[inputLine++].getArgument();
    if (imageFile.matches("\\s*")) {
      imageParameters.validateAndSet(inputArgs[inputLine++].getArgument());
    }
    imodFiducialPosFile = inputArgs[inputLine++].getArgument();
    asciiFiducialPosFile = inputArgs[inputLine++].getArgument();
    tiltAngleSolutionFile = inputArgs[inputLine++].getArgument();
    transformSolutionFile = inputArgs[inputLine++].getArgument();
    solutionType = Integer.parseInt(inputArgs[inputLine++].getArgument());

    includeExcludeType = Integer.parseInt(inputArgs[inputLine++].getArgument());
    if (includeExcludeType > 0) {
      includeExcludeList.parseString(inputArgs[inputLine++].getArgument());
    }
    initialImageRotation =
      Double.parseDouble(inputArgs[inputLine++].getArgument());
    rotationAngleSolutionType =
      Integer.parseInt(inputArgs[inputLine++].getArgument());

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
    // TODO what if the next argument is not a filename!
    tiltAngleSpec.setTiltAngleFilename(inputArgs[inputLine++].getArgument());

    tiltAngleOffset = Double.parseDouble(inputArgs[inputLine++].getArgument());

    try {
      //  Tilt angle solution parameters
      tiltAngleSolution.type =
        Integer.parseInt(inputArgs[inputLine++].getArgument());

      //  NOTE shouldn't be a specific integer
      //        what about others
      if (!(tiltAngleSolution.type == 0
        || tiltAngleSolution.type == 2
        || tiltAngleSolution.type == 5)) {
        String message = "Don't know how to handle arbitrary tilt views yet!!!";
        throw new InvalidParameterException(message);
      }
      if (tiltAngleSolution.type == 5) {
        inputLine = parseGroup(tiltAngleSolution, inputArgs, inputLine);
      }

      //  Magnification solution parameters
      magnificationSolution.referenceView.validateAndSet(
        inputArgs[inputLine++].getArgument());
      magnificationSolution.type =
        Integer.parseInt(inputArgs[inputLine++].getArgument());
      //  NOTE shouldn't be a specific integer, what about others
      if (magnificationSolution.type == 2) {
        String message =
          "Don't know how to handle arbitrary magnification views yet!!!";
        throw new InvalidParameterException(message);
      }
      if (magnificationSolution.type > 2) {
        inputLine = parseGroup(magnificationSolution, inputArgs, inputLine);
      }

      // Compression solution parameters
      compressionSolution.referenceView.validateAndSet(
        inputArgs[inputLine++].getArgument());
      if (compressionSolution.referenceView.getInt(0) > 0) {
        compressionSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());
        //  NOTE shouldn't be a specific integer, what about others
        if (compressionSolution.type == 2) {
          String message =
            "Don't know how to handle arbitrary compression views yet!!!";
          throw new InvalidParameterException(message);
        }
        if (compressionSolution.type > 2) {
          inputLine = parseGroup(compressionSolution, inputArgs, inputLine);
        }
      }

      distortionSolutionType =
        Integer.parseInt(inputArgs[inputLine++].getArgument());
      //  If the distortion solution type is 1 then both the xstretch and
      //  and skew parameters are stored in the xstretch solution parameters
      //  If the distortion solution type is 2 then the xstretch solution
      //  parameters are loaded now and the skew parameters are loaded in the
      //  next statement block
      if (distortionSolutionType > 0) {
        xstretchSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());

        if (xstretchSolution.type == 2) {
          String message =
            "Don't know how to handle arbitrary distortion views yet!!!";
          throw new InvalidParameterException(message);
        }

        if (xstretchSolution.type > 2) {
          inputLine = parseGroup(xstretchSolution, inputArgs, inputLine);
        }
      }

      if (distortionSolutionType == 2) {
        skewSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());

        if (skewSolution.type == 2) {
          String message =
            "Don't know how to handle arbitrary distortion views yet!!!";
          throw new InvalidParameterException(message);
        }

        if (skewSolution.type > 2) {
          inputLine = parseGroup(skewSolution, inputArgs, inputLine);
        }
      }

      residualThreshold =
        Double.parseDouble(inputArgs[inputLine++].getArgument());
      nSurfaceAnalysis = Integer.parseInt(inputArgs[inputLine++].getArgument());
      minimizationParams.validateAndSet(inputArgs[inputLine++].getArgument());

      //  Check to see if tranformations are being computed
      //  FIXME is the test variable correct?
      if (solutionType > 0) {
        tiltAxisZShift =
          Double.parseDouble(inputArgs[inputLine++].getArgument());
        tiltAxisXShift =
          Double.parseDouble(inputArgs[inputLine++].getArgument());
      }

      //  Local alignment parsing
      int localAlignmentState =
        Integer.parseInt(inputArgs[inputLine++].getArgument());
      if (localAlignmentState == 1) {
        localAlignments = true;
      }
      else {
        localAlignments = false;
      }
      //  NOTE do we always want to do this?
      if (inputArgs.length > inputLine) {
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localTransformFile = inputArgs[inputLine++].getArgument();
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        nLocalPatches.validateAndSet(inputArgs[inputLine++].getArgument());
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        minLocalPatchSize.validateAndSet(inputArgs[inputLine++].getArgument());
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        minLocalFiducials.validateAndSet(inputArgs[inputLine++].getArgument());
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        int fixFiducialState =
          Integer.parseInt(inputArgs[inputLine++].getArgument());
        fixLocalFiducialCoodinates = false;
        if (fixFiducialState == 1) {
          fixLocalFiducialCoodinates = true;
        }
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localOutputSelection.validateAndSet(
          inputArgs[inputLine++].getArgument());

        //  local rotation solution parameters

        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localRotationSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());
        if (localRotationSolution.type == 2) {
          String message =
            "Don't know how to handle arbitrary local rotation views yet!!!";
          throw new InvalidParameterException(message);
        }
        if (localRotationSolution.type > 2) {
          inputLine = parseGroup(localRotationSolution, inputArgs, inputLine);
        }

        // local tilt solution parameters
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localTiltSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());

        if (!(localTiltSolution.type == 0
          || localTiltSolution.type == 2
          || localTiltSolution.type == 5)) {
          String message =
            "Don't know how to handle arbitrary local tilt views yet!!!";
          throw new InvalidParameterException(message);
        }
        if (localTiltSolution.type == 5) {
          inputLine = parseGroup(localTiltSolution, inputArgs, inputLine);
        }

        //  local magnification solution parameters
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localMagnificationSolution.referenceView.validateAndSet(
          inputArgs[inputLine++].getArgument());
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localMagnificationSolution.type =
          Integer.parseInt(inputArgs[inputLine++].getArgument());
        //  NOTE shouldn't be a specific integer, what about others
        if (localMagnificationSolution.type == 2) {
          String message =
            "Don't know how to handle arbitrary local magnification views yet!!!";
          throw new InvalidParameterException(message);
        }
        if (localMagnificationSolution.type > 2) {
          inputLine =
            parseGroup(localMagnificationSolution, inputArgs, inputLine);
        }

        // local distortion solution parameters
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        localDistortionSolutionType =
          Integer.parseInt(inputArgs[inputLine++].getArgument());
        //  Duplicate the distortion solution type for both xstretch and skew
        if (localDistortionSolutionType == 1) {
          inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
          localXstretchSolution.type =
            Integer.parseInt(inputArgs[inputLine++].getArgument());
          localSkewSolution.type = xstretchSolution.type;
          if (localXstretchSolution.type == 2) {
            String message =
              "Don't know how to handle arbitrary local distortion views yet!!!";
            throw new InvalidParameterException(message);
          }
          if (localXstretchSolution.type > 2) {
            inputLine = parseGroup(localXstretchSolution, inputArgs, inputLine);
          }
        }
        if (localDistortionSolutionType == 2) {
          inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
          localXstretchSolution.type =
            Integer.parseInt(inputArgs[inputLine++].getArgument());
          if (localXstretchSolution.type == 2) {
            String message =
              "Don't know how to handle arbitrary local distortion views yet!!!";
            throw new InvalidParameterException(message);
          }
          if (localXstretchSolution.type > 2) {
            inputLine = parseGroup(localXstretchSolution, inputArgs, inputLine);
          }
          inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
          localSkewSolution.type =
            Integer.parseInt(inputArgs[inputLine++].getArgument());
          if (localSkewSolution.type == 2) {
            String message =
              "Don't know how to handle arbitrary local distortion views yet!!!";
            throw new InvalidParameterException(message);
          }
          if (localSkewSolution.type > 2) {
            inputLine = parseGroup(localSkewSolution, inputArgs, inputLine);
          }
        }
      }
    }
    catch (FortranInputSyntaxException except) {
      String message =
        "Parse error in tiltalign command, standard input argument: "
          + String.valueOf(inputLine)
          + "\n"
          + except.getMessage();
      throw new FortranInputSyntaxException(message, except.getNewString());
    }

  }

  /**
   * Update the script command with the current valus of this TiltxcorrParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getComScriptArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    inputArgs = putComScriptArguments(inputArgs);
    scriptCommand.setInputArguments(inputArgs);
  }

  //
  //  Set the values of the tiltalign parameters
  //
  public void setModelFile(String filename) {
    modelFile = filename;
  }

  public void setImageFile(String filename) {
    imageFile = filename;
  }

  public void setImageParameters(String newImageParameters)
    throws FortranInputSyntaxException {
    imageParameters.validateAndSet(newImageParameters);
  }

  public void setImodFiducialPosFile(String filename) {
    imodFiducialPosFile = filename;
  }

  public void setAsciiFiducialPosFile(String filename) {
    asciiFiducialPosFile = filename;
  }

  public void setTiltAngleSolutionFile(String filename) {
    tiltAngleSolutionFile = filename;
  }

  public void setTransformSolutionFile(String filename) {
    transformSolutionFile = filename;
  }

  //TODO validation
  public void setSolutionType(int type) {
    solutionType = type;
  }

  public void setIncludeExcludeType(int code) {
    includeExcludeType = code;
  }

  //TODO validation
  public void setIncludeExcludeList(String zList) {
    includeExcludeList.parseString(zList);
  }

  public void setInitialImageRoation(double angle) {
    initialImageRotation = angle;
  }

  public void setRotationAngleSolutionType(int type) {
    rotationAngleSolutionType = type;
  }

  public void setAdditionalViewGroups(String newList) {
    additionalViewGroups.parseString(newList);
    nAdditionalViewSets = additionalViewGroups.getNElements();
  }

  public void setTiltAngleOffset(String newTiltAngleOffset) {
    tiltAngleOffset = Double.parseDouble(newTiltAngleOffset);
  }

  public void setTiltAngleSolutionType(int type) {
    tiltAngleSolution.type = type;
  }

  public void setTiltAngleSolutionGroupSize(int size) {
    tiltAngleSolution.params.set(0, size);
  }

  public void setTiltAngleSolutionGroupSize(String size) {
    tiltAngleSolution.params.set(0, Integer.parseInt(size));
  }

  public void setTiltAngleSolutionAdditionalGroups(String list) {
    tiltAngleSolution.additionalGroups.parseString(list);
    tiltAngleSolution.params.set(
      1,
      tiltAngleSolution.additionalGroups.getNElements());
  }

  public void setMagnificationReferenceView(String newReferenceView)
    throws FortranInputSyntaxException {
    magnificationSolution.referenceView.validateAndSet(newReferenceView);
  }

  public void setMagnificationType(int type) {
    magnificationSolution.type = type;
  }

  public void setMagnificationSolutionGroupSize(int size) {
    magnificationSolution.params.set(0, size);
  }

  public void setMagnificationSolutionGroupSize(String size) {
    magnificationSolution.params.set(0, Integer.parseInt(size));
  }

  public void setMagnificationSolutionAdditionalGroups(String list) {
    magnificationSolution.additionalGroups.parseString(list);
    magnificationSolution.params.set(
      1,
      magnificationSolution.additionalGroups.getNElements());
  }

  public void setCompressionReferenceView(String newReferenceView)
    throws FortranInputSyntaxException {
    compressionSolution.referenceView.validateAndSet(newReferenceView);
  }

  public void setCompressionType(int type) {
    compressionSolution.type = type;
  }

  public void setCompressionSolutionGroupSize(int size) {
    compressionSolution.params.set(0, size);
  }

  public void setCompressionSolutionGroupSize(String size) {
    compressionSolution.params.set(0, Integer.parseInt(size));
  }

  public void setCompressionSolutionAdditionalGroups(String list) {
    compressionSolution.additionalGroups.parseString(list);
    compressionSolution.params.set(
      1,
      compressionSolution.additionalGroups.getNElements());
  }

  public void setDistortionSolutionType(int type) {
    distortionSolutionType = type;
  }

  public void setXstretchType(int type) {
    xstretchSolution.type = type;
  }

  public void setXstretchSolutionGroupSize(int size) {
    xstretchSolution.params.set(0, size);
  }

  public void setXstretchSolutionGroupSize(String size) {
    xstretchSolution.params.set(0, Integer.parseInt(size));
  }

  public void setXstretchSolutionAdditionalGroups(String list) {
    xstretchSolution.additionalGroups.parseString(list);
    xstretchSolution.params.set(
      1,
      xstretchSolution.additionalGroups.getNElements());
  }

  public void setSkewType(int type) {
    skewSolution.type = type;
  }

  public void setSkewSolutionGroupSize(int size) {
    skewSolution.params.set(0, size);
  }

  public void setSkewSolutionGroupSize(String size) {
    skewSolution.params.set(0, Integer.parseInt(size));
  }

  public void setSkewSolutionAdditionalGroups(String list) {
    skewSolution.additionalGroups.parseString(list);
    skewSolution.params.set(1, skewSolution.additionalGroups.getNElements());
  }

  public void setResidualThreshold(double threshold) {
    residualThreshold = threshold;
  }

  public void setNSurfaceAnalysis(String newNSurfaceAnalysis) {
    nSurfaceAnalysis = Integer.parseInt(newNSurfaceAnalysis);
  }

  public void setMinimizationParams(String params)
    throws FortranInputSyntaxException {
    minimizationParams.validateAndSet(params);
  }

  public void setTiltAxisZShift(double shift) {
    tiltAxisZShift = shift;
  }

  public void setTiltAxisZShift(String shift) {
    tiltAxisZShift = Double.parseDouble(shift);
  }

  public void setTiltAxisXShift(double shift) {
    tiltAxisXShift = shift;
  }

  public void setTiltAxisXShift(String shift) {
    tiltAxisXShift = Double.parseDouble(shift);
  }

  public void setLocalAlignments(boolean state) {
    localAlignments = state;
  }

  public void setLocalTransformFile(String filename) {
    localTransformFile = filename;
  }

  public void setMetroFactor(String factor) {
    minimizationParams.set(0, Double.parseDouble(factor));
  }

  public void setCycleLimit(String limit) {
    minimizationParams.set(1, Integer.parseInt(limit));
  }

  public void setNLocalPatches(String params)
    throws FortranInputSyntaxException {
    nLocalPatches.validateAndSet(params);
  }

  public void setMinLocalPatchSize(String params)
    throws FortranInputSyntaxException {
    minLocalPatchSize.validateAndSet(params);
  }

  public void setMinLocalFiducials(String params)
    throws FortranInputSyntaxException {
    minLocalFiducials.validateAndSet(params);
  }

  public void setFixLocalFiducialCoodinates(boolean state) {
    fixLocalFiducialCoodinates = state;
  }

  public void setLocalOutputSelection(String params)
    throws FortranInputSyntaxException {
    localOutputSelection.validateAndSet(params);
  }

  public void setLocalRotationSolutionType(int type) {
    localRotationSolution.type = type;
  }

  public void setLocalRotationSolutionGroupSize(int size) {
    localRotationSolution.params.set(0, size);
  }

  public void setLocalRotationSolutionGroupSize(String size) {
    localRotationSolution.params.set(0, Integer.parseInt(size));
  }

  public void setLocalRotationSolutionAdditionalGroups(String list) {
    localRotationSolution.additionalGroups.parseString(list);
    localRotationSolution.params.set(
      1,
      localRotationSolution.additionalGroups.getNElements());
  }

  public void setLocalTiltSolutionType(int type) {
    localTiltSolution.type = type;
  }

  public void setLocalTiltSolutionGroupSize(int size) {
    localTiltSolution.params.set(0, size);
  }

  public void setLocalTiltSolutionGroupSize(String size) {
    localTiltSolution.params.set(0, Integer.parseInt(size));
  }

  public void setLocalTiltSolutionAdditionalGroups(String list) {
    localTiltSolution.additionalGroups.parseString(list);
    localTiltSolution.params.set(
      1,
      localTiltSolution.additionalGroups.getNElements());
  }

  public void setLocalMagnificationReferenceView(String newReferenceView)
    throws FortranInputSyntaxException {
    localMagnificationSolution.referenceView.validateAndSet(newReferenceView);
  }

  public void setLocalMagnificationType(int type) {
    localMagnificationSolution.type = type;
  }

  public void setLocalMagnificationSolutionGroupSize(int size) {
    localMagnificationSolution.params.set(0, size);
  }

  public void setLocalMagnificationSolutionGroupSize(String size) {
    localMagnificationSolution.params.set(0, Integer.parseInt(size));
  }

  public void setLocalMagnificationSolutionAdditionalGroups(String list) {
    localMagnificationSolution.additionalGroups.parseString(list);
    localMagnificationSolution.params.set(
      1,
      localMagnificationSolution.additionalGroups.getNElements());
  }

  public void setLocalDistortionSolutionType(int type) {
    localDistortionSolutionType = type;
  }

  public void setLocalXstretchType(int type) {
    localXstretchSolution.type = type;
  }

  public void setLocalXstretchSolutionGroupSize(int size) {
    localXstretchSolution.params.set(0, size);
  }

  public void setLocalXstretchSolutionGroupSize(String size) {
    localXstretchSolution.params.set(0, Integer.parseInt(size));
  }

  public void setLocalXstretchSolutionAdditionalGroups(String list) {
    localXstretchSolution.additionalGroups.parseString(list);
    localXstretchSolution.params.set(
      1,
      localXstretchSolution.additionalGroups.getNElements());
  }

  public void setLocalSkewType(int type) {
    localSkewSolution.type = type;
  }

  public void setLocalSkewSolutionGroupSize(int size) {
    localSkewSolution.params.set(0, size);
  }

  public void setLocalSkewSolutionGroupSize(String size) {
    localSkewSolution.params.set(0, Integer.parseInt(size));
  }

  public void setLocalSkewSolutionAdditionalGroups(String list) {
    localSkewSolution.additionalGroups.parseString(list);
    localSkewSolution.params.set(
      1,
      localSkewSolution.additionalGroups.getNElements());
  }

  /**
   * Return a string representation of the values in the object
   */
  public String toString() {
    StringBuffer buffer = new StringBuffer();

    buffer.append("\nModel file: ");
    buffer.append(modelFile);

    buffer.append("\nImage file: ");
    buffer.append(imageFile);

    buffer.append("\nImage parameters: ");
    buffer.append(imageParameters.toString());

    buffer.append("\nIMOD Fiducial Pos File: ");
    buffer.append(imodFiducialPosFile);

    buffer.append("\nASCII Fiducial Pos File: ");
    buffer.append(asciiFiducialPosFile);

    buffer.append("\nTilt Angle Solution File: ");
    buffer.append(tiltAngleSolutionFile);

    buffer.append("\n Transform Solution File: ");
    buffer.append(transformSolutionFile);

    buffer.append("\nSolution Type: ");
    buffer.append(solutionType);

    buffer.append("\nInclude Points: ");
    buffer.append(includeExcludeType);

    buffer.append("\nInclude Points ZRange: ");
    buffer.append(includeExcludeList);

    buffer.append("\nInitial Image Rotation: ");
    buffer.append(initialImageRotation);

    buffer.append("\nRotation Angle Solution Type: ");
    buffer.append(rotationAngleSolutionType);

    buffer.append("\nN Additional View Sets: ");
    buffer.append(nAdditionalViewSets);

    buffer.append("\nadditionalViewGroups: ");
    buffer.append(additionalViewGroups);

    buffer.append("\ntiltAngleSpec: ");
    buffer.append(tiltAngleSpec);

    buffer.append("\ntiltAngleOffset: ");
    buffer.append(tiltAngleOffset);

    buffer.append("\ntiltAngleSolution.type: ");
    buffer.append(tiltAngleSolution.type);

    buffer.append("\ntiltAngleSolution.params: ");
    buffer.append(tiltAngleSolution.params.toString());

    buffer.append("\tiltAngleSolution.additionalGroups: ");
    buffer.append(tiltAngleSolution.additionalGroups);

    buffer.append("\nmagnificationSolution.referenceView: ");
    buffer.append(magnificationSolution.referenceView);

    buffer.append("\nmagnificationSolution.type: ");
    buffer.append(magnificationSolution.type);

    buffer.append("\nmagnificationSolution.params: ");
    buffer.append(magnificationSolution.params);

    buffer.append("\nmagnificationSolution.additionalGroups: ");
    buffer.append(magnificationSolution.additionalGroups);

    buffer.append("\ncompressionSolution.referenceView: ");
    buffer.append(compressionSolution.referenceView);

    buffer.append("\ncompressionSolution.type: ");
    buffer.append(compressionSolution.type);

    buffer.append("\ncompressionSolution.params: ");
    buffer.append(compressionSolution.params);

    buffer.append("\ncompressionSolution.additionalGroups: ");
    buffer.append(compressionSolution.additionalGroups);

    buffer.append("\ndistortionSolutionType: ");
    buffer.append(distortionSolutionType);

    buffer.append("\nxstretchSolution.type: ");
    buffer.append(xstretchSolution.type);

    buffer.append("\nxstretchSolution.params: ");
    buffer.append(xstretchSolution.params);

    buffer.append("\nxstretchSolution.additionalGroups: ");
    buffer.append(xstretchSolution.additionalGroups);

    buffer.append("\nskewSolution.type: ");
    buffer.append(skewSolution.type);

    buffer.append("\nskewSolution.params: ");
    buffer.append(skewSolution.params);

    buffer.append("\nskewSolution.additionalGroups: ");
    buffer.append(skewSolution.additionalGroups);

    buffer.append("\nresidualThreshold: ");
    buffer.append(residualThreshold);

    buffer.append("\nnSurfaceAnalysis: ");
    buffer.append(nSurfaceAnalysis);

    buffer.append("\nminimizationParams: ");
    buffer.append(minimizationParams);

    buffer.append("\nlocalAlignments: ");
    buffer.append(localAlignments);

    buffer.append("\ntiltAxisZShift: ");
    buffer.append(tiltAxisZShift);

    buffer.append("\ntiltAxisXShift: ");
    buffer.append(tiltAxisXShift);

    buffer.append("\nlocalTransformFile: ");
    buffer.append(localTransformFile);

    buffer.append("\nnLocalPatches: ");
    buffer.append(nLocalPatches);

    buffer.append("\nminLocalPatchSize: ");
    buffer.append(minLocalPatchSize);

    buffer.append("\nminLocalFiducials: ");
    buffer.append(minLocalFiducials);

    buffer.append("\nfixLocalFiducialCoodinates: ");
    buffer.append(fixLocalFiducialCoodinates);

    buffer.append("\nlocalOutputSelection: ");
    buffer.append(localOutputSelection);

    buffer.append("\nlocalRotationSolution.type: ");
    buffer.append(localRotationSolution.type);

    buffer.append("\nlocalRotationSolution.params: ");
    buffer.append(localRotationSolution.params);

    buffer.append("\nlocalRotationSolution.additionalGroups: ");
    buffer.append(localRotationSolution.additionalGroups);

    buffer.append("\nlocalTiltSolution.type: ");
    buffer.append(localTiltSolution.type);

    buffer.append("\nlocalTiltSolution.params: ");
    buffer.append(localTiltSolution.params);

    buffer.append("\nlocalTiltSolution.additionalGroups: ");
    buffer.append(localTiltSolution.additionalGroups);

    buffer.append("\nlocalMagnificationReferenceView: ");
    buffer.append(localMagnificationSolution.referenceView);

    buffer.append("\nlocalMagnificationSolution.type: ");
    buffer.append(localMagnificationSolution.type);

    buffer.append("\nlocalMagnificationSolution.params: ");
    buffer.append(localMagnificationSolution.params);

    buffer.append("\nlocalMagnificationSolution.additionalGroups: ");
    buffer.append(localMagnificationSolution.additionalGroups);

    buffer.append("\nlocalDistortionSolutionType: ");
    buffer.append(localDistortionSolutionType);

    buffer.append("\nlocalXstretchSolution.type: ");
    buffer.append(localXstretchSolution.type);

    buffer.append("\nlocalXstretchSolution.params: ");
    buffer.append(localXstretchSolution.params);

    buffer.append("\nlocalXstretchSolution.additionalGroups: ");
    buffer.append(localXstretchSolution.additionalGroups);

    buffer.append("\nlocalSkewSolution.type: ");
    buffer.append(localSkewSolution.type);

    buffer.append("\nlocalSkewSolution.params: ");
    buffer.append(localSkewSolution.params);

    buffer.append("\nlocalSkewSolution.additionalGroups: ");
    buffer.append(localSkewSolution.additionalGroups);

    buffer.append("\n");
    return buffer.toString();
  }

  /**
   * Parse a FortranInputString and StringList group
   */
  private int parseGroup(
    TiltalignSolution solution,
    ComScriptInputArg[] inputArgs,
    int inputLine)
    throws FortranInputSyntaxException {
    inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
    solution.params.validateAndSet(inputArgs[inputLine++].getArgument());

    int nGroups = solution.params.getInt(1);
    if (nGroups > 0) {
      solution.additionalGroups = new StringList(nGroups);
      for (int i = 0; i < nGroups; i++) {
        inputLine = getNextNonBlankArgIndex(inputArgs, inputLine);
        solution.additionalGroups.set(i, inputArgs[inputLine++].getArgument());
      }
    }
    return inputLine;
  }

  /**
   * Update the inputArguments array with new parameters
   * @param inputArgs the array of existing input arguments, some of these
   * will be used for default values that TiltalignParam currently does not
   * modify.
   * @return the new array of ComScriptInputArgs.  Note that this is a newly
   * allocated array since the number of elements may be different than the
   * input parameter.
   */
  private ComScriptInputArg[] putComScriptArguments(ComScriptInputArg[] inputArgs) {
    ArrayList inputArgList = new ArrayList();

    //  Fill in the input argument sequence, the srcListCount variable
    //  acts as an index into the existing input argument array
    int srcListCount = 0;

    inputArgs[srcListCount].setArgument(modelFile);
    inputArgList.add(inputArgs[srcListCount++]);

    //  Sync the existing and new image file and image parameters
    if (inputArgs[srcListCount].getArgument().matches("\\s*")) {
      inputArgs[srcListCount].setArgument("");
      inputArgList.add(inputArgs[srcListCount++]);

      //  Both blank followed by image parameter
      if (imageFile.matches("\\s*")) {
        inputArgs[srcListCount].setArgument(imageParameters);
        inputArgList.add(inputArgs[srcListCount]);
      }
      srcListCount++;
    }
    //  Only one existing ouput argument
    else {
      inputArgs[srcListCount].setArgument(imageFile);
      inputArgList.add(inputArgs[srcListCount++]);
      if (imageFile.matches("\\s*")) {
        ComScriptInputArg newArg = new ComScriptInputArg();
        newArg.setArgument(imageParameters);
      }
    }

    inputArgs[srcListCount].setArgument(imodFiducialPosFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(asciiFiducialPosFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(tiltAngleSolutionFile);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(transformSolutionFile);
    inputArgList.add(inputArgs[srcListCount++]);

    int existingSolutionType =
      Integer.parseInt(inputArgs[srcListCount].getArgument());

    inputArgs[srcListCount].setArgument(solutionType);
    inputArgList.add(inputArgs[srcListCount++]);

    //  Sync the existing and new include points parameters
    int existingIncludePoints =
      Integer.parseInt(inputArgs[srcListCount].getArgument());
    //  Existing input sequence is single line
    if (existingIncludePoints == 0) {
      inputArgs[srcListCount].setArgument(includeExcludeType);
      inputArgList.add(inputArgs[srcListCount++]);

      //  New include points is multiline
      if (includeExcludeType > 0) {
        ComScriptInputArg newArg = new ComScriptInputArg();
        newArg.setArgument(includeExcludeList.toString());
        inputArgList.add(newArg);
      }
    }
    //  Existing multiline input sequence
    else {
      inputArgs[srcListCount].setArgument(includeExcludeType);
      inputArgList.add(inputArgs[srcListCount++]);

      //  New include points is multiline
      if (includeExcludeType > 0) {
        inputArgs[srcListCount].setArgument(includeExcludeList.toString());
        inputArgList.add(inputArgs[srcListCount]);
      }
      srcListCount++;
    }

    inputArgs[srcListCount].setArgument(initialImageRotation);
    inputArgList.add(inputArgs[srcListCount++]);

    inputArgs[srcListCount].setArgument(rotationAngleSolutionType);
    inputArgList.add(inputArgs[srcListCount++]);

    // Increment the source list counter to skip the old view groups, the
    // comments for those entries are most likely not applicable
    int nSrcSets = Integer.parseInt(inputArgs[srcListCount].getArgument());
    inputArgs[srcListCount].setArgument(nAdditionalViewSets);
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

    inputArgs[srcListCount].setArgument(tiltAngleOffset);
    inputArgList.add(inputArgs[srcListCount++]);

    try {
      srcListCount =
        replaceTiltAngleParameters(
          tiltAngleSolution,
          inputArgs,
          srcListCount,
          inputArgList);

      inputArgs[srcListCount].setArgument(magnificationSolution.referenceView);
      inputArgList.add(inputArgs[srcListCount++]);

      //  FIXME: this looses the comments to the magnification selection
      srcListCount = skipExistingSolnParams(inputArgs, srcListCount);
      updateSolution(magnificationSolution, inputArgList);

      srcListCount =
        replaceCompressionParameters(
          compressionSolution,
          inputArgs,
          srcListCount,
          inputArgList);

      srcListCount =
        replaceDistortionParameters(
          distortionSolutionType,
          xstretchSolution,
          skewSolution,
          inputArgs,
          srcListCount,
          inputArgList);

      inputArgs[srcListCount].setArgument(residualThreshold);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(nSurfaceAnalysis);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(minimizationParams);
      inputArgList.add(inputArgs[srcListCount++]);

      //  Axis shift parameters
      if (existingSolutionType > -1) {
        if (solutionType > -1) {
          inputArgs[srcListCount].setArgument(tiltAxisZShift);
          inputArgList.add(inputArgs[srcListCount++]);
          inputArgs[srcListCount].setArgument(tiltAxisXShift);
          inputArgList.add(inputArgs[srcListCount++]);
        }
        else {
          srcListCount = srcListCount + 2;
        }
      }
      else {
        if (solutionType > -1) {
          ComScriptInputArg newArg = new ComScriptInputArg();
          newArg.setArgument(tiltAxisZShift);
          inputArgList.add(newArg);
          newArg = new ComScriptInputArg();
          newArg.setArgument(tiltAxisXShift);
          inputArgList.add(newArg);
        }
      }

      //  Local alignments
      inputArgs[srcListCount].setArgument(localAlignments);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(localTransformFile);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(nLocalPatches);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(minLocalPatchSize);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(minLocalFiducials);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(fixLocalFiducialCoodinates);
      inputArgList.add(inputArgs[srcListCount++]);

      inputArgs[srcListCount].setArgument(localOutputSelection);
      inputArgList.add(inputArgs[srcListCount++]);

      srcListCount = skipExistingSolnParams(inputArgs, srcListCount);
      updateSolution(localRotationSolution, inputArgList);

      srcListCount =
        replaceTiltAngleParameters(
          localTiltSolution,
          inputArgs,
          srcListCount,
          inputArgList);

      srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
      inputArgs[srcListCount].setArgument(
        localMagnificationSolution.referenceView);
      inputArgList.add(inputArgs[srcListCount++]);

      srcListCount = skipExistingSolnParams(inputArgs, srcListCount);
      updateSolution(localMagnificationSolution, inputArgList);

      srcListCount =
        replaceDistortionParameters(
          localDistortionSolutionType,
          localXstretchSolution,
          localSkewSolution,
          inputArgs,
          srcListCount,
          inputArgList);
    }
    catch (Exception except) {
      except.printStackTrace();
      //  TODO this should probably throw an excpetion or set a state so that
      //  some other code above a handles it
      String[] errorMessage = new String[6];
      errorMessage[0] = "TiltalignParam Error";
      errorMessage[1] = "Existing Tiltalign parameter was incorrect";
      errorMessage[2] =
        "The align*.com file appears to have changed inappropriately on disk";
      errorMessage[3] =
        "Input string: " + inputArgs[srcListCount].getArgument();
      errorMessage[4] = "Argument #: " + String.valueOf(srcListCount);
      errorMessage[5] = except.getMessage();
      JOptionPane.showMessageDialog(
        null,
        errorMessage,
        "TiltalignParam Error",
        JOptionPane.ERROR_MESSAGE);

    }

    return (ComScriptInputArg[]) inputArgList.toArray(
      new ComScriptInputArg[inputArgList.size()]);
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the tiltalign command
   */
  private ComScriptInputArg[] getComScriptArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals("tiltalign")) {
      throw (new BadComScriptException("Not a tiltalign command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length < 17) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to tiltalign command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected at least 24."));
    }

    return inputArgs;
  }

  /**
   * Generic solution parameter replacement method
   */
  private int skipExistingSolnParams(
    ComScriptInputArg[] inputArgs,
    int srcListCount)
    throws InvalidParameterException, FortranInputSyntaxException {

    //  Solution parameters, need to figure out how many
    //  lines are in the existing input subsequence
    srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
    int existingSolutionType =
      Integer.parseInt(inputArgs[srcListCount].getArgument());

    if (existingSolutionType == 2) {
      String message = "Don't know how to handle arbitrary views yet!!!";
      throw new InvalidParameterException(message);
    }

    //  Skip the type argument
    srcListCount++;

    //  Skip the corrent number of existing arguments
    if (existingSolutionType > 2) {
      FortranInputString existingParams = new FortranInputString(2);
      existingParams.validateAndSet(inputArgs[srcListCount++].getArgument());
      int nGroups = existingParams.getInt(1);
      srcListCount = srcListCount + nGroups;
    }
    return srcListCount;
  }

  private void updateSolution(
    TiltalignSolution solution,
    ArrayList inputArgList) {

    //  Add the new solution parameters to the list
    ComScriptInputArg newArg = new ComScriptInputArg();
    newArg.setArgument(solution.type);
    inputArgList.add(newArg);

    if (solution.type > 2) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(solution.params);
      inputArgList.add(newArg);
      for (int i = 0; i < solution.params.getInt(1); i++) {
        newArg = new ComScriptInputArg();
        newArg.setArgument(solution.additionalGroups.get(i));
        inputArgList.add(newArg);
      }
    }
  }

  /**
   * Replace the tilt angle parameters
   */
  private int replaceTiltAngleParameters(
    TiltalignSolution solution,
    ComScriptInputArg[] inputArgs,
    int srcListCount,
    ArrayList inputArgList)
    throws InvalidParameterException, FortranInputSyntaxException {

    //  Skip over the correct number of existing tilt angle solution type
    //  and parameters, this requires parsing the existing tilt angle solution
    //  parameter sequence
    srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
    int existingTiltSoltnType =
      Integer.parseInt(inputArgs[srcListCount].getArgument());
    if (!(existingTiltSoltnType == 0
      || existingTiltSoltnType == 2
      || existingTiltSoltnType == 5)) {
      String message = "Don't know how to handle arbitrary tilt views yet!!!";
      throw new InvalidParameterException(message);
    }
    srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
    inputArgs[srcListCount].setArgument(solution.type);
    inputArgList.add(inputArgs[srcListCount++]);

    //  Skip the corrent number of existing arguments
    if (existingTiltSoltnType == 5) {
      FortranInputString existingParams = new FortranInputString(2);
      existingParams.validateAndSet(inputArgs[srcListCount++].getArgument());
      int nGroups = existingParams.getInt(1);
      srcListCount = srcListCount + nGroups;
    }

    //  Add the new tilt angle parameters to the list
    if (solution.type == 5) {
      ComScriptInputArg newArg = new ComScriptInputArg();
      newArg.setArgument(solution.params);
      inputArgList.add(newArg);
      for (int i = 0; i < solution.params.getInt(1); i++) {
        newArg = new ComScriptInputArg();
        newArg.setArgument(solution.additionalGroups.get(i));
        inputArgList.add(newArg);
      }
    }
    return srcListCount;
  }

  private int replaceCompressionParameters(
    TiltalignSolution solution,
    ComScriptInputArg[] inputArgs,
    int srcListCount,
    ArrayList inputArgList)
    throws InvalidParameterException, FortranInputSyntaxException {

    //  Solution parameters, need to figure out how many
    //  lines are in the existing input subsequence
    srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
    int existingCompressionReferenceView =
      Integer.parseInt(inputArgs[srcListCount].getArgument());

    inputArgs[srcListCount].setArgument(solution.referenceView);
    inputArgList.add(inputArgs[srcListCount++]);

    if (existingCompressionReferenceView > 0) {
      int existingSolutionType =
        Integer.parseInt(inputArgs[srcListCount++].getArgument());
      if (existingSolutionType == 2) {
        String message = "Don't know how to handle arbitrary views yet!!!";
        throw new InvalidParameterException(message);
      }

      //  Skip the corrent number of existing arguments
      if (existingSolutionType > 2) {
        FortranInputString existingParams = new FortranInputString(2);
        existingParams.validateAndSet(inputArgs[srcListCount++].getArgument());
        int nGroups = existingParams.getInt(1);
        srcListCount = srcListCount + nGroups;
      }
    }

    //  Check the current reference view to see if we should include any more
    //  parameters
    if (solution.referenceView.getInt(0) > 0) {
      ComScriptInputArg newArg = new ComScriptInputArg();
      newArg.setArgument(solution.type);
      inputArgList.add(newArg);

      //  Add the new solution parameters to the list
      if (solution.type > 2) {
        newArg = new ComScriptInputArg();
        newArg.setArgument(solution.params);
        inputArgList.add(newArg);
        for (int i = 0; i < solution.params.getInt(1); i++) {
          newArg = new ComScriptInputArg();
          newArg.setArgument(solution.additionalGroups.get(i));
          inputArgList.add(newArg);
        }
      }
    }
    return srcListCount;
  }

  /**
   * Replace the distortion parameters
   */
  private int replaceDistortionParameters(
    int solutionType,
    TiltalignSolution xstretch,
    TiltalignSolution skew,
    ComScriptInputArg[] inputArgs,
    int srcListCount,
    ArrayList inputArgList)
    throws InvalidParameterException, FortranInputSyntaxException {

    srcListCount = getNextNonBlankArgIndex(inputArgs, srcListCount);
    int existingDistortionType =
      Integer.parseInt(inputArgs[srcListCount].getArgument());

    inputArgs[srcListCount].setArgument(solutionType);
    inputArgList.add(inputArgs[srcListCount++]);

    // Skip the existing X stretch solution parameters
    if (existingDistortionType > 0) {
      srcListCount = skipExistingSolnParams(inputArgs, srcListCount);
    }

    // Skip the existing skew solution distortion parameters
    if (existingDistortionType == 2) {
      srcListCount = skipExistingSolnParams(inputArgs, srcListCount);
    }

    //  Update the new xstretch solution parameters
    if (solutionType > 0) {
      updateSolution(xstretch, inputArgList);
    }
    //  Update the new skew solution parameters
    if (solutionType == 2) {
      updateSolution(skew, inputArgList);
    }
    return srcListCount;
  }

  private int getNextNonBlankArgIndex(
    ComScriptInputArg[] inputArgs,
    int inputLine) {

    while (inputArgs[inputLine].getArgument().matches("^\\s*$"))
      inputLine++;

    return inputLine;
  }
}
