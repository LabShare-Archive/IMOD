package etomo.comscript;

import java.util.ArrayList;

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
 * <p> Revision 2.9  2003/10/22 21:31:20  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.8  2003/08/21 22:17:48  rickg
 * <p> Added density scaling setters
 * <p>
 * <p> Revision 2.7  2003/07/25 22:52:37  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.6  2003/06/25 22:15:32  rickg
 * <p> Manage all tilt parameters
 * <p>
 * <p> Revision 2.5  2003/06/23 23:27:39  rickg
 * <p> Stricter typing of parameters
 * <p>
 * <p> Revision 2.4  2003/06/10 22:59:59  rickg
 * <p> Changes to match full implementation in progress
 * <p>
 * <p> Revision 2.3  2003/05/23 21:26:47  rickg
 * <p> Implemented radial filter parameters
 * <p>
 * <p> Revision 2.2  2003/03/20 17:24:22  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3  2003/01/03 20:02:39  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.2  2002/10/17 16:19:53  rickg
 * <p> Implemented useExcludeList flag
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltParam extends ConstTiltParam implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    //  Get the input and output file names from the input arguments
    int nInputArgs = inputArgs.length;
    int argIndex = 0;
    inputFile = inputArgs[argIndex++].getArgument();
    outputFile = inputArgs[argIndex++].getArgument();
    boolean foundDone = false;

    for (int i = argIndex; i < nInputArgs; i++) {
      // split the line into the parameter name and the rest of the line
      String[] tokens = inputArgs[i].getArgument().split("\\s+", 2);

      if (tokens[0].equals("ANGLES")) {
        angles = tokens[1];
        useAngles = true;
      }

      if (tokens[0].equals("COMPFRACTION")) {
        compressionFraction = Double.parseDouble(tokens[1]);
        useCompressionFraction = true;
      }

      if (tokens[0].equals("COMPRESS")) {
        compression = tokens[1];
        useCompression = true;
      }

      if (tokens[0].equals("COSINTERP")) {
        String[] params = tokens[1].split("\\s+", 2);
        cosInterpOrder = Integer.parseInt(params[0]);
        cosInterpFactor = Double.parseDouble(params[1]);
        useCosInterp = true;
      }

      if (tokens[0].equals("DENSWEIGHT")) {
        densityWeightParams = tokens[1];
        useDensityWeight = true;
      }

      if (tokens[0].equals("DONE")) {
        foundDone = true;
      }

      if (tokens[0].equals("EXCLUDE")) {
        exclude = tokens[1];
        useExclude = true;
      }

      if (tokens[0].equals("EXCLUDELIST")) {
        excludeList.parseString(tokens[1]);
        useExcludeList = true;
      }

      if (tokens[0].equals("FBPINTERP")) {
        fastBackProjInterpOrder = Integer.parseInt(tokens[1]);
        useFastBackProjInterpOrder = true;
      }

      if (tokens[0].equals("FULLIMAGE")) {
        String[] params = tokens[1].split("\\s+", 2);
        fullImageX = Integer.parseInt(params[0]);
        fullImageY = Integer.parseInt(params[1]);
        useFullImage = true;
      }

      if (tokens[0].equals("INCLUDE")) {
        include = tokens[1];
        useInclude = true;
      }

      if (tokens[0].equals("LOCALFILE")) {
        localAlignFile = tokens[1];
        useLocalAlignFile = true;
      }

      if (tokens[0].equals("LOG")) {
        logOffset = Double.parseDouble(tokens[1]);
        useLogOffset = true;
      }

      if (tokens[0].equals("MASK")) {
        mask = Double.parseDouble(tokens[1]);
        useMask = true;
      }

      if (tokens[0].equals("MODE")) {
        mode = Integer.parseInt(tokens[1]);
        useMode = true;
      }

      if (tokens[0].equals("OFFSET")) {
        String[] params = tokens[1].split("\\s+", 2);
        tiltAngleOffset = Double.parseDouble(params[0]);
        useTiltAngleOffset = true;
        if (params.length > 1) {
          tiltAxisOffset = Double.parseDouble(params[1]);
          useTiltAxisOffset = true;
        }
        else {
          useTiltAxisOffset = false;
        }
      }

      if (tokens[0].equals("PARALLEL")) {
        perpendicular = false;
        usePerpendicular = false;
        parallel = true;
        useParallel = true;
      }

      if (tokens[0].equals("PERPENDICULAR")) {
        perpendicular = true;
        usePerpendicular = true;
        parallel = false;
        useParallel = false;
      }

      if (tokens[0].equals("RADIAL")) {
        String[] params = tokens[1].split("\\s+", 2);
        radialBandwidth = Double.parseDouble(params[0]);
        radialFalloff = Double.parseDouble(params[1]);
        useRadialWeightingFunction = true;
      }

      if (tokens[0].equals("REPLICATE")) {
        String[] params = tokens[1].split("\\s+", 2);
        nReplicate = Integer.parseInt(params[0]);
        incReplicate = Integer.parseInt(tokens[1]);
        useReplicate = true;
      }

      if (tokens[0].equals("SCALE")) {
        String[] params = tokens[1].split("\\s+", 2);
        scaleFLevel = Double.parseDouble(params[0]);
        scaleCoeff = Double.parseDouble(params[1]);
        useScale = true;
      }

      if (tokens[0].equals("SHIFT")) {
        String[] params = tokens[1].split("\\s+", 2);
        xOffset = Double.parseDouble(params[0]);
        useXOffset = true;
        if (params.length > 1) {
          zOffset = Double.parseDouble(params[1]);
          useZOffset = true;
        }
      }

      if (tokens[0].equals("SLICE")) {
        String[] params = tokens[1].split("\\s+", 3);
        idxSliceStart = Integer.parseInt(params[0]);
        idxSliceStop = Integer.parseInt(params[1]);
        useSlice = true;
        if (params.length > 2) {
          idxSliceIncr = Integer.parseInt(params[2]);
          useSliceIncr = true;
        }
        else {
          useSliceIncr = false;
        }
      }

      if (tokens[0].equals("SUBSETSTART")) {
        String[] params = tokens[1].split("\\s+", 2);
        idxXSubsetStart = Integer.parseInt(params[0]);
        idxYSubsetStart = Integer.parseInt(params[1]);
        useSubsetStart = true;
      }

      if (tokens[0].equals("THICKNESS")) {
        thickness = Integer.parseInt(tokens[1]);
        useThickness = true;
      }

      if (tokens[0].equals("TILTFILE")) {
        tiltFile = tokens[1];
        useTiltfile = true;
      }

      if (tokens[0].equals("TITLE")) {
        title = tokens[1];
        useTitle = true;
      }

      if (tokens[0].equals("WIDTH")) {
        width = Integer.parseInt(tokens[1]);
        useWidth = true;
      }

      if (tokens[0].equals("XAXISTILT")) {
        xAxisTilt = Double.parseDouble(tokens[1]);
        useXAxisTilt = true;
      }

      if (tokens[0].equals("XTILTFILE")) {
        xTiltFile = tokens[1];
        useXTiltFile = true;
      }

      if (tokens[0].equals("XTILTINTERP")) {
        xTiltInterp = Integer.parseInt(tokens[1]);
        useXTiltInterp = true;
      }

    }

  }

  /**
   * Update the script command with the current valus of this TiltParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    // Create a new command line argument array

    ArrayList cmdLineArgs = new ArrayList(32);

    ComScriptInputArg newArg = new ComScriptInputArg();

    newArg.setArgument(inputFile);
    cmdLineArgs.add(newArg);

    newArg = new ComScriptInputArg();
    newArg.setArgument(outputFile);
    cmdLineArgs.add(newArg);

    if (useAngles) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("ANGLES " + angles);
      cmdLineArgs.add(newArg);
    }

    if (useCompressionFraction) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPFRACTION " + String.valueOf(compressionFraction));
      cmdLineArgs.add(newArg);
    }

    if (useCompression) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPRESS " + compression);
      cmdLineArgs.add(newArg);
    }

    if (useCosInterp) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "COSINTERP "
          + String.valueOf(cosInterpOrder)
          + " "
          + String.valueOf(cosInterpFactor));
      cmdLineArgs.add(newArg);
    }

    if (useDensityWeight) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("DENSWEIGHT " + densityWeightParams);
      cmdLineArgs.add(newArg);
    }

    if (useExclude) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDE " + exclude);
      cmdLineArgs.add(newArg);
    }

    if (useExcludeList) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST " + excludeList.toString());
      cmdLineArgs.add(newArg);
    }

    if (useFastBackProjInterpOrder) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "FBPINTERP " + String.valueOf(fastBackProjInterpOrder));
      cmdLineArgs.add(newArg);
    }

    if (useFullImage) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "FULLIMAGE "
          + String.valueOf(fullImageX)
          + " "
          + String.valueOf(fullImageY));
      cmdLineArgs.add(newArg);
    }

    if (useInclude) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("INCLUDE " + include);
      cmdLineArgs.add(newArg);
    }

    if (useLocalAlignFile) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOCALFILE " + localAlignFile);
      cmdLineArgs.add(newArg);
    }

    if (useLogOffset) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOG " + String.valueOf(logOffset));
      cmdLineArgs.add(newArg);
    }

    if (useMode) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MODE " + String.valueOf(mode));
      cmdLineArgs.add(newArg);
    }

    if (useMask) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MASK " + String.valueOf(mask));
      cmdLineArgs.add(newArg);
    }

    if (useTiltAngleOffset) {
      String arg = "OFFSET " + String.valueOf(tiltAngleOffset);
      if (useTiltAxisOffset) {
        arg += " " + String.valueOf(tiltAxisOffset);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }

    if (useParallel) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PARALLEL");
      cmdLineArgs.add(newArg);
    }

    if (usePerpendicular) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PERPENDICULAR");
      cmdLineArgs.add(newArg);
    }

    if (useRadialWeightingFunction) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "RADIAL "
          + String.valueOf(radialBandwidth)
          + " "
          + String.valueOf(radialFalloff));
      cmdLineArgs.add(newArg);
    }

    if (useReplicate) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "REPLICATE "
          + String.valueOf(nReplicate)
          + " "
          + String.valueOf(incReplicate));
      cmdLineArgs.add(newArg);
    }

    if (useScale) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "SCALE "
          + String.valueOf(scaleFLevel)
          + " "
          + String.valueOf(scaleCoeff));
      cmdLineArgs.add(newArg);
    }

    if (useXOffset) {
      String arg = "SHIFT " + String.valueOf(xOffset);
      if (useZOffset) {
        arg = arg + " " + String.valueOf(zOffset);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }

    if (useSlice) {
      String arg =
        "SLICE "
          + String.valueOf(idxSliceStart)
          + " "
          + String.valueOf(idxSliceStop);
      if (useSliceIncr) {
        arg += " " + String.valueOf(idxSliceIncr);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }

    if (useSubsetStart) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "SUBSETSTART "
          + String.valueOf(idxXSubsetStart)
          + " "
          + String.valueOf(idxYSubsetStart));
      cmdLineArgs.add(newArg);
    }

    if (useThickness) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("THICKNESS " + String.valueOf(thickness));
      cmdLineArgs.add(newArg);
    }

    if (useTiltfile) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("TILTFILE " + tiltFile);
      cmdLineArgs.add(newArg);
    }

    if (useWidth) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("WIDTH " + String.valueOf(width));
      cmdLineArgs.add(newArg);
    }

    if (useXAxisTilt) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XAXISTILT " + String.valueOf(xAxisTilt));
      cmdLineArgs.add(newArg);
    }

    if (useXTiltFile) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XTILTFILE " + xTiltFile);
      cmdLineArgs.add(newArg);
    }

    if (useXTiltInterp) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XTILTINTERP " + String.valueOf(xTiltInterp));
      cmdLineArgs.add(newArg);
    }

    newArg = new ComScriptInputArg();
    newArg.setArgument("DONE");
    cmdLineArgs.add(newArg);

    int nArgs = cmdLineArgs.size();
    scriptCommand.setInputArguments(
      (ComScriptInputArg[]) cmdLineArgs.toArray(new ComScriptInputArg[nArgs]));
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the tilt command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals("tilt")) {
      throw (new BadComScriptException("Not a tiltalign command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length < 3) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to tiltalign command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected at least 3."));
    }

    return inputArgs;
  }

  public void setInputFile(String file) {
    inputFile = file;
  }

  public void setOutputFile(String file) {
    outputFile = file;
  }

  public void setLogShift(double shift) {
    logOffset = shift;
    useLogOffset = true;
  }

  //  NOTE needs documentation for use
  public void setLocalAlignFile(String filename) {
    localAlignFile = filename;
    if (filename != "") {
      useLocalAlignFile = true;
    }
    else {
      useLocalAlignFile = false;
    }
  }

  public void setMode(int newMode) {
    mode = newMode;
    useMode = true;
  }

  public void setParallel() {
    parallel = true;
    useParallel = true;
    perpendicular = false;
    usePerpendicular = false;
  }

  public void setPerpendicular() {
    parallel = false;
    useParallel = false;
    perpendicular = true;
    usePerpendicular = true;
  }

  public void setRadialBandwidth(double value) {
    radialBandwidth = value;
    useRadialWeightingFunction = true;
  }

  /**
   * @param string
   */
  public void setRadialFalloff(double value) {
    radialFalloff = value;
    useRadialWeightingFunction = true;
  }

  public void setSubsetStart(int xStart, int yStart) {
    idxXSubsetStart = xStart;
    idxYSubsetStart = yStart;
    useSubsetStart = true;
  }

  public void setScale(double fLevel, double coef) {
    scaleCoeff = coef;
    scaleFLevel = fLevel;
    useScale = true;
  }

  public void setThickness(int newThickness) {
    thickness = newThickness;
    useThickness = true;
  }

  public void setTiltFile(String filename) {
    tiltFile = filename;
    useTiltfile = true;
  }

  public void setXAxisTilt(double angle) {
    xAxisTilt = angle;
    useXAxisTilt = true;
  }

  /**
   * @param i
   */
  public void setIdxSliceIncr(int i) {
    idxSliceIncr = i;
    useSliceIncr(true);
  }

  /**
   * @param i
   */
  public void setIdxSliceStart(int i) {
    idxSliceStart = i;
    useSlice(true);
  }

  /**
   * @param i
   */
  public void setIdxSliceStop(int i) {
    idxSliceStop = i;
    useSlice(true);
  }

  /**
   * Sets the excludeList.
   * @param excludeList The excludeList to set
   */
  public void setExcludeList(String list) {
    excludeList.parseString(list);
    if (excludeList.getNElements() > 0) {
      useExcludeList = true;
    }
    else {
      useExcludeList = false;
    }
  }

  /**
   * @param i
   */
  public void setWidth(int i) {
    width = i;
    useWidth = true;
  }

  /**
   * @param d
   */
  public void setXOffset(double d) {
    xOffset = d;
    useXOffset = true;
  }

  /**
   * @param d
   */
  public void setZOffset(double d) {
    zOffset = d;
    useZOffset = true;
  }

  /**
   * @param b
   */
  public void useZOffset(boolean b) {
    useZOffset = b;
  }

  /**
   * @param d
   */
  public void setTiltAngleOffset(double d) {
    tiltAngleOffset = d;
    useTiltAngleOffset = true;
  }

  /**
   * @param d
   */
  public void setTiltAxisOffset(double d) {
    tiltAxisOffset = d;
    useTiltAngleOffset = true;
  }
  /**
   * @param b
   */
  public void useAngles(boolean b) {
    useAngles = b;
  }

  /**
   * @param b
   */
  public void useCompression(boolean b) {
    useCompression = b;
  }

  /**
   * @param b
   */
  public void useCompressionFraction(boolean b) {
    useCompressionFraction = b;
  }

  /**
   * @param b
   */
  public void useCosInterp(boolean b) {
    useCosInterp = b;
  }

  /**
   * @param b
   */
  public void useDensityWeight(boolean b) {
    useDensityWeight = b;
  }

  /**
   * @param b
   */
  public void useExclude(boolean b) {
    useExclude = b;
  }

  /**
   * @param b
   */
  public void useExcludeList(boolean b) {
    useExcludeList = b;
  }

  /**
   * @param b
   */
  public void useFastBackProjInterpOrder(boolean b) {
    useFastBackProjInterpOrder = b;
  }

  /**
   * @param b
   */
  public void useFullImage(boolean b) {
    useFullImage = b;
  }

  /**
   * @param b
   */
  public void useInclude(boolean b) {
    useInclude = b;
  }

  /**
   * @param b
   */
  public void useLocalAlignFile(boolean b) {
    useLocalAlignFile = b;
  }

  /**
   * @param b
   */
  public void useLocalScale(boolean b) {
    useLocalScale = b;
  }

  /**
   * @param b
   */
  public void useLogOffset(boolean b) {
    useLogOffset = b;
  }

  /**
   * @param b
   */
  public void useMask(boolean b) {
    useMask = b;
  }

  /**
   * @param b
   */
  public void useMode(boolean b) {
    useMode = b;
  }

  /**
   * @param b
   */
  public void useParallel(boolean b) {
    useParallel = b;
  }

  /**
   * @param b
   */
  public void usePerpendicular(boolean b) {
    usePerpendicular = b;
  }

  /**
   * @param b
   */
  public void useRadialWeightingFunction(boolean b) {
    useRadialWeightingFunction = b;
  }

  /**
   * @param b
   */
  public void useReplicate(boolean b) {
    useReplicate = b;
  }

  /**
   * @param b
   */
  public void useScale(boolean b) {
    useScale = b;
  }

  /**
   * @param b
   */
  public void useSlice(boolean b) {
    useSlice = b;
  }

  /**
   * @param b
   */
  public void useSliceIncr(boolean b) {
    useSliceIncr = b;
  }
  /**
   * @param b
   */
  public void useSubsetStart(boolean b) {
    useSubsetStart = b;
  }

  /**
   * @param b
   */
  public void useThickness(boolean b) {
    useThickness = b;
  }

  /**
   * @param b
   */
  public void useTiltfile(boolean b) {
    useTiltfile = b;
  }

  /**
   * @param b
   */
  public void useTitle(boolean b) {
    useTitle = b;
  }

  /**
   * @param b
   */
  public void useWidth(boolean b) {
    useWidth = b;
  }

  /**
   * @param b
   */
  public void useXAxisTilt(boolean b) {
    useXAxisTilt = b;
  }

  /**
   * @param b
   */
  public void useXOffset(boolean b) {
    useXOffset = b;
  }

  /**
   * @param b
   */
  public void useXTiltFile(boolean b) {
    useXTiltFile = b;
  }

  /**
   * @param b
   */
  public void useXTiltInterp(boolean b) {
    useXTiltInterp = b;
  }

  /**
   * @param b
   */
  public void useAngleOffsets(boolean b) {
    useTiltAngleOffset = b;
  }

  /**
   * @param scaleCoeff
   */
  public void setScaleCoeff(double scaleCoeff) {
    this.scaleCoeff = scaleCoeff;
  }

  /**
   * @param scaleFLevel
   */
  public void setScaleFLevel(double scaleFLevel) {
    this.scaleFLevel = scaleFLevel;
  }

}
