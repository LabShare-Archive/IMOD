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
  public void initialize(ComScriptCommand scriptCommand)
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
      String[] tokens = inputArgs[i].getArgument().split("\\s+", 0);

      if (tokens[0].equals("DONE")) {
        foundDone = true;
      }

      if (tokens[0].equals("ANGLES")) {
        for (int j = 1; j < tokens.length; j++) {
          angles.add(tokens[j]);
        }
        useAngles = true;
      }

      if (tokens[0].equals("COMPFRACTION")) {
        compressionFraction = Double.parseDouble(tokens[1]);
        useCompressionFraction = true;
      }

      if (tokens[0].equals("COMPRESS")) {
        compression = Double.parseDouble(tokens[1]);
        useCompression = true;
      }

      if (tokens[0].equals("COSINTERP")) {
        cosInterpOrder = Integer.parseInt(tokens[1]);
        cosInterpFactor = Double.parseDouble(tokens[2]);
        useCosInterp = true;
      }

      if (tokens[0].equals("DENSWEIGHT")) {
        densityWeightNadjacent = tokens[1];
        for (int j = 2; j < tokens.length; j++) {
          densityWeights.add(tokens[j]);
        }
        useDensityWeight = true;
      }

      if (tokens[0].equals("EXCLUDE")) {
        for (int j = 1; j < tokens.length; j++) {
          exclude.add(tokens[j]);
        }
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
        fullImageX = Integer.parseInt(tokens[1]);
        fullImageY = Integer.parseInt(tokens[2]);
        useFullImage = true;
      }

      if (tokens[0].equals("LOCALFILE")) {
        localAlignFile = tokens[1];
        useLocalAlignFile = true;
      }

      if (tokens[0].equals("LOG")) {
        logShift = Double.parseDouble(tokens[1]);
        useLogShift = true;
      }

      if (tokens[0].equals("MODE")) {
        mode = Integer.parseInt(tokens[1]);
        useMode = true;
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
        radialBandwidth = Double.parseDouble(tokens[1]);
        radialFalloff = Double.parseDouble(tokens[2]);
        useRadialWeightingFunction = true;
      }

      if (tokens[0].equals("SCALE")) {
        scaleFLevel = Double.parseDouble(tokens[1]);
        scaleCoeff = Double.parseDouble(tokens[2]);
        useScale = true;
      }

      if (tokens[0].equals("SUBSETSTART")) {
        idxXSubsetStart = Integer.parseInt(tokens[1]);
        idxYSubsetStart = Integer.parseInt(tokens[2]);
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

      if (tokens[0].equals("XAXISTILT")) {
        xAxisTilt = Double.parseDouble(tokens[1]);
        useXAxisTilt = true;
      }
    }

  }

  /**
   * Update the script command with the current valus of this TiltParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScript(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    // Create a new command line argument array

    ArrayList cmdLineArgs = new ArrayList(32);

    ComScriptInputArg newArg = new ComScriptInputArg();

    newArg.setArgument(inputFile);
    cmdLineArgs.add(newArg);

    newArg = new ComScriptInputArg();
    newArg.setArgument(outputFile);
    cmdLineArgs.add(newArg);

    if (useExcludeList) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST " + excludeList.toString());
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

    if (useLocalAlignFile) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOCALFILE " + localAlignFile);
      cmdLineArgs.add(newArg);
    }

    if (useLogShift) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOG " + String.valueOf(logShift));
      cmdLineArgs.add(newArg);
    }

    if (useMode) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MODE " + String.valueOf(mode));
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

    if (useScale) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
        "SCALE "
          + String.valueOf(scaleFLevel)
          + " "
          + String.valueOf(scaleCoeff));
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

    if (useXAxisTilt) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XAXISTILT " + String.valueOf(xAxisTilt));
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
    logShift = shift;
    useLogShift = true;
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

}
