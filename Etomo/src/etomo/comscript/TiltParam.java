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
 * <p> Revision 1.2  2002/10/17 16:19:53  rickg
 * <p> Implemented useExcludeList flag
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltParam extends ConstTilt {
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
      String[] tokens = inputArgs[i].getArgument().split("\\s+", 2);

      if (tokens[0].equals("DONE")) {
        foundDone = true;
      }

      if (tokens[0].equals("EXCLUDELIST")) {
        excludeList.parseString(tokens[1]);
        useExcludeList = true;
      }

      if (tokens[0].equals("FULLIMAGE")) {
        fullImage = tokens[1];
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
        radialWeightingFunction = tokens[1];
        useRadialWeightingFunction = true;
      }

      if (tokens[0].equals("SCALE")) {
        scale = tokens[1];
        useScale = true;
      }

      if (tokens[0].equals("SUBSETSTART")) {
        subsetStart = tokens[1];
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
      newArg.setArgument("FULLIMAGE " + fullImage);
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
      newArg.setArgument("RADIAL " + radialWeightingFunction);
      cmdLineArgs.add(newArg);
    }

    if (useScale) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("SCALE " + scale);
      cmdLineArgs.add(newArg);
    }

    if (useSubsetStart) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("SUBSETSTART " + subsetStart);
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

  //  FIXME: needs documentation for use
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

  public void setRadialWeightingFunction(String params) {
    radialWeightingFunction = params;
    useRadialWeightingFunction = true;
  }

  public void setSubsetStart(String params) {
    subsetStart = params;
    useSubsetStart = true;
  }

  public void setScale(String params) {
    scale = params;
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
