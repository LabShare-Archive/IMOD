/**
 * <p>Description: Tilt command model.</p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.8  2005/06/10 22:55:19  sueh
 * <p> bug# 583, bug# 682  Upgraded tilt.com to have all unbinned parameters
 * <p> and a binning value.  No longer managing full image size in tilt.com,
 * <p> except to upgrade the file.  Added function:  updateOldVersion.
 * <p> Removed functions:  setFullImageX, setFullImageY.
 * <p>
 * <p> Revision 3.7  2005/04/25 20:41:18  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.6  2005/03/29 19:54:19  sueh
 * <p> bug# 623 When montaging, setting full image size when updating tilt.com
 * <p> from the .ali file.  When the .ali file is not available set the full image size
 * <p> from running goodframe on the X and Y sizes in the .st file.
 * <p>
 * <p> Revision 3.5  2005/01/12 18:34:07  sueh
 * <p> bug# 505 Added excludeList2.
 * <p>
 * <p> Revision 3.4  2005/01/08 01:46:13  sueh
 * <p> bug# 578 Added dataset name and axis id to constructor.  Read and
 * <p> update ZFACTORFILE in comscript.
 * <p>
 * <p> Revision 3.3  2004/07/20 23:06:28  sueh
 * <p> bug# 502 adding fiducialess, which is not stored in tilt.  It
 * <p> inactivates the local file parameter
 * <p>
 * <p> Revision 3.2  2004/04/12 16:51:07  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.1  2004/03/24 02:55:44  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
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
package etomo.comscript;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.Goodframe;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Montagesize;

public class TiltParam extends ConstTiltParam implements CommandParam {
  public static final String rcsid = 
  "$Id$";

  public TiltParam(String datasetName, AxisID axisID) {
    super(datasetName, axisID);
  }
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
      if (imageBinned.isNamed(tokens[0])) {
        imageBinned.set(tokens[1]);
      }
      if (tokens[0].equals("ANGLES")) {
        angles = tokens[1];
      }
      if (tokens[0].equals("COMPFRACTION")) {
        compressionFraction = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("COMPRESS")) {
        compression = tokens[1];
      }
      if (tokens[0].equals("COSINTERP")) {
        String[] params = tokens[1].split("\\s+", 2);
        cosInterpOrder = Integer.parseInt(params[0]);
        cosInterpFactor = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals("DENSWEIGHT")) {
        densityWeightParams = tokens[1];
      }
      if (tokens[0].equals("DONE")) {
        foundDone = true;
      }
      if (tokens[0].equals("EXCLUDE")) {
        exclude = tokens[1];
      }
      if (tokens[0].equals("EXCLUDELIST")) {
        excludeList.parseString(tokens[1]);
      }
      if (tokens[0].equals("EXCLUDELIST2")) {
        excludeList2.parseString(tokens[1]);
      }
      if (tokens[0].equals("FBPINTERP")) {
        fastBackProjInterpOrder = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("FULLIMAGE")) {
        String[] params = tokens[1].split("\\s+", 2);
        fullImageX = Integer.parseInt(params[0]);
        fullImageY = Integer.parseInt(params[1]);
      }
      if (tokens[0].equals("INCLUDE")) {
        include = tokens[1];
      }
      if (tokens[0].equals("LOCALFILE")) {
        localAlignFile = tokens[1];
      }
      if (tokens[0].equals("LOG")) {
        logOffset = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("MASK")) {
        mask = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("MODE")) {
        mode = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("OFFSET")) {
        String[] params = tokens[1].split("\\s+", 2);
        tiltAngleOffset = Float.parseFloat(params[0]);
        if (params.length > 1) {
          tiltAxisOffset = Float.parseFloat(params[1]);
        }
      }
      if (tokens[0].equals("PARALLEL")) {
        perpendicular = false;
        parallel = true;
      }
      if (tokens[0].equals("PERPENDICULAR")) {
        perpendicular = true;
        parallel = false;
      }
      if (tokens[0].equals("RADIAL")) {
        String[] params = tokens[1].split("\\s+", 2);
        radialBandwidth = Float.parseFloat(params[0]);
        radialFalloff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals("REPLICATE")) {
        String[] params = tokens[1].split("\\s+", 2);
        nReplicate = Integer.parseInt(params[0]);
        incReplicate = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("SCALE")) {
        String[] params = tokens[1].split("\\s+", 2);
        scaleFLevel = Float.parseFloat(params[0]);
        scaleCoeff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals("SHIFT")) {
        String[] params = tokens[1].split("\\s+", 2);
        xOffset = Float.parseFloat(params[0]);
        if (params.length > 1) {
          zOffset = Float.parseFloat(params[1]);
        }
      }
      if (tokens[0].equals("SLICE")) {
        String[] params = tokens[1].split("\\s+", 3);
        idxSliceStart = Integer.parseInt(params[0]);
        idxSliceStop = Integer.parseInt(params[1]);
        if (params.length > 2) {
          incrSlice = Integer.parseInt(params[2]);
        }
      }
      if (tokens[0].equals("SUBSETSTART")) {
        String[] params = tokens[1].split("\\s+", 2);
        idxXSubsetStart = Integer.parseInt(params[0]);
        idxYSubsetStart = Integer.parseInt(params[1]);
      }
      if (tokens[0].equals("THICKNESS")) {
        thickness = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("TILTFILE")) {
        tiltFile = tokens[1];
      }
      if (tokens[0].equals("TITLE")) {
        title = tokens[1];
      }
      if (tokens[0].equals("WIDTH")) {
        width = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("XAXISTILT")) {
        xAxisTilt = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("XTILTFILE")) {
        xTiltFile = tokens[1];
      }
      if (tokens[0].equals("XTILTINTERP")) {
        xTiltInterp = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("ZFACTORFILE")) {
        useZFactors = true;
        zFactorFileName = tokens[1];
      }
    }
    loadedFromFile = true;
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
    if (imageBinned.isUseInScript()) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(imageBinned.getName() + " " + imageBinned.getInteger());
      cmdLineArgs.add(newArg);
    }
    if (!angles.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("ANGLES " + angles);
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(compressionFraction)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPFRACTION " + String.valueOf(compressionFraction));
      cmdLineArgs.add(newArg);
    }
    if (!compression.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPRESS " + compression);
      cmdLineArgs.add(newArg);
    }
    if (cosInterpOrder > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "COSINTERP "
      + String.valueOf(cosInterpOrder)
      + " "
      + String.valueOf(cosInterpFactor));
      cmdLineArgs.add(newArg);
    }
    if (!densityWeightParams.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("DENSWEIGHT " + densityWeightParams);
      cmdLineArgs.add(newArg);
    }
    if (!exclude.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDE " + exclude);
      cmdLineArgs.add(newArg);
    }
    if (excludeList.getNElements() > 0) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST " + excludeList.toString());
      cmdLineArgs.add(newArg);
    }
    if (excludeList2.getNElements() > 0) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST2 " + excludeList2.toString());
      cmdLineArgs.add(newArg);
    }
    if (fastBackProjInterpOrder > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "FBPINTERP " + String.valueOf(fastBackProjInterpOrder));
      cmdLineArgs.add(newArg);
    }
    if (fullImageX > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "FULLIMAGE "
      + String.valueOf(fullImageX)
      + " "
      + String.valueOf(fullImageY));
      cmdLineArgs.add(newArg);
    }
    if (!include.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("INCLUDE " + include);
      cmdLineArgs.add(newArg);
    }
    if (!localAlignFile.equals("") && !fiducialess) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOCALFILE " + localAlignFile);
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(logOffset)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOG " + String.valueOf(logOffset));
      cmdLineArgs.add(newArg);
    }
    if (mode > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MODE " + String.valueOf(mode));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(mask)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MASK " + String.valueOf(mask));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(tiltAngleOffset)) {
      String arg = "OFFSET " + String.valueOf(tiltAngleOffset);
      if (!Float.isNaN(tiltAxisOffset)) {
        arg += " " + String.valueOf(tiltAxisOffset);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }
    if (parallel) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PARALLEL");
      cmdLineArgs.add(newArg);
    }
    if (perpendicular) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PERPENDICULAR");
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(radialBandwidth)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "RADIAL "
      + String.valueOf(radialBandwidth)
      + " "
      + String.valueOf(radialFalloff));
      cmdLineArgs.add(newArg);
    }
    if (nReplicate > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "REPLICATE "
      + String.valueOf(nReplicate)
      + " "
      + String.valueOf(incReplicate));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(scaleFLevel)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "SCALE "
      + String.valueOf(scaleFLevel)
      + " "
      + String.valueOf(scaleCoeff));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(xOffset)) {
      String arg = "SHIFT " + String.valueOf(xOffset);
      if (!Float.isNaN(zOffset)) {
        arg = arg + " " + String.valueOf(zOffset);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }
    if (idxSliceStart > Integer.MIN_VALUE) {
      String arg = 
      "SLICE "
      + String.valueOf(idxSliceStart)
      + " "
      + String.valueOf(idxSliceStop);
      if (incrSlice > Integer.MIN_VALUE) {
        arg += " " + String.valueOf(incrSlice);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }
    if (idxXSubsetStart > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(
      "SUBSETSTART "
      + String.valueOf(idxXSubsetStart)
      + " "
      + String.valueOf(idxYSubsetStart));
      cmdLineArgs.add(newArg);
    }
    if (thickness > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("THICKNESS " + String.valueOf(thickness));
      cmdLineArgs.add(newArg);
    }
    if (!tiltFile.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("TILTFILE " + tiltFile);
      cmdLineArgs.add(newArg);
    }
    if (width > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("WIDTH " + String.valueOf(width));
      cmdLineArgs.add(newArg);
    }
    if (xAxisTilt > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XAXISTILT " + String.valueOf(xAxisTilt));
      cmdLineArgs.add(newArg);
    }
    if (!xTiltFile.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XTILTFILE " + xTiltFile);
      cmdLineArgs.add(newArg);
    }
    if (xTiltInterp > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XTILTINTERP " + String.valueOf(xTiltInterp));
      cmdLineArgs.add(newArg);
    }
    if (useZFactors) {
      if (zFactorFileName == null || zFactorFileName.matches("\\s*")) {
        zFactorFileName = TiltalignParam.getOutputZFactorFileName(datasetName, axisID);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument("ZFACTORFILE " + zFactorFileName);
      cmdLineArgs.add(newArg);
    }
    newArg = new ComScriptInputArg();
    newArg.setArgument("DONE");
    cmdLineArgs.add(newArg);
    int nArgs = cmdLineArgs.size();
    scriptCommand.setInputArguments(
    (ComScriptInputArg[]) cmdLineArgs.toArray(new ComScriptInputArg[nArgs]));
  }
  
  public void initializeDefaults() {
  }
  
  public ConstEtomoNumber setImageBinned(int imageBinned) {
    return this.imageBinned.set(imageBinned);
  }
  
  /**
   * If the current binning can be retrieved, set imageBinned to current
   * binning.  If not, and imageBinned is null then set imageBinned to 1.
   * @return
   */
  public ConstEtomoNumber setImageBinned() {
    EtomoNumber currentBinning = new EtomoNumber(EtomoNumber.LONG_TYPE);
    currentBinning.set(EtomoDirector.getInstance()
        .getCurrentReconManager().getStackBinning(axisID, ".ali", true));
    if (!currentBinning.isNull()) {
      imageBinned.set(currentBinning);
    }
    else if (imageBinned.isNull()) {
       imageBinned.set(1);
    }
    return imageBinned;
  }

  /**
   * Sets the excludeList.
   * @param excludeList The excludeList to set
   */
  public void setExcludeList(String list) {
    excludeList.parseString(list);
  }
  
  /**
   * Sets the excludeList2.
   * @param excludeList2 The excludeList2 to set
   */
  public void setExcludeList2(String list) {
    excludeList2.parseString(list);
  }

  public void resetExcludeList() {
    excludeList.setNElements(0);
  }
  
  public void setMontageFullImage(String userDir, int binning) {
    //Try to get the montage size from .ali file
    try {
      File aliFile = new File(userDir, datasetName + axisID.getExtension()
          + BlendmontParam.OUTPUT_FILE_EXTENSION);
      if (aliFile.exists()) {
        MRCHeader header = new MRCHeader(aliFile.getAbsolutePath(), axisID);
        header.read();
        fullImageX = header.getNColumns();
        fullImageY = header.getNRows();
        return;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    //If the .ali file is not available, use the .st file and adjust it with
    //goodframe
    try {
      Montagesize montagesize = Montagesize.getInstance(userDir, datasetName, axisID);
      if (montagesize.isFileExists()) {
        Goodframe goodframe = new Goodframe(axisID);
        goodframe.run(montagesize.getX().getInteger(), montagesize.getY().getInteger());
        fullImageX = goodframe.getFirstOutput().getInteger() / binning;
        fullImageY = goodframe.getSecondOutput().getInteger() / binning;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }

  }

  /**
   * @param i
   */
  public void setIncrSlice(int i) {
    incrSlice = i;
  }

  public void resetIncrSlice() {
    incrSlice = Integer.MIN_VALUE;
  }

  /**
   * @param i
   */
  public void setIdxSliceStart(int i) {
    idxSliceStart = i;
  }

  /**
   * @param i
   */
  public void setIdxSliceStop(int i) {
    idxSliceStop = i;
  }

  public void resetIdxSlice() {
    idxSliceStart = Integer.MIN_VALUE;
    idxSliceStop = Integer.MIN_VALUE;
    incrSlice = Integer.MIN_VALUE;
  }

  public void setInputFile(String file) {
    inputFile = file;
  }

  public void resetInputFile() {
    inputFile = "";
  }

  public void setLocalAlignFile(String filename) {
    localAlignFile = filename;
  }

  public void resetLocalAlignFile() {
    localAlignFile = "";
  }

  public void setLogShift(float shift) {
    logOffset = shift;
  }

  public void resetLogShift() {
    logOffset = Float.NaN;
  }

  public void setMode(int newMode) {
    mode = newMode;
  }

  public void resetMode() {
    mode = Integer.MIN_VALUE;
  }

  public void setOutputFile(String file) {
    outputFile = file;
  }

  public void resetOutputFile() {
    outputFile = "";
  }

  public void setParallel() {
    parallel = true;
    perpendicular = false;
  }

  public void setPerpendicular() {
    parallel = false;
    perpendicular = true;
  }

  public void resetAxisOrder() {
    parallel = false;
    perpendicular = false;
  }

  public void setRadialBandwidth(float value) {
    radialBandwidth = value;
  }

  /**
   * @param string
   */
  public void setRadialFalloff(float value) {
    radialFalloff = value;
  }

  public void resetRadialFilter() {
    radialBandwidth = Float.NaN;
    radialFalloff = Float.NaN;
  }

  public void setScale(float fLevel, float coef) {
    scaleCoeff = coef;
    scaleFLevel = fLevel;
  }

  public void resetScale() {
    scaleCoeff = Float.NaN;
    scaleFLevel = Float.NaN;
  }

  /**
   * @param scaleCoeff
   */
  public void setScaleCoeff(float scaleCoeff) {
    this.scaleCoeff = scaleCoeff;
  }

  /**
   * @param scaleFLevel
   */
  public void setScaleFLevel(float scaleFLevel) {
    this.scaleFLevel = scaleFLevel;
  }

  public void setSubsetStart(int xStart, int yStart) {
    idxXSubsetStart = xStart;
    idxYSubsetStart = yStart;
  }

  public void resetSubsetStart() {
    idxXSubsetStart = Integer.MIN_VALUE;
    idxYSubsetStart = Integer.MIN_VALUE;
  }

  public void setThickness(int newThickness) {
    thickness = newThickness;
  }

  public void resetThickness() {
    thickness = Integer.MIN_VALUE;
  }

  /**
   * @param d
   */
  public void setTiltAngleOffset(float d) {
    tiltAngleOffset = d;
  }

  public void resetTiltAngleOffset() {
    tiltAngleOffset = Float.NaN;
  }

  /**
   * @param d
   */
  public void setTiltAxisOffset(float d) {
    tiltAxisOffset = d;
  }

  public void resetTiltAxisOffset() {
    tiltAxisOffset = Float.NaN;
  }

  public void setTiltFile(String filename) {
    tiltFile = filename;
  }

  /**
   * @param i
   */
  public void setWidth(int i) {
    width = i;
  }

  public void resetWidth() {
    width = Integer.MIN_VALUE;
  }

  public void setXAxisTilt(float angle) {
    xAxisTilt = angle;
  }

  public void resetXAxisTilt() {
    xAxisTilt = Float.NaN;
  }

  /**
   * @param d
   */
  public void setXOffset(float d) {
    xOffset = d;
  }

  public void resetXOffset() {
    xOffset = Float.NaN;
  }

  /**
   * @param d
   */
  public void setZOffset(float d) {
    zOffset = d;
  }

  public void resetZOffset() {
    zOffset = Float.NaN;
  }
  
  public void setUseZFactors(boolean useZFactors) {
    this.useZFactors = useZFactors;
  }
  
  public void setFiducialess(boolean fiducialess) {
    this.fiducialess = fiducialess;
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
  
  /**
   * Backward compatibility fix.  Unbinned all the parameters which where binned
   * in the old version.  Ignore parameters with reset values.  Will throw an
   * IllegalStateException if it doesn't think that it is an old version.  The
   * param should be loaded from a com file before running this function.
   * @param binning
   * @return true if changes where made
   */
  public boolean upgradeOldVersion(int correctionBinning, long currentBinning) {
    if (!isOldVersion()) {
      return false;
    }
    imageBinned.set(currentBinning);
    //Currently this function only multiplies by binning, so there is nothing to
    //do if binning is 1.
    if (correctionBinning != 1) {
      if (fullImageX != Integer.MIN_VALUE && fullImageX != 0) {
        fullImageX *= correctionBinning;
      }
      if (fullImageY != Integer.MIN_VALUE && fullImageY != 0) {
        fullImageY *= correctionBinning;
      }
      if (width != Integer.MIN_VALUE && width != 0) {
        width *= correctionBinning;
      }
      if (!Float.isNaN(zOffset) && zOffset != 0) {
        zOffset *= correctionBinning;
      }
      if (!Float.isNaN(xOffset) && xOffset != 0) {
        xOffset *= correctionBinning;
      }
      if (idxSliceStart != Integer.MIN_VALUE && idxSliceStart != 0) {
        idxSliceStart *= correctionBinning;
      }
      if (idxSliceStop != Integer.MIN_VALUE && idxSliceStop != 0) {
        idxSliceStop *= correctionBinning;
      }
      if (thickness != Integer.MIN_VALUE && thickness != 0) {
        thickness *= correctionBinning;
      }
    }
    StringBuffer buffer = new StringBuffer("\nUpgraded tilt"
        + axisID.getExtension() + ".com:\n");
    if (correctionBinning > 1) {
      buffer.append("Multiplied binned FullImage, Width, Offset,"
          + " IdxSliceStart, and/or Thickness by " + correctionBinning + ".\n");
    }
    buffer.append("Added " + imageBinned.getName() + " " + currentBinning
        + ".\n");
    System.err.println(buffer.toString());
    return true;
  }
}
