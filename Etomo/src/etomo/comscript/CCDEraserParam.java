package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.ui.UIHarness;

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
 * <p> Revision 3.8  2009/03/17 00:30:54  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.7  2008/12/02 21:15:31  sueh
 * <p> bug# 1157 Changed betterRadius to a ScriptParameter of type double.
 * <p>
 * <p> Revision 3.6  2008/11/20 01:28:01  sueh
 * <p> bug# 1147 Made CCDEraserParam able to create commands, as well as
 * <p> update comscripts.  Added genOptions, getCommandArray, validate, and
 * <p> betterRadius.
 * <p>
 * <p> Revision 3.5  2007/12/13 21:54:03  sueh
 * <p> bug# 1057 Added boundaryReplacementList.
 * <p>
 * <p> Revision 3.4  2005/02/22 20:54:33  sueh
 * <p> bug# 600 Making parameter name constants into public static final strings.
 * <p>
 * <p> Revision 3.3  2004/06/25 00:30:49  sueh
 * <p> bug# 467 Comscript change!  OuterRadius is an out-of-date parameter that may
 * <p> appear in some PIP-style comscripts.  annulusWidth is a new
 * <p> parameter.
 * <p>
 * <p> Revision 3.2  2004/04/12 16:48:55  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.1  2003/11/18 17:59:34  rickg
 * <p> Bug #363 Spelling error correction on AllSectionObjects
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/07/25 22:51:51  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.5  2003/07/22 22:14:47  rickg
 * <p> Default parameters for automatic x-ray detection
 * <p> Delete keys containing empty parameters.
 * <p>
 * <p> Revision 2.4  2003/07/11 23:15:39  rickg
 * <p> New key/value parameters structure
 * <p> new ccderaser mode
 * <p>
 * <p> Revision 2.3  2003/07/09 16:00:51  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.2  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CCDEraserParam extends ConstCCDEraserParam implements CommandParam {
  public static final String rcsid = "$Id$";

  private static final int COMMAND_SIZE = 1;

  private final ScriptParameter betterRadius = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "BetterRadius");

  private String[] commandArray = null;
  private boolean debug = true;

  private final BaseManager manager;

  public CCDEraserParam(BaseManager manager) {
    this.manager = manager;
  }

  /**
   * creates the command, if it doesn't exist, and returns command array
   */
  public String[] getCommandArray() {
    if (commandArray == null) {
      ArrayList options = genOptions();
      commandArray = new String[options.size() + COMMAND_SIZE];
      commandArray[0] = BaseManager.getIMODBinPath()
          + ProcessName.CCD_ERASER.toString();
      for (int i = 0; i < options.size(); i++) {
        commandArray[i + COMMAND_SIZE] = (String) options.get(i);
      }
      if (debug) {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < commandArray.length; i++) {
          buffer.append(commandArray[i]);
          if (i < commandArray.length - 1) {
            buffer.append(' ');
          }
        }
        System.err.println(buffer.toString());
      }
    }
    return commandArray;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-" + INPUT_FILE_KEY);
    options.add(inputFile);
    options.add("-" + OUTPUT_FILE_KEY);
    options.add(outputFile);
    options.add("-" + "ModelFile");
    options.add(modelFile);
    options.add("-" + betterRadius.getName());
    options.add(betterRadius.toString());
    options.add("-" + POLYNOMIAL_ORDER_KEY);
    options.add(polynomialOrder);
    options.add("-MergePatches");
    options.add("-ExcludeAdjacent");
    options.add("-CircleObjects");
    options.add("/");
    return options;
  }

  public boolean validate() {
    if (betterRadius.isNull()) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Empty Better Radius value.  Please enter a value.", "Entry Error");
      return false;
    }
    return true;
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the ccderaser command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException {

    //  Check to be sure that it is a ccderaser command
    if (!scriptCommand.getCommand().equals("ccderaser")) {
      throw (new BadComScriptException("Not a ccderaser command"));
    }

    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      findPeaks = scriptCommand.hasKeyword(FIND_PEAKS_KEY);
      peakCriterion = scriptCommand.getValue(PEAK_CRITERION_KEY);
      diffCriterion = scriptCommand.getValue(DIFF_CRITERION_KEY);
      growCriterion = scriptCommand.getValue(GROW_CRITERION_KEY);
      scanCriterion = scriptCommand.getValue(SCAN_CRITERION_KEY);
      maximumRadius = scriptCommand.getValue(MAXIMUM_RADIUS_KEY);
      annulusWidth = scriptCommand.getValue(ANNULUS_WIDTH_KEY);
      xyScanSize = scriptCommand.getValue(X_Y_SCAN_SIZE_KEY);
      edgeExclusion = scriptCommand.getValue(EDGE_EXCLUSION_WIDTH_KEY);
      pointModel = scriptCommand.getValue("PointModel");
      trialMode = scriptCommand.hasKeyword(TRIAL_MODE_KEY);

      inputFile = scriptCommand.getValue(INPUT_FILE_KEY);
      outputFile = scriptCommand.getValue(OUTPUT_FILE_KEY);
      modelFile = scriptCommand.getValue("ModelFile");
      globalReplacementList = scriptCommand.getValue(ALL_SECTION_OBJECTS_KEY);
      localReplacementList = scriptCommand.getValue(LINE_OBJECTS_KEY);
      boundaryReplacementList = scriptCommand.getValue(BOUNDARY_OBJECTS_KEY);
      borderPixels = scriptCommand.getValue(BORDER_SIZE_KEY);
      polynomialOrder = scriptCommand.getValue(POLYNOMIAL_ORDER_KEY);
      includeAdjacentPoints = !scriptCommand.hasKeyword("ExcludeAdjacent");

      //handle out-of-date parameters
      outerRadius = scriptCommand.getValue("OuterRadius");
      if (!outerRadius.equals("")) {
        convertOuterRadius();
      }
    }
    else {
      inputFile = inputArgs[0].getArgument();
      outputFile = inputArgs[1].getArgument();
      modelFile = inputArgs[2].getArgument();
      globalReplacementList = inputArgs[3].getArgument();
      localReplacementList = inputArgs[4].getArgument();
      borderPixels = inputArgs[5].getArgument();
      polynomialOrder = inputArgs[6].getArgument();
      includeAdjacentPoints = inputArgs[7].getArgument().matches("\\s*1\\s*");

      //  Turn on the automatic mode with the defaults from the new com script
      findPeaks = true;
      String junk[] = inputFile.split("\\.st");
      String datasetName = junk[0];
      outputFile = datasetName + "_fixed.st";
      peakCriterion = "10.0";
      diffCriterion = "8.0";
      growCriterion = "4.0";
      edgeExclusion = "4";
      pointModel = datasetName + "_peak.mod";
      maximumRadius = "2.1";
      annulusWidth = "2.0";
      xyScanSize = "100";
      scanCriterion = "3.0";
    }
  }

  /**
   * Update the script command with the
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {

    //  Check to be sure that it is a ccderaser xommand
    if (!scriptCommand.getCommand().equals("ccderaser")) {
      throw (new BadComScriptException("Not a ccderaser command"));
    }
    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();

    scriptCommand.setValue(INPUT_FILE_KEY, inputFile);

    if (!outputFile.equals("")) {
      scriptCommand.setValue(OUTPUT_FILE_KEY, outputFile);
    }
    else {
      scriptCommand.deleteKey(OUTPUT_FILE_KEY);
    }

    if (findPeaks) {
      scriptCommand.setValue(FIND_PEAKS_KEY, "");
    }
    else {
      scriptCommand.deleteKey(FIND_PEAKS_KEY);
    }

    if (!peakCriterion.equals("")) {
      scriptCommand.setValue(PEAK_CRITERION_KEY, peakCriterion);
    }
    else {
      scriptCommand.deleteKey(PEAK_CRITERION_KEY);
    }

    if (!diffCriterion.equals("")) {
      scriptCommand.setValue(DIFF_CRITERION_KEY, diffCriterion);
    }
    else {
      scriptCommand.deleteKey(DIFF_CRITERION_KEY);
    }

    if (!growCriterion.equals("")) {
      scriptCommand.setValue(GROW_CRITERION_KEY, growCriterion);
    }
    else {
      scriptCommand.deleteKey(GROW_CRITERION_KEY);
    }

    if (!scanCriterion.equals("")) {
      scriptCommand.setValue(SCAN_CRITERION_KEY, scanCriterion);
    }
    else {
      scriptCommand.deleteKey(SCAN_CRITERION_KEY);
    }

    if (!maximumRadius.equals("")) {
      scriptCommand.setValue(MAXIMUM_RADIUS_KEY, maximumRadius);
    }
    else {
      scriptCommand.deleteKey(MAXIMUM_RADIUS_KEY);
    }

    if (!annulusWidth.equals("")) {
      scriptCommand.setValue(ANNULUS_WIDTH_KEY, annulusWidth);
    }
    else {
      scriptCommand.deleteKey(ANNULUS_WIDTH_KEY);
    }

    if (!xyScanSize.equals("")) {
      scriptCommand.setValue(X_Y_SCAN_SIZE_KEY, xyScanSize);
    }
    else {
      scriptCommand.deleteKey(X_Y_SCAN_SIZE_KEY);
    }

    if (!edgeExclusion.equals("")) {
      scriptCommand.setValue(EDGE_EXCLUSION_WIDTH_KEY, edgeExclusion);
    }
    else {
      scriptCommand.deleteKey(EDGE_EXCLUSION_WIDTH_KEY);
    }

    if (!pointModel.equals("")) {
      scriptCommand.setValue("PointModel", pointModel);
    }
    else {
      scriptCommand.deleteKey("PointModel");
    }

    if (!modelFile.equals("")) {
      scriptCommand.setValue("ModelFile", modelFile);
    }
    else {
      scriptCommand.deleteKey("ModelFile");
    }

    if (!globalReplacementList.equals("")) {
      scriptCommand.setValue(ALL_SECTION_OBJECTS_KEY, globalReplacementList);
    }
    else {
      scriptCommand.deleteKey(ALL_SECTION_OBJECTS_KEY);
    }

    if (!localReplacementList.equals("")) {
      scriptCommand.setValue(LINE_OBJECTS_KEY, localReplacementList);
    }
    else {
      scriptCommand.deleteKey(LINE_OBJECTS_KEY);
    }

    if (!boundaryReplacementList.equals("")) {
      scriptCommand.setValue(BOUNDARY_OBJECTS_KEY, boundaryReplacementList);
    }
    else {
      scriptCommand.deleteKey(BOUNDARY_OBJECTS_KEY);
    }

    if (!borderPixels.equals("")) {
      scriptCommand.setValue(BORDER_SIZE_KEY, borderPixels);
    }
    else {
      scriptCommand.deleteKey(BORDER_SIZE_KEY);
    }

    if (!polynomialOrder.equals("")) {
      scriptCommand.setValue(POLYNOMIAL_ORDER_KEY, polynomialOrder);
    }
    else {
      scriptCommand.deleteKey(POLYNOMIAL_ORDER_KEY);
    }

    if (includeAdjacentPoints) {
      scriptCommand.deleteKey("ExcludeAdjacent");
    }
    else {
      scriptCommand.setValue("ExcludeAdjacent", "");
    }

    if (trialMode) {
      scriptCommand.setValue(TRIAL_MODE_KEY, "");
    }
    else {
      scriptCommand.deleteKey(TRIAL_MODE_KEY);
    }

    //remove out-of-date parameters
    if (!outerRadius.equals("")) {
      scriptCommand.deleteKey("OuterRadius");
    }
  }

  public void initializeDefaults() {
  }

  /**
   * Converts outerRadius to annulusWidth.
   * Does nothing if annulusWidth has a value.
   * Ccderaser handles empty annulusWidth and maximumRadius.
   * Conversion from outerRadius to annulusWidth is
   * annulusWidth = outerRadius - maximumWidth.
   */
  protected void convertOuterRadius() {
    //if annulusWidth already set then return
    if (!annulusWidth.equals("") || outerRadius.equals("")
        || maximumRadius.equals("")) {
      return;
    }
    annulusWidth = String.valueOf(Float.parseFloat(outerRadius)
        - Float.parseFloat(maximumRadius));
  }

  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  public void setModelFile(String modelFile) {
    this.modelFile = modelFile;
  }

  public void setGlobalReplacementList(String globalReplacementList) {
    this.globalReplacementList = globalReplacementList;
  }

  public void setLocalReplacementList(String localReplacementList) {
    this.localReplacementList = localReplacementList;
  }

  public void setBoundaryReplacementList(String boundaryReplacementList) {
    this.boundaryReplacementList = boundaryReplacementList;
  }

  public void setBorderPixels(String borderPixels) {
    this.borderPixels = borderPixels;
  }

  public void setPolynomialOrder(String polynomialOrder) {
    this.polynomialOrder = polynomialOrder;
  }

  public void setIncludeAdjacentPoints(boolean includeAdjacentPoints) {
    this.includeAdjacentPoints = includeAdjacentPoints;
  }

  /**
   * @param string
   */
  public void setDiffCriterion(String string) {
    diffCriterion = string;
  }

  /**
   * @param string
   */
  public void setEdgeExclusion(String string) {
    edgeExclusion = string;
  }

  /**
   * @param b
   */
  public void setFindPeaks(boolean b) {
    findPeaks = b;
  }

  /**
   * @param string
   */
  public void setGrowCriterion(String string) {
    growCriterion = string;
  }

  /**
   * @param string
   */
  public void setMaximumRadius(String string) {
    maximumRadius = string;
  }

  /**
   * @param string
   */
  public void setAnnulusWidth(String string) {
    annulusWidth = string;
  }

  /**
   * @param string
   */
  public void setPeakCriterion(String string) {
    peakCriterion = string;
  }

  /**
   * @param string
   */
  public void setPointModel(String string) {
    pointModel = string;
  }

  /**
   * @param b
   */
  public void setTrialMode(boolean b) {
    trialMode = b;
  }

  /**
   * @param string
   */
  public void setXyScanSize(String string) {
    xyScanSize = string;
  }

  /**
   * @param string
   */
  public void setScanCriterion(String string) {
    scanCriterion = string;
  }

  public void setBetterRadius(double input) {
    betterRadius.set(input);
  }
}
