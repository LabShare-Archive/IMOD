package etomo.comscript;

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

public class CCDEraserParam
  extends ConstCCDEraserParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

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
      findPeaks = scriptCommand.hasKeyword("FindPeaks");
      peakCriterion = scriptCommand.getValue("PeakCriterion");
      diffCriterion = scriptCommand.getValue("DiffCriterion");
      growCriterion = scriptCommand.getValue("GrowCriterion");
      scanCriterion = scriptCommand.getValue("ScanCriterion");
      maximumRadius = scriptCommand.getValue("MaximumRadius");
      annulusWidth = scriptCommand.getValue(ANNULUS_WIDTH);
      xyScanSize = scriptCommand.getValue("XYScanSize");
      edgeExclusion = scriptCommand.getValue("EdgeExclusionWidth");
      pointModel = scriptCommand.getValue("PointModel");
      trialMode = scriptCommand.hasKeyword("TrialMode");

      inputFile = scriptCommand.getValue("InputFile");
      outputFile = scriptCommand.getValue("OutputFile");
      modelFile = scriptCommand.getValue("ModelFile");
      globalReplacementList = scriptCommand.getValue("AllSectionObjects");
      localReplacementList = scriptCommand.getValue("LineObjects");
      borderPixels = scriptCommand.getValue("BorderSize");
      polynomialOrder = scriptCommand.getValue("PolynomialOrder");
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

    scriptCommand.setValue("InputFile", inputFile);

    if (!outputFile.equals("")) {
      scriptCommand.setValue("OutputFile", outputFile);
    }
    else {
      scriptCommand.deleteKey("OutputFile");
    }

    if (findPeaks) {
      scriptCommand.setValue("FindPeaks", "");
    }
    else {
      scriptCommand.deleteKey("FindPeaks");
    }

    if (!peakCriterion.equals("")) {
      scriptCommand.setValue("PeakCriterion", peakCriterion);
    }
    else {
      scriptCommand.deleteKey("PeakCriterion");
    }

    if (!diffCriterion.equals("")) {
      scriptCommand.setValue("DiffCriterion", diffCriterion);
    }
    else {
      scriptCommand.deleteKey("DiffCriterion");
    }

    if (!growCriterion.equals("")) {
      scriptCommand.setValue("GrowCriterion", growCriterion);
    }
    else {
      scriptCommand.deleteKey("GrowCriterion");
    }

    if (!scanCriterion.equals("")) {
      scriptCommand.setValue("ScanCriterion", scanCriterion);
    }
    else {
      scriptCommand.deleteKey("ScanCriterion");
    }

    if (!maximumRadius.equals("")) {
      scriptCommand.setValue("MaximumRadius", maximumRadius);
    }
    else {
      scriptCommand.deleteKey("MaximumRadius");
    }

    if (!annulusWidth.equals("")) {
      scriptCommand.setValue(ANNULUS_WIDTH, annulusWidth);
    }
    else {
      scriptCommand.deleteKey(ANNULUS_WIDTH);
    }

    if (!xyScanSize.equals("")) {
      scriptCommand.setValue("XYScanSize", xyScanSize);
    }
    else {
      scriptCommand.deleteKey("XYScanSize");
    }

    if (!edgeExclusion.equals("")) {
      scriptCommand.setValue("EdgeExclusionWidth", edgeExclusion);
    }
    else {
      scriptCommand.deleteKey("EdgeExclusionWidth");
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
      scriptCommand.setValue("AllSectionObjects", globalReplacementList);
    }
    else {
      scriptCommand.deleteKey("AllSectionObjects");
    }

    if (!localReplacementList.equals("")) {
      scriptCommand.setValue("LineObjects", localReplacementList);
    }
    else {
      scriptCommand.deleteKey("LineObjects");
    }

    if (!borderPixels.equals("")) {
      scriptCommand.setValue("BorderSize", borderPixels);
    }
    else {
      scriptCommand.deleteKey("BorderSize");
    }

    if (!polynomialOrder.equals("")) {
      scriptCommand.setValue("PolynomialOrder", polynomialOrder);
    }
    else {
      scriptCommand.deleteKey("PolynomialOrder");
    }

    if (includeAdjacentPoints) {
      scriptCommand.deleteKey("ExcludeAdjacent");
    }
    else {
      scriptCommand.setValue("ExcludeAdjacent", "");
    }

    if (trialMode) {
      scriptCommand.setValue("TrialMode", "");
    }
    else {
      scriptCommand.deleteKey("TrialMode");
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
    if (!annulusWidth.equals("") || outerRadius.equals("") || maximumRadius.equals("")) {
      return;
    }
    annulusWidth = String.valueOf(Float.parseFloat(outerRadius) -  Float.parseFloat(maximumRadius));
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

}
