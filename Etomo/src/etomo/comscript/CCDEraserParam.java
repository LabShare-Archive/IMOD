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
  public void parseComScript(ComScriptCommand scriptCommand)
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
      outerRadius = scriptCommand.getValue("OuterRadius");
      xyScanSize = scriptCommand.getValue("XYScanSize");
      edgeExclusion = scriptCommand.getValue("EdgeExclusionWidth");
      pointModel = scriptCommand.getValue("PointModel");
      trialMode = scriptCommand.hasKeyword("TrialMode");

      inputFile = scriptCommand.getValue("InputFile");
      outputFile = scriptCommand.getValue("OutputFile");
      modelFile = scriptCommand.getValue("ModelFile");
      globalReplacementList = scriptCommand.getValue("AllSectionsObjects");
      localReplacementList = scriptCommand.getValue("LineObjects");
      borderPixels = scriptCommand.getValue("BorderSize");
      polynomialOrder = scriptCommand.getValue("PolynomialOrder");
      includeAdjacentPoints = !scriptCommand.hasKeyword("ExcludeAdjacent");
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
    }
  }

  /**
   * Update the script command with the
   */
  public void updateComScript(ComScriptCommand scriptCommand)
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

    if (findPeaks) {
      scriptCommand.setValue("FindPeaks", "");
    }
    else {
      scriptCommand.deleteKey("FindPeaks");
    }

    if (!peakCriterion.equals("")) {
      scriptCommand.setValue("PeakCriterion", peakCriterion);
    }

    if (!diffCriterion.equals("")) {
      scriptCommand.setValue("DiffCriterion", diffCriterion);
    }

    if (!growCriterion.equals("")) {
      scriptCommand.setValue("GrowCriterion", growCriterion);
    }

    if (!scanCriterion.equals("")) {
      scriptCommand.setValue("ScanCriterion", scanCriterion);
    }

    if (!maximumRadius.equals("")) {
      scriptCommand.setValue("MaximumRadius", maximumRadius);
    }

    if (!outerRadius.equals("")) {
      scriptCommand.setValue("OuterRadius", outerRadius);
    }

    if (!xyScanSize.equals("")) {
      scriptCommand.setValue("XYScanSize", xyScanSize);
    }

    if (!edgeExclusion.equals("")) {
      scriptCommand.setValue("EdgeExclusionWidth", edgeExclusion);
    }

    if (!pointModel.equals("")) {
      scriptCommand.setValue("PointModel", pointModel);
    }

    if (!modelFile.equals("")) {
      scriptCommand.setValue("ModelFile", modelFile);
    }
    if (!globalReplacementList.equals("")) {
      scriptCommand.setValue("AllSectionsObjects", globalReplacementList);
    }
    if (!localReplacementList.equals("")) {
      scriptCommand.setValue("LineObjects", localReplacementList);
    }
    if (!borderPixels.equals("")) {
      scriptCommand.setValue("BorderSize", borderPixels);
    }
    if (!polynomialOrder.equals("")) {
      scriptCommand.setValue("PolynomialOrder", polynomialOrder);
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
  public void setOuterRadius(String string) {
    outerRadius = string;
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
