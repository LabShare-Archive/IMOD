/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.3  2004/03/29 20:51:46  sueh
 * <p> $bug# 409 add inputFile
 * <p> $
 * <p> $Revision 1.2  2004/03/25 00:47:18  sueh
 * <p> $bug# 409, bug# 418 added InverseRooloffRadiusSigma, moved utilities functions
 * <p> $to ParamUtilities
 * <p> $
 * <p> $Revision 1.1  2004/03/24 18:15:51  sueh
 * <p> $bug# 409 MTF filter params
 * <p> $$ </p>
 */

package etomo.comscript;

public class MTFFilterParam
  extends ConstMTFFilterParam
  implements CommandParam {
  public static final String rcsid = "$$Id$$";
  
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      if (scriptCommand.hasKeyword("InputFile")) {
        inputFile = scriptCommand.getValue("InputFile");
      }
      if (scriptCommand.hasKeyword("OutputFile")) {
        outputFile = scriptCommand.getValue("OutputFile");
      }
      if (scriptCommand.hasKeyword("MtfFile")) {
        mtfFile = scriptCommand.getValue("MtfFile");
      }
      if (scriptCommand.hasKeyword("MaximumInverse")) {
        maximumInverse = Double.parseDouble(scriptCommand.getValue("MaximumInverse"));
      }
      if (scriptCommand.hasKeyword("LowPassRadiusSigma")) {
        lowPassRadiusSigma.validateAndSet(scriptCommand.getValue("LowPassRadiusSigma"));
      }
      if (scriptCommand.hasKeyword("InverseRolloffRadiusSigma")) {
        inverseRolloffRadiusSigma.validateAndSet(scriptCommand.getValue("InverseRolloffRadiusSigma"));
      }
    }
    else {
      throw new InvalidParameterException("MTF Filter:  Missing parameter, -StandardInput.  Use Etomo to create .com file.");
    }
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    scriptCommand.useKeywordValue();
    ParamUtilities.updateParameter(scriptCommand, "InputFile", inputFile, true);
    ParamUtilities.updateParameter(scriptCommand, "OutputFile", outputFile);
    ParamUtilities.updateParameter(scriptCommand, "MtfFile", mtfFile);
    ParamUtilities.updateParameter(scriptCommand, "MaximumInverse", maximumInverse);
    ParamUtilities.updateParameter(scriptCommand, "LowPassRadiusSigma", lowPassRadiusSigma);
    ParamUtilities.updateParameter(scriptCommand, "InverseRolloffRadiusSigma", inverseRolloffRadiusSigma);
  }
  
  public void initializeDefaults() {
    maximumInverse = 4.0;
    inverseRolloffRadiusSigma.set(0, 0.12);
    inverseRolloffRadiusSigma.set(1, 0.05);
    lowPassRadiusSigma.set(0, 0.35);
    lowPassRadiusSigma.set(1, 0.05);
  }

  public void setInputFile(String inputFile) {
    this.inputFile = new String(inputFile);
  }
  public void setOutputFile(String outputFile) {
    this.outputFile = new String(outputFile);
  }
  public void setMtfFile(String mtfFile) {
    this.mtfFile = new String(mtfFile);
  }
  public void setMaximumInverse(String maximumInverse) {
    this.maximumInverse = ParamUtilities.setDouble(maximumInverse);
  }
  public void setLowPassRadiusSigma(String lowPassRadiusSigma)
    throws FortranInputSyntaxException {
      ParamUtilities.set(lowPassRadiusSigma, this.lowPassRadiusSigma);
  }
  public void setInverseRolloffRadiusSigma(String inverseRolloffRadiusSigma)
    throws FortranInputSyntaxException {
      ParamUtilities.set(inverseRolloffRadiusSigma, this.inverseRolloffRadiusSigma);
  }
}
