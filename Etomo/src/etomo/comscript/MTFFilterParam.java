
package etomo.comscript;

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
 * <p> $$Log$$ </p>
 */
public class MTFFilterParam
  extends ConstMTFFilterParam
  implements CommandParam {
  public static final String rcsid = "$$Id$$";
  
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      inputFile = scriptCommand.getValue("InputFile");
      if (scriptCommand.hasKeyword("OutputFile")) {
        outputFile = scriptCommand.getValue("OutputFile");
      }
      if (scriptCommand.hasKeyword("MtfFile")) {
        mtfFile = scriptCommand.getValue("MtfFile");
      }
      if (scriptCommand.hasKeyword("MaximumInverse")) {
        maximumInverse = Double.parseDouble(scriptCommand.getValue("MaximumInverse"));
      }
      if (scriptCommand.hasKeyword("HighFrequencyRadiusSigma")) {
        highFrequencyRadiusSigma.validateAndSet(scriptCommand.getValue("HighFrequencyRadiusSigma"));
      }
    }
    else {
      throw new InvalidParameterException("MTF Filter:  Missing parameter, -StandardInput.  Use Etomo to create .com file.");
    }
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    scriptCommand.useKeywordValue();
    updateParameter(scriptCommand, "InputFile", inputFile, true);
    updateParameter(scriptCommand, "OutputFile", outputFile);
    updateParameter(scriptCommand, "MtfFile", mtfFile);
    updateParameter(scriptCommand, "MaximumInverse", maximumInverse, defaultMaximumInverse);
    updateParameter(scriptCommand, "HighFrequencyRadiusSigma", highFrequencyRadiusSigma);
  }

  protected void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    String value)
    throws BadComScriptException {
    updateParameter(scriptCommand, key, value, false);
  }

  protected void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    String value,
    boolean required)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (value != null && value.length() > 0) {
      scriptCommand.setValue(key, value);
    }
    else {
      scriptCommand.deleteKey(key);
      if (required) {
        throw new BadComScriptException(
          "MTF Filter:  Missing parameter value, " + key + ".");
      }
    }
  }
  
  protected void updateParameter(ComScriptCommand scriptCommand, String key, double value, double defaultValue)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (value != Double.NaN && value != defaultValue) {
      scriptCommand.setValue(key, Double.toString(value));
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  protected void updateParameter(ComScriptCommand scriptCommand, String key, FortranInputString value)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (!value.isDefault()) {
      scriptCommand.setValue(key, value.toString());
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }


  public void setOutputFile(String outputFile) {
    this.outputFile = new String(outputFile);
  }
  public void setMtfFile(String mtfFile) {
    this.mtfFile = new String(mtfFile);
  }
  public void setMaximumInverse(String maximumInverse) {
    if (maximumInverse == null || !maximumInverse.matches("\\S+")) {
      this.maximumInverse = Double.NaN;
    }
    else {
      this.maximumInverse = Double.parseDouble(maximumInverse);
    }
  }
  public void setHighFrequencyRadiusSigma(String highFrequencyRadiusSigma)
    throws FortranInputSyntaxException {
    if (highFrequencyRadiusSigma == null) {
      
    }
    this.highFrequencyRadiusSigma.validateAndSet(highFrequencyRadiusSigma);
  }
}
