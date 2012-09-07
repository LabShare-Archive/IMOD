/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2005/06/10 23:02:02  sueh
 * <p> bug# 583, bug# 683  Setting xfproduct scale shifts in align.com the same
 * <p> way binning is set.  Moved the details of setting scale shifts into
 * <p> XfproductParam.
 * <p>
 * <p> Revision 1.3  2004/04/12 16:51:26  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 1.2  2004/03/15 17:41:49  rickg
 * <p> Error message correction
 * <p>
 * <p> Revision 1.1  2004/03/12 21:18:41  rickg
 * <p> Bug# 390 Initial revision
 * <p> </p>
 */

package etomo.comscript;

public class XfproductParam extends ConstXfproductParam implements CommandParam {
  public static final String rcsid = "$Id$";

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {

    //  Check to be sure that it is a xfproduct command
    if (!scriptCommand.getCommand().equals("xfproduct")) {
      throw (new BadComScriptException("Not a xfproduct command"));
    }

    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      inputFile1 = scriptCommand.getValue("InputFile1");
      inputFile2 = scriptCommand.getValue("InputFile2");
      outputFile = scriptCommand.getValue("OutputFile");
      scaleShifts.validateAndSet(scriptCommand.getValue("ScaleShifts"));
    }
    else {
      inputFile1 = inputArgs[0].getArgument();
      inputFile2 = inputArgs[1].getArgument();
      outputFile = inputArgs[2].getArgument();
    }
  }

  /**
   * Update the script command with the attributes of this object
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {

    //  Check to be sure that it is a ccderaser xommand
    if (!scriptCommand.getCommand().equals("xfproduct")) {
      throw (new BadComScriptException("Not a xfproduct command"));
    }
    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();

    if (!inputFile1.equals("")) {
      scriptCommand.setValue("InputFile1", inputFile1);
    }
    else {
      scriptCommand.deleteKey("InputFile1");
    }

    if (!inputFile2.equals("")) {
      scriptCommand.setValue("InputFile2", inputFile2);
    }
    else {
      scriptCommand.deleteKey("InputFile2");
    }

    if (!outputFile.equals("")) {
      scriptCommand.setValue("OutputFile", outputFile);
    }
    else {
      scriptCommand.deleteKey("OutputFile");
    }

    if (scaleShifts.valuesSet() && !scaleShifts.isDefault()) {
      scriptCommand.setValue("ScaleShifts", scaleShifts.toString());
    }
    else {
      scriptCommand.deleteKey("ScaleShifts");
    }
  }

  public void initializeDefaults() {
  }

  /**
   * @param inputFile1 The inputFile1 to set.
   */
  public void setInputFile1(String inputFile1) {
    this.inputFile1 = inputFile1;
  }

  /**
   * @param inputFile2 The inputFile2 to set.
   */
  public void setInputFile2(String inputFile2) {
    this.inputFile2 = inputFile2;
  }

  /**
   * @param outputFile The outputFile to set.
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  /**
   * @param scaleShifts The scaleShifts to set.
   */
  public void setScaleShifts(int binning) throws FortranInputSyntaxException {
    if (binning > 1) {
      scaleShifts.validateAndSet("1," + String.valueOf(binning));
    }
    else {
      scaleShifts.validateAndSet("/");
    }
  }
}
