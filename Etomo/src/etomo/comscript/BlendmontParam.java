package etomo.comscript;

import java.io.File;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class BlendmontParam implements CommandParam {
  public static  final String  rcsid =  "$Id$";
  
  public static final String GOTO_LABEL = "doblend";
  public static final String COMMAND_NAME = "blendmont";
  public static final int XCORR_MODE = -1;
  public static final int PREBLEND_MODE = -2;
  public static final int BLEND_MODE = -3;
  
  private AxisID axisID;
  private String datasetName;
  private EtomoBoolean2 readInXcorrs;
  private EtomoBoolean2 oldEdgeFunctions;
  private int mode = XCORR_MODE;
  
  public BlendmontParam(String datasetName, AxisID axisID) {
    this(datasetName, axisID, XCORR_MODE);
  }
  
  public BlendmontParam(String datasetName, AxisID axisID, int mode) {
    this.datasetName = datasetName;
    this.axisID = axisID;
    this.mode = mode;
    readInXcorrs = new EtomoBoolean2("ReadInXcorrs");
    readInXcorrs.setDisplayAsInteger(true);
    oldEdgeFunctions = new EtomoBoolean2("OldEdgeFunctions");
    oldEdgeFunctions.setDisplayAsInteger(true);
  }
  
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
  throws BadComScriptException, InvalidParameterException,
  FortranInputSyntaxException {
    readInXcorrs.parse(scriptCommand);
    oldEdgeFunctions.parse(scriptCommand);
  }
  
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
  throws BadComScriptException {
    readInXcorrs.setInScript(scriptCommand);
    oldEdgeFunctions.setInScript(scriptCommand);
  }
  
  public void initializeDefaults() {
  }
  
  /**
   * Sets the state of blendmont parameters based on the .edc and .xef files
   * @return true if blendmont needs to be run, false if blendmont does not need
   * to be run
   */
  public boolean setBlendmontState() {
    File ecdFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".ecd");
    File xefFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".xef");
    File yefFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".yef");
    File stackFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".st");
    File blendFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".bl");
    //Read in xcorr output if it exists.  Turn on for preblend and blend.
    readInXcorrs.set(mode == PREBLEND_MODE || mode == BLEND_MODE
        || ecdFile.exists());
    //Use existing edge functions, if they are up to date.  Turn on for blend.
    oldEdgeFunctions.set(mode == BLEND_MODE
        || (xefFile.exists() && yefFile.exists()
            && ecdFile.lastModified() <= xefFile.lastModified() && ecdFile
            .lastModified() <= yefFile.lastModified()));
    //If xcorr output exists and the edge functions are up to date, then don't
    //run blendmont, as long as the blendmont output is more recent then the
    //stack.
    if (readInXcorrs.is() && oldEdgeFunctions.is() && blendFile.exists()
        && stackFile.lastModified() < blendFile.lastModified()) {
      return false;
    }
    else {
      return true;
    }
  }
  
  public static String getCommandFileName(int mode) {
    switch (mode) {
    case PREBLEND_MODE:
      return "preblend";
    case BLEND_MODE:
      return "blend";
    case XCORR_MODE:
    default:
      return "xcorr";
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/03/08 00:44:12  sueh
* <p> bug# 533 Added a mode because the rules for setting readInXcorrs are
* <p> different in xcorr and preblend.  Changed set...State functions to set
* <p> readInXcorrs correctly.
* <p>
* <p> Revision 1.1  2005/03/04 00:07:03  sueh
* <p> bug# 533 Param object for the blendmont command.
* <p> </p>
*/