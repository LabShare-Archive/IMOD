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
  
  private AxisID axisID;
  private String datasetName;
  private EtomoBoolean2 readInXcorrs;
  private EtomoBoolean2 oldEdgeFunctions;
  //temporary fix
  private EtomoBoolean2 imagesAreBinned;
  
  public BlendmontParam(String datasetName, AxisID axisID) {
    this.datasetName = datasetName;
    this.axisID = axisID;
    readInXcorrs = new EtomoBoolean2("ReadInXcorrs");
    readInXcorrs.setUpdateAsInteger(true);
    oldEdgeFunctions = new EtomoBoolean2("OldEdgeFunctions");
    oldEdgeFunctions.setUpdateAsInteger(true);
    imagesAreBinned = new EtomoBoolean2("ImagesAreBinned");
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
    imagesAreBinned.setInScript(scriptCommand);
  }
  
  public void initializeDefaults() {
  }
  
  /**
   * Sets the state of blendmont parameters based on the .edc and .xef files
   * @return true if blendmont needs to be run, false if blendmont does not need
   * to be run
   */
  public boolean setBlendmontState() {
    File edcFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".edc");
    File xefFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".xef");
    File stackFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".st");
    File blendFile = new File(EtomoDirector.getInstance()
        .getCurrentPropertyUserDir(), datasetName + axisID.getExtension()
        + ".bl");
    //Read in xcorr output if it exists.
    readInXcorrs.set(edcFile.exists());
    //Use existing edge functions, if they are up to date.
    oldEdgeFunctions.set(!xefFile.exists()
        || edcFile.lastModified() > xefFile.lastModified());
    //If xcorr output exists and the edge functions are up to date, then don't
    //run blendmont, unless the stack is newer then the blendmont output.
    return !readInXcorrs.is() || !oldEdgeFunctions.is()
        || stackFile.lastModified() > blendFile.lastModified();
  }
}
/**
* <p> $Log$ </p>
*/