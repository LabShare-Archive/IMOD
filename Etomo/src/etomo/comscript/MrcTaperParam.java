package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.2  2010/03/03 04:53:02  sueh
 * <p> bug# 1311 Changed FileType.NEWST_OR_BLEND_OUTPUT to
 * <p> ALIGNED_STACK.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p> </p>
 */
public final class MrcTaperParam implements CommandParam {
  public static final String rcsid = "$Id$";

  static final String COMMAND_NAME = "mrctaper";

  private final BaseManager manager;
  private final AxisID axisID;

  private String inputFile = null;

  MrcTaperParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();
    for (int i = 0; i < cmdLineArgs.length; i++) {
      //  Is it an argument or filename
      if (cmdLineArgs[i].startsWith("-")) {
      }
      // input and output filename arguments
      else {
        /*if (i == (cmdLineArgs.length - 1)) {
         outputFile.add(cmdLineArgs[i]);
         }
         else {*/
        inputFile = cmdLineArgs[i];
        //}
      }
    }
  }

  /**
   * Update the script command with the current valus of this NewstParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    // Create a new command line argument array
    ArrayList cmdLineArgs = new ArrayList(1);
    if (inputFile != null) {
      cmdLineArgs.add(inputFile);
    }
    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));
  }

  public void initializeDefaults() {
    inputFile = FileType.ALIGNED_STACK.getFileName(manager, axisID);
  }

  private void reset() {
    inputFile = null;
  }

  public void setInputFile(String input) {
    inputFile = input;
  }
}
