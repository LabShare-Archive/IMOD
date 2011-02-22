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
 * <p> $Revision 1.3  2010/04/28 16:00:15  sueh
 * <p> $bug# 1344 Reformatted.
 * <p> $
 * <p> $Revision 1.2  2004/06/24 20:20:34  sueh
 * <p> $bug# 482 Used ParamUtilities functions.  Added additional
 * <p> $matchshifts parameters.
 * <p> $
 * <p> $Revision 1.1  2004/06/24 18:37:29  sueh
 * <p> $bug# 482 param for matchshifts command
 * <p> $$</p>
 */

package etomo.comscript;

import java.util.ArrayList;

public class MatchshiftsParam extends ConstMatchshiftsParam implements CommandParam {
  public static final String rcsid = "$$Id$$";

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (cmdLineArgs.length < 5) {
      throw new InvalidParameterException("Matchshifts:  Missing parameter.");
    }
    int i = 0;
    rootName1 = cmdLineArgs[i++];
    rootName2 = cmdLineArgs[i++];
    xDim = ParamUtilities.parseInt(cmdLineArgs[i++]);
    yDim = ParamUtilities.parseInt(cmdLineArgs[i++]);
    zDim = ParamUtilities.parseInt(cmdLineArgs[i++]);
    if (cmdLineArgs.length >= 6) {
      xfIn = cmdLineArgs[i++];
    }
    if (cmdLineArgs.length >= 7) {
      xfOut = cmdLineArgs[i++];
    }
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    // Create a new command line argument array
    ArrayList cmdLineArgs = new ArrayList(20);

    if (!ParamUtilities.isEmpty(rootName1)) {
      cmdLineArgs.add(rootName1);
    }
    if (!ParamUtilities.isEmpty(rootName2)) {
      cmdLineArgs.add(rootName2);
    }
    if (xDim != Integer.MIN_VALUE) {
      cmdLineArgs.add(ParamUtilities.valueOf(xDim));
    }
    if (yDim != Integer.MIN_VALUE) {
      cmdLineArgs.add(ParamUtilities.valueOf(yDim));
    }
    if (zDim != Integer.MIN_VALUE) {
      cmdLineArgs.add(ParamUtilities.valueOf(zDim));
    }
    if (!ParamUtilities.isEmpty(xfIn)) {
      cmdLineArgs.add(xfIn);
    }
    if (!ParamUtilities.isEmpty(xfOut)) {
      cmdLineArgs.add(xfOut);
    }
    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));
  }

  public void initializeDefaults() {
  }
}
