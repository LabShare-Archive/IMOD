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
 * <p> $$Log$$</p>
 */

package etomo.comscript;

import java.util.ArrayList;

public class MatchshiftsParam
  extends ConstMatchshiftsParam
  implements CommandParam {
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
    xDim = Integer.parseInt(cmdLineArgs[i++]);
    yDim = Integer.parseInt(cmdLineArgs[i++]);
    zDim = Integer.parseInt(cmdLineArgs[i++]);
    System.out.println("parseComScriptCommand:rootName1= " + rootName1 + ",rootName2=" + rootName2);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
      // Create a new command line argument array
      ArrayList cmdLineArgs = new ArrayList(20);

      if (!rootName1.equals("\\s+")) {
        cmdLineArgs.add(rootName1);
        System.out.println("updateComScriptCommand:added rootName1");
      }
      if (!rootName2.equals("\\s+")) {
        cmdLineArgs.add(rootName2);
        System.out.println("updateComScriptCommand:added rootName1");
      }
      if (xDim != Integer.MIN_VALUE) {
        cmdLineArgs.add(String.valueOf(xDim));
      } 
      if (yDim != Integer.MIN_VALUE) {
        cmdLineArgs.add(String.valueOf(yDim));
      } 
      if (zDim != Integer.MIN_VALUE) {
        cmdLineArgs.add(String.valueOf(zDim));
      }
      int nArgs = cmdLineArgs.size();
      scriptCommand.setCommandLineArgs(
        (String[]) cmdLineArgs.toArray(new String[nArgs]));
  }
  
  public void initializeDefaults() {
  }
}
