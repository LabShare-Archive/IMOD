package etomo.comscript;

import java.util.ArrayList;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$
 * <p> $Revision 1.2  2010/04/28 15:54:07  sueh
 * <p> $bug# 1344 Reformatted.
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:34:30  sueh
 * <p> $param object for the echo command
 * <p> $$ </p>
 */

public class EchoParam extends ConstEchoParam implements CommandParam {
  public static final String rcsid = "$$Id$$";

  /* (non-Javadoc)
   * @see 
   * etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();

    for (int i = 0; i < cmdLineArgs.length; i++) {
      string.append(cmdLineArgs[i] + ' ');
    }
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript 
   * (etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {

    // Create a new command line argument array
    ArrayList cmdLineArgs = new ArrayList(20);

    cmdLineArgs.add(string);

    int nArgs = cmdLineArgs.size();
    if (nArgs == 1) {
      String[] args = new String[1];
      args[0] = cmdLineArgs.get(0).toString();
      scriptCommand.setCommandLineArgs(args);
    }
    else {
      scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));
    }
  }

  public void initializeDefaults() {
    reset();
  }

  /**
   * Sets the string.
   * @param string - The string to set
   */
  public void setString(String string) {
    this.string = new StringBuffer(string);
  }
}
