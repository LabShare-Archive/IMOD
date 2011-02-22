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
 * <p> $Revision 1.2  2010/04/28 15:58:31  sueh
 * <p> $bug# 1344 Reformatted.
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:35:14  sueh
 * <p> $bug# 508 param object for the goto command
 * <p> $$ </p>
 */

public class GotoParam extends ConstGotoParam implements CommandParam {
  public static final String rcsid = "$$Id$$";

  /* (non-Javadoc)
   * @see 
   * etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    // TODO error checking - throw exceptions for bad syntax
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();

    label = cmdLineArgs[0];
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript 
   * (etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {

    // Create a new command line argument array
    ArrayList cmdLineArgs = new ArrayList(20);

    cmdLineArgs.add(label);

    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs.toArray(new String[nArgs]));
  }

  public void initializeDefaults() {
    reset();
  }

  /**
   * Sets the label.
   * @param label The label to set
   */
  public void setLabel(String label) {
    this.label = label;
  }
}
