package etomo.comscript;

import java.util.ArrayList;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class SetParam extends ConstSetParam implements CommandParam {
  public static final String rcsid = "$Id$";

  public SetParam(String expectedName, int etomoNumberType) {
    super(expectedName, etomoNumberType);
  }

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

    name = cmdLineArgs[0];
    if (expectedName != null && !name.equals(expectedName)) {
      throw new InvalidParameterException("Expecting " + expectedName
          + ", not " + name);
    }

    if (!cmdLineArgs[1].equals(delimiter)) {
      throw new InvalidParameterException("Expecting " + delimiter + ", not "
          + cmdLineArgs[1]);
    }
    if (cmdLineArgs.length > 2) {
      if (numeric) {
        numericValue.set(cmdLineArgs[2]);
      }
      else {
        value = cmdLineArgs[2];
      }
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

    cmdLineArgs.add(name);
    cmdLineArgs.add(delimiter);
    if (numeric) {
      cmdLineArgs.add(numericValue.toString(true));
    }
    else {
      cmdLineArgs.add(value);
    }

    scriptCommand.setCommandLineArgs((String[]) cmdLineArgs
        .toArray(new String[cmdLineArgs.size()]));
  }

  public void initializeDefaults() {
    reset();
  }

  /**
   * Sets the value.
   * @param value The value to set
   */
  public void setValue(String value) {
    if (numeric) {
      numericValue.set(value);
    }
    else {
      this.value = value;
    }
  }

}