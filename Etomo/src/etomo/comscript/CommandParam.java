package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */

public interface CommandParam {
  /**
   * Initialize the parameter object from the ComScriptCommand object
   * @param scriptCommand
   */
  public void initialize(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException;

  /**
   * Replace the parametes of the ComScriptCommand with the current 
   * CommandParameter object's parameters
   * @param scriptCommand
   */
  public void updateComScript(ComScriptCommand scriptCommand)
    throws BadComScriptException;
}
