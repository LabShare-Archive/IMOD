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
 * <p> $Log$
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public interface CommandParam {
  /**
   * Initialize the parameter object from the ComScriptCommand object
   * @param scriptCommand
   */
  public void parseComScript(ComScriptCommand scriptCommand)
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
