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
 * <p> Revision 3.1  2004/04/12 16:53:12  sueh
 * <p> bug# 409 added intializeDefaults() so that defaults can be set by the param in
 * <p> the ComScriptManager when the .com file is not available
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.3  2003/07/25 22:56:12  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.2  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public interface CommandParam {
  /**
   * Initialize the parameter object from the ComScriptCommand object
   * @param scriptCommand
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException;

  /**
   * Replace the parameters of the ComScriptCommand with the current 
   * CommandParameter object's parameters
   * @param scriptCommand
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException;

  public void initializeDefaults();
}
