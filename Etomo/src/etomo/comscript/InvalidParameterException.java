package etomo.comscript;

/**
 * <p>Description: This exception is thrown when an invalid parameter or set
 * of parameters is detected.  These can be detected either in a com script
 * or due to user input.</p>
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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/07/11 23:16:23  rickg
 * <p> Spelling fix
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class InvalidParameterException extends Exception {
  public static final String rcsid = "$Id$";

  String comScript = "unknown";
  String command = "unknown";
  String parameter = "unknown";
  int lineNumber = 0;

  public InvalidParameterException(String message) {
    super(message);
  }

  public InvalidParameterException(String message, String comScript, String command,
      String parameter, int lineNumber) {
    super(message);
    this.comScript = comScript;
    this.command = command;
    this.parameter = parameter;
    this.lineNumber = lineNumber;
  }

  public String getComScript() {
    return comScript;
  }

  public String getCommand() {
    return command;
  }

  public String getParameter() {
    return parameter;
  }

  public int getLineNumber() {
    return lineNumber;
  }

}
