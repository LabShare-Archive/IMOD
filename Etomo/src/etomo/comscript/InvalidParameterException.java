package etomo.comscript;

/**
 * <p>Description: This exception is thrown when an invalida parameter or set
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
 * <p> $Log$ </p>
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

  public InvalidParameterException(String message, String comScript,
    String command, String parameter, int lineNumber) {
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
