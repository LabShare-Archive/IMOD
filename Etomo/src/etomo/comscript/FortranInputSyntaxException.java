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

public class FortranInputSyntaxException extends Exception {
  public static final String rcsid = "$Id$";

  String newString = "";

  public FortranInputSyntaxException(String message) {
    super(message);
  }

  public FortranInputSyntaxException(String message, String newValues) {
    super(message);
    newString = newValues;
  }

  public String getNewString() {
    return newString;
  }
}
