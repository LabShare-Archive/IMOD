package etomo.type;

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
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/03/20 17:28:39  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltAngleType {
  public static final String rcsid = "$Id$";

  private final String name;

  private TiltAngleType(String name) {
    this.name = name;
  }

  public static final TiltAngleType EXTRACT = new TiltAngleType("Extract");
  public static final TiltAngleType RANGE = new TiltAngleType("Range");
  public static final TiltAngleType FILE = new TiltAngleType("File");
  public static final TiltAngleType LIST = new TiltAngleType("List");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an TiltAngleType type and returns the
   *  correct static object.  The string is case insensitive.  Null is returned
   * if the string is not one of the possibilities from toString().
   */
  public static TiltAngleType fromString(String name) {
    if (name.compareToIgnoreCase(EXTRACT.toString()) == 0) {
      return EXTRACT;
    }
    if (name.compareToIgnoreCase(RANGE.toString()) == 0) {
      return RANGE;
    }
    if (name.compareToIgnoreCase(FILE.toString()) == 0) {
      return FILE;
    }
    if (name.compareToIgnoreCase(LIST.toString()) == 0) {
      return LIST;
    }

    //  TODO Don't return null throw an exception for bad arguments
    return null;
  }

  /**
   * Takes the integer representation used in the imod FORTRAN code as an
   * argument and returns a TiltAngleType representation.
   * @param typeSpec -1 for a list of tilt angles, 0 for a file containing the
   * tilt angles, 1 for range of tilt angles.
   * @return the TiltAngleType specified by the argument.
   */
  public static TiltAngleType parseInt(int typeSpec) {
    if (typeSpec == -1) {
      return LIST;
    }
    if (typeSpec == 0) {
      return FILE;
    }
    if (typeSpec == 1) {
      return RANGE;
    }
    return null;
  }

  /**
   * Returns the integer representation using in the IMOD FORTRAN code for this
   * tile axis type.  There is no code in the FORTRAN programs for "Extract"
   * which appears to be only used in copytomocoms.
   */
  public int toInt() {
    if (name.equals("List")) {
      return -1;
    }
    if (name.equals("File")) {
      return 0;
    }
    if (name.equals("Range")) {
      return 1;
    }

    return -1000;
  }

}
