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
public class AxisID {
  public static final String rcsid =
    "$Id$";

  private final String name;

  private AxisID(String name) {
    this.name = name;
  }

  public static final AxisID ONLY = new AxisID("Only");
  public static final AxisID FIRST = new AxisID("First");
  public static final AxisID SECOND = new AxisID("Second");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Returns the extension associated with the specific AxisID.
   */
  public String getExtension() {
    if (this == ONLY) {
      return "";
    }
    if (this == FIRST) {
      return "a";
    }
    if (this == SECOND) {
      return "b";
    }
    return "ERROR";
  }
  /**
   * Takes a string representation of an AxisID type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static AxisID fromString(String name) {
    if (name.compareToIgnoreCase(ONLY.toString()) == 0) {
      return ONLY;
    }
    if (name.compareToIgnoreCase(FIRST.toString()) == 0) {
      return FIRST;
    }
    if (name.compareToIgnoreCase(SECOND.toString()) == 0) {
      return SECOND;
    }

    return null;
  }
}
