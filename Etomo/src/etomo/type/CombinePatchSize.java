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
 * <p> Revision 3.1  2006/05/12 00:08:45  sueh
 * <p> bug# 857 Placed the setupcombine options in getOption().
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
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

public final class CombinePatchSize {
  public static final String rcsid = "$Id$";

  private final String name;

  private CombinePatchSize(String name) {
    this.name = name;
  }

  public static final CombinePatchSize SMALL = new CombinePatchSize("Small");
  public static final CombinePatchSize MEDIUM = new CombinePatchSize("Medium");
  public static final CombinePatchSize LARGE = new CombinePatchSize("Large");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an CombinePatchSize type and returns the
   * correct static object.  The string is case insensitive.  Null is returned if
   * the string is not one of the possibilities from toString().
   */
  public static CombinePatchSize fromString(String name) {
    if (name.compareToIgnoreCase(SMALL.toString()) == 0) {
      return SMALL;
    }
    if (name.compareToIgnoreCase(MEDIUM.toString()) == 0) {
      return MEDIUM;
    }
    if (name.compareToIgnoreCase(LARGE.toString()) == 0) {
      return LARGE;
    }

    return null;
  }

  public String getOption() {
    if (this == SMALL) {
      return "S";
    }
    if (this == MEDIUM) {
      return "M";
    }
    if (this == LARGE) {
      return "L";
    }
    return null;
  }
}
