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
 * <p> $Log$ </p>
 */

public class CombinePatchSize {
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
    if(name.compareToIgnoreCase(SMALL.toString()) == 0) {
      return SMALL;
    }
    if(name.compareToIgnoreCase(MEDIUM.toString()) == 0) {
      return MEDIUM;
    }
    if(name.compareToIgnoreCase(LARGE.toString()) == 0) {
      return LARGE;
    }

    return null;
  }
}
