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
public class FiducialMatch {
  public static final String rcsid = "$Id$";

  private final String name;

  private FiducialMatch(String name) {
    this.name = name;
  }

  public static final FiducialMatch BOTH_SIDES = new FiducialMatch("BothSides");
  public static final FiducialMatch ONE_SIDE = new FiducialMatch("OneSide");
  public static final FiducialMatch ONE_SIDE_INVERTED =
    new FiducialMatch("OneSideInverted");
  public static final FiducialMatch USE_MODEL = new FiducialMatch("UseModel");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an FiducialMatch type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static FiducialMatch fromString(String name) {
    if(name.compareToIgnoreCase(BOTH_SIDES.toString()) == 0) {
      return BOTH_SIDES;
    }
    if(name.compareToIgnoreCase(ONE_SIDE.toString()) == 0) {
      return ONE_SIDE;
    }
    if(name.compareToIgnoreCase(ONE_SIDE_INVERTED.toString()) == 0) {
      return ONE_SIDE_INVERTED;
    }
    if(name.compareToIgnoreCase(USE_MODEL.toString()) == 0) {
      return USE_MODEL;
    }

    return null;
  }
}
