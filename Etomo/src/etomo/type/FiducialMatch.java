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
 * <p> Revision 3.1  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
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
public final class FiducialMatch {
  public static final String rcsid =
    "$Id$";

  private final String name;

  private FiducialMatch(String name) {
    this.name = name;
  }
  public static final FiducialMatch NOT_SET = new FiducialMatch("Not Set");
  public static final FiducialMatch BOTH_SIDES = new FiducialMatch("BothSides");
  public static final FiducialMatch ONE_SIDE = new FiducialMatch("OneSide");
  public static final FiducialMatch ONE_SIDE_INVERTED =
    new FiducialMatch("OneSideInverted");
  public static final FiducialMatch USE_MODEL = new FiducialMatch("UseModel");
  public static final FiducialMatch USE_MODEL_ONLY = new FiducialMatch("UseModelOnly");
  
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
    if (name.compareToIgnoreCase(BOTH_SIDES.toString()) == 0) {
      return BOTH_SIDES;
    }
    if (name.compareToIgnoreCase(ONE_SIDE.toString()) == 0) {
      return ONE_SIDE;
    }
    if (name.compareToIgnoreCase(ONE_SIDE_INVERTED.toString()) == 0) {
      return ONE_SIDE_INVERTED;
    }
    if (name.compareToIgnoreCase(USE_MODEL.toString()) == 0) {
      return USE_MODEL;
    }
    if (name.compareToIgnoreCase(USE_MODEL_ONLY.toString()) == 0) {
      return USE_MODEL_ONLY;
    }

    return null;
  }
  
  public String getOption() {
    if (this == NOT_SET) {
      return null;
    }
    if (this == BOTH_SIDES) {
      return "2";
    }
    if (this == ONE_SIDE) {
      return "1";
    }
    if (this == ONE_SIDE_INVERTED) {
      return "-1";
    }
    if (this == USE_MODEL) {
      return "0";
    }
    if (this == USE_MODEL_ONLY) {
      return "-2";
    }
    return null;
  }
}
