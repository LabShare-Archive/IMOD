package etomo.type;

//
// This is the equivalent of an enum from C/C++.  See Effective Java page 105.
//

/**
 * <p>Description: Axis type is a non-instantiable class to enumerate the two
 *  axis types SINGLE_AXIS and DUAL_AXIS</p>
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
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class AxisType {
  public static final String rcsid =
    "$Id$";

  private final String name;

  private AxisType(String name) {
    this.name = name;
  }

  public static final AxisType SINGLE_AXIS = new AxisType("Single Axis");
  public static final AxisType DUAL_AXIS = new AxisType("Dual Axis");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an AxisType type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static AxisType fromString(String name) {
    if (name.compareToIgnoreCase(SINGLE_AXIS.toString()) == 0) {
      return SINGLE_AXIS;
    }
    if (name.compareToIgnoreCase(DUAL_AXIS.toString()) == 0) {
      return DUAL_AXIS;
    }
    return null;
  }

}
