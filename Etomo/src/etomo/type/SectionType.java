package etomo.type;
/*
 * <p>Description: Section type definitions</p>
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
public class SectionType {
  public static final String rcsid = "$Id$";

  private final String name;

  private SectionType(String name) {
    this.name = name;
  }

  public static final SectionType SINGLE = new
    SectionType("Single");

  public static final SectionType SERIAL = new
    SectionType("Serial");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an SectionType type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static SectionType fromString(String name) {
    if(name.compareToIgnoreCase(SINGLE.toString()) == 0) {
      return SINGLE;
    }
    if(name.compareToIgnoreCase(SERIAL.toString()) == 0) {
      return SERIAL;
    }
    return null;
  }
}
