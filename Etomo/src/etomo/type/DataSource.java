package etomo.type;

//
// This is the equivalent of an enum from C/C++.  See Effective Java page 105.
//

/**
 * <p>Description: Data source type definition.  Possible values are
 *  DataSource.CCD and DataSource.FILM.</p>
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
public class DataSource {
  public static final String rcsid = "$Id$";

  private final String name;

  private DataSource(String name) {
    this.name = name;
  }

  //
  //  Only two instances are ever created
  //
  public static final DataSource CCD = new DataSource("CCD");
  public static final DataSource FILM = new DataSource("Film");

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of a DataSource type and returns the correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the possibilities from toString().
   */
  public static DataSource fromString(String name) {
    if (name.compareToIgnoreCase(CCD.toString()) == 0) {
      return CCD;
    }
    if (name.compareToIgnoreCase(FILM.toString()) == 0) {
      return FILM;
    }
    return null;
  }
}
