package etomo.process;

/*
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
 * <p> Revision 2.1  2003/03/20 17:27:10  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessState {
  public static final String rcsid =
    "$Id$";

  private final String name;

  private ProcessState(String name) {
    this.name = name;
  }

  public static final ProcessState NOTSTARTED = new ProcessState("Not started");
  public static final ProcessState INPROGRESS = new ProcessState("In progress");
  public static final ProcessState COMPLETE = new ProcessState("Complete");

  /**
   * Returns a string representation of the object
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an ProcessState type and returns the
   * correct static object.  The string is case insensitive.  Null is returned
   * if the string is not one of the possibilities from toString().
   */
  public static ProcessState fromString(String name) {
    if (name.compareToIgnoreCase(NOTSTARTED.toString()) == 0) {
      return NOTSTARTED;
    }
    if (name.compareToIgnoreCase(INPROGRESS.toString()) == 0) {
      return INPROGRESS;
    }
    if (name.compareToIgnoreCase(COMPLETE.toString()) == 0) {
      return COMPLETE;
    }

    //  TODO don't return null throw an exception for bad objects
    return null;
  }
}
