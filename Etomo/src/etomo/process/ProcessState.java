package etomo.process;


// FIXME don't return null throw an exception for bad objects
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
 * <p> $Log$ </p>
 */
public class ProcessState {
  public static final String rcsid = "$Id$";

  private final String name;

  private ProcessState(String name) {
    this.name = name;
  }

  public static final ProcessState NOTSTARTED = new
    ProcessState("Not started");
  public static final ProcessState INPROGRESS = new
    ProcessState("In progress");
  public static final ProcessState COMPLETE = new
    ProcessState("Complete");

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
    if(name.compareToIgnoreCase(NOTSTARTED.toString()) == 0) {
      return NOTSTARTED;
    }
    if(name.compareToIgnoreCase(INPROGRESS.toString()) == 0) {
      return INPROGRESS;
    }
    if(name.compareToIgnoreCase(COMPLETE.toString()) == 0) {
      return COMPLETE;
    }
    return null;
  }
}
