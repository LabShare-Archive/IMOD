package etomo.type;

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
public class DialogExitState {
  public static final String rcsid = "$Id$";

  private final String name;

  private DialogExitState(String name) {
    this.name = name;
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  public static final DialogExitState CANCEL = new
    DialogExitState("Cancel");
  public static final DialogExitState POSTPONE = new
    DialogExitState("Postpone");
  public static final DialogExitState EXECUTE = new
    DialogExitState("Execute");
}
