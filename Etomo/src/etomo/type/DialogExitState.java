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
 * <p> $Log$
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
public class DialogExitState {
  public static final String rcsid =
    "$Id$";

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

  public static final DialogExitState CANCEL = new DialogExitState("Cancel");
  public static final DialogExitState POSTPONE =
    new DialogExitState("Postpone");
  public static final DialogExitState EXECUTE = new DialogExitState("Execute");
}
