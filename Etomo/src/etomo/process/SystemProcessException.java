package etomo.process;

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
 * <p> Revision 1.2.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/10/14 17:17:02  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/20 16:59:50  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class SystemProcessException extends Exception {
  public static final String rcsid =
    "$Id$";

  public SystemProcessException(String message) {
    super(message);
  }
}
