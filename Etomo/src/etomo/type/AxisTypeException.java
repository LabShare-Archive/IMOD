package etomo.type;

/**
 * <p>Description: An exception class to signify incorrect AxisType parameters.</p>
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
 * <p> Revision 1.1  2002/09/20 16:50:17  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class AxisTypeException extends Exception {
  public static final String rcsid =
    "$Id$";

  public AxisTypeException(String message) {
    super(message);
  }
}
