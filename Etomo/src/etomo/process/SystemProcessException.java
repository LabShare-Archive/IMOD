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
