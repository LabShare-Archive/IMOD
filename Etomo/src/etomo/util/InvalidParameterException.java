package etomo.util;

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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1  2002/10/03 19:16:02  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class InvalidParameterException extends Exception {

  public InvalidParameterException(String message) {
    super(message);
  }
}
