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
 * <p> $Log$ </p>
 */
public class AxisTypeException extends Exception {
  public static final String rcsid = "$Id$";
  
  public AxisTypeException(String message) {
    super(message);
  }
}
