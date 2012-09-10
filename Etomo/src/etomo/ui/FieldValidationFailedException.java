package etomo.ui;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class FieldValidationFailedException extends Exception {
  public static final String rcsid = "$Id:$";

  public FieldValidationFailedException(final String message) {
    super(message);
  }
}
