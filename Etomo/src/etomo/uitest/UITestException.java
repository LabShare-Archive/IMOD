package etomo.uitest;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2006/11/03 19:00:39  sueh
 * <p> bug# 954 Exception for UITests.
 * <p> </p>
 */
final class UITestException extends Exception {
  public static final String rcsid = "$Id$";

  UITestException(String message) {
    super(message);
  }
}
