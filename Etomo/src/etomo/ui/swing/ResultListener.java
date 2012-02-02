package etomo.ui.swing;

/**
* <p>Description: Listens for a result from an object.</p>
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
interface ResultListener {
  public static final String rcsid = "$Id:$";

  /**
   * Object being listened to should call its listeners with its this pointer.
   * @param resultOrigin
   */
  public void processResult(final Object resultOrigin);
}
