package etomo;
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
public interface TaskInterface {
  public static  final String  rcsid =  "$Id:$";
  
  /**
   * If true, then the user will be warned if the they exit before the task is started.
   * @return
   */
  public boolean okToDrop();
}
