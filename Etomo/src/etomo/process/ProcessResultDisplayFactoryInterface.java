package etomo.process;

import etomo.type.ProcessResultDisplay;

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
* <p> $Log$ </p>
*/
public interface ProcessResultDisplayFactoryInterface {
  public static  final String  rcsid =  "$Id$";
  
  public ProcessResultDisplay getProcessResultDisplay(int dependencyIndex);
}
