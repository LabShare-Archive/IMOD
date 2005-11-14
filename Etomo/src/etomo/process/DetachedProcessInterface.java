package etomo.process;

import java.io.BufferedReader;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
*/
interface DetachedProcessInterface extends SystemProcessInterface {
  public static  final String  rcsid =  "$Id$";
  
  public BufferedReader getOutputFileReader();
}
/**
* <p> $Log$ </p>
*/
