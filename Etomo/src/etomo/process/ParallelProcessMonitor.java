package etomo.process;
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
*/
public interface ParallelProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public void drop(String computer);
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/08/30 18:46:06  sueh
* <p> bug# 532 Interface for ProcesschunksProcessMonitor.  Allows another
* <p> class to tell monitor to drop a computer.
* <p> </p>
*/