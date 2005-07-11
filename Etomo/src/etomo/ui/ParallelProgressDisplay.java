package etomo.ui;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public interface ParallelProgressDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public int getCpusSelected();
  public void signalRandomRestart();
  public void signalRandomSuccess();
  public void signalStartProgress();
}
