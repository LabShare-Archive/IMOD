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
* <p> $Log$
* <p> Revision 1.1  2005/07/11 23:13:46  sueh
* <p> bug# 619 Added an interface that provides a way for a parallel process
* <p> monitor to communicate with the parallel process panel without knowing
* <p> anything about the gui.
* <p> </p>
*/
public interface ParallelProgressDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public int getCpusSelected();
  public void setupParallelProgressDisplay();
  public void teardownParallelProgressDisplay();
  
  //TEMP demo functions
  public void signalStartProgress();
  public void signalRandomRestart();
  public void signalRandomSuccess();
}
