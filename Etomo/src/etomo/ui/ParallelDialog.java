package etomo.ui;

import etomo.comscript.ParallelParam;

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
*/
public interface ParallelDialog {
  public static  final String  rcsid =  "$Id$";
  
  public void resume();
  public ParallelProgressDisplay getParallelProgressDisplay();
  public void getParameters(ParallelParam param);
  public void resetParallelPanel();
}
/**
* <p> $Log$
* <p> Revision 1.4  2005/09/16 18:10:37  sueh
* <p> bug# 532 Added getParallelProgressDisplay() and getParameters().
* <p>
* <p> Revision 1.3  2005/07/11 23:08:20  sueh
* <p> bug# 619 Deleted the ParallelDialog class which extends JDialog and was
* <p> used for showing the parallel panel in a demo because it isn't needed.
* <p> Added interface ParallelDialog to provide a generic parent for
* <p> ParallelPanel.  Gives a generic way to call a resume function.
* <p> </p>
*/