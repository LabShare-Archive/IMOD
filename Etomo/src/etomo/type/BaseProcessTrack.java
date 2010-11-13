package etomo.type;

import etomo.process.ProcessState;
import etomo.storage.Storable;
import etomo.ui.swing.AbstractParallelDialog;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
* <p> Revision 1.5  2006/03/20 17:54:40  sueh
* <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
* <p>
* <p> Revision 1.4  2005/09/21 16:11:38  sueh
* <p> bug# 532 Added setState(ProcessState, AxisID, ParallelDialog) so that
* <p> one processchunks function in BaseManager can handle multiple dialogs.
* <p>
* <p> Revision 1.3  2004/12/14 21:40:21  sueh
* <p> bug # 565 Removing JoinProcessTrack so there is no commom code.
* <p> Use BaseProcessTrack as an interface because BaseManager is using it.
* <p>
* <p> Revision 1.2  2004/11/19 23:31:43  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/09/29 19:16:18  sueh
* <p> bug# 520 Base class for ProcessTrack and JoinProcessTrack.  Implements
* <p> Storable with generic functions and abstract functions.
* <p> </p>
*/
public interface BaseProcessTrack extends Storable {
  public static  final String  rcsid =  "$Id$";
  
  public String getRevisionNumber();
  public boolean isModified();
  public void resetModified();
  public void setState(ProcessState processState, AxisID axisID,
      AbstractParallelDialog parallelDialog);
}
