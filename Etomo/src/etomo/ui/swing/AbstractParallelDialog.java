package etomo.ui.swing;

import etomo.comscript.ParallelParam;
import etomo.type.DialogType;

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
public interface AbstractParallelDialog {
  public static  final String  rcsid =  "$Id$";
  
  public void getParameters(ParallelParam param);
  public DialogType getDialogType();
}
/**
* <p> $Log$
* <p> Revision 1.1  2010/11/13 16:07:35  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.2  2006/07/28 19:44:28  sueh
* <p> bug# 868 Changed AbstractParallelDialog.isParallel to
* <p> usingParallelProcessing because isParallel is too similar to a standard get
* <p> function.
* <p>
* <p> Revision 1.1  2006/03/20 18:00:57  sueh
* <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
* <p>
* <p> Revision 1.8  2005/10/15 00:34:37  sueh
* <p> bug# 532 Added isParallel().  Should return true if any parallel processing
* <p> checkbox in the dialog is selected.
* <p>
* <p> Revision 1.7  2005/09/21 16:46:16  sueh
* <p> bug# 532 Removed all resume functionality from the dialogs.  Removed
* <p> resume().  Removed getParallelProgressDisplay() because the parallel
* <p> panel can be gotten from the manager.
* <p>
* <p> Revision 1.6  2005/09/16 21:20:41  sueh
* <p> bug# 532 Changed ParallelDialog.resetParallelPanel() to
* <p> resetParallelProgressDisplay() because ParallelDialog is generic.
* <p>
* <p> Revision 1.5  2005/09/16 20:56:43  sueh
* <p> bug# 532 Moved call to resetParallelPanel() to
* <p> ApplicationManager.processchunks().  Added resetParallelPanel() to
* <p> ParallelDialog.
* <p>
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