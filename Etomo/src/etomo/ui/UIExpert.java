package etomo.ui;

import etomo.type.DialogExitState;
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
 */
public interface UIExpert {
  public static final String rcsid = "$Id$";

  public void startNextProcess(ProcessResultDisplay processResultDisplay);
  public void openDialog();
  public void saveAction();
  public void saveDialog(DialogExitState exitState);
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/06/09 19:53:02  sueh
 * <p> bug# 870 Added ways for ApplicationManager to force an exit state.  Added a
 * <p> DialogExitState parameter to doneDialogand saveDialog.
 * <p>
 * <p> Revision 1.1  2006/05/19 19:53:13  sueh
 * <p> bug# 866 Interface for TomogramPositioningExpert.
 * <p> </p>
 */
