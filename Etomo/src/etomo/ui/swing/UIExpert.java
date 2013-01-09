package etomo.ui.swing;

import etomo.ProcessSeries;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
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

  public void openDialog();

  public boolean startNextProcess(ProcessSeries.Process process,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display);

  public void saveAction();

  public void saveDialog(DialogExitState exitState);
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2010/04/28 16:48:49  sueh
 * <p> bug# 1344 In startNextProcess changed the process parameter into a
 * <p> ProcessSeries.Process.
 * <p>
 * <p> Revision 1.7  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.6  2008/05/28 02:53:12  sueh
 * <p> bug# 1111 Adding dialogType to startNextProcess since its been added to
 * <p> manager.startNextProcess.  It probably won't be used in UIExpert.
 * <p>
 * <p> Revision 1.5  2008/05/03 00:58:38  sueh
 * <p> bug# 847 Passing ProcessSeries to startNextProcess.
 * <p>
 * <p> Revision 1.4  2006/07/26 16:44:28  sueh
 * <p> bug# 868 formatted
 * <p>
 * <p> Revision 1.3  2006/06/30 20:04:58  sueh
 * <p> bug# 877 Added saveAction().
 * <p>
 * <p> Revision 1.2  2006/06/09 19:53:02  sueh
 * <p> bug# 870 Added ways for ApplicationManager to force an exit state.  Added a
 * <p> DialogExitState parameter to doneDialogand saveDialog.
 * <p>
 * <p> Revision 1.1  2006/05/19 19:53:13  sueh
 * <p> bug# 866 Interface for TomogramPositioningExpert.
 * <p> </p>
 */
