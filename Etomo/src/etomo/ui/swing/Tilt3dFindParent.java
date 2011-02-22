package etomo.ui.swing;

import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:19:51  sueh
 * <p> bug# 1420 Getting rid of some of the panel parents by handling common
 * <p> needs with generic interfaces:  ParallelProcessEnabledDialog.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.2  2010/04/09 03:02:04  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
interface Tilt3dFindParent {
  public static final String rcsid = "$Id$";

  public void tilt3dFindAction(ProcessResultDisplay processResultDisplay,
      Deferred3dmodButton deferred3dmodButton, Run3dmodMenuOptions run3dmodMenuOptions,
      final ProcessingMethod processingMethod);
}
