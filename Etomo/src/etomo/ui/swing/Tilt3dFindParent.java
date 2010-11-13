package etomo.ui.swing;

import etomo.type.ProcessResultDisplay;
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
 * <p> Revision 3.2  2010/04/09 03:02:04  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
interface Tilt3dFindParent extends TiltParent {
  public static final String rcsid = "$Id$";

  public void tilt3dFindAction(ProcessResultDisplay processResultDisplay,
      Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions);
}
