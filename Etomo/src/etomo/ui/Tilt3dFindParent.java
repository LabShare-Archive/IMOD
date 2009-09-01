package etomo.ui;

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
 * <p> $Log$ </p>
 */
interface Tilt3dFindParent extends TiltParent {
  public static final String rcsid = "$Id$";

  public void tilt3dFindAction(Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions);
}
