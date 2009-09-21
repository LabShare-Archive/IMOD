package etomo.process;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;

/**
 * <p>Description: for processes TILT_3dFIND and TILT_3dFIND_REPROJECT.  Cannot
 * reconnect.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
final class Tilt3dFindProcessMonitor extends TiltProcessMonitor {
  public static final String rcsid = "$Id$";

  private final ConstTiltParam tiltParam;

  public Tilt3dFindProcessMonitor(final ApplicationManager appMgr, final AxisID id,
      final  ProcessName processName, final ConstTiltParam tiltParam) {
    super(appMgr, id, processName);
    this.tiltParam = tiltParam;
  }
  
  /**
   * Returns the instance of TiltParam that was passed into the constructor.
   * @return
   */
  ConstTiltParam getTiltParam() {
    return tiltParam;
  }
}
