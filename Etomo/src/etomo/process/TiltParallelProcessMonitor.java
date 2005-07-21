package etomo.process;

import etomo.type.AxisID;
import etomo.ui.ParallelProgressDisplay;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class TiltParallelProcessMonitor implements Runnable {
  public static final String rcsid = "$Id$";

  private ParallelProgressDisplay progressDisplay = null;
  private static TiltParallelProcessMonitor monitorA = null;
  private static TiltParallelProcessMonitor monitorB = null;

  public static final TiltParallelProcessMonitor getInstance(AxisID axisID,
      ParallelProgressDisplay progressDisplay) {
    if (axisID == AxisID.SECOND) {
      if (monitorB != null) {
        return monitorB;
      }
      monitorB = new TiltParallelProcessMonitor(progressDisplay);
      return monitorB;
    }
    if (monitorA != null) {
      return monitorA;
    }
    monitorA = new TiltParallelProcessMonitor(progressDisplay);
    return monitorA;
  }
  
  public final void reset() {
    progressDisplay.teardownParallelProgressDisplay();
    progressDisplay = null;
  }

  private TiltParallelProcessMonitor(ParallelProgressDisplay progressDisplay) {
    this.progressDisplay = progressDisplay;
    progressDisplay.setupParallelProgressDisplay();
  }
  
  public void run() {
  }
}
/**
 * <p> $Log$ </p>
 */
