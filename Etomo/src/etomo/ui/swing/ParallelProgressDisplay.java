package etomo.ui.swing;

import java.util.Map;

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
 * 
 * <p> $Log$
 * <p> Revision 1.12  2009/04/20 20:06:42  sueh
 * <p> bug# 1192 Added setComputerMap.
 * <p>
 * <p> Revision 1.11  2007/09/27 21:01:31  sueh
 * <p> bug# 1044 Removed setParallelProcessMonitor because the parallel process
 * <p> monitor was never used in any Parallel Progress Display.  Added
 * <p> msgEndingProcess and resetResults.
 * <p>
 * <p> Revision 1.10  2005/11/19 02:44:47  sueh
 * <p> bug# 744 Turned msgInterruptingProcess into msgKillingProcess and
 * <p> msgPausingProcess, so that the kill could turn off the pause button.
 * <p> Added msgStartingProcessOnSelectedComputers so that the failure
 * <p> reason could be erased when running processchunks.
 * <p>
 * <p> Revision 1.9  2005/09/27 23:40:55  sueh
 * <p> bug# 532 Removed some responsibility from ParallelProgressDisplay
 * <p> because AxisProgressPanel is not using this interface anymore.
 * <p>
 * <p> Revision 1.8  2005/09/22 21:28:49  sueh
 * <p> bug# 532 Moved the parallel process panel to AxisProcessPanel.
 * <p>
 * <p> Revision 1.7  2005/09/01 18:02:33  sueh
 * <p> bug# 532 Added a drop reason.
 * <p>
 * <p> Revision 1.6  2005/08/30 19:22:08  sueh
 * <p> bug# 532 Added setParallelProcessMonitor() to set a monitor in the display.
 * <p>
 * <p> Revision 1.5  2005/08/22 18:09:55  sueh
 * <p> bug# 532 Added setPausedEnabled, to control pause button.  Added
 * <p> msgInterruptingProcess().
 * <p>
 * <p> Revision 1.4  2005/08/04 20:14:44  sueh
 * <p> bug# 532  Removed demo functions.  Added functions:  addSuccess,
 * <p> addRestart, and drop.
 * <p>
 * <p> Revision 1.3  2005/08/01 18:11:50  sueh
 * <p> bug# 532 Changed ProcessorTableRow.signalRestart() to addRestart.
 * <p>
 * <p> Revision 1.2  2005/07/21 22:21:29  sueh
 * <p> bug# 532 Added setup and teardownParallelProgressDisplay() for
 * <p> ParallelProgressDisplay so that the pause button can function like a kill
 * <p> button.
 * <p>
 * <p> Revision 1.1  2005/07/11 23:13:46  sueh
 * <p> bug# 619 Added an interface that provides a way for a parallel process
 * <p> monitor to communicate with the parallel process panel without knowing
 * <p> anything about the gui.
 * <p> </p>
 */
public interface ParallelProgressDisplay {
  public static final String rcsid = "$Id$";

  public void msgDropped(String computer, String reason);

  public void addSuccess(String computer);

  public void addRestart(String computer);

  public void msgKillingProcess();

  public void msgPausingProcess();

  public void msgStartingProcessOnSelectedComputers();

  public void msgEndingProcess();

  public void resetResults();

  /**
   * Used by reconnect.  Sets the computers and CPUs that where is use when the
   * parallel process was last being tracked by Etomo.
   * @param computerMap
   */
  public void setComputerMap(Map<String,String> computerMap);

  public void msgProcessStarted();
}
