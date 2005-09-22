package etomo.ui;

import java.awt.Container;

import etomo.process.ParallelProcessMonitor;

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
  public static  final String  rcsid =  "$Id$";
  
  public int getCPUsSelected();
  public void addSuccess(String computer);
  public void addRestart(String computer);
  public void msgDropped(String computer, String reason);
  public void setPauseEnabled(boolean pauseEnabled);
  public void msgInterruptingProcess();
  public void setParallelProcessMonitor(ParallelProcessMonitor monitor);
  public Container getContainer();
  public void pack();
  public void start();
  public void stop();
}
