/**
 * <p>Description: A simple progress bar updating thread for demo mode</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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
 * <p> Revision 3.1  2004/11/19 23:19:29  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.1  2004/09/29 17:52:13  sueh
 * <p> bug# 520 Switched to BaseManager.  Removing pass-through function
 * <p> calls.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.2  2003/10/01 18:17:46  rickg
 * <p> demoTime now specified in constructor
 * <p>
 * <p> Revision 1.1  2003/10/01 17:10:07  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.process;

import etomo.BaseManager;
import etomo.type.AxisID;
/**
 * @deprecated
 * @author sueh
 */
public class DemoProcessMonitor implements Runnable {
  public static final String rcsid = "$Id$";
  BaseManager manager;
  AxisID axisID;
  long processStartTime;
  int updatePeriod = 500;
  int demoTime;
  String message;

  public DemoProcessMonitor(BaseManager mgr, AxisID id, String message, int demoTime) {
    manager = mgr;
    axisID = id;
    this.message = message;
    this.demoTime = demoTime;
  }

  public void run() {

    // Reset the progressBar 
    long processStartTime = System.currentTimeMillis();
    manager.getMainPanel().setProgressBar(" ", 1, axisID);
    manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
      return;
    }
    manager.getMainPanel().setProgressBar(message, 100, axisID);
    long elapsedTime = System.currentTimeMillis() - processStartTime;
    while (elapsedTime < (demoTime - 100)) {
      double fractionDone = (double) elapsedTime / demoTime;
      double percentage = Math.round(fractionDone * 100);
      double remainingTime = demoTime - elapsedTime;
      int minutes = (int) Math.floor(remainingTime / 60000);
      int seconds = (int) Math.floor((remainingTime - minutes * 60000) / 1000.0);

      String message = String.valueOf(percentage) + "%   ETC: " + String.valueOf(minutes)
          + ":";
      if (seconds < 10) {
        message = message + "0" + String.valueOf(seconds);
      }
      else {
        message = message + String.valueOf(seconds);
      }
      manager.getMainPanel().setProgressBarValue((int) (elapsedTime / (demoTime / 100)),
          message, axisID);

      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException e) {
        return;
      }
      elapsedTime = System.currentTimeMillis() - processStartTime;
    }
  }
}
