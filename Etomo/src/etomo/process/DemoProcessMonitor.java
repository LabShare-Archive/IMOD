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
 * <p> $Log$ </p>
 */
package etomo.process;

import etomo.ApplicationManager;
import etomo.type.AxisID;

public class DemoProcessMonitor implements Runnable {
  public static final String rcsid = "$Id$";
  ApplicationManager applicationManager;
  AxisID axisID;
  long processStartTime;
  int updatePeriod = 500;
  String message;

  public DemoProcessMonitor(
    ApplicationManager appMgr,
    AxisID id,
    String message) {
    applicationManager = appMgr;
    axisID = id;
    this.message = message;
  }

  public void run() {
  	System.err.println(message);
    // Reset the progressBar 
    long processStartTime = System.currentTimeMillis();
		applicationManager.setProgressBar(" ", 1, axisID);
    applicationManager.setProgressBarValue(0, "Starting...", axisID);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
      return;
    }
    applicationManager.setProgressBar(message, 100, axisID);
    long elapsedTime = System.currentTimeMillis() - processStartTime;
    while (elapsedTime < 4700) {
      double fractionDone = (double)elapsedTime / 5000.0;
      double percentage = Math.round(fractionDone * 100);
      double remainingTime = 5000 - elapsedTime;
      int minutes = (int) Math.floor(remainingTime / 60000);
      int seconds =
        (int) Math.floor((remainingTime - minutes * 60000) / 1000.0);

      String message =
        String.valueOf(percentage)
          + "%   ETC: "
          + String.valueOf(minutes)
          + ":";
      if (seconds < 10) {
        message = message + "0" + String.valueOf(seconds);
      }
      else {
        message = message + String.valueOf(seconds);
      }
      applicationManager.setProgressBarValue((int) (elapsedTime/50), message, axisID);

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
