package etomo.process;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004, 2005</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3dEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.type.AxisID;

public class XcorrProcessWatcher implements Runnable {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager = null;
  private AxisID axisID = null;
  private boolean blendmont = false;

  /**
   * Construct a xcorr process watcher
   * @param appMgr
   * @param id
   */
  public XcorrProcessWatcher(ApplicationManager applicationManager,
      AxisID axisID, boolean blendmont) {
    this.applicationManager = applicationManager;
    this.axisID = axisID;
    this.blendmont = blendmont;
  }

  public void run() {
    if (blendmont) {
      BlendmontProcessMonitor blendmontMonitor = new BlendmontProcessMonitor(
          applicationManager, axisID, BlendmontParam.XCORR_MODE);
      Thread blendmontThread = new Thread(blendmontMonitor);
      blendmontThread.start();
      while (!blendmontMonitor.isDone()) {
        try {
          Thread.sleep(100);
        }
        catch (Exception e) {
          //not expecting any exception here
          e.printStackTrace();
          //send an interrupt to the monitor so it can clean up
          blendmontThread.interrupt();
          return;
        }
      }
    }
    TiltxcorrProcessWatcher tiltxcorrMonitor = new TiltxcorrProcessWatcher(
        applicationManager, axisID, blendmont);
    Thread tiltxcorrThread = new Thread(tiltxcorrMonitor);
    tiltxcorrThread.start();
    while (!tiltxcorrMonitor.isDone()) {
      try {
        Thread.sleep(100);
      }
      catch (Exception e) {
        //only expecting interrupt here
        if (!(e instanceof InterruptedException)) {
          e.printStackTrace();
        }
        //send an interrupt to the monitor so it can clean up
        tiltxcorrThread.interrupt();
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 3.6  2005/03/09 18:11:15  sueh
 * <p> bug# 533 This class used to watch tiltxcorr in the xcorr script (see
 * <p> TiltxcorrProcessWatcher).  Now it watches the xcorr script.  First it uses
 * <p> BlendmontProcessMonitor to watch blendmont, then it uses
 * <p> TiltxcorrProcessWatcher to watch tiltxcorr.  It needs to know whether
 * <p> blendmont is going to run.
 * <p>
 * <p> Revision 3.5  2004/11/19 23:26:42  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.4.4.1  2004/09/29 19:12:32  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.4  2004/04/23 20:03:08  sueh
 * <p> bug# 83 allowing initializeProgressBar() to be called before
 * <p> nSections is set
 * <p>
 * <p> Revision 3.3  2004/03/16 21:53:40  sueh
 * <p> bug# 413  when last line in the log file found, starting incrementing waitingForExit
 * <p> counter
 * <p>
 * <p> Revision 3.2  2004/03/13 01:57:35  sueh
 * <p> bug# 413 fixed backward monitor by counting lines
 * <p> possible solution for LogFileProcessMonitor.run() infinite loop in comments.
 * <p>
 * <p> Revision 3.1  2003/11/26 23:38:03  rickg
 * <p> Changed name of logFileReader
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.7  2003/08/05 21:16:26  rickg
 * <p> Correctly set nSections.
 * <p> SystemProcessInterface object is no longer necessary
 * <p>
 * <p> Revision 1.6  2003/08/04 22:23:50  rickg
 * <p> Now derived from LogFileProcessMonitor
 * <p>
 * <p> Revision 1.5  2003/06/27 20:17:51  rickg
 * <p> Fixed javadoc header
 * <p> </p>
 */