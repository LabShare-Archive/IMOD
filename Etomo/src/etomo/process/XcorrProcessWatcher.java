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
import etomo.type.ProcessEndState;

public class XcorrProcessWatcher implements ProcessMonitor {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager = null;
  private AxisID axisID = null;
  private boolean blendmont = false;
  private ProcessEndState endState = null;
  private boolean stop = false;
  private boolean running = false;

  public void dumpState() {
  }

  /**
   * Construct a xcorr process watcher
   * @param appMgr
   * @param id
   */
  public XcorrProcessWatcher(ApplicationManager applicationManager, AxisID axisID,
      boolean blendmont) {
    this.applicationManager = applicationManager;
    this.axisID = axisID;
    this.blendmont = blendmont;
  }

  public void stop() {
    stop = true;
  }

  public boolean isRunning() {
    return running;
  }

  public void useMessageReporter() {
  }

  public void run() {
    running = true;
    if (blendmont) {
      BlendmontProcessMonitor blendmontMonitor = new BlendmontProcessMonitor(
          applicationManager, axisID, BlendmontParam.Mode.XCORR);
      blendmontMonitor.setLastProcess(false);
      Thread blendmontThread = new Thread(blendmontMonitor);
      blendmontThread.start();
      while (!blendmontMonitor.isDone() && !stop) {
        try {
          Thread.sleep(100);
        }
        catch (Exception e) {
          setProcessEndState(ProcessEndState.DONE);
          // not expecting any exception here
          e.printStackTrace();
          // send an interrupt to the monitor so it can clean up
          blendmontThread.interrupt();
          return;
        }
      }
    }
    TiltxcorrProcessWatcher tiltxcorrMonitor = new TiltxcorrProcessWatcher(
        applicationManager, axisID, blendmont);
    Thread tiltxcorrThread = new Thread(tiltxcorrMonitor);
    tiltxcorrThread.start();
    while (!tiltxcorrMonitor.isDone() && !stop) {
      try {
        Thread.sleep(100);
      }
      catch (Exception e) {
        // only expecting interrupt here
        if (!(e instanceof InterruptedException)) {
          e.printStackTrace();
        }
        // send an interrupt to the monitor so it can clean up
        tiltxcorrThread.interrupt();
      }
    }
    setProcessEndState(ProcessEndState.DONE);
    running = false;
  }

  public void msgLogFileRenamed() {
  }

  /**
   * set end state
   * @param endState
   */
  public synchronized final void setProcessEndState(ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  public synchronized final ProcessEndState getProcessEndState() {
    return endState;
  }

  public void kill(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalKill(axisID);
  }

  public ProcessMessages getProcessMessages() {
    return null;
  }

  public String getStatusString() {
    return null;
  }

  public void pause(SystemProcessInterface process, AxisID axisID) {
    throw new IllegalStateException("pause illegal in this monitor");
  }

  public boolean isPausing() {
    return false;
  }

  public void setWillResume() {
  }
}
/**
 * <p> $Log$
 * <p> Revision 3.19  2010/01/11 23:51:24  sueh
 * <p> bug# 1299 Added useMessageReporter.
 * <p>
 * <p> Revision 3.18  2008/01/14 22:00:12  sueh
 * <p> bug# 1050 Added stop() and isRunning() to allow ProcessMonitor classes to work
 * <p> with ReconnectProcess.
 * <p>
 * <p> Revision 3.17  2007/02/05 23:02:59  sueh
 * <p> bug# 962 Move comscript mode info to inner class.
 * <p>
 * <p> Revision 3.16  2006/09/25 16:36:37  sueh
 * <p> bug# 931 Added empty function msgLogFileRenamed().
 * <p>
 * <p> Revision 3.15  2005/11/19 02:41:44  sueh
 * <p> bug# 744 Moving pause, getStatusString, and getProcessMessages to
 * <p> ProcessMonitor because they are potentially valid things to do for any
 * <p> monitor, not just monitors of detached processes.
 * <p>
 * <p> Revision 3.14  2005/08/30 18:53:21  sueh
 * <p> bug# 532 Removed functions that only belong to
 * <p> BackgroundProcessMonitor:  getErrorMessage, getStatusString, pause,
 * <p> and setProcess.
 * <p>
 * <p> Revision 3.13  2005/08/27 22:33:52  sueh
 * <p> bug# 532 Add an empty getErrorMessage() to implement ProcessMonitor.
 * <p> This is used by ProcesschunksProcessMonitor.
 * <p>
 * <p> Revision 3.12  2005/08/22 17:09:28  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.
 * <p>
 * <p> Revision 3.11  2005/08/15 18:27:58  sueh
 * <p> bug# 532  Added kill and pause functions to implement ProcessMonitor.
 * <p> Both kill and pause signal interrupt.  Change updateState to handle the
 * <p> interrupt message and send the correct string, based on whether a kill or
 * <p> a pause was requested.
 * <p>
 * <p> Revision 3.10  2005/08/04 19:52:53  sueh
 * <p> bug# 532 Added empty setProcess() to implement ProcessMonitor.
 * <p>
 * <p> Revision 3.9  2005/07/26 21:47:32  sueh
 * <p> bug# 701 Implementing ProcessMonitor, which extends Runnable.
 * <p> Added a ProcessEndState member variable.  Set it to DONE when the
 * <p> end of the process is detected.  In the future, will set the
 * <p> ProcessEndState variable to FAILED, if necessary to get the correct
 * <p> progress bar behavior.
 * <p>
 * <p> Revision 3.8  2005/03/11 01:35:08  sueh
 * <p> bug# 533 Setting BlendmontProcessMonitor.lastProcess to false, so it
 * <p> doesn't display "done".
 * <p>
 * <p> Revision 3.7  2005/03/09 22:31:26  sueh
 * <p> bug# 533 Catching the interrupt exception sent by ComScriptProcess.
 * <p> If it is sent while blendmont is running, there is a problem.  Pass the
 * <p> interrupt to the current monitor so that it can stop the progress bar.
 * <p>
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
