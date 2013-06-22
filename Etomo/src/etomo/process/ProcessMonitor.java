package etomo.process;

import etomo.type.AxisID;
import etomo.type.ProcessEndState;

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
public interface ProcessMonitor extends Monitor {
  public static final String rcsid = "$Id$";

  public void dumpState();

  public void setProcessEndState(ProcessEndState endState);

  public ProcessEndState getProcessEndState();

  public void kill(SystemProcessInterface process, AxisID axisID);

  public void pause(SystemProcessInterface process, AxisID axisID);

  public String getStatusString();

  public ProcessMessages getProcessMessages();

  public void msgLogFileRenamed();

  /**
   * Stop the monitor.  Doesn't effect the process.
   */
  public void stop();

  /**
   * 
   * @return true if monitor is running.
   */
  public boolean isRunning();

  /**
   * Use MessageReporter instance in the monitor thread.
   */
  public void useMessageReporter();
}
/**
 * <p> $Log$
 * <p> Revision 1.9  2008/01/14 20:34:42  sueh
 * <p> bug# 1050 Added stop() and isRunning() to allow ProcessMonitor classes to work
 * <p> with ReconnectProcess.
 * <p>
 * <p> Revision 1.8  2006/09/25 16:36:29  sueh
 * <p> bug# 931 Added msgLogFileRenamed().
 * <p>
 * <p> Revision 1.7  2005/11/19 02:26:10  sueh
 * <p> bug# 744 Moving pause, getStatusString, and getProcessMessages to
 * <p> ProcessMonitor because they are potentially valid things to do for any
 * <p> monitor, not just monitors of detached processes.
 * <p>
 * <p> Revision 1.6  2005/08/30 18:52:08  sueh
 * <p> bug# Removed functions that only belong to BackgroundProcessMonitor:
 * <p> getErrorMessage, getStatusString, pause, and setProcess.
 * <p>
 * <p> Revision 1.5  2005/08/27 22:32:12  sueh
 * <p> bug# 532 Add getErrorMessage().  This is used by
 * <p> ProcesschunksProcessMonitor.
 * <p>
 * <p> Revision 1.4  2005/08/22 17:07:03  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.
 * <p>
 * <p> Revision 1.3  2005/08/15 18:25:21  sueh
 * <p> bug# 532 Adding kill and pause functions to all process monitors to allow
 * <p> processchunks to kill with an interrupt signal instead of a kill signal.
 * <p>
 * <p> Revision 1.2  2005/08/04 19:48:42  sueh
 * <p> bug# 532 Added setProcess() for the processchunks process monitor.
 * <p>
 * <p> Revision 1.1  2005/07/26 21:41:12  sueh
 * <p> bug# 701 interface for all comscript monitors so they can be passed to
 * <p> ComScriptProcess.
 * <p> </p>
 */
