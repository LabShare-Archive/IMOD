package etomo.ui.swing;

import etomo.type.ConstEtomoNumber;

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
public interface LoadDisplay {
  public static final String rcsid = "$Id$";

  public void setLoad(String computer, double load1, double load5, int users,
      String usersTooltip);

  public void msgLoadFailed(String computer, String reason, String tooltip);

  public void msgStartingProcess(String computer, String failureReason1,
      String failureReason2);

  public void setCPUUsage(String computer, double CPUUsage,
      ConstEtomoNumber numberOfProcessors);

  public void startLoad();

  public void stopLoad();

  public void endLoad();

  public void setLoad(String computer, String[] loadArray);
}
/**
* <p> $Log$
* <p> Revision 1.2  2011/02/22 18:13:51  sueh
* <p> bug# 1437 Reformatting.
* <p>
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.1  2007/09/27 20:52:52  sueh
* <p> bug# 1044 Changed "Load average" to "Load".  "Load average" now refers to the load average received when displaying computers in the processor table.  "Load" refers to any kind of load.
* <p>
* <p> Revision 1.11  2007/05/25 00:26:31  sueh
* <p> bug# 994 Passing tooltip to msgLoadAverageFailed and
* <p> msgStartingProcess.
* <p>
* <p> Revision 1.10  2006/11/29 00:19:37  sueh
* <p> bug# 934 Added the parameter String failureReason to msgStarting(), so that it
* <p> won't delete a failure reason by another processes.
* <p>
* <p> Revision 1.9  2006/11/18 00:49:13  sueh
* <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
* <p>
* <p> Revision 1.8  2006/11/08 21:06:51  sueh
* <p> bug# 936:  SetLoadAverage:  Remove load15 and add users.
* <p>
* <p> Revision 1.7  2006/02/08 03:35:22  sueh
* <p> bug# 796 Use imodwindcpu instead of w for windows.
* <p>
* <p> Revision 1.6  2005/11/19 02:42:21  sueh
* <p> bug# 744 Changed parallel processing display clearFailureReason
* <p> function to msgStartingProcess.  This hides the implementation.
* <p>
* <p> Revision 1.5  2005/09/10 01:54:40  sueh
* <p> bug# 532 Added clearFailureReason() so that the failure reason can be
* <p> cleared when a new connection to the computer is attempted.
* <p>
* <p> Revision 1.4  2005/09/09 21:46:52  sueh
* <p> bug# 532 moved call to clearLoadAverage() to msgLoadAverageFailed().
* <p>
* <p> Revision 1.3  2005/09/01 18:01:17  sueh
* <p> bug# 532 Added clearLoadAverage() to clear the load averages when the
* <p> load average command fails.
* <p>
* <p> Revision 1.2  2005/08/30 19:18:36  sueh
* <p> bug# 532 Provide a way to tell the load average display that a load average
* <p> commmand failed.
* <p>
* <p> Revision 1.1  2005/08/22 17:55:06  sueh
* <p> bug# 532 Interface for a display that can communicate with a
* <p> LoadAverageMonitor and can set the load average.
* <p> </p>
*/
