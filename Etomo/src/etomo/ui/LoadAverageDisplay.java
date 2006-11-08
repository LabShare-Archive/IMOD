package etomo.ui;

import etomo.process.LoadAverageMonitor;

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
public interface LoadAverageDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public LoadAverageMonitor getLoadAverageMonitor();
  public void setLoadAverage(String computer, double load1, double load5, int users);
  public void msgLoadAverageFailed(String computer, String reason);
  public void msgStartingProcess(String computer);
  public void setCPUUsage(String computer, double CPUUsage);
}
/**
* <p> $Log$
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