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
public interface ProcessMonitor extends Runnable {
  public static  final String  rcsid =  "$Id$";
  
  public void setProcessEndState(ProcessEndState endState);
  public ProcessEndState getProcessEndState();
  public void setProcess(SystemProcessInterface process);
  public void kill(SystemProcessInterface process, AxisID axisID);
  public void pause(SystemProcessInterface process, AxisID axisID);
  public String getStatusString();
  public String getErrorMessage();
}
/**
* <p> $Log$
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