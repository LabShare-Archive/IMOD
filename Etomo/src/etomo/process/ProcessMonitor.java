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
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/08/04 19:48:42  sueh
* <p> bug# 532 Added setProcess() for the processchunks process monitor.
* <p>
* <p> Revision 1.1  2005/07/26 21:41:12  sueh
* <p> bug# 701 interface for all comscript monitors so they can be passed to
* <p> ComScriptProcess.
* <p> </p>
*/