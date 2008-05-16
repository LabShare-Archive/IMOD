package etomo.process;

import etomo.type.ProcessEndState;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface OutfileProcessMonitor extends DetachedProcessMonitor {
  public static final String rcsid = "$Id$";
  
  public String getPid();
  public void endMonitor(ProcessEndState endState);
  public String getSubProcessName();
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/12/01 00:56:59  sueh
* <p> bug# 937 Added endMonitor so that DetachedProcess.notifyKill can stop the
* <p> monitor.
* <p>
* <p> Revision 1.1  2006/06/05 16:27:00  sueh
* <p> bug# 766 Interface for monitors which are responsible for getting the pid.
* <p> </p>
*/