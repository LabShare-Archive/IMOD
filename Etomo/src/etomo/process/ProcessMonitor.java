package etomo.process;

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
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/07/26 21:41:12  sueh
* <p> bug# 701 interface for all comscript monitors so they can be passed to
* <p> ComScriptProcess.
* <p> </p>
*/