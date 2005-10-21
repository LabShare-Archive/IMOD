package etomo.process;

import etomo.type.AxisID;
import etomo.type.ProcessEndState;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002 - 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/

public interface SystemProcessInterface {
  public static  final String  rcsid =  "$Id$";
  
  public String[] getStdOutput();
  /**
   * Get standard output while the process is running
   * @return
   */
  public String[] getCurrentStdOutput();
  public String[] getCurrentStdError();
  public String[] getStdError();
  public boolean isStarted();
  public boolean isDone();
  public String getShellProcessID();
  public void notifyKilled();
  public void setProcessEndState(ProcessEndState endState);
  public void pause(AxisID axisID);
  public void kill(AxisID axisID);
  public void signalKill(AxisID axisID);
  public void signalInterrupt(AxisID axisID);
  public void setCurrentStdInput(String input);
}
/**
* <p> $Log$
* <p> Revision 3.4  2005/08/15 17:59:34  sueh
* <p> bug# 532   Processchunks needs to be killed with an interrupt instead of
* <p> a kill, so a processchunks specific class has to make the decision of
* <p> what type of signal to send.  The processchunks monitor needs to be
* <p> able to write to standard input after the interrupt.  Added functions:
* <p> pause, signalInterrupt, signalKill, kill, and setCurrentStdInput.
* <p>
* <p> Revision 3.3  2005/08/04 19:52:07  sueh
* <p> bug# 532 Added getCurrentStdOutput() for processchunks monitor.
* <p>
* <p> Revision 3.2  2005/07/26 21:46:49  sueh
* <p> bug# 701 Changed notifyKill() to notifyKilled().  Added
* <p> setProcessendState().
* <p> </p>
*/