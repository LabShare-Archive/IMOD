package etomo.process;

import java.util.Map;

import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ProcessEndState;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002 - 2006</p>
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
  public static final String rcsid = "$Id$";

  public String toString();

  public String[] getStdOutput();

  public String[] getStdError();

  public boolean isStarted();

  public boolean isDone();

  public String getShellProcessID();

  public void notifyKilled();

  public void setProcessEndState(ProcessEndState endState);

  public void pause(AxisID axisID);

  public void kill(AxisID axisID);

  public void signalKill(AxisID axisID);

  public void setProcessResultDisplay(ProcessResultDisplay processResultDisplay);

  public ProcessData getProcessData();

  public boolean isNohup();

  public ConstProcessSeries getProcessSeries();

  public void setComputerMap(Map<String,String> computerMap);

  public void setProcessingMethod(ProcessingMethod processingMethod);
}
/**
* <p> $Log$
* <p> Revision 3.12  2009/04/20 20:06:01  sueh
* <p> bug# 1192 Added setComputerMap.
* <p>
* <p> Revision 3.11  2008/05/03 00:43:09  sueh
* <p> bug# 847 Added getProcessSeries().
* <p>
* <p> Revision 3.10  2006/06/15 16:17:06  sueh
* <p> bug# 871 Added isNohup().
* <p>
* <p> Revision 3.9  2006/06/05 18:03:44  sueh
* <p> bug# Added getPRocessData().
* <p>
* <p> Revision 3.8  2006/01/31 20:46:20  sueh
* <p> bug# 521 Added setProcessResultDisplay to SystemProcessInterface.
* <p> This allows the last ProcessResultDisplay used by the combine monitor
* <p> to be assigned to the process.
* <p>
* <p> Revision 3.7  2006/01/20 20:56:55  sueh
* <p> updated copyright year
* <p>
* <p> Revision 3.6  2005/11/19 02:39:58  sueh
* <p> bug# 744 Remove unecessary functions:  getCurentStdError,
* <p> getCurrentStdOutput, isDone, setCurrentStdInput, signalInterrupt.
* <p>
* <p> Revision 3.5  2005/10/21 19:56:04  sueh
* <p> bug# 742 Added getCurrentStdError().
* <p>
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
