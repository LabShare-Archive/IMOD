package etomo.process;

import etomo.type.AxisID;

/**
 * <p>
 * Description: Provides an interface to a threadable class to execute IMOD com
 * scripts in the background.
 * </p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.4  2005/07/26 17:36:50  sueh
 * <p> $bug# 701 Changed BackgroundProcessMonitor to extend ProcessMonitor
 * <p> $so that a class that implements BackgroundProcessMonitor can be
 * <p> $passed to ComScriptProcess.
 * <p> $
 * <p> $Revision 1.3  2005/04/25 20:43:27  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 1.2  2004/08/23 23:32:42  sueh
 * <p> $bug# 508 changed setKilled(boolean) to kill().  changed isDone()
 * <p> $to isSuccessful
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:47:30  sueh
 * <p> $bug# 508 Generic interface for CombineProcessMonitor.
 * <p> $$ </p>
 */
public interface BackgroundProcessMonitor extends ProcessMonitor {
  public static final String rcsid = "$$Id$$";
  
  public void setProcess(SystemProcessInterface process);
  public void pause(SystemProcessInterface process, AxisID axisID);
  public String getStatusString();
  public String getErrorMessage();
}
