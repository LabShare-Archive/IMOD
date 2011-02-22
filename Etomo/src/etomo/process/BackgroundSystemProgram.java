package etomo.process;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;

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
 * <p> $Revision 1.10  2010/02/17 04:49:20  sueh
 * <p> $bug# 1301 Using the manager instead of the manager key do pop up
 * <p> $messages.
 * <p> $
 * <p> $Revision 1.9  2009/03/17 00:34:23  sueh
 * <p> $bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p> $
 * <p> $Revision 1.8  2005/11/19 02:14:52  sueh
 * <p> $bug# 744 BackgroundSystemProgram is used by
 * <p> $DetachedProcessMonitor as well as BackgroundComScriptProcess.  It is
 * <p> $the SystemProgram for detached processes because it uses the
 * <p> $monitor's isProcessRunning function to halt processing until the process
 * <p> $is done.
 * <p> $
 * <p> $Revision 1.7  2005/08/30 18:36:38  sueh
 * <p> $bug# 532 Changing monitor interface because the combine monitor now
 * <p> $implements BackgroundComScriptMonitor.
 * <p> $
 * <p> $Revision 1.6  2005/07/29 00:51:04  sueh
 * <p> $bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> $because the current manager changes when the user changes the tab.
 * <p> $Passing the manager where its needed.
 * <p> $
 * <p> $Revision 1.5  2005/04/25 20:43:44  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 1.4  2004/12/04 00:39:59  sueh
 * <p> $bug# 569 Handling directory paths with spaces:  converting from a
 * <p> $command line to a command array to prevent the command line from
 * <p> $being split on white space.
 * <p> $
 * <p> $Revision 1.3  2004/08/24 17:31:24  sueh
 * <p> $bug# 508 speed up kill by leaving waitForProcess on interrupt.
 * <p> $
 * <p> $Revision 1.2  2004/08/23 23:33:48  sueh
 * <p> $bug# 508 changed BackgroundProcessMonitor.isDone() to
 * <p> $isSuccessful
 * <p> $
 * <p> $Revision 1.1  2004/08/19 01:50:00  sueh
 * <p> $bug# 508 Inherits SystemProgram.  Waits for the end of the
 * <p> $process by watching the process monitor.  Gets the exit
 * <p> $value by asking the process monitor.
 * <p> $$ </p>
 */
public class BackgroundSystemProgram extends SystemProgram {
  public static final String rcsid = "$$Id$$";

  private static final int SLEEP = 100;

  private final DetachedProcessMonitor monitor;

  public BackgroundSystemProgram(BaseManager manager, String[] command,
      DetachedProcessMonitor monitor, AxisID axisID) {
    super(manager, manager.getPropertyUserDir(), command, axisID);
    this.monitor = monitor;
  }

  /**
   * use process monitor to wait for a background process to finish
   * 
   */
  protected void waitForProcess() {
    //wait until process is finished
    try {
      while (monitor.isProcessRunning()) {
        Thread.sleep(SLEEP);
      }
    }
    catch (InterruptedException e) {
    }
  }

  /**
   * 
   * @param process
   * @return
   */
  protected int getProcessExitValue(Process process) {
    if (monitor.isProcessRunning()) {
      throw new IllegalStateException("getExitValue() called while process is running.");
    }
    if (monitor.getProcessEndState() == ProcessEndState.DONE) {
      return 0;
    }
    return 1;
  }
}