package etomo.process;

import etomo.BaseManager;

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
  
  BackgroundProcessMonitor backgroundProcessMonitor = null;
  
  /**
   * 
   */
  public BackgroundSystemProgram(BaseManager manager, String command,
    BackgroundProcessMonitor backgroundProcessMonitor) {
    super(manager.getPropertyUserDir(), command, backgroundProcessMonitor
        .getAxisID());
    this.backgroundProcessMonitor = backgroundProcessMonitor;
  }
  
  public BackgroundSystemProgram(BaseManager manager, String[] command,
      BackgroundProcessMonitor backgroundProcessMonitor) {
      super(manager.getPropertyUserDir(), command, backgroundProcessMonitor
        .getAxisID());
      this.backgroundProcessMonitor = backgroundProcessMonitor;
    }

  /**
   * use process monitor to wait for a background process to finish
   * 
   */
  protected void waitForProcess() {
    //wait until process is finished
    try {
      while (backgroundProcessMonitor.isProcessRunning()) {
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
    if (backgroundProcessMonitor.isProcessRunning()) {
      throw new IllegalStateException(
        "getExitValue() called while process is running.");
    }
    if (backgroundProcessMonitor.isSuccessful()) {
      return 0;
    }
    return 1;
  }
}
