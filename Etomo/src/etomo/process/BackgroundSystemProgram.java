package etomo.process;

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
 * <p> $$Log$$ </p>
 */
public class BackgroundSystemProgram extends SystemProgram {
  public static final String rcsid = "$$Id$$";
  
  private static final int SLEEP = 100;
  
  BackgroundProcessMonitor backgroundProcessMonitor = null;
  
  
  //Public constructors should have a BackgroundProcessMonitor parameter, so
  //that processMonitor is never null
  
  /**
   * 
   */
  public BackgroundSystemProgram(String command,
    BackgroundProcessMonitor backgroundProcessMonitor) {
    super(command);
    this.backgroundProcessMonitor = backgroundProcessMonitor;
  }

  /**
   * use process monitor to wait for a background process to finish
   * 
   */
  protected void waitForProcess() {
    //System.out.println("start waitForProcess:isProcessRunning=" + backgroundProcessMonitor.isProcessRunning());
    //wait until process is finished
    while (backgroundProcessMonitor.isProcessRunning()) {
      try {
        Thread.sleep(SLEEP);
      }
      catch (InterruptedException e) {
      }
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
    if (backgroundProcessMonitor.isDone()) {
      return 0;
    }
    return 1;
  }
}
