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
public interface BackgroundProcessMonitor {
  public static final String rcsid = "$$Id$$";
  
  abstract public boolean isDone();
  abstract public void setKilled(boolean killed);
  abstract public boolean isProcessRunning();
}
