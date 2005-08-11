package etomo.type;

import etomo.ui.UIHarness;

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
public class Run3dmodMenuOptions {
  public static  final String  rcsid =  "$Id$";
  
  private boolean startupWindow = false;
  private boolean binBy2 = false;
  private boolean allowBinningInZ = false;
  private boolean noOptions = false;
  
  public String toString() {
    return "[startupWindow=" + startupWindow + ",binBy2=" + binBy2
        + ",\nallowBinningInZ=" + allowBinningInZ + ",noOptions=" + noOptions
        + "]";
  }
  
  public void setNoOptions(boolean noOptions) {
    this.noOptions = noOptions;
    if (noOptions) {
      startupWindow = false;
      binBy2 = false;
    }
  }
  
  public void getOptions() {
    if (noOptions) {
      return;
    }
    boolean startupWindow = UIHarness.INSTANCE.is3dmodStartUpWindow();
    if (startupWindow) {
      this.startupWindow  = true;
    }
    boolean binBy2  = UIHarness.INSTANCE.is3dmodBinBy2();
    if (binBy2) {
      this.binBy2  = true;
    }
  }
  
  public void setAllowBinningInZ(boolean allowBinningInZ) {
    this.allowBinningInZ = allowBinningInZ;
  }
  
  public void setStartupWindow(boolean startupWindow) {
    if (noOptions) {
      return;
    }
    this.startupWindow = startupWindow;
  }
  
  public void setBinBy2(boolean binBy2) {
    if (noOptions) {
      return;
    }
    this.binBy2 = binBy2;
  }
  
  public boolean isBinBy2() {
    return binBy2;
  }
  
  public boolean isAllowBinningInZ() {
    return allowBinningInZ;
  }
  
  public boolean isStartupWindow() {
    return startupWindow;
  }
}
/**
* <p> $Log$ </p>
*/