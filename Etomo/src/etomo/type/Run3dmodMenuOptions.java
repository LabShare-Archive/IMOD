package etomo.type;

import etomo.ui.swing.UIHarness;

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
  public static final String rcsid = "$Id$";

  private boolean startupWindow = false;
  private boolean binBy2 = false;
  private boolean allowBinningInZ = false;
  private boolean noOptions = false;

  public String toString() {
    return "[startupWindow=" + startupWindow + ",binBy2=" + binBy2
        + ",\nallowBinningInZ=" + allowBinningInZ + ",noOptions=" + noOptions + "]";
  }

  public void setNoOptions(boolean noOptions) {
    this.noOptions = noOptions;
    if (noOptions) {
      startupWindow = false;
      binBy2 = false;
    }
  }

  public void orGlobalOptions() {
    if (noOptions) {
      return;
    }
    boolean startupWindow = UIHarness.INSTANCE.is3dmodStartupWindow();
    if (startupWindow) {
      this.startupWindow = true;
    }
    boolean binBy2 = UIHarness.INSTANCE.is3dmodBinBy2();
    if (binBy2) {
      this.binBy2 = true;
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
 * <p> $Log$
 * <p> Revision 1.4  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2008/05/03 00:46:02  sueh
 * <p> bug# 847 Renamed setOptions to orGlobalOptions, which is a better
 * <p> description of its functionality.
 * <p>
 * <p> Revision 1.2  2005/08/12 00:21:16  sueh
 * <p> bug# 711 changed StartUpWindow to StartupWindow.
 * <p>
 * <p> Revision 1.1  2005/08/11 23:41:31  sueh
 * <p> bug# 711  Class to store menu options for running 3dmod.  Change enum
 * <p> Run3dmodMenuOption to Run3dmodMenuOptions, which can turn on
 * <p> multiple options at once.  This allows ImodState to combine input from
 * <p> the context menu and the pulldown menu.
 * <p> </p>
 */
