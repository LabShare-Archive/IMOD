package etomo.ui;

import java.awt.Color;

import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2006/04/06 23:33:58  sueh
 * <p> bug# 844 Added colors for the join and the generic parallel processing
 * <p> windows.
 * <p>
 * <p> Revision 1.1  2005/04/16 01:54:54  sueh
 * <p> bug# 615 Class to hold static colors.
 * <p> </p>
 */
public final class Colors {
  public static final String rcsid = "$Id$";

  static final int java1_5Change = 17;//half the difference between 1.4 color and 1.5 color
  private static Color backgroundA = null;
  private static Color backgroundB = null;
  private static Color backgroundJoin = null;
  private static Color backgroundParallel = null;

  static Color getBackgroundA() {
    if (backgroundA == null) {
      backgroundA = getBackground(153, 179, 204);
    }
    return backgroundA;
  }
  
  static Color getBackgroundB() {
    if (backgroundB == null) {
      backgroundB = getBackground(153, 204, 179);
    }
    return backgroundB;
  }
  
  static Color getBackgroundJoin() {
    if (backgroundJoin == null) {
      backgroundJoin = getBackground(179, 153, 204);
    }
    return backgroundJoin;
  }
  
  static Color getBackgroundParallel() {
    if (backgroundParallel == null) {
      backgroundParallel = getBackground(204, 153, 179);
    }
    return backgroundParallel;
  }

  private static Color getBackground(int r, int g, int b) {
    int change = 0;
    if (Utilities.isJava1_5()) {
      change = java1_5Change;
    }
    Color background = new Color(r + change, g + change, b + change);
    return background;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/04/06 23:33:58  sueh
 * <p> bug# 844 Added colors for the join and the generic parallel processing
 * <p> windows.
 * <p>
 * <p> Revision 1.1  2005/04/16 01:54:54  sueh
 * <p> bug# 615 Class to hold static colors.
 * <p> </p>
 */