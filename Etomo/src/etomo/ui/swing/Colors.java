package etomo.ui.swing;

import java.awt.Color;

import javax.swing.plaf.ColorUIResource;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.11  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.10  2010/01/13 21:54:50  sueh
 * <p> bug# 1298 Fixed highlight colors.
 * <p>
 * <p> Revision 1.9  2009/02/27 03:52:44  sueh
 * <p> bug# 1172 Added experimental automation recording background and
 * <p> border colors.  Bug# 1188 No longer checking version when setting the background colors because Java 1.4 is no longer supported.
 * <p>
 * <p> Revision 1.8  2007/04/02 21:47:43  sueh
 * <p> bug# 964 Added CELL_DISABLED_FOREGROUND.
 * <p>
 * <p> Revision 1.7  2007/03/27 19:30:49  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.6  2007/03/01 01:28:42  sueh
 * <p> bug# 964 Made InputCell colors constant and moved them to Colors.
 * <p>
 * <p> Revision 1.5  2007/02/20 20:36:10  sueh
 * <p> bug# 964 Improved the colors.
 * <p>
 * <p> Revision 1.4  2007/02/19 22:00:41  sueh
 * <p> bug# 964 Added background color for PEET interface.
 * <p>
 * <p> Revision 1.3  2006/04/07 23:32:18  sueh
 * <p> bug# 846 Changing the background colors for java 1.5.
 * <p>
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

  static final ColorUIResource CELL_FOREGROUND = new ColorUIResource(0, 0, 0);
  static final ColorUIResource CELL_NOT_IN_USE_FOREGROUND = new ColorUIResource(102, 102,
      102);
  static final ColorUIResource CELL_ERROR_BACKGROUND = new ColorUIResource(255, 204, 204);
  static final ColorUIResource CELL_ERROR_BACKGROUND_NOT_EDITABLE = new ColorUIResource(
      230, 184, 184);// 223,179,179?
  static final ColorUIResource BACKGROUND = new ColorUIResource(255, 255, 255);
  static final ColorUIResource WARNING_BACKGROUND = new ColorUIResource(255, 255, 204);
  static final ColorUIResource WARNING_BACKGROUND_NOT_EDITABLE = new ColorUIResource(230,
      230, 184);
  static final ColorUIResource HIGHLIGHT_BACKGROUND = new ColorUIResource(204, 255, 255);
  static final ColorUIResource HIGHLIGHT_BACKGROUND_NOT_EDITABLE = new ColorUIResource(
      184, 230, 230);
  static final ColorUIResource FOREGROUND = new ColorUIResource(0, 0, 0);

  private static final ColorUIResource BACKGROUND_GREYOUT = new ColorUIResource(25, 25,
      25);
  static final ColorUIResource CELL_DISABLED_FOREGROUND = new ColorUIResource(120, 120,
      120);
  static final Color AVAILABLE_BACKGROUND = new ColorUIResource(224, 240, 255);
  static final Color AVAILABLE_BORDER = new ColorUIResource(153, 204, 255);
  private static final int BACKGROUND_ADJUSTMENT = 20;

  private static Color backgroundA = null;
  private static Color backgroundB = null;
  private static Color backgroundJoin = null;
  private static Color backgroundParallel = null;
  private static Color backgroundTools = null;
  private static ColorUIResource cellNotEditableBackground = null;

  static Color getBackgroundA() {
    if (backgroundA == null) {
      backgroundA = new Color(173, 199, 224);// saphire
    }
    return backgroundA;
  }

  static Color getBackgroundB() {
    if (backgroundB == null) {
      backgroundB = new Color(173, 224, 199);// jade
    }
    return backgroundB;
  }

  static Color getBackgroundJoin() {
    if (backgroundJoin == null) {
      backgroundJoin = new Color(199, 173, 224);// violet
    }
    return backgroundJoin;
  }

  static Color getBackgroundParallel() {
    if (backgroundParallel == null) {
      backgroundParallel = new Color(186, 224, 173);// lime
    }
    return backgroundParallel;
  }

  static Color getBackgroundTools() {
    if (backgroundTools == null) {
      backgroundTools = new Color(173, 212, 224);// azure
    }
    return backgroundTools;
  }

  static ColorUIResource getCellNotEditableBackground() {
    if (cellNotEditableBackground == null) {
      cellNotEditableBackground = subtractColor(BACKGROUND, BACKGROUND_GREYOUT);
    }
    return cellNotEditableBackground;
  }

  static ColorUIResource subtractColor(Color color, Color subtractColor) {
    return new ColorUIResource(color.getRed() - subtractColor.getRed(), color.getGreen()
        - subtractColor.getGreen(), color.getBlue() - subtractColor.getBlue());
  }

  private static ColorUIResource addColor(Color color, Color subtractColor) {
    return new ColorUIResource(color.getRed() + subtractColor.getRed(), color.getGreen()
        + subtractColor.getGreen(), color.getBlue() + subtractColor.getBlue());
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.11  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.10  2010/01/13 21:54:50  sueh
 * <p> bug# 1298 Fixed highlight colors.
 * <p>
 * <p> Revision 1.9  2009/02/27 03:52:44  sueh
 * <p> bug# 1172 Added experimental automation recording background and
 * <p> border colors.  Bug# 1188 No longer checking version when setting the background colors because Java 1.4 is no longer supported.
 * <p>
 * <p> Revision 1.8  2007/04/02 21:47:43  sueh
 * <p> bug# 964 Added CELL_DISABLED_FOREGROUND.
 * <p>
 * <p> Revision 1.7  2007/03/27 19:30:49  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.6  2007/03/01 01:28:42  sueh
 * <p> bug# 964 Made InputCell colors constant and moved them to Colors.
 * <p>
 * <p> Revision 1.5  2007/02/20 20:36:10  sueh
 * <p> bug# 964 Improved the colors.
 * <p>
 * <p> Revision 1.4  2007/02/19 22:00:41  sueh
 * <p> bug# 964 Added background color for PEET interface.
 * <p>
 * <p> Revision 1.3  2006/04/07 23:32:18  sueh
 * <p> bug# 846 Changing the background colors for java 1.5.
 * <p>
 * <p> Revision 1.2  2006/04/06 23:33:58  sueh
 * <p> bug# 844 Added colors for the join and the generic parallel processing
 * <p> windows.
 * <p>
 * <p> Revision 1.1  2005/04/16 01:54:54  sueh
 * <p> bug# 615 Class to hold static colors.
 * <p> </p>
 */
