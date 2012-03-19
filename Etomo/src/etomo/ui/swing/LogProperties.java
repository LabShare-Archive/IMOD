package etomo.ui.swing;

import java.awt.Dimension;
import java.awt.Point;
import java.util.Properties;

import etomo.type.ConstLogProperties;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
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
* <p> Revision 1.1  2009/03/05 23:30:17  sueh
* <p> bug# 1194 Holds the frame size and location.
* <p> </p>
*/
public final class LogProperties implements ConstLogProperties {
  public static final String rcsid = "$Id$";

  private static final String VISIBLE_TAG = "Visible";
  private static final boolean VISIBLE_DEFAULT = true;

  private final EtomoNumber frameSizeWidth = new EtomoNumber("FrameSize.Width");
  private final EtomoNumber frameSizeHeight = new EtomoNumber("FrameSize.Height");
  private final EtomoNumber frameLocationX = new EtomoNumber("FrameLocation.X");
  private final EtomoNumber frameLocationY = new EtomoNumber("FrameLocation.Y");

  private EtomoBoolean2 visible = null;

  public LogProperties() {
    frameLocationX.setDisplayValue(200);
    frameLocationY.setDisplayValue(200);
  }

  void setFrameSize(Dimension dimension) {
    if (dimension == null) {
      frameSizeWidth.reset();
      frameSizeHeight.reset();
    }
    else {
      frameSizeWidth.set(dimension.width);
      frameSizeHeight.set(dimension.height);
    }
  }

  void setVisible(final boolean input) {
    if (visible == null) {
      visible = new EtomoBoolean2(VISIBLE_TAG);
    }
    visible.set(input);
  }

  public boolean isVisible() {
    if (visible == null) {
      return VISIBLE_DEFAULT;
    }
    return visible.is();
  }

  public Dimension getFrameSize() {
    if (frameSizeWidth.isNull() || frameSizeHeight.isNull()) {
      return null;
    }
    return new Dimension(frameSizeWidth.getInt(), frameSizeHeight.getInt());
  }

  void setFrameLocation(Point point) {
    if (point == null) {
      frameLocationX.reset();
      frameLocationY.reset();
    }
    else {
      frameLocationX.set(point.x);
      frameLocationY.set(point.y);
    }
  }

  public int getFrameLocationX() {
    return frameLocationX.getInt();
  }

  public int getFrameLocationY() {
    return frameLocationY.getInt();
  }

  public void store(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    frameSizeWidth.store(props, prepend);
    frameSizeHeight.store(props, prepend);
    frameLocationX.store(props, prepend);
    frameLocationY.store(props, prepend);
    EtomoBoolean2.store(visible, props, prepend, VISIBLE_TAG);
  }

  public void load(Properties props, String prepend) {
    // reset
    frameSizeWidth.reset();
    frameSizeHeight.reset();
    frameLocationX.reset();
    frameLocationY.reset();
    if (visible != null) {
      visible.set(VISIBLE_DEFAULT);
    }
    // load
    prepend = getPrepend(prepend);
    frameSizeWidth.load(props, prepend);
    frameSizeHeight.load(props, prepend);
    frameLocationX.load(props, prepend);
    frameLocationY.load(props, prepend);
    visible = EtomoBoolean2.load(visible, VISIBLE_TAG, props, prepend);
  }

  private String getPrepend(String prepend) {
    if (prepend == "") {
      prepend = "LogPanel";
    }
    else {
      prepend += "." + prepend;
    }
    return prepend;
  }
}
