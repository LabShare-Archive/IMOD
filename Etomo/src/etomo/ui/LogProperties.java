package etomo.ui;

import java.awt.Dimension;
import java.awt.Point;
import java.util.Properties;

import etomo.type.ConstLogProperties;
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
* <p> $Log$ </p>
*/
public final class LogProperties implements ConstLogProperties {
  public static final String rcsid = "$Id$";

  private final EtomoNumber frameSizeWidth = new EtomoNumber("FrameSize.Width");
  private final EtomoNumber frameSizeHeight = new EtomoNumber(
      "FrameSize.Height");
  private final EtomoNumber frameLocationX = new EtomoNumber("FrameLocation.X");
  private final EtomoNumber frameLocationY = new EtomoNumber("FrameLocation.Y");

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

  public Dimension getFrameSize() {
    if (frameSizeWidth.isNull()||frameSizeHeight.isNull()) {
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
  }

  public void load(Properties props, String prepend) {
    //reset
    frameSizeWidth.reset();
    frameSizeHeight.reset();
    frameLocationX.reset();
    frameLocationY.reset();
    //load
    prepend = getPrepend(prepend);
    frameSizeWidth.load(props, prepend);
    frameSizeHeight.load(props, prepend);
    frameLocationX.load(props, prepend);
    frameLocationY.load(props, prepend);
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

