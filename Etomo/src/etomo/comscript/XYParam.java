package etomo.comscript;

import java.util.Properties;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class XYParam {
  public static final String rcsid = "$Id$";
  
  private final EtomoNumber xMin;
  private final EtomoNumber xMax;
  private final EtomoNumber yMin;
  private final EtomoNumber yMax;

  public XYParam(String name) {
    xMin = new EtomoNumber(name+"XMin");
    xMax = new EtomoNumber(name+"XMax");
    yMin = new EtomoNumber(name+"YMin");
    yMax = new EtomoNumber(name+"YMax");
  }

  void store(Properties props, String prepend) {
    xMin.store(props, prepend);
    xMax.store(props, prepend);
    yMin.store(props, prepend);
    yMax.store(props, prepend);
  }

  void load(Properties props, String prepend) {
    xMin.load(props, prepend);
    xMax.load(props, prepend);
    yMin.load(props, prepend);
    yMax.load(props, prepend);
  }

  public ConstEtomoNumber getXMin() {
    return xMin;
  }

  public ConstEtomoNumber getXMax() {
    return xMax;
  }

  public ConstEtomoNumber getYMin() {
    return yMin;
  }

  public ConstEtomoNumber getYMax() {
    return yMax;
  }

  public void setXMin(String xMin) {
    this.xMin.set(xMin);
  }

  public void setXMax(String xMax) {
    this.xMax.set(xMax);
  }

  public void setYMin(String yMin) {
    this.yMin.set(yMin);
  }

  public void setYMax(String yMax) {
    this.yMax.set(yMax);
  }
  
  public void setXMin(int xMin) {
    this.xMin.set(xMin);
  }

  public void setXMax(int xMax) {
    this.xMax.set(xMax);
  }

  public void setYMin(int yMin) {
    this.yMin.set(yMin);
  }

  public void setYMax(int yMax) {
    this.yMax.set(yMax);
  }

  public boolean equals(XYParam xyParam) {
    if (!xMin.equals(xyParam.xMin)) {
      return false;
    }
    if (!xMax.equals(xyParam.xMax)) {
      return false;
    }
    if (!yMin.equals(xyParam.yMin)) {
      return false;
    }
    if (!yMax.equals(xyParam.yMax)) {
      return false;
    }
    return true;
  }
}
/**
 * <p> $Log$ </p>
 */
