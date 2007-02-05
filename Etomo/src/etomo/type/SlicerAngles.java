package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
* <p> Revision 1.3  2006/04/06 20:15:02  sueh
* <p> bug# 808 Turned x, y, and z into EtomoNumbers.  Added load and store.
* <p>
* <p> Revision 1.2  2004/11/19 23:40:04  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/09/22 22:08:37  sueh
* <p> bug# 520 Added a class to store slicer angles.
* <p> </p>
*/
public final class SlicerAngles {
  public static  final String  rcsid =  "$Id$";
  
  private static final String NAME = "SlicerAngles";
  
  private final EtomoNumber x = new EtomoNumber(EtomoNumber.Type.DOUBLE, "X");
  private final EtomoNumber y = new EtomoNumber(EtomoNumber.Type.DOUBLE, "Y");
  private final EtomoNumber z = new EtomoNumber(EtomoNumber.Type.DOUBLE, "Z");
  
  public boolean isComplete() {
    return !x.isNull() && !y.isNull() && !z.isNull();
  }
  
  public void add(String angle) throws NumberFormatException {
    if (isComplete()) {
      return;
    }
    if (x.isNull()) {
      x.set(angle);
    }
    else if (y.isNull()) {
      y.set(angle);
    }
    else {
      z.set(angle);
    }
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    x.store(props, prepend);
    y.store(props, prepend);
    z.store(props, prepend);
  }
  
  public void load(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    x.load(props, prepend);
    y.load(props, prepend);
    z.load(props, prepend);
  }
  
  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return NAME;
    }
    return prepend + "." + NAME;
  }
  
  public boolean isEmpty() {
    return x.isNull() && y.isNull() && z.isNull();
  }
  
  public void setX(ConstEtomoNumber x) {
    this.x.set(x);
  }
  
  public void setY(ConstEtomoNumber y) {
    this.y.set(y);
  }
  
  public void setZ(ConstEtomoNumber z) {
    this.z.set(z);
  }
  
  public ConstEtomoNumber getX() {
    return x;
  }
  
  public ConstEtomoNumber getY() {
    return y;
  }
  
  public ConstEtomoNumber getZ() {
    return z;
  }
  
  public String toString() {
    return "x=" + x + ",y=" + y + ",z=" + z;
  }
  
}
