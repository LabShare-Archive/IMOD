package etomo.type;
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
* <p> Revision 1.1.2.1  2004/09/22 22:08:37  sueh
* <p> bug# 520 Added a class to store slicer angles.
* <p> </p>
*/
public class SlicerAngles {
  public static  final String  rcsid =  "$Id$";
  double x = Double.NaN;
  double y = Double.NaN;
  double z = Double.NaN;
  
  public boolean isComplete() {
    return !Double.isNaN(x) && !Double.isNaN(y) && !Double.isNaN(z);
  }
  
  public void add(String angleString) throws NumberFormatException {
    if (isComplete()) {
      return;
    }
    double angle = Double.parseDouble(angleString);
    if (Double.isNaN(x)) {
      x = angle;
    }
    else if (Double.isNaN(y)) {
      y = angle;
    }
    else {
      z = angle;
    }
  }
  
  public boolean isEmpty() {
    return Double.isNaN(x) && Double.isNaN(y) && Double.isNaN(z);
  }
  
  public String getXText() {
    if (x == Double.NaN) {
      return "";
    }
    return Double.toString(x);
  }
  
  public String getYText() {
    if (y == Double.NaN) {
      return "";
    }
    return Double.toString(y);
  }
  
  public String getZText() {
    if (z == Double.NaN) {
      return "";
    }
    return Double.toString(z);
  }
  
  public String toString() {
    return "x=" + x + ",y=" + y + ",z=" + z;
  }
  
}
