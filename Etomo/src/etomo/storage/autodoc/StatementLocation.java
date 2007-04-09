package etomo.storage.autodoc;

import java.util.Vector;

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
public final class StatementLocation {
  public static  final String  rcsid =  "$Id$";

  private int index = 0;
  
  StatementLocation() {
  }
  
  int getIndex() {
    return index;
  }
  
  void setIndex(int index) {
    this.index = index;
  }
  
  void increment() {
    index++;
  }
  
  boolean isOutOfRange(Vector list) {
    if (list == null) {
      return true;
    }
    return index >= list.size();
  }
  
  public String toString() {
    return "index=" + index;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/04/25 18:54:42  sueh
* <p> bug# 787 Added toString().
* <p>
* <p> Revision 1.1  2006/01/12 17:03:12  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.1  2006/01/11 22:17:12  sueh
* <p> bug# The location of a NameValuePair in a Vector.
* <p> </p>
*/