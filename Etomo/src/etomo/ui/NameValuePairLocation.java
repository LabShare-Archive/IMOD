package etomo.ui;

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
class NameValuePairLocation {
  public static  final String  rcsid =  "$Id$";

  private int index = 0;
  
  NameValuePairLocation() {
  }
  
  final int getIndex() {
    return index;
  }
  
  final void setIndex(int index) {
    this.index = index;
  }
  
  final void increment() {
    index++;
  }
  
  final boolean isOutOfRange(Vector list) {
    if (list == null) {
      return true;
    }
    return index >= list.size();
  }
}
/**
* <p> $Log$ </p>
*/