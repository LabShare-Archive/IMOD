package etomo.ui;
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
final class AttributeLocation {
  public static  final String  rcsid =  "$Id$";
  
  int index = -1;
  
  AttributeLocation(int index) {
    this.index = index;
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
}
/**
* <p> $Log$ </p>
*/