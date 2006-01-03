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
  
  String name = null;
  int index = -1;
  
  AttributeLocation(int index) {
    this.index = index;
  }
  
  AttributeLocation(String name, int index) {
    this.name = name;
    this.index = index;
  }
  
  final String getName() {
    return name;
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
* <p> $Log$
* <p> Revision 1.1  2005/12/23 02:11:18  sueh
* <p> bug# 675 Location in an ordered list of attributes.
* <p> </p>
*/