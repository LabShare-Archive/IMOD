package etomo.ui;

/**
* <p>Description:</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$$ </p>
*/

public class SectionLocation {
  public static final String rcsid = "$$Id$$";
  String type = null;
  int index = -1;
  
  SectionLocation(String type, int index) {
    this.type = type;
    this.index = index;
  }
  
  public String getType() {
    return type;
  }
  
  public int getIndex() {
    return index;
  }
  
  public void setIndex(int index) {
    this.index = index;
  }
}
