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

public interface AttributeCollection {
  public static final String rcsid = "$$Id$$";
  
  public AttributeCollection addAttribute(Token name);
}
