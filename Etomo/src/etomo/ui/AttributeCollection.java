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
* <p> $$Log$
* <p> $Revision 1.1  2004/01/01 00:43:20  sueh
* <p> $bug# 372 correct interface name, was AttributeInterface
* <p> $$ </p>
*/

interface AttributeCollection {
  public static final String rcsid = "$$Id$$";
  
  AttributeCollection addAttribute(Token name);
}
