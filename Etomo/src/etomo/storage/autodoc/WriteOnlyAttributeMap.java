package etomo.storage.autodoc;

import etomo.ui.Token;

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
abstract class WriteOnlyAttributeMap {
  public static final String rcsid = "$Id$";
  
  abstract WriteOnlyAttributeMap addAttribute(Token name);
  abstract boolean isGlobal();
  abstract boolean isAttribute();  
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/06/14 00:35:06  sueh
* <p> bug# 852 Added function isGlobal so that it is possible to tell whether an attribute
* <p> is global or part of a section.
* <p>
* <p> Revision 1.1  2006/01/12 17:03:46  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.1  2006/01/11 23:21:20  sueh
* <p> bug# 675 Renamed AttributeCollection to WriteOnlyAttributeMap for better
* <p> name flexibility.
* <p> </p>
*/
/**
* <p> Old Log: AttributeCollection.java
* <p> $Revision 1.2  2005/05/17 19:32:38  sueh
* <p> $bug# 372 Reducing the visibility of the interface and the abstract function.
* <p> $
* <p> $Revision 1.1  2004/01/01 00:43:20  sueh
* <p> $bug# 372 correct interface name, was AttributeInterface
* <p> $$ </p>
*/