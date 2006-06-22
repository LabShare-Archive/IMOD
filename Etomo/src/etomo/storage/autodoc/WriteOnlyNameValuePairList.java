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
abstract class WriteOnlyNameValuePairList extends WriteOnlyAttributeMap {
  public static  final String  rcsid =  "$Id$";
  
  abstract void addNameValuePair(Attribute attrib, int valueIndex);
  abstract Section addSection(Token type, Token name);
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/01/12 17:03:56  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.1  2006/01/11 23:22:02  sueh
* <p> bug# 675 A generic way to add name/value pairs to Autodoc's and Section's.
* <p> </p>
*/