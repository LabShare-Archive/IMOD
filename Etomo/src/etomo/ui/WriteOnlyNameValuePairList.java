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
interface WriteOnlyNameValuePairList {
  public static  final String  rcsid =  "$Id$";
  
  void addNameValuePair(Attribute attrib, int valueIndex);
}
/**
* <p> $Log$ </p>
*/