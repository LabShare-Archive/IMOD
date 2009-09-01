package etomo.ui;

import etomo.type.ImageFileType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.1  2009/06/05 02:12:40  sueh
* <p> bug# 1219 An interface for the class that is responsible for
* <p> FlattenWarpPanel.
* <p> </p>
*/
interface FlattenWarpParent {
  public static  final String  rcsid =  "$Id$";
  
  public ImageFileType getFileTypeForSurfaceModel();
}
