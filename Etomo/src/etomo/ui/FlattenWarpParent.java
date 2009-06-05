package etomo.ui;

import etomo.type.FileType;

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
* <p> $Log$ </p>
*/
interface FlattenWarpParent {
  public static  final String  rcsid =  "$Id$";
  
  public FileType getFileTypeForSurfaceModel();
}
