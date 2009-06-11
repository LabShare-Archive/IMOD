package etomo.ui;

import etomo.comscript.FlattenWarpParam;

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
public interface FlattenWarpDisplay {
  public static  final String  rcsid =  "$Id$";
  
 public boolean getParameters(FlattenWarpParam param);
}
