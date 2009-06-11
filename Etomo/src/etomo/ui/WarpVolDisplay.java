package etomo.ui;

import etomo.comscript.WarpVolParam;

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
public interface WarpVolDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public boolean getParameters(WarpVolParam param);
}
