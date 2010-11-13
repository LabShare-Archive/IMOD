package etomo.ui.swing;

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
* <p> $Log$
* <p> Revision 1.1  2009/06/11 16:52:21  sueh
* <p> bug# 1221 Interface to a display which contains flattenwarp parameters.
* <p> </p>
*/
public interface FlattenWarpDisplay {
  public static  final String  rcsid =  "$Id$";
  
 public boolean getParameters(FlattenWarpParam param);
}
