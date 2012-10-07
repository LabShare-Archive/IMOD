package etomo.ui.swing;

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
* <p> $Log$
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.1  2009/06/11 17:02:26  sueh
* <p> bug# 1221 Interface to a display which contains warpvol parameters.
* <p> </p>
*/
public interface WarpVolDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(WarpVolParam param, boolean doValidation);
}
