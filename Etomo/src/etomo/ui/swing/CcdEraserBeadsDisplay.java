package etomo.ui.swing;

import etomo.comscript.CCDEraserParam;

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
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/06/16 22:45:48  sueh
 * <p> bug# 1221 Changed the name.
 * <p>
 * <p> Revision 1.1  2009/06/11 16:50:15  sueh
 * <p> bug# 1221 Interface to a display which contains ccderaser parameters.
 * <p> </p>
 */
public interface CcdEraserBeadsDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(final CCDEraserParam ccdEraserParams);
}
