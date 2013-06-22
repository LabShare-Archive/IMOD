package etomo.ui.swing;

import etomo.comscript.CCDEraserParam;
import etomo.comscript.MakecomfileParam;

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
 * <p> Revision 1.1  2009/06/16 22:45:48  sueh
 * <p> bug# 1221 Changed the name.
 * <p>
 * <p> Revision 1.1  2009/06/11 16:50:15  sueh
 * <p> bug# 1221 Interface to a display which contains ccderaser parameters.
 * <p> </p>
 */
public interface CcdEraserDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(CCDEraserParam ccdEraserParams, boolean doValidation);

  public boolean getParameters(MakecomfileParam param, boolean doValidation);
}
