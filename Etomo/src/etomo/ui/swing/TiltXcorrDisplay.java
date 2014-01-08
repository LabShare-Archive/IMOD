package etomo.ui.swing;

import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.ImodchopcontsParam;
import etomo.comscript.TiltxcorrParam;
import etomo.type.PanelId;

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
 * <p> Revision 1.2  2010/03/03 05:09:27  sueh
 * <p> bug# 1311 Added getPanelId.
 * <p>
 * <p> Revision 1.1  2009/06/12 19:51:02  sueh
 * <p> bug# 1221 Interface for a display containing the tiltxcorr parameters.
 * <p> </p>
 */
public interface TiltXcorrDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(TiltxcorrParam tiltXcorrParams, boolean doValidation)
      throws FortranInputSyntaxException;

  public PanelId getPanelId();

  public boolean getParameters(ImodchopcontsParam param, boolean doValidation);
}
