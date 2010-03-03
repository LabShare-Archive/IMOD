package etomo.ui;

import etomo.comscript.FortranInputSyntaxException;
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
 * <p> Revision 1.1  2009/06/12 19:51:02  sueh
 * <p> bug# 1221 Interface for a display containing the tiltxcorr parameters.
 * <p> </p>
 */
public interface TiltXcorrDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(TiltxcorrParam tiltXcorrParams)
      throws FortranInputSyntaxException;

  public PanelId getPanelId();
}
