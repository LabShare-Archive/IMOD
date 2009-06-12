package etomo.ui;

import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltxcorrParam;

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
public interface TiltXcorrDisplay extends ProcessDisplay {
  public static final String rcsid = "$Id$";

  public void getParameters(TiltxcorrParam tiltXcorrParams)
      throws FortranInputSyntaxException;
}
