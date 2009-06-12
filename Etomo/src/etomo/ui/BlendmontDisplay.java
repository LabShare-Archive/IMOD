package etomo.ui;

import java.io.IOException;

import etomo.comscript.BlendmontParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.util.InvalidParameterException;

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
public interface BlendmontDisplay {
  public static final String rcsid = "$Id$";

  public void getParameters(BlendmontParam blendmontParam)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException;
}
