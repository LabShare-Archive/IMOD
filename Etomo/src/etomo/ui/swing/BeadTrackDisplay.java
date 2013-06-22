package etomo.ui.swing;

import etomo.comscript.BeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.InvalidEtomoNumberException;

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
 * <p> Revision 1.1  2009/06/15 20:13:38  sueh
 * <p> bug# 1221 Interface for a display containing the beadtrack parameters.
 * <p> </p>
 */
public interface BeadTrackDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(BeadtrackParam beadtrackParams, boolean doValidation)
      throws FortranInputSyntaxException, InvalidEtomoNumberException;
}
