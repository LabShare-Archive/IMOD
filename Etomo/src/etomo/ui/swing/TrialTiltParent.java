package etomo.ui.swing;

import java.io.IOException;

import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
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
 * <p> $Log$
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
interface TrialTiltParent {
  public static final String rcsid = "$Id$";

  public boolean getParameters(TiltParam tiltParam)
      throws NumberFormatException, InvalidParameterException, IOException;

  public boolean getParameters(final SplittiltParam param);
  public boolean isParallelProcess();
}
