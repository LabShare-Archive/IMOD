package etomo.ui.swing;

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
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 18:02:47  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/06/12 19:47:05  sueh
 * <p> bug# 1221 Interface for a display containing the blendmont parameters.
 * <p> </p>
 */
public interface BlendmontDisplay {
  public static final String rcsid = "$Id$";

  public boolean getParameters(BlendmontParam blendmontParam, boolean doValidation)
      throws FortranInputSyntaxException, InvalidParameterException, IOException;

  public void setParameters(BlendmontParam param);

  public boolean validate();
}
