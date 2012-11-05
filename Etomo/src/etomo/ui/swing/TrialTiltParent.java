package etomo.ui.swing;

import java.io.IOException;

import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.type.ProcessingMethod;
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
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
interface TrialTiltParent {
  public static final String rcsid = "$Id$";

  public boolean getParameters(TiltParam tiltParam, boolean doValidation)
      throws NumberFormatException, InvalidParameterException, IOException;

  public boolean getParameters(final SplittiltParam param,boolean doValidation);

  public ProcessingMethod getProcessingMethod();
}
