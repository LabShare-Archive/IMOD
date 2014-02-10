package etomo.comscript;

import java.io.IOException;

import etomo.BaseManager;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.util.Goodframe;
import etomo.util.InvalidParameterException;
import etomo.util.Montagesize;

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
 * <p> Revision 1.4  2011/02/22 03:38:32  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2009/03/17 00:33:25  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.1  2008/12/15 23:01:50  sueh
 * <p> bug# 1161 Added Utilities functions is90DegreeImageRotation and
 * <p> getGoodframeFromMontageSize.
 * <p> </p>
 */
public final class Utilities {
  public static final String rcsid = "$Id$";

  public static final String MONTAGE_SEPARATION = "-10";

  static boolean is90DegreeImageRotation(double imageRotation) {
    return (imageRotation > 45 && imageRotation < 135)
        || (imageRotation < -45 && imageRotation > -135);
  }

  static Goodframe getGoodframeFromMontageSize(AxisID axisID, BaseManager manager) {
    Montagesize montagesize = Montagesize.getInstance(manager, axisID,
        DatasetTool.STANDARD_DATASET_EXT);
    try {
      montagesize.read(manager);
      if (montagesize.isFileExists()) {
        Goodframe goodframe = new Goodframe(manager.getPropertyUserDir(), axisID);
        goodframe.run(manager, montagesize.getX().getInt(), montagesize.getY().getInt());
        return goodframe;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return null;
  }
}
