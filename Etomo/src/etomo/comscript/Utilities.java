package etomo.comscript;

import java.io.IOException;

import etomo.BaseManager;
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
 * <p> Revision 1.1  2008/12/15 23:01:50  sueh
 * <p> bug# 1161 Added Utilities functions is90DegreeImageRotation and
 * <p> getGoodframeFromMontageSize.
 * <p> </p>
 */
final class Utilities {
  public static final String rcsid = "$Id$";

  static boolean is90DegreeImageRotation(float imageRotation) {
    return (imageRotation > 45 && imageRotation < 135)
        || (imageRotation < -45 && imageRotation > -135);
  }

  static Goodframe getGoodframeFromMontageSize(AxisID axisID,
      BaseManager manager) {
    Montagesize montagesize = Montagesize.getInstance(manager, axisID, manager
        .getManagerKey());
    try {
      montagesize.read();
      if (montagesize.isFileExists()) {
        Goodframe goodframe = new Goodframe(manager.getPropertyUserDir(),
            axisID, manager.getManagerKey());
        goodframe.run(montagesize.getX().getInt(), montagesize.getY().getInt());
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
