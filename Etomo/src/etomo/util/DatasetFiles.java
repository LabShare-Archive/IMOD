package etomo.util;

import java.io.File;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class DatasetFiles {
  public static final String rcsid = "$Id$";

  public final static File getTomogram(AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      return Utilities.getFile(axisID, "_full.rec");
    }
    return Utilities.getFile(axisID, ".rec");
  }

  public final static File getCombinedTomogram() {
    if (EtomoDirector.getInstance().getCurrentManager().getBaseMetaData()
        .getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return Utilities.getFile("sum.rec");
  }

  public final static File getTrimmedTomogram() {
    EtomoDirector director = EtomoDirector.getInstance();
    File file = new File(director.getCurrentPropertyUserDir(), director
        .getCurrentName() + ".rec");
    return file;
  }
  
  public final static File getTrimmedTomogram(AxisID axisID) {
    if (EtomoDirector.getInstance().getCurrentManager().getBaseMetaData()
        .getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return Utilities.getFile(".rec");
  }
}
/**
 * <p> $Log$ </p>
 */