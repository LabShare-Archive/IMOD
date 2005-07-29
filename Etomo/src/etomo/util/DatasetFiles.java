package etomo.util;

import java.io.File;

import etomo.BaseManager;
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

  public final static File getTomogram(BaseManager manager, AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      return Utilities.getFile(manager, axisID, "_full.rec");
    }
    return Utilities.getFile(manager, axisID, ".rec");
  }

  public final static File getCombinedTomogram(BaseManager manager) {
    if (manager.getBaseMetaData().getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return Utilities.getFile(manager.getPropertyUserDir(), "sum.rec");
  }

  public final static File getTrimmedTomogram(BaseManager manager) {
    EtomoDirector director = EtomoDirector.getInstance();
    File file = new File(manager.getPropertyUserDir(), manager.getBaseMetaData().getName() + ".rec");
    return file;
  }
  
  public final static File getTrimmedTomogram(BaseManager manager, AxisID axisID) {
    if (manager.getBaseMetaData().getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return Utilities.getFile(manager.getPropertyUserDir(), ".rec");
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2005/07/20 17:55:37  sueh
 * <p> bug# 700 Class containing static functions which can return all the
 * <p> different files of files in a dataset by description and encapsulates the
 * <p> details of how to create them.  Dependent on EtomoDirector.
 * <p> getCurrentManager().
 * <p> </p>
 */