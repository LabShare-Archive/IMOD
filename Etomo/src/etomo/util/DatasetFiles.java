package etomo.util;

import java.io.File;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;

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
  
  public final static File getCommandsFile(BaseManager manager, String rootName) {
    return new File(manager.getPropertyUserDir(), getCommandsFileName(rootName));
  }
  
  public final static String getCommandsFileName(String rootName) {
    return rootName + ".cmds";
  }
  
  public final static File getOriginalStack(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    AxisType axisType = metaData.getAxisType();
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    else if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.FIRST) {
      axisID = AxisID.ONLY;
    }
    else if (axisType == AxisType.NOT_SET) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + "_orig.st");
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2005/09/07 20:53:26  sueh
 * <p> bug# 532 Added functions to get a commands file (.cmds).
 * <p>
 * <p> Revision 1.2  2005/07/29 00:55:00  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.1  2005/07/20 17:55:37  sueh
 * <p> bug# 700 Class containing static functions which can return all the
 * <p> different files of files in a dataset by description and encapsulates the
 * <p> details of how to create them.  Dependent on EtomoDirector.
 * <p> getCurrentManager().
 * <p> </p>
 */