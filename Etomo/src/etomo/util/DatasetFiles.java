package etomo.util;

import java.io.File;

import etomo.BaseManager;
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

  private static final String TOMO_EXT = ".rec";
  private static final String STACK_EXT = ".st";
  
  //Stacks
  
  public final static File getOriginalStack(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + "_orig" + STACK_EXT);
  }
  
  public final static File getStack(BaseManager manager, AxisID axisID) {
    return getStack(manager.getPropertyUserDir(), manager.getBaseMetaData(), axisID);
  }
  
  public final static File getStack(String propertyUserDir,
      BaseMetaData metaData, AxisID axisID) {
    return new File(propertyUserDir, getStackName(metaData, axisID));
  }
  
  public final static String getStackName(BaseManager manager, AxisID axisID) {
    return getStackName(manager.getBaseMetaData(), axisID);
  }
  
  private final static String getStackName(BaseMetaData metaData, AxisID axisID) {
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + STACK_EXT;
  }

  //Tomograms
  
  public final static File getTomogram(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    if (axisID == AxisID.ONLY) {
      return new File(manager.getPropertyUserDir(), metaData.getName() + "_full" + TOMO_EXT);
    }
    return new File(manager.getPropertyUserDir(), metaData.getName() + axisID.getExtension() + TOMO_EXT);
  }

  public final static File getCombinedTomogram(BaseManager manager) {
    if (manager.getBaseMetaData().getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), "sum" + TOMO_EXT);
  }
  
  public final static File getTrimmedTomogram(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName() + axisID.getExtension() + TOMO_EXT);
  }
  
  //Other files
  
  public final static File getCommandsFile(BaseManager manager, String rootName) {
    return new File(manager.getPropertyUserDir(), getCommandsFileName(rootName));
  }
  
  public final static String getCommandsFileName(String rootName) {
    return rootName + ".cmds";
  }
    
  public final static File getRawTilt(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getRawTiltName(manager, axisID));
  }
  
  public final static String getRawTiltName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".rawtlt";
  }
  
  public final static File getPieceListFile(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getPieceListFileName(manager, axisID));
  }
  
  public final static String getPieceListFileName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".pl";
  }
  
  public final static File getMagGradient(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getMagGradientName(manager, axisID));
  }
  
  public final static String getMagGradientName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".maggrad";
  }
  
  //private
  
  private final static AxisID correctAxisID(BaseMetaData metaData, AxisID axisID) {
    AxisType axisType = metaData.getAxisType();
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      return AxisID.FIRST;
    }
    else if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.FIRST) {
      return AxisID.ONLY;
    }
    else if (axisType == null || axisType == AxisType.NOT_SET) {
      throw new IllegalStateException(
          "AxisType is not set.  AxisType must be set before getting a dataset file name containing the axisID extension.");
    }
    return axisID;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2005/10/19 00:21:18  sueh
 * <p> bug# 673 Added getOriginalStack() to return the File object for the
 * <p> original stack (_orig.st).
 * <p>
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