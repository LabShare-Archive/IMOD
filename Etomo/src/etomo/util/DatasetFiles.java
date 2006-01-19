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
  private static final String ROTATED_TOMO_EXT = ".rot";
  private static final String STACK_EXT = ".st";

  //Stacks

  public final static File getOriginalStack(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + "_orig" + STACK_EXT);
  }

  public final static File getStack(BaseManager manager, AxisID axisID) {
    return getStack(manager.getPropertyUserDir(), manager.getBaseMetaData(),
        axisID);
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

  public final static String getStackName(String dataset, AxisType axisType,
      AxisID axisID) {
    axisID = correctAxisID(axisType, axisID);
    return dataset + axisID.getExtension() + STACK_EXT;
  }

  public final static String getSeedName(String dataset, AxisType axisType,
      AxisID axisID) {
    axisID = correctAxisID(axisType, axisID);
    return dataset + axisID.getExtension() + ".seed";
  }

  //Tomograms

  public final static File getTomogram(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    if (axisID == AxisID.ONLY) {
      return new File(manager.getPropertyUserDir(), metaData.getName()
          + "_full" + TOMO_EXT);
    }
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + TOMO_EXT);
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
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + TOMO_EXT);
  }

  public final static boolean isRotatedTomogram(File tomogram) {
    String tomogramName = tomogram.getName();
    if (tomogramName.substring(tomogramName.lastIndexOf('.')).equals(
        ROTATED_TOMO_EXT)) {
      return true;
    }
    return false;
  }

  public final static File getRotatedTomogram(BaseManager manager, File tomogram) {
    String tomogramName = tomogram.getName();
    return new File(manager.getPropertyUserDir(), tomogramName.substring(0,
        tomogramName.lastIndexOf('.')) + ROTATED_TOMO_EXT);
  }

  //Other dataset files

  public final static File getRawTilt(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getRawTiltName(manager,
        axisID));
  }

  public final static String getRawTiltName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".rawtlt";
  }

  public final static File getPieceListFile(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getPieceListFileName(manager,
        axisID));
  }

  public final static String getPieceListFileName(BaseManager manager,
      AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".pl";
  }

  public final static File getMagGradient(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getMagGradientName(manager,
        axisID));
  }

  public final static String getMagGradientName(BaseManager manager,
      AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".maggrad";
  }

  //other etomo files

  public final static File getCommandsFile(BaseManager manager, String rootName) {
    return new File(manager.getPropertyUserDir(), getCommandsFileName(rootName));
  }

  public final static String getCommandsFileName(String rootName) {
    return rootName + ".cmds";
  }

  public final static File getAutodoc(File dir, String name) {
      return new File(dir, getAutodocName(name));
  }

  final static String getAutodocName(String name) {
    return name + ".adoc";
  }

  public final static File getShellScript(BaseManager manager,
      String commandName, AxisID axisID) {
    axisID = correctAxisID(manager.getBaseMetaData(), axisID);
    return new File(manager.getPropertyUserDir(), commandName
        + axisID.getExtension() + ".csh");
  }

  public final static File getOutFile(BaseManager manager, String commandName,
      AxisID axisID) {
    axisID = correctAxisID(manager.getBaseMetaData(), axisID);
    return new File(manager.getPropertyUserDir(), commandName
        + axisID.getExtension() + ".out");
  }

  //private

  private final static AxisID correctAxisID(BaseMetaData metaData, AxisID axisID) {
    return correctAxisID(metaData.getAxisType(), axisID);
  }
  
  private final static AxisID correctAxisID(AxisType axisType, AxisID axisID) {
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      return AxisID.FIRST;
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.FIRST) {
      return AxisID.ONLY;
    }
    if (axisType == null || axisType == AxisType.NOT_SET) {
      throw new IllegalStateException(
          "AxisType is not set.  AxisType must be set before getting a dataset file name containing the axisID extension.");
    }
    return axisID;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.9  2005/12/23 02:27:09  sueh
 * <p> bug# 675 Added getStackName(String dataset, AxisType, AxisID) and
 * <p> getSeedName(String dataset, AxisType, AxisID) to create file names
 * <p> without running etomo.
 * <p>
 * <p> Revision 1.8  2005/11/29 22:53:59  sueh
 * <p> bug# 757 Added getRotated() and isRotated().
 * <p>
 * <p> Revision 1.7  2005/11/19 02:45:37  sueh
 * <p> bug# 744 Added getOutFile and getShellScript.
 * <p>
 * <p> Revision 1.6  2005/11/10 18:17:38  sueh
 * <p> bug# 733 Added getAutodoc and getAutodocName
 * <p>
 * <p> Revision 1.5  2005/10/27 00:37:37  sueh
 * <p> bug# 725 Added functions:  private correctAxisID, getMagGradientName,
 * <p> getPieceListFile, getPieceListFileName, getRawTilt, getRawTiltName,
 * <p> getStack, getStackName.
 * <p>
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