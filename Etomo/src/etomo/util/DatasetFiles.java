package etomo.util;

import java.io.File;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.ProcessName;

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

  public static final String MATLAB_PARAM_FILE_EXT=".prm";
  public static final String PARALLEL_DATA_FILE_EXT = ".epp";
  public static final String PEET_DATA_FILE_EXT = ".epe";
  public static final String ROTATED_TOMO_EXT = ".rot";
  public static final String COMSCRIPT_EXT = ".com";
  public static final String TOMO_EXT = ".rec";
  private static final String STACK_EXT = ".st";
  public static final String MODEL_EXT = ".mod";
  private static final String PATCH_VECTOR_STRING = "patch_vector";
  public static final String PATCH_VECTOR_MODEL = PATCH_VECTOR_STRING
      + MODEL_EXT;
  public static final String PATCH_VECTOR_CCC_MODEL = PATCH_VECTOR_STRING
      + "_ccc" + MODEL_EXT;
  public static final String LOG_EXT = ".log";
  public static final char BACKUP_CHAR = '~';
  public static final String TRANSFER_FID_LOG = "transferfid.log";
  public static final String PATCH_OUT = "patch.out";
  public static final String VOLCOMBINE_START_LOG = ProcessName.VOLCOMBINE
      .toString()
      + "-start" + LOG_EXT;
  public static final String VOLCOMBINE_FINISH_LOG = ProcessName.VOLCOMBINE
      .toString()
      + "-finish" + LOG_EXT;
  public static final String JOIN_EXT = ".join";
  public static final String REFINE_NAME = "_refine";
  public static final String XFJOINTOMO_LOG = "xfjointomo"+LOG_EXT;
  private static final String XG_EXT = ".xg";

  private static File calibrationDir = null;
  private static File distortionDir = null;

  //Stacks

  public static File getOriginalStack(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), manager.getName()
        + axisID.getExtension() + "_orig" + STACK_EXT);
  }

  public static File getStack(BaseManager manager, AxisID axisID) {
    return getStack(manager.getPropertyUserDir(), manager.getBaseMetaData(),
        axisID);
  }

  public static File getStack(String propertyUserDir, BaseMetaData metaData,
      AxisID axisID) {
    return new File(propertyUserDir, getStackName(metaData, axisID));
  }

  public static String getStackName(BaseManager manager, AxisID axisID) {
    return getStackName(manager.getBaseMetaData(), axisID);
  }

  private static String getStackName(BaseMetaData metaData, AxisID axisID) {
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + STACK_EXT;
  }

  public static String getStackName(String dataset, AxisType axisType,
      AxisID axisID) {
    axisID = correctAxisID(axisType, axisID);
    return dataset + axisID.getExtension() + STACK_EXT;
  }

  public static File getSeedFile(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + ".seed");
  }

  //Tomograms

  public static String getTomogramName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    if (axisID == AxisID.ONLY) {
      return metaData.getName() + "_full" + TOMO_EXT;
    }
    return metaData.getName() + axisID.getExtension() + TOMO_EXT;
  }

  public static File getTomogram(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getTomogramName(manager,
        axisID));
  }

  public static File getCombinedTomogram(BaseManager manager) {
    if (manager.getBaseMetaData().getAxisType() != AxisType.DUAL_AXIS) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), "sum" + TOMO_EXT);
  }

  public static File getTrimmedTomogram(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + TOMO_EXT);
  }

  public static boolean isRotatedTomogram(File tomogram) {
    String tomogramName = tomogram.getName();
    if (tomogramName.substring(tomogramName.lastIndexOf('.')).equals(
        ROTATED_TOMO_EXT)) {
      return true;
    }
    return false;
  }

  public static File getRotatedTomogram(BaseManager manager, File tomogram) {
    String tomogramName = tomogram.getName();
    return new File(manager.getPropertyUserDir(), tomogramName.substring(0,
        tomogramName.lastIndexOf('.'))
        + ROTATED_TOMO_EXT);
  }

  //Other dataset files

  public static String getLogName(BaseManager manager, AxisID axisID,
      ProcessName processName) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return processName.toString() + axisID.getExtension() + LOG_EXT;
  }

  public static File getAxisOnlyComFile(BaseManager manager,
      ProcessName processName) {
    BaseMetaData metaData = manager.getBaseMetaData();
    return new File(manager.getPropertyUserDir(), processName.toString()
        + COMSCRIPT_EXT);
  }

  public static File getDatasetFile(BaseManager manager, AxisID axisID,
      String filename) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return new File(manager.getPropertyUserDir(), metaData.getName()
        + axisID.getExtension() + filename);
  }

  public static File getRawTilt(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getRawTiltName(manager,
        axisID));
  }

  public static String getRawTiltName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".rawtlt";
  }

  public static String getFiducialModelName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".fid";
  }
  
  public static String getXTiltFileName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".xtilt";
  }

  public static String getJoinInfoName(BaseManager manager) {
    return manager.getName() + ".info";
  }

  public static File getFiducialModelFile(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getFiducialModelName(manager,
        axisID));
  }

  public static File getPieceListFile(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getPieceListFileName(manager,
        axisID));
  }

  public static String getPieceListFileName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".pl";
  }

  public static File getMagGradient(BaseManager manager, AxisID axisID) {
    return new File(manager.getPropertyUserDir(), getMagGradientName(manager,
        axisID));
  }

  public static String getJoinFileName(boolean trial, BaseManager manager) {
    return manager.getBaseMetaData().getName()
        + (trial ? "_trial" : "") + JOIN_EXT;
  }

  public static File getJoinFile(boolean trial, BaseManager manager) {
    return new File(manager.getPropertyUserDir(), getJoinFileName(trial,
        manager));
  }
  
  public static String getRefineXfFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName()+REFINE_NAME+".xf";
  }
  
  public static String getRefineXgFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName()+REFINE_NAME+XG_EXT;
  }
  
  public static File getRefineXgFile(BaseManager manager) {
    return new File(manager.getPropertyUserDir(),getRefineXgFileName(manager));
  }

  public static String getRefineJoinXgFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName()+REFINE_NAME+"join"+XG_EXT;
  }
  
  public static String getModeledJoinFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName() + "_modeled" + JOIN_EXT;
  }

  public static File getModeledJoinFile(JoinManager manager) {
    return new File(manager.getPropertyUserDir(),
        getModeledJoinFileName(manager));
  }

  public static String getRefineModelFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName() + REFINE_NAME + MODEL_EXT;
  }
  
  public static String getRefineAlignedModelFileName(BaseManager manager) {
    return manager.getBaseMetaData().getName() + REFINE_NAME + ".alimod";
  }
  
  public static File getRefineAlignedModelFile(BaseManager manager) {
    return new File(manager.getPropertyUserDir(),getRefineAlignedModelFileName(manager));
  }

  public static File getRefineModelFile(JoinManager manager) {
    return new File(manager.getPropertyUserDir(),
        getRefineModelFileName(manager));
  }

  public static String getMagGradientName(BaseManager manager, AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    axisID = correctAxisID(metaData, axisID);
    return metaData.getName() + axisID.getExtension() + ".maggrad";
  }

  public static String getTransferFidCoordFileName() {
    return "transferfid.coord";
  }

  public static File getTransferFidCoordFile(BaseManager manager) {
    return new File(manager.getPropertyUserDir(), getTransferFidCoordFileName());
  }

  public static File getPatchVectorModel(BaseManager manager) {
    return new File(manager.getPropertyUserDir(), PATCH_VECTOR_MODEL);
  }

  //other etomo files

  public static File getCommandsFile(BaseManager manager, String rootName) {
    return new File(manager.getPropertyUserDir(), getCommandsFileName(rootName));
  }

  public static String getCommandsFileName(String rootName) {
    return rootName + ".cmds";
  }

  public static File getAutodoc(File dir, String name) {
    return new File(dir, getAutodocName(name));
  }

  final static String getAutodocName(String name) {
    return name + ".adoc";
  }

  public static File getShellScript(BaseManager manager, String commandName,
      AxisID axisID) {
    axisID = correctAxisID(manager.getBaseMetaData(), axisID);
    return new File(manager.getPropertyUserDir(), commandName
        + axisID.getExtension() + ".csh");
  }

  public static File getOutFile(BaseManager manager, String commandName,
      AxisID axisID) {
    axisID = correctAxisID(manager.getBaseMetaData(), axisID);
    return new File(manager.getPropertyUserDir(), getOutFileName(manager,
        commandName, axisID));
  }

  public static String getOutFileName(BaseManager manager, String commandName,
      AxisID axisID) {
    axisID = correctAxisID(manager.getBaseMetaData(), axisID);
    return commandName + axisID.getExtension() + ".out";
  }

  //log files

  public static String getTomopitchLogFileName(BaseManager manager,
      AxisID axisID) {
    return ProcessName.TOMOPITCH.toString()
        + correctAxisID(manager.getBaseMetaData(), axisID).getExtension()
        + LOG_EXT;
  }

  //com scripts

  public static File getCombineCom(BaseManager manager) {
    return new File(manager.getPropertyUserDir(), ProcessName.COMBINE
        + COMSCRIPT_EXT);
  }

  //directories

  public static File getCalibrationDir(String propertyUserDir, AxisID axisID) {
    if (calibrationDir == null) {
      String calibDirVar = EnvironmentVariable.INSTANCE.getValue(
          propertyUserDir, EnvironmentVariable.CALIB_DIR, axisID);
      if (!calibDirVar.equals("")) {
        calibrationDir = new File(calibDirVar);
      }
    }
    return calibrationDir;
  }

  public static File getDistortionDir(String propertyUserDir, AxisID axisID) {
    if (calibrationDir == null) {
      getCalibrationDir(propertyUserDir, axisID);
    }
    if (calibrationDir == null) {
      return null;
    }
    if (distortionDir == null) {
      distortionDir = new File(calibrationDir, "Distortion");
    }
    return distortionDir;
  }

  //private

  private static AxisID correctAxisID(BaseMetaData metaData, AxisID axisID) {
    return correctAxisID(metaData.getAxisType(), axisID);
  }

  private static AxisID correctAxisID(AxisType axisType, AxisID axisID) {
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

  public static String getParallelDataFileName(String rootName) {
    return rootName + PARALLEL_DATA_FILE_EXT;
  }
  
  public static String getRootName(File paramFile) {
    String rootFileName=paramFile.getName();
    int extensionIndex=rootFileName.indexOf('.');
    if (extensionIndex==-1) {
      return rootFileName;
    }
    return rootFileName.substring(0,extensionIndex);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.33  2007/03/01 01:47:08  sueh
 * <p> bug# 964 Added .prm file extension.
 * <p>
 * <p> Revision 1.32  2007/02/21 04:25:58  sueh
 * <p> bug# 964 Added getRootName() to strip off the extension of a file name and
 * <p> return the root.
 * <p>
 * <p> Revision 1.31  2007/02/19 22:04:30  sueh
 * <p> bug# 964 Added PEET interface data file extension (.epe).
 * <p>
 * <p> Revision 1.30  2007/02/05 23:46:51  sueh
 * <p> bug# 962 Added join model and rejoin file info.
 * <p>
 * <p> Revision 1.29  2006/12/02 04:59:55  sueh
 * <p> bug# 944 Add constants for volcombine-start.log and volcombine-finish.log.
 * <p>
 * <p> Revision 1.28  2006/10/13 22:31:25  sueh
 * <p> bug# 919 Deleted getJoinInfo().  Added getJoinInfoName().
 * <p>
 * <p> Revision 1.27  2006/10/11 10:13:31  sueh
 * <p> bug# 931 Managing the commands pipe and the process output with LogFile so
 * <p> that the file access problem which appears in Windows will show up in Linux.
 * <p> Added getOutFileName().
 * <p>
 * <p> Revision 1.26  2006/10/10 05:26:47  sueh
 * <p> bug# 931 Adding names of transfer log and patch out.
 * <p>
 * <p> Revision 1.25  2006/09/19 22:39:53  sueh
 * <p> bug# 928 Added PATCH_VECTOR_NAME, PATCH_VECTOR_CCC_NAME,
 * <p> and getPatchVectorModel().
 * <p>
 * <p> Revision 1.24  2006/09/14 00:06:20  sueh
 * <p> bug# 921 Added getTomogram()
 * <p>
 * <p> Revision 1.23  2006/08/18 00:13:26  sueh
 * <p> bug# 914 Added getAxisOnlyComFile.
 * <p>
 * <p> Revision 1.22  2006/08/11 21:47:52  sueh
 * <p> bug# 816 Added getLogName
 * <p>
 * <p> Revision 1.21  2006/08/02 22:28:15  sueh
 * <p> bug# 769 Added getLogFile()
 * <p>
 * <p> Revision 1.20  2006/07/21 22:27:57  sueh
 * <p> bug# 901 Removed test
 * <p>
 * <p> Revision 1.19  2006/07/21 22:24:19  sueh
 * <p> bug# 901 Added getCalibrationDir and getDistortionDir
 * <p>
 * <p> Revision 1.18  2006/05/23 23:08:28  sueh
 * <p> bug# 617 Change getFiducialModelFile() to return a file with a parent specified.
 * <p>
 * <p> Revision 1.17  2006/05/23 21:02:06  sueh
 * <p> bug# 617 Changed getFiducialModel() to getFiducialModelName(). Added
 * <p> getFiducialModelFile() and getSeedFile().  Removed unused function
 * <p> getSeedName.
 * <p>
 * <p> Revision 1.16  2006/05/19 19:54:09  sueh
 * <p> bug# 866 Added getDatasetFile()
 * <p>
 * <p> Revision 1.15  2006/05/16 21:39:22  sueh
 * <p> bug# 856 Added getCombineCom(), getFiducialMode(), and
 * <p> getTransferFidCoorFile().
 * <p>
 * <p> Revision 1.14  2006/05/12 18:25:04  sueh
 * <p> bug# 856 Added getTransferFidCoordFileName
 * <p>
 * <p> Revision 1.13  2006/05/11 19:59:45  sueh
 * <p> bug# 838 Added getTomopitchLogFileName().
 * <p>
 * <p> Revision 1.12  2006/04/06 20:34:13  sueh
 * <p> bug# 808 Added the comscript extension and the rotation file extension.
 * <p>
 * <p> Revision 1.11  2006/03/20 18:09:15  sueh
 * <p> bug# 835 Added a function to get the ParallelManager data file (.epp).
 * <p>
 * <p> Revision 1.10  2006/01/19 21:35:59  sueh
 * <p> bug# 757 The path of the rotated tomogram should be property user dir
 * <p>
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
