package etomo.logic;

import java.awt.Component;
import java.io.File;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;

import etomo.BaseManager;
import etomo.storage.DataFileFilter;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DataFileType;
import etomo.ui.swing.UIHarness;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class DatasetDirectory {
  public static final String rcsid = "$Id:$";

  private static final String MESSAGE_TITLE = "Invalid Dataset Directory";

  private DatasetDirectory() {
  }

  /**
   * Validates the dataset directory, including sharing.
   * @param manager - for popping up error message
   * @param axisID - for popping up error message
   * @param inputFile - input file (such as .st file) or the data file
   * @param dataFileType - type of the new dataset
   * @param axisType - axis type of the new dataset - only required for reconstructions
   * @return
   */
  public static boolean validateDatasetName(final BaseManager manager,
      final AxisID axisID, final File inputFile, final DataFileType dataFileType,
      final AxisType axisType) {
    String errorMessage = null;
    if (inputFile == null) {
      errorMessage = "No input file specified.";
    }
    else if (!inputFile.exists()) {
      errorMessage = "Input file does not exist: " + inputFile.getAbsolutePath();
    }
    else if (!inputFile.isFile()) {
      errorMessage = inputFile.getAbsolutePath() + " must be a file.";
    }
    else if (!inputFile.canRead()) {
      errorMessage = "Unreadable input file: " + inputFile.getAbsolutePath();
    }
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, MESSAGE_TITLE, axisID);
      return false;
    }
    String inputFileName = inputFile.getName();
    int extIndex = inputFileName.lastIndexOf('.');
    String inputFileRoot = inputFileName;
    if (extIndex != -1) {
      inputFileRoot = inputFileName.substring(0, extIndex);
    }
    File directory = inputFile.getParentFile();
    return validateDatasetName(manager, null, axisID, directory, inputFileRoot,
        dataFileType, axisType);
  }

  /**
   * Validates the dataset directory, including sharing.
   * @param manager - for popping up error message
   * @param axisID - for popping up error message
   * @param directory - directory in which the new dataset will be created
   * @param inputFileRoot - root name of the new dataset
   * @param dataFileType - type of the new dataset
   * @param axisType -  axis type of the new dataset - only required for reconstructions
   * @return
   */
  public static boolean validateDatasetName(final BaseManager manager,
      final AxisID axisID, final File directory, final String inputFileRoot,
      final DataFileType dataFileType, final AxisType axisType) {
    return validateDatasetName(manager, null, axisID, directory, inputFileRoot,
        dataFileType, axisType);
  }

  /**
   * Validates the dataset directory, including sharing.
   * @param manager - for popping up error message
   * @param axisID - for popping up error message
   * @param directory - directory in which the new dataset will be created
   * @param inputFileRoot - root name of the new dataset
   * @param dataFileType - type of the new dataset
   * @param axisType -  axis type of the new dataset - only required for reconstructions
   * @return
   */
  public static boolean validateDatasetName(final BaseManager manager,
      final Component parentComponent, final AxisID axisID, final File directory,
      final String inputFileRoot, final DataFileType dataFileType, final AxisType axisType) {
    String errorMessage = null;
    if (!directory.exists()) {
      errorMessage = "Directory does not exist: " + directory.getAbsolutePath();
    }
    else if (!directory.isDirectory()) {
      errorMessage = directory.getAbsolutePath() + " must be a directory.";
    }
    else if (!directory.canRead()) {
      errorMessage = "Unreadable directory: " + directory.getAbsolutePath();
    }
    else if (!directory.canWrite()) {
      errorMessage = "Unwritable directory: " + directory.getAbsolutePath();
    }
    else if (dataFileType == null) {
      errorMessage = "No data file type specified";
    }
    else {
      File[] fileList = directory.listFiles(new DataFileFilter());
      if (fileList == null || fileList.length == 0) {
        return true;
      }
      for (int i = 0; i < fileList.length; i++) {
        if (fileList[i] != null) {
          boolean canShare = true;
          if (dataFileType.hasAxisType) {
            AxisType fileAxisType = null;
            // If the existing data file does not have an axis type, the axis types
            // don't matter.
            if (DataFileType.getInstance(fileList[i].getName()).hasAxisType) {
              fileAxisType = AxisType.getInstance(LogFile.getLineContaining(fileList[i],
                  "Setup.AxisType"));
            }
            canShare = canShareWith(dataFileType, inputFileRoot, axisType,
                fileList[i].getName(), fileAxisType);
          }
          else {
            canShare = canShareWith(dataFileType, inputFileRoot, fileList[i].getName());
          }
          if (!canShare) {
            errorMessage = "Cannot create " + dataFileType + " dataset " + inputFileRoot
                + " in " + directory + " because " + fileList[i].getName()
                + " cannot share a directory with this new dataset.  Please select "
                + "another directory.";
            break;
          }
        }
      }
    }
    if (errorMessage == null) {
      return true;
    }
    if (parentComponent == null) {
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, MESSAGE_TITLE, axisID);
    }
    else {
      UIHarness.INSTANCE.openMessageDialog(manager, parentComponent, errorMessage,
          MESSAGE_TITLE, axisID);
    }
    return false;

  }

  /**
   * Returns true if newDataFileType can share a directory with another data file
   * (existingDataFileName).  This function cannot allow .edf files to share a directory.
   * Call reconCanShareWith to allow .edf file sharing.
   * @param newRoot - the root of the project to be created
   * @param existingDataFileName - a data file in the directory to be shared

   * @return
   */
  static boolean canShareWith(final DataFileType newDataFileType, final String newRoot,
      final String existingDataFileName) {
    if (newDataFileType.hasAxisType) {
      // handle incorrect data file type
      new InvalidParameterException("Warning: unable to share directories containing "
          + newDataFileType
          + " file types.  Wrong canShareWith called.  Calling correct canShareWith "
          + "without axis type information.").printStackTrace();
      return canShareWith(newDataFileType, newRoot, null, existingDataFileName, null);
    }
    // Get the type of the existing data file
    DataFileType existingDataFileType = DataFileType.getInstance(existingDataFileName);
    if (existingDataFileType == null) {
      // Its not a data file
      return true;
    }
    // Get existing data file root
    int extIndex = existingDataFileName.lastIndexOf('.');
    String root = existingDataFileName;
    if (extIndex != -1) {
      root = existingDataFileName.substring(0, extIndex);
    }
    if (newDataFileType == DataFileType.JOIN) {
      if (existingDataFileType == DataFileType.SERIAL_SECTIONS) {
        return false;
      }
      return true;
    }
    if (newDataFileType == DataFileType.PARALLEL) {
      return true;
    }
    if (newDataFileType == DataFileType.PEET) {
      if (existingDataFileType == DataFileType.PEET) {
        // Can share a PEET directory if the root is the same
        return root.equals(newRoot);
      }
      if (existingDataFileType == DataFileType.SERIAL_SECTIONS) {
        return false;
      }
      return true;
    }
    if (newDataFileType == DataFileType.SERIAL_SECTIONS) {
      if (existingDataFileType == DataFileType.RECON
          || existingDataFileType == DataFileType.JOIN
          || existingDataFileType == DataFileType.PEET) {
        return false;
      }
      if (existingDataFileType == DataFileType.SERIAL_SECTIONS) {
        // Can share a SERIAL_SECTIONS directory if the root is the same
        return root.equals(newRoot);
      }
      return true;
    }
    if (newDataFileType == DataFileType.TOOLS) {
      if (existingDataFileType == DataFileType.RECON
          || existingDataFileType == DataFileType.JOIN
          || existingDataFileType == DataFileType.PEET
          || existingDataFileType == DataFileType.SERIAL_SECTIONS) {
        // Can share a major project directory, if the root is different
        return !root.equals(newRoot);
      }
      return true;
    }
    return false;
  }

  /**
   * CanShareWith function for DataFileTypes that have an axis type (.edf).
   * Returns true if newDataFileType can share a directory with another data file 
   * (existingDataFileName).  The Axis Type parameters can be null if 
   * existingDataFileName is not an .edf file.
   * @param inputFile - an input file of the project to be created
   * @param newAxisType AxisType - axis type of the new dataset
   * @param existingDataFileName - a data file in the directory to be shared
   * @param existingAxisType - axis type of the existingDataFileName data file
   * @return
   */
  static boolean canShareWith(final DataFileType newDataFileType, String newRoot,
      final AxisType newAxisType, final String existingDataFileName,
      final AxisType existingAxisType) {
    // check new root
    if (newRoot == null || newRoot.matches("\\s*")) {
      return true;
    }
    // handle incorrect data file types
    if (!newDataFileType.hasAxisType) {
      new IllegalStateException("Wrong canShareWith function called - " + newDataFileType
          + " does not have an axis type.  Calling correct canShareWith.  newRoot:"
          + newRoot).printStackTrace();
      return canShareWith(newDataFileType, existingDataFileName, newRoot);
    }
    // Get the type of the existing data file
    DataFileType existingDataFileType = DataFileType.getInstance(existingDataFileName);
    if (existingDataFileType == null) {
      // Its not a data file
      return true;
    }
    // Get existing data file root
    int extIndex = existingDataFileName.lastIndexOf('.');
    String root = existingDataFileName;
    if (extIndex != -1) {
      root = existingDataFileName.substring(0, extIndex);
    }
    // Can't share if the newAxisType is missing
    if ((newAxisType == null || newAxisType == AxisType.NOT_SET)
        && existingDataFileType.hasAxisType) {
      new InvalidParameterException(
          "Warning: dual and single axis reconstructions of the same stack cannot "
              + "share a directory.\nNewAxisType wasn't set for a " + newDataFileType
              + " data file.  hasAxisType:" + newDataFileType.hasAxisType
              + ",existingDataFileName:" + existingDataFileName + ",newRoot:" + newRoot
              + ",root:" + root).printStackTrace();
      return false;
    }
    // If the data file type uses the axis letter, and the new root (newRoot) ends in "a"
    // or "b", strip the axis letter (BBa -> BB, jawa -> jaw).
    boolean stripped = false;
    String strippedLetter = null;
    if (newDataFileType.hasAxisType && (newRoot.endsWith("a") || newRoot.endsWith("b"))) {
      stripped = true;
      if (newAxisType == AxisType.SINGLE_AXIS) {
        // Record the letter that is stripped because b and a may not be compatible for
        // single axis data file types.
        strippedLetter = newRoot.substring(newRoot.length() - 1);
      }
      newRoot = newRoot.substring(0, newRoot.length() - 1);
    }
    // check for sharing with another .edf file
    // Can share a RECON directory if the root is the same
    if (existingDataFileType == DataFileType.RECON) {
      // Can't share if the existingAxisType is missing
      // Don't have enough information to avoid matching stacks with similar names
      if ((existingAxisType == null || existingAxisType == AxisType.NOT_SET)
          && existingDataFileType.hasAxisType) {
        new InvalidAlgorithmParameterException(
            "Warning: dual and single axis reconstructions of the same stack cannot "
                + "share a directory.\nExistingAxisType wasn't set for a "
                + existingDataFileType + " data file.  hasAxisType:"
                + newDataFileType.hasAxisType + ",existingDataFileName:"
                + existingDataFileName + ",newRoot:" + newRoot + ",root:" + root)
            .printStackTrace();
        return false;
      }
      // Match the root without an axis letter
      if (root.equals(newRoot)) {
        if (stripped) {
          if (existingAxisType == AxisType.SINGLE_AXIS) {
            // The existing data file is associated with root.st, while the new .edf
            // will be associated with roota.st/rootb.st, so they cannot share the
            // directory.
            return false;
          }
          return true;
        }
        if (existingAxisType == AxisType.DUAL_AXIS) {
          // The existing data file is associated with roota.st/rootb.st, while the new
          // .edf with be associated with root.st, so they cannot share the directory.
          return false;
        }
        return true;
      }
      // Don't add an axis letter to a root that didn't originally have one
      if (!stripped) {
        return false;
      }
      // single axis can match the same single axis .edf file, or a dual axis file
      if (newAxisType == AxisType.SINGLE_AXIS) {
        if (strippedLetter != null) {
          if (root.equals(newRoot + strippedLetter)) {
            if (existingAxisType == AxisType.DUAL_AXIS) {
              // The existing data file is associated with rootxa.st/rootxb.st, while
              // the new .edf with be associated with rootx.st, so they cannot share the
              // directory.
              return false;
            }
            return true;
          }
          return false;
        }
        new IllegalStateException("Letter was stripped, but not recorded.  usesAxisID:"
            + newDataFileType.hasAxisType + ",existingDataFileName:"
            + existingDataFileName + ",newRoot:" + newRoot + ",root:" + root)
            .printStackTrace();
        return false;
      }
      // Dual axis can match the same dual axis file, or both single axis files
      // Add the stripped axis letters back to find a match with root. This is because
      // dual can share a dataset with single or dual if they use the same stack(s).
      if (root.equals(newRoot + AxisID.FIRST.getExtension())) {
        if (existingAxisType == AxisType.DUAL_AXIS) {
          // The existing data file is associated with rootaa.st/rootab.st, while
          // the new .edf with be associated with roota.st, so they cannot share the
          // directory.
          return false;
        }
        return true;
      }
      if (root.equals(newRoot + AxisID.SECOND.getExtension())) {
        if (existingAxisType == AxisType.DUAL_AXIS) {
          // The existing data file is associated with rootba.st/rootbb.st, while
          // the new .edf with be associated with rootb.st, so they cannot share the
          // directory.
          return false;
        }
        return true;
      }
      return false;
    }
    if (existingDataFileType == DataFileType.SERIAL_SECTIONS) {
      return false;
    }
    return true;
  }
}
