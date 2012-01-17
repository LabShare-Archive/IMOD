package etomo.logic;

import java.io.File;

import etomo.EtomoDirector;
import etomo.storage.PeetFileFilter;
import etomo.util.DatasetFiles;
import etomo.util.FilePath;

/**
* <p>Description: The data necessary to create a PEET dataset, including an absolute path
* to the dataset location.</p>
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
public final class PeetStartupData {
  public static final String rcsid = "$Id:$";

  private File directory = null;
  private File copyFrom = null;
  private String baseName = null;

  /**
   * Sets directory to an absolute path.  If input isn't an absolute path, it uses the
   * directory in which etomo was run to make an absolute path.
   * @param input
   */
  public void setDirectory(final String input) {
    if (input == null) {
      return;
    }
    directory = FilePath.buildAbsoluteFile(EtomoDirector.INSTANCE.getOriginalUserDir(),
        input);
  }

  /**
   * Sets copyFrom to an absolute path.  If input isn't an absolute path, it uses the
   * directory in which etomo was run to make an absolute path.
   * @param input
   */
  public void setCopyFrom(final String input) {
    if (input == null) {
      return;
    }
    copyFrom = FilePath.buildAbsoluteFile(EtomoDirector.INSTANCE.getOriginalUserDir(),
        input);
  }

  public void setBaseName(final String input) {
    baseName = input;
  }

  /**
   * Validation
   * @return an error message or null if valid
   */
  public String validate() {
    // Only one .epe file per directory
    // OK to use directory if it contains an .epe file of the same name
    File[] paramFiles = directory.listFiles(new PeetFileFilter(false));
    if (paramFiles != null
        && paramFiles.length > 0
        && (paramFiles.length > 1 || !DatasetFiles.getPeetRootName(
            paramFiles[0].getName()).equals(baseName))) {
      return "The directory " + directory.getAbsolutePath() + " can contain only one "
          + DatasetFiles.PEET_DATA_FILE_EXT + " file.";
    }
    if (isCopyFrom() && copyFrom.getParentFile().equals(directory)) {
      return "Cannot duplicate a project in the same directory.";
    }
    return null;
  }

  public String getBaseName() {
    return baseName;
  }

  /**
   * Returns the absolute path of directory rather then the member variable itself, which
   * should not be changed directly.
   * @return
   */
  public String getDirectory() {
    return directory.getAbsolutePath();
  }

  /**
   * Returns the absolute path of copyFrom rather then the member variable itself, which
   * should not be changed directly.
   * @return
   */
  public String getCopyFrom() {
    return copyFrom.getAbsolutePath();
  }

  public boolean isCopyFrom() {
    return copyFrom != null;
  }

  /**
   * Builds and returns the param file
   * @return
   */
  public File getParamFile() {
    return new File(directory, baseName + DatasetFiles.PEET_DATA_FILE_EXT);
  }
}
