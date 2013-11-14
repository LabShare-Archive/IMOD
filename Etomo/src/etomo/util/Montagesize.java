package etomo.util;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.process.ProcessMessages;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

/**
 * <p>Description: Runs montagesize on .st files.  Creates once instance per file
 * Rereads only when file has changed.</p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class Montagesize {
  public static final String rcsid = "$Id$";
  
  private static final String EXT = ".pl";
  //
  // n'ton member variables
  //
  private static Hashtable instances = new Hashtable();
  //
  // member variables to prevent unnecessary reads
  //
  private FileModifiedFlag modifiedFlag;
  //
  // other member variables
  //
  private EtomoNumber x = new EtomoNumber(EtomoNumber.Type.INTEGER);
  private EtomoNumber y = new EtomoNumber(EtomoNumber.Type.INTEGER);
  private EtomoNumber z = new EtomoNumber(EtomoNumber.Type.INTEGER);

  private final File file;
  private final String propertyUserDir;
  private final AxisID axisID;

  private boolean fileExists = false;
  String[] commandArray = null;
  private boolean ignorePieceListFile = false;
  private int exitValue = -1;

  //
  // n'ton functions
  //
  /**
   * private constructor
   */
  private Montagesize(String propertyUserDir, File file, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.file = file;
    this.axisID = axisID;
    modifiedFlag = new FileModifiedFlag(file);
  }

  /**
   * Function to get an instance of the class
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static Montagesize getInstance(BaseManager manager, AxisID axisID,
      final String fileExtension) {
    File keyFile = Utilities.getFile(manager, axisID, fileExtension);
    String key = makeKey(keyFile);
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize == null) {
      return createInstance(manager.getPropertyUserDir(), key, keyFile, axisID);
    }
    montagesize.setToDefaults();
    return montagesize;
  }

  /**
   * Function to get an instance of the class
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static Montagesize getInstance(String fileLocation, String filename,
      AxisID axisID) {
    File keyFile = Utilities.getFile(fileLocation, filename);
    String key = makeKey(keyFile);
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize == null) {
      return createInstance(fileLocation, key, keyFile, axisID);
    }
    montagesize.setToDefaults();
    return montagesize;
  }

  /**
   * Function to create and save an instance of the class.  Just returns the
   * instance if it already exists.
   * @param key
   * @param file
   * @param axisID
   * @return
   */
  private static synchronized Montagesize createInstance(String propertyUserDir,
      String key, File file, AxisID axisID) {
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize != null) {
      return montagesize;
    }
    montagesize = new Montagesize(propertyUserDir, file, axisID);
    instances.put(key, montagesize);
    montagesize.selfTestInvariants();
    return montagesize;
  }

  /**
   * Make a unique key from a file
   * @param file
   * @return
   */
  private static String makeKey(File file) {
    return file.getAbsolutePath();
  }

  public String getFile() {
    return file.getAbsolutePath();
  }

  //
  // other functions
  //
  /**
   * construct a piece list file
   * @return
   */
  private File makePieceListFile() {
    String filePath = file.getAbsolutePath();
    int extensionIndex = filePath.lastIndexOf(".");
    if (extensionIndex == -1 || extensionIndex == 0) {
      return new File(filePath + EXT);
    }
    return new File(filePath.substring(0, extensionIndex) + EXT);
  }

  public boolean pieceListFileExists() {
    return makePieceListFile().exists();
  }

  /**
   * reset results
   *
   */
  private void reset() {
    fileExists = false;
    x.reset();
    y.reset();
    z.reset();
    commandArray = null;
    exitValue = -1;
  }

  private final void buildCommand() {
    if (commandArray != null) {
      return;
    }
    if (file == null) {
      return;
    }
    File pieceListFile = makePieceListFile();
    if (pieceListFile.exists() && !ignorePieceListFile) {
      commandArray = new String[3];
    }
    else {
      commandArray = new String[2];
    }
    commandArray[0] = ApplicationManager.getIMODBinPath() + "montagesize";
    commandArray[1] = file.getAbsolutePath();
    if (pieceListFile.exists() && !ignorePieceListFile) {
      // bug# 1336
      commandArray[2] = pieceListFile.getAbsolutePath();
    }
  }

  private void setToDefaults() {
    setIgnorePieceListFile(false);
  }

  public synchronized void setIgnorePieceListFile(final boolean input) {
    if (ignorePieceListFile != input) {
      modifiedFlag.reset();
      reset();
    }
    ignorePieceListFile = input;
  }

  public int getExitValue() {
    return exitValue;
  }

  /**
   * run montagesize on the file
   * @throws IOException
   * @throws InvalidParameterException
   * @returns true if attempted to read
   */
  public synchronized boolean read(BaseManager manager) throws IOException,
      InvalidParameterException {
    if (file.isDirectory()) {
      throw new IOException(file + "is not a file.");
    }
    if (!file.exists()) {
      reset();
      return false;
    }
    fileExists = true;
    // If the file hasn't been modified, don't reread
    if (!modifiedFlag.isModifiedSinceLastRead()) {
      return false;
    }
    // put first timestamp after decide to read
    Utilities.timestamp("read", "montagesize", file, Utilities.STARTED_STATUS);
    // Run the montagesize command on the file.
    buildCommand();
    SystemProgram montagesize = new SystemProgram(manager, propertyUserDir, commandArray,
        axisID);
    modifiedFlag.setReadingNow();
    montagesize.run();

    exitValue = montagesize.getExitValue();
    if (exitValue != 0) {
      String[] stdOutput = montagesize.getStdOutput();
      if (stdOutput != null && stdOutput.length > 0) {
        ProcessMessages messages = montagesize.getProcessMessages();
        if (messages.errorListSize() > 0) {
          String message = "montagesize returned an error while reading"
              + file.getAbsolutePath() + ":\n";
          for (int i = 0; i < messages.errorListSize(); i++) {
            message = message + messages.getError(i) + "\n";
          }
          Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
          throw new InvalidParameterException(message);
        }
      }
    }
    // Throw an exception if the file can not be read
    String[] stdError = montagesize.getStdError();
    if (stdError != null && stdError.length > 0) {
      String message = "montagesize returned an error while reading"
          + file.getAbsolutePath() + ":\n";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
      throw new InvalidParameterException(message);
    }

    // Parse the output
    String[] stdOutput = montagesize.getStdOutput();
    if (stdOutput == null || stdOutput.length < 1) {
      Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
      throw new IOException("montagesize returned no data while reading"
          + file.getAbsolutePath());
    }

    for (int i = 0; i < stdOutput.length; i++) {
      // Parse the size of the data
      // Note the initial space in the string below
      String outputLine = stdOutput[i].trim();
      if (outputLine.startsWith("Total NX, NY, NZ:")) {
        String[] tokens = outputLine.split("\\s+");
        if (tokens.length < 7) {
          Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
          throw new IOException(
              "Montagesize returned less than three parameters for image size while reading"
                  + file.getAbsolutePath());
        }
        x.set(tokens[4]);
        y.set(tokens[5]);
        z.set(tokens[6]);
        if (!x.isValid() || x.isNull()) {
          Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
          throw new NumberFormatException("NX is not set, token is " + tokens[4] + "\n"
              + x.getInvalidReason());
        }
        if (!y.isValid() || y.isNull()) {
          Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
          throw new NumberFormatException("NY is not set, token is " + tokens[5] + "\n"
              + y.getInvalidReason());
        }
        if (!z.isValid() || z.isNull()) {
          Utilities.timestamp("read", "montagesize", file, Utilities.FAILED_STATUS);
          throw new NumberFormatException("NZ is not set, token is " + tokens[6] + "\n"
              + z.getInvalidReason());
        }
      }
    }
    Utilities.timestamp("read", "montagesize", file, Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * 
   * @return
   */
  public ConstEtomoNumber getX() {
    return x;
  }

  /**
   * 
   * @return
   */
  public ConstEtomoNumber getY() {
    return y;
  }

  /**
   * 
   * @return
   */
  public ConstEtomoNumber getZ() {
    return z;
  }

  /**
   * 
   * @return
   */
  public boolean isFileExists() {
    return fileExists;
  }

  //
  // self test functions
  //
  void selfTestInvariants() {
    if (!Utilities.isSelfTest()) {
      return;
    }
    if (instances == null) {
      throw new IllegalStateException("instances is null");
    }
    if (file == null || file.isDirectory()) {
      throw new IllegalStateException("file is null");
    }
    String key = makeKey(file);
    if (key == null || key.matches("\\s*")) {
      throw new IllegalStateException("unable to make key: filename="
          + file.getAbsolutePath());
    }
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize == null) {
      throw new IllegalStateException("this instance is not in instances: key=" + key);
    }
  }

  public String toString() {
    return getClass().getName() + "[" + super.toString() + paramString() + "]";
  }

  protected String paramString() {
    return ",file=" + file + ",fileExists=" + fileExists + ",x=" + x + ",y=" + y + ",z="
        + z + ",axisID=" + axisID;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.21  2010/03/27 05:13:39  sueh
 * <p> bug# 1336 Fixed the command line.
 * <p>
 * <p> Revision 1.20  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 1.19  2009/03/17 00:46:43  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.18  2009/02/13 02:39:32  sueh
 * <p> bug# 1152 Added a getInstance function which can construct a file from
 * <p> the file location and the file name.
 * <p>
 * <p> Revision 1.17  2008/11/21 22:39:31  sueh
 * <p> bug# 1138 The command line is too long so do not use the absolute path
 * <p> for the .pl file.
 * <p>
 * <p> Revision 1.16  2007/12/26 22:41:29  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.15  2007/09/07 00:30:43  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.14  2007/08/16 16:38:23  sueh
 * <p> bug# 1035 Added getFile().
 * <p>
 * <p> Revision 1.13  2007/02/05 23:47:15  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.12  2005/11/02 22:15:51  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.
 * <p>
 * <p> Revision 1.11  2005/10/28 18:58:09  sueh
 * <p> bug# 747 Standardizing SystemProgram message parsing.
 * <p>
 * <p> Revision 1.10  2005/10/27 00:38:32  sueh
 * <p> bug# 725 Added buildCommand().  Changed String filename to File file.
 * <p>
 * <p> Revision 1.9  2005/09/09 21:48:24  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
 * <p> Revision 1.8  2005/08/27 22:44:01  sueh
 * <p> bug# 532 In Utilities.timestamp() change the int status to String status,
 * <p> since it doesn't have to be compared.
 * <p>
 * <p> Revision 1.7  2005/07/29 00:55:23  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.6  2005/06/21 00:54:03  sueh
 * <p> bug# 522 Changed File file to String filename.  Avoid holding on to the file
 * <p> in case that would make problems in Windows.  Read() returns true if it
 * <p> attempts to read.  Overiding toString().
 * <p>
 * <p> Revision 1.5  2005/06/20 17:05:36  sueh
 * <p> bug# 522 Changed  Montagesize so that read() is not called
 * <p> automatically.
 * <p>
 * <p> Revision 1.4  2005/06/17 20:04:38  sueh
 * <p> bug# 685 Added timestamps to read().
 * <p>
 * <p> Revision 1.3  2005/04/25 21:42:49  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.2  2005/03/29 19:57:25  sueh
 * <p> bug# 623 Added fileExists state.  Added pieceListFile to the command
 * <p> string, if it exists.
 * <p>
 * <p> Revision 1.1  2005/03/08 02:01:47  sueh
 * <p> bug# 533 Object to call montagesize.
 * <p> </p>
 */
