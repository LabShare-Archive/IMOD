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
import etomo.type.FileType;
import etomo.ui.swing.UIHarness;

/**
 * <p>Description: An interface to the header information in a MRC Image 
 * file.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.36  2010/11/13 16:08:59  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.35  2010/04/08 18:09:31  sueh
 * <p> bug# 1348 Whoops... do need to trim.
 * <p>
 * <p> Revision 3.34  2010/04/08 18:07:44  sueh
 * <p> bug# 1348 In parseTiltAxis removed unnecessary trim command.
 * <p>
 * <p> Revision 3.33  2010/04/08 18:05:48  sueh
 * <p> bug# 1348 Changed imageRotation to an EtomoNumber.  Handling an
 * <p> equals without spaces for one parsing situation involving this value.
 * <p>
 * <p> Revision 3.32  2010/03/09 22:08:36  sueh
 * <p> bug# 1325 Added getInstance(BaseManager,AxisID,FileType).
 * <p>
 * <p> Revision 3.31  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 3.30  2009/09/21 18:11:46  sueh
 * <p> bug# 1267 Added getInstanceFromFileName to run header using a full
 * <p> file name instead of an extension.
 * <p>
 * <p> Revision 3.29  2009/09/17 19:13:26  sueh
 * <p> bug# 1257 Adding debug functionality.
 * <p>
 * <p> Revision 3.28  2009/06/15 20:27:19  sueh
 * <p> bug# 1221 Reformatted.
 * <p>
 * <p> Revision 3.27  2009/06/10 17:28:12  sueh
 * <p> bug# 1202 Moved parsing information for columns, row, and sections info
 * <p> constants.  Added brief header constants.
 * <p>
 * <p> Revision 3.26  2009/03/17 00:46:43  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.25  2009/02/13 02:40:34  sueh
 * <p> bug# 1176 In read, when the file does not exist put a warning in the log
 * <p> file instead of throwing an exception.  Also return false in this case.
 * <p>
 * <p> Revision 3.24  2007/02/05 23:47:28  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 3.23  2006/11/15 21:36:38  sueh
 * <p> bug# 872 Removed setTestInvariants
 * <p>
 * <p> Revision 3.22  2006/09/21 16:41:50  sueh
 * <p> bug# 680 Changed MRCHeader.x/y/zPixelSize to EtomoNumber.  Added
 * <p> parsePixelSpacing to check for errors.
 * <p>
 * <p> Revision 3.21  2006/08/11 00:18:50  sueh
 * <p> Read():  added more information to error messages.
 * <p>
 * <p> Revision 3.20  2006/05/19 19:54:37  sueh
 * <p> bug# 866 Added getInstance(BaseManager, AxisID).
 * <p>
 * <p> Revision 3.19  2005/11/10 18:19:00  sueh
 * <p> bug# 733 Changed propertyUserDir to fileLocation since it doesn't have to
 * <p> be the working directory.
 * <p>
 * <p> Revision 3.18  2005/11/02 22:16:05  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.
 * <p>
 * <p> Revision 3.17  2005/10/28 18:58:17  sueh
 * <p> bug# 747 Standardizing SystemProgram message parsing.
 * <p>
 * <p> Revision 3.16  2005/09/09 21:48:35  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
 * <p> Revision 3.15  2005/08/27 22:44:10  sueh
 * <p> bug# 532 In Utilities.timestamp() change the int status to String status,
 * <p> since it doesn't have to be compared.
 * <p>
 * <p> Revision 3.14  2005/07/29 00:55:29  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.13  2005/06/21 00:54:32  sueh
 * <p> bug# 522 Changed File file to String filename.  Avoid holding on to the file
 * <p> in case that would make problems in Windows.  Read() returns true if it
 * <p> attempts to read.  Overiding toString().
 * <p>
 * <p> Revision 3.12  2005/06/20 17:06:43  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Added a flag to prevent rereading
 * <p> when the file has not been changed.
 * <p>
 * <p> Revision 3.11  2005/06/17 20:04:58  sueh
 * <p> bug# 685 Added timestamp functions for ComScript and File types.
 * <p> Added code to the main timestamp function to strip the path from a file
 * <p> name.  These changes reduces the amount of timestamp related code
 * <p> being executed when debug is off.
 * <p>
 * <p> Revision 3.10  2005/06/17 19:18:53  sueh
 * <p> bug# 685 Added timestamps to the read function.
 * <p>
 * <p> Revision 3.9  2005/04/25 21:43:12  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.8  2005/01/14 03:14:32  sueh
 * <p> Prevented non-error messages from showing up in the err.log  file unless
 * <p> debug is on.
 * <p>
 * <p> Revision 3.7  2004/11/20 00:11:48  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.6.2.2  2004/10/06 02:31:52  sueh
 * <p> bug# 520 Removed System.out.print statements.
 * <p>
 * <p> Revision 3.6.2.1  2004/10/01 17:31:03  sueh
 * <p> bug# 520 Improving error handling:  parsing standard out to catch
 * <p> unknown file format, and catching errors when parsing Nrows and
 * <p> Nsections to make sure that the members are set to -1 when parsing
 * <p> fails.
 * <p>
 * <p> Revision 3.6  2004/06/29 23:56:10  sueh
 * <p> bug# 487 adding pixel spacing for x, y, and z.  These variables
 * <p> are equals to the pixel size variables, except when pixel size
 * <p> in the header is one and the FEI pixel size exists.  In this case
 * <p> pixel spacing is 1.
 * <p>
 * <p> Revision 3.5  2004/06/21 18:40:22  rickg
 * <p> Bug #480 Added FEI pixel size parser.
 * <p>
 * <p> Revision 3.4  2004/04/22 23:23:37  rickg
 * <p> Switched getIMODBinPath method
 * <p>
 * <p> Revision 3.3  2004/03/09 23:26:40  rickg
 * <p> Moved return within parseTiltAxis
 * <p>
 * <p> Revision 3.2  2004/03/09 23:21:24  rickg
 * <p> Bug# 386 Added parsing of new rotation and binning parmaters
 * <p>
 * <p> Revision 3.1  2004/01/13 22:41:08  rickg
 * <p> Bug #376 Allow for spaces in the filename.  Needed to call the
 * <p> string array version of SystemProgram
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from 
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.4  2003/10/07 23:06:54  rickg
 * <p> Fixed string typo
 * <p>
 * <p> Revision 2.3  2003/07/01 19:30:22  rickg
 * <p> added mode parsing
 * <p>
 * <p> Revision 2.2  2003/05/20 21:26:18  rickg
 * <p> Added pixel size and image rotation parsers
 * <p>
 * <p> Revision 2.1  2003/05/08 23:17:50  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4.2.1  2003/01/24 18:45:05  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.4  2002/10/10 19:22:24  rickg
 * <p> Enable debugging output from SystemProgram
 * <p>
 * <p> Revision 1.3  2002/10/08 17:03:49  rickg
 * <p> Swapped position of nColumns and nRows in read()
 * <p>
 * <p> Revision 1.2  2002/10/03 19:17:48  rickg
 * <p> Added new InvalidParameterException.
 * <p> Corrected parsing of header output to account for leading space.
 * <p>
 * <p> Revision 1.1  2002/10/03 03:58:46  rickg
 * <p> Initial revision, in development
 * <p>
 */

public class MRCHeader {
  //
  // n'ton member variables
  //
  private static Hashtable instances = new Hashtable();
  //
  // member variables to prevent unnecessary reads
  //
  private FileModifiedFlag modifiedFlag;

  public static final String SIZE_HEADER = "Number of columns, rows, sections";
  public static int N_SECTIONS_INDEX = 8;
  private static int N_ROWS_INDEX = N_SECTIONS_INDEX - 1;
  private static int N_COLUMNS_INDEX = N_SECTIONS_INDEX - 2;

  // This is information about the header process. Will only be used by
  // MrcHeader if it is run with the "-brief" option.
  public static String SIZE_HEADER_BRIEF = "Dimensions:";
  public static int N_SECTIONS_INDEX_BRIEF = 3;

  //
  // other member variables
  //
  private String filename;
  private int nColumns = -1;
  private int nRows = -1;
  private int nSections = -1;
  private int mode = -1;
  private final EtomoNumber xPixelSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber yPixelSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber zPixelSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber twodir = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private double xPixelSpacing = Double.NaN;
  private double yPixelSpacing = Double.NaN;
  private double zPixelSpacing = Double.NaN;
  private EtomoNumber imageRotation = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private int binning = Integer.MIN_VALUE;
  private AxisID axisID;
  private final String fileLocation;

  private boolean debug = false;

  private MRCHeader(String fileLocation, File file, AxisID axisID) {
    this.fileLocation = fileLocation;
    filename = file.getAbsolutePath();
    this.axisID = axisID;
    modifiedFlag = new FileModifiedFlag(file);
  }

  public static MRCHeader getInstance(BaseManager manager, AxisID axisID, String fileExt) {
    return MRCHeader.getInstance(manager.getPropertyUserDir(), DatasetFiles
        .getDatasetFile(manager, axisID, fileExt).getAbsolutePath(), axisID);
  }

  public static MRCHeader getInstanceFromFileName(BaseManager manager, AxisID axisID,
      String fileName) {
    return MRCHeader.getInstance(manager.getPropertyUserDir(), DatasetFiles
        .getDatasetFileFromFileName(manager, axisID, fileName).getAbsolutePath(), axisID);
  }

  /**
   * Function to get an instance of the class
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static MRCHeader getInstance(BaseManager manager, AxisID axisID,
      FileType fileType) {
    File keyFile = Utilities.getFile(manager.getPropertyUserDir(),
        fileType.getFileName(manager, axisID));
    String key = makeKey(keyFile);
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader == null) {
      return createInstance(manager.getPropertyUserDir(), key, keyFile, axisID);
    }
    return mrcHeader;
  }

  /**
   * Function to get an instance of the class
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static MRCHeader getInstance(String fileLocation, String filename, AxisID axisID) {
    File keyFile = Utilities.getFile(fileLocation, filename);
    String key = makeKey(keyFile);
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader == null) {
      return createInstance(fileLocation, key, keyFile, axisID);
    }
    return mrcHeader;
  }

  /**
   * Function to create and save an instance of the class.  Just returns the
   * instance if it already exists.
   * @param key
   * @param file
   * @param axisID
   * @return
   */
  private static synchronized MRCHeader createInstance(String fileLocation, String key,
      File file, AxisID axisID) {
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader != null) {
      return mrcHeader;
    }
    mrcHeader = new MRCHeader(fileLocation, file, axisID);
    instances.put(key, mrcHeader);
    return mrcHeader;
  }

  /**
   * Make a unique key from a file
   * @param file
   * @return
   */
  private static String makeKey(File file) {
    return file.getAbsolutePath();
  }

  //
  // other functions
  //
  /**
   * @returns true if file exists
   */
  public synchronized boolean read(BaseManager manager) throws IOException,
      InvalidParameterException {
    File file = Utilities.getFile(fileLocation, filename);
    if (filename == null || filename.matches("\\s*") || file.isDirectory()) {
      throw new IOException("No filename specified");
    }
    if (!file.exists()) {
      System.err.println("WARNING: attempting to read the header of "
          + file.getAbsolutePath() + ", which doesn't exist.");
      return false;
    }
    // If the file hasn't changed, don't reread
    if (!modifiedFlag.isModifiedSinceLastRead()) {
      return true;
    }
    Utilities.timestamp("read", "header", filename, Utilities.STARTED_STATUS);

    // Run the header command on the filename, need to use a String[] here to
    // prevent the Runtime from breaking up the command and arguments at spaces.
    String[] commandArray = new String[2];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "header";
    commandArray[1] = filename;
    SystemProgram header = new SystemProgram(manager, fileLocation, commandArray, axisID);
    modifiedFlag.setReadingNow();
    header.run();

    if (header.getExitValue() != 0) {
      ProcessMessages messages = header.getProcessMessages();
      if (messages.errorListSize() > 0) {
        String message = "header returned an error:\n";
        for (int i = 0; i < messages.errorListSize(); i++) {
          message = message + messages.getError(i) + "\n";
        }
        Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
        throw new InvalidParameterException(filename + ":" + message);
      }
    }
    // Throw an exception if the file can not be read
    String[] stdError = header.getStdError();
    if (stdError != null && stdError.length > 0) {
      String message = "header returned an error:\n";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
      throw new InvalidParameterException(filename + ":" + message);
    }

    // Parse the output
    String[] stdOutput = header.getStdOutput();
    if (stdOutput == null || stdOutput.length < 1) {
      Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
      throw new IOException("header returned no data");
    }

    for (int i = 0; i < stdOutput.length; i++) {
      // Parse the size of the data
      // Note the initial space in the string below
      // Need to get brief header and regular header in the same way, so change
      // so that the output is trimmed for this parse.
      if (stdOutput[i].trim().startsWith(SIZE_HEADER)) {
        String[] tokens = stdOutput[i].trim().split("\\s+");
        if (debug) {
          System.out.print("tokens=");
          if (tokens != null) {
            for (int j = 0; j < tokens.length; j++) {
              System.out.print(tokens[j] + ",");
            }
          }
        }
        if (tokens.length < N_SECTIONS_INDEX + 1) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException(
              "Header returned less than three parameters for image size");
        }
        nColumns = Integer.parseInt(tokens[N_COLUMNS_INDEX]);
        try {
          nRows = Integer.parseInt(tokens[N_ROWS_INDEX]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nRows = -1;
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new NumberFormatException("nRows not set, token is "
              + tokens[N_ROWS_INDEX]);
        }
        try {
          nSections = Integer.parseInt(tokens[N_SECTIONS_INDEX]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nSections = -1;
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new NumberFormatException("nSections not set, token is "
              + tokens[N_SECTIONS_INDEX]);
        }
      }

      // Parse the mode
      if (stdOutput[i].startsWith(" Map mode")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 5) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException("Header returned less than one parameter for the mode");
        }
        mode = Integer.parseInt(tokens[4]);
      }
      // PixelsParsed will be set to true if there are no errors parsing
      // "Pixel Spacing".
      boolean pixelsParsed = false;
      // Parse the pixels size
      if (stdOutput[i].startsWith(" Pixel spacing")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 7) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException(
              "Header returned less than three parameters for pixel size");
        }
        pixelsParsed = parsePixelSpacing(manager, xPixelSize, tokens[4], true);
        pixelsParsed = pixelsParsed
            && parsePixelSpacing(manager, yPixelSize, tokens[5], !pixelsParsed);
        pixelsParsed = pixelsParsed
            && parsePixelSpacing(manager, zPixelSize, tokens[6], !pixelsParsed);

        xPixelSpacing = xPixelSize.getDouble();
        yPixelSpacing = yPixelSize.getDouble();
        zPixelSpacing = zPixelSize.getDouble();
      }

      // If the pixel sizes are default value scan for FEI pixel size in the
      // comment section
      if (xPixelSize.equals(1.0) && yPixelSize.equals(1.0) && yPixelSize.equals(1.0)) {
        parseFEIPixelSize(manager, stdOutput[i], !pixelsParsed);
      }

      // Parse the rotation angle and/or binning from the comment section
      parseTiltAxis(stdOutput[i]);
      parseBinning(stdOutput[i]);
      parseTwodir(stdOutput[i]);
    }
    Utilities.timestamp("read", "header", filename, Utilities.FINISHED_STATUS);
    return true;
  }

  /**
   * Parse pixel spacing (pixel size).  If there is an error, pop up an error
   * message, if requested
   * @param pixelSpacing - current pixel spacing
   * @param sPixelSpacing - string to parse
   * @param popupErrorMessage - pop up an error message is there is an error
   * @return success boolean
   */
  private boolean parsePixelSpacing(BaseManager manager, EtomoNumber pixelSpacing,
      String sPixelSpacing, boolean popupErrorMessage) {
    pixelSpacing.set(sPixelSpacing);
    double dPixelSpacing = pixelSpacing.getDouble();
    String errorMessage = null;
    if (!pixelSpacing.isValid() || dPixelSpacing == -1 || dPixelSpacing == 0) {
      if (popupErrorMessage) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Invalid pixel spacing:  "
            + sPixelSpacing + ".  Fix the mrc header in " + filename
            + " with alterheader.", "Header Error", axisID);
      }
      return false;
    }
    return true;
  }

  /**
   * Returns the nColumns.
   * @return int
   */
  public int getNColumns() {
    return nColumns;
  }

  /**
   * Returns the nRows.
   * @return int
   */
  public int getNRows() {
    return nRows;
  }

  public ConstEtomoNumber getTwodir() {
    return twodir;
  }

  /**
   * Returns the nSections.
   * @return int
   */
  public int getNSections() {
    return nSections;
  }

  /**
   * Return the mode (type) of data in the file.
   * @return
   */
  public int getMode() {
    return mode;
  }

  /**
   * Return the image rotation in degrees if present in the header.  If the
   * header has not been read or the image rotation is not available
   * imageRotation will be null.
   * @return ConstEtomoNumber
   */
  public ConstEtomoNumber getImageRotation() {
    return imageRotation;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getXPixelSize() {
    return xPixelSize;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getYPixelSize() {
    return yPixelSize;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getZPixelSize() {
    return zPixelSize;
  }

  /**
   * 
   * @return
   */
  public double getXPixelSpacing() {
    return xPixelSpacing;
  }

  /**
   * Return the binning value found in the header or Ingeter.MIN_VALUE if
   * no binning value was found
   */
  public int getBinning() {
    return binning;
  }

  /**
   * Parse the tilt axis parameter from the comments, looking for new form
   * first then old.
   * @param line
   */
  private void parseTiltAxis(String line) {
    if (line.matches(".*Tilt axis angle =.*")) {
      String[] tokens = line.split("\\s+");
      if (tokens.length > 5) {
        imageRotation.set(tokens[5].substring(0, tokens[5].length() - 1));
        return;
      }
    }
    line = line.trim();
    if (line.startsWith("Tilt axis rotation angle")) {
      // Handle an "=" sign without a following space. This can happen with
      // Fortran output because it wants to put numbers in fixed-width columns.
      String[] pair = line.split("=");
      String[] valueTokens = pair[1].trim().split("\\s+");
      if (valueTokens.length > 0) {
        imageRotation.set(valueTokens[0]);
      }
    }
  }

  /**
   * Parse the binning parameter from the comments
   * @param line
   */
  private void parseBinning(String line) {
    if (line.matches(".*, binning =.*")) {
      String[] tokens = line.split("\\s+");
      if (tokens.length > 8) {
        binning = Integer.parseInt(tokens[8]);
      }
    }
  }

  /**
   * Parse the binning parameter from the comments
   * @param line
   */
  private void parseTwodir(String line) {
    String tag = "bidir";
    if (line.indexOf(tag) != -1 && line.indexOf("RO image file") == -1) {
      String[] array = line.trim().split("\\s*\\=\\s*|\\s*,\\s*|\\s+");
      if (array != null) {
        for (int i = 0; i < array.length; i++) {
          if (array[i].equals(tag) && i < array.length - 1) {
            twodir.set(array[i + 1]);
            break;
          }
        }
      }
    }
  }

  /**
   * FEI pixel size parser
   * Pixel spacing should remain set to 1
   * @param line
   */
  private void parseFEIPixelSize(BaseManager manager, String line, boolean userMessage) {
    if (line.matches(".*Pixel size in nanometers.*")) {
      String[] tokens = line.split("\\s+");
      if (tokens.length > 6) {
        if (parsePixelSpacing(manager, xPixelSize, tokens[6], userMessage)) {
          xPixelSize.set(xPixelSize.getDouble() * 10.0);
        }
        yPixelSize.set(xPixelSize);
        zPixelSize.set(yPixelSize);
      }
    }
  }

  public String toString() {
    return getClass().getName() + "[" + super.toString() + paramString() + "]";
  }

  protected String paramString() {
    return ",\nfilename=" + filename + ",nColumns=" + nColumns + ",nRows=" + nRows
        + ",\nnSections=" + nSections + ",mode=" + mode + ",\nxPixelSize=" + xPixelSize
        + ",yPixelSize=" + yPixelSize + ",\nzPixelSize=" + zPixelSize + ",xPixelSpacing="
        + xPixelSpacing + ",\nyPixelSpacing=" + yPixelSpacing + ",zPixelSpacing="
        + zPixelSpacing + ",\nimageRotation=" + imageRotation + ",binning=" + binning
        + ",\naxisID=" + axisID;
  }
}
