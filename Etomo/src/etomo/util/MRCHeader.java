package etomo.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import etomo.ApplicationManager;
import etomo.process.SystemProgram;
import etomo.type.AxisID;

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
  //n'ton member variables
  //
  private static Hashtable instances = new Hashtable();
  //
  //member variables to prevent unnecessary reads
  //
  private FileModifiedFlag modifiedFlag;
  //
  //other member variables
  //
  private String filename;
  private int nColumns = -1;
  private int nRows = -1;
  private int nSections = -1;
  private int mode = -1;
  private double xPixelSize = Double.NaN;
  private double yPixelSize = Double.NaN;
  private double zPixelSize = Double.NaN;
  private double xPixelSpacing = Double.NaN;
  private double yPixelSpacing = Double.NaN;
  private double zPixelSpacing = Double.NaN;
  private double imageRotation = Double.NaN;
  private int binning = Integer.MIN_VALUE;
  private AxisID axisID;
  private final String propertyUserDir;

  private MRCHeader(String propertyUserDir, File file, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    filename = file.getAbsolutePath();
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
  public static MRCHeader getInstance(String propertyUserDir, String filename,
      AxisID axisID) {
    File keyFile = Utilities.getFile(propertyUserDir, filename);
    String key = makeKey(keyFile);
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader == null) {
      return createInstance(propertyUserDir, key, keyFile, axisID);
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
  private static synchronized MRCHeader createInstance(String propertyUserDir,
      String key, File file, AxisID axisID) {
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader != null) {
      return mrcHeader;
    }
    mrcHeader = new MRCHeader(propertyUserDir, file, axisID);
    instances.put(key, mrcHeader);
    mrcHeader.selfTestInvariants();
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
  //other functions
  //
  /**
   * @returns true if a read was attempted
   */
  public synchronized boolean read() throws IOException, InvalidParameterException {
    File file = Utilities.getFile(propertyUserDir, filename);
    if (filename == null || filename.matches("\\s*") || file.isDirectory()) {
      throw new IOException("No filename specified");
    }
    if (!file.exists()) {
      throw new IOException("file, " + file.getAbsolutePath() + ", does not exist");
    }
    //If the file hasn't changed, don't reread
    if (!modifiedFlag.isModifiedSinceLastRead()) {
      return false;
    }
    Utilities.timestamp("read", "header", filename, Utilities.STARTED_STATUS);

    // Run the header command on the filename, need to use a String[] here to
    // prevent the Runtime from breaking up the command and arguments at spaces.
		String[] commandArray = new String[2];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "header";
    commandArray[1] = filename;
    SystemProgram header = new SystemProgram(propertyUserDir,
        commandArray, axisID);
    header.setDebug(Utilities.isDebug());
    modifiedFlag.setReadingNow();
    header.run();

    if (header.getExitValue() != 0) {
      String[] stdOutput = header.getStdOutput();
      if (stdOutput.length > 0) {
        ArrayList errorList = SystemProgram.parseError(stdOutput);
        if (errorList.size() > 0) {
          String message = "header returned an error:\n";
          for (int i = 0; i < errorList.size(); i++) {
            message = message + errorList.get(i) + "\n";
          }
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new InvalidParameterException(message);
        }
      }
    }
    // Throw an exception if the file can not be read
    String[] stdError = header.getStdError();
    if (stdError.length > 0) {
      String message = "header returned an error:\n";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
      throw new InvalidParameterException(message);
    }

    // Parse the output
    String[] stdOutput = header.getStdOutput();
    if (stdOutput.length < 1) {
      Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
      throw new IOException("header returned no data");
    }

    for (int i = 0; i < stdOutput.length; i++) {
      //  Parse the size of the data
      //  Note the initial space in the string below
      if (stdOutput[i].startsWith(" Number of columns, rows, section")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 10) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException("Header returned less than three parameters for image size");
        }
        nColumns = Integer.parseInt(tokens[7]);
        try {
          nRows = Integer.parseInt(tokens[8]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nRows = -1;
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new NumberFormatException("nRows not set, token is " + tokens[8]);
        }
        try {
          nSections = Integer.parseInt(tokens[9]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nSections = -1;
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new NumberFormatException("nSections not set, token is " + tokens[9]);
        }
      }

      //  Parse the mode
      if (stdOutput[i].startsWith(" Map mode")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 5) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException("Header returned less than one parameter for the mode");
        }
        mode = Integer.parseInt(tokens[4]);
      }

      // Parse the pixels size
      if (stdOutput[i].startsWith(" Pixel spacing")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 7) {
          Utilities.timestamp("read", "header", filename, Utilities.FAILED_STATUS);
          throw new IOException("Header returned less than three parameters for pixel size");
        }
        xPixelSize = Double.parseDouble(tokens[4]);
        yPixelSize = Double.parseDouble(tokens[5]);
        zPixelSize = Double.parseDouble(tokens[6]);
        
        xPixelSpacing = xPixelSize;
        yPixelSpacing = yPixelSize;
        zPixelSpacing = zPixelSize;
      }

      // If the pixel sizes are default value scan for FEI pixel size in the
      // comment section
      if(xPixelSize == 1.0 && yPixelSize == 1.0 && yPixelSize == 1.0) {
        parseFEIPixelSize(stdOutput[i]);
      }
      
      // Parse the rotation angle and/or binning from the comment section
      parseTiltAxis(stdOutput[i]);
      parseBinning(stdOutput[i]);
    }
    Utilities.timestamp("read", "header", filename, Utilities.FINISHED_STATUS);
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
   * header has not been read or the image rotation is not available return
   * Double.NaN
   * @return
   */
  public double getImageRotation() {
    return imageRotation;
  }
  /**
   * @return
   */
  public double getXPixelSize() {
    return xPixelSize;
  }

  /**
   * @return
   */
  public double getYPixelSize() {
    return yPixelSize;
  }

  /**
   * @return
   */
  public double getZPixelSize() {
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
  public int getBinning(){
    return binning;
  }

  /**
   * Parse the tilt axis parameter from the comments, looking for new form
   * first then old.
   * @param line
   */
  private void parseTiltAxis(String line) {
    if(line.matches(".*Tilt axis angle =.*")){
      String[] tokens = line.split("\\s+");
      if (tokens.length > 5) {
        imageRotation = 
          Double.parseDouble(tokens[5].substring(0, tokens[5].length()-1));
        return;
      }
    }
    if (line.startsWith("          Tilt axis rotation angle")) {
      String[] tokens = line.split("\\s+");
      if (tokens.length > 6) {
        imageRotation = Double.parseDouble(tokens[6]);
      }
    }
  }
  
  /**
   * Parse the binning parameter from the comments
   * @param line
   */
  private void parseBinning(String line){
    if(line.matches(".*, binning =.*")){
      String[] tokens = line.split("\\s+");
      if (tokens.length > 8) {
        binning = Integer.parseInt(tokens[8]);
      }
    }
  }
  
  /**
   * FEI pixel size parser
   * @param line
   */
  private void parseFEIPixelSize(String line){
    if(line.matches(".*Pixel size in nanometers.*")){
      String[] tokens = line.split("\\s+");
      if (tokens.length > 6) {
        xPixelSize = Double.parseDouble(tokens[6]) * 10.0;
        yPixelSize = xPixelSize;
        zPixelSize = yPixelSize;
      }
    }
  }

  //
  //self test functions
  //
  void selfTestInvariants() {
    if(!Utilities.isSelfTest()) {
      return;
    }
    if (instances == null) {
      throw new IllegalStateException("instances is null");
    }
    if (filename == null || filename.matches("\\s*")) {
      throw new IllegalStateException("file is null");
    }
    String key = makeKey(Utilities.getFile(propertyUserDir, filename));
    if (key == null || key.matches("\\s*")) {
      throw new IllegalStateException("unable to make key: filename=" + filename);
    }
    MRCHeader mrcHeader = (MRCHeader) instances.get(key);
    if (mrcHeader == null || mrcHeader != this) {
      throw new IllegalStateException("this instance is not in instances: key="
          + key + ",filename=" + filename);
    }
  }
  
  public String toString() {
    return getClass().getName() + "[" + super.toString() + paramString() + "]";
  }

  protected String paramString() {
    return ",\nfilename=" + filename + ",nColumns=" + nColumns + ",nRows="
        + nRows + ",\nnSections=" + nSections + ",mode=" + mode
        + ",\nxPixelSize=" + xPixelSize + ",yPixelSize=" + yPixelSize
        + ",\nzPixelSize=" + zPixelSize + ",xPixelSpacing=" + xPixelSpacing
        + ",\nyPixelSpacing=" + yPixelSpacing + ",zPixelSpacing=" + zPixelSpacing
        + ",\nimageRotation=" + imageRotation + ",binning=" + binning
        + ",\naxisID=" + axisID;
  }
}
