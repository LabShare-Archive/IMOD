package etomo.util;

import java.io.IOException;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;

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
  private boolean debug = false;

  public MRCHeader(String name) {
    filename = new String(name);
    debug = EtomoDirector.getInstance().isDebug();
  }

  public void read() throws IOException, InvalidParameterException {
    if (filename == null || filename.length() == 0) {
      throw new IOException("No filename specified");
    }

    // Run the header command on the filename, need to use a String[] here to
    // prevent the Runtime from breaking up the command and arguments at spaces.
		String[] commandArray = new String[2];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "header";
    commandArray[1] = filename;
    SystemProgram header = new SystemProgram(commandArray);
    header.setDebug(debug);
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
      throw new InvalidParameterException(message);
    }

    // Parse the output
    String[] stdOutput = header.getStdOutput();
    if (stdOutput.length < 1) {
      throw new IOException("header returned no data");
    }

    for (int i = 0; i < stdOutput.length; i++) {
      //  Parse the size of the data
      //  Note the initial space in the string below
      if (stdOutput[i].startsWith(" Number of columns, rows, section")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 10) {
          throw new IOException("Header returned less than three parameters for image size");
        }
        nColumns = Integer.parseInt(tokens[7]);
        try {
          nRows = Integer.parseInt(tokens[8]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nRows = -1;
          throw new NumberFormatException("nRows not set, token is " + tokens[8]);
        }
        try {
          nSections = Integer.parseInt(tokens[9]);
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          nSections = -1;
          throw new NumberFormatException("nSections not set, token is " + tokens[9]);
        }
      }

      //  Parse the mode
      if (stdOutput[i].startsWith(" Map mode")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 5) {
          throw new IOException("Header returned less than one parameter for the mode");
        }
        mode = Integer.parseInt(tokens[4]);
      }

      // Parse the pixels size
      if (stdOutput[i].startsWith(" Pixel spacing")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 7) {
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
   * Returns the filename.
   * @return String
   */
  public String getFilename() {
    return filename;
  }

  /**
   * Sets the filename.
   * @param filename The filename to set
   */
  public void setFilename(String filename) {
    this.filename = filename;
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

}
