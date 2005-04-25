package etomo.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
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
  public static  final String  rcsid =  "$Id$";
  
  private static Hashtable instances = new Hashtable();
  private File stack = null;
  private boolean fileExists = false;
  private EtomoNumber x = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private EtomoNumber y = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private EtomoNumber z = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private long lastModified = EtomoNumber.LONG_NULL_VALUE;
  private String directory;
  private String datasetName;
  private AxisID axisID;
  
  /**
   * private constructor initializes stack
   * @param directory
   * @param datasetName
   * @param axisID
   */
  private Montagesize(String directory, String datasetName, AxisID axisID) {
    this.directory = directory;
    this.datasetName = datasetName;
    this.axisID = axisID;
    stack = makeFile(directory, datasetName, axisID);
  }

  /**
   * public function to get an instance of Montagesize
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static Montagesize getInstance(String directory, String datasetName,
      AxisID axisID) throws IOException, InvalidParameterException {
    File key = makeFile(directory, datasetName, axisID);
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize == null) {
      montagesize = createInstance(directory, datasetName, axisID);
    }
    montagesize.read();
    return (Montagesize) instances.get(key);
  }

  /**
   * 
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  private static File makeFile(String directory, String datasetName,
      AxisID axisID) {
    return new File(directory, datasetName + axisID.getExtension() + ".st");
  }

  /**
   * private synchronized function to create an instance of Montagesize
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  private static synchronized Montagesize createInstance(String directory,
      String datasetName, AxisID axisID) throws IOException,
      InvalidParameterException {
    Montagesize montagesize = new Montagesize(directory, datasetName, axisID);
    instances.put(montagesize.stack, montagesize);
    return montagesize;
  }

  /**
   * 
   * @throws IOException
   * @throws InvalidParameterException
   */
  private void read() throws IOException, InvalidParameterException {
    if (stack == null) {
      throw new IOException("No stack specified.");
    }
    if (!stack.exists()) {
      fileExists = false;
      return;
    }
    fileExists = true;
    //If the stack hasn't changed, don't reread
    long curLastModified = stack.lastModified();
    if (lastModified != EtomoNumber.LONG_NULL_VALUE
        && curLastModified == lastModified) {
      return;
    }
    lastModified = curLastModified;
    //Run the montagesize command on the stack.
    File pieceListFile = new File(directory, datasetName + axisID.getExtension() + ".pl");
    String[] commandArray;
    if (pieceListFile.exists()) {
      commandArray = new String[3];
    }
    else {
      commandArray = new String[2];
    }
    commandArray[0] = ApplicationManager.getIMODBinPath() + "montagesize";
    commandArray[1] = stack.getAbsolutePath();
    if (pieceListFile.exists()) {
      commandArray[2] = pieceListFile.getAbsolutePath();
    }
    SystemProgram montagesize = new SystemProgram(commandArray, axisID);
    montagesize.setDebug(EtomoDirector.getInstance().isDebug());
    montagesize.run();

    if (montagesize.getExitValue() != 0) {
      String[] stdOutput = montagesize.getStdOutput();
      if (stdOutput.length > 0) {
        ArrayList errorList = SystemProgram.parseError(stdOutput);
        if (errorList.size() > 0) {
          String message = "montagesize returned an error:\n";
          for (int i = 0; i < errorList.size(); i++) {
            message = message + errorList.get(i) + "\n";
          }
          throw new InvalidParameterException(message);
        }
      }
    }
    // Throw an exception if the file can not be read
    String[] stdError = montagesize.getStdError();
    if (stdError.length > 0) {
      String message = "montagesize returned an error:\n";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      throw new InvalidParameterException(message);
    }

    // Parse the output
    String[] stdOutput = montagesize.getStdOutput();
    if (stdOutput.length < 1) {
      throw new IOException("montagesize returned no data");
    }

    for (int i = 0; i < stdOutput.length; i++) {
      //  Parse the size of the data
      //  Note the initial space in the string below
      String outputLine = stdOutput[i].trim();
      if (outputLine.startsWith("Total NX, NY, NZ:")) {
        String[] tokens = outputLine.split("\\s+");
        if (tokens.length < 7) {
          throw new IOException(
              "Montagesize returned less than three parameters for image size");
        }
        x.set(tokens[4]);
        y.set(tokens[5]);
        z.set(tokens[6]);
        if (!x.isValid() || x.isNull()) {
          throw new NumberFormatException("NX is not set, token is "
              + tokens[4] + "\n" + x.getInvalidReason());
        }
        if (!y.isValid() || y.isNull()) {
          throw new NumberFormatException("NY is not set, token is "
              + tokens[5] + "\n" + y.getInvalidReason());
        }
        if (!z.isValid() || z.isNull()) {
          throw new NumberFormatException("NZ is not set, token is "
              + tokens[6] + "\n" + z.getInvalidReason());
        }
      }
    }
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
  
  public boolean isFileExists() {
    return fileExists;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2005/03/29 19:57:25  sueh
 * <p> bug# 623 Added fileExists state.  Added pieceListFile to the command
 * <p> string, if it exists.
 * <p>
 * <p> Revision 1.1  2005/03/08 02:01:47  sueh
 * <p> bug# 533 Object to call montagesize.
 * <p> </p>
 */