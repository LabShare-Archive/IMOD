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
  private EtomoNumber z = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private long lastModified = EtomoNumber.LONG_NULL_VALUE;
  
  /**
   * private constructor initializes stack
   * @param directory
   * @param datasetName
   * @param axisID
   */
  private Montagesize(String directory, String datasetName, AxisID axisID) {
    stack = makeStack(directory, datasetName, axisID);
  }
  
  /**
   * public function to get an instance of Montagesize
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  public static Montagesize getInstance(String directory, String datasetName, AxisID axisID) throws IOException, InvalidParameterException {
    File key = makeStack(directory, datasetName, axisID);
    Montagesize montagesize = (Montagesize) instances.get(key);
    if (montagesize == null) {
      montagesize = createInstance(directory, datasetName, axisID);
    }
    montagesize.read();
    return (Montagesize) instances.get(key);
  }
  
  private static File makeStack(String directory, String datasetName, AxisID axisID) {
    return new File(directory, datasetName + axisID.getExtension() + ".st");
  }
  
  /**
   * private synchronized function to create an instance of Montagesize
   * @param directory
   * @param datasetName
   * @param axisID
   * @return
   */
  private static synchronized Montagesize createInstance(String directory, String datasetName, AxisID axisID) throws IOException, InvalidParameterException {
    Montagesize montagesize = new Montagesize(directory, datasetName, axisID);
    instances.put(montagesize.stack, montagesize);
    return montagesize;
  }
  
  private void read() throws IOException, InvalidParameterException {
    if (stack == null) {
      throw new IOException("No stack specified.");
    }
    if (!stack.exists()) {
      throw new IOException(stack.getAbsolutePath() + 
          " does not exist.");
    }
    //If the stack hasn't changed, don't reread
    long curLastModified = stack.lastModified();
    if (lastModified != EtomoNumber.LONG_NULL_VALUE && curLastModified == lastModified) {
      return;
    }
    lastModified = curLastModified;
    //Run the montagesize command on the stack.
    String[] commandArray = new String[2];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "montagesize";
    commandArray[1] = stack.getAbsolutePath();
    SystemProgram montagesize = new SystemProgram(commandArray);
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
          throw new IOException("Montagesize returned less than three parameters for image size");
        }
        z.set(tokens[6]);
        if (!z.isValid() || z.isNull()) {
          throw new NumberFormatException("NZ is not set, token is " + tokens[6] + "\n" + z.getInvalidReason());
        }
      }
    }
  }
  
  public ConstEtomoNumber getZ() {
    return z;
  }
}
/**
* <p> $Log$ </p>
*/