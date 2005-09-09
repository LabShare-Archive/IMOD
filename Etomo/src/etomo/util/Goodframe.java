package etomo.util;

import java.io.IOException;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;

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
public class Goodframe {
  public static final String rcsid = "$Id$";

  private EtomoNumber firstOutput = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private EtomoNumber secondOutput = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private AxisID axisID;
  private final String propertyUserDir;

  public Goodframe(String propertyUserDir, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
  }
  /**
   * 
   * @throws IOException
   * @throws InvalidParameterException
   */
  public void run(int firstInput, int secondInput) throws IOException,
      InvalidParameterException {
    Utilities.timestamp("run", "goodframe", Utilities.STARTED_STATUS);
    //Run the goodframe command.
    String[] commandArray = new String[3];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "goodframe";
    commandArray[1] = Integer.toString(firstInput);
    commandArray[2] = Integer.toString(secondInput);
    SystemProgram groupframe = new SystemProgram(propertyUserDir, commandArray,
        axisID);
    groupframe.setDebug(EtomoDirector.getInstance().isDebug());
    groupframe.run();

    if (groupframe.getExitValue() != 0) {
      String[] stdOutput = groupframe.getStdOutput();
      if (stdOutput != null && stdOutput.length > 0) {
        ArrayList errorList = SystemProgram.parseError(stdOutput);
        if (errorList.size() > 0) {
          String message = "groupframe returned an error:\n";
          for (int i = 0; i < errorList.size(); i++) {
            message = message + errorList.get(i) + "\n";
          }
          Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
          throw new InvalidParameterException(message);
        }
      }
    }
    // Throw an exception if the file can not be read
    String[] stdError = groupframe.getStdError();
    if (stdError != null && stdError.length > 0) {
      String message = "groupframe returned an error:\n";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new InvalidParameterException(message);
    }

    // Parse the output
    String[] stdOutput = groupframe.getStdOutput();
    if (stdOutput != null && stdOutput.length < 1) {
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new IOException("groupframe returned no data");
    }
    //  Parse the size of the data
    //  Note the initial space in the string below
    String outputLine = stdOutput[0].trim();
    String[] tokens = outputLine.split("\\s+");
    if (tokens.length < 2) {
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new IOException("groupframe returned less than two outputs");
    }
    firstOutput.set(tokens[0]);
    secondOutput.set(tokens[1]);
    if (!firstOutput.isValid() || firstOutput.isNull()) {
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new NumberFormatException("firstOutput is not set, token is "
          + tokens[0] + "\n" + firstOutput.getInvalidReason());
    }
    if (!secondOutput.isValid() || secondOutput.isNull()) {
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new NumberFormatException("secondOutput is not set, token is "
          + tokens[1] + "\n" + secondOutput.getInvalidReason());
    }
    Utilities.timestamp("run", "goodframe", Utilities.FINISHED_STATUS);
  }
  
  /**
   * 
   * @return
   */
  public ConstEtomoNumber getFirstOutput() {
    return firstOutput;
  }
  
  /**
   * 
   * @return
   */
  public ConstEtomoNumber getSecondOutput() {
    return secondOutput;
  }
}
/**
* <p> $Log$
* <p> Revision 1.5  2005/08/27 22:43:40  sueh
* <p> bug# 532 In Utilities.timestamp() change the int status to String status,
* <p> since it doesn't have to be compared.
* <p>
* <p> Revision 1.4  2005/07/29 00:55:12  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.3  2005/06/17 20:03:42  sueh
* <p> bug# 685 Added timestamps to run().
* <p>
* <p> Revision 1.2  2005/04/25 21:42:34  sueh
* <p> bug# 615 Passing the axis where a command originates to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.
* <p>
* <p> Revision 1.1  2005/03/29 19:55:49  sueh
* <p> bug# 623 Class to run goodframe command.
* <p> </p>
*/