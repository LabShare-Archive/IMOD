package etomo.util;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.process.ProcessMessages;
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

  private final AxisID axisID;
  private final String propertyUserDir;

  private EtomoNumber[] output;

  public Goodframe(String propertyUserDir, AxisID axisID) {
    this.propertyUserDir = propertyUserDir;
    this.axisID = axisID;
  }

  public void run(BaseManager manager, int firstInput, int secondInput)
      throws IOException, InvalidParameterException {
    run(manager, new String[] { Integer.toString(firstInput),
        Integer.toString(secondInput) });
  }

  /**
   * 
   * @throws IOException
   * @throws InvalidParameterException
   */
  public void run(BaseManager manager, String[] input) throws IOException,
      InvalidParameterException, NumberFormatException {
    Utilities.timestamp("run", "goodframe", Utilities.STARTED_STATUS);
    //Run the goodframe command.
    String[] commandArray = new String[input.length + 1];
    commandArray[0] = ApplicationManager.getIMODBinPath() + "goodframe";
    for (int i = 0; i < input.length; i++) {
      commandArray[i + 1] = input[i];
    }
    SystemProgram groupframe = new SystemProgram(manager, propertyUserDir, commandArray,
        axisID);
    groupframe.run();

    if (groupframe.getExitValue() != 0) {
      ProcessMessages messages = groupframe.getProcessMessages();
      if (messages.errorListSize() > 0) {
        String message = "groupframe returned an error:\n";
        for (int i = 0; i < messages.errorListSize(); i++) {
          message = message + messages.getError(i) + "\n";
        }
        Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
        throw new InvalidParameterException(message);
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
    if (tokens.length < input.length) {
      Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
      throw new IOException("groupframe returned less than " + input.length + " outputs");
    }
    output = new EtomoNumber[input.length];
    for (int i = 0; i < output.length; i++) {
      output[i] = new EtomoNumber();
      output[i].set(tokens[i]);
      if (!output[i].isValid() || output[i].isNull()) {
        Utilities.timestamp("run", "goodframe", Utilities.FAILED_STATUS);
        throw new NumberFormatException("Output " + i + " is not set, token is "
            + tokens[i] + "\n" + output[i].getInvalidReason());
      }
    }
    Utilities.timestamp("run", "goodframe", Utilities.FINISHED_STATUS);
  }

  /**
   * @return output[index] if it exists, otherwise null
   */
  public ConstEtomoNumber getOutput(int index) {
    if (output == null || output.length < index + 1) {
      return null;
    }
    return output[index];
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.14  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 1.13  2009/03/17 00:46:43  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.12  2009/02/26 17:24:59  sueh
 * <p> bug# 1184 Changed Goodframe so that it can handle any number of
 * <p> inputs and outputs.
 * <p>
 * <p> Revision 1.11  2007/12/26 22:41:13  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.10  2007/09/07 00:30:35  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.9  2007/02/05 23:47:04  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.8  2005/11/02 22:15:37  sueh
 * <p> bug# 754 Parsing errors and warnings inside ProcessMessages.
 * <p>
 * <p> Revision 1.7  2005/10/28 18:57:59  sueh
 * <p> bug# 747 Standardizing SystemProgram message parsing.
 * <p>
 * <p> Revision 1.6  2005/09/09 21:48:06  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
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
