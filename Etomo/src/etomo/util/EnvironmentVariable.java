package etomo.util;

import java.util.HashMap;

import etomo.process.SystemProgram;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class EnvironmentVariable {
  public static final String rcsid = "$Id$";

  public static final EnvironmentVariable INSTANCE = new EnvironmentVariable();

  private final HashMap variableList = new HashMap();

  private EnvironmentVariable() {
  }

  /**
   * Return an environment variable value
   * 
   * @param varName
   * @return String
   */
  public String getValue(String propertyUserDir, String varName, AxisID axisID) {
    String value = "";
    //prevent multiple reads and writes at the same time
    synchronized (variableList) {
      if (variableList.containsKey(varName)) {
        value = (String) variableList.get(varName);
        if (value == null) {
          return "";
        }
        return value;
      }
    }
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram readEnvVar;
    String osName = System.getProperty("os.name");

    if (osName.startsWith("Windows")) {
      String var = "%" + varName + "%";
      readEnvVar = new SystemProgram(propertyUserDir, new String[] { "cmd.exe",
          "/C", "echo", var }, axisID);
      try {
        readEnvVar.run();
      }
      catch (Exception excep) {
        excep.printStackTrace();
        System.err.println(excep.getMessage());
        System.err.println("Unable to run cmd command to find " + varName
            + " environment variable");

        return "";
      }
      String[] stderr = readEnvVar.getStdError();
      if (stderr != null && stderr.length > 0) {
        System.err.println("Error running 'cmd.exe' command");
        for (int i = 0; i < stderr.length; i++) {
          System.err.println(stderr[i]);
        }
      }
      // Return the first line from the command
      String[] stdout = readEnvVar.getStdOutput();
      if (stdout != null && stdout.length > 0) {
        //if the variable isn't set, echo will return the string sent to it
        if (!stdout[0].equals(var)) {
          value = stdout[0];
        }
      }
    }
    //  Non windows environment
    else {
      readEnvVar = new SystemProgram(propertyUserDir, new String[] { "env" },
          axisID);
      try {
        readEnvVar.run();
      }
      catch (Exception excep) {
        excep.printStackTrace();
        System.err.println(excep.getMessage());
        System.err.println("Unable to run env command to find " + varName
            + " environment variable");

        return "";
      }
      String[] stderr = readEnvVar.getStdError();
      if (stderr.length > 0) {
        System.err.println("Error running 'env' command");
        for (int i = 0; i < stderr.length; i++) {
          System.err.println(stderr[i]);
        }
      }

      // Search through the evironment string array to find the request
      // environment variable
      String searchString = varName + "=";
      int nChar = searchString.length();
      String[] stdout = readEnvVar.getStdOutput();
      for (int i = 0; i < stdout.length; i++) {
        if (stdout[i].indexOf(searchString) == 0) {
          value = stdout[i].substring(nChar);
          break;
        }
      }
    }
    //prevent multiple reads and writes at the same time
    synchronized (variableList) {
      variableList.put(varName, value);
    }
    return value;
  }
}
/**
 * <p> $Log$ </p>
 */
