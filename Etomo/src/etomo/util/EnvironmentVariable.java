package etomo.util;

import java.util.HashMap;

import etomo.BaseManager;
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

  public static final String CALIB_DIR = "IMOD_CALIB_DIR";
  public static final String PARTICLE_DIR = "PARTICLE_DIR";

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
  public String getValue(BaseManager manager, String propertyUserDir, String varName,
      AxisID axisID) {
    String value = "";
    // prevent multiple reads and writes at the same time
    synchronized (variableList) {
      if (variableList.containsKey(varName)) {
        value = (String) variableList.get(varName);
        if (value == null) {
          return "";
        }
        return value;
      }
    }
    // There is not a real good way to access the system environment variables
    // since the primary method was deprecated
    SystemProgram readEnvVar;
    if (Utilities.isWindowsOS()) {
      String var = "%" + varName + "%";
      readEnvVar = new SystemProgram(manager, propertyUserDir, new String[] { "cmd.exe",
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
        // if the variable isn't set, echo will return the string sent to it
        if (!stdout[0].equals(var)) {
          value = stdout[0];
        }
      }
    }
    // Non windows environment
    else {
      readEnvVar = new SystemProgram(manager, propertyUserDir, new String[] { "env" },
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
    // prevent multiple reads and writes at the same time
    synchronized (variableList) {
      variableList.put(varName, value);
    }
    return value;
  }

  /**
   * Return true if an environment variable value exists.  Doesn't use
   * variableList because getValue doesn't distinguish between an empty env var
   * and a non-existant one.  If unable to check, returns false.
   * 
   * @param varName
   * @return boolean
   */
  public boolean exists(BaseManager manager, String propertyUserDir, String varName,
      AxisID axisID) {
    // There is not a real good way to access the system environment variables
    // since the primary method was deprecated
    SystemProgram readEnvVar;
    if (Utilities.isWindowsOS()) {
      String var = "%" + varName + "%";
      readEnvVar = new SystemProgram(manager, propertyUserDir, new String[] { "cmd.exe",
          "/C", "echo", var }, axisID);
      try {
        readEnvVar.run();
      }
      catch (Exception excep) {
        excep.printStackTrace();
        System.err.println(excep.getMessage());
        System.err.println("Unable to run cmd command to find " + varName
            + " environment variable");

        return false;
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
        // if the variable isn't set, echo will return the string sent to it
        if (!stdout[0].equals(var)) {
          return true;
        }
      }
    }
    // Non windows environment
    else {
      readEnvVar = new SystemProgram(manager, propertyUserDir, new String[] { "env" },
          axisID);
      try {
        readEnvVar.run();
      }
      catch (Exception excep) {
        excep.printStackTrace();
        System.err.println(excep.getMessage());
        System.err.println("Unable to run env command to find " + varName
            + " environment variable");

        return false;
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
          return true;
        }
      }
    }
    return false;
  }
}
/**
 * <p>
 * $Log$
 * Revision 1.6  2010/02/17 05:05:58  sueh
 * bug# 1301 Using manager instead of manager key for popping up
 * messages.
 *
 * Revision 1.5  2009/06/10 17:27:05  sueh
 * bug# 1202 Added exists.
 *
 * Revision 1.4  2009/03/17 00:46:43  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 1.3  2006/07/21 22:24:47  sueh
 * bug# 901 Added IMOD_CALIB_DIR constant
 *
 * Revision 1.2  2006/07/03 23:35:36  sueh
 * Using the Utilities.isWindows() function instead of testing for windows here.
 *
 * <p>
 * Revision 1.1 2006/06/30 16:30:12 sueh
 * <p>
 * bug# 883 Added a class to get and store environment variables.
 * <p>
 * </p>
 */
