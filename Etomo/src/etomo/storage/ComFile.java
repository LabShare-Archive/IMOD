package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import etomo.BaseManager;
import etomo.type.AxisID;

/**
* <p>Description: Reads from one .com file at a time.</p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class ComFile {
  public static final String rcsid = "$Id:$";

  private final AxisID axisID;
  private final BaseManager manager;
  private final String subdirectory;

  private LogFile logFile = null;
  private String comFileName = null;
  private String programName = null;

  public ComFile(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    subdirectory = null;
  }

  public ComFile(final BaseManager manager, final AxisID axisID, final String subdirectory) {
    this.manager = manager;
    this.axisID = axisID;
    this.subdirectory = subdirectory;
  }

  /**
   * @param input
   * @return true if comFileName has been set and is the same as input
   */
  public boolean equalsComFileName(final String input) {
    if (comFileName == null) {
      return false;
    }
    return comFileName.equals(input);
  }

  /**
   * @param input
   * @return true if programName has been set and is the same as input
   */
  public boolean equalsProgramName(final String input) {
    if (programName == null) {
      return false;
    }
    return programName.equals(input);
  }

  /**
   * Sets comFileName.  Closes the current reader, if it exists, and resets to null the
   * other member variables involved with reading.
   * @param input
   */
  public void setComFileName(final String input) {
    if (!equalsComFileName(input)) {
      logFile = null;
      comFileName = input;
      programName = null;
    }
  }

  /**
   * Attempts to find an instance of programName in the .com file.  Opens the file for
   * reading and searches for the program name.  If it finds the program name it fills
   * a map with parameter names/values and returns it.
   * @param programName
   * @param errmsg
   * @return
   */
  public Map<String, String> getCommandMap(final String programName,
      final StringBuffer errmsg) {
    this.programName = programName;
    // Create LogFile instance if is hasn't already been created
    if (logFile == null) {
      String dirPath;
      if (subdirectory != null) {
        dirPath = new File(manager.getPropertyUserDir(), subdirectory).getAbsolutePath();
      }
      else {
        dirPath = manager.getPropertyUserDir();
      }
      File file = new File(dirPath, comFileName + axisID.getExtension() + ".com");
      try {
        logFile = LogFile.getInstance(file);
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        errmsg.append(e.getMessage());
        return null;
      }
    }
    Map<String, String> commandMap = null;
    LogFile.ReaderId id = null;
    try {
      try {
        id = logFile.openReader();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        errmsg.append("unable to open " + logFile.getAbsolutePath() + "\n"
            + e.getMessage());
        return null;
      }
      catch (FileNotFoundException e) {
        // Not every com file requested will exist
        return null;
      }
      // Find the command.
      String line = null;
      while ((line = logFile.readLine(id)) != null
          && !line.matches("\\$\\Q" + programName + "\\E\\s+\\Q-StandardInput\\E")) {
      }
      if (line != null) {
        // Load the command's standard input parameters.
        commandMap = new HashMap<String, String>();
        while ((line = logFile.readLine(id)) != null) {
          line = line.trim();
          if (line.startsWith("$")) {
            // End of command
            break;
          }
          if (line.equals("") || line.startsWith("#")) {
            // Ignoring comments and empty lines
            continue;
          }
          // Load the parameter
          String[] array = line.split("\\s+");
          if (array == null || array.length < 1) {
            continue;
          }
          String key = array[0];
          if (array.length == 1) {
            // Parameter has no value

            commandMap.put(key, null);
          }
          else {
            // Parameter has a value
            commandMap.put(key, line.substring(key.length(), line.length()).trim());
          }
        }
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      errmsg
          .append("unable to read " + logFile.getAbsolutePath() + "\n" + e.getMessage());
    }
    catch (IOException e) {
      e.printStackTrace();
      errmsg
          .append("unable to read " + logFile.getAbsolutePath() + "\n" + e.getMessage());
    }
    if (id != null) {
      logFile.closeRead(id);
    }
    return commandMap;
  }
}
