/**
 * <p>Description: A class containing utility methods.</p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$
 *
 * <p> $$Log$
 * <p> $Revision 3.24  2005/07/29 00:56:32  sueh
 * <p> $bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> $because the current manager changes when the user changes the tab.
 * <p> $Passing the manager where its needed.
 * <p> $
 * <p> $Revision 3.23  2005/06/21 20:34:03  sueh
 * <p> $bug# 522 Changed getFile(String) to handle absolute filenames from
 * <p> $windows which contain the drive letter.
 * <p> $
 * <p> $Revision 3.22  2005/06/21 00:56:52  sueh
 * <p> $bug# 522 Added isWindowsOS().  Added getFile(AxisID, String extension),
 * <p> $Added getFile(String filename).
 * <p> $
 * <p> $Revision 3.21  2005/06/20 17:09:31  sueh
 * <p> $bug# 522 Added isSelfTest().
 * <p> $
 * <p> $Revision 3.20  2005/06/17 20:02:21  sueh
 * <p> $bug# 685 Added timestamp functions for ComScript and File types.
 * <p> $Added code to the main timestamp function to strip the path from a file
 * <p> $name.  These changes reduces the amount of timestamp related code
 * <p> $being executed when debug is off.
 * <p> $
 * <p> $Revision 3.19  2005/06/17 19:18:04  sueh
 * <p> $bug# 685 Put all timestamp functionality into one function.  Added
 * <p> $buttonTimestamp to provide an interface to the main timestamp function.
 * <p> $
 * <p> $Revision 3.18  2005/06/17 17:49:18  sueh
 * <p> $bug# 685 Added getTimestamp() and setStartTime() to get a relative
 * <p> $timestamp.
 * <p> $
 * <p> $Revision 3.17  2005/06/17 16:48:07  sueh
 * <p> $bug# 685 Changed timestamp to milliseconds number.
 * <p> $
 * <p> $Revision 3.16  2005/06/17 00:36:01  sueh
 * <p> $bug# 685 Added timestamp functions and isDebug().
 * <p> $
 * <p> $Revision 3.15  2005/05/31 23:12:11  sueh
 * <p> $bug# 667 Changed EtomoDirector.getCurrentMetaData() to
 * <p> $getCurrentName().
 * <p> $
 * <p> $Revision 3.14  2005/05/26 21:09:44  rickg
 * <p> $Print out file name with error dialog
 * <p> $
 * <p> $Revision 3.13  2005/05/18 22:49:50  sueh
 * <p> $bug# 662 Changed Utilities.fileExists() to get metaData from
 * <p> $EtomoDirector, instead of receiving it as a parameter.  Moved getFile()
 * <p> $from ApplicationManager to this class.
 * <p> $
 * <p> $Revision 3.12  2005/04/25 21:44:27  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 3.11  2004/11/20 00:15:34  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
 * <p> $Revision 3.10.2.6  2004/11/15 22:26:49  sueh
 * <p> $bug# 520 Moved all of file validation to Utilities so that is called be called
 * <p> $from other places.
 * <p> $
 * <p> $Revision 3.10.2.5  2004/10/29 01:23:27  sueh
 * <p> $bug# 520 Added isValidFile().
 * <p> $
 * <p> $Revision 3.10.2.4  2004/10/28 17:09:38  sueh
 * <p> $bug# 520 Adding mostRecentFile.
 * <p> $
 * <p> $Revision 3.10.2.3  2004/10/11 02:29:06  sueh
 * <p> $bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> $property.  This property would need a different value for each manager.
 * <p> $This variable can be retrieved from the manager if the object knows its
 * <p> $manager.  Otherwise it can retrieve it from the current manager using the
 * <p> $EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> $gets the value from the "user.dir" property.
 * <p> $
 * <p> $Revision 3.10.2.2  2004/10/08 16:41:57  sueh
 * <p> $bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> $member variables non-static.
 * <p> $
 * <p> $Revision 3.10.2.1  2004/09/03 21:19:31  sueh
 * <p> $bug# 520 calling functions from EtomoDirector instead of
 * <p> $ApplicationManager
 * <p> $
 * <p> $Revision 3.10  2004/08/06 23:11:30  sueh
 * <p> $bug# 508 added a writeFile() function, which writes an array
 * <p> $of strings to a file.  If newFile is true, it will call Utilities.renameFile(),
 * <p> $and then write the strings to a new, empty file.
 * <p> $
 * <p> $Revision 3.9  2004/07/16 23:01:27  sueh
 * <p> $bug# 501 sending System.out prints only when debug is set
 * <p> $
 * <p> $Revision 3.8  2004/07/13 17:26:50  sueh
 * <p> $bug# 429 make fix global
 * <p> $
 * <p> $Revision 3.7  2004/04/28 19:57:30  rickg
 * <p> $bug #429 Created file rename function to handle windows bug
 * <p> $
 * <p> $Revision 3.6  2004/04/26 23:32:13  rickg
 * <p> $Checked for null buffers, because nio does work on 2.4 kernels
 * <p> $not on 2.6 kernel yet
 * <p> $
 * <p> $Revision 3.5  2004/04/26 23:19:20  rickg
 * <p> $Added buffering to the non nio file copy
 * <p> $
 * <p> $Revision 3.4  2004/04/22 23:23:04  rickg
 * <p> $Added copyFile
 * <p> $Modified calling parameter in fileExists
 * <p> $
 * <p> $Revision 3.3  2004/04/08 19:12:10  rickg
 * <p> $Added millisToMinAndSecs method
 * <p> $
 * <p> $Revision 3.2  2003/12/05 01:24:23  sueh
 * <p> $bug242 moved getEnvironmentVariable() to Utilities
 * <p> $
 * <p> $Revision 3.1  2003/11/27 00:05:28  rickg
 * <p> $Added debugPrint static method
 * <p> $
 * <p> $Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> $Version 1.0.0
 * <p> $
 * <p> $Revision 1.2  2003/10/10 23:17:01  sueh
 * <p> $bug251 removing marks
 * <p> $
 * <p> $Revision 1.1  2003/10/07 22:43:13  sueh
 * <p> $bug251 moved transferfid from fine alignment dialog
 * <p> $to fiducial model dialog
 * <p> $$</p>
 */

package etomo.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.text.DecimalFormat;
import java.util.Date;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.ui.UIHarness;
import etomo.comscript.ComScript;
import etomo.process.SystemProgram;

public class Utilities {
  
  private static boolean retrievedDebug = false;
  private static boolean debug = false;
  private static boolean retrievedSelfTest = false;
  private static boolean selfTest = false;
  private static boolean retrievedOS = false;
  private static boolean windowsOS = false;
  private static long startTime = 0;
  private static DecimalFormat timestampFormat = new DecimalFormat(".000");
  
  private Utilities() {
  }

  /**
   * Convert milliseconds into a string of the format Minutes:Seconds
   * @param milliseconds
   * @return
   */
  public static String millisToMinAndSecs(double milliseconds) {
    int minutes = (int) Math.floor(milliseconds / 60000);
    int seconds = (int) Math.floor((milliseconds - minutes * 60000) / 1000.0);
    String strSeconds = "";
    //  Add aleading zero if less than 10 seconds
    if (seconds < 10) {
      strSeconds = "0";
    }
    strSeconds += String.valueOf(seconds);
    return String.valueOf(minutes) + ":" + strSeconds;
  }

  /**
   * Check see if the particular dataset file exists
   * @param metaData
   * @param extension
   * @param axisID
   * @return true if the file exist
   */
  public static boolean fileExists(BaseManager manager, String extension, AxisID axisID) {
    File file = new File(manager.getPropertyUserDir(), manager.getBaseMetaData().getName()
        + axisID.getExtension() + extension);
    if (file.exists()) {
      return true;
    }
    return false;
  }
  
  /**
   * Creates a file name and a file.  If the file doesn't exist and mustExist is
   * true, it complains and returns null, otherwise it returns the file.
   * @param mustExist
   * @param axisID
   * @param extension
   * @param fileType A string used in the error dialog
   * @return
   */
  public static File getFile(BaseManager manager, boolean mustExist, AxisID axisID,
      String extension, String fileDescription) {
    File file = new File(manager.getPropertyUserDir(), manager
        .getBaseMetaData().getName()
        + axisID.getExtension() + extension);
    if (!file.exists() && mustExist) {
      UIHarness.INSTANCE.openMessageDialog("The " + fileDescription + " file: "
          + file.getAbsolutePath() + " doesn't exist.", "Missing "
          + fileDescription, axisID);
      return null;
    }
    return file;
  }
  
  public static File getFile(BaseManager manager, AxisID axisID,
      String extension) {
    File file = new File(manager.getPropertyUserDir(), manager
        .getBaseMetaData().getName()
        + axisID.getExtension() + extension);
    return file;
  }

  public static File getFile(String propertyUserDir, String filename) {
    filename = filename.trim();
    if (filename == null || filename.matches("\\s*")) {
      return new File(propertyUserDir);
    }
    if (filename.charAt(0) == File.separatorChar) {
      return new File(filename);
    }
    if (isWindowsOS()) {
      int driveIndex = filename.indexOf(':');
      if (driveIndex != -1 && driveIndex < filename.length() - 1
          && filename.charAt(driveIndex + 1) == File.separatorChar) {
        return new File(filename);
      }
    }
    return new File(propertyUserDir, filename);
  }

  /**
   * Rename a file working around the Windows bug
   * This need serious work arounds because of the random failure bugs on
   * windows.  See sun java bugs: 4017593, 4017593, 4042592
   */
  public static void renameFile(File source, File destination)
      throws IOException {
    if (!source.exists()) {
      return;
    }
    // Delete the existing backup file if it exists, otherwise the call will
    // fail on windows 
    if (destination.exists()) {
      Utilities.debugPrint(destination.getAbsolutePath() + " exists, deleting");
      if (!destination.delete()) {
        System.err.println("Unable to delete destination file: "
            + destination.getAbsolutePath());
        if (destination.exists()) {
          System.err.println(destination.getAbsolutePath() + " still exists!");
        }
        else {
          System.err
            .println(destination.getAbsolutePath() + " does not exist!");
        }
      }
    }

    // Rename the existing log file
    if (source.exists()) {
      Utilities.debugPrint(source.getAbsolutePath() + " exists");

      if (!source.renameTo(destination)) {
        if (source.exists()) {
          System.err.println(source.getAbsolutePath() + " still exists");
        }
        else {
          System.err.println(source.getAbsolutePath() + " does not exist!");
        }

        if (destination.exists()) {
          System.err.println(destination.getAbsolutePath() + " still exists!");
        }
        else {
          System.err.println(destination.getAbsolutePath() + " does not exist");
        }
        System.err.println("Unable to rename log file to: "
            + destination.getAbsolutePath());
        String message = "Unable to rename " + source.getAbsolutePath()
            + " to " + destination.getAbsolutePath();

        throw (new IOException(message));
      }
    }
  }
  
  public static File mostRecentFile(String propertyUserDir, String file1Name,
      String file2Name, String file3Name, String file4Name) {
    File file1 = null;
    File file2 = null;
    File file3 = null;
    File file4 = null;
    if (file1Name != null) {
      file1 = new File(propertyUserDir, file1Name);
    }
    if (file2Name != null) {
      file2 = new File(propertyUserDir, file2Name);
    }
    if (file3Name != null) {
      file3 = new File(propertyUserDir, file3Name);
    }
    if (file4Name != null) {
      file4 = new File(propertyUserDir, file4Name);
    }
    long file1Time = 0;
    long file2Time = 0;
    long file3Time = 0;
    long file4Time = 0;
    if (file1 != null && file1.exists()) {
      file1Time = file1.lastModified();
    }
    if (file2 != null && file2.exists()) {
      file2Time = file2.lastModified();
    }
    if (file3 != null && file3.exists()) {
      file3Time = file3.lastModified();
    }
    if (file4 != null && file4.exists()) {
      file4Time = file4.lastModified();
    }
    if (file1Time >= file2Time && file1Time >= file3Time && file1Time >= file4Time) {
      return file1;
    }
    if (file2Time >= file3Time && file2Time >= file4Time) {
      return file2;
    }
    if (file3Time >= file4Time) {
      return file3;
    }
    return file4;
  }

  /**
   * Copy a file using the fastest method available.
   */
  public static void copyFile(File source, File destination) throws IOException {
    // Try using the nio method but if it fails fall back to BufferedFileReader/
    // BufferedFileWriter approach
    FileInputStream sourceStream = new FileInputStream(source);
    FileOutputStream destStream = new FileOutputStream(destination);
    BufferedInputStream sourceBuffer = null;
    BufferedOutputStream destBuffer = null;
    FileChannel sourceChannel = null;
    FileChannel destChannel = null;
    try {
      sourceChannel = sourceStream.getChannel();
      destChannel = destStream.getChannel();

      sourceChannel.transferTo(0L, sourceChannel.size(), destChannel);
    }

    catch (IOException exception) {
      // Buffer the stream for performance
      sourceBuffer = new BufferedInputStream(sourceStream);
      destBuffer = new BufferedOutputStream(destStream);
      int byteIn;
      while ((byteIn = sourceBuffer.read()) != -1)
        destBuffer.write(byteIn);
    }

    //  TODO: does each object need to be closed indivudually
    if (sourceBuffer != null) {
      sourceBuffer.close();
    }
    sourceStream.close();
    if (destBuffer != null) {
      destBuffer.close();
    }
    destStream.close();
  }

  /**
   * Print out the specified string to err if the debug flag is set
   *
   * @param string
   */
  static public void debugPrint(String string) {
    debugPrint(string, false);
  }
  
  /**
   * Print out the specified string to err or out if the debug flag is set
   *
   * @param string
   */
  static public void debugPrint(String string, boolean toOut) {
    if (EtomoDirector.getInstance().isDebug()) {
      if (toOut) {
        System.out.println(string);
      }
      else {
        System.err.println(string);
      }
    }
  }


  /**
   * Return an environment variable value
   * 
   * @param varName
   * @return String
   */
  static public String getEnvironmentVariable(String propertyUserDir,
      String varName, AxisID axisID) {
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram readEnvVar;
    String osName = System.getProperty("os.name");

    if (osName.startsWith("Windows")) {
      readEnvVar = new SystemProgram(propertyUserDir, "cmd.exe /C echo %" + varName
          + "%", axisID);
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
      if (stderr.length > 0) {
        System.err.println("Error running 'cmd.exe' command");
        for (int i = 0; i < stderr.length; i++) {
          System.err.println(stderr[i]);
        }
      }

      // Return the first line from the command
      String[] stdout = readEnvVar.getStdOutput();
      if (stdout.length > 0) {
        return stdout[0];
      }
    }

    //  Non windows environment
    else {

      readEnvVar = new SystemProgram(propertyUserDir, "env", axisID);
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
          return stdout[i].substring(nChar);
        }
      }
    }
    return "";
  }

  /**
   * 
   * @param file
   * @param strings
   * @throws IOException
   */  
  public static void writeFile(File file, String[] strings, boolean newFile)
    throws IOException {
    if (file == null) {
      throw new IOException();
    }
    if (newFile) {
      Utilities.renameFile(
        file,
        new File(file.getAbsolutePath() + "~"));
    }
    if (strings == null || strings.length == 0) {
      return;
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(file));
    for (int i = 0; i < strings.length; i++) {
      bufferedWriter.write(strings[i]);
      bufferedWriter.newLine();
    }
    if (bufferedWriter != null) {
      bufferedWriter.close();
    }
  }
  
  /**
   * validates a file and appends failure reason to invalidReason
   * @param file
   * @param invalidReason - must not be null
   * @param exists
   * @param canRead
   * @param canWrite
   * @param isDirectory
   * @return
   */
  public static boolean isValidFile(File file, String fileDescription,
      StringBuffer invalidReason, boolean exists, boolean canRead,
      boolean canWrite, boolean isDirectory) {
    boolean isValid = true;
    if (file == null) {
      if (fileDescription != null && !fileDescription.matches("\\s+")) {
        invalidReason.append(fileDescription + " was not entered.\n");
      } 
      else if (isDirectory) {
        invalidReason.append("No directory name was entered.\n");
      }
      else {
        invalidReason.append("No file name was entered.\n");
      }
      return false;
    }
    if (exists && !file.exists()) {
      invalidReason.append(file.getAbsolutePath() + " must exist.\n");
      isValid = false;
    }
    if (canRead && !file.canRead()) {
      invalidReason.append(file.getAbsolutePath() + " must be readable.\n");
      isValid = false;
    }
    if (canWrite && !file.canWrite()) {
      invalidReason.append(file.getAbsolutePath() + " must be writable.\n");
      isValid = false;
    }
    if (isDirectory && !file.isDirectory()) {
      invalidReason.append(file.getAbsolutePath() + " must be a directory.\n");
      isValid = false;
    }
    if (!isDirectory && file.isDirectory()) {
      invalidReason.append(file.getAbsolutePath() + " must be a file.\n");
      isValid = false;
    }
    return isValid;
  }
  
  /**
   * Print timestamp in error log
   * @param command
   */
  public static void buttonTimestamp(String command) {
    timestamp("PRESSED", command, (String) null, -100);
  }

  /**
   * Print timestamp in error log
   * @param command
   * @param container
   */
  public static void buttonTimestamp(String command, String container) {
    timestamp("PRESSED", command, container, -100);
  }

  /**
   * Print timestamp in error log
   * @param process
   * @param container
   * @param status
   */
  public static void timestamp(String process, String container, int status) {
    timestamp(process, null, container, status);
  }
  
  public static void timestamp(String process, ProcessName container, int status) {
    timestamp(process, null, container.toString(), status);
  }
  
  /**
   * Print timestamp in error log
   * @param process
   * @param command
   * @param container
   * @param status
   */
  public static void timestamp(String process, String command, File container,
      int status) {
    if (!isDebug()) {
      return;
    }
    timestamp(process, command, container.getName(), status);
  }
  
  /**
   * Print timestamp in error log
   * @param process
   * @param command
   * @param container
   * @param status
   */
  public static void timestamp(String process, String command, ComScript container,
      int status) {
    if (!isDebug()) {
      return;
    }
    timestamp(process, command, container.getName(), status);
  }

  /**
   * Print timestamp in error log
   * @param process
   * @param command
   * @param container
   * @param status 0 = started, 1 = finished, -1 = failed, or -100 = null
   */
  public static void timestamp(String process, String command,
      String container, int status) {
    if (!isDebug()) {
      return;
    }
    String statusString = null;
    switch (status) {
    case 0:
      statusString = " started";
      break;
    case 1:
      statusString = "finished";
      break;
    case -1:
      statusString = "  failed";
      break;
    case -100:
      break;
    default:
      throw new IllegalStateException("bad status: status=" + status);
    }
    StringBuffer buffer = new StringBuffer("TIMESTAMP: ");
    if (process != null) {
      buffer.append(process + " ");
    }
    if (command != null) {
      buffer.append(command + " ");
      if (container != null) {
        buffer.append("in ");
      }
    }
    if (container != null) {
      int separatorIndex = container.lastIndexOf(File.separatorChar);
      if (separatorIndex != -1 && separatorIndex < container.length() - 1) {
        container = container.substring(separatorIndex + 1);
      }
      buffer.append(container + " ");
    }
    if (statusString != null) {
      buffer.append(statusString + " ");
    }
    buffer.append("at " + getTimestamp());
    System.err.println(buffer);
  }

  public static boolean isDebug() {
    if (!retrievedDebug) {
      debug = EtomoDirector.getInstance().isDebug();
      retrievedDebug = true;
    }
    return debug;
  }
  
  public static boolean isSelfTest() {
    if (!retrievedSelfTest) {
      selfTest = EtomoDirector.getInstance().isSelfTest();
      retrievedSelfTest = true;
    }
    return selfTest;
  }
  
  public static void setStartTime() {
    startTime = new Date().getTime();
  }

  public static String getTimestamp() {
    return timestampFormat.format((new Date().getTime() - startTime) / 1000.0);
  }

  public static boolean isWindowsOS() {
    if (!retrievedOS) {
      String osName = System.getProperty("os.name").toLowerCase();
      windowsOS = osName.indexOf("windows") != -1;
    }
    return windowsOS;
  }
}