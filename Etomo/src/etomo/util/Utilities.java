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

import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.type.AxisID;
import etomo.process.SystemProgram;

public class Utilities {
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
  static public boolean fileExists(ConstMetaData metaData, String extension,
      AxisID axisID) {
    String workingDirectory = System.getProperty("user.dir");
    File file = new File(workingDirectory, metaData.getDatasetName()
        + axisID.getExtension() + extension);
    if (file.exists()) {
      return true;
    }
    return false;
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
    if (ApplicationManager.isDebug()) {
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
  static public String getEnvironmentVariable(String varName) {
    //  There is not a real good way to access the system environment variables
    //  since the primary method was deprecated
    SystemProgram readEnvVar;
    String osName = System.getProperty("os.name");

    if (osName.startsWith("Windows")) {
      readEnvVar = new SystemProgram("cmd.exe /C echo %" + varName + "%");
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

      readEnvVar = new SystemProgram("env");
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
}