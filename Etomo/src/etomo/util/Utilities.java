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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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
    if(sourceBuffer != null) {
      sourceBuffer.close();
    }
    sourceStream.close();
    if(destBuffer != null) {
      destBuffer.close();
    }
    destStream.close();
  }

  /**
   * Print out the specified string if the debug flag is set
   *
   * @param string
   */
  static public void debugPrint(String string) {
    if (ApplicationManager.isDebug()) {
      System.err.println(string);
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

}