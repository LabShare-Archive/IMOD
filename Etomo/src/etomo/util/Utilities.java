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
 * <p> $Revision 3.80  2011/06/28 20:02:20  sueh
 * <p> $Removed test print from isWindowsOS.
 * <p> $
 * <p> $Revision 3.79  2011/06/28 16:57:11  sueh
 * <p> $In windowsOS added print statement during test.
 * <p> $
 * <p> $Revision 3.78  2011/06/28 02:34:00  sueh
 * <p> $Bug# 1501 Corrected error message.
 * <p> $
 * <p> $Revision 3.77  2011/02/22 21:54:38  sueh
 * <p> $bug# 1437 Reformatting.
 * <p> $
 * <p> $Revision 3.76  2010/11/13 16:08:59  sueh
 * <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p> $
 * <p> $Revision 3.75  2010/05/20 23:55:19  sueh
 * <p> $bug# 1360 Adding new versions off checkExistingDir and getExistingDir
 * <p> $that allow a custom failure message.
 * <p> $
 * <p> $Revision 3.74  2010/04/28 16:51:17  sueh
 * <p> $bug# 1344 Added backFile.
 * <p> $
 * <p> $Revision 3.73  2010/04/09 15:46:52  sueh
 * <p> $bug# 1353 Removed null pointer exception problem in getFile(String,String).
 * <p> $
 * <p> $Revision 3.72  2010/03/12 04:28:30  sueh
 * <p> $bug# 1325 Added deleteFileType.
 * <p> $
 * <p> $Revision 3.71  2010/03/09 22:09:12  sueh
 * <p> $bug# 1325 Added getStackBinning.
 * <p> $
 * <p> $Revision 3.70  2010/02/26 20:38:42  sueh
 * <p> $Changing the complex popup titles are making it hard to complete the
 * <p> $uitests.
 * <p> $
 * <p> $Revision 3.69  2010/02/23 20:32:47  sueh
 * <p> $Removed unnecessary stack trace print in rename.
 * <p> $
 * <p> $Revision 3.68  2010/02/17 05:05:58  sueh
 * <p> $bug# 1301 Using manager instead of manager key for popping up
 * <p> $messages.
 * <p> $
 * <p> $Revision 3.67  2009/09/02 22:47:07  sueh
 * <p> $bug# 1254 Added isValidStack.
 * <p> $
 * <p> $Revision 3.66  2009/06/10 17:28:44  sueh
 * <p> $bug# 1202 Corrected comment.
 * <p> $
 * <p> $Revision 3.65  2009/03/17 00:46:43  sueh
 * <p> $bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p> $
 * <p> $Revision 3.64  2009/02/27 03:54:54  sueh
 * <p> $bug# 1188 Removed unnecessary Java version check.
 * <p> $
 * <p> $Revision 3.63  2009/02/04 23:38:03  sueh
 * <p> $bug# 1158 Added getDataTimeStamp.
 * <p> $
 * <p> $Revision 3.62  2009/01/20 20:34:32  sueh
 * <p> $bug# 1102 Changed convertLabelToName(String,ProcessName) to
 * <p> $convertLabelToName(String,String,String).
 * <p> $
 * <p> $Revision 3.61  2008/12/10 18:35:02  sueh
 * <p> $bug# 1162 Added dateTimeStamp and managerStamp.
 * <p> $
 * <p> $Revision 3.60  2008/12/02 21:25:47  sueh
 * <p> $bug# 1157 Removed unecessary rounding functions breakAndTrim, covertToScienticNotation, and round.
 * <p> $
 * <p> $Revision 3.59  2008/05/13 23:09:34  sueh
 * <p> $bug# 847 Removed unnecessary print statement.
 * <p> $
 * <p> $Revision 3.58  2008/04/02 02:29:07  sueh
 * <p> $bug# 1095 In getExistingDir simplified matches calls.
 * <p> $
 * <p> $Revision 3.57  2008/03/06 00:28:40  sueh
 * <p> $Fixed Easter Egg.
 * <p> $
 * <p> $Revision 3.56  2008/02/01 01:40:55  sueh
 * <p> $bug# 1075 Fixed Easter egg.
 * <p> $
 * <p> $Revision 3.55  2008/02/01 01:37:41  sueh
 * <p> $bug# 1075 Easter egg.
 * <p> $
 * <p> $Revision 3.54  2007/12/26 22:42:14  sueh
 * <p> $bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p> $
 * <p> $Revision 3.53  2007/09/07 00:31:37  sueh
 * <p> $bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> $instead of getInstance and createInstance.
 * <p> $
 * <p> $Revision 3.52  2007/03/01 01:48:23  sueh
 * <p> $bug# 964 Added LogFile to Autodoc.
 * <p> $
 * <p> $Revision 3.51  2006/10/24 23:35:40  sueh
 * <p> $bug# 947, bug# 948
 * <p> $
 * <p> $Revision 3.50  2006/08/18 23:26:31  sueh
 * <p> $bug# 852 convertLabelToName:  keeping the leading dash
 * <p> $
 * <p> $Revision 3.49  2006/07/04 18:15:28  sueh
 * <p> $bug# 897 renameFile():  Tell user to close 3dmod in Windows.
 * <p> $
 * <p> $Revision 3.48  2006/06/30 16:30:54  sueh
 * <p> $bug# 883 Replaced getEnvironmentVariable() with EnvironmentVariable, a class
 * <p> $to get and store environment variables.
 * <p> $
 * <p> $Revision 3.47  2006/06/14 00:47:26  sueh
 * <p> $bug# 852 Changed the int type to an inner Type claass.
 * <p> $
 * <p> $Revision 3.46  2006/06/07 20:38:56  sueh
 * <p> $bug# 766 Added isMacOS().
 * <p> $
 * <p> $Revision 3.45  2006/05/22 22:53:12  sueh
 * <p> $bug# 577 Placed commands in a String[] rather then a String.
 * <p> $
 * <p> $Revision 3.44  2006/04/28 21:13:14  sueh
 * <p> $bug# 787 ConvertLabelToName:  when there is nothing in the string
 * <p> $except for characters to be stripped, return "-".  This makes the expand
 * <p> $button work.
 * <p> $
 * <p> $Revision 3.43  2006/04/25 19:42:03  sueh
 * <p> $bug# 787 Made getTimestamp() public.
 * <p> $
 * <p> $Revision 3.42  2006/04/07 23:32:57  sueh
 * <p> $bug# 846 Changing the background colors for java 1.5.
 * <p> $
 * <p> $Revision 3.41  2006/04/06 20:34:58  sueh
 * <p> $bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> $util.Utilities.
 * <p> $
 * <p> $Revision 3.40  2006/03/20 18:10:07  sueh
 * <p> $bug# 835 Added getName (a convenience function) to the managers.
 * <p> $
 * <p> $Revision 3.39  2006/02/16 16:56:59  sueh
 * <p> $bug# 796 Windows:  in getEnvironmentVariable(), handle missing
 * <p> $environment variable.
 * <p> $
 * <p> $Revision 3.38  2006/01/06 21:41:34  sueh
 * <p> $bug# 793 Fixed getDir(String, AxisID) so it returns null when the environment
 * <p> $variable isn't set.
 * <p> $
 * <p> $Revision 3.37  2006/01/04 00:34:31  sueh
 * <p> $bug# 675 Making class more independent from EtomoDirector.
 * <p> $
 * <p> $Revision 3.36  2005/12/09 20:40:30  sueh
 * <p> $bug#776 added findMessageAndOpenDialog                                                                                                                                                                    bug#
 * <p> $
 * <p> $Revision 3.35  2005/12/05 21:58:35  sueh
 * <p> $bug# 674 Added comments to the new functions.
 * <p> $
 * <p> $Revision 3.34  2005/12/05 21:41:17  sueh
 * <p> $bug# 674 Added round(), convertToScientificNotation(), and
 * <p> $breakAndTrim().
 * <p> $
 * <p> $Revision 3.33  2005/11/29 22:54:26  sueh
 * <p> $Removed print statement.
 * <p> $
 * <p> $Revision 3.32  2005/11/21 22:04:07  sueh
 * <p> $bug# 733 In isWindowOS(), only call System.getProperty("os.name") one
 * <p> $time per application instance.
 * <p> $
 * <p> $Revision 3.31  2005/11/21 18:17:25  sueh
 * <p> $bug# 733 problem with unit tests in windows.
 * <p> $
 * <p> $Revision 3.30  2005/09/14 20:27:17  sueh
 * <p> $bug# 532 Added timestamp(void).
 * <p> $
 * <p> $Revision 3.29  2005/09/13 00:38:44  sueh
 * <p> $bug# 532 call isDebug() in isTimestamp().
 * <p> $
 * <p> $Revision 3.28  2005/09/13 00:28:36  sueh
 * <p> $Prevent logging timestamp unless either the debug or timestamp option is
 * <p> $on.
 * <p> $
 * <p> $Revision 3.27  2005/09/09 21:48:44  sueh
 * <p> $bug# 532 Handling null from stderr and stdout.
 * <p> $
 * <p> $Revision 3.26  2005/08/27 22:44:21  sueh
 * <p> $bug# 532 In Utilities.timestamp() change the int status to String status,
 * <p> $since it doesn't have to be compared.
 * <p> $
 * <p> $Revision 3.25  2005/08/24 22:41:03  sueh
 * <p> $bug# 715 Added a version of timestamp() which uses ProcessName.
 * <p> $
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

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.ui.swing.Token;
import etomo.ui.swing.UIHarness;
import etomo.comscript.ComScript;

public class Utilities {
  public static final String ACTION_TAG = "Etomo Action: ";

  private static boolean retrievedDebug = false;
  private static boolean debug = false;
  private static boolean retrievedSelfTest = false;
  private static boolean selfTest = false;
  private static boolean timestamp = false;
  private static boolean setWindowsOS = false;
  private static boolean windowsOS = false;
  private static boolean setMacOS = false;
  private static boolean macOS = false;
  private static boolean java1_5 = false;
  private static boolean setJava1_5 = false;
  private static long startTime = 0;
  private static DecimalFormat timestampFormat = new DecimalFormat(".000");
  public static final String STARTED_STATUS = " started";
  public static final String FINISHED_STATUS = "finished";
  public static final String FAILED_STATUS = "  failed";

  private Utilities() {
  }

  /**
   * Returns an element from a comma-divided list stored in a string.  Returns null if the
   * requested element does not exist.  The element's leading and trailing whitespace will
   * be removed from the return value.
   * @param list - comma-divided list
   * @param index - index of the element
   * @return
   */
  public static String getElementFromList(final String list, final int index) {
    if (list != null && index >= 0) {
      String[] array = list.split("\\s*,\\s*");
      if (array != null && index < array.length) {
        return array[index];
      }
    }
    return null;
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
    // Add a leading zero if less than 10 seconds
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
    File file = new File(manager.getPropertyUserDir(), manager.getBaseMetaData()
        .getName() + axisID.getExtension() + extension);
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
    File file = new File(manager.getPropertyUserDir(), manager.getName()
        + axisID.getExtension() + extension);
    if (!file.exists() && mustExist) {
      UIHarness.INSTANCE.openMessageDialog(manager, "The " + fileDescription + " file: "
          + file.getAbsolutePath() + " doesn't exist.", "Missing File", axisID);
      return null;
    }
    return file;
  }

  public static File getFile(BaseManager manager, AxisID axisID, String extension) {
    File file = new File(manager.getPropertyUserDir(), manager.getName()
        + axisID.getExtension() + extension);
    return file;
  }

  public static boolean isAprilFools() {
    return new Date().toString().indexOf("Apr 01 ") != -1;
  }

  public static File getFile(String propertyUserDir, String filename) {
    if (filename == null || filename.matches("\\s*")) {
      return new File(propertyUserDir);
    }
    filename = filename.trim();
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
   * Returns false if MRCHeader throws an exception when it reads file.
   * @param file
   * @param manager
   * @param axisID
   * @return
   */
  public static boolean isValidStack(File file, BaseManager manager, AxisID axisID) {
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        file.getName(), axisID);
    boolean validMrcFile = false;
    try {
      header.read(manager);
      validMrcFile = true;
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return validMrcFile;
  }

  public static void backupFile(File source) throws IOException {
    if (source == null) {
      return;
    }
    renameFile(source, new File(source.getAbsolutePath() + "~"));
  }

  public static String prepareRenameActionMessage(final File from, final File to) {
    if (!EtomoDirector.INSTANCE.getArguments().isActions()) {
      return null;
    }
    return ACTION_TAG + "Renamed " + from.getName() + " to " + to.getName();
  }

  public static String prepareCopyActionMessage(final File from, final File to) {
    if (!EtomoDirector.INSTANCE.getArguments().isActions()) {
      return null;
    }
    return ACTION_TAG + "Copied " + from.getName() + " to " + to.getName();
  }

  /**
   * Returns the command action message.  Returns null if the --actions parameter was not
   * set, or the commandAction string is null.
   * @param commandAction
   * @return
   */
  public static String getCommandActionMessage(final String commandAction) {
    if (!EtomoDirector.INSTANCE.getArguments().isActions() || commandAction == null) {
      return null;
    }
    return ACTION_TAG + "Ran " + commandAction;
  }

  public static String getCommandAction(final String[] commandArray,
      final String[] stdInput) {
    if (commandArray == null || commandArray.length == 0) {
      return null;
    }
    String command = commandArray[0].trim();
    if ((commandArray.length == 1 && (command.equals("env") || command.equals("hostname") || (command
        .equals("tcsh") && (stdInput == null || stdInput.length == 0))))
        || command.endsWith("ssh")
        || command.equals("ps")
        || command.endsWith("3dmod")
        || command.endsWith("imodsendevent")) {
      return null;
    }
    int commandLength = 1;

    int stdMax = 0;
    if (command.endsWith(ProcessName.CLIP.toString())) {
      commandLength = 2;
    }
    else if (command.endsWith("bash")) {
      commandLength = 4;
    }
    else if (command.endsWith("python")) {
      if (commandArray.length < 3) {
        stdMax = -1;
      }
      else {
        if (commandArray.length >= 3 && commandArray[2].endsWith("startprocess")) {
          commandLength = 6;
        }
        else {
          commandLength = 4;
        }
      }
    }
    else if (command.endsWith("tcsh")) {
      commandLength = 3;
      stdMax = 2;
    }
    else if (command.endsWith("cmd.exe")) {
      commandLength = 3;
    }
    else if (command.startsWith("sh")) {
      commandLength = 2;
    }
    StringBuffer buffer = new StringBuffer();
    String param = null;
    int chIndex = -1;
    boolean showDash = false;
    for (int i = 0; i < commandArray.length; i++) {
      param = commandArray[i];
      if (param.equals("None")) {
        continue;
      }
      if (param.endsWith("alignlog")) {
        showDash = true;
      }
      if (!showDash
          && (param.startsWith("-") || (isWindowsOS() && param.length() == 2 && param
              .startsWith("/")))) {
        continue;
      }
      if (i >= commandLength) {
        // print file names after the command is printed
        // Eliminate strings that aren't file names.
        // Unable to print files that don't have an extension
        if (param.indexOf(".") == -1) {
          continue;
        }
        else {
          // Eliminate numbers
          try {
            Double.parseDouble(param);
            continue;
          }
          catch (NumberFormatException e) {
            // Eliminate lists of numbers
            if (param.indexOf(",") != -1 || param.indexOf("-") != -1) {
              boolean listOfNumbers = true;
              String[] list = param.split("\\s*[,-]\\s*");
              if (list != null) {
                for (int j = 0; j < list.length; j++) {
                  try {
                    if (list[j] != null && list[j].length() > 0) {
                      Double.parseDouble(list[j]);
                    }
                  }
                  catch (NumberFormatException f) {
                    listOfNumbers = false;
                  }
                }
              }
              if (listOfNumbers) {
                continue;
              }
            }
            // Probably a file - remove the file path
            chIndex = param.lastIndexOf(File.separator);
            if (isWindowsOS()) {
              // Windows paths are sometimes built with /.
              chIndex = Math.max(chIndex, param.lastIndexOf("/"));
            }
            if (chIndex != -1) {
              param = param.substring(chIndex + 1);
            }
          }
        }
      }
      else {
        chIndex = param.lastIndexOf(File.separator);
        if (isWindowsOS()) {
          // Windows paths are sometimes built with /.
          chIndex = Math.max(chIndex, param.lastIndexOf("/"));
        }
        if (chIndex != -1) {
          param = param.substring(chIndex + 1);
        }
      }
      if (param != null) {
        buffer.append(param + " ");
      }
    }
    if (stdInput != null) {
      int length;
      if (stdMax == -1) {
        // Unlimited search
        length = stdInput.length;
      }
      else {
        length = Math.min(stdMax, stdInput.length);
      }
      boolean done = false;
      for (int i = 0; i < length; i++) {
        param = stdInput[i].trim();
        if (param.startsWith("#") || param.startsWith("nohup")) {
          continue;
        }
        if (command.endsWith("python")) {
          if (param.startsWith("makeBackupFile")) {
            chIndex = param.indexOf(".log");
            int quoteIndex = param.indexOf("\'");
            if (chIndex != -1 && quoteIndex != -1) {
              param = param.substring(quoteIndex + 1, chIndex);
            }
            done = true;
          }
          else {
            continue;
          }
        }
        else if (param.startsWith("if")) {
          chIndex = param.indexOf(".log");
          int quoteIndex = param.indexOf("\"");
          if (chIndex != -1 && quoteIndex != -1) {
            param = param.substring(quoteIndex + 1, chIndex);
          }
        }
        if (param != null) {
          buffer.append(param);
        }
        if (done) {
          break;
        }
      }
    }
    return buffer.toString().trim();
  }

  public static String getCommandAction(String commandLine) {
    if (commandLine == null || commandLine.length() == 0) {
      return null;
    }
    commandLine = commandLine.trim();
    if (commandLine.equals("env") || commandLine.equals("hostname")
        || commandLine.indexOf("ssh") != -1 || commandLine.indexOf("ps") != -1
        || commandLine.indexOf("3dmod") != -1
        || commandLine.indexOf("imodsendevent") != -1) {
      return null;
    }
    return commandLine;
  }

  /**
   * Returns an action messages if this functionality is turned on and the dialog type has
   * changed.
   * @param dialogType
   * @param axisID
   * @param oldDialogType
   * @return
   */
  public static String prepareDialogActionMessage(final DialogType dialogType,
      final AxisID axisID, final DialogType oldDialogType) {
    if (!EtomoDirector.INSTANCE.getArguments().isActions() || dialogType == null
        || dialogType == oldDialogType) {
      return null;
    }
    String axis = "";
    if (axisID != AxisID.ONLY) {
      axis = " in " + axisID.toString() + " axis";
    }
    return ACTION_TAG + "Opened dialog " + dialogType.toString() + axis;
  }

  /**
   * Rename a file working around the Windows bug.  Never delete delete a file in this
   * function.  If the destination file exists, fail.
   * This need serious work arounds because of the random failure bugs on
   * windows.  See sun java bugs: 4017593, 4017593, 4042592
   * @param source - file to be moved
   * @param destination - new file for source file
   * @throws IOException
   */
  public static boolean renameFileSafely(final BaseManager manager, final AxisID axisID,
      final File source, final File destination) throws IOException {
    if (source == null || destination == null) {
      System.err.println("WARNING: unable to rename - source or destination is null");
      return false;
    }
    if (!source.exists()) {
      System.err.println("WARNING: unable to rename - " + source.getName()
          + " does not exist.");
      return false;
    }
    // If the destination exists, exit
    if (destination.exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to rename " + source.getName() + " to " + destination.getName()
              + " because the target file already exists.", "Rename Failed", axisID);
      return false;
    }
    // Rename the existing log file
    if (source.exists()) {
      Utilities.debugPrint(source.getAbsolutePath() + " exists");
      String actionMessage = prepareRenameActionMessage(source, destination);
      if (!source.renameTo(destination)) {
        if (source.exists()) {
          System.err.println(source.getAbsolutePath() + " still exists");
        }
        else {
          System.err.println(source.getAbsolutePath() + " does not exist");
        }
        if (destination.exists()) {
          System.err.println(destination.getAbsolutePath() + " exists");
        }
        else {
          System.err.println(destination.getAbsolutePath() + " does not exist");
        }
        System.err.println("Unable to rename file to: " + destination.getAbsolutePath());
        StringBuffer message = new StringBuffer("Unable to rename "
            + source.getAbsolutePath() + " to " + destination.getAbsolutePath());
        if (isWindowsOS()) {
          message.append("\nIf either of these files is open in 3dmod, close 3dmod.");
        }
        throw (new IOException(message.toString()));
      }
      else if (actionMessage != null) {
        System.err.println(actionMessage);
      }
    }
    return true;
  }

  /**
   * Rename a file working around the Windows bug
   * This need serious work arounds because of the random failure bugs on
   * windows.  See sun java bugs: 4017593, 4017593, 4042592
   * @param source - file to be moved
   * @param destination - new file for source file
   */
  public static void renameFile(File source, File destination) throws IOException {
    if (!source.exists()) {
      return;
    }
    // Delete the existing backup file if it exists, otherwise the call will
    // fail on windows
    if (destination.exists()) {
      // TODO implement interactive Bug# 1696
      Utilities.debugPrint(destination.getAbsolutePath() + " exists, deleting");
      if (!destination.delete()) {
        System.err.println("Unable to delete destination file: "
            + destination.getAbsolutePath());
        if (destination.exists()) {
          System.err.println(destination.getAbsolutePath() + " still exists!");
        }
        else {
          System.err.println(destination.getAbsolutePath() + " does not exist!");
        }
      }
    }

    // Rename the existing log file
    if (source.exists()) {
      Utilities.debugPrint(source.getAbsolutePath() + " exists");
      String actionMessage = prepareRenameActionMessage(source, destination);
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
        System.err.println("Unable to rename file to: " + destination.getAbsolutePath());
        StringBuffer message = new StringBuffer("Unable to rename "
            + source.getAbsolutePath() + " to " + destination.getAbsolutePath());
        if (isWindowsOS()) {
          message.append("\nIf either of these files is open in 3dmod, close 3dmod.");
        }
        throw (new IOException(message.toString()));
      }
      else if (actionMessage != null) {
        System.err.println(actionMessage);
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
   * Returns the FileType corresponding to the most recent file in propertyUserDir.
   * @param manager
   * @param axisID
   * @param fileTypes
   * @param defaultIndex
   * @return
   */
  public static FileType mostRecentFile(final BaseManager manager, final AxisID axisID,
      final FileType[] fileTypes, final int defaultIndex) {
    if (fileTypes == null) {
      return null;
    }
    int mostRecentIndex = -1;
    File file = null;
    long mostRecentFileTime = 0;
    if (defaultIndex >= 0 && defaultIndex < fileTypes.length
        && fileTypes[defaultIndex] != null) {
      mostRecentIndex = defaultIndex;
      file = fileTypes[mostRecentIndex].getFile(manager, axisID);
      if (file != null && file.exists()) {
        mostRecentFileTime = file.lastModified();
      }
    }
    for (int i = 0; i < fileTypes.length; i++) {
      if (i == defaultIndex) {
        continue;
      }
      if (fileTypes[i] != null) {
        file = fileTypes[i].getFile(manager, axisID);
        if (file != null && file.exists()) {
          long fileTime = file.lastModified();
          if (fileTime > mostRecentFileTime) {
            mostRecentFileTime = fileTime;
            mostRecentIndex = i;
          }
        }
      }
    }
    if (mostRecentIndex == -1) {
      return null;
    }
    return fileTypes[mostRecentIndex];
  }

  public static void copyFile(final FileType source, final FileType destination,
      final BaseManager manager, final AxisID axisID) throws IOException {
    copyFile(source.getFile(manager, axisID), destination.getFile(manager, axisID));
  }

  public static void copyFile(final File source, final FileType destination,
      final BaseManager manager, final AxisID axisID) throws IOException {
    copyFile(source, destination.getFile(manager, axisID));
  }

  /**
   * Copy a file using the fastest method available.
   */
  public static void copyFile(File source, File destination) throws IOException {
    if (source != null && source.equals(destination)) {
      System.err.println("Warning:Can't copy a file to itself:source:" + source
          + ",destination:" + destination);
      return;
    }
    // Try using the nio method but if it fails fall back to BufferedFileReader/
    // BufferedFileWriter approach
    String actionMessage = prepareCopyActionMessage(source, destination);
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

    // TODO: does each object need to be closed indivudually
    if (sourceBuffer != null) {
      sourceBuffer.close();
    }
    sourceStream.close();
    if (destBuffer != null) {
      destBuffer.close();
    }
    destStream.close();
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
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
    if (EtomoDirector.INSTANCE.getArguments().isDebug()) {
      if (toOut) {
        System.out.println(string);
      }
      else {
        System.err.println(string);
      }
    }
  }

  public static boolean deleteFileType(BaseManager manager, AxisID axisID,
      FileType fileType) {
    File file = new File(manager.getPropertyUserDir(), fileType.getFileName(manager,
        axisID));
    if (file.isFile()) {
      if (file.exists()) {
        if (!file.delete()) {
          StringBuffer message = new StringBuffer("Unable to delete file: "
              + file.getAbsolutePath());
          if (Utilities.isWindowsOS()) {
            message.append("\nIf this file is open in 3dmod, close 3dmod.");
          }
          UIHarness.INSTANCE.openMessageDialog(manager, message.toString(),
              "Can not delete file", axisID);
          return false;
        }
      }
    }
    else if (file.isDirectory()) {
      SystemProgram remove = new SystemProgram(manager, manager.getPropertyUserDir(),
          new String[] { "python", "-u",
              ApplicationManager.getIMODBinPath() + "b3dremove", "-r",
              file.getAbsolutePath() }, axisID);
      remove.run();
      if (file.exists()) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Cannot delete the directory",
            "Can not delete directory", axisID);
        return false;
      }
    }
    return true;
  }

  /**
   * @param file - a bad file causes null to be returned
   * @return the name of file, stripped of its extension
   */
  public static String getStrippedFileName(final File file) {
    if (file == null) {
      return null;
    }
    String name = file.getName();
    if (name == null || name.matches("\\s*")) {
      return null;
    }
    int extIndex = name.lastIndexOf('.');
    if (extIndex == -1) {
      return name;
    }
    return name.substring(0, extIndex);
  }

  /**
   * Strips the last colon (and anything following it) and returns it.
   * @param string
   * @return
   */
  public static String stripLabel(final String label) {
    int colonIndex = label.lastIndexOf(':');
    if (colonIndex == -1) {
      return label;
    }
    return label.substring(0, colonIndex);
  }

  /**
   * Strips the last colon (and anything following it) and returns the string in single
   * quotes.
   * @param string
   * @return
   */
  public static String quoteLabel(final String label) {
    char quote = '\'';
    return quote + stripLabel(label) + quote;
  }

  public static String escapeSpaces(final String string) {
    int index = string.indexOf(' ');
    if (index == -1) {
      return string;
    }
    StringBuffer buffer = new StringBuffer();
    int prevIndex = 0;
    while (index != -1) {
      buffer.append(string.substring(prevIndex, index));
      buffer.append("\\ ");
      prevIndex = ++index;
      index = string.indexOf(' ', prevIndex);
    }
    buffer.append(string.substring(prevIndex));
    return buffer.toString();
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
      Utilities.renameFile(file, new File(file.getAbsolutePath() + "~"));
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
      StringBuffer invalidReason, boolean exists, boolean canRead, boolean canWrite,
      boolean isDirectory) {
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
    timestamp("PRESSED", command, (String) null, null);
  }

  /**
   * Print timestamp in error log
   * @param command
   * @param container
   */
  public static void buttonTimestamp(String command, String container) {
    timestamp("PRESSED", command, container, null);
  }

  /**
   * Print timestamp in error log
   * @param process
   * @param container
   * @param status
   */
  public static void timestamp(String process, String container, String status) {
    if (!timestamp) {
      return;
    }
    timestamp(process, null, container, status);
  }

  public static void timestamp(String process, ProcessName container, String status) {
    if (!timestamp) {
      return;
    }
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
      String status) {
    if (!timestamp) {
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
      String status) {
    if (!timestamp) {
      return;
    }
    timestamp(process, command, container.getName(), status);
  }

  public static void timestamp(String command, String status) {
    if (!timestamp) {
      return;
    }
    timestamp(null, command, (String) null, status);
  }

  /**
   * Print timestamp in error log
   * @param process
   * @param command
   * @param container
   * @param status 0 = started, 1 = finished, -1 = failed, or -100 = null
   */
  public static void timestamp(String process, String command, String container,
      String status) {
    if (!timestamp) {
      return;
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
    if (status != null) {
      buffer.append(status + " ");
    }
    buffer.append("at " + getTimestamp());
    System.err.println(buffer);
  }

  public static void timestamp() {
    System.err.println("TIMESTAMP:  " + getTimestamp());
  }

  public static boolean isDebug() {
    if (!retrievedDebug) {
      debug = EtomoDirector.INSTANCE.getArguments().isDebug();
      retrievedDebug = true;
    }
    return debug;
  }

  public static void setTimestamp(boolean timestamp) {
    Utilities.timestamp = timestamp;
  }

  public static boolean isSelfTest() {
    if (!retrievedSelfTest) {
      selfTest = EtomoDirector.INSTANCE.getArguments().isSelfTest();
      retrievedSelfTest = true;
    }
    return selfTest;
  }

  public static void setStartTime() {
    startTime = new Date().getTime();
  }

  public static String getDateTimeStamp(final boolean includeMs) {
    Date date = new Date();
    return date.toString() + (includeMs ? ", " + date.getTime() % 1000 + " ms" : "");
  }

  public static String getDateTimeStamp() {
    return getDateTimeStamp(false);
  }

  public static void dateTimeStamp() {
    System.err.println(getDateTimeStamp(false));
  }

  public static void managerStamp(String dir, String name) {
    System.err.println("\n++++++++++++++++");
    dateTimeStamp();
    if (dir != null) {
      System.err.println(dir);
    }
    if (name != null) {
      System.err.println(name);
    }
    System.err.println("++++++++++++++++\n");
  }

  public static String getTimestamp() {
    return timestampFormat.format((new Date().getTime() - startTime) / 1000.0);
  }

  public static boolean isWindowsOS() {
    if (!setWindowsOS) {
      String osName = System.getProperty("os.name").toLowerCase();
      windowsOS = osName.indexOf("windows") != -1;
      setWindowsOS = true;
    }
    return windowsOS;
  }

  public static boolean isMacOS() {
    if (!setMacOS) {
      String osName = System.getProperty("os.name").toLowerCase();
      macOS = osName.indexOf("mac") != -1;
      setMacOS = true;
    }
    return macOS;
  }

  public static final void findMessageAndOpenDialog(BaseManager manager, AxisID axisID,
      String[] searchLines, String startsWith, String title) {
    if (searchLines == null) {
      return;
    }
    for (int i = 0; i < searchLines.length; i++) {
      if (searchLines[i].startsWith(startsWith)) {
        UIHarness.INSTANCE.openInfoMessageDialog(manager, searchLines[i], title, axisID);
      }
    }
  }

  public static final File getExistingDir(BaseManager manager, String envVariable,
      AxisID axisID, String notFoundMessage) {
    if (envVariable == null || envVariable.matches("\\s*")) {
      return null;
    }
    String dirName = EnvironmentVariable.INSTANCE.getValue(manager, null, envVariable,
        axisID);
    if (dirName == null || dirName.matches("\\s*")) {
      return null;
    }
    File dir = new File(dirName);
    if (!checkExistingDir(dir, envVariable, notFoundMessage)) {
      return null;
    }
    return dir;
  }

  public static final File getExistingDir(BaseManager manager, String envVariable,
      AxisID axisID) {
    if (envVariable == null || envVariable.matches("\\s*")) {
      return null;
    }
    String dirName = EnvironmentVariable.INSTANCE.getValue(manager, null, envVariable,
        axisID);
    if (dirName == null || dirName.matches("\\s*")) {
      return null;
    }
    File dir = new File(dirName);
    if (!checkExistingDir(dir, envVariable)) {
      return null;
    }
    return dir;
  }

  public static final boolean checkExistingDir(File dir, String envVariable,
      final String notFoundMessage) {
    if (!dir.exists() || !dir.isDirectory() || !dir.canRead()) {
      System.err.println(notFoundMessage);
      return false;
    }
    return true;
  }

  public static final boolean checkExistingDir(File dir, String envVariable) {
    if (!dir.exists()) {
      System.err.println("Warning:  " + dir.getAbsolutePath() + " does not exist.  See $"
          + envVariable + ".");
      return false;
    }
    if (!dir.isDirectory()) {
      System.err.println("Warning:  " + dir.getAbsolutePath()
          + " is not a directory.  See $" + envVariable + ".");
      return false;
    }
    if (!dir.canRead()) {
      System.err.println("Warning:  cannot read " + dir.getAbsolutePath() + ".  See $"
          + envVariable + ".");
      return false;
    }
    return true;
  }

  public static final String convertLabelToName(String label1, String label2,
      String label3) {
    StringBuffer buffer = new StringBuffer();
    if (label1 != null) {
      buffer.append(label1 + " ");
    }
    if (label2 != null) {
      buffer.append(label2 + " ");
    }
    if (label3 != null) {
      buffer.append(label3 + " ");
    }
    return convertLabelToName(buffer.toString());
  }

  public static final ConstEtomoNumber convertNanometersToMicrons(
      final ConstEtomoNumber nm) {
    EtomoNumber microns = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    microns.set(nm);
    microns.divideBy(1000);
    return microns;
  }

  public static final String convertMicronsToNanometers(final String microns) {
    EtomoNumber nm = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    nm.set(microns);
    nm.multiply(1000);
    return nm.toString();
  }

  /**
   * Convert a UI label to a name that can be used as a key. The returned string
   * should contain no whitespace.  A single dash is used to separate words.
   * Explanatory strings (stuff in parenthesis), punctuation (colons), and html
   * (anything in angle brackets) are removed.  It also removes anything after a
   * colon because there probably shouldn't be anything after a colon.
   * 
   * The returned string is not guaranteed to be unique in the UI panel or
   * dialog.
   * @param label
   * @return
   */
  public static final String convertLabelToName(String label) {
    if (label == null) {
      return null;
    }
    // Place the label into a tokenizer
    String name = label.trim().toLowerCase();
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(name);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    boolean firstToken = true;
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      return label;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return label;
    }
    // Remove unnecessary symbols and strings from the label.
    boolean ignoreParen = false;
    boolean ignoreBracket = false;
    while (token != null && !token.is(Token.Type.EOF) && !token.is(Token.Type.EOL)) {
      if (token.equals(Token.Type.SYMBOL, '(')) {
        // ignore parenthesis and everything in them
        ignoreParen = true;
      }
      else if (token.equals(Token.Type.SYMBOL, ')')) {
        ignoreParen = false;
      }
      else if (token.equals(Token.Type.SYMBOL, '<')) {
        // Replace html (angle brackets and contents) with a space. The space is
        // necessary when a <br> is used.
        ignoreBracket = true;
        buffer.append(' ');
      }
      else if (token.equals(Token.Type.SYMBOL, '>')) {
        ignoreBracket = false;
      }
      else if (token.equals(Token.Type.SYMBOL, ':')) {
        // ignore colons and everything after them
        break;
      }
      else if (!ignoreParen && !ignoreBracket) {
        // Convert a dash to a space so that any mix of dashes and whitespace
        // in the original label gets converted to a single dash in the next
        // loop. If the '-' is the first token, keep it
        if (token.equals(Token.Type.SYMBOL, '-') && !firstToken) {
          buffer.append(' ');
        }
        // Remove "." because it is recognized by autodoc. Assuming that the "."
        // is from an abbreviation.
        else if (!token.equals(Token.Type.SYMBOL, '.')) {
          buffer.append(token.getValue());
        }
      }
      try {
        token = tokenizer.next();
        firstToken = false;
      }
      catch (IOException e) {
        e.printStackTrace();
        break;
      }
    }
    // Load the processed string into the tokenizer
    name = buffer.toString().trim();
    // handle a string with nothing but strippable characters in it
    if (name.length() == 0) {
      return "-";
    }
    tokenizer = new PrimativeTokenizer(name);
    buffer = new StringBuffer();
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      return name;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return label;
    }
    // Convert interior whitespace to a single dash
    while (token != null && !token.is(Token.Type.EOF) && !token.is(Token.Type.EOL)) {
      if (token.is(Token.Type.WHITESPACE)) {
        buffer.append('-');
      }
      else {
        buffer.append(token.getValue());
      }
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        break;
      }
    }
    return buffer.toString();
  }

  /**
   * Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @return binning (default 1)
   */
  public static int getStackBinning(BaseManager manager, AxisID axisID,
      FileType stackFileType) {
    MRCHeader stackHeader = MRCHeader.getInstance(manager, axisID, stackFileType);
    MRCHeader rawstackHeader = MRCHeader.getInstance(manager, axisID, FileType.RAW_STACK);
    try {
      if (!rawstackHeader.read(manager) || !stackHeader.read(manager)) {
        return 1;
      }
    }
    catch (InvalidParameterException e) {
      // missing file
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      return 1;
    }
    int binning = 1;
    double rawstackXPixelSpacing = rawstackHeader.getXPixelSpacing();
    if (rawstackXPixelSpacing > 0) {
      binning = (int) Math.round(stackHeader.getXPixelSpacing() / rawstackXPixelSpacing);
    }
    if (binning != 1 && binning < 1) {
      return 1;
    }
    return binning;
  }
}