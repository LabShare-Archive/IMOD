/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.24  2009/03/17 00:31:35  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.23  2009/02/25 00:14:09  sueh
 * bug# 1182 Made sphericalAberration a double.
 *
 * Revision 3.22  2008/10/27 17:47:34  sueh
 * bug# 1141 Added options to setup ctf files only:  ctfFiles,
 * sphericalAberration, and voltage.
 *
 * Revision 3.21  2007/12/26 22:11:48  sueh
 * bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 *
 * Revision 3.20  2007/12/10 21:55:17  sueh
 * bug# 1041 Formatted.
 *
 * Revision 3.19  2007/09/07 00:18:04  sueh
 * bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * instead of getInstance and createInstance.
 *
 * Revision 3.18  2006/06/05 16:11:34  sueh
 * bug# 766 Using multi line messages automatically; don't have to set it.
 *
 * Revision 3.17  2006/05/22 22:38:36  sueh
 * bug# 577 Placed the command in an ArrayList rather then a String.
 *
 * Revision 3.16  2006/03/16 01:49:31  sueh
 * bug# 830 Handling exitValue != 0 without an exception.
 *
 * Revision 3.15  2005/11/02 21:35:51  sueh
 * bug# 754 Parsing errors and warnings inside ProcessMessages.
 * Replaced getErrors() and getWarnings() with getProcessMessages.
 *
 * Revision 3.14  2005/10/28 18:46:06  sueh
 * bug# 725 standardizing message parsing in SystemProgram.  Passing
 * multilineError to SystemProgram constructor.
 *
 * Revision 3.13  2005/10/27 00:11:47  sueh
 * bug# 725 Putting warnings into error log when debug is off.
 *
 * Revision 3.12  2005/07/29 00:44:59  sueh
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * because the current manager changes when the user changes the tab.
 * Passing the manager where its needed.
 *
 * Revision 3.11  2005/05/10 17:31:04  sueh
 * bug# 660 Added comment.
 *
 * Revision 3.10  2005/05/10 16:57:09  sueh
 * bug# 660 Added getWarnings() to get an array of warnings from standard
 * error.
 *
 * Revision 3.9  2005/04/25 20:39:25  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.
 *
 * Revision 3.8  2005/03/02 23:12:53  sueh
 * bug# 533 Adding -focus and -bfocus.
 *
 * Revision 3.7  2005/03/02 00:11:02  sueh
 * bug# 611 Added mag gradients correction file.
 *
 * Revision 3.6  2005/01/14 02:58:44  sueh
 * Prevented non-error messages from showing up in the err.log  file unless
 * debug is on.
 *
 * Revision 3.5  2004/11/19 22:53:54  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.4.4.1  2004/10/11 02:02:02  sueh
 * bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * property.  This property would need a different value for each manager.
 * This variable can be retrieved from the manager if the object knows its
 * manager.  Otherwise it can retrieve it from the current manager using the
 * EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * gets the value from the "user.dir" property.
 *
 * Revision 3.4  2004/04/22 23:28:13  rickg
 * *** empty log message ***
 *
 * Revision 3.3  2004/04/06 03:00:40  rickg
 * Updated imageRotation to store axis separately
 *
 * Revision 3.2  2004/02/24 18:51:36  sueh
 * bug# 385 added binning and distortion file
 *
 * Revision 3.1  2004/02/20 02:32:35  sueh
 * bug# 385 add getOptions() - places command line options
 * into a Vector.  Place all options on the command line
 *
 * Revision 3.0  2003/11/07 23:19:00  rickg
 * Version 1.0.0
 *
 * Revision 2.13  2003/11/06 16:50:27  rickg
 * Removed -e flag for tcsh execution for all but the com scripts
 *
 * Revision 2.12  2003/11/04 20:56:11  rickg
 * Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 *
 * Revision 2.11  2003/11/04 00:51:59  rickg
 * Bug #345 Explicitly set path to script using IMOD_DIR
 *
 * <p>
 * Revision 2.10 2003/05/13 19:59:43 rickg
 * <p>
 * Added -f option to tcsh call
 * <p>
 * <p>
 * Revision 2.9 2003/05/12 23:23:14 rickg
 * <p>
 * Changed tcsh call to tcsh -ec
 * <p>
 * <p>
 * Revision 2.8 2003/05/12 01:21:05 rickg
 * <p>
 * Added explici tcsh call to copytomocoms
 * <p>
 * <p>
 * Revision 2.7 2003/05/08 23:19:03 rickg
 * <p>
 * Standardized debug setting
 * <p>
 * <p>
 * Revision 2.6 2003/05/07 22:31:59 rickg
 * <p>
 * Don't need to set working directory since it defaults to user.dir
 * <p>
 * System property user.dir now defines the working directory
 * <p>
 * <p>
 * Revision 2.5 2003/04/29 20:22:38 rickg
 * <p>
 * Handles all three cases of tilt angle specification now
 * <p>
 * <p>
 * Revision 2.4 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 2.3 2003/03/20 17:22:18 rickg
 * <p>
 * Comment update
 * <p>
 * <p>
 * Revision 2.2 2003/03/02 23:30:41 rickg
 * <p>
 * Combine layout in progress
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 20:45:45 rickg
 * <p>
 * Debug messages to stderr instead of stdout
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.4 2002/10/10 18:54:01 rickg
 * <p>
 * Enabled SystemProgram debugging and remove local
 * <p>
 * writing to stdout.
 * <p>
 * <p>
 * Revision 1.3 2002/10/09 21:35:44 rickg
 * <p>
 * Removed stdout messages, can now be gotten from the enableDebug method
 * <p>
 * in SystemProgram
 * <p>
 * <p>
 * Revision 1.2 2002/10/09 21:19:40 rickg
 * <p>
 * Reformat from emacs
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */

package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DataSource;
import etomo.type.EtomoNumber;
import etomo.type.TiltAngleType;
import etomo.type.ViewType;

public final class CopyTomoComs {
  public static final String rcsid = "$Id$";

  private final ArrayList command = new ArrayList();
  private final EtomoNumber voltage = new EtomoNumber();
  private final EtomoNumber sphericalAberration = new EtomoNumber(
      EtomoNumber.Type.DOUBLE);
  private final EtomoNumber ctfFiles = new EtomoNumber();

  private final BaseManager manager;

  private StringBuffer commandLine = null;
  private int exitValue;
  private ConstMetaData metaData;
  private boolean debug;
  private SystemProgram copytomocoms = null;

  public CopyTomoComs(ApplicationManager manager) {
    this.manager = manager;
    metaData = manager.getConstMetaData();
    debug = EtomoDirector.INSTANCE.getArguments().isDebug();
  }

  public void setup() {
    // Create a new SystemProgram object for copytomocom, set the
    // working directory and stdin array.
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    command.add("tcsh");
    command.add("-f");
    command.add(ApplicationManager.getIMODBinPath() + "copytomocoms");
    genOptions();
    copytomocoms = new SystemProgram(manager.getPropertyUserDir(), command,
        AxisID.ONLY, manager.getManagerKey());
    //genStdInputSequence();
  }

  public void setVoltage(ConstEtomoNumber input) {
    voltage.set(input);
  }

  public void setSphericalAberration(ConstEtomoNumber input) {
    sphericalAberration.set(input);
  }

  public void setCTFFiles(CtfFilesValue ctfFilesValue) {
    ctfFiles.set(ctfFilesValue.get());
  }

  /**
   * Return the current command line string
   * 
   * @return
   */
  public String getCommandLine() {
    if (copytomocoms == null) {
      return "";
    }
    return copytomocoms.getCommandLine();
  }

  private void genOptions() {
    boolean montage = false;
    boolean gradient = false;
    //  Dataset name
    command.add("-name");
    command.add(metaData.getDatasetName());
    //  View type: single or montaged
    if (metaData.getViewType() == ViewType.MONTAGE) {
      command.add("-montage");
      montage = true;
    }
    //  Backup directory
    String backupDirectory = metaData.getBackupDirectory();
    if (!backupDirectory.equals("")) {
      command.add("-backup");
      command.add(metaData.getBackupDirectory());
    }
    //  Data source: CCD or film
    if (metaData.getDataSource() == DataSource.FILM) {
      command.add("-film");
    }
    //  Pixel size
    command.add("-pixel");
    command.add(String.valueOf(metaData.getPixelSize()));
    //  Fiducial diameter
    command.add("-gold");
    command.add(String.valueOf(metaData.getFiducialDiameter()));
    // Image rotation
    command.add("-rotation");
    command.add(String.valueOf(metaData.getImageRotation(AxisID.FIRST)));
    // A first tilt angle and tilt angle incriment
    if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
      command.add("-firstinc");
      command.add(String.valueOf(metaData.getTiltAngleSpecA().getRangeMin())
          + "," + String.valueOf(metaData.getTiltAngleSpecA().getRangeStep()));
    }
    // Use an existing rawtilt file (this assumes that one is there and has
    // not been deleted by checkTiltAngleFiles()
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.FILE) {
      command.add("-userawtlt");
    }
    //  Extract the tilt angle data from the stack
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
      command.add("-extract");
    }
    //List of views to exclude from processing
    String excludeProjections = metaData.getExcludeProjectionsA();
    if (!excludeProjections.equals("")) {
      command.add("-skip");
      command.add(excludeProjections);
    }
    if (!voltage.isNull()) {
      command.add("-voltage");
      command.add(voltage.toString());
    }
    if (!sphericalAberration.isNull()) {
      command.add("-Cs");
      command.add(sphericalAberration.toString());
    }
    if (!ctfFiles.isNull()) {
      command.add("-CTFfiles");
      command.add(ctfFiles.toString());
    }

    //Undistort images with the given .idf file
    String distortionFile = metaData.getDistortionFile();
    if (!distortionFile.equals("")) {
      command.add("-distort");
      command.add(distortionFile);
    }
    //Binning of raw stacks (needed to undistort if ambiguous)
    command.add("-binning");
    command.add(String.valueOf(metaData.getBinning()));
    // Mag gradients correction file
    String magGradientFile = metaData.getMagGradientFile();
    if (!magGradientFile.equals("")) {
      command.add("-gradient");
      command.add(magGradientFile);
      gradient = true;
      //It is only necessary to know if the focus was adjusted between montages
      //if a mag gradients correction file is being used.
      if (montage && metaData.getAdjustedFocusA().is()) {
        command.add("-focus");
      }
    }

    //  Axis type: single or dual
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      command.add("-dual");
      // B image rotation
      command.add("-brotation");
      command.add(String.valueOf(metaData.getImageRotation(AxisID.SECOND)));
      // B first tilt angle and tilt angle incriment
      if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.RANGE) {
        command.add("-bfirstinc");
        command
            .add(String.valueOf(metaData.getTiltAngleSpecB().getRangeMin())
                + ","
                + String.valueOf(metaData.getTiltAngleSpecB().getRangeStep()));
      }
      //Take tilt angle from a .rawtlt file - B
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.FILE) {
        command.add("-buserawtlt");
      }
      //  Extract the tilt angle data from the stack - B
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
        command.add("-bextract");
      }
      //List of views to exclude from processing - B
      excludeProjections = metaData.getExcludeProjectionsB();
      if (!excludeProjections.equals("")) {
        command.add("-bskip");
        command.add(excludeProjections);
      }
      if (montage && gradient && metaData.getAdjustedFocusB().is()) {
        command.add("-bfocus");
      }
    }
    // Options removed:
    //  CCDEraser and local alignment entries
    //  Always yes tiltalign relies on local entries to save default values
    //  even if they are not used.
  }

  /**
   * @deprecated
   * Generate the standard input sequence
   */
  private void genStdInputSequence() {
    String[] tempStdInput = new String[19];

    //  compile the input sequence to copytomocoms
    int lineCount = 0;

    //  Axis type: single or dual
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      tempStdInput[lineCount++] = "1";
    }
    else {
      tempStdInput[lineCount++] = "2";
    }

    //  Data source: CCD or film
    if (metaData.getDataSource() == DataSource.CCD) {
      tempStdInput[lineCount++] = "c";
    }
    else {
      tempStdInput[lineCount++] = "f";
    }

    //  View type: single or montaged
    if (metaData.getViewType() == ViewType.SINGLE_VIEW) {
      tempStdInput[lineCount++] = "n";
    }
    else {
      tempStdInput[lineCount++] = "y";
    }

    //  CCDEraser and local alignment entries
    //  Always yes tiltalign relies on local entries to save default values
    //  even if they are not used.
    tempStdInput[lineCount++] = "y";
    tempStdInput[lineCount++] = "y";

    //  Dataset name
    tempStdInput[lineCount++] = metaData.getDatasetName();

    //  Backup directory
    tempStdInput[lineCount++] = metaData.getBackupDirectory();

    //  Pixel size
    tempStdInput[lineCount++] = String.valueOf(metaData.getPixelSize());

    //  Fiducial diameter
    tempStdInput[lineCount++] = String.valueOf(metaData.getFiducialDiameter());

    // Image rotation
    tempStdInput[lineCount++] = String.valueOf(metaData
        .getImageRotation(AxisID.FIRST));

    //  Extract the tilt angle data from the stack
    if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
      tempStdInput[lineCount++] = "y";
      tempStdInput[lineCount++] = "0";
    }

    //  Specify a range, creating the rawtilt file
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
      tempStdInput[lineCount++] = "n";
      tempStdInput[lineCount++] = "1";
      tempStdInput[lineCount++] = String.valueOf(metaData.getTiltAngleSpecA()
          .getRangeMin()
          + "," + String.valueOf(metaData.getTiltAngleSpecA().getRangeStep()));
    }
    // Use an existing rawtilt file (this assumes that one is there and has
    // not been deleted by checkTiltAngleFiles()
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.FILE) {
      tempStdInput[lineCount++] = "0";
    }

    else {
      //  TODO Specification of all tilt alngles is not yet implemented
      tempStdInput[lineCount++] = "n";
      tempStdInput[lineCount++] = "-1";
      System.err
          .println("Specification of all tilt alngles is not yet implemented");
    }

    //  Exclude list
    tempStdInput[lineCount++] = metaData.getExcludeProjectionsA();

    // Second axis entries
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {

      //    Image rotation
      tempStdInput[lineCount++] = String.valueOf(metaData
          .getImageRotation(AxisID.SECOND));

      //    Extract the tilt angle data from the stack
      if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
        tempStdInput[lineCount++] = "y";
        tempStdInput[lineCount++] = "0";
      }

      //    Specify a range, creating the rawtilt file
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.RANGE) {
        tempStdInput[lineCount++] = "n";
        tempStdInput[lineCount++] = "1";
        tempStdInput[lineCount++] = String
            .valueOf(metaData.getTiltAngleSpecB().getRangeMin() + ","
                + String.valueOf(metaData.getTiltAngleSpecB().getRangeStep()));
      }

      //    Specify a range, creating the rawtilt file
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.FILE) {
        tempStdInput[lineCount++] = "0";
      }

      else {
        //  TODO Specification of all tilt alngles is not yet implemented
        tempStdInput[lineCount++] = "n";
        tempStdInput[lineCount++] = "-1";
        System.err
            .println("Specification of all tilt alngles is not yet implemented");
      }

      //  Exclude list
      tempStdInput[lineCount++] = metaData.getExcludeProjectionsB();
    }

    //  Copy the temporary stdInput to the real stdInput to get the number
    //  of array elements correct
    String[] stdInput = new String[lineCount];
    for (int i = 0; i < lineCount; i++) {
      stdInput[i] = tempStdInput[i];
    }
    copytomocoms.setStdInput(stdInput);
  }

  /**
   * Execute the copytomocoms script
   * 
   * @return @throws
   *         IOException
   */
  public int run() {
    if (copytomocoms == null) {
      return -1;
    }
    int exitValue;

    //  Delete the rawtilt files if extract raw tilts is selected
    checkTiltAngleFiles();

    //  Execute the script
    copytomocoms.setDebug(debug);
    copytomocoms.run();
    exitValue = copytomocoms.getExitValue();
    return exitValue;
  }

  public String getStdErrorString() {
    if (copytomocoms == null) {
      return "ERROR: Copytomocoms is null.";
    }
    return copytomocoms.getStdErrorString();
  }

  public String[] getStdError() {
    if (copytomocoms == null) {
      return new String[] { "ERROR: Copytomocoms is null." };
    }
    return copytomocoms.getStdError();
  }

  /**
   * returns a String array of warnings - one warning per element
   * make sure that warnings get into the error log
   * @return
   */
  public ProcessMessages getProcessMessages() {
    if (copytomocoms == null) {
      return null;
    }
    return copytomocoms.getProcessMessages();
  }

  /**
   * Check to see if the tilt angle files exist and the tilt angle type is not
   * FILE. They need to be deleted because the copytomocoms script is not
   * consistent in the sequence of responses expected.
   */
  private void checkTiltAngleFiles() {
    String workingDirectory = manager.getPropertyUserDir();
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      if (metaData.getTiltAngleSpecA().getType() != TiltAngleType.FILE) {
        File rawTiltFile = new File(workingDirectory, metaData.getDatasetName()
            + ".rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
    }
    else {
      if (metaData.getTiltAngleSpecA().getType() != TiltAngleType.FILE) {
        File rawTiltFile = new File(workingDirectory, metaData.getDatasetName()
            + "a.rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
      if (metaData.getTiltAngleSpecB().getType() != TiltAngleType.FILE) {
        File rawTiltFile = new File(workingDirectory, metaData.getDatasetName()
            + "b.rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
    }
  }

  public static final class CtfFilesValue {
    public static final CtfFilesValue CTF_PLOTTER = new CtfFilesValue(1);
    public static final CtfFilesValue CTF_CORRECTION = new CtfFilesValue(2);

    private final int value;

    private CtfFilesValue(int value) {
      this.value = value;
    }

    private int get() {
      return value;
    }
  }
}
