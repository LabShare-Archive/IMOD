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
 * Revision 3.30  2011/02/26 04:19:59  sueh
 * bug# 1453 In setup making a python call instead of a tcsh call.
 *
 * Revision 3.29  2011/02/21 21:23:41  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 3.28  2010/11/13 16:03:15  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 3.27  2010/02/17 04:47:54  sueh
 * bug# 1301 Using the manager instead of the manager key do pop up
 * messages.
 *
 * Revision 3.26  2009/08/20 23:36:54  sueh
 * bug# 1255 Ported from 3.13.
 *
 * Revision 3.22.2.1  2009/08/20 22:43:35  sueh
 * bug# 1255 Added -CT option.  Preventing running without the -CT option
 * except during setup.  Prevent deletion of .rawtlt except during setup.
 *
 * Revision 3.25  2009/08/19 00:27:17  sueh
 * bug# 1255 Added -CT option to genOptions.
 *
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.process.SystemProgram;
import etomo.storage.DirectiveFile;
import etomo.storage.DirectiveFileCollection;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DataSource;
import etomo.type.DirectiveFileType;
import etomo.type.EtomoNumber;
import etomo.type.TiltAngleType;
import etomo.type.ViewType;
import etomo.ui.swing.UIHarness;

public final class CopyTomoComs {
  public static final String rcsid = "$Id$";

  private static final String CHANGE_PARAMETERS_FILE_TAG = "change";

  private final List<String> command = new ArrayList<String>();
  private final EtomoNumber voltage = new EtomoNumber();
  private final EtomoNumber sphericalAberration = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber ctfFiles = new EtomoNumber();

  private final ApplicationManager manager;

  private StringBuffer commandLine = null;
  private int exitValue;
  private ConstMetaData metaData;
  private boolean debug;
  private SystemProgram copytomocoms = null;
  private boolean directiveDrivenAutomation = false;
  private DirectiveFileCollection directiveFileCollection = null;

  public CopyTomoComs(final ApplicationManager manager) {
    this.manager = manager;
    metaData = manager.getConstMetaData();
    debug = EtomoDirector.INSTANCE.getArguments().isDebug();
  }

  public boolean setup() {
    if (!genOptions()) {
      return false;
    }
    String[] params = null;
    int size = command.size();
    if (size == 1) {
      params = new String[] { command.get(0) };
    }
    else {
      params = command.toArray(new String[size]);
    }
    String[] command = new String[] { "python", "-u",
        ApplicationManager.getIMODBinPath() + "copytomocoms", "-StandardInput" };
    copytomocoms = new SystemProgram(manager, manager.getPropertyUserDir(), command,
        AxisID.ONLY, true, true);
    if (params != null) {
      copytomocoms.setStdInput(params);
    }
    return true;
  }

  public void setDirectiveDrivenAutomation(final boolean input) {
    directiveDrivenAutomation = input;
  }

  public void setDirectiveFileCollection(final DirectiveFileCollection input) {
    directiveFileCollection = input;
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

  private void overrideParameter(final Map<String, String> commandMap, final String key,
      final String value) {
    if (commandMap.containsKey(key)) {
      commandMap.remove(key);
    }
    commandMap.put(key, value);
  }

  private boolean genOptions() {
    Map<String, String> commandMap = new HashMap<String, String>();
    // Add options from the directive file collection. This is the main source of
    // parameters for batch processing. For interactive processing, this fills in the
    // parameters that setup dialog doesn't know about. The parameters that setup dialog
    // does know about are used to override the directive file collection parameters in
    // the next section.
    if (directiveFileCollection != null) {
      // Load the setupset.copyarg directives.
      Iterator<Entry<String, String>> iterator = directiveFileCollection
          .getCopyArgEntrySet().iterator();
      if (iterator != null) {
        while (iterator.hasNext()) {
          Entry<String, String> entry = iterator.next();
          commandMap.put(entry.getKey(), entry.getValue());
        }
      }
    }
    if (!directiveDrivenAutomation) {
      // Override the parameters from the directive file collection.
      // Copytomocoms overrides the existing .com files. Make sure that the full
      // functionality is only used during setup.
      if (!manager.isNewManager() && ctfFiles.isNull()) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "ERROR:  Attempting to rebuild .com files when setup is already completed.",
            "Etomo Error");
        return false;
      }
      boolean montage = false;
      boolean gradient = false;
      // Dataset name
      overrideParameter(commandMap, DirectiveFile.NAME_NAME, metaData.getDatasetName());
      // View type: single or montaged
      if (metaData.getViewType() == ViewType.MONTAGE) {
        overrideParameter(commandMap, DirectiveFile.MONTAGE_NAME, "1");
        montage = true;
      }
      // Backup directory
      String backupDirectory = metaData.getBackupDirectory();
      if (!backupDirectory.equals("")) {
        overrideParameter(commandMap, "backup", backupDirectory);
      }
      // Data source: CCD or film
      if (metaData.getDataSource() == DataSource.FILM) {
        overrideParameter(commandMap, "film", "1");
      }
      // Pixel size
      overrideParameter(commandMap, DirectiveFile.PIXEL_NAME,
          String.valueOf(metaData.getPixelSize()));
      // Fiducial diameter
      overrideParameter(commandMap, DirectiveFile.GOLD_NAME,
          String.valueOf(metaData.getFiducialDiameter()));
      // Image rotation
      ConstEtomoNumber n = metaData.getImageRotation(AxisID.FIRST);
      if (!n.isNull()) {
        overrideParameter(commandMap, DirectiveFile.ROTATION_NAME, n.toString());
      }
      // A first tilt angle and tilt angle incriment
      if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
        overrideParameter(
            commandMap,
            DirectiveFile.FIRST_INC_NAME,
            String.valueOf(metaData.getTiltAngleSpecA().getRangeMin()) + ","
                + String.valueOf(metaData.getTiltAngleSpecA().getRangeStep()));
      }
      // Use an existing rawtilt file (this assumes that one is there and has
      // not been deleted by checkTiltAngleFiles()
      else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.FILE) {
        overrideParameter(commandMap, DirectiveFile.USE_RAW_TLT_NAME, "1");
      }
      // Extract the tilt angle data from the stack
      else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
        overrideParameter(commandMap, DirectiveFile.EXTRACT_NAME, "1");
      }
      // List of views to exclude from processing
      String excludeProjections = metaData.getExcludeProjectionsA();
      if (!excludeProjections.equals("")) {
        overrideParameter(commandMap, DirectiveFile.SKIP_NAME, excludeProjections);
      }
      if (metaData.isTwodir(AxisID.FIRST)) {
        overrideParameter(commandMap, DirectiveFile.TWODIR_NAME,
            metaData.getTwodir(AxisID.FIRST));
      }
      if (!voltage.isNull()) {
        overrideParameter(commandMap, DirectiveFile.VOLTAGE_NAME, voltage.toString());
      }
      if (!sphericalAberration.isNull()) {
        overrideParameter(commandMap, DirectiveFile.CS_NAME,
            sphericalAberration.toString());
      }
      // Only create ctf files.
      if (!ctfFiles.isNull()) {
        overrideParameter(commandMap, "CTFfiles", ctfFiles.toString());
      }
      // Undistort images with the given .idf file
      String distortionFile = metaData.getDistortionFile();
      if (!distortionFile.equals("")) {
        overrideParameter(commandMap, DirectiveFile.DISTORT_NAME, distortionFile);
      }
      // Binning of raw stacks (needed to undistort if ambiguous)
      overrideParameter(commandMap, DirectiveFile.BINNING_NAME,
          String.valueOf(metaData.getBinning()));
      // Mag gradients correction file
      String magGradientFile = metaData.getMagGradientFile();
      if (!magGradientFile.equals("")) {
        overrideParameter(commandMap, DirectiveFile.GRADIENT_NAME, magGradientFile);
        gradient = true;
        // It is only necessary to know if the focus was adjusted between montages
        // if a mag gradients correction file is being used.
        if (montage && metaData.getAdjustedFocusA().is()) {
          overrideParameter(commandMap, DirectiveFile.FOCUS_NAME, "1");
        }
      }

      // Axis type: single or dual
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        overrideParameter(commandMap, DirectiveFile.DUAL_NAME, "1");
        // There is only one image rotation value in setup dialog and it is used to set
        // both image rotation values. Use the directive file B image rotation if it
        // exists.
        String key = DirectiveFile.convertAttributeName(AxisID.SECOND,
            DirectiveFile.ROTATION_NAME);
        if (!commandMap.containsKey(key)) {
          n = metaData.getImageRotation(AxisID.SECOND);
          if (!n.isNull()) {
            commandMap.put(key, n.toString());
          }
        }
        // B first tilt angle and tilt angle incriment
        if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.RANGE) {
          overrideParameter(
              commandMap,
              DirectiveFile.convertAttributeName(AxisID.SECOND,
                  DirectiveFile.FIRST_INC_NAME),
              String.valueOf(metaData.getTiltAngleSpecB().getRangeMin()) + ","
                  + String.valueOf(metaData.getTiltAngleSpecB().getRangeStep()));
        }
        // Take tilt angle from a .rawtlt file - B
        else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.FILE) {
          overrideParameter(commandMap, DirectiveFile.convertAttributeName(AxisID.SECOND,
              DirectiveFile.USE_RAW_TLT_NAME), "1");
        }
        // Extract the tilt angle data from the stack - B
        else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
          overrideParameter(commandMap, DirectiveFile.convertAttributeName(AxisID.SECOND,
              DirectiveFile.EXTRACT_NAME), "1");
        }
        // List of views to exclude from processing - B
        excludeProjections = metaData.getExcludeProjectionsB();
        if (!excludeProjections.equals("")) {
          overrideParameter(commandMap,
              DirectiveFile.convertAttributeName(AxisID.SECOND, DirectiveFile.SKIP_NAME),
              excludeProjections);
        }
        if (metaData.isTwodir(AxisID.SECOND)) {
          overrideParameter(commandMap, DirectiveFile.convertAttributeName(AxisID.SECOND,
              DirectiveFile.TWODIR_NAME), metaData.getTwodir(AxisID.SECOND));
        }
        if (montage && gradient && metaData.getAdjustedFocusB().is()) {
          overrideParameter(
              commandMap,
              DirectiveFile.convertAttributeName(AxisID.SECOND, DirectiveFile.FOCUS_NAME),
              "1");
        }
      }
    }
    // Place the map entries in the command list.
    Set<Map.Entry<String, String>> entrySet = commandMap.entrySet();
    if (entrySet != null) {
      Iterator<Map.Entry<String, String>> iterator = entrySet.iterator();
      while (iterator.hasNext()) {
        Map.Entry entry = iterator.next();
        command.add(entry.getKey() + " " + entry.getValue());
      }
    }
    if (metaData.isSetFEIPixelSize()) {
      command.add("fei");
    }
    if (directiveFileCollection != null) {
      DirectiveFile directiveFile = directiveFileCollection
          .getDirectiveFile(DirectiveFileType.SCOPE);
      if (directiveFile != null) {
        command.add(CHANGE_PARAMETERS_FILE_TAG + " "
            + directiveFile.getFile().getAbsolutePath());
      }
      directiveFile = directiveFileCollection.getDirectiveFile(DirectiveFileType.SYSTEM);
      if (directiveFile != null) {
        command.add(CHANGE_PARAMETERS_FILE_TAG + " "
            + directiveFile.getFile().getAbsolutePath());
      }
      directiveFile = directiveFileCollection.getDirectiveFile(DirectiveFileType.USER);
      if (directiveFile != null) {
        command.add(CHANGE_PARAMETERS_FILE_TAG + " "
            + directiveFile.getFile().getAbsolutePath());
      }
      directiveFile = directiveFileCollection.getDirectiveFile(DirectiveFileType.BATCH);
      if (directiveFile != null) {
        command.add(CHANGE_PARAMETERS_FILE_TAG + " "
            + directiveFile.getFile().getAbsolutePath());
      }
    }
    // Options removed:
    // CCDEraser and local alignment entries
    // Always yes tiltalign relies on local entries to save default values
    // even if they are not used.
    return true;
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

    // Delete the rawtilt files if extract raw tilts is selected
    checkTiltAngleFiles();

    // Execute the script
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
   * The raw tilt files still need to be deleted because the user might be
   * restarting with a trimmed stack, making the old raw tilt files invalid.
   * Only do this during setup.  Don't delete when using option -CT.
   */
  private void checkTiltAngleFiles() {
    if (!manager.isNewManager() || !ctfFiles.isNull()) {
      return;
    }
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
