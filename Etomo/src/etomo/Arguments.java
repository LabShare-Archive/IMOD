package etomo;

import java.awt.GraphicsEnvironment;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.ViewType;
import etomo.ui.swing.UIComponent;
import etomo.ui.swing.UIHarness;
import etomo.util.Utilities;

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
 * 
 * <p> $Log$
 * <p> Revision 1.15  2011/08/25 21:49:24  sueh
 * <p> Bug# 1441 Added --actions parameter.
 * <p>
 * <p> Revision 1.14  2011/02/10 04:30:49  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.13  2010/10/13 20:17:21  sueh
 * <p> bug# 1392 Added ignoreSettings.  Added another suptopic in the help
 * <p> mesage.
 * <p>
 * <p> Revision 1.12  2010/09/08 19:15:01  sueh
 * <p> bug# 1401 Added reconAutomation.
 * <p>
 * <p> Revision 1.11  2010/07/07 21:21:34  sueh
 * <p> bug# 1387 Added debugLevel.
 * <p>
 * <p> Revision 1.10  2010/06/04 20:00:55  sueh
 * <p> bug# 1380 Added --ignoreloc.
 * <p>
 * <p> Revision 1.9  2010/04/10 00:28:41  sueh
 * <p> bug# 1349 Added --autoclose3dmod and reorganized help string.
 * <p>
 * <p> Revision 1.8  2010/03/03 04:50:04  sueh
 * <p> bug# 1311 Added setDebug.
 * <p>
 * <p> Revision 1.7  2009/09/22 20:42:28  sueh
 * <p> bug# 1259 Made debug tab public.
 * <p>
 * <p> Revision 1.6  2009/05/06 20:45:02  sueh
 * <p> bug# 1207 Found a simpler way of exiting etomo.  Removed setExit.
 * <p>
 * <p> Revision 1.5  2009/04/13 22:19:26  sueh
 * <p> bug# 1207 Added setExit so that exit automation can be turned on by Etomo.
 * <p>
 * <p> Revision 1.4  2009/03/23 16:46:19  sueh
 * <p> bug# 1187 Added --listen parameter.
 * <p>
 * <p> Revision 1.3  2009/03/09 17:22:06  sueh
 * <p> bug# 1172 For now just adding --fg option.
 * <p>
 * <p> Revision 1.2  2008/11/11 23:46:31  sueh
 * <p> bug# 1149 Made --names option available for testing.
 * <p>
 * <p> Revision 1.1  2007/12/26 21:54:20  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p> </p>
 */
public final class Arguments {
  public static final String rcsid = "$Id$";

  public static final String NAMES_TAG = "--names";
  public static final String SELFTEST_TAG = "--selftest";
  public static final String TEST_TAG = "--test";
  static final String HEADLESS_TAG = "--headless";
  private static final String HELP1_TAG = "-h";
  private static final String HELP2_TAG = "--help";
  public static final String DEBUG_TAG = "--debug";
  private static final String MEMORY_TAG = "--memory";
  private static final String NEWSTUFF_TAG = "--newstuff";
  private static final String TIMESTAMP_TAG = "--timestamp";
  /**
   * @deprecated
   */
  private static final String DEMO_TAG = "--demo";
  private static final String DIR_TAG = "--dir";
  private static final String DATASET_TAG = "--dataset";
  private static final String AXIS_TAG = "--axis";
  private static final String FRAME_TAG = "--frame";
  private static final String FIDUCIAL_TAG = "--fiducial";
  private static final String SCAN_TAG = "--scan";
  private static final String CREATE_TAG = "--create";
  private static final String EXIT_TAG = "--exit";
  private static final String FG_TAG = "--fg";// foreground
  private static final String LISTEN_TAG = "--listen";
  private static final String AUTO_CLOSE_3DMOD_TAG = "--autoclose3dmod";
  private static final String IGNORE_LOC_TAG = "--ignoreloc";
  private static final String IGNORE_SETTINGS_TAG = "--ignoresettings";
  private static final String ACTIONS_TAG = "--actions";
  public static final String DIRECTIVE_TAG = "--directive";
  private static final String CPUS_TAG = "--cpus";
  private static final String GPUS_TAG = "--gpus";
  private static final String FROM_BRT_TAG = "--fromBRT";

  static final String HELP_MESSAGE = "\nOptions:" + "\n  " + HELP1_TAG + ", " + HELP2_TAG
      + "\tSend this message to standard out and exit."

      + "\n\n  " + LISTEN_TAG
      + "\tForces all 3dmods to be run with the -L option.  This only has"
      + "\n\t\tan effect on Windows computers because -L is always used on"
      + "\n\t\tLinux and Mac."

      + "\n\n  " + TIMESTAMP_TAG
      + "\tSend timestamps to standard error before and after processes"
      + "\n\t\tare run."

      + "\n\nFile-Based Automation Options:"

      + "\n  " + FROM_BRT_TAG
      + "\tPrevents eTomo from validating the directive file and the"
      + "\n\t\ttemplate files.  Used by batchruntomo.  Not useful when eTomo"
      + "\n\t\tis called from the command line."

      + "\n\n  " + DIRECTIVE_TAG + " \"directive-file.adoc\""
      + "\n\t\tCauses automation to be performed based on the directive file."
      + "\n\t\tNo interface will come up and most command-line-based"
      + "\n\t\tautomation options (below) will be ignored.  However " + CPUS_TAG + " and"
      + "\n\t\t" + GPUS_TAG + " work with both types of automation.  Implied options:"
      + "\n\t\t" + FG_TAG + ", " + HEADLESS_TAG + ", " + CREATE_TAG + ", and " + EXIT_TAG
      + ".  The directive file must end in \".adoc\" and conform to autodoc syntax."

      + "\n\nCommand-Line-Based Automation Options:"

      + "\n  " + AXIS_TAG + " " + AxisType.SINGLE_AXIS.getValue() + "|"
      + AxisType.DUAL_AXIS.getValue()
      + "\n\t\tFor automation.  Sets the Axis Type in the Setup Tomogram"
      + "\n\t\tdialog."

      + "\n\n  " + CPUS_TAG + " [ignored]"
      + "\n\t\tTurns on the Parallel Processing checkbox in the Setup Tomogram"
      + "\n\t\tdialog."

      + "\n\n  " + CREATE_TAG
      + "\tFor automation.  Runs Create Com Scripts in the Setup Tomogram"
      + "\n\t\tdialog."

      + "\n\n  " + DATASET_TAG + " tilt_series_file|dataset_name"
      + "\n\t\tFor automation.  Sets Dataset Name in the Setup Tomogram"
      + "\n\t\tdialog.  Can be set to a file containing a tilt series or to"
      + "\n\t\tthe dataset name.  A dataset name is the root name of the tilt"
      + "\n\t\tseries file, excluding the extension - and the axis extension"
      + "\n\t\t(\"a\" or \"b\") in the case of dual axis.  Must be in the local"
      + "\n\t\tdirectory unless the " + DIR_TAG + " option is used."

      + "\n\n  " + DIR_TAG + " \"directory_path\""
      + "\n\t\tFor automation.  The absolute or relative directory containing"
      + "\n\t\tthe file or dataset specified with the " + DATASET_TAG + " option."

      + "\n\n  " + EXIT_TAG
      + "\tFor automation.  Causes Etomo to exit after the Setup Tomogram"
      + "\n\t\tdialog is completed."

      + "\n\n  " + FG_TAG
      + "\t\tUsed with automation.  Causes Etomo to be run in the foreground"
      + "\n\t\trather then in the background.  This is useful when running"
      + "\n\t\tEtomo with automation from a script; a script will not wait"
      + "\n\t\tuntil Etomo is done unless Etomo is running in the foreground."

      + "\n\n  " + FIDUCIAL_TAG + " fiducial_diameter"
      + "\n\t\tFor automation.  Sets the Fiducial Diameter (a double) in the"
      + "\n\t\tSetup Tomogram dialog."

      + "\n\n  " + FRAME_TAG + " " + ViewType.SINGLE_VIEW.getParamValue() + "|"
      + ViewType.MONTAGE.getParamValue()
      + "\n\t\tFor automation.  Sets the Frame Type in the Setup Tomogram"
      + "\n\t\tdialog."

      + "\n\n  " + GPUS_TAG + " [ignored]"
      + "\n\t\tTurns on the Graphics Card Processing checkbox in the Setup"
      + "\n\t\tTomogram dialog."

      + "\n\n  " + SCAN_TAG
      + "\tFor automation.  Runs Scan Header in the Setup Tomogram dialog."

      + "\n\nDiagnostic Options:"

      + "\n  " + ACTIONS_TAG
      + "\tPrint actions and file names, without printing other debug"
      + "\n\t\tinformation.  File names which do not contain an extension or"
      + "\n\t\tare entirely numeric will not be printed."

      + "\n\n  " + DEBUG_TAG + " [level]"
      + "\n\t\tSend extra information to standard error.  The " + DEBUG_TAG + " option"
      + "\n\t\tincludes the following options:  " + MEMORY_TAG + " and " + TIMESTAMP_TAG
      + ".\n\t\tLevel can be 0 (debug is off), 1 (default), 2 (more"
      + "\n\t\tinformation), or 3 (output that may degrade eTomo's"
      + "\n\t\tperformance)."

      + "\n\n  " + IGNORE_SETTINGS_TAG
      + "\n\t\tPrevents the .etomo from loading from and saving to the .etomo"
      + "\n\t\tconfiguration file."

      + "\n\n  " + MEMORY_TAG + " [interval]"
      + "\n\t\tLog memory usage statements before and after processes are run."
      + "\n\t\tThe interval is an integer which denotes the interval in"
      + "\n\t\tminutes at which to send additional memory usage statements."

      + "\n\n  " + SELFTEST_TAG
      + "\tCauses Etomo to do some internal testing.  Etomo may run more"
      + "\n\t\tslowly."

      + "\n\nDevelopment and Testing Options:"

      + "\n  " + AUTO_CLOSE_3DMOD_TAG
      + "\n\t\tFor user interface testing.  Instead of popping up a message"
      + "\n\t\tasking to close an open 3dmod instance, Etomo automatically"
      + "\n\t\tcloses the 3dmod instance."

      + "\n\n  " + HEADLESS_TAG
      + "\tFor testing.  No window is created.  Used for unit testing."

      + "\n\n  " + IGNORE_LOC_TAG
      + "\tFor user interface testing.  Keeps eTomo from using the last"
      + "\n\t\tlocation information that is saved in .etomo."

      + "\n\n  " + NAMES_TAG
      + "\tFor testing.  Send the names of screen elements to standard"
      + "\n\t\tout.  For writing automated regression tests."

      + "\n\n  " + NEWSTUFF_TAG
      + "\tMay cause Etomo to run with unreleased functionality."

      + "\n\n  " + TEST_TAG
      + "\tFor testing.  Test mode is used for unit testing and automated"
      + "\n\t\tregression testing."

      + "\n\nDeprecated Options:"

      + "\n  " + DEMO_TAG + "\tDeprecated.";

  private final ArrayList paramFileNameList = new ArrayList();

  private boolean debug = false;
  private int debugLevel = 0;
  /**
   * If arguments hasn't been initialized yet, then
   * assume that this is a test because UITest does alot of work before it can
   * run EtomoDirector.main.
   */
  private boolean test = true;
  private boolean headless = GraphicsEnvironment.isHeadless();
  /**
   * If arguments hasn't been initialized yet, then
   * assume that this is a self test is true because UITest does alot of work
   * before it can
   * run EtomoDirector.main.
   */
  private boolean selfTest = true;
  private boolean newstuff = false;
  private boolean displayMemory = false;
  private boolean help = false;
  private boolean printNames = false;
  private int iDisplayMemory = 0;
  private boolean axis = false;
  private AxisType atAxis = null;
  private boolean frame = false;
  private ViewType vtFrame = null;
  private boolean scan = false;
  private boolean dataset = false;
  private String sDataset = null;
  private boolean dir = false;
  private File fDir = null;
  private boolean create = false;
  private boolean exit = false;
  private boolean listen = false;
  private boolean autoClose3dmod = false;
  private boolean ignoreLoc = false;
  private boolean reconAutomation = false;
  private boolean ignoreSettings = false;
  private boolean actions = false;
  private boolean directive = false;
  private File fDirective = null;
  private List<String> errorMessageList = new ArrayList<String>();
  private List<String> warningMessageList = new ArrayList<String>();
  private boolean fiducial = false;
  private boolean cpus = false;
  private boolean gpus = false;
  private boolean fromBRT = false;

  private final EtomoNumber enFiducial = new EtomoNumber(EtomoNumber.Type.DOUBLE);

  Arguments() {
  }

  public boolean isReconAutomation() {
    return reconAutomation;
  }

  public boolean isHeadless() {
    return headless;
  }

  public boolean isDebug() {
    return debug;
  }

  public boolean isSelfTest() {
    return selfTest;
  }

  /**
   * @deprecated
   * @return
   */
  public boolean isDemo() {
    return false;
  }

  public boolean isDirective() {
    return directive;
  }

  public File getDirective() {
    return fDirective;
  }

  public AxisType getAxis() {
    return atAxis;
  }

  public boolean isTest() {
    return test;
  }

  public void setDebug(final boolean input) {
    debug = input;
  }

  public boolean isScan() {
    return scan;
  }

  public boolean isCreate() {
    return create;
  }

  public boolean isExit() {
    return exit;
  }
  
  public boolean isFromBRT() {
    return fromBRT;
  }

  public boolean isPrintNames() {
    return printNames;
  }

  public boolean isListen() {
    return listen;
  }

  public boolean isAutoClose3dmod() {
    return autoClose3dmod;
  }

  public boolean isNewstuff() {
    return newstuff;
  }

  public ViewType getFrame() {
    return vtFrame;
  }

  public String getDataset() {
    return sDataset;
  }

  public File getDir() {
    return fDir;
  }

  public ConstEtomoNumber getFiducial() {
    return enFiducial;
  }

  public boolean isIgnoreLoc() {
    return ignoreLoc;
  }

  public boolean isIgnoreSettings() {
    return ignoreSettings;
  }

  public boolean isActions() {
    return actions;
  }

  public boolean isCpus() {
    return cpus;
  }

  public boolean isGpus() {
    return gpus;
  }

  /**
   * Parse the command line. This method will return a non-empty string if there
   * is a etomo data .
   * 
   * @param The
   *          command line arguments
   * @return A string that will be set to the etomo data filename if one is
   *         found on the command line otherwise it is "".
   */
  ArrayList parse(String[] args) {
    test = false;
    selfTest = false;
    // Parse the command line arguments
    int i = 0;
    while (i < args.length) {
      // Filename argument should be the only one not beginning with at least
      // one dash
      if (!args[i].startsWith("--")) {
        paramFileNameList.add(args[i]);
      }
      else if (args[i].equals(HELP1_TAG) || args[i].equals(HELP2_TAG)) {
        help = true;
      }
      else if (args[i].equals(TEST_TAG)) {
        test = true;
      }
      else if (args[i].equals(HEADLESS_TAG)) {
        headless = true;
      }
      else if (args[i].equals(SELFTEST_TAG)) {
        selfTest = true;
      }
      else if (args[i].equals(NAMES_TAG)) {
        printNames = true;
      }
      else if (args[i].equals(MEMORY_TAG)) {
        displayMemory = true;
        // Optional value: --memory can be used alone, or followed by an integer
        // (displayMemoryInterval). If displayMemoryInterval is set, then the memory usage
        // will be sent to etomo_err.log every displayMemoryInterval minutes.
        if (i < args.length - 1 && !args[i + 1].startsWith("--")) {
          try {
            iDisplayMemory = Math.abs(Integer.parseInt(args[i + 1]));
            i++;
          }
          catch (NumberFormatException e) {
            iDisplayMemory = 0;
          }
        }
      }
      else if (args[i].equals(DEBUG_TAG)) {
        debug = true;
        debugLevel = 1;
        // Optional value: --debug can be used alone, or followed by an integer
        // (debugLevel).
        if (i < args.length - 1 && !args[i + 1].startsWith("--")) {
          try {
            debugLevel = Math.abs(Integer.parseInt(args[i + 1]));
            if (debugLevel == 0) {
              debug = false;
            }
            i++;
          }
          catch (NumberFormatException e) {
          }
        }
      }
      else if (args[i].equals(TIMESTAMP_TAG)) {
        Utilities.setTimestamp(true);
      }
      else if (args[i].equals(NEWSTUFF_TAG)) {
        newstuff = true;
      }
      else if (args[i].equals(DATASET_TAG)) {
        dataset = true;
        reconAutomation = true;
        if (i < args.length - 1) {
          sDataset = args[i + 1];
          i++;
        }
      }
      else if (args[i].equals(DIR_TAG)) {
        dir = true;
        reconAutomation = true;
        if (i < args.length - 1) {
          // The quotes have already been stripped.
          fDir = new File(args[i + 1]);
          i++;
        }
      }
      else if (args[i].equals(AXIS_TAG)) {
        axis = true;
        reconAutomation = true;
        if (i < args.length - 1) {
          atAxis = AxisType.fromString(args[i + 1]);
          if (atAxis != null) {
            i++;
          }
        }
      }
      else if (args[i].equals(FRAME_TAG)) {
        frame = true;
        reconAutomation = true;
        if (i < args.length - 1) {
          vtFrame = ViewType.fromString(args[i + 1]);
          if (vtFrame != null) {
            i++;
          }
        }
      }
      else if (args[i].equals(FIDUCIAL_TAG)) {
        fiducial = true;
        reconAutomation = true;
        if (i < args.length - 1) {
          enFiducial.set(args[i + 1]);
          if (!enFiducial.isNull()) {
            i++;
          }
        }
      }
      else if (args[i].equals(SCAN_TAG)) {
        reconAutomation = true;
        scan = true;
      }
      else if (args[i].equals(CREATE_TAG)) {
        reconAutomation = true;
        create = true;
      }
      else if (args[i].equals(EXIT_TAG)) {
        exit = true;
      }
      else if (args[i].equals(FG_TAG)) {
        reconAutomation = true;
      }
      else if (args[i].equals(LISTEN_TAG)) {
        listen = true;
      }
      else if (args[i].equals(AUTO_CLOSE_3DMOD_TAG)) {
        autoClose3dmod = true;
      }
      else if (args[i].equals(IGNORE_LOC_TAG)) {
        ignoreLoc = true;
      }
      else if (args[i].equals(IGNORE_SETTINGS_TAG)) {
        ignoreSettings = true;
      }
      else if (args[i].equals(ACTIONS_TAG)) {
        actions = true;
      }
      else if (args[i].equals(DIRECTIVE_TAG)) {
        directive = true;
        reconAutomation = true;
        headless = true;
        create = true;
        exit = true;
        if (i < args.length - 1) {
          // The quotes have already been stripped.
          fDirective = new File(args[i + 1]);
          i++;
        }
      }
      else if (args[i].equals(CPUS_TAG)) {
        cpus = true;
        // Ignored optional value: the parallel processing description is currently being
        // ignored.
        if (i < args.length - 1 && !args[i + 1].startsWith("--")) {
          i++;
        }
      }
      else if (args[i].equals(GPUS_TAG)) {
        gpus = true;
        // Ignored optional value: the gpu processing description is currently being
        // ignored.
        if (i < args.length - 1 && !args[i + 1].startsWith("--")) {
          i++;
        }
      }
      else if (args[i].equals(FROM_BRT_TAG)) {
        fromBRT = true;
      }
      else {
        errorMessageList.add("WARNING:  unknown argument, " + args[i] + ", ignored.");
      }
      i++;
    }
    return paramFileNameList;
  }

  /**
   * Returns null if the parameters are valid, or a list of error messages if one or more
   * parameters are not valid.
   * @return
   */
  boolean validate(final UIComponent component) {
    if (directive) {
      if (fDirective == null) {
        errorMessageList.add("Missing " + DIRECTIVE_TAG + " parameter value.");
      }
      else if (!fDirective.exists()) {
        errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
            + fDirective.getAbsolutePath() + ", does not exist.");
      }
      else {
        if (fDirective.isDirectory()) {
          errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
              + fDirective.getAbsolutePath() + ", is a directory.");
        }
        if (!fDirective.canRead()) {
          errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
              + fDirective.getAbsolutePath() + ", is not readable.");
        }
      }
      if (axis || dataset || dir || fiducial || frame) {
        warningMessageList.add("The parameters: " + AXIS_TAG + ", " + DATASET_TAG + ", "
            + DIR_TAG + ", " + FIDUCIAL_TAG + ", and " + FRAME_TAG + " are ignored when "
            + DIRECTIVE_TAG + " is used.");
      }
    }
    if (axis && atAxis == null) {
      errorMessageList.add("Missing or invalid " + AXIS_TAG + " parameter value.");
    }
    if (dataset && sDataset == null) {
      errorMessageList.add("Missing " + DATASET_TAG + " parameter value.");
    }
    if (dir) {
      if (fDir == null) {
        errorMessageList.add("Missing " + DIR_TAG + " parameter value.");
      }
      else if (!fDir.exists()) {
        errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
            + ", does not exist.");
      }
      else {
        if (!fDir.isDirectory()) {
          errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
              + ", is not a directory.");
        }
        if (!fDir.canRead()) {
          errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
              + ", is not readable.");
        }
        if (!fDir.canWrite()) {
          errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
              + ", is not writable.");
        }
      }
    }
    if (fiducial) {
      if (enFiducial.isNull()) {
        String errorMessage = enFiducial.getInvalidReason();
        if (errorMessage.equals("")) {
          errorMessageList.add("Missing " + FIDUCIAL_TAG + " parameter value.");
        }
        else {
          errorMessageList.add(FIDUCIAL_TAG + " parameter value is an invalid number: "
              + errorMessage);
        }
      }
      else if (enFiducial.isNegative()) {
        errorMessageList.add(FIDUCIAL_TAG + " parameter value, " + enFiducial
            + ", cannot be negative.");
      }
    }
    if (frame && vtFrame == null) {
      errorMessageList.add("Missing or invalid " + FRAME_TAG + " parameter value.");
    }
    if (!errorMessageList.isEmpty()) {
      String title = "Parameter Error";
      String message = "Error in the parameter list:";
      if (errorMessageList.size() == 1) {
        UIHarness.INSTANCE.openMessageDialog(component,
            message + "\n" + errorMessageList.get(0), title);
      }
      else {
        errorMessageList.add(0, message);
        UIHarness.INSTANCE.openMessageDialog(component,
            errorMessageList.toArray(new String[errorMessageList.size()]), title);
      }
      return false;
    }
    if (!warningMessageList.isEmpty()) {
      String title = "Parameter Warning";
      String message = "Parameter Warning:";
      if (warningMessageList.size() == 1) {
        UIHarness.INSTANCE.openMessageDialog(component, message + "\n"
            + warningMessageList.get(0), title);
      }
      else {
        warningMessageList.add(0, message);
        UIHarness.INSTANCE.openMessageDialog(component,
            warningMessageList.toArray(new String[warningMessageList.size()]), title);
      }
    }
    return true;
  }

  ArrayList getParamFileNameList() {
    return paramFileNameList;
  }

  boolean isHelp() {
    return help;
  }

  int getDisplayMemoryInterval() {
    return iDisplayMemory;
  }

  public boolean isDebugLevel(final int level) {
    return debugLevel >= level;
  }

  boolean isDisplayMemory() {
    return displayMemory;
  }
}
