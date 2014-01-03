package etomo;

import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import etomo.storage.DataFileFilter;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.ViewType;
import etomo.ui.UIComponent;
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
  private static final String[] HELP_TAGS = new String[] { "--help", "--h", "-h" };
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

  private final List<String> paramFileNameList = new ArrayList<String>();

  private boolean debug = false;
  private DebugLevel debugLevel = DebugLevel.OFF;
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

  static void printHelpMessage() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(null, AutodocFactory.ETOMO, AxisID.ONLY);
      if (autodoc != null) {
        String dash = "-";
        if (autodoc.getAttribute(EtomoAutodoc.DOUBLE_DASH_ATTRIBUTE_NAME) != null) {
          dash = "--";
        }
        // Get this information in order
        SectionLocation sectionLocation = autodoc.getSectionLocation();
        if (sectionLocation != null) {
          ReadOnlySection section = null;
          ReadOnlyAttribute attribute = null;
          String attributeValue = null;
          String sectionType = null;
          while ((section = autodoc.nextSection(sectionLocation)) != null) {
            sectionType = section.getType();
            if (sectionType.equals(EtomoAutodoc.HEADER_SECTION_NAME)) {
              if ((attribute = section.getAttribute(EtomoAutodoc.USAGE_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                // Print section header
                System.out.println("\n" + attributeValue);
              }
            }
            else if (sectionType.equals(EtomoAutodoc.FIELD_SECTION_NAME)) {
              // Print parameter
              System.out.print(dash + section.getName());
              // Look for short parameter name
              if ((attribute = section.getAttribute(EtomoAutodoc.SHORT_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                System.out.print(" OR " + dash + attributeValue);
                if (section.getName().equals("help")) {
                  if (dash.equals("--")) {
                    System.out.print(" OR -" + attributeValue);
                  }
                  else {
                    System.out.print(" OR --" + attributeValue);
                  }
                }
              }
              // Look for value description
              if ((attribute = section.getAttribute(EtomoAutodoc.FORMAT_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                System.out.println("   " + stripManpageFormatting(attributeValue));
              }
              else if ((attribute = section
                  .getAttribute(EtomoAutodoc.TYPE_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                if (attributeValue.equals(EtomoAutodoc.BOOLEAN_TYPE)) {
                  System.out.println();
                }
                if (attributeValue.equals(EtomoAutodoc.FLOAT_TYPE)) {
                  System.out.println("   " + "Floating point");
                }
                if (attributeValue.equals(EtomoAutodoc.INTEGER_TYPE)) {
                  System.out.println("   " + "Integer");
                }
              }
              // Look for parameter description
              if ((attribute = section.getAttribute(EtomoAutodoc.USAGE_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                System.out.println("     " + attributeValue);
              }
              else if ((attribute = section
                  .getAttribute(EtomoAutodoc.MANPAGE_ATTRIBUTE_NAME)) != null
                  && (attributeValue = attribute.getValue()) != null) {
                System.out.println("     " + stripManpageFormatting(attributeValue));
              }
            }
          }
          return;
        }
      }
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    System.out.println("\nFor more information run 'man etomo'.");
  }

  private static String stripManpageFormatting(final String input) {
    if (input.indexOf("\\f") != -1) {
      String regexp = "\\\\f";
      return input.replaceAll(regexp + "B", "").replaceAll(regexp + "I", "")
          .replaceAll(regexp + "R", "");
    }
    return input;
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

  public DebugLevel getDebugLevel() {
    return debugLevel;
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
  void parse(String[] args) {
    test = false;
    selfTest = false;
    // Parse the command line arguments
    int i = 0;
    while (i < args.length) {
      // Filename argument should be the only one not beginning with at least
      // one dash.
      if (!args[i].startsWith("-")) {
        paramFileNameList.add(args[i]);
      }
      else {
        if (paramFileNameList.size() > 0) {
          // File name arguments can only be at the end of the command.
          errorMessageList.add("WARNING:  unknown argument(s), "
              + paramFileNameList.toString() + ", ignored.");
          paramFileNameList.clear();
        }
        boolean tagFound = false;
        for (int tagIndex = 0; tagIndex < HELP_TAGS.length; tagIndex++) {
          if (args[i].equals(HELP_TAGS[tagIndex])) {
            tagFound = true;
            break;
          }
        }
        if (tagFound) {
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
          // (displayMemoryInterval). If displayMemoryInterval is set, then the memory
          // usage
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
          debugLevel = DebugLevel.DEFAULT;
          // Optional value: --debug can be used alone, or followed by an integer
          // (debugLevel).
          if (i < args.length - 1 && !args[i + 1].startsWith("--")) {
            try {
              debugLevel = DebugLevel
                  .getInstance(Math.abs(Integer.parseInt(args[i + 1])));
              if (debugLevel == DebugLevel.OFF) {
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
          // Ignored optional value: the parallel processing description is currently
          // being
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
      }
      i++;
    }
  }

  /**
   * Returns null if the parameters are valid, or a list of error messages if one or more
   * parameters are not valid.
   * @return
   */
  boolean validate(final UIComponent component) {
    for (Iterator<String> i = paramFileNameList.iterator(); i.hasNext();) {
      File file = new File(i.next());
      if (!file.exists()) {
        errorMessageList.add("File parameter, " + file.getAbsolutePath()
            + ", does not exist.");
      }
      else {
        if (!file.canRead()) {
          errorMessageList.add("File parameter, " + file.getAbsolutePath()
              + ", is not readable.");
        }
        if (file.isDirectory()) {
          errorMessageList.add("File parameter, " + file.getAbsolutePath()
              + ", is a directory.");
        }
        if (!file.canWrite()) {
          errorMessageList.add("File parameter, " + file.getAbsolutePath()
              + ", is not writable.");
        }
        DataFileFilter fileFilter = new DataFileFilter();
        if (!fileFilter.accept(file)) {
          errorMessageList.add("File parameter, " + file.getAbsolutePath()
              + ", is not a " + fileFilter.getDescription() + ".");
        }
      }
    }
    if (directive) {
      if (fDirective == null) {
        errorMessageList.add("Missing " + DIRECTIVE_TAG + " parameter value.");
      }
      else {
        if (!fDirective.getName().endsWith(AutodocFactory.EXTENSION)) {
          errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
              + fDirective.getAbsolutePath() + ", has the wrong extension.");
        }
        if (!fDirective.exists()) {
          errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
              + fDirective.getAbsolutePath() + ", does not exist.");
        }
        else {
          if (!fDirective.canRead()) {
            errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
                + fDirective.getAbsolutePath() + ", is not readable.");
          }
          if (fDirective.isDirectory()) {
            errorMessageList.add(DIRECTIVE_TAG + " parameter value, "
                + fDirective.getAbsolutePath() + ", is a directory.");
          }
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
        if (!fDir.canRead()) {
          errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
              + ", is not readable.");
        }
        if (!fDir.isDirectory()) {
          errorMessageList.add(DIR_TAG + " parameter value, " + fDir.getAbsolutePath()
              + ", is not a directory.");
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

  List<String> getParamFileNameList() {
    return paramFileNameList;
  }

  boolean isHelp() {
    return help;
  }

  int getDisplayMemoryInterval() {
    return iDisplayMemory;
  }

  boolean isDisplayMemory() {
    return displayMemory;
  }

  public static final class DebugLevel {
    public static final DebugLevel OFF = new DebugLevel(0);
    private static final DebugLevel LIMITED = new DebugLevel(-1);
    public static final DebugLevel STANDARD = new DebugLevel(1);
    private static final DebugLevel EXTRA = new DebugLevel(2);
    private static final DebugLevel VERBOSE = new DebugLevel(3);
    public static final DebugLevel EXTRA_VERBOSE = new DebugLevel(4);

    private static final DebugLevel DEFAULT = STANDARD;

    private final int value;

    private DebugLevel(final int value) {
      this.value = value;
    }

    private static DebugLevel getInstance(final int value) {
      if (value == OFF.value) {
        return OFF;
      }
      if (value == LIMITED.value) {
        return LIMITED;
      }
      if (value == STANDARD.value) {
        return STANDARD;
      }
      if (value == EXTRA.value) {
        return EXTRA;
      }
      if (value == VERBOSE.value) {
        return VERBOSE;
      }
      if (value == EXTRA_VERBOSE.value) {
        return EXTRA_VERBOSE;
      }
      return DEFAULT;
    }

    public boolean isOn() {
      return this != OFF;
    }

    /**
     * Same as isOn, except that it excludes LIMITED.
     * @return
     */
    public boolean isStandard() {
      return this == STANDARD || this == EXTRA || this == VERBOSE
          || this == EXTRA_VERBOSE;
    }

    public boolean isExtra() {
      return this == EXTRA || this == VERBOSE || this == EXTRA_VERBOSE;
    }

    public boolean isVerbose() {
      return this == VERBOSE || this == EXTRA_VERBOSE;
    }

    public boolean isExtraVerbose() {
      return this == EXTRA_VERBOSE;
    }
  }
}
