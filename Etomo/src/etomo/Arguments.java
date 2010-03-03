package etomo;

import java.awt.GraphicsEnvironment;
import java.util.ArrayList;

import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.ViewType;
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
  private static final String DEMO_TAG = "--demo";
  private static final String DIR_TAG = "--dir";
  private static final String DATASET_TAG = "--dataset";
  private static final String AXIS_TAG = "--axis";
  private static final String FRAME_TAG = "--frame";
  private static final String FIDUCIAL_TAG = "--fiducial";
  private static final String SCAN_TAG = "--scan";
  private static final String CREATE_TAG = "--create";
  private static final String EXIT_TAG = "--exit";
  private static final String FG_TAG = "--fg";//foreground
  private static final String LISTEN_TAG = "--listen";

  static final String HELP_MESSAGE = "Options:\n  "
      + AXIS_TAG
      + " "
      + AxisType.SINGLE_AXIS.getValue()
      + "|"
      + AxisType.DUAL_AXIS.getValue()
      + "\n\t\tFor automation.  Sets the Axis Type in the Setup Tomogram dialog."
      + "\n\n  "
      + CREATE_TAG
      + "\tFor automation.  Runs Create Com Scripts in the Setup Tomogram"
      + "\n\t\tdialog."
      + "\n\n  "
      + DATASET_TAG
      + " tilt_series_file|dataset_name"
      + "\n\t\tFor automation.  Sets Dataset Name in the Setup  Tomogram dialog."
      + "\n\t\tCan be set to a file containing a tilt series or to the dataset"
      + "\n\t\t name.  A dataset name is the root name of the tilt series file,"
      + "\n\t\texcluding the extension (and the axis extension - \"a\" or \"b\""
      + "\n\t\t - in the case of dual axis).  Must be in the local directory"
      + "\n\t\tunless the "
      + DIR_TAG
      + " option is used."
      + "\n\n  "
      + DEBUG_TAG
      + "\tSend extra information to standard error.  The "
      + DEBUG_TAG
      + " option"
      + "\n\t\tincludes the following options:  "
      + MEMORY_TAG
      + " and "
      + TIMESTAMP_TAG
      + "."
      + "\n\n  "
      + DEMO_TAG
      + "\tDeprecated."
      + "\n\n  "
      + DIR_TAG
      + " \"directory_path\""
      + "\n\t\tFor automation.  The absolute or relative directory containing"
      + "\n\t\tthe file or dataset specified with the "
      + DATASET_TAG
      + " option."
      + "\n\n  "
      + EXIT_TAG
      + "\tFor automation.  Causes Etomo to exit after the Setup Tomogram dialog"
      + "\n\t\tis completed."
      + "\n\n  "
      + FG_TAG
      + "\t\tUsed with automation.  Must be the first option.  Causes Etomo to "
      + "\n\t\tbe run in the foreground rather then in the background.  This is"
      + "\n\t\tuseful when running Etomo with automation from a script; a"
      + "\n\t\tscript will not wait until Etomo is done unless Etomo is running"
      + "\n\t\tin the foreground."
      + "\n\n  "
      + FIDUCIAL_TAG
      + " fiducial_diameter"
      + "\n\t\tFor automation.  Sets the Fiducial Diameter (a double) in the"
      + "\n\t\tSetup Tomogram dialog."
      + "\n\n  "
      + FRAME_TAG
      + " "
      + ViewType.SINGLE_VIEW.getValue()
      + "|"
      + ViewType.MONTAGE.getValue()
      + "\n\t\tFor automation.  Sets the Frame Type in the Setup Tomogram"
      + "\n\t\tdialog."
      + "\n\n  "
      + HEADLESS_TAG
      + "\tFor testing.  No window is created.  Used for unit testing."
      + "\n\n  "
      + HELP1_TAG
      + ", "
      + HELP2_TAG
      + "\tSend this message to standard out and exit."
      + "\n\n  "
      + LISTEN_TAG
      + "\n\t\tForces all 3dmods to be run with the -L option.  This only has an "
      + "\n\t\teffect on Windows computers because -L is always used on Linux"
      + "\n\t\tand Mac."
      + "\n\n  "
      + MEMORY_TAG
      + " [interval]"
      + "\n\t\tLog memory usage statements before and after processes are run."
      + "\n\t\tThe interval is an integer which denotes the interval in minutes"
      + "\n\t\tat which to send additional memory usage statements."
      + "\n\n  "
      + NAMES_TAG
      + "\tFor testing.  Send the names of screen elements to standard out.  For"
      + "\n\t\twriting automated regression tests."
      + "\n\n  "
      + NEWSTUFF_TAG
      + "\tMay cause Etomo to run with unreleased functionality."
      + "\n\n  "
      + SCAN_TAG
      + "\tFor automation.  Runs Scan Header in the Setup Tomogram dialog."
      + "\n\n  "
      + SELFTEST_TAG
      + "\tCauses Etomo to do some internal testing.  Etomo may run more slowly."
      + "\n\n  "
      + TEST_TAG
      + "\tFor testing.  Test mode used for unit testing and automated"
      + "\n\t\tregression testing."
      + "\n\n  "
      + TIMESTAMP_TAG
      + "\tSend timestamps to standard error before and after processes are run."
      + "\n";

  private final ArrayList paramFileNameList = new ArrayList();

  private boolean debug = false;
  private boolean demo = false;
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
  private int displayMemoryInterval = 0;
  private AxisType axis = null;
  private ViewType frame = null;
  private boolean scan = false;
  private String dataset = null;
  private String dir = null;
  private boolean create = false;
  private boolean exit = false;
  private String automationFile = null;
  private boolean listen = false;

  private final EtomoNumber fiducial = new EtomoNumber(EtomoNumber.Type.DOUBLE);

  Arguments() {
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

  public boolean isDemo() {
    return demo;
  }

  public AxisType getAxis() {
    return axis;
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

  public boolean isAutomation() {
    return automationFile != null;
  }

  public String getAutomationFile() {
    return automationFile;
  }

  public boolean isCreate() {
    return create;
  }

  public boolean isExit() {
    return exit;
  }

  public boolean isPrintNames() {
    return printNames;
  }

  public boolean isListen() {
    return listen;
  }

  public boolean isNewstuff() {
    return newstuff;
  }

  public ViewType getFrame() {
    return frame;
  }

  public String getDataset() {
    return dataset;
  }

  public String getDir() {
    return dir;
  }

  public ConstEtomoNumber getFiducial() {
    return fiducial;
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
    //  Parse the command line arguments
    int i = 0;
    while (i < args.length) {
      // Filename argument should be the only one not beginning with at least
      // one dash
      if (!args[i].startsWith("-")) {
        paramFileNameList.add(args[i]);
      }
      else if (args[i].equals(HELP1_TAG) || args[i].equals(HELP2_TAG)) {
        help = true;
      }
      else if (args[i].equals(DEBUG_TAG)) {
        debug = true;
      }
      else if (args[i].equals(DEMO_TAG)) {
        demo = true;
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
        //--memory can be used alone, or followed by an integer
        //(displayMemoryInterval).  If displayMemoryInterval is set, then the
        //memory usage will be sent to etomo_err.log every displayMemoryInterval
        //minutes.
        if (i < args.length - 1) {
          try {
            displayMemoryInterval = Math.abs(Integer.parseInt(args[i + 1]));
            i++;
          }
          catch (NumberFormatException e) {
            displayMemoryInterval = 0;
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
        if (i < args.length - 1) {
          dataset = args[i + 1];
          i++;
        }
      }
      else if (args[i].equals(DIR_TAG)) {
        if (i < args.length - 1) {
          //the quotes will be stripped by the program
          dir = args[i + 1];
          i++;
        }
      }
      else if (args[i].equals(AXIS_TAG)) {
        if (i < args.length - 1) {
          axis = AxisType.fromString(args[i + 1]);
          if (axis != null) {
            i++;
          }
        }
      }
      else if (args[i].equals(FRAME_TAG)) {
        if (i < args.length - 1) {
          frame = ViewType.fromString(args[i + 1]);
          if (frame != null) {
            i++;
          }
        }
      }
      else if (args[i].equals(FIDUCIAL_TAG)) {
        if (i < args.length - 1) {
          fiducial.set(args[i + 1]);
          if (!fiducial.isNull()) {
            i++;
          }
        }
      }
      else if (args[i].equals(SCAN_TAG)) {
        scan = true;
      }
      else if (args[i].equals(CREATE_TAG)) {
        create = true;
      }
      else if (args[i].equals(EXIT_TAG)) {
        exit = true;
      }
      else if (args[i].equals(FG_TAG)) {
        if (i > 0) {
          System.err
              .println("WARNING:  option --fg had no effect; must be the first option to have an effect.");
        }
      }
      else if (args[i].equals(LISTEN_TAG)) {
        listen = true;
      }
      else {
        System.err.println("WARNING:  unknown argument, " + args[i]
            + ", ignored.");
      }
      i++;
    }
    return paramFileNameList;
  }

  ArrayList getParamFileNameList() {
    return paramFileNameList;
  }

  boolean isHelp() {
    return help;
  }

  int getDisplayMemoryInterval() {
    return displayMemoryInterval;
  }

  boolean isDisplayMemory() {
    return displayMemory;
  }
}
