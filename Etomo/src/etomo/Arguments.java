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
 * <p> $Log$ </p>
 */
public final class Arguments {
  public static final String rcsid = "$Id$";

  public static final String SELFTEST_TAG = "--selftest";
  public static final String TEST_TAG = "--test";

  static final String HEADLESS_TAG = "--headless";

  private static final String HELP1_TAG = "-h";
  private static final String HELP2_TAG = "--help";
  private static final String DEBUG_TAG = "--debug";
  private static final String MEMORY_TAG = "--memory";
  private static final String NAMES_TAG = "--names";
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

  static final String HELP_MESSAGE = "\nOptions:\n  "
      + DEBUG_TAG
      + "\tSend extra information to standard error.  The "
      + DEBUG_TAG
      + " option\n\t\tincludes the following options:  "
      + MEMORY_TAG
      + " and "
      + TIMESTAMP_TAG
      + ".\n  "
      + HELP1_TAG
      + ", "
      + HELP2_TAG
      + "\tSend this message to standard out and exit.\n  "
      + MEMORY_TAG
      + " [Integer]\n\t\tSend memory usage statements to standard error before "
      + "and\n\t\tafter processes are run.  The integer is the interval (in\n\t\t"
      + "minutes) at which to send additional memory usage statements.\n  "
      + NAMES_TAG
      + "\tSend the names of screen elements to standard out.  For writing\n\t\t"
      + "automated regression tests.\n  "
      + NEWSTUFF_TAG
      + "\tMay cause Etomo to run with unreleased functionality.\n  "
      + SELFTEST_TAG
      + "\tCauses Etomo to do some internal testing.  Etomo may run more\n\t\t"
      + "slowly.\n  "
      + TIMESTAMP_TAG
      + "\tSend timestamps to standard error before and after processes\n\t\tare "
      + "run.\n\nReconstruction Setup Options:\n  "
      + DATASET_TAG
      + " tilt_series_file | dataset_name\n\t\tSets Dataset Name in the Setup "
      + "Tomogram dialog.  Can be set to\n\t\ta file containing a tilt series or "
      + "to the dataset name.  A\n\t\tdataset name is the root name of the tilt "
      + "series file,\n\t\texcluding the extension (and the axis extension - "
      + "\"a\" or \"b\" -\n\t\tin the case of dual axis).  Must be in the local "
      + "directory\n\t\tunless the "
      + DIR_TAG
      + " option is used.\n  "
      + DIR_TAG
      + " \"directory_path\"\n\t\tThe absolute or relative directory of the file "
      + "or dataset\n\t\tspecified with the "
      + DATASET_TAG
      + " option.\n  "
      + AXIS_TAG
      + " "
      + AxisType.SINGLE_AXIS.getValue()
      + "|"
      + AxisType.DUAL_AXIS.getValue()
      + "\n\t\tSets the Axis Type in the Setup Tomogram dialog.  \n  "
      + FRAME_TAG
      + " "
      + ViewType.SINGLE_VIEW.getValue()
      + "|"
      + ViewType.MONTAGE.getValue()
      + "\n\t\tSets the Frame Type in the Setup Tomogram dialog.\n  "
      + FIDUCIAL_TAG
      + " Double\n\t\tSets the Fiducial Diameter in the Setup Tomogram dialog.\n  "
      + SCAN_TAG
      + "\tRuns Scan Header in the Setup Tomogram dialog.\n  "
      + CREATE_TAG
      + "\tRuns Create Com Scripts in the Setup Tomogram dialog.\n  "
      + EXIT_TAG
      + "\tCauses Etomo to exit after the Setup Tomogram dialog is completed.\n\nOther Options:\n  Testing Only:\n    "
      + HEADLESS_TAG
      + "\tNo window is created.  Used for unit testing.\n    "
      + TEST_TAG
      + "\tTest mode used for unit testing and automated regression\n\t\ttesting."
      + "\n  Deprecated:\n    " + DEMO_TAG + "\n";

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

  public boolean isScan() {
    return scan;
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
