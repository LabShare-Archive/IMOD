package etomo.uitest;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class UITestAction {
  public static final String rcsid = "$Id$";

  private static final String COPY_STRING = "copy";
  private static final String DATA_DIR_STRING = "datadir";
  private static final String DATA_FILE_STRING = "datafile";
  private static final String DATASET_STRING = "dataset";
  private static final String DATASET_DIR_STRING = "datasetdir";
  private static final String DURATION_STRING = "duration";
  private static final String FUNCTION_STRING = "function";
  private static final String SET_STRING = "set";
  private static final String SLEEP_STRING = "sleep";
  private static final String STOP_STRING = "stop";
  private static final String TEST_FROM_STRING = "testfrom";
  private static final String VERBOSE_STRING = "verbose";
  private static final String WAIT_FOR_STRING = "waitfor";

  public static final UITestAction WAIT_FOR = new UITestAction(WAIT_FOR_STRING);
  static final UITestAction ADOC = new UITestAction("adoc");
  static final UITestAction ASSERT = new UITestAction("assert");
  static final UITestAction COPY = new UITestAction(COPY_STRING);
  static final UITestAction DATA_FILE = new UITestAction(DATA_FILE_STRING);
  static final UITestAction DATASET = new UITestAction(DATASET_STRING);
  static final UITestAction DATASET_DIR = new UITestAction(DATASET_DIR_STRING);
  static final UITestAction DURATION = new UITestAction(DURATION_STRING);
  static final UITestAction DATA_DIR = new UITestAction(DATA_DIR_STRING);
  static final UITestAction FUNCTION = new UITestAction(FUNCTION_STRING);
  static final UITestAction SET = new UITestAction(SET_STRING);
  static final UITestAction SLEEP = new UITestAction(SLEEP_STRING);
  static final UITestAction STOP = new UITestAction(STOP_STRING);
  static final UITestAction TEST_FROM = new UITestAction(TEST_FROM_STRING);
  static final UITestAction VERBOSE = new UITestAction(VERBOSE_STRING);
  static final UITestAction INTERFACE = new UITestAction(InterfaceSection.TYPE);
  static final UITestAction DIALOG = new UITestAction(
      DialogSectionCommand.SECTION_TYPE);

  private final String action;

  private UITestAction(String action) {
    this.action = action;
  }

  public String toString() {
    return action;
  }

  static UITestAction getInstance(String action) {
    if (action == null) {
      return null;
    }
    if (action.equals(ADOC.action)) {
      return ADOC;
    }
    if (action.equals(ASSERT.action)) {
      return ASSERT;
    }
    if (action.equals(COPY_STRING)) {
      return COPY;
    }
    if (action.equals(DATA_FILE_STRING)) {
      return DATA_FILE;
    }
    if (action.equals(DATASET_STRING)) {
      return DATASET;
    }
    if (action.equals(DATASET_DIR_STRING)) {
      return DATASET_DIR;
    }
    if (action.equals(DURATION_STRING)) {
      return DURATION;
    }
    if (action.equals(DATA_DIR_STRING)) {
      return DATA_DIR;
    }
    if (action.equals(FUNCTION_STRING)) {
      return FUNCTION;
    }
    if (action.equals(SET_STRING)) {
      return SET;
    }
    if (action.equals(SLEEP_STRING)) {
      return SLEEP;
    }
    if (action.equals(STOP_STRING)) {
      return STOP;
    }
    if (action.equals(TEST_FROM_STRING)) {
      return TEST_FROM;
    }
    if (action.equals(VERBOSE_STRING)) {
      return VERBOSE;
    }
    if (action.equals(WAIT_FOR_STRING)) {
      return WAIT_FOR;
    }
    if (action.equals(INTERFACE.action)) {
      return INTERFACE;
    }
    if (action.equals(DIALOG.action)) {
      return DIALOG;
    }
    return null;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2006/10/24 23:33:51  sueh
 * <p> bug# 948 Changed filedir to datadir.  Removed testdir.
 * <p>
 * <p> Revision 1.7  2006/10/10 05:22:34  sueh
 * <p> bug# 931  getInstance():  preventing null pointer exception.
 * <p>
 * <p> Revision 1.6  2006/08/28 18:26:50  sueh
 * <p> bug# 923 Changed the uitest source attribute to filedir.  Global filedir is an
 * <p> absolute file path.
 * <p>
 * <p> Revision 1.5  2006/06/27 22:35:16  sueh
 * <p> bug# 852 Added FUNCTION.
 * <p>
 * <p> Revision 1.4  2006/06/14 00:35:32  sueh
 * <p> bug# 852 Removed PROCESS, it is not an action.
 * <p>
 * <p> Revision 1.3  2006/05/01 21:18:17  sueh
 * <p> bug# 787 Removed fiducial diameter, replacing it with set.
 * <p>
 * <p> Revision 1.2  2006/04/28 20:57:10  sueh
 * <p> bug# 787 Added actions for uitest.adoc and uitestaxis.adoc globals.
 * <p> Removed EXIT.
 * <p>
 * <p> Revision 1.1  2006/04/25 18:58:56  sueh
 * <p> bug# 787 An enum style class which contains the actions found in
 * <p> uitestaxis.adoc files.
 * <p> </p>
 */
