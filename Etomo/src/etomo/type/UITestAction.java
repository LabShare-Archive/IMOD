package etomo.type;

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
  
  private static final String ADOC_STRING = "adoc";
  private static final String ASSERT_STRING = "assert";
  private static final String COPY_STRING = "copy";
  private static final String DATA_FILE_STRING = "datafile";
  private static final String DATASET_STRING = "dataset";
  private static final String DATASET_DIR_STRING = "datasetdir";
  private static final String DURATION_STRING = "duration";
  private static final String FILE_DIR_STRING = "filedir";
  private static final String FUNCTION_STRING = "function";
  private static final String SET_STRING = "set";
  private static final String SLEEP_STRING = "sleep";
  private static final String STOP_STRING = "stop";
  private static final String TEST_DIR_STRING = "testdir";
  private static final String TEST_FROM_STRING = "testfrom";
  private static final String VERBOSE_STRING = "verbose";
  private static final String WAIT_FOR_STRING = "waitfor";
  
  public static final UITestAction ADOC = new UITestAction(ADOC_STRING);
  public static final UITestAction ASSERT = new UITestAction(ASSERT_STRING);
  public static final UITestAction COPY = new UITestAction(COPY_STRING);
  public static final UITestAction DATA_FILE = new UITestAction(DATA_FILE_STRING);
  public static final UITestAction DATASET = new UITestAction(DATASET_STRING);
  public static final UITestAction DATASET_DIR = new UITestAction(DATASET_DIR_STRING);
  public static final UITestAction DURATION = new UITestAction(DURATION_STRING);
  public static final UITestAction FILE_DIR = new UITestAction(FILE_DIR_STRING);
  public static final UITestAction FUNCTION = new UITestAction(FUNCTION_STRING);
  public static final UITestAction SET = new UITestAction(SET_STRING);
  public static final UITestAction SLEEP = new UITestAction(SLEEP_STRING);
  public static final UITestAction STOP = new UITestAction(STOP_STRING);
  public static final UITestAction TEST_DIR = new UITestAction(TEST_DIR_STRING);
  public static final UITestAction TEST_FROM = new UITestAction(TEST_FROM_STRING);
  public static final UITestAction VERBOSE = new UITestAction(VERBOSE_STRING);
  public static final UITestAction WAIT_FOR = new UITestAction(WAIT_FOR_STRING);
  
  private final String action;
  
  private UITestAction(String action) {
    this.action = action;
  }
  
  public String toString() {
    return action;
  }
  
  public static UITestAction getInstance(String action) {
    if (action.equals(ADOC_STRING)) {
      return ADOC;
    }
    if (action.equals(ASSERT_STRING)) {
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
    if (action.equals(FILE_DIR_STRING)) {
      return FILE_DIR;
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
    if (action.equals(TEST_DIR_STRING)) {
      return TEST_DIR;
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
    return null;
  }
}
/**
* <p> $Log$
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