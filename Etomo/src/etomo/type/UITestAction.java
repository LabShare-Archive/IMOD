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
  private static final String EXIT_STRING = "exit";
  private static final String PROCESS_STRING = "process";
  private static final String SLEEP_STRING = "sleep";
  private static final String WAIT_FOR_STRING = "waitfor";

  public static final UITestAction ADOC = new UITestAction(ADOC_STRING);
  public static final UITestAction ASSERT = new UITestAction(ASSERT_STRING);
  public static final UITestAction COPY = new UITestAction(COPY_STRING);
  public static final UITestAction EXIT = new UITestAction(EXIT_STRING);
  public static final UITestAction SLEEP = new UITestAction(SLEEP_STRING);
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
    if (action.equals(EXIT_STRING)) {
      return EXIT;
    }
    if (action.equals(SLEEP_STRING)) {
      return SLEEP;
    }
    if (action.equals(WAIT_FOR_STRING)) {
      return WAIT_FOR;
    }
    return null;
  }
}
/**
* <p> $Log$ </p>
*/