package etomo.type;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ProcessEndState {
  public static final String rcsid = "$Id$";

  private static final int doneIndex = 0;
  private static final int failedIndex = 1;
  private static final int killedIndex = 2;
  private static final int pausedIndex = 3;

  private static final String DONE_NAME = "done";
  private static final String FAILED_NAME = "failed";
  private static final String KILLED_NAME = "killed";
  private static final String PAUSED_NAME = "paused";

  public static final int TOTAL = pausedIndex + 1;

  private final String name;
  private final int index;

  public void dumpState() {
    System.err.print("[name:" + name + ",index:" + index + "]");
  }

  private ProcessEndState(int index) {
    this.index = index;
    name = toString(index);
  }

  public static boolean isValid(final String string) {
    return DONE.equals(string) || FAILED.equals(string) || KILLED.equals(string)
        || PAUSED.equals(string);
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  public int toIndex() {
    return index;
  }

  /**
   * set end state
   * Kill and pause may be interpreted as a failure or completion in another
   * class, so don't override a kill or pause setting.
   * A failure may be interpreted as a completion in another
   * class, so don't override a failure setting with a done setting.
   * @param endState
   */
  public final static ProcessEndState precedence(ProcessEndState existingEndState,
      ProcessEndState newEndState) {
    if (existingEndState == ProcessEndState.KILLED
        || existingEndState == ProcessEndState.PAUSED) {
      return existingEndState;
    }
    if (existingEndState == ProcessEndState.FAILED && newEndState == ProcessEndState.DONE) {
      return existingEndState;
    }
    return newEndState;
  }

  public static final ProcessEndState DONE = new ProcessEndState(doneIndex);
  public static final ProcessEndState FAILED = new ProcessEndState(failedIndex);
  public static final ProcessEndState KILLED = new ProcessEndState(killedIndex);
  public static final ProcessEndState PAUSED = new ProcessEndState(pausedIndex);

  private String toString(int index) {
    switch (index) {
    case doneIndex:
      return DONE_NAME;
    case failedIndex:
      return FAILED_NAME;
    case killedIndex:
      return KILLED_NAME;
    case pausedIndex:
      return PAUSED_NAME;
    }
    return "";
  }

  public boolean equals(final String string) {
    return name.equals(string);
  }

  public static ProcessEndState getInstance(String name) {
    if (name.equals(DONE_NAME)) {
      return DONE;
    }
    if (name.equals(FAILED_NAME)) {
      return FAILED;
    }
    if (name.equals(KILLED_NAME)) {
      return KILLED;
    }
    if (name.equals(PAUSED_NAME)) {
      return PAUSED;
    }
    return null;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/04/25 18:57:14  sueh
* <p> bug# 787 Added getInstance(String).
* <p>
* <p> Revision 1.1  2005/07/26 23:01:37  sueh
* <p> bug# 701 Enum of process end states.
* <p> </p>
*/
