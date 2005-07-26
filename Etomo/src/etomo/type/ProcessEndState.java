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
  public static  final String  rcsid =  "$Id$";
  
  private static final int doneIndex = 0;
  private static final int failedIndex = 1;
  private static final int killedIndex = 2;
  private static final int pausedIndex = 3;
  
  public static final int TOTAL = pausedIndex + 1;
  
  private final String name;
  private final int index;

  private ProcessEndState(int index) {
    this.index = index;
    name = toString(index);
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
  public final static ProcessEndState precedence(
      ProcessEndState existingEndState, ProcessEndState newEndState) {
    if (existingEndState == ProcessEndState.KILLED
        || existingEndState == ProcessEndState.PAUSED) {
      return existingEndState;
    }
    if (existingEndState == ProcessEndState.FAILED
        && newEndState == ProcessEndState.DONE) {
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
      return "done";
    case failedIndex:
      return "failed";
    case killedIndex:
      return "killed";
    case pausedIndex:
      return "paused";
    }
    return "";
  }

}
/**
* <p> $Log$ </p>
*/