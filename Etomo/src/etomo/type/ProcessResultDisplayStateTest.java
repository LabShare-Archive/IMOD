package etomo.type;

import junit.framework.TestCase;
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
public final class ProcessResultDisplayStateTest extends TestCase {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * Constructor for ProcessResultDisplayTest.
   * @param name
   */
  public ProcessResultDisplayStateTest() {
    super();
  }
  
  public void testMsgProcessStarting() {
    //start with done == false
    Display display = new Display();
    assertFalse(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    display.msgProcessStarting();
    //temporarily sets done to true, in case user exits
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertTrue(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    //start with done == true
    display = new Display();
    display.setProcessDone(true);
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    display.msgProcessStarting();
    assertTrue(display.isDone());
    assertTrue(display.isOriginalState());
    assertTrue(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
  }
  
  public void testMsgProcessSucceeded() {
    //single process
    Display display = new Display();
    display.msgProcessStarting();
    display.msgProcessSucceeded();
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    //multiple processes
    display = new Display();
    display.msgProcessStarting();
    display.msgSecondaryProcess();
    assertTrue(display.isSecondaryProcess());
    display.msgProcessSucceeded();
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertTrue(display.isSecondaryProcess());
  }
  
  public void testMsgProcessFailed() {
    //single process
    Display display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgProcessFailed();
    assertFalse(display.isDone());
    assertTrue(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    //multiple processes
    display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgSecondaryProcess();
    display.msgProcessFailed();
    assertFalse(display.isDone());
    assertTrue(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertTrue(display.isSecondaryProcess());
  }
  
  public void testMsgProcessFailedToStart() {
    //single process
    Display display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgProcessFailedToStart();
    //msgProcessFailedToStart should set done equal to originalState
    assertEquals(display.isDone(), display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    //multiple processes
    display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgSecondaryProcess();
    //should behave like msgProcessFailed
    display.msgProcessFailedToStart();
    assertFalse(display.isDone());
    assertTrue(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertTrue(display.isSecondaryProcess());
  }
  
  private final class Display implements ProcessResultDisplay {
    private final ProcessResultDisplayState state;
    private boolean done = false;
    
    private Display() {
      this.state = new ProcessResultDisplayState(this);
    }
    
    public boolean getOriginalState() {
      return done;
    }
    
    public void setProcessDone(boolean done) {
      this.done = done;
    }
    
    public void msgProcessStarting() {
      state.msgProcessStarting();
    }
    
    public void msgProcessSucceeded() {
      state.msgProcessSucceeded();
    }
   
    public void msgProcessFailed() {
      state.msgProcessFailed();
    }
    
    public void msgProcessFailedToStart() {
      state.msgProcessFailedToStart();
    }

    public void msgSecondaryProcess() {
      state.msgSecondaryProcess();
    }
    
    boolean isDone() {
      return done;
    }
    
    protected final boolean isOriginalState() {
      return state.isOriginalState();
    }
    
    protected final boolean isProcessRunning() {
      return state.isProcessRunning();
    }
    
    protected final boolean isSecondaryProcess() {
      return state.isSecondaryProcess();
    }
  }
}
/**
* <p> $Log$ </p>
*/