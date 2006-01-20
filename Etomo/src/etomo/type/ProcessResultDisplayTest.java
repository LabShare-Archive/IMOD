package etomo.type;

import junit.framework.TestCase;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ProcessResultDisplayTest extends TestCase {
  public static  final String  rcsid =  "$Id$";
  
  private ProcessResultDisplayChild test = null;

  /**
   * Constructor for ProcessResultDisplayTest.
   * @param name
   */
  public ProcessResultDisplayTest() {
    super();
    test = new ProcessResultDisplayChild();
  }
  
  public void testMsgProcessStarting() {
    //start with done == false
    test = new ProcessResultDisplayChild();
    assertFalse(test.isDone());
    assertFalse(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    test.msgProcessStarting();
    //temporarily sets done to true, in case user exits
    assertTrue(test.isDone());
    assertFalse(test.isOriginalState());
    assertTrue(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    //start with done == true
    test = new ProcessResultDisplayChild();
    test.setProcessDone(true);
    assertTrue(test.isDone());
    assertFalse(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    test.msgProcessStarting();
    assertTrue(test.isDone());
    assertTrue(test.isOriginalState());
    assertTrue(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
  }
  
  public void testMsgProcessSucceeded() {
    //single process
    test = new ProcessResultDisplayChild();
    test.msgProcessStarting();
    test.msgProcessSucceeded();
    assertTrue(test.isDone());
    assertFalse(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    //multiple processes
    test = new ProcessResultDisplayChild();
    test.msgProcessStarting();
    test.msgSecondaryProcess();
    assertTrue(test.isSecondaryProcess());
    test.msgProcessSucceeded();
    assertTrue(test.isDone());
    assertFalse(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertTrue(test.isSecondaryProcess());
  }
  
  public void testMsgProcessFailed() {
    //single process
    test = new ProcessResultDisplayChild();
    test.setProcessDone(true);
    test.msgProcessStarting();
    test.msgProcessFailed();
    assertFalse(test.isDone());
    assertTrue(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    //multiple processes
    test = new ProcessResultDisplayChild();
    test.setProcessDone(true);
    test.msgProcessStarting();
    test.msgSecondaryProcess();
    test.msgProcessFailed();
    assertFalse(test.isDone());
    assertTrue(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertTrue(test.isSecondaryProcess());
  }
  
  public void testMsgProcessFailedToStart() {
    //single process
    test = new ProcessResultDisplayChild();
    test.setProcessDone(true);
    test.msgProcessStarting();
    test.msgProcessFailedToStart();
    //msgProcessFailedToStart should set done equal to originalState
    assertEquals(test.isDone(), test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertFalse(test.isSecondaryProcess());
    //multiple processes
    test = new ProcessResultDisplayChild();
    test.setProcessDone(true);
    test.msgProcessStarting();
    test.msgSecondaryProcess();
    //should behave like msgProcessFailed
    test.msgProcessFailedToStart();
    assertFalse(test.isDone());
    assertTrue(test.isOriginalState());
    assertFalse(test.isProcessRunning());
    assertTrue(test.isSecondaryProcess());
  }
  
  private final class ProcessResultDisplayChild extends ProcessResultDisplay {
    private boolean done = false;
    
    protected void setProcessDone(boolean done) {
      this.done = done;
    }
    
    protected boolean getOriginalState() {
      return done;
    }
    
    protected boolean isDone() {
      return done;
    }
  }
}
/**
* <p> $Log$ </p>
*/
