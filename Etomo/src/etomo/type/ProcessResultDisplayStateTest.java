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
  public static final String rcsid = "$Id$";

  Display displayNotRunning = null;
  Display displayControlNotRunning = null;
  Display displayRunning = null;
  Display displayControlRunning = null;
  Display displayFailedToStart = null;
  Display displayControlFailedToStart = null;

  /**
   * Constructor for ProcessResultDisplayTest.
   * @param name
   */
  public ProcessResultDisplayStateTest() {
    super();
  }

  public void testMsgProcessStarting() {
    // start with done == false
    Display display = new Display();
    assertFalse(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    display.msgProcessStarting();
    // temporarily sets done to true, in case user exits
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertTrue(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    // start with done == true
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
    // single process
    Display display = new Display();
    display.msgProcessStarting();
    display.msgProcessSucceeded();
    assertTrue(display.isDone());
    assertFalse(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    // multiple processes
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
    // single process
    Display display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgProcessFailed();
    assertFalse(display.isDone());
    assertTrue(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    // multiple processes
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
    // single process
    Display display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgProcessFailedToStart();
    // msgProcessFailedToStart done should set done equal to originalState
    assertEquals(display.isDone(), display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertFalse(display.isSecondaryProcess());
    // multiple processes
    display = new Display();
    display.setProcessDone(true);
    display.msgProcessStarting();
    display.msgSecondaryProcess();
    // should behave like msgProcessFailed
    display.msgProcessFailedToStart();
    assertFalse(display.isDone());
    assertTrue(display.isOriginalState());
    assertFalse(display.isProcessRunning());
    assertTrue(display.isSecondaryProcess());
  }

  /**
   * following displays should be turned off when the display is successful
   */
  public void testFollowingDisplays() {
    Display display = new Display();
    boolean expectedTestState = false;
    setupTestsAndControls(expectedTestState);
    // setup child displays
    Display displayChild = new Display();
    Display displayGrandChild = new Display();
    Display displayGreatGrandChild = new Display();
    displayChild.setProcessDone(!expectedTestState);
    displayGrandChild.setProcessDone(!expectedTestState);
    displayGreatGrandChild.setProcessDone(!expectedTestState);
    // setup child vectors
    displayGrandChild.addDependentDisplay(displayGreatGrandChild);
    displayChild.addDependentDisplay(displayGrandChild);
    displayChild.addDependentDisplay(displayGreatGrandChild);
    // add test displays to display.followingDisplays
    display.addDependentDisplay(displayNotRunning);
    display.addDependentDisplay(displayFailedToStart);
    display.addDependentDisplay(displayRunning);
    display.addDependentDisplay(displayChild);
    display.addDependentDisplay(displayGrandChild);
    display.addDependentDisplay(displayGreatGrandChild);
    // run process in display
    display.msgProcessStarting();
    // this call should change done and original state to false in the following displays
    display.msgProcessSucceeded();
    assertTestsNEControls(expectedTestState);
    // assert child vectors
    assertEquals(displayChild.isDone(), expectedTestState);
    assertEquals(displayGrandChild.isDone(), expectedTestState);
    assertEquals(displayGreatGrandChild.isDone(), expectedTestState);
  }

  /**
   * parent displays should be turned off when the display fails
   */
  public void testParentDisplays() {
    Display display = new Display();
    setupTestsAndControls(false);
    // add test displays to display.parentDisplays
    display.addFailureDisplay(displayNotRunning);
    display.addFailureDisplay(displayFailedToStart);
    display.addFailureDisplay(displayRunning);
    // run process in display
    display.msgProcessStarting();
    // this call should change done and original state to false in the parent displays
    display.msgProcessFailed();
    assertTestsNEControls(false);
  }

  /**
   * sister displays should be turned on when the display is successful
   */
  public void testSisterDisplays() {
    Display display = new Display();
    setupTestsAndControls(true);
    // add test displays to display.sisterDisplays
    display.addSuccessDisplay(displayNotRunning);
    display.addSuccessDisplay(displayFailedToStart);
    display.addSuccessDisplay(displayRunning);
    // run process in display
    display.msgProcessStarting();
    // this call should change done and original state to false in the sister displays
    display.msgProcessSucceeded();
    assertTestsNEControls(true);
  }

  private void setupTestsAndControls(boolean expectedTestState) {
    // create test displays and control displays
    displayNotRunning = new Display();
    displayControlNotRunning = new Display();
    displayRunning = new Display();
    displayControlRunning = new Display();
    displayFailedToStart = new Display();
    displayControlFailedToStart = new Display();
    // set values in test displays and control displays
    // not running
    displayNotRunning.setProcessDone(!expectedTestState);
    displayControlNotRunning.setProcessDone(!expectedTestState);
    // running
    displayRunning.setProcessDone(!expectedTestState);
    displayControlRunning.setProcessDone(!expectedTestState);
    displayRunning.msgProcessStarting();
    displayControlRunning.msgProcessStarting();
    // failed to start
    displayFailedToStart.setProcessDone(!expectedTestState);
    displayControlFailedToStart.setProcessDone(!expectedTestState);
    displayFailedToStart.msgProcessStarting();
    displayControlFailedToStart.msgProcessStarting();
    displayFailedToStart.setOriginalState(!expectedTestState);
    displayControlFailedToStart.setOriginalState(!expectedTestState);
  }

  private void assertTestsNEControls(boolean expectedTestState) {
    // test done - should be set to done in the test displays
    assertEquals(displayNotRunning.isDone(), expectedTestState);
    assertEquals(displayControlNotRunning.isDone(), !expectedTestState);
    assertEquals(displayRunning.isDone(), expectedTestState);
    assertTrue(displayControlRunning.isDone());
    assertEquals(displayFailedToStart.isDone(), expectedTestState);
    assertTrue(displayControlFailedToStart.isDone());
    // test original state - should be set to done in the test display
    displayFailedToStart.msgProcessFailedToStart();
    displayControlFailedToStart.msgProcessFailedToStart();
    assertEquals(displayFailedToStart.isDone(), expectedTestState);
    assertEquals(displayControlFailedToStart.isDone(), !expectedTestState);
  }

  private final class Display implements ProcessResultDisplay {
    private final ProcessResultDisplayState state;
    private boolean done = false;

    public void dumpState() {
    }

    private Display() {
      this.state = new ProcessResultDisplayState(this);
    }

    public boolean getOriginalState() {
      return done;
    }

    public void setProcessDone(boolean done) {
      this.done = done;
    }

    public void setScreenState(BaseScreenState screenState) {
    }

    public void msgProcessStarting() {
      state.msgProcessStarting();
    }

    public void msg(ProcessResult processResult) {
      if (processResult == ProcessResult.SUCCEEDED) {
        state.msgProcessSucceeded();
      }
      else if (processResult == ProcessResult.FAILED) {
        state.msgProcessFailed();
      }
      else if (processResult == ProcessResult.FAILED_TO_START) {
        state.msgProcessFailedToStart();
      }
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

    public void addDependentDisplay(ProcessResultDisplay dependentDisplay) {
      state.addDependentDisplay(dependentDisplay);
    }

    public void setOriginalState(boolean originalState) {
      state.setOriginalState(originalState);
    }

    public void addFailureDisplay(ProcessResultDisplay failureDisplay) {
      state.addFailureDisplay(failureDisplay);
    }

    public void addSuccessDisplay(ProcessResultDisplay successDisplay) {
      state.addSuccessDisplay(successDisplay);
    }

    public void updateDisplay() {
    }

    protected boolean isDone() {
      return done;
    }

    protected boolean isOriginalState() {
      return state.isOriginalState();
    }

    protected boolean isProcessRunning() {
      return state.isProcessRunning();
    }

    protected boolean isSecondaryProcess() {
      return state.isSecondaryProcess();
    }

    public int getDependencyIndex() {
      return state.getDependencyIndex();
    }

    public void setDependencyIndex(int dependencyIndex) {
      state.setDependencyIndex(dependencyIndex);
    }

    public String getName() {
      return "";
    }

    public String getButtonStateKey() {
      return null;
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.4  2006/07/26 16:38:59  sueh
* <p> bug# 868 Added msg(ProcessResult) to Display
* <p>
* <p> Revision 1.3  2006/02/06 21:19:13  sueh
* <p> bug# 521 ProcessResultDisplayState:  changed following display to
* <p> dependent display.  Added dependecy index and initialized.
* <p>
* <p> Revision 1.2  2006/01/31 20:48:57  sueh
* <p> bug# 521 Testing the following display
* <p>
* <p> Revision 1.1  2006/01/26 21:59:42  sueh
* <p> bug# 401 Turn ProcessResultDisplay into an interface.  Place the
* <p> functionality into ProcessResultDisplayState.  This allows a greater
* <p> variety of classes to be ProcessResultDisplay's.
* <p> </p>
*/
