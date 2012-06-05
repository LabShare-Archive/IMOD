package etomo.type;

import etomo.ui.SimpleProcessResultDisplay;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class ProcessResultDisplayProxy implements ProcessResultDisplay {
  public static final String rcsid = "$Id:$";

  private final ProcessResultDisplayState state = new ProcessResultDisplayState(this);

  private final SimpleProcessResultDisplay display;

  public ProcessResultDisplayProxy(final SimpleProcessResultDisplay display) {
    this.display = display;
  }

  public void setProcessDone(boolean done) {
    display.setProcessDone(done);
  }

  public void dumpState() {
  }

  public boolean getOriginalState() {
    return false;
  }

  public void setScreenState(BaseScreenState screenState) {
  }

  public void msgProcessStarting() {
    state.msgProcessStarting();
  }

  public void msg(ProcessResult processResult) {
    state.msg(processResult);
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

  public void setDependencyIndex(int dependencyIndex) {
    state.setDependencyIndex(dependencyIndex);
  }

  public int getDependencyIndex() {
    return state.getDependencyIndex();
  }
}
