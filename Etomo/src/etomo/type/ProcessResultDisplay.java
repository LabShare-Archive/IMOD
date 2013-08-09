package etomo.type;

/**
 * <p>Description: A parent class for a class which can display the result of a
 * process.  The display is assumed to binary:  done or not done.  This class can
 * handle a single process or a sequence of processes.</p>
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
public interface ProcessResultDisplay {
  public static final String rcsid = "$Id$";

  public void dumpState();

  /**
   * get the original state of the display.  Will return to this state if the
   * process fails to start.
   * @return
   */
  public boolean getOriginalState();

  /**
   * Override the retrieved original state.
   * @param originalState
   */
  public void setOriginalState(boolean originalState);

  /**
   * set the state of the button
   * @param done
   */
  public void setProcessDone(boolean done);

  public void setScreenState(BaseScreenState screenState);

  public void msg(ProcessResult displayState);

  /**
   * Call one of these functions this function when the first process starts.
   *
   */
  public void msgProcessStarting();

  /**
   * Call this function if the final process succeeds.
   *
   */
  public void msgProcessSucceeded();

  /**
   * Call this function if the final process fails.
   *
   */
  public void msgProcessFailed();

  /**
   * Call this function if any process fails to start.
   * For a secondary process (any process after the first process) this is
   * equivalent to calling msgProcessFailed.
   */
  public void msgProcessFailedToStart();

  /**
   * Call this function when a secondary process (any process after the first
   * process) starts.
   *
   */
  public void msgSecondaryProcess();

  /**
   * Add dependentDisplay to the dependentDisplayList member variable.
   * Dependent displays have a dependency on the process associated with the
   * instance.  They will be turned off when the instance's process either
   * succeeds or fails.
   * @param dependentDisplay
   */
  public void addDependentDisplay(ProcessResultDisplay dependentDisplay);

  /**
   * Add failureDisplay to the failureDisplayList member variable.  Displays in
   * FailureDisplayList will be turned off when the instance fails.
   * @param failureDisplay
   */
  public void addFailureDisplay(ProcessResultDisplay failureDisplay);

  /**
   * Add successDisplay to the successDisplayList member variable.  Displays in
   * SuccessDisplayList will be turned on when the instance succeeds.
   * @param successDisplay
   */
  public void addSuccessDisplay(ProcessResultDisplay successDisplay);

  /**
   * sets the display's location in the dependency list
   * @param dependencyIndex
   */
  public void setDependencyIndex(int dependencyIndex);

  /**
   * gets the display's location in the dependency list
   * @return dependencyIndex
   */
  public int getDependencyIndex();

  public String getButtonStateKey();
}
/**
 * <p> $Log$
 * <p> Revision 1.6  2007/09/10 20:33:10  sueh
 * <p> bug# 925 Removed lazy initialization for ProcessResultDisplay so initialized is
 * <p> no longer needed.
 * <p>
 * <p> Revision 1.5  2006/07/26 16:36:45  sueh
 * <p> bug# 868 Added msg(ProcessResult)
 * <p>
 * <p> Revision 1.4  2006/02/06 21:16:17  sueh
 * <p> bug# 521 Changed following display to dependent display.  Added
 * <p> dependecy index and initialized.
 * <p>
 * <p> Revision 1.3  2006/01/31 20:50:37  sueh
 * <p> bug# 521 Added failureDisplayList, successDisplayList, and
 * <p> followingDisplayList to change the state of other displays when the
 * <p> process associated with the current instance succeeds or fails.
 * <p> Added BaseScreenState so that buttons on dialogs not current
 * <p> displayed could change their state.  Without the ability to change the
 * <p> screen state setting, they're old state would be reloaded when the
 * <p> dialog was redisplayed.
 * <p>
 * <p> Revision 1.2  2006/01/26 21:58:58  sueh
 * <p> bug# 401 Turn this class into an interface.  Place the functionality into
 * <p> ProcessResultDisplayState.  This allows a greater variety of classes to be
 * <p> ProcessResultDisplay's.
 * <p>
 * <p> Revision 1.1  2006/01/20 21:02:00  sueh
 * <p> bug# 401 A class which manipulates the state of its child based on the
 * <p> result of a process.
 * <p> </p>
 */
