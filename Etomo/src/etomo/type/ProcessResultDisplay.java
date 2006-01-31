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

  public String getName();
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

  /**
   * Call one of these functionsthis function when the first process starts.
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
   * Add followingDisplay to the followingDisplays member variable.
   * Following displays have a dependency on the process associated with the
   * instance.  They will be turned off when the instance is successful.
   * @param followingDisplay
   */
  public void addFollowingDisplay(ProcessResultDisplay followingDisplay);

  /**
   * Add failureDisplay to the failureDisplays member variable.  FailureDisplays
   * will be turned off when the instance fails.
   * @param failureDisplay
   */
  public void addFailureDisplay(ProcessResultDisplay failureDisplay);

  /**
   * Add successDisplay to the successDisplays member variable.  SuccessDisplays
   * will be turned on when the instance fails.
   * @param successDisplay
   */
  public void addSuccessDisplay(ProcessResultDisplay successDisplay);
}
/**
 * <p> $Log$
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