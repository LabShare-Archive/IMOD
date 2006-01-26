package etomo.type;
/**
* <p>Description: A parent class for a class which can display the result of a
* process.  The display is assumed to binary:  done or not done.  This class can
* handle a single process or a sequence of processes.</p>
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
public interface ProcessResultDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public boolean getOriginalState();
  public void setProcessDone(boolean done);
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
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/01/20 21:02:00  sueh
* <p> bug# 401 A class which manipulates the state of its child based on the
* <p> result of a process.
* <p> </p>
*/