package etomo.process;

/**
 * <p>Description: ImodProcess opens an instance of imod with the specfied stack
 * projection stack(s) and possibly model files.  Model files can also be
 * loaded and changed after the process has started.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.14  2003/09/25 22:17:17  rickg
 * <p> Corrected a sendevent comment
 * <p>
 * <p> Revision 2.13  2003/08/25 22:18:39  rickg
 * <p> Removed errant model opening for the tomogram where a matching
 * <p> or patch region model had been previously opened
 * <p>
 * <p> Revision 2.12  2003/08/05 21:20:45  rickg
 * <p> Added movieMode
 * <p>
 * <p> Revision 2.11  2003/07/25 23:00:33  rickg
 * <p> openModel does not automatically switch 3dmod to model mode
 * <p> now
 * <p>
 * <p> Revision 2.10  2003/06/05 21:12:23  rickg
 * <p> Added model mode and raise messages
 * <p> fill cache flag is functional
 * <p>
 * <p> Revision 2.9  2003/05/27 08:44:03  rickg
 * <p> Removed TODO
 * <p>
 * <p> Revision 2.8  2003/05/15 20:19:41  rickg
 * <p> Removed extraneous debug printing
 * <p>
 * <p> Revision 2.7  2003/05/12 23:26:29  rickg
 * <p> imod -D -> 3dmod
 * <p> commad line reporting (need to check debug state)
 * <p>
 * <p> Revision 2.6  2003/05/07 22:28:30  rickg
 * <p> Implemented fillCache mechanism, but not enabled
 * <p>
 * <p> Revision 2.5  2003/04/28 23:25:26  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.4  2003/03/19 00:23:22  rickg
 * <p> Added model view option
 * <p>
 * <p> Revision 2.3  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.2  2003/01/31 05:34:08  rickg
 * <p> Support for foreground imod/qtimod through -W
 * <p>
 * <p> Revision 2.1  2003/01/29 21:09:05  rickg
 * <p> Added sleep to wait for imod process to exit and then
 * <p> some when.  For some reason the windowID/processID
 * <p> strings were not available
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6  2002/10/16 17:36:24  rickg
 * <p> reformat
 * <p>
 * <p> Revision 1.5  2002/09/20 17:06:38  rickg
 * <p> Added typed exceptions
 * <p> Added a quit method
 * <p> Check for ProcessID before running PS in isRunning
 * <p>
 * <p> Revision 1.4  2002/09/19 22:47:45  rickg
 * <p> More robust method to extract process and window ID from imod
 * <p>
 * <p> Revision 1.3  2002/09/18 23:39:26  rickg
 * <p> Moved opening to a separate method
 * <p> Opening checks to see if the imod process already exists
 * <p>
 * <p> Revision 1.2  2002/09/17 23:20:31  rickg
 * <p> Complete basic operation
 * <p>
 * <p> Revision 1.1  2002/09/13 21:28:44  rickg
 * <p> initial entry
 * <p>
 * <p> </p>
 */
public class ImodProcess {
  public static final String rcsid =
    "$Id$";

  public static final String MESSAGE_OPEN_MODEL = "1";
  public static final String MESSAGE_SAVE_MODEL = "2";
  public static final String MESSAGE_VIEW_MODEL = "3";
  public static final String MESSAGE_CLOSE = "4";
  public static final String MESSAGE_RAISE = "5";
  public static final String MESSAGE_MODEL_MODE = "6";

  private String datasetName = "";
  private String modelName = "";
  private String windowID = "";
  private boolean swapYZ = false;
  private boolean modelView = false;
  private boolean fillCache = false;

  private Thread imodThread;

  /**
   * @param datasetName
   */
  public void setDatasetName(String datasetName) {
    this.datasetName = datasetName;
  }

  /**
   * @param modelName
   */
  public void setModelName(String modelName) {
    this.modelName = modelName;
  }

  /**
   * Dataset only constructor
   * @param A string specifying the path to the projection stack file
   */
  public ImodProcess(String dataset) {
    datasetName = dataset;
  }

  /**
   * Dataset and model file constructor
   * @param dataset A string specifying the path to the projection stack file
   * @param model A string specifying the path to the IMOD model file
   */
  public ImodProcess(String dataset, String model) {
    datasetName = dataset;
    modelName = model;
  }

  /**
   * Open the 3dmod process if is not already open.
   */
  public void open() throws SystemProcessException {
    if (isRunning()) {
      raise3dmod();
      return;
    }

    //  Reset the window string
    windowID = "";

    //  Collect the command line options
    String options = "";
    if (swapYZ) {
      options = "-Y ";
    }

    if (modelView) {
      options = "-V ";
    }

    // Fill cache implementation
    if (fillCache) {
      options = "-F ";
    }

    String command = "3dmod -W " + options + datasetName + " " + modelName;
    InteractiveSystemProgram imod = new InteractiveSystemProgram(command);

    //  Start the 3dmod program thread and wait for it to finish
    imodThread = new Thread(imod);
    imodThread.start();

    //  Check the stderr of the 3dmod process for the windowID and the
    String line;
    while (imodThread.isAlive() && windowID.equals("")) {

      while ((line = imod.readStderr()) != null) {
        if (line.indexOf("Window id = ") != -1) {
          String[] words = line.split("\\s+");
          if (words.length < 4) {
            throw (
              new SystemProcessException("Could not parse window ID from imod\n"));
          }
          windowID = words[3];
        }
      }

      //  Wait a litte while for 3dmod to generate some stderr output
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }

    //  If imod exited before getting the window report the problem to the user
    if (windowID.equals("")) {
      String message =
        "3dmod returned: " + String.valueOf(imod.getExitValue()) + "\n";

      while ((line = imod.readStderr()) != null) {
        System.err.println(line);
        message = message + "stderr: " + line + "\n";
      }

      while ((line = imod.readStdout()) != null) {
        message = message + "stdout: " + line + "\n";
        line = imod.readStdout();
      }

      throw (new SystemProcessException(message));
    }
  }

  /**
   * Send the quit messsage to imod
   */
  public void quit() throws SystemProcessException {
    if (isRunning()) {
      String[] messages = new String[1];
      messages[0] = "4";
      imodSendEvent(messages);
    }
  }

  /**
   * Check to see if this 3dmod process is running
   */
  public boolean isRunning() {
    if (imodThread == null) {
      return false;
    }
    return imodThread.isAlive();
  }

  /**
   * Open a new model file
   */
  public void openModel(String newModelName) throws SystemProcessException {
    modelName = newModelName;
    String[] args = new String[2];
    args[0] = MESSAGE_OPEN_MODEL;
    args[1] = newModelName;
    imodSendEvent(args);
  }

  /**
   * Save the current model file
   */
  public void saveModel() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_SAVE_MODEL;
    imodSendEvent(args);
  }

  /**
   * View the current model file
   */
  public void viewModel() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_VIEW_MODEL;
    imodSendEvent(args);
  }

  /**
   * Switch the 3dmod process to model mode
   * @throws SystemProcessException
   */
  public void modelMode() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_MODEL_MODE;
    imodSendEvent(args);
  }

  /**
   * Switch the 3dmod process to movie mode
   * @throws SystemProcessException
   */
  public void movieMode() throws SystemProcessException {
    String[] args = new String[2];
    args[0] = MESSAGE_MODEL_MODE;
    args[1] = "0";
    imodSendEvent(args);
  }
  
  /**
   * Raise the 3dmod window
   * @throws SystemProcessException
   */
  public void raise3dmod() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_RAISE;
    imodSendEvent(args);
  }

  /**
   * Send an event to 3dmod using the imodsendevent command
   */
  private void imodSendEvent(String[] args) throws SystemProcessException {
    if (windowID == "") {
      throw (new SystemProcessException("No window ID available for imod"));
    }

    String command = "imodsendevent " + windowID + " ";
    for (int i = 0; i < args.length; i++) {
      command = command + args[i] + " ";
    }
    InteractiveSystemProgram imodSendEvent =
      new InteractiveSystemProgram(command);

    //  Start the imodSendEvent program thread and wait for it to finish
    Thread sendEventThread = new Thread(imodSendEvent);
    sendEventThread.start();
    try {
      sendEventThread.join();
    }
    catch (Exception except) {
      except.printStackTrace();
    }

    // Check imodSendEvent's exit code, if it is not zero read in the
    // stderr/stdout stream and throw an exception describing why the file 
    // was not loaded
    if (imodSendEvent.getExitValue() != 0) {

      String message =
        "imodsendevent returned: "
          + String.valueOf(imodSendEvent.getExitValue())
          + "\n";

      String line = imodSendEvent.readStderr();
      while (line != null) {
        message = message + "stderr: " + line + "\n";
        line = imodSendEvent.readStderr();
      }

      line = imodSendEvent.readStdout();
      while (line != null) {
        message = message + "stdout: " + line + "\n";
        line = imodSendEvent.readStdout();
      }

      throw (new SystemProcessException(message));
    }
  }

  /**
   * Returns the datasetName.
   * @return String
   */
  public String getDatasetName() {
    return datasetName;
  }

  /**
   * Returns the modelName.
   * @return String
   */
  public String getModelName() {
    return modelName;
  }

  /**
   * Returns the windowID.
   * @return String
   */
  public String getWindowID() {
    return windowID;
  }

  /**
   * Returns the swapYZ.
   * @return String
   */
  public boolean getSwapYZ() {
    return swapYZ;
  }

  /**
   * Returns the windowID.
   * @return String
   */
  public void setSwapYZ(boolean state) {
    swapYZ = state;
  }

  /**
   * @return boolean
   */
  public boolean isModelView() {
    return modelView;
  }

  /**
   * Sets the modelView.
   * @param modelView The modelView to set
   */
  public void setModelView(boolean modelView) {
    this.modelView = modelView;
  }

  /**
   * @return
   */
  public boolean isFillCache() {
    return fillCache;
  }

  /**
   * @param b
   */
  public void setFillCache(boolean b) {
    fillCache = b;
  }

}
