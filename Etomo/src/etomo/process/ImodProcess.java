
package etomo.process;

import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

import etomo.ApplicationManager;
import etomo.EtomoDirector;

/**
 * <p> Description: ImodProcess opens an instance of imod with the specfied stack
 * projection stack(s) and possibly model files. Model files can also be loaded
 * and changed after the process has started. </p>
 * 
 * <p> Copyright: Copyright (c) 2002 </p>
 * 
 * <p> Organization: Boulder Laboratory for 3D Fine Structure, 
 * University of Colorado </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.19  2004/11/24 18:10:36  sueh
 * <p> bug# 520 Added binning in XY.
 * <p>
 * <p> Revision 3.18  2004/11/19 23:21:39  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.17.4.3  2004/10/08 15:57:43  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.17.4.2  2004/09/22 22:07:25  sueh
 * <p> bug# 520 Added get slicer angles functionality.
 * <p>
 * <p> Revision 3.17.4.1  2004/09/03 21:11:24  sueh
 * <p> bug# 520 calling from EtomoDirector.isDebug
 * <p>
 * <p> Revision 3.17  2004/06/22 22:54:50  sueh
 * <p> bug# 462 Removed fillCache.  bug# 455 added openWithModel
 * <p> functionality to handle opening a model while preserving contrast.
 * <p> Added open contours functions
 * <p>
 * <p> Revision 3.16  2004/06/17 01:29:52  sueh
 * <p> added 3dmod command to err log because it is useful to see
 * <p>
 * <p> Revision 3.15  2004/06/10 18:23:11  sueh
 * <p> bug# 463 add setOpenBeadFixerMessage() to add the open
 * <p> bead fixer message to the message list
 * <p>
 * <p> Revision 3.14  2004/06/07 18:42:06  sueh
 * <p> bug# 457 added functions to add messages to list.
 * <p> Added a function to send the messages to 3dmod using
 * <p> imodSendEvent.
 * <p>
 * <p> Revision 3.13  2004/06/07 16:58:44  rickg
 * <p> Bug #452 added debug output for imodsendevent since we have
 * <p> been having diffuculty with it.
 * <p>
 * <p> Revision 3.12  2004/05/13 20:11:21  sueh
 * <p> bug# 33 allowing imodSendAndReceive() to receive any type
 * <p> of result data
 * <p>
 * <p> Revision 3.11  2004/05/07 19:43:57  sueh
 * <p> bug# 33 adding getRubberbandCoordinates(),
 * <p> imodSendAndReceive(), parseError().
 * <p> Keeping InteractiveSystemProgram imod around for send
 * <p> and receive.
 * <p>
 * <p> Revision 3.10  2004/05/06 20:21:47  sueh
 * <p> bug# 33 added getRubberbandCoordinates(), passing back the
 * <p> InteractiveSystemProgram from imodSendEvent()
 * <p>
 * <p> Revision 3.9  2004/05/03 22:22:25  sueh
 * <p> bug# 416 added binning (-B)
 * <p>
 * <p> Revision 3.8  2004/04/30 21:11:52  sueh
 * <p> bug# 428 add open ZaP window message
 * <p>
 * <p> Revision 3.7  2004/04/27 22:02:58  sueh
 * <p> bug# 320 removing test
 * <p>
 * <p> Revision 3.6  2004/04/26 17:05:05  sueh
 * <p> bug# 320 Commented out code - no functional change.
 * <p> Experimenting with a fix for this bug.
 * <p>
 * <p> Revision 3.5  2004/04/22 23:26:11  rickg
 * <p> Switched getIMODBinPath method
 * <p>
 * <p> Revision 3.4  2004/02/07 03:04:59  sueh
 * <p> bug# 169 Added setWorkingDirectory().
 * <p>
 * <p> Revision 3.3  2003/11/21 23:54:49  sueh
 * <p> bug242 Added toString() function
 * <p>
 * <p> Revision 3.2  2003/11/12 17:14:36  sueh
 * <p> removing debug prints
 * <p>
 * <p> Revision 3.1  2003/11/11 00:23:59  sueh
 * <p> Bug349 add useModv "-view" default false, add
 * <p> outputWindowID  "-W" default true, open(): -W is a default
 * <p> option rather then a constant, multiple options allowed
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.18  2003/11/05 20:28:42  rickg
 * <p> Bug #292 Added openPreserveContrast and openBeadfixer methods
 * <p>
 * <p> Revision 2.17  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.16  2003/11/04 17:45:21  rickg
 * <p> Bug #345 Explicitly set path to 3dmodusing IMOD_DIR
 * <p>
 * <p> Revision 2.15 2003/11/04 01:03:37 rickg
 * <p> Javadoc comment fix
 * <p>
 * <p> Revision 2.14 2003/09/25 22:17:17 rickg
 * <p> Corrected a sendevent comment
 * <p>
 * <p> Revision 2.13 2003/08/25 22:18:39 rickg
 * <p> Removed errant model opening for the tomogram where a matching
 * <p> or patch region model had been previously opened
 * <p>
 * <p> Revision 2.12 2003/08/05 21:20:45 rickg
 * <p> Added movieMode
 * <p>
 * <p> Revision 2.11 2003/07/25 23:00:33 rickg
 * <p> openModel does not automatically switch 3dmod to model mode
 * <p> now
 * <p>
 * <p> Revision 2.10 2003/06/05 21:12:23 rickg
 * <p> Added model mode and raise messages
 * <p> fill cache flag is functional
 * <p>
 * <p> Revision 2.9 2003/05/27 08:44:03 rickg
 * <p> Removed TODO
 * <p>
 * <p> Revision 2.8 2003/05/15 20:19:41 rickg
 * <p> Removed extraneous debug printing
 * <p>
 * <p> Revision 2.7 2003/05/12 23:26:29 rickg
 * <p> imod -D -> 3dmod
 * <p> commad line reporting (need to check debug state)
 * <p>
 * <p> Revision 2.6 2003/05/07 22:28:30 rickg
 * <p> Implemented fillCache mechanism, but not enabled
 * <p>
 * <p> Revision 2.5 2003/04/28 23:25:26 rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.4 2003/03/19 00:23:22 rickg
 * <p> Added model view option
 * <p>
 * <p> Revision 2.3 2003/03/02 23:30:41 rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.2 2003/01/31 05:34:08 rickg
 * <p> Support for foreground imod/qtimod through -W
 * <p>
 * <p> Revision 2.1 2003/01/29 21:09:05 rickg
 * <p> Added sleep to wait for imod process to exit and then
 * <p> some when. For some reason the windowID/processID
 * <p> strings were not available
 * <p>
 * <p> Revision 2.0 2003/01/24 20:30:31 rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6 2002/10/16 17:36:24 rickg
 * <p> reformat
 * <p>
 * <p> Revision 1.5 2002/09/20 17:06:38 rickg
 * <p> Added typed exceptions
 * <p> Added a quit method
 * <p> Check for ProcessID before running PS in isRunning
 * <p>
 * <p> Revision 1.4 2002/09/19 22:47:45 rickg
 * <p> More robust method to extract process and window ID from imod
 * <p>
 * <p> Revision 1.3 2002/09/18 23:39:26 rickg
 * <p> Moved opening to a separate method
 * <p> Opening checks to see if the imod process already exists
 * <p>
 * <p> Revision 1.2 2002/09/17 23:20:31 rickg
 * <p> Complete basic operation
 * <p>
 * <p> Revision 1.1 2002/09/13 21:28:44 rickg
 * <p> initial entry
 * <p>
 * </p>
 */
public class ImodProcess {
  public static final String rcsid = "$Id$";

  public static final String MESSAGE_OPEN_MODEL = "1";
  public static final String MESSAGE_SAVE_MODEL = "2";
  public static final String MESSAGE_VIEW_MODEL = "3";
  public static final String MESSAGE_CLOSE = "4";
  public static final String MESSAGE_RAISE = "5";
  public static final String MESSAGE_MODEL_MODE = "6";
  public static final String MESSAGE_OPEN_KEEP_BW = "7";
  public static final String MESSAGE_OPEN_BEADFIXER = "8";
  public static final String MESSAGE_ONE_ZAP_OPEN = "9";
  public static final String MESSAGE_RUBBERBAND = "10";
  public static final String MESSAGE_OBJ_PROPERTIES = "11";
  public static final String MESSAGE_NEWOBJ_PROPERTIES = "12";
  public static final String MESSAGE_SLICER_ANGLES = "13";
  
  public static final String ERROR_STRING = "ERROR:";
  public static final String WARNING_STRING = "WARNING:";
  public static final String IMOD_SEND_EVENT_STRING = "imodsendevent returned:";
  public static final String RUBBERBAND_RESULTS_STRING = "Rubberband:";
  public static final String SLICER_ANGLES_RESULTS_STRING1 = "Slicer";
  public static final String SLICER_ANGLES_RESULTS_STRING2 = "angles:";
  
  public static final String TRUE = "1";
  public static final String FALSE = "0";
  public static final int CIRCLE = 1;

  private static final int defaultBinning = 1;

  private String datasetName = "";
  private String modelName = "";
  private String windowID = "";
  private boolean swapYZ = false;
  private boolean modelView = false;
  private boolean useModv = false;
  private boolean outputWindowID = true;
  private boolean openWithModel = true;
  private File workingDirectory = null;
  private int binning = defaultBinning;
  private int binningXY = defaultBinning;
  InteractiveSystemProgram imod = null;
  private Vector sendArguments = new Vector();

  private Thread imodThread;

  /**
   * Constructor for using imodv
   * 
   */
  public ImodProcess() {
  }

  /**
   * Dataset only constructor
   * 
   * @param A string specifying the path to the projection stack file
   */
  public ImodProcess(String dataset) {
    datasetName = dataset;
  }

  /**
   * Dataset and model file constructor
   * 
   * @param dataset A string specifying the path to the projection stack file
   * @param model A string specifying the path to the IMOD model file
   */
  public ImodProcess(String dataset, String model) {
    datasetName = dataset;
    modelName = model;
  }

  /**
   * Change the dataset name
   * 
   * @param datasetName
   */
  public void setDatasetName(String datasetName) {
    this.datasetName = datasetName;
  }

  /**
   * Specify or change the model name
   * 
   * @param modelName
   */
  public void setModelName(String modelName) {
    this.modelName = modelName;
  }

  public void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }
  
  /**
   * When openWithModel is true 3dmod will open with a model, if a model is set.
   * The default for openWithModel is true.
   * Some open model options cannot be sent to 3dmod during open.  Turn off this
   * option to prevent opening the model during open.
   * Example: MESSAGE_OPEN_KEEP_BW
   * @param openWithoutModel
   */
  public void setOpenWithModel(boolean openWithModel) {
    this.openWithModel = openWithModel; 
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
    ArrayList commandOptions = new ArrayList();
    commandOptions.add(ApplicationManager.getIMODBinPath() + "3dmod");
    //  Collect the command line options

    if (outputWindowID) {
      commandOptions.add("-W");
    }

    if (swapYZ) {
      commandOptions.add("-Y");
    }

    if (modelView) {
      commandOptions.add("-V");
    }

    if (useModv) {
      commandOptions.add("-view");
    }
    
    if (binning > defaultBinning) {
      commandOptions.add("-B");
      commandOptions.add(Integer.toString(binning));
    }
    
    if (binningXY > defaultBinning) {
      commandOptions.add("-b");
      commandOptions.add(Integer.toString(binningXY));
    }
    
    commandOptions.add(datasetName);

    if (openWithModel) {
      commandOptions.add(modelName);
    }
    String[] commandArray = new String[commandOptions.size()];
    for (int i = 0; i < commandOptions.size(); i++) {
      commandArray[i] = (String) commandOptions.get(i);
      if (EtomoDirector.getInstance().isDebug()) {
        System.err.println(commandArray[i]);
      }
    }
    imod = new InteractiveSystemProgram(commandArray);
    if (workingDirectory != null) {
      imod.setWorkingDirectory(workingDirectory);
    }

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
            throw (new SystemProcessException(
              "Could not parse window ID from imod\n"));
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
    if (windowID.equals("") && outputWindowID) {
      String message = "3dmod returned: " + String.valueOf(imod.getExitValue())
          + "\n";

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
      messages[0] = MESSAGE_CLOSE;
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
   * Places arguments to open a model on the argument list.
   * @param newModelName
   */
  public void setOpenModelMessage(String newModelName) {
    modelName = newModelName;
    sendArguments.add(MESSAGE_OPEN_MODEL);
    sendArguments.add(newModelName);
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
   * Places arguments to open a model and preserve contrast on the argument
   * list.
   * @param newModelName
   */
  public void setOpenModelPreserveContrastMessage(String newModelName) {
    sendArguments.add(MESSAGE_OPEN_KEEP_BW);
    sendArguments.add(newModelName);
  }

  /**
   * Open a new model file, Preserve the constrast settings
   * 
   * @throws SystemProcessException
   */
  public void openModelPreserveContrast(String newModelName)
      throws SystemProcessException {
    String[] args = new String[2];
    args[0] = MESSAGE_OPEN_KEEP_BW;
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
   * Adds a message which sets new contours to be open
   * Message description:
   * 12 0 1 1 7 0
   * 12 says to do it to a new (empty) contour only (11 would be unconditional)
   * 0 is for object 1
   * 1 sets it to open
   * 1 sets it to display circles
   * 7 makes circle size be 7
   * 0 keeps 3D size at 0
   */
  public void setNewContoursMessage(boolean open) {
    setNewObjectMessage(0, open, CIRCLE, 7, 0);
  }
  
  /**
   * 
   * @param object
   * @param open
   * @param symbol
   * @param size
   * @param size3D
   */
  public void setNewObjectMessage(
    int object,
    boolean open,
    int symbol,
    int size,
    int size3D) {
    sendArguments.add(MESSAGE_NEWOBJ_PROPERTIES);
    sendArguments.add(String.valueOf(object));
    sendArguments.add(open ? TRUE : FALSE);
    sendArguments.add(String.valueOf(symbol));
    sendArguments.add(String.valueOf(size));
    sendArguments.add(String.valueOf(size3D));
  } 
 
  /**
   * Places arguments to set model mode on the argument list.
   */
  public void setModelModeMessage() {
    sendArguments.add(MESSAGE_MODEL_MODE);
    sendArguments.add("1");
  }

  /**
   * Switch the 3dmod process to model mode
   * 
   * @throws SystemProcessException
   */
  public void modelMode() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_MODEL_MODE;
    imodSendEvent(args);
  }

  /**
   * Places arguments to set movie mode on the argument list.
   */
  public void setMovieModeMessage() {
    sendArguments.add(MESSAGE_MODEL_MODE);
    sendArguments.add("0");
  }

  /**
   * Switch the 3dmod process to movie mode
   * 
   * @throws SystemProcessException
   */
  public void movieMode() throws SystemProcessException {
    String[] args = new String[2];
    args[0] = MESSAGE_MODEL_MODE;
    args[1] = "0";
    imodSendEvent(args);
  }

  /**
   * Places arguments to raise 3dmod on the argument list.
   */
  public void setRaise3dmodMessage() {
    sendArguments.add(MESSAGE_RAISE);
  }

  /**
   * Raise the 3dmod window
   * 
   * @throws SystemProcessException
   */
  public void raise3dmod() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_RAISE;
    imodSendEvent(args);
  }
  
  /**
   * Places arguments to open one zap window and raise 3dmod on the argument
   * list.
   */
  public void setOpenZapWindowMessage() {
    sendArguments.add(MESSAGE_ONE_ZAP_OPEN);
  }
  
  /**
   * Open one zap window and raise 3dmod.
   */
  public void openZapWindow() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_ONE_ZAP_OPEN;
    imodSendEvent(args);
  }

  /**
   * Places arguments to open the beadfixer dialog on the argument list.
   */
  public void setOpenBeadFixerMessage() {
    sendArguments.add(MESSAGE_OPEN_BEADFIXER);
  }
  /**
   * Open the beadfixer dialog
   * 
   * @throws SystemProcessException
   */
  public void openBeadFixer() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_OPEN_BEADFIXER;
    imodSendEvent(args);
  }
  
  /**
   * Sends message requesting rubberband coordinates.
   * Should not be used with sendMessages().
   * @return rubberband coordinates and error messages
   * @throws SystemProcessException
   */
  public Vector getRubberbandCoordinates() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_RUBBERBAND;
    return imodSendAndReceive(args);
  }
  
  public Vector getSlicerAngles() throws SystemProcessException {
    String[] args = new String[1];
    args[0] = MESSAGE_SLICER_ANGLES;
    return imodSendAndReceive(args);
  }

  /**
   * Sends all messages collected in the argument list via imodSendEvent().
   * Clears the argument list.
   * @throws SystemProcessException
   */
  public void sendMessages() throws SystemProcessException {
    if (sendArguments.size() == 0) {
      return;
    }
    imodSendEvent((String[]) sendArguments.toArray(new String[sendArguments.size()]));
    sendArguments.clear();
  }
  
  /**
   * Sends a message and then records the results found in the error stream.
   * @param args
   * @return
   * @throws SystemProcessException
   */
  protected Vector imodSendAndReceive(String[] args) throws SystemProcessException {
    Vector results = new Vector();
    imodSendEvent(args, results);
    //3dmod sends the results before it returns 
    //the exit value to imodSendEvent - no waiting
    if (imod == null) {
      return results;
    }
    String line;
    line = imod.readStderr();
    if (line == null) {
      return results;
    }
    //Currently assuming results can only be on one line.
    do {
      if (!parseError(line, results)) {
        String[] words = line.split("\\s+");
        for (int i = 0; i < words.length; i++) {
          results.add(words[i]);
        } 
      }
    } while ((line = imod.readStderr()) != null);
    return results;
  }
  
  protected boolean parseError(String line, Vector errorMessage) {
    //Currently assuming that an error or warning message will be only one
    //line and contain ERROR_STRING or WARNING_STRING.
    int index = line.indexOf(ERROR_STRING);
    if (index != -1) {
      errorMessage.add(line.substring(index));
      return true;
    }
    index = line.indexOf(WARNING_STRING);
    if (index != -1) {
      errorMessage.add(line.substring(index));
      return true;
    }
    return false;
  }


  private void imodSendEvent(String[] args) throws SystemProcessException {
    imodSendEvent(args, null);
  }
  /**
   * Send an event to 3dmod using the imodsendevent command
   */
  private void imodSendEvent(String[] args, Vector messages) throws SystemProcessException {
    if (windowID.equals("")) {
      throw (new SystemProcessException("No window ID available for imod"));
    }
    String command = ApplicationManager.getIMODBinPath() + "imodsendevent "
        + windowID + " ";
    for (int i = 0; i < args.length; i++) {
      command = command + args[i] + " ";
    }
    if(EtomoDirector.getInstance().isDebug()) {
      System.err.print(command);
    }
    InteractiveSystemProgram imodSendEvent = new InteractiveSystemProgram(
      command);

    //  Start the imodSendEvent program thread and wait for it to finish
    Thread sendEventThread = new Thread(imodSendEvent);
    sendEventThread.start();
    try {
      sendEventThread.join();
    }
    catch (Exception except) {
      except.printStackTrace();
    }
    if(EtomoDirector.getInstance().isDebug()) {
      System.err.println("...done");
    }

    // Check imodSendEvent's exit code, if it is not zero read in the
    // stderr/stdout stream and throw an exception describing why the file
    // was not loaded
    if (imodSendEvent.getExitValue() != 0) {

      String message = IMOD_SEND_EVENT_STRING + " "
          + String.valueOf(imodSendEvent.getExitValue()) + "\n";

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
      
      if (messages == null) {
        throw (new SystemProcessException(message));
      }
      messages.add(message);
    }
  }

  /**
   * Returns the datasetName.
   * 
   * @return String
   */
  public String getDatasetName() {
    return datasetName;
  }

  /**
   * Returns the modelName.
   * 
   * @return String
   */
  public String getModelName() {
    return modelName;
  }

  /**
   * Returns the windowID.
   * 
   * @return String
   */
  public String getWindowID() {
    return windowID;
  }

  /**
   * Returns the swapYZ.
   * 
   * @return String
   */
  public boolean getSwapYZ() {
    return swapYZ;
  }

  /**
   * Returns the windowID.
   * 
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
   * 
   * @param modelView The modelView to set
   */
  public void setModelView(boolean modelView) {
    this.modelView = modelView;
  }

  /**
   * @return
   */
  public boolean isUseModv() {
    return useModv;
  }

  /**
   * @param b
   */
  public void setUseModv(boolean b) {
    useModv = b;
  }

  /**
   * @return
   */
  public boolean isOutputWindowID() {
    return outputWindowID;
  }

  /**
   * @param b
   */
  public void setOutputWindowID(boolean b) {
    outputWindowID = b;
  }
  
  public void setBinning(int binning) {
    if (binning < defaultBinning) {
      this.binning = defaultBinning;
    }
    else {
      this.binning = binning;
    }
  }
  
  public void setBinningXY(int binningXY) {
    if (binningXY < defaultBinning) {
      this.binningXY = defaultBinning;
    }
    else {
      this.binningXY = binningXY;
    }
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",datasetName=" + datasetName + ", modelName=" + modelName
        + ", windowID=" + windowID + ", swapYZ=" + swapYZ + ", modelView=" 
        + modelView + ", useModv=" + useModv
        + ", outputWindowID=" + outputWindowID + ", binning=" + binning;
  }

}