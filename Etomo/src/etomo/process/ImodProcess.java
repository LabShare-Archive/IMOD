package etomo.process;

/*
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

  private String datasetName = "";
  private String modelName = "";
  private String windowID = "";
  private String processID = "";
  private boolean swapYZ = false;

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
   * Open the imod process if is not already open.
   */
  public void open() throws Exception {
    if(isRunning()) {
      return;
    }

    String stringYZ = "";
    if(swapYZ) {
      stringYZ = "-Y ";
    }
    String command = "imod -W " + stringYZ + datasetName + " " + modelName;
    InteractiveSystemProgram imod = new InteractiveSystemProgram(command);

    //  Start the imod program thread and wait for it to finish
    Thread imodThread = new Thread(imod);
    imodThread.start();
    try {
      imodThread.join();
    }
    catch (Exception except) {
      except.printStackTrace();
    }

    // Check imod's exit code, if it is zero parse the windowID from
    // stderr stream, otherwise throw an exception describing why the file 
    // was not loaded
    if (imod.getExitValue() == 0) {

      String line = imod.readStderr();
      if(line == null) {
        throw(new Exception("Did not find window ID from imod\n"));
      }

      String[] words = line.split("\\s+");
      if(words.length < 4) {
         throw(new Exception("Could not parse window ID from imod\n"));
      } 
      windowID = words[3];

      line = imod.readStderr();
      if(line == null) {
        throw(new Exception("Did not find process ID from imod\n"));
      }

      words = line.split("\\s+");
      if(words.length < 4) {
         throw(new Exception("Could not parse process ID from imod\n"));
      } 
      processID = words[3];

    }
    else {
      String message = "imod returned: " + String.valueOf(imod.getExitValue())
        + "\n";

      String line = imod.readStderr();
      while (line != null) {
        message = message + "stderr: " + line + "\n";
        line = imod.readStderr();
      }

      line = imod.readStdout();
      while (line != null) {
        message = message + "stdout: " + line + "\n";
        line = imod.readStdout();
      }
      
      throw(new Exception(message));
    }
  }


  /**
   * Check to see if this imod process is running
   */
  private boolean isRunning() {
    SystemProgram checkImod = new SystemProgram("ps -p " + processID);
    checkImod.run();
    String[] stdout = checkImod.getStdOutput();

    if(stdout.length < 2) {
      return false;
    }
    if(stdout[1].indexOf("imod") == -1) {
      return false;
    }
    return true;
  }
  /**
   * Open a new model file
   */
  public void openModel(String newModelName) throws Exception {
    modelName = newModelName;
    String[] args = new String[2];
    args[0] = MESSAGE_OPEN_MODEL;
    args[1] = newModelName;
    imodSendEvent(args);
  }


  /**
   * Save the current model file
   */
  public void saveModel() throws Exception {
    String[] args = new String[1];
    args[0] = MESSAGE_SAVE_MODEL;
    imodSendEvent(args);
  }


  /**
   * View the current model file
   */
  public void viewModel() throws Exception {
    String[] args = new String[1];
    args[0] = MESSAGE_VIEW_MODEL;
    imodSendEvent(args);
  }


  /**
   * Send an event to imod using the imodsendevent command
   */
  private void imodSendEvent(String[] args) throws Exception {
    if(windowID == "") {
      throw(new Exception("No window ID available for imod"));
    }

    String command = "imodsendevent " + windowID + " ";
    for(int i = 0;i < args.length; i++) {
      command  = command + args[i] + " ";
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
    
    // Check imodSendEvent's exit code, if it is zero parse the windowID from
    // stderr stream, otherwise throw an exception describing why the file 
    // was not loaded
    if (imodSendEvent.getExitValue() != 0) {

      String message = "imodsendevent returned: " + 
      String.valueOf(imodSendEvent.getExitValue())
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
      
      throw(new Exception(message));
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

}
