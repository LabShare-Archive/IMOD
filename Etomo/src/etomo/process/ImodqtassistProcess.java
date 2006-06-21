package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.UIHarness;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class ImodqtassistProcess {
  public static final String rcsid = "$Id$";

  public static final ImodqtassistProcess INSTANCE = new ImodqtassistProcess();
  private InteractiveSystemProgram program = null;

  private ImodqtassistProcess() {
  }

  public void open(BaseManager manager, String action, AxisID axisID) {
    if (program == null) {
      run(manager, axisID);
    }
    try {
      send(action, axisID);
    }
    catch (IOException e) {
      //try running the program again, in case it died
      run(manager, axisID);
      try {
        send(action, axisID);
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
    }
  }

  private void send(String action, AxisID axisID) throws IOException {
    program.setCurrentStdInput(action);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    StringBuffer buffer = new StringBuffer();
    String line = null;
    while ((line = program.readStderr()) != null) {
      if (line.startsWith("ERROR:") || line.startsWith("WARNING:")) {
      UIHarness.INSTANCE.openMessageDialog(line,
          "Problem Displaying Help Topic", axisID);}
    }
  }

  private void run(BaseManager manager, AxisID axisID) {
    if (program == null) {
      //construct the interactive system program
      String command[] = new String[5];
      command[0] = "imodqtassist";
      command[1] = "-p";
      command[2] = "IMOD.adp";
      command[3] = "-k";
      command[4] = "html";
      program = new InteractiveSystemProgram(manager, command, axisID);
    }
    //run program
    new Thread(program).start();
    //Wait while the imodqtassist starts
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
  }

  public void quit() {
    if (program == null) {
      return;
    }
    try {
      program.setCurrentStdInput("q");
    }
    catch (IOException e) {
      //program is probably already dead
    }
    program = null;
  }
}
/**
 * <p> $Log$ </p>
 */
