package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.swing.UIHarness;

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
      send(manager, action, axisID);
    }
    catch (IOException e) {
      //try running the program again, in case it died
      run(manager, axisID);
      try {
        send(manager, action, axisID);
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
    }
  }

  private void send(BaseManager manager, String action, AxisID axisID) throws IOException {
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
        UIHarness.INSTANCE.openMessageDialog(manager, line,
            "Problem Displaying Help Topic", axisID);
      }
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
 * <p> $Log$
 * <p> Revision 1.4  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2009/03/17 00:36:21  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.1  2006/06/21 15:47:32  sueh
 * <p> bug# 581 Runs imodqtassist and sends commands to in via its stdin.  Quits
 * <p> imodqtassist when etomo exits.
 * <p> </p>
 */
