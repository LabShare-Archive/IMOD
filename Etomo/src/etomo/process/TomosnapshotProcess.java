package etomo.process;

import etomo.comscript.TomosnapshotParam;
import etomo.type.AxisID;
import etomo.ui.UIHarness;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
final class TomosnapshotProcess implements Runnable {
  public static final String rcsid = "$Id$";

  private final AxisID axisID;

  TomosnapshotProcess(AxisID axisID) {
    this.axisID = axisID;
  }

  public void run() {
    //Run tomosnapshot.
    SystemProgram sysProgram = new SystemProgram(
        System.getProperty("user.dir"), new TomosnapshotParam(axisID)
            .getCommandArray(), axisID, null);
    Thread thread = new Thread(sysProgram);
    thread.start();
    //Wait until tomosnapshot is done.
    try {
      while (!sysProgram.isDone()) {
        Thread.sleep(50);
      }
    }
    catch (InterruptedException e) {
    }
    //Pop up done message.
    String[] stdout = sysProgram.getStdOutput();
    if (sysProgram.isDone() && sysProgram.getExitValue() == 0) {
      if (stdout == null || stdout.length == 0) {
        UIHarness.INSTANCE.openMessageDialog("Snapshot file created in "
            + System.getProperty("user.dir"), "Snapshot Created", axisID, null);
      }
      else {
        UIHarness.INSTANCE.openMessageDialog(stdout[stdout.length - 1],
            "Snapshot Created", axisID, null);
      }
      return;
    }
    //Handle error or interrupt
    StringBuffer errorMessage = new StringBuffer();
    String title;
    if (!sysProgram.isDone()) {
      //Handle interrupt.
      title = "Process Incomplete";
      errorMessage.append("Tomosnapshot process did not finish.");
    }
    else {
      //Handle error.
      title = "Process Failed";
      errorMessage.append("Unable to run tomosnapshot in "
          + System.getProperty("user.dir") + ".  Exit value is "
          + sysProgram.getExitValue() + ".  ");
    }
    //Pop up message for error or interrupt.
    if (stdout != null) {
      for (int i = 0; i < stdout.length; i++) {
        String line = stdout[i].trim();
        if (line.startsWith("ERROR:")) {
          errorMessage.append(stdout[i] + "  ");
        }
      }
    }
    String[] stderr = sysProgram.getStdError();
    if (stderr != null) {
      for (int i = 0; i < stderr.length; i++) {
        errorMessage.append(stderr[i] + "  ");
      }
    }
    UIHarness.INSTANCE.openMessageDialog(errorMessage.toString(),
        title, axisID, null);
  }
}
