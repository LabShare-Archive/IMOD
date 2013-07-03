package etomo.process;

import etomo.BaseManager;
import etomo.comscript.TomosnapshotParam;
import etomo.type.AxisID;
import etomo.ui.swing.UIHarness;

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
 * <p> $Log$
 * <p> Revision 1.5  2011/02/22 04:12:01  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.4  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2009/04/10 22:47:46  sueh
 * <p> bug# 1206 Put the right symbol on the success popup.
 * <p>
 * <p> Revision 1.1  2009/04/06 22:39:49  sueh
 * <p> bug# 1206 Class to run run tomosnapshot on a separate thread without a manager or
 * <p> EtomoDirector.
 * <p> </p>
 */
final class TomosnapshotProcess implements Runnable {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;
  private final AxisID axisID;
  private final boolean thumbnail;

  TomosnapshotProcess(final BaseManager manager, final AxisID axisID,
      final boolean thumbnail) {
    this.manager = manager;
    this.axisID = axisID;
    this.thumbnail = thumbnail;
  }

  public void run() {
    // Run tomosnapshot.
    TomosnapshotParam param = new TomosnapshotParam(manager, axisID);
    if (thumbnail
        && UIHarness.INSTANCE.openYesNoDialogWithDefaultNo(manager,
            "Should the snapshot of this dataset include thumbnails of image data?",
            "Include Image Data?", axisID)) {
      param.setThumbnail(true);
    }
    SystemProgram sysProgram = new SystemProgram(null, System.getProperty("user.dir"),
        param.getCommandArray(), axisID);

    Thread thread = new Thread(sysProgram);
    thread.start();
    // Wait until tomosnapshot is done.
    try {
      while (!sysProgram.isDone()) {
        Thread.sleep(50);
      }
    }
    catch (InterruptedException e) {
    }
    // Pop up done message.
    String[] stdout = sysProgram.getStdOutput();
    if (sysProgram.isDone() && sysProgram.getExitValue() == 0) {
      if (stdout == null || stdout.length == 0) {
        UIHarness.INSTANCE.openMessageDialog(null,
            "Snapshot file created in " + System.getProperty("user.dir"),
            "Snapshot Created", axisID);
      }
      else {
        UIHarness.INSTANCE.openInfoMessageDialog(null, stdout[stdout.length - 1],
            "Snapshot Created", axisID);
      }
      return;
    }
    // Handle error or interrupt
    StringBuffer errorMessage = new StringBuffer();
    String title;
    if (!sysProgram.isDone()) {
      // Handle interrupt.
      title = "Process Incomplete";
      errorMessage.append("Tomosnapshot process did not finish.");
    }
    else {
      // Handle error.
      title = "Process Failed";
      errorMessage.append("Unable to run tomosnapshot in "
          + System.getProperty("user.dir") + ".  Exit value is "
          + sysProgram.getExitValue() + ".  ");
    }
    // Pop up message for error or interrupt.
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
    UIHarness.INSTANCE.openMessageDialog(null, errorMessage.toString(), title, axisID);
  }
}
