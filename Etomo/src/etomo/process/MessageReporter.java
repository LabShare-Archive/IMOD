package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.SwingUtilities;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.ui.UIHarness;

/**
 * <p>Description: Checks a file for a line starting with "MESSAGE:".  When it
 * finds a matching line, it pops up a message with the line.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/01/11 23:50:10  sueh
 * <p> bug# 1299 Class that tells the main thread to pop up messages starting
 * <p> with "MESSAGE:".
 * <p> </p>
 */
final class MessageReporter {
  public static final String rcsid = "$Id$";

  private static final String TOKEN = "MESSAGE:";//"Error returned";

  private final LogFile file;
  private final BaseManager manager;
  private final AxisID axisID;

  private LogFile.ReaderId id = null;

  MessageReporter(BaseManager manager, AxisID axisID, LogFile file) {
    this.manager = manager;
    this.axisID = axisID;
    this.file = file;
  }

  void checkForMessages() {
    if (id == null) {
      try {
        id = file.openReader();
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
    }
    if (id != null) {
      String line = null;
      try {
        while ((line = file.readLine(id)) != null) {
          if (line.trim().startsWith(TOKEN)) {
            SwingUtilities.invokeLater(new PopupLater(manager, axisID, line
                .substring(line.indexOf(TOKEN) + TOKEN.length())));
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
    }
  }

  void close() {
    if (id == null) {
      return;
    }
    file.closeReader(id);
    id = null;
  }

  private static final class PopupLater implements Runnable {
    private final String message;
    private final BaseManager manager;
    private final AxisID axisID;

    private PopupLater(BaseManager manager, AxisID axisID, String message) {
      this.message = message;
      this.manager = manager;
      this.axisID = axisID;
    }

    public void run() {
      UIHarness.INSTANCE.openMessageDialog(message, "Process Message", manager
          .getManagerKey());
    }
  }
}
