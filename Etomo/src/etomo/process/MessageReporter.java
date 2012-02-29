package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import javax.swing.SwingUtilities;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.ui.swing.UIHarness;

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
 * <p> Revision 1.4  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.2  2010/01/12 04:58:20  sueh
 * <p> bug# 1299 Removed print statement
 * <p>
 * <p> Revision 1.1  2010/01/11 23:50:10  sueh
 * <p> bug# 1299 Class that tells the main thread to pop up messages starting
 * <p> with "MESSAGE:".
 * <p> </p>
 */
final class MessageReporter {
  public static final String rcsid = "$Id$";

  private static final String TOKEN = "MESSAGE:";//"Error returned";

  private final Set printedMessageSet = new HashSet<String>();

  private final LogFile file;
  private final AxisID axisID;

  private LogFile.ReaderId id = null;

  MessageReporter(AxisID axisID, LogFile file) {
    this.axisID = axisID;
    this.file = file;
  }

  void checkForMessages(BaseManager manager) {
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
            if (!printedMessageSet.contains(line)) {
              printedMessageSet.add(line);
              SwingUtilities.invokeLater(new PopupLater(manager, axisID, line
                  .substring(line.indexOf(TOKEN) + TOKEN.length())));
            }
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
    file.closeRead(id);
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
      UIHarness.INSTANCE.openMessageDialog(manager, message, "Process Message");
    }
  }
}
