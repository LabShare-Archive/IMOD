package etomo.ui.swing;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;

import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.type.AxisID;
import etomo.util.Utilities;

/**
 * <p>Description: Uses SwingUtilities.invokeLater to add timestamps and lines
 * to a LogInterface.</p>
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
 * <p> Revision 1.2  2011/02/22 18:08:09  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2010/02/17 04:54:36  sueh
 * <p> bug# 1301 Logging functionaity shared by LogPanel and ToolsDialog.
 * <p> </p>
 */
final class EtomoLogger {
  public static final String rcsid = "$Id$";

  private final LogInterface logInterface;

  EtomoLogger(final LogInterface logInterface) {
    this.logInterface = logInterface;
  }

  synchronized void loadMessages(List<String> lineList) throws LogFile.LockException,
      IOException {
    SwingUtilities.invokeLater(new AppendLater(true, lineList));
  }

  public void logMessage(String line1, String line2) {
    SwingUtilities
        .invokeLater(new AppendLater(Utilities.getDateTimeStamp(), line1, line2));
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    if (loggable == null) {
      return;
    }
    try {
      SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(), loggable
          .getName() + " - " + axisID + " axis:", loggable.getLogMessage()));
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      SwingUtilities
          .invokeLater(new AppendLater("Unable to log message:", e.getMessage()));
    }
    catch (IOException e) {
      e.printStackTrace();
      SwingUtilities
          .invokeLater(new AppendLater("Unable to log message:", e.getMessage()));
    }
  }

  public void logMessage(String title, AxisID axisID, String[] message) {
    SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(), title
        + " - " + axisID + " axis:", message));
  }

  public void logMessage(String title, AxisID axisID, List<String> message) {
    SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(), title
        + " - " + axisID + " axis:", message));
  }

  public void logMessage(final String title, final AxisID axisID) {
    SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(), title
        + " - " + axisID + " axis:"));
  }

  public void logMessage(final String message) {
    SwingUtilities.invokeLater(new AppendLater(message));
  }

  public void logMessage(final File file) {
    SwingUtilities.invokeLater(new AppendLater(file));
  }

  private final class AppendLater implements Runnable {
    boolean loadingFromFile = false;
    private String line1 = null;
    private String line2 = null;
    private String line3 = null;
    private String[] stringArray = null;
    private List<String> lineList = null;
    private File file = null;

    private AppendLater(String line1) {
      this.line1 = line1;
    }

    private AppendLater(String line1, String line2) {
      this.line1 = line1;
      this.line2 = line2;
    }

    private AppendLater(String line1, String line2, String line3) {
      this.line1 = line1;
      this.line2 = line2;
      this.line3 = line3;
    }

    private AppendLater(String line1, String line2, String[] stringArray) {
      this.line1 = line1;
      this.line2 = line2;
      this.stringArray = stringArray;
    }

    private AppendLater(String line1, String line2, List<String> lineList) {
      this.line1 = line1;
      this.line2 = line2;
      this.lineList = lineList;
    }

    private AppendLater(boolean loadingFromFile, List<String> lineList) {
      this.loadingFromFile = loadingFromFile;
      this.lineList = lineList;
    }

    private AppendLater(final File file) {
      this.file = file;
    }

    /**
     * Append lines and lineList to textArea.
     */
    public void run() {
      newLine();
      if (line1 != null) {
        newLine();
        logInterface.append(line1);
      }
      if (line2 != null) {
        newLine();
        logInterface.append(line2);
      }
      if (line3 != null) {
        newLine();
        logInterface.append(line3);
      }
      if (stringArray != null) {
        for (int i = 0; i < stringArray.length; i++) {
          newLine();
          logInterface.append((String) stringArray[i]);
        }
      }
      if (lineList != null) {
        for (int i = 0; i < lineList.size(); i++) {
          newLine();
          logInterface.append(lineList.get(i));
        }
      }
      if (file != null && file.exists() && file.isFile() && file.canRead()) {
        newLine();
        logInterface.append("Logging from file: " + file.getAbsolutePath());
        newLine();
        try {
          LogFile logFile = LogFile.getInstance(file);
          LogFile.ReaderId id = logFile.openReader();
          String line = null;
          while ((line = logFile.readLine(id)) != null) {
            logInterface.append(line);
            newLine();
          }
        }
        catch (LogFile.LockException e) {
          e.printStackTrace();
          System.err.println("Unable to log from file.  " + e.getMessage());
        }
        catch (FileNotFoundException e) {
          e.printStackTrace();
          System.err.println("Unable to log from file.  " + e.getMessage());
        }
        catch (IOException e) {
          e.printStackTrace();
          System.err.println("Unable to log from file.  " + e.getMessage());
        }
      }
      if (!loadingFromFile) {
        logInterface.msgChanged();
      }
    }

    /**
     * Appends a newline character if the last line in the text area is not empty
     *
     */
    private void newLine() {
      try {
        // messages should be alone on a line
        int lastLineEndOffset = logInterface.getLineEndOffset();
        if (lastLineEndOffset != 0) {
          logInterface.append("\n");
        }
      }
      catch (BadLocationException e) {
        e.printStackTrace();
      }
    }
  }
}
