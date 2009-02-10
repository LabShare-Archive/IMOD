package etomo.ui;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;

import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.util.Utilities;

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
 * <p> Revision 1.1  2009/02/04 23:35:18  sueh
 * <p> bug# 1158 The log window panel.  One instance per manager is created.
 * <p> </p>
 */
public final class LogPanel {
  public static final String rcsid = "$Id$";

  static final String TITLE = "Project Log";
  static final String SAVE_LABEL = "Save Log";
  static final String LOG_WINDOW_LABEL = "Hide Log Window";
  static final String LINE_SEPARATOR = System.getProperty("line.separator");

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtchedBorder border = new EtchedBorder(TITLE);
  private final JTextArea textArea = new JTextArea(10, 60);
  private JScrollPane scrollPane = new JScrollPane(textArea);

  private LogFile file = null;
  private String userDir = null;
  private String datasetName = null;
  private boolean changed = false;
  private boolean fileFailed = false;
  private boolean writeFailed = false;
  private Dimension frameSize = null;
  private Point frameLocation = null;

  private LogPanel() {
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(border.getBorder());
    rootPanel.add(scrollPane);
  }

  public static LogPanel getInstance() {
    LogPanel instance = new LogPanel();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    textArea.addKeyListener(new LogPanelKeyListener(this));
  }

  /**
   * Sets the title.  If paramFile and metaData are set then also sets ready to
   * true.  Sends a message to the LogFrame.  Reads the file into the text area.
   * Must be run with a paramFile and metaData before save() can be run.
   * Synchronized to prevent save() from running before setTitle has been
   * competed.  If this is a new dataset, sets up a header for the log.
   * @param paramFile
   * @param metaData
   */
  synchronized void setTitle(File paramFile, BaseMetaData metaData,
      String propertyUserDir) {
    if (metaData != null && paramFile != null) {
      datasetName = metaData.getName();
      border.setTitle(datasetName + " " + TITLE);
      userDir = propertyUserDir;
      if (openFile()) {
        if (file.exists()) {
          if (file != null) {
            LogFile.ReaderId readerId = null;
            try {
              readerId = file.openReader();
              String line = file.readLine(readerId);
              List lineList = new ArrayList();
              while (line != null) {
                lineList.add(line);
                line = file.readLine(readerId);
              }
              SwingUtilities.invokeLater(new AppendLater(true, lineList));
              file.closeReader(readerId);
            }
            catch (LogFile.LockException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog("Unabled to load "
                  + file.getAbsolutePath(), "System Error");
            }
            catch (IOException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog("Unabled to load "
                  + file.getAbsolutePath(), "System Error");
            }
          }
        }
        else {
          SwingUtilities.invokeLater(new AppendLater(Utilities
              .getDateTimeStamp(), datasetName, userDir));
        }
      }
    }
    else {
      datasetName = null;
      border.setTitle(TITLE);
      file = null;
    }
    UIHarness.INSTANCE.msgChanged(this);
  }

  /**
   * Save the frame size from when this panel was displayed.
   * @param input
   */
  void setFrameSize(Dimension input) {
    frameSize = input;
  }

  Dimension getFrameSize() {
    return frameSize;
  }

  /**
   * Save the frame location from when this panel was displayed.
   * @param input
   */
  void setFrameLocation(Point input) {
    frameLocation = input;
  }

  Point getFrameLocation() {
    return frameLocation;
  }

  /**
   * Always save.
   */
  public void menuSave() {
    save(true);
  }

  /**
   * Save if something has changed.
   */
  public void save() {
    save(false);
  }

  /**
   * Save textArea to file.
   * @param force - when true, save even if changed is false
   */
  private synchronized void save(boolean force) {
    if (!force && !changed) {
      return;
    }
    if (!openFile()) {
      return;
    }
    try {
      file.backup();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    LogFile.WriterId writerId = null;
    try {
      writerId = file.openWriter();
      changed = false;
      file.write(textArea.getText(), writerId);
      file.closeWriter(writerId);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      if (!writeFailed) {
        writeFailed = true;
        UIHarness.INSTANCE.openMessageDialog("Unabled to write to file "
            + file.getAbsolutePath(), "System Error");
        if (writerId != null && !writerId.isEmpty()) {
          file.closeWriter(writerId);
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      if (!writeFailed) {
        writeFailed = true;
        UIHarness.INSTANCE.openMessageDialog("Unabled to write to file "
            + file.getAbsolutePath(), "System Error");
        if (writerId != null && !writerId.isEmpty()) {
          file.closeWriter(writerId);
        }
      }
    }
  }

  /**
   * Set file if necessary.
   * @return  True if file can be used.
   */
  private boolean openFile() {
    //Can't open log until the dataset and userDir is set
    if (datasetName == null || userDir == null) {
      return false;
    }
    if (file == null) {
      String fileName = datasetName + "_project.log";
      try {
        file = LogFile.getInstance(userDir, datasetName + "_project.log");
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        if (!fileFailed) {
          fileFailed = true;
          UIHarness.INSTANCE.openMessageDialog("Unabled to access file "
              + fileName + " in " + userDir, "System Error");
        }
        return false;
      }
    }
    return true;
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    if (loggable == null) {
      return;
    }
    try {
      SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(),
          loggable.getName() + " - " + axisID + " axis:", loggable
              .getLogMessage()));
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      SwingUtilities.invokeLater(new AppendLater("Unable to log message:", e
          .getMessage()));
    }
    catch (IOException e) {
      e.printStackTrace();
      SwingUtilities.invokeLater(new AppendLater("Unable to log message:", e
          .getMessage()));
    }
  }

  public void logMessage(String title, AxisID axisID, String[] message) {
    SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(),
        title + " - " + axisID + " axis:", message));
  }

  private void msgChanged() {
    changed = true;
  }

  String getTitle() {
    return border.getTitle();
  }

  JPanel getRootPanel() {
    return rootPanel;
  }

  private final class AppendLater implements Runnable {
    boolean loadingFromFile = false;
    private String line1 = null;
    private String line2 = null;
    private String line3 = null;
    private String[] stringArray = null;
    private List lineList = null;

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

    private AppendLater(String line1, String line2, List lineList) {
      this.line1 = line1;
      this.line2 = line2;
      this.lineList = lineList;
    }

    private AppendLater(boolean loadingFromFile, List lineList) {
      this.loadingFromFile = loadingFromFile;
      this.lineList = lineList;
    }

    /**
     * Append lines and lineList to textArea.
     */
    public void run() {
      if (line1 != null) {
        newLine();
        textArea.append(line1);
      }
      if (line2 != null) {
        newLine();
        textArea.append(line2);
      }
      if (line3 != null) {
        newLine();
        textArea.append(line3);
      }
      if (stringArray != null) {
        for (int i = 0; i < stringArray.length; i++) {
          newLine();
          textArea.append((String) stringArray[i]);
        }
      }
      if (lineList != null) {
        for (int i = 0; i < lineList.size(); i++) {
          newLine();
          textArea.append((String) lineList.get(i));
        }
      }
      if (!loadingFromFile) {
        msgChanged();
      }
    }

    /**
     * Appends a newline character if the last line in the text area is not empty
     *
     */
    private void newLine() {
      try {
        //messages should be alone on a line
        int lastLineEndOffset = textArea.getLineEndOffset(textArea
            .getLineCount() - 1);
        if (lastLineEndOffset != 0) {
          //textArea.append("\n");
          textArea.append(LINE_SEPARATOR);
        }
      }
      catch (BadLocationException e) {
        e.printStackTrace();
      }
    }
  }

  private static final class LogPanelKeyListener implements KeyListener {
    private final LogPanel adaptee;

    private LogPanelKeyListener(LogPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void keyPressed(KeyEvent event) {
    }

    public void keyReleased(KeyEvent event) {
    }

    public void keyTyped(KeyEvent event) {
      adaptee.msgChanged();
    }
  }
}
