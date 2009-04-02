package etomo.ui;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;

import etomo.ManagerKey;
import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.ConstLogProperties;
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
 * <p> Revision 1.5  2009/03/16 23:24:30  sueh
 * <p> bug# 1186 In logMessage only log if the parameter managerKey equals the member
 * <p> variable managerKey.
 * <p>
 * <p> Revision 1.4  2009/03/05 23:29:21  sueh
 * <p> bug# 1194 Manage frame size and location with LogProperties.
 * <p>
 * <p> Revision 1.3  2009/02/13 02:34:31  sueh
 * <p> bug# 1158 In save, Stripping line endings and adding line endings that
 * <p> match the OS.
 * <p>
 * <p> Revision 1.2  2009/02/10 23:37:04  sueh
 * <p> bug# 1158 In newline() using line.separator instead of "\n".
 * <p>
 * <p> Revision 1.1  2009/02/04 23:35:18  sueh
 * <p> bug# 1158 The log window panel.  One instance per manager is created.
 * <p> </p>
 */
public final class LogPanel implements Storable {
  public static final String rcsid = "$Id$";

  static final String TITLE = "Project Log";
  static final String SAVE_LABEL = "Save Log";
  static final String LOG_WINDOW_LABEL = "Hide Log Window";

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtchedBorder border = new EtchedBorder(TITLE);
  private final JTextArea textArea = new JTextArea(10, 60);
  private final JScrollPane scrollPane = new JScrollPane(textArea);
  private final ManagerKey managerKey;

  private LogFile file = null;
  private String userDir = null;
  private String datasetName = null;
  private boolean changed = false;
  private boolean fileFailed = false;
  private boolean writeFailed = false;
  private ConstLogProperties frameProperties = null;
  private boolean frameVisible = true;

  private LogPanel(ManagerKey managerKey) {
    this.managerKey = managerKey;
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(border.getBorder());
    rootPanel.add(scrollPane);
  }

  public static LogPanel getInstance(ManagerKey managerKey) {
    LogPanel instance = new LogPanel(managerKey);
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
                  + file.getAbsolutePath(), "System Error", managerKey);
            }
            catch (IOException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog("Unabled to load "
                  + file.getAbsolutePath(), "System Error", managerKey);
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
    UIHarness.INSTANCE.msgLogChanged(this);
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
      String text = textArea.getText();
      //LogFile.newLine() will put the appropriate line endings in the file.
      //Strip the line endings by breaking up the text by \n.  Strip windows
      //line endings (\r\n) by removing the \r which, if it is in use, will now
      //be at the end of each line.  This also adds a new line to the end of the
      //file, if there is not one already there.  This should also preserve
      //empty lines.
      String[] lineArray = text.split("\n");
      if (lineArray != null) {
        for (int i = 0; i < lineArray.length; i++) {
          //Preserve an empty line by calling newLine.
          if (lineArray[i] != null && lineArray[i].length() != 0) {
            //Look for Windows line ending.
            if (lineArray[i].charAt(lineArray[i].length() - 1) == '\r') {
              //Preserve an empty line by calling newLine.
              if (lineArray[i].length() > 1) {
                //Write a line which has a Windows line ending (strip \r).
                file.write(
                    lineArray[i].substring(0, lineArray[i].length() - 2),
                    writerId);
              }
            }
            else {
              //Write a line which has a Linux line ending.
              file.write(lineArray[i], writerId);
            }
          }
          file.newLine(writerId);
        }
        file.closeWriter(writerId);
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      if (!writeFailed) {
        writeFailed = true;
        UIHarness.INSTANCE.openMessageDialog("Unabled to write to file "
            + file.getAbsolutePath(), "System Error", managerKey);
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
            + file.getAbsolutePath(), "System Error", managerKey);
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
        file = LogFile.getInstance(userDir, datasetName + "_project.log",
            managerKey);
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        if (!fileFailed) {
          fileFailed = true;
          UIHarness.INSTANCE.openMessageDialog("Unabled to access file "
              + fileName + " in " + userDir, "System Error", managerKey);
        }
        return false;
      }
    }
    return true;
  }

  public void logMessage(Loggable loggable, AxisID axisID, ManagerKey managerKey) {
    //If managerKey parameter is null, always log.
    if (loggable == null
        || (managerKey != null && !managerKey.equals(this.managerKey))) {
      return;
    }
    try {
      SwingUtilities.invokeLater(new AppendLater(Utilities.getDateTimeStamp(),
          loggable.getName() + " - " + axisID + " axis:", loggable
              .getLogMessage(managerKey)));
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

  public void logMessage(String title, AxisID axisID, String[] message,
      ManagerKey managerKey) {
    //If managerKey parameter is null, always log.
    if ((managerKey != null && !managerKey.equals(this.managerKey))) {
      return;
    }
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

  /**
   * Gets frame properties.-
   * @return
   */
  public ConstLogProperties getFrameProperties() {
    return frameProperties;
  }

  /**
   * Sets frame properties
   * @param constLogProperties
   */
  void setFrameProperties(ConstLogProperties constLogProperties) {
    frameProperties = constLogProperties;
  }

  /**
   * Loads the frame properties from props.
   */
  public void load(Properties props) {
    if (frameProperties == null) {
      frameProperties = new LogProperties();
    }
    frameProperties.load(props, "");
  }

  /**
   * Updates the frame properties and returns them.
   * @return
   */
  public ConstLogProperties getCurrentFrameProperties() {
    UIHarness.INSTANCE.msgUpdateLogProperties(this);
    return frameProperties;
  }

  /**
   * Updates the frame properties and stores them.
   */
  public void store(Properties props) {
    UIHarness.INSTANCE.msgUpdateLogProperties(this);
    if (frameProperties != null) {
      frameProperties.store(props, "");
    }
  }

  boolean isFrameVisible() {
    return frameVisible;
  }

  void setFrameVisible(boolean visible) {
    frameVisible = visible;
  }

  private final class AppendLater implements Runnable {
    boolean loadingFromFile = false;
    private String line1 = null;
    private String line2 = null;
    private String line3 = null;
    private String[] stringArray = null;
    private List lineList = null;

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
      newLine();
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
          textArea.append("\n");
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