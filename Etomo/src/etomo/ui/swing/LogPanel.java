package etomo.ui.swing;

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
import javax.swing.text.BadLocationException;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.ConstLogProperties;

/**
 * <p>Description: Text of a project log.  Associated with one manager.  Do not
 * pass the manager when popping up messages.  This could lead to an infinite
 * loop because every message with a manager gets logged.</p>
 * 
 * <p>Copyright: Copyright 2008 - 2010</p>
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
 * <p> Revision 1.2  2011/02/22 18:14:12  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.7  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.6  2009/04/02 19:18:10  sueh
 * <p> bug# 1206 Handle a null managerKey parameter.  Log if the managerKey parameter is
 * <p> null.
 * <p>
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
public final class LogPanel implements Storable, LogInterface {
  public static final String rcsid = "$Id$";

  static final String TITLE = "Project Log";
  static final String SAVE_LABEL = "Save Log";
  static final String LOG_WINDOW_LABEL = "Hide Log Window";

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtchedBorder border = new EtchedBorder(TITLE);
  private final JTextArea textArea = new JTextArea(10, 60);
  private final JScrollPane scrollPane = new JScrollPane(textArea);
  private final EtomoLogger logger = new EtomoLogger(this);

  private LogFile file = null;
  private String userDir = null;
  private String datasetName = null;
  private boolean changed = false;
  private boolean fileFailed = false;
  private boolean writeFailed = false;
  private ConstLogProperties frameProperties = null;
  private boolean frameVisible = true;

  private final BaseManager manager;

  private LogPanel(BaseManager manager) {
    this.manager = manager;
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(border.getBorder());
    rootPanel.add(scrollPane);
  }

  public static LogPanel getInstance(BaseManager manager) {
    LogPanel instance = new LogPanel(manager);
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
  synchronized void setTitle(File paramFile, BaseMetaData metaData, String propertyUserDir) {
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
              logger.loadMessages(lineList);
              file.closeReader(readerId);
            }
            catch (LogFile.LockException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog(null, "Unabled to load "
                  + file.getAbsolutePath(), "System Error");
            }
            catch (IOException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog(null, "Unabled to load "
                  + file.getAbsolutePath(), "System Error");
            }
          }
        }
        else {
          logger.logMessage(datasetName, userDir);
        }
      }
    }
    else {
      datasetName = null;
      border.setTitle(TITLE);
      file = null;
    }
    UIHarness.INSTANCE.msgLogChanged(manager, this);
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
                file
                    .write(lineArray[i].substring(0, lineArray[i].length() - 2), writerId);
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
        UIHarness.INSTANCE.openMessageDialog(null, "Unabled to write to file "
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
        UIHarness.INSTANCE.openMessageDialog(null, "Unabled to write to file "
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
          UIHarness.INSTANCE.openMessageDialog(null, "Unabled to access file " + fileName
              + " in " + userDir, "System Error");
        }
        return false;
      }
    }
    return true;
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    logger.logMessage(loggable, axisID);
  }

  public void logMessage(String title, AxisID axisID, String[] message) {
    logger.logMessage(title, axisID, message);
  }
  
  public void logMessage(String title, AxisID axisID, List message) {
    logger.logMessage(title, axisID, message);
  }

  public void msgChanged() {
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

  public void append(String line) {
    textArea.append(line);
  }

  public int getLineEndOffset() throws BadLocationException {
    return textArea.getLineEndOffset(textArea.getLineCount() - 1);
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