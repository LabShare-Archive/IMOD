package etomo.ui.swing;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.text.BadLocationException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.ui.LogProperties;

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
public final class LogWindow implements LogInterface, LogProperties {
  public static final String rcsid = "$Id$";

  static final String TITLE = "Project Log";
  private static final boolean VISIBLE_DEFAULT = true;
  private static final String PREPEND = "ProjectLog";

  private final EtomoNumber frameSizeWidthProperty = new EtomoNumber("FrameSize.Width");
  private final EtomoNumber frameSizeHeightProperty = new EtomoNumber("FrameSize.Height");
  private final EtomoNumber frameLocationXProperty = new EtomoNumber("FrameLocation.X");
  private final EtomoNumber frameLocationYProperty = new EtomoNumber("FrameLocation.Y");
  private final EtomoBoolean2 visibleProperty = new EtomoBoolean2("Visible");
  private final JMenuBar menuBar = new JMenuBar();
  private final JMenu menuFile = new Menu("File");
  private final JMenuItem menuSave = new MenuItem("Save Log", KeyEvent.VK_S);
  private final JMenu menuView = new Menu("View");
  private final JMenuItem menuHide = new MenuItem("Hide Log Window", KeyEvent.VK_H);
  private final JMenuItem menuFitWindow = new MenuItem("Fit Log Window", KeyEvent.VK_F);
  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtchedBorder border = new EtchedBorder(TITLE);
  private final JTextArea textArea = new JTextArea(10, 60);
  private final JScrollPane scrollPane = new JScrollPane(textArea);
  private final EtomoLogger logger = new EtomoLogger(this);
  private final LogFrame frame = new LogFrame(menuHide);

  private LogFile file = null;
  private String userDir = null;
  private String datasetName = null;
  private boolean changed = false;
  private boolean fileFailed = false;
  private boolean writeFailed = false;
  private boolean displayed = false;

  private LogWindow() {
    frameSizeWidthProperty.setDisplayValue(683);
    frameSizeHeightProperty.setDisplayValue(230);
    visibleProperty.set(VISIBLE_DEFAULT);
  }

  public static LogWindow getInstance() {
    if (EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      return null;
    }
    LogWindow instance = new LogWindow();
    instance.createWindow();
    instance.addListeners();
    return instance;
  }

  private void createWindow() {
    // init
    frame.setVisible(false);
    // frame
    frame.setTitle(border.getTitle());
    frame.setJMenuBar(menuBar);
    // menu
    menuBar.add(menuFile);
    menuBar.add(menuView);
    menuFile.add(menuSave);
    menuView.add(menuHide);
    menuView.add(menuFitWindow);
    // content pane
    Container contentPane = frame.getContentPane();
    if (contentPane != null) {
      contentPane.add(rootPanel);
    }
    // root panel
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(border.getBorder());
    rootPanel.add(scrollPane);
  }

  private void addListeners() {
    // Mnemonics for the main menu bar
    menuFile.setMnemonic(KeyEvent.VK_F);
    menuView.setMnemonic(KeyEvent.VK_V);
    // Accelerators
    menuHide.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, ActionEvent.CTRL_MASK));
    menuFitWindow.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
        ActionEvent.CTRL_MASK));
    // Bind the menu items to their listeners
    MenuActionListener actionListener = new MenuActionListener(this);
    menuSave.addActionListener(actionListener);
    menuHide.addActionListener(actionListener);
    menuFitWindow.addActionListener(actionListener);
    // text area
    textArea.addKeyListener(new LogWindowKeyListener(this));
  }

  private void action(String command) {
    if (menuSave.getActionCommand().equals(command)) {
      menuSave();
    }
    else if (menuHide.getActionCommand().equals(command)) {
      hide();
      visibleProperty.set(false);
    }
    if (menuFitWindow.getActionCommand().equals(command)) {
      fit();
    }
  }

  private void fit() {
    frame.repaint();
    frame.pack();
  }

  /**
   * Called when the manager's interface is either displayed or hidden.  Hide the log
   * window when the interface is hidden.  Remember whether is was visible or not.  When
   * the interface is made current, show it if it was visible the last time the interface
   * was displayed.
   * @param current - true when interface is displayed
   */
  public final void msgCurrentManagerChanged(final boolean current,
      final boolean startupPopupOpen) {
    if (current && !startupPopupOpen && !EtomoDirector.INSTANCE.getArguments().isTest()) {
      if (visibleProperty.is() && !frame.isVisible()) {
        show();
      }
    }
    else if (frame.isVisible()) {
      // LogProperties.visible are being kept up to date, so just hide the frame. When
      // the manager is made visible again, the log window will be made visible too.

      // For test don't update the visibility property.
      frame.setVisible(false);
    }
  }

  /**
   * Makes the window visible.  Use this function instead of calling
   * JFrame.setVisible(true) directly.  Updates LogProperties.visible.  Sets the
   * location and size the first time it is made visible.
   * @param visible
   */
  public void show() {
    try {
      Thread.sleep(50);
    }
    catch (InterruptedException e) {
    }
    if (!displayed) {
      displayed = true;
      if (!frameLocationXProperty.isNull()) {
        frame.setLocation(frameLocationXProperty.getInt(),
            frameLocationYProperty.getInt());
      }
      else {
        frame.setLocationByPlatform(true);
      }
      frame.setSize(new Dimension(frameSizeWidthProperty.getInt(),
          frameSizeHeightProperty.getInt()));
    }
    frame.setVisible(true);
    visibleProperty.set(true);
  }

  /**
   * Makes the window invisible.  Updates LogProperties.visible.  Do not use this when
   * hiding the log because the manager interface is being hidden, because the properties
   * should not be updated in that case.
   */
  private void hide() {
    frame.setVisible(false);
    visibleProperty.set(false);
  }

  public void showHide() {
    if (!frame.isVisible()) {
      show();
    }
    else {
      hide();
    }
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
              List<String> lineList = new ArrayList<String>();
              while (line != null) {
                lineList.add(line);
                line = file.readLine(readerId);
              }
              logger.loadMessages(lineList);
              file.closeRead(readerId);
            }
            catch (LogFile.LockException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog((BaseManager) null, "Unabled to load "
                  + file.getAbsolutePath(), "System Error");
            }
            catch (IOException e) {
              e.printStackTrace();
              UIHarness.INSTANCE.openMessageDialog((BaseManager) null, "Unabled to load "
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
    frame.setTitle(border.getTitle());
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
      if (!file.isBackedup()) {
        file.doubleBackupOnce();
      }
      else {
        file.backup();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    LogFile.WriterId writerId = null;
    try {
      writerId = file.openWriter();
      changed = false;
      String text = textArea.getText();
      // LogFile.newLine() will put the appropriate line endings in the file.
      // Strip the line endings by breaking up the text by \n. Strip windows
      // line endings (\r\n) by removing the \r which, if it is in use, will now
      // be at the end of each line. This also adds a new line to the end of the
      // file, if there is not one already there. This should also preserve
      // empty lines.
      String[] lineArray = text.split("\n");
      if (lineArray != null) {
        for (int i = 0; i < lineArray.length; i++) {
          // Preserve an empty line by calling newLine.
          if (lineArray[i] != null && lineArray[i].length() != 0) {
            // Look for Windows line ending.
            if (lineArray[i].charAt(lineArray[i].length() - 1) == '\r') {
              // Preserve an empty line by calling newLine.
              if (lineArray[i].length() > 1) {
                // Write a line which has a Windows line ending (strip \r).
                file.write(lineArray[i].substring(0, lineArray[i].length() - 2), writerId);
              }
            }
            else {
              // Write a line which has a Linux line ending.
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
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
            "Unabled to write to file " + file.getAbsolutePath(), "System Error");
        if (writerId != null && !writerId.isEmpty()) {
          file.closeWriter(writerId);
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      if (!writeFailed) {
        writeFailed = true;
        UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
            "Unabled to write to file " + file.getAbsolutePath(), "System Error");
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
    // Can't open log until the dataset and userDir is set
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
          UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
              "Unabled to access file " + fileName + " in " + userDir, "System Error");
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

  public void logMessage(String title, AxisID axisID, List<String> message) {
    logger.logMessage(title, axisID, message);
  }

  public void logMessage(String title, AxisID axisID) {
    logger.logMessage(title, axisID);
  }

  public void logMessage(final String message) {
    logger.logMessage(message);
  }

  public void logMessage(final File file) {
    logger.logMessage(file);
  }

  public void msgChanged() {
    changed = true;
  }

  JPanel getRootPanel() {
    return rootPanel;
  }

  public void append(String line) {
    textArea.append(line);
  }

  public int getLineEndOffset() throws BadLocationException {
    return textArea.getLineEndOffset(textArea.getLineCount() - 1);
  }

  public void store(final Properties props, String prepend) {
    prepend = getPrepend(prepend);
    Dimension size = frame.getSize();
    // A panel that has never been displayed will have a height and width of 0.
    if (size.width <= 0 || size.height <= 0) {
      return;
    }
    // update properties
    frameSizeWidthProperty.set(size.width);
    frameSizeHeightProperty.set(size.height);
    Point point = frame.getLocation();
    frameLocationXProperty.set(point.x);
    frameLocationYProperty.set(point.y);
    // store
    frameSizeWidthProperty.store(props, prepend);
    frameSizeHeightProperty.store(props, prepend);
    frameLocationXProperty.store(props, prepend);
    frameLocationYProperty.store(props, prepend);
    visibleProperty.store(props, prepend);
  }

  public void load(final Properties props, String prepend) {
    // reset
    frameSizeWidthProperty.reset();
    frameSizeHeightProperty.reset();
    frameLocationXProperty.reset();
    frameLocationYProperty.reset();
    visibleProperty.set(VISIBLE_DEFAULT);
    // load
    prepend = getPrepend(prepend);
    frameSizeWidthProperty.load(props, prepend);
    frameSizeHeightProperty.load(props, prepend);
    frameLocationXProperty.load(props, prepend);
    frameLocationYProperty.load(props, prepend);
    visibleProperty.load(props, prepend, VISIBLE_DEFAULT);
  }

  private String getPrepend(String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      prepend = PREPEND;
    }
    else {
      prepend += "." + PREPEND;
    }
    return prepend;
  }

  private static final class LogWindowKeyListener implements KeyListener {
    private final LogWindow adaptee;

    private LogWindowKeyListener(LogWindow adaptee) {
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

  private static final class LogFrame extends JFrame {
    private final JMenuItem menuHide;

    private LogFrame(final JMenuItem menuHide) {
      this.menuHide = menuHide;
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    }

    /**
     * Overridden so we can hide when window is closed
     */
    protected void processWindowEvent(WindowEvent event) {
      super.processWindowEvent(event);
      if (event.getID() == WindowEvent.WINDOW_CLOSING) {
        menuHide.doClick();
      }
    }
  }

  private static final class MenuActionListener implements ActionListener {
    private LogWindow window;

    private MenuActionListener(final LogWindow window) {
      this.window = window;
    }

    public void actionPerformed(final ActionEvent event) {
      window.action(event.getActionCommand());
    }
  }
}