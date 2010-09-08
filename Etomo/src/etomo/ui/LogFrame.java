package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import etomo.EtomoDirector;
import etomo.type.ConstLogProperties;

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
 * <p> Revision 1.4  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.3  2009/09/29 00:05:51  sueh
 * <p> bug# 1228 Put all calls to repaint and pack in one function, so all the
 * <p> originations of these calls can be traced.
 * <p>
 * <p> Revision 1.2  2009/03/05 23:28:43  sueh
 * <p> bug# 1194 Get the LogProperties from either the LogPanel or user config.
 * <p>
 * <p> Revision 1.1  2009/02/04 23:34:34  sueh
 * <p> bug# 1158 Frame for the log window.  Hold the log window for the
 * <p> current manager.  Can be fitted independently of the rest of Etomo.
 * <p> Stores its current size and location in each log window.  Can be hidden
 * <p> without affecting automatic logging.
 * <p> </p>
 */
final class LogFrame extends JFrame {
  public static final String rcsid = "$Id$";

  private LogPanel curLogPanel = null;

  private final JMenuBar menuBar = new JMenuBar();
  private final JMenu menuFile = new Menu("File");
  private final JMenuItem menuSave = new MenuItem(LogPanel.SAVE_LABEL,
      KeyEvent.VK_S);

  private final JMenu menuView = new Menu("View");
  private final JMenuItem menuLogWindow = new MenuItem(
      LogPanel.LOG_WINDOW_LABEL, KeyEvent.VK_L);
  private final JMenuItem menuFitWindow = new MenuItem("Fit Log Window",
      KeyEvent.VK_F);

  private LogFrame() {
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    //menu
    setJMenuBar(menuBar);
    menuBar.add(menuFile);
    menuBar.add(menuView);
    menuFile.add(menuSave);
    menuView.add(menuLogWindow);
    menuView.add(menuFitWindow);
  }

  static LogFrame getInstance() {
    LogFrame instance = new LogFrame();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    //  Mnemonics for the main menu bar
    menuFile.setMnemonic(KeyEvent.VK_F);
    menuView.setMnemonic(KeyEvent.VK_V);

    //  Accelerators
    menuLogWindow.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
        ActionEvent.CTRL_MASK));
    menuFitWindow.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
        ActionEvent.CTRL_MASK));

    //  Bind the menu items to their listeners
    MenuActionListener actionListener = new MenuActionListener(this);
    menuSave.addActionListener(actionListener);
    menuLogWindow.addActionListener(actionListener);
    menuFitWindow.addActionListener(actionListener);
  }

  /**
   * Get a LogProperties instance filled with values from the current state of
   * the frame.
   * @return
   */
  private ConstLogProperties getProperties() {
    LogProperties properties = new LogProperties();
    properties.setFrameSize(getSize());
    properties.setFrameLocation(getLocation());
    return properties;
  }

  /**
   * Gets and displays a new log panel.  Saves the current frame properties in
   * the old log panel.  Changes the frame to match the frame properties in the
   * new log panel.
   * @param logPanel
   */
  void setPanel(LogPanel logPanel) {
    if (logPanel==null) {
      return;
    }
    Container contentPane = getContentPane();
    contentPane.removeAll();
    //the old log panel needs to remember its size and location
    if (curLogPanel != null) {
      //Save the current state in the old log panel
      curLogPanel.setFrameProperties(getProperties());
      curLogPanel.setFrameVisible(isVisible());
    }
    //Set the new panel
    curLogPanel = logPanel;
    if (curLogPanel != null) {
      UIHarness.INSTANCE.setEnabledLogWindowMenuItem(true);
      contentPane.add(curLogPanel.getRootPanel());
      setTitle(curLogPanel.getTitle());
      //get the last size and location for this log panel
      ConstLogProperties properties = curLogPanel.getFrameProperties();
      if (properties == null) {
        properties = EtomoDirector.INSTANCE.getUserConfiguration()
            .getLogProperties();
      }
      if (properties == null) {
        properties = new LogProperties();
      }
      setPreferredSize(properties.getFrameSize());
      setLocation(properties.getFrameLocationX(), properties
          .getFrameLocationY());
      setVisible(curLogPanel.isFrameVisible());
      if (isVisible()) {
        refresh();
      }
    }
    else {
      //This happens when exiting
      UIHarness.INSTANCE.setEnabledLogWindowMenuItem(false);
      setVisible(false);
    }
  }
  
  private void refresh() {
    repaint();
    pack();
  }
  
  /**
   * Toggles setVisible().
   */
  void showHide() {
    setVisible(!isVisible());
    if (isVisible()) {
      refresh();
    }
  }

  /**
   * Hides the frame.
   *
   */
  private void hideLogFrame() {
    setVisible(false);
  }

  /**
   * Message sent by LogPanel when it has changed.  If LogFrame does not
   * currently contain this logPanel instance, the message is ignored.
   * @param ready
   * @param logPanel
   */
  void msgChanged(LogPanel logPanel) {
    if (curLogPanel == logPanel) {
      setTitle(curLogPanel.getTitle());
      //refresh();
      repaint();
    }
  }

  /**
   * Message sent by LogPanel when its needs to have the current frame
   * properties.  If LogFrame does not currently contain this logPanel instance,
   * the message is ignored.
   * @param ready
   * @param logPanel
   */
  void msgUpdateProperties(LogPanel logPanel) {
    if (curLogPanel == logPanel) {
      curLogPanel.setFrameProperties(getProperties());
    }
  }

  /**
   * Overridden so we can hide when window is closed
   */
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING) {
      menuLogWindow.doClick();
    }
  }

  /**
   * Handle menu actions
   * @param event
   */
  void menuAction(ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (menuSave.getActionCommand().equals(actionCommand)) {
      if (curLogPanel != null) {
        curLogPanel.menuSave();
      }
    }
    else if (menuLogWindow.getActionCommand().equals(actionCommand)) {
      hideLogFrame();
    }
    if (menuFitWindow.getActionCommand().equals(actionCommand)) {
      //Getting rid of preferred size lets the frame fit to original size of the
      //text area.
      setPreferredSize(null);
      refresh();
    }
  }

  //  File menu action listener
  private static final class MenuActionListener implements ActionListener {
    private LogFrame adaptee;

    private MenuActionListener(final LogFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.menuAction(event);
    }
  }
}
