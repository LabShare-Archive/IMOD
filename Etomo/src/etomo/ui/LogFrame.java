package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

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
final class LogFrame extends JFrame {
  public static final String rcsid = "$Id$";

  private LogPanel curLogPanel = null;
  private boolean hide = false;

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
   * Get, pack, and display a logPanel.  
   * @param logPanel
   */
  void setPanel(LogPanel logPanel) {
    Container contentPane = getContentPane();
    contentPane.removeAll();
    if (curLogPanel != null) {
      //the log panel needs to remember its size and location
      curLogPanel.setFrameSize(getSize());
      curLogPanel.setFrameLocation(getLocation());
    }
    curLogPanel = logPanel;
    if (curLogPanel != null) {
      UIHarness.INSTANCE.setEnabledLogWindowMenuItem(true);
      contentPane.add(curLogPanel.getRootPanel());
      setTitle(curLogPanel.getTitle());
      //get the last size and location for this log panel
      Dimension size = curLogPanel.getFrameSize();
      if (size != null) {
        setPreferredSize(size);
      }
      Point location = curLogPanel.getFrameLocation();
      if (location != null) {
        setLocation(location);
      }
      setVisible(true);
    }
    else {
      UIHarness.INSTANCE.setEnabledLogWindowMenuItem(false);
      setVisible(false);
    }
  }

  void showHideFrame() {
    hide = !hide;
    setVisible(!hide);
  }

  public void hideFrame() {
    hide = true;
    setVisible(!hide);
  }

  /**
   * Message sent by LogPanel when it has changed.  If LogFrame does not
   * currently contain this logPanel instance, ignore the message.
   * @param ready
   * @param logPanel
   */
  void msgChanged(LogPanel logPanel) {
    if (curLogPanel == logPanel) {
      setTitle(curLogPanel.getTitle());
      repaint();
      pack();
    }
  }

  public void setVisible(boolean visible) {
    visible = visible && !hide;
    if (visible) {
      repaint();
      pack();
    }
    super.setVisible(visible);
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
      hideFrame();
    }
    if (menuFitWindow.getActionCommand().equals(actionCommand)) {
      pack();
      repaint();
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
