package etomo.ui.swing;

import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.FrameType;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class SubFrame extends EtomoFrame {
  public static final String rcsid = "$Id$";

  public static final String NAME = "sub-frame";

  private final MainFrame mainFrame;
  private JPanel rootPanel;
  private JLabel statusBar;

  SubFrame(MainFrame mainFrame) {
    register();
    this.mainFrame = mainFrame;
  }

  final synchronized void register() {
    if (subFrame != null) {
      throw new IllegalStateException("Only one instance of SubFrame is allowed.");
    }
    subFrame = this;
    main = false;
  }

  /**
   * One-time initialization.  Sets the mainPanel, creates the menus, and
   * displays the axis panel.
   * @param title
   * @param currentManager
   * @param mRUList
   */
  void initialize(String title, BaseManager currentManager, String[] mRUList) {
    super.initialize();
    this.currentManager = currentManager;
    setTitle(title);
    mainPanel = mainFrame.getMainPanel();
    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    // set name
    String name = Utilities.convertLabelToName(NAME);
    rootPanel.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.PANEL.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
    statusBar = new JLabel(mainPanel.getStatusBarText());
    // menu.setEnabled(currentManager);
    menu.setEnabled(getOtherFrame().menu);
    menu.setMRUFileLabels(mRUList);
    setVisible(true);
  }

  final FrameType getFrameType() {
    return FrameType.Sub;
  }

  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING) {
      mainFrame.showAxisA();
    }
  }

  /**
   * Set the main panel when switching managers.
   *
   */
  void setMainPanel(String title, BaseManager currentManager) {
    this.currentManager = currentManager;
    mainPanel = mainFrame.getMainPanel();
    setTitle(title);
    statusBar.setText(mainPanel.getStatusBarText());
  }

  /**
   * Override superclass to call mainFrame for command which require switching
   * axis or handling the log window.
   */
  public void menuViewAction(ActionEvent event) {
    if (menu.equalsAxisA(event) || menu.equalsAxisB(event) || menu.equalsAxisBoth(event)
        || menu.equalsLogWindow(event)) {
      mainFrame.menuViewAction(event);
    }
    else {
      super.menuViewAction(event);
    }
  }

  public void setVisible(boolean visible) {
    if (visible) {
      setAxis();
    }
    super.setVisible(visible);
  }

  /**
   * Refresh or add the axis scroll panel and the status bar, and set the
   * location.
   *
   */
  private void setAxis() {
    rootPanel.removeAll();
    JScrollPane axis = mainPanel.showBothAxis();
    if (axis != null) {
      rootPanel.add(axis, BorderLayout.CENTER);
    }
    rootPanel.add(statusBar, BorderLayout.SOUTH);
    validate();
  }

  void moveSubFrame() {
    Rectangle mainFrameBounds = mainFrame.getBounds();
    Rectangle deviceBounds = mainFrame.getGraphicsConfiguration().getBounds();
    int xLocation = deviceBounds.x + mainFrameBounds.x + mainFrameBounds.width;
    if (xLocation > deviceBounds.x + deviceBounds.width) {
      xLocation = (deviceBounds.x + deviceBounds.width) / 2;
    }
    setLocation(xLocation, deviceBounds.y + mainFrameBounds.y);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 21:37:59  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.13  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.12  2009/11/20 17:36:54  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.11  2009/02/04 23:36:26  sueh
 * <p> bug# 1158 Add a View pull down menu and menu options for the log
 * <p> frame.
 * <p>
 * <p> Revision 1.10  2009/01/20 20:30:16  sueh
 * <p> bug# 1102 Printing out the name of the sub-frame.
 * <p>
 * <p> Revision 1.9  2008/05/30 21:34:28  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.8  2006/04/25 19:21:52  sueh
 * <p> bug# 787 Named the sub frame.
 * <p>
 * <p> Revision 1.7  2005/12/09 20:35:54  sueh
 * <p> bug# 776 In EtomoMenu removed the getActionCommand... functions and
 * <p> replaced them with equals().
 * <p>
 * <p> Revision 1.6  2005/04/27 02:19:03  sueh
 * <p> bug# 615 Preserve the location of SubFrame.  Attempting to use the
 * <p> relative position of SubFrame in the virtual desktop (this doesn't work, at
 * <p> least on Fedora).  Override setVisible(boolean visible) to preserve the frame's
 * <p> location on visible == false and call setAxis() on visible == true.
 * <p>
 * <p> Revision 1.5  2005/04/26 17:41:54  sueh
 * <p> bug# 615 Made MainFrame, SubFrame, and EtomoFrame package-level
 * <p> classes.  All MainFrame functionality is handled through UIHarness to
 * <p> make Etomo more compatible with JUnit.  Fixed function access levels.
 * <p>
 * <p> Revision 1.4  2005/04/25 21:41:09  sueh
 * <p> bug# 615 Moving message dialog functions, menu appearance functions,
 * <p> and fitting and repainting functions from mainPanel and the child frame
 * <p> classes to EtomoFrame.  Added register() to initialize a static instance
 * <p> variable in EtomoFrame.
 * <p>
 * <p> Revision 1.3  2005/04/21 20:54:49  sueh
 * <p> bug# 615 Moved menu handling to EtomoFrame so that SubFrame can
 * <p> respond directly to menu commands.  Added title bar.
 * <p>
 * <p> Revision 1.2  2005/04/20 01:53:04  sueh
 * <p> bug# 615 Overrode pack() to prevent the scrollbar from disappearing.
 * <p> Added a menu (EtomoMenu).  Added action functions.
 * <p>
 * <p> Revision 1.1  2005/04/16 02:04:48  sueh
 * <p> bug# 615 Subordinate frame to hold the B axis when both axis are
 * <p> displayed.
 * <p> </p>
 */
