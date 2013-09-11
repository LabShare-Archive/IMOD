package etomo.ui.swing;

import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.FrameType;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.2  2011/02/22 18:15:10  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2010/02/17 04:55:20  sueh
 * <p> bug# 1301 Independent frame associated with a single manager.
 * <p> </p>
 */
public final class ManagerFrame extends AbstractFrame {
  public static final String rcsid = "$Id$";

  public static final String NAME = "manager-frame";

  private final EtomoMenu menu;

  private final BaseManager manager;
  private final JPanel rootPanel;
  private final boolean savable;

  private ManagerFrame(final BaseManager manager, final boolean savable) {
    this.manager = manager;
    this.savable = savable;
    rootPanel = (JPanel) getContentPane();
    menu = EtomoMenu.getInstance(this, savable);
  }

  static ManagerFrame getInstance(final BaseManager manager, final boolean savable) {
    ManagerFrame instance = new ManagerFrame(manager, savable);
    instance.initialize();
    instance.addListeners();
    return instance;
  }

  private void initialize() {
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    if (manager == null) {
      throw new NullPointerException("manager is null");
    }
    // set name
    String name = Utilities.convertLabelToName(NAME);
    rootPanel.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.PANEL.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
    ImageIcon iconEtomo = new ImageIcon(ClassLoader.getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());
    setJMenuBar(menu.getMenuBar());
    setTitle(manager.getName());
    rootPanel.add(manager.getMainPanel());
    rootPanel.repaint();
    setVisible(true);
  }

  public FrameType getFrameType() {
    return null;
  }

  private void addListeners() {
    addWindowFocusListener(new ManagerWindowFocusListener(manager));
  }

  public void menuFileAction(ActionEvent event) {
    menu.menuFileAction(event);
  }

  void save(AxisID axisID) {
    if (savable) {
      manager.saveToFile();
    }
  }

  void saveAs() {
    if (savable) {
      manager.saveAsToFile();
    }
  }

  void cancel() {
    setVisible(false);
    dispose();
  }

  void close() {
    if (manager.closeFrame()) {
      setVisible(false);
      dispose();
    }
  }

  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING
        && !EtomoDirector.INSTANCE.getArguments().isTest()) {
      close();
    }
  }

  /**
   * Handle some of the view menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  public void menuViewAction(ActionEvent event) {
    // Run fitWindow on both frames.
    if (menu.equalsFitWindow(event)) {
      UIHarness.INSTANCE.pack(true, manager);
    }
    else {
      throw new IllegalStateException(
          "Cannot handled menu command in this class.  command="
              + event.getActionCommand());
    }
  }

  public void pack(boolean force) {
    if (!force && !EtomoDirector.INSTANCE.getUserConfiguration().isAutoFit()) {
      setVisible(true);
    }
    else {
      Rectangle bounds = getBounds();
      bounds.height++;
      bounds.width++;
      setBounds(bounds);
      try {
        super.pack();
      }
      catch (NullPointerException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * Handle some of the options menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  public void menuOptionsAction(ActionEvent event) {
    if (menu.equalsSettings(event)) {
      EtomoDirector.INSTANCE.openSettingsDialog();
    }
    else {
      throw new IllegalStateException(
          "Cannot handled menu command in this class.  command="
              + event.getActionCommand());
    }
  }

  public void menuToolsAction(ActionEvent event) {
    menu.menuToolsAction(AxisID.ONLY, event);
  }

  /**
   * Handle help menu actions
   * @param event
   */
  public void menuHelpAction(ActionEvent event) {
    menu.menuHelpAction(manager, AxisID.ONLY, this, event);
  }

  private static final class ManagerWindowFocusListener implements WindowFocusListener {
    private final BaseManager manager;

    private ManagerWindowFocusListener(BaseManager manager) {
      this.manager = manager;
    }

    public void windowGainedFocus(WindowEvent e) {
      manager.makePropertyUserDirLocal();
    }

    public void windowLostFocus(WindowEvent e) {
    }
  }
}
