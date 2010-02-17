package etomo.ui;

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
 * <p> $Log$ </p>
 */
public final class ManagerFrame extends AbstractFrame {
  public static final String rcsid = "$Id$";

  public static final String NAME = "manager-frame";

  private final EtomoMenu menu = new EtomoMenu(true);

  private final BaseManager manager;
  private final JPanel rootPanel;

  private ManagerFrame(BaseManager manager) {
    this.manager = manager;
    rootPanel = (JPanel) getContentPane();
  }

  static ManagerFrame getInstance(BaseManager manager) {
    ManagerFrame instance = new ManagerFrame(manager);
    instance.initialize();
    instance.addListeners();
    return instance;
  }

  private void initialize() {
    if (manager == null) {
      throw new NullPointerException("manager is null");
    }
    //set name
    String name = Utilities.convertLabelToName(NAME);
    rootPanel.setName(UITestFieldType.PANEL.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.PANEL.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
    ImageIcon iconEtomo = new ImageIcon(ClassLoader
        .getSystemResource("images/etomo.png"));
    setIconImage(iconEtomo.getImage());
    menu.createMenus(this);
    setJMenuBar(menu.getMenuBar());
    setTitle(manager.getName());
    rootPanel.add(manager.getMainPanel());
    rootPanel.repaint();
    setVisible(true);
  }

  private void addListeners() {
    addWindowFocusListener(new ManagerWindowFocusListener(manager));
  }

  public LogFrame getLogFrame() {
    return null;
  }

  public void menuFileAction(ActionEvent event) {
  }

  /**
   * Handle some of the view menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  public void menuViewAction(ActionEvent event) {
    //Run fitWindow on both frames.
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

  private static final class ManagerWindowFocusListener implements
      WindowFocusListener {
    private final BaseManager manager;

    private ManagerWindowFocusListener(BaseManager manager) {
      this.manager = manager;
    }

    public void windowGainedFocus(WindowEvent e) {
      manager.makeCurrent();
    }

    public void windowLostFocus(WindowEvent e) {
    }
  }
}
