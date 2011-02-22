package etomo.ui.swing;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.border.BevelBorder;

/**
 * <p>Description: Panel that holds paging buttons.  May also receive display
 * Components and define key actions on them.  Passes button and key action
 * paging commands on to Viewport.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of ComponentColorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2008/10/07 16:43:13  sueh
 * <p> bug# 1113 Improved named - changed parent... to focusableParent....
 * <p>
 * <p> Revision 1.2  2008/10/06 22:40:03  sueh
 * <p> bug# 1113 Changed tooltips to reflect whether hotkeys are used.
 * <p>
 * <p> Revision 1.1  2008/09/30 22:00:03  sueh
 * <p> bug# 1113 Added a panel to hold paging buttons and set hotkeys for
 * <p> paging.
 * <p> </p>
 */

final class PagingPanel {
  public static final String rcsid = "$Id$";

  private final JPanel rootPanel = new JPanel();
  private final MultiLineButton btnHome = new MultiLineButton();
  private final MultiLineButton btnPageUp = new MultiLineButton();
  private final MultiLineButton btnUp = new MultiLineButton();
  private final MultiLineButton btnDown = new MultiLineButton();
  private final MultiLineButton btnPageDown = new MultiLineButton();
  private final MultiLineButton btnEnd = new MultiLineButton();

  private final Viewport viewport;
  private final JComponent focusableParent1;
  private final JComponent focusableParent2;
  private final JComponent focusableParent3;
  private final String uniqueKey;

  private PagingPanel(final Viewport viewport, final JComponent focusableParent1,
      final JComponent focusableParent2, final JComponent focusableParent3,
      final String uniqueKey) {
    this.viewport = viewport;
    this.focusableParent1 = focusableParent1;
    this.focusableParent2 = focusableParent2;
    this.focusableParent3 = focusableParent3;
    this.uniqueKey = uniqueKey;
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(btnHome.getComponent());
    rootPanel.add(btnPageUp.getComponent());
    rootPanel.add(btnUp.getComponent());
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(btnDown.getComponent());
    rootPanel.add(btnPageDown.getComponent());
    rootPanel.add(btnEnd.getComponent());
  }

  /**
   * Get PagingPanel instance with hotkey connections to 0 to three
   * JComponents.  The JComponents can be the panel that the table has been
   * placed on.  The uniqueKey uniquely identifies the table.  This is necessary
   * when two tables appear on the same JPanel, so that separate actions are
   * stored (no sure if this works yet).
   * @param viewer
   * @param parent1
   * @param parent2
   * @param uniqueKey
   * @return
   */
  static PagingPanel getInstance(final Viewport viewport,
      final JComponent focusableParent1, final JComponent focusableParent2,
      final JComponent focusableParent3, final String uniqueKey) {
    PagingPanel instance = new PagingPanel(viewport, focusableParent1, focusableParent2,
        focusableParent3, uniqueKey);
    instance.init(focusableParent1 != null || focusableParent2 != null
        || focusableParent3 != null);
    instance.addListeners();
    return instance;
  }

  private void init(boolean hotkeys) {
    setupButton(btnHome, "home.png", "Top of table");
    setupButton(btnPageUp, "pageUp.png", "Page up" + (hotkeys ? " - Page Up button" : ""));
    setupButton(btnUp, "up.png", "Up one line" + (hotkeys ? " - Up Arrow button" : ""));
    setupButton(btnDown, "down.png", "Down one line"
        + (hotkeys ? " - Down Arrow button" : ""));
    setupButton(btnPageDown, "pageDown.png", "Page down"
        + (hotkeys ? " - Page Down button" : ""));
    setupButton(btnEnd, "end.png", "Bottom of table");
  }

  private void setupButton(final MultiLineButton button, final String iconFile,
      final String tooltip) {
    button.setManualName();
    button.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    button.setIcon(new ImageIcon(ClassLoader.getSystemResource("images/" + iconFile)));
    Dimension size = button.getPreferredSize();
    if (size.width < size.height) {
      size.width = size.height;
    }
    button.setSize(size);
    button.setToolTipText(tooltip);
  }

  /**
   * Optionally add a key stroke action for one parent.
   * @param inputMap1
   * @param actionMap1
   * @param inputMap2
   * @param actionMap2
   * @param key
   */
  private void addAction(final InputMap inputMap, final ActionMap actionMap,
      final int keyStroke, final String key, final AbstractAction action) {
    if (inputMap != null) {
      inputMap.put(KeyStroke.getKeyStroke(keyStroke, 0), key);
      actionMap.put(key, action);
    }
  }

  private void addListeners() {
    InputMap inputMap1 = null;
    ActionMap actionMap1 = null;
    InputMap inputMap2 = null;
    ActionMap actionMap2 = null;
    InputMap inputMap3 = null;
    ActionMap actionMap3 = null;
    if (focusableParent1 != null) {
      inputMap1 = focusableParent1
          .getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
      actionMap1 = focusableParent1.getActionMap();
    }
    if (focusableParent2 != null) {
      inputMap2 = focusableParent2
          .getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
      actionMap2 = focusableParent2.getActionMap();
    }
    if (focusableParent3 != null) {
      inputMap3 = focusableParent3
          .getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
      actionMap3 = focusableParent3.getActionMap();
    }
    //home
    btnHome.addActionListener(new PagingPanelHomeListener(viewport));
    //page up
    AbstractAction action = new PagingPanelPageUpAction(viewport);
    addAction(inputMap1, actionMap1, KeyEvent.VK_PAGE_UP, uniqueKey + "PAGE_UP", action);
    addAction(inputMap2, actionMap2, KeyEvent.VK_PAGE_UP, uniqueKey + "PAGE_UP", action);
    addAction(inputMap3, actionMap3, KeyEvent.VK_PAGE_UP, uniqueKey + "PAGE_UP", action);
    btnPageUp.addActionListener(action);
    //up
    action = new PagingPanelUpAction(viewport);
    addAction(inputMap1, actionMap1, KeyEvent.VK_UP, uniqueKey + "UP", action);
    addAction(inputMap2, actionMap2, KeyEvent.VK_UP, uniqueKey + "UP", action);
    addAction(inputMap3, actionMap3, KeyEvent.VK_UP, uniqueKey + "UP", action);
    btnUp.addActionListener(action);
    //down
    action = new PagingPanelDownAction(viewport);
    addAction(inputMap1, actionMap1, KeyEvent.VK_DOWN, uniqueKey + "DOWN", action);
    addAction(inputMap2, actionMap2, KeyEvent.VK_DOWN, uniqueKey + "DOWN", action);
    addAction(inputMap3, actionMap3, KeyEvent.VK_DOWN, uniqueKey + "DOWN", action);
    btnDown.addActionListener(action);
    //page down
    action = new PagingPanelPageDownAction(viewport);
    addAction(inputMap1, actionMap1, KeyEvent.VK_PAGE_DOWN, uniqueKey + "PAGE_DOWN",
        action);
    addAction(inputMap2, actionMap2, KeyEvent.VK_PAGE_DOWN, uniqueKey + "PAGE_DOWN",
        action);
    addAction(inputMap3, actionMap3, KeyEvent.VK_PAGE_DOWN, uniqueKey + "PAGE_DOWN",
        action);
    btnPageDown.addActionListener(action);
    //end
    btnEnd.addActionListener(new PagingPanelEndListener(viewport));
  }

  Container getContainer() {
    return rootPanel;
  }

  void setUpEnabled(final boolean enabled) {
    btnHome.setEnabled(enabled);
    btnPageUp.setEnabled(enabled);
    btnUp.setEnabled(enabled);
  }

  void setDownEnabled(final boolean enabled) {
    btnEnd.setEnabled(enabled);
    btnPageDown.setEnabled(enabled);
    btnDown.setEnabled(enabled);
  }

  void setVisible(final boolean visible) {
    rootPanel.setVisible(visible);
  }

  private static final class PagingPanelHomeListener implements ActionListener {
    private final Viewport viewport;

    private PagingPanelHomeListener(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.homeButtonAction();
    }
  }

  private static final class PagingPanelPageUpAction extends AbstractAction {
    private final Viewport viewport;

    private PagingPanelPageUpAction(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.pageUpButtonAction();
    }
  }

  private static final class PagingPanelUpAction extends AbstractAction {
    private final Viewport viewport;

    private PagingPanelUpAction(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.upButtonAction();
    }
  }

  private static final class PagingPanelDownAction extends AbstractAction {
    private final Viewport viewport;

    private PagingPanelDownAction(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.downButtonAction();
    }
  }

  private static final class PagingPanelPageDownAction extends AbstractAction {
    private final Viewport viewport;

    private PagingPanelPageDownAction(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.pageDownButtonAction();
    }
  }

  private static final class PagingPanelEndListener implements ActionListener {
    private final Viewport viewport;

    private PagingPanelEndListener(final Viewport viewport) {
      this.viewport = viewport;
    }

    public void actionPerformed(final ActionEvent event) {
      viewport.endButtonAction();
    }
  }
}
