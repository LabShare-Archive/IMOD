package etomo.ui;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
final class HighlighterButton {
  public static final String rcsid = "$Id$";

  private static final HashedLists groupLists = new HashedLists();

  private final Highlightable parent;
  private final Highlightable group;
  
  //fields initialized once in init()
  private boolean initialized = false;
  private HeaderCell cell;

  /**
   * Lazy constructor
   * @param parent
   * @param group
   */
  HighlighterButton(Highlightable parent, Highlightable group) {
    this.parent = parent;
    this.group = group;
  }

  boolean isHighlighted() {
    init();
    return cell.isSelected();
  }

  int getWidth() {
    init();
    return cell.getWidth();
  }

  void setToolTipText(String text) {
    init();
    cell.setToolTipText(text);
  }

  void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    init();
    cell.add(panel, layout, constraints);
  }

  void remove() {
    init();
    cell.remove();
  }

  void action() {
    init();
    boolean highlight = cell.isSelected();
    parent.highlight(highlight);
    if (group == null) {
      return;
    }
    //If turning on the highlight, all other highlighters in the group must be
    //turned off
    ArrayList list = groupLists.get(group);
    if (list == null) {
      throw new IllegalStateException("Should be in the list.  group=" + group);
    }
    for (int i = 0; i < list.size(); i++) {
      HighlighterButton highlighterButton = (HighlighterButton) list.get(i);
      if (highlighterButton != this) {
        highlighterButton.turnOffHighlight();
      }
    }
    //The group may also need to respond to the highlight
    group.highlight(highlight);
    if (!highlight) {
      return;
    }
  }

  protected final Component getComponent() {
    init();
    return cell.getComponent();
  }

  protected void setForeground() {
  }

  private void init() {
    if (initialized) {
      return;
    }
    initialized = true;
    //group
    if (group != null) {
      groupLists.put(group, this);
    }
    //button
    cell = HeaderCell.getToggleInstance("=>", (int) (40 * UIParameters.INSTANCE
        .getFontSizeAdjustment()));
    cell.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    cell.setEnabled(true);
    //Dimension size = button.getPreferredSize();
    //size.width = (int) (40 * UIParameters.INSTANCE.getFontSizeAdjustment());
    //cell.setSize(size);
    //action
    cell.addActionListener(new HBActionListener(this));
  }

  private void turnOffHighlight() {
    cell.setSelected(false);
    parent.highlight(false);
  }

  private static final class HBActionListener implements ActionListener {
    private final HighlighterButton highlighterButton;

    HBActionListener(HighlighterButton highlighterButton) {
      this.highlighterButton = highlighterButton;
    }

    public void actionPerformed(ActionEvent actionEvent) {
      highlighterButton.action();
    }
  }

  private static final class HashedLists {
    //fields initialized once in init()
    private boolean initialized = false;
    private Hashtable hash = new Hashtable();

    void put(Object groupKey, HighlighterButton element) {
      init();
      ArrayList list = (ArrayList) hash.get(groupKey);
      if (list == null) {
        list = new ArrayList();
        list.add(element);
        hash.put(groupKey, list);
      }
      else {
        list.add(element);
      }
    }

    ArrayList get(Object groupKey) {
      init();
      return (ArrayList) hash.get(groupKey);
    }

    private void init() {
      if (initialized) {
        return;
      }
      initialized = true;
      hash = new Hashtable();
    }
  }
}
