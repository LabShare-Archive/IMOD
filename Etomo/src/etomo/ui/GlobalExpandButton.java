package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * <p>Description: Dialog level expand button.  Used for the dialog level
 * advanced/basic button</p>
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
final class GlobalExpandButton {
  private final MultiLineButton button;
  private final String contractedLabel;
  private final String expandedLabel;

  private List expandButtonList = null;
  private List expandableList = null;

  private boolean expanded = false;

  private GlobalExpandButton(String contractedLabel, String expandedLabel) {
    this.contractedLabel = contractedLabel;
    this.expandedLabel = expandedLabel;
    button = new MultiLineButton(contractedLabel);
  }

  static GlobalExpandButton getInstance(String contractedLabel,
      String expandedLabel) {
    GlobalExpandButton instance = new GlobalExpandButton(contractedLabel,
        expandedLabel);
    instance.setListeners();
    return instance;
  }

  private void setListeners() {
    button.addActionListener(new GlobalExpandButtonActionListener(this));
  }

  void register(ExpandButton expandButton) {
    if (expandButtonList == null) {
      expandButtonList = new ArrayList();
    }
    expandButtonList.add(expandButton);
  }

  void register(Expandable expandable) {
    if (expandableList == null) {
      expandableList = new ArrayList();
    }
    expandableList.add(expandable);
  }

  Component getComponent() {
    return button.getComponent();
  }

  boolean isExpanded() {
    return expanded;
  }

  void setVisible(boolean visible) {
    button.setVisible(visible);
  }

  void setToolTipText(String tooltip) {
    button.setToolTipText(tooltip);
  }

  /**
   * Used to change the state of this button when all registered objects have
   * been changed to the opposite state by other buttons.
   * Message received when a registered expand button changes state.  When all
   * registered expand buttons have the same state, and that state is different
   * from this button's state, change the state of this button and notify its
   * list of Expandables.
   * @param activeExpandButton
   * @param expanded
   */
  void msgExpandButtonAction(ExpandButton activeExpandButton, boolean isExpanded) {
    if (expanded == isExpanded || expandableList != null
        || expandButtonList == null) {
      //Same state as this button - nothing to do; or some advanced items only
      //appear when the global button is used, so the dialog can't be completely
      //expanded using ExpandButtons.
      return;
    }
    Iterator iterator = expandButtonList.iterator();
    while (iterator.hasNext()) {
      ExpandButton expandButton = (ExpandButton) iterator.next();
      if (expandButton != activeExpandButton) {
        if (expandButton.isExpanded() == expanded) {
          //Not all the expand buttons are a different state as this button
          // - don't change this button.
          return;
        }
      }
    }
    //All expand buttons have the state that is opposite to this button's state.
    //And all expandable fields in the dialog are controlled by expand buttons.
    //So change this button.
    toggleState();
  }

  /**
   * Toggle state.  Does't cause lists to expand.
   */
  private void toggleState() {
    if (expanded) {
      expanded = false;
      button.setText(contractedLabel);
    }
    else {
      expanded = true;
      button.setText(expandedLabel);
    }
  }

  /**
   * Change state to expanded.  Doesn't cause lists to expand.
   * @param expanded
   */
  void changeState(boolean expanded) {
    this.expanded = expanded;
    if (expanded) {
      button.setText(expandedLabel);
    }
    else {
      button.setText(contractedLabel);
    }
  }

  /**
   * Change the state of this button and notify lists.  If notifyExpandButtons
   * is false then action was called by msgExpandButtonAction, so the expand
   * buttons are already correct.
   * @param notifyExpandButtons
   */
  private void action() {
    toggleState();
    //This button was pressed.
    //Tell objects controlled by this button to expand.
    if (expandableList != null) {
      Iterator iterator = expandableList.iterator();
      while (iterator.hasNext()) {
        ((Expandable) iterator.next()).expand(this);
      }
    }
    //Tell registered expand buttons to expand.
    if (expandButtonList != null) {
      Iterator iterator = expandButtonList.iterator();
      while (iterator.hasNext()) {
        ((ExpandButton) iterator.next()).setExpanded(expanded);
      }
    }
  }

  private static final class GlobalExpandButtonActionListener implements
      ActionListener {

    private final GlobalExpandButton adaptee;

    private GlobalExpandButtonActionListener(final GlobalExpandButton adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }
}
