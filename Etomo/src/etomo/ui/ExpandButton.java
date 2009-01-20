package etomo.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.DialogType;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: Button with a expanded state variable.  When expanded is true,
 * the button displays "<" so that the state can be changed to expanded == false.
 * When expanded is false, the button displays ">" so that the state can be
 * changed to expanded == true.  The button is created with a component which
 * displays the button and implements Expandable.  When the button is pressed,
 * its state is changed and it calls Expandable.expand(ExpandButton).</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.19  2008/05/30 22:31:51  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.18  2008/05/30 21:28:31  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.17  2007/12/26 22:23:44  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.16  2007/09/07 00:26:56  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.15  2007/03/20 00:44:28  sueh
 * <p> bug# 964 Added remove() to remove component from the jpanelContainer which
 * <p> was saved in add().
 * <p>
 * <p> Revision 1.14  2007/03/01 01:33:21  sueh
 * <p> bug# 964 removed unnecesary protected modifier
 * <p>
 * <p> Revision 1.13  2006/04/28 20:58:35  sueh
 * <p> bug# 787 Moved type information (which kind of expander) and strings
 * <p> associated with the type to a static inner class:  Type.
 * <p>
 * <p> Revision 1.12  2006/03/28 17:02:24  sueh
 * <p> bug# 437 Overrode getButtonState() and setButtonState because state
 * <p> in an expand button is associated with expanded/not expanded instead of
 * <p> selected/not selected.
 * <p>
 * <p> Revision 1.11  2006/03/28 00:49:30  sueh
 * <p> bug# 437 Always turn on MultiLineButton.manualName because expander
 * <p> buttons' text is a symbol, not a name.  Override MultiLineButton.
 * <p> createButtonStateKey because expander buttons don't have a done state.
 * <p>
 * <p> Revision 1.10  2005/12/14 20:54:55  sueh
 * <p> bug# 784 Added context sensitive tool tips.
 * <p>
 * <p> Revision 1.9  2005/11/14 22:02:46  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 1.8  2005/09/27 23:28:16  sueh
 * <p> bug# 532 Added a expanded state string and a contracted state string.
 * <p> Added getState() which returns the string respresenting the current state.
 * <p> Added setState() which calls setExpanded() based on a state string.
 * <p>
 * <p> Revision 1.7  2005/09/20 18:35:21  sueh
 * <p> bug# 532 Using static get instance functions instead of the constructor
 * <p> because the options that must be passed to the constructor are getting
 * <p> too complicated.  Made the join expanding button the same size as the
 * <p> other expander buttons.
 * <p>
 * <p> Revision 1.6  2005/08/10 20:42:32  sueh
 * <p> bug# 711 To set a non-standard button size in MultLineButton, call
 * <p> setSize(Dimension).
 * <p>
 * <p> Revision 1.5  2005/08/09 20:21:09  sueh
 * <p> bug# 711  No longer inheriting JButton in MultiLineButton.
 * <p>
 * <p> Revision 1.4  2005/07/11 22:56:16  sueh
 * <p> bug# 619 Handled null container in buttonAction().
 * <p>
 * <p> Revision 1.3  2005/07/06 23:34:57  sueh
 * <p> bug# 619 Removed the member variablesdoubleSpacePanelContainer and
 * <p> jpanelContainer, since they don't seem to be in use.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:52:24  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/13 23:04:32  sueh
 * <p> bug# 520 Added add() functions.  Add() functions remember the panel the
 * <p> button was added to so a remove() can be called without passing the
 * <p> panel.
 * <p>
 * <p> Revision 1.1.2.3  2004/09/22 22:10:30  sueh
 * <p> bug# 520 Inheriting from MultiLineButton so that ExpandButton with
 * <p> disable correctly.  ExpandButton is not a MultiLineButton, but the real
 * <p> problem is that MultiLineButton should have named differently (something
 * <p> like HtmlFormattedButton).
 * <p>
 * <p> Revision 1.1.2.2  2004/09/21 17:57:47  sueh
 * <p> bug# 520 setting a maximum size of the expand button
 * <p>
 * <p> Revision 1.1.2.1  2004/09/17 21:37:21  sueh
 * <p> bug# 520 This class is a button which changes its text from ">" to "<"
 * <p> when pressed.  It remembers its state.  Its has its own listener and
 * <p> tells the component it is displayed on when it has been pressed.
 * <p> </p>
 */
public final class ExpandButton extends MultiLineButton {
  public static final String rcsid = "$Id$";

  private static final Type DEFAULT_TYPE = Type.MORE;

  private final Type type;
  private final Expandable container;

  private boolean expanded;
  private JPanel jpanelContainer = null;

  static ExpandButton getInstance(Expandable container, ExpandButton.Type type) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(container, type);
    return instance;
  }

  static ExpandButton getExpandedInstance(Expandable container,
      ExpandButton.Type type) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(container, type, true);
    return instance;
  }

  /**
   * Create a button with ">" if expanded is false, or "<" expanded is true.
   * Add an action listener and a raised, beveled border to make it 3D.  Set the
   * size so that it is square, unless the width needs to be greater then the
   * height.  The button remembers the expanded state and keeps track of it.
   * The button can be used on any ui.
   * @param component
   */
  private ExpandButton(Expandable container, ExpandButton.Type type) {
    this(container, type, false);
  }

  private ExpandButton(Expandable container, ExpandButton.Type type,
      boolean expanded) {
    super();
    this.container = container;
    this.type = type;
    this.expanded = expanded;
    super.setManualName();
    super.setText(type.getSymbol(expanded));
    setToolTipText(type.getToolTip(expanded));
    addActionListener(new ExpandButtonActionListener(this));
    setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    Dimension size = getPreferredSize();
    if (size.width < size.height) {
      size.width = size.height;
    }
    setSize(size);
  }

  /**
   * Calls the setName function of the button.  Called when the text is set,
   * unless manualName is true.
   * @param label
   */
  void setName(String associatedLabel) {
    String name = Utilities.convertLabelToName(associatedLabel);
    getButton().setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.MINI_BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  /**
   * 
   * @return expanded
   */
  boolean isExpanded() {
    return expanded;
  }

  public static boolean isExpanded(JButton button) {
    return Type.isExpandedSymbol(button.getText());
  }

  /**
   * Overrides MultiLineButton.createButtonStateKey.  Necessary because an
   * expander button doesn't have a done state.
   * @param dialogType
   * @return
   */
  String createButtonStateKey(DialogType dialogType) {
    String stateKey = dialogType.getStorableName() + '.' + getName();
    setStateKey(stateKey);
    return stateKey;
  }

  /**
   * Overrides super class.Returns isExpanded() since the button state is
   * expanded or contracted
   * @return
   */
  boolean getButtonState() {
    return isExpanded();
  }

  void setButtonState(boolean state) {
    setOriginalProcessResultDisplayState(state);
    setExpanded(state);
  }

  String getState() {
    return type.getState(expanded);
  }

  void setState(String state) {
    if (state == null) {
      return;
    }
    if (state.equals(type.getExpandedState()) && !expanded) {
      setExpanded(true);
    }
    else if (state.equals(type.getContractedState()) && expanded) {
      setExpanded(false);
    }
  }

  void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    layout.setConstraints(getComponent(), constraints);
    panel.add(getComponent());
    jpanelContainer = panel;
  }

  void remove() {
    if (jpanelContainer != null) {
      jpanelContainer.remove(getComponent());
      jpanelContainer = null;
    }
  }

  /**
   * Equals may be used by the Expandable component to identify a button.  So
   * equals(ExpandButton) must call Object.equals(Object) so that each button
   * has a unique signature.
   * @param that
   * @return
   */
  boolean equals(ExpandButton that) {
    return equals((Object) that);
  }

  /**
   * set button setting to expanded and force a call to expand(), even if the
   * value of the button isn't changed.  To allows the screen to be initialized.
   * @param expanded
   */
  void setExpanded(boolean expanded) {
    //prevent buttonAction from ignoring an unchanged button value
    if (this.expanded == expanded) {
      this.expanded = !expanded;
    }
    buttonAction();
  }

  /**
   * Called by the action listener.  Changes the expanded state of the button.
   * Changes the text to "<" when button is expanded, or ">" when button is not
   * expanded.  Calls the Expandable.expand(ExpandButton) when finished setting
   * the expand state.
   */
  void buttonAction() {
    expanded = !expanded;
    super.setText(type.getSymbol(expanded));
    setToolTipText(type.getToolTip(expanded));
    if (container == null) {
      return;
    }
    container.expand(this);
  }

  /**
   * Action listener for this class
   */
  class ExpandButtonActionListener implements ActionListener {

    ExpandButton adaptee;

    ExpandButtonActionListener(ExpandButton adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction();
    }
  }

  static final class Type {

    private static final String MORE_EXPANDED_SYMBOL = "<html>&lt";
    private static final String ADVANCED_EXPANDED_SYMBOL = "B";
    private static final String OPEN_EXPANDED_SYMBOL = "-";

    static final Type MORE = new Type("more", MORE_EXPANDED_SYMBOL,
        "Show less.", "less", "<html>&gt", "Show more.");
    static final Type ADVANCED = new Type("advanced", ADVANCED_EXPANDED_SYMBOL,
        "Show basic options.", "basic", "A", "Show all options.");
    static final Type OPEN = new Type("open", OPEN_EXPANDED_SYMBOL,
        "Close panel.", "closed", "+", "Open panel.");

    private final String expandedState;
    private final String expandedSymbol;
    private final String expandedToolTip;
    private final String contractedState;
    private final String contractedSymbol;
    private final String contractedToolTip;

    private Type(String expandedState, String expandedSymbol,
        String expandedToolTip, String contractedState,
        String contractedSymbol, String contractedToolTip) {
      this.expandedState = expandedState;
      this.expandedSymbol = expandedSymbol;
      this.expandedToolTip = expandedToolTip;
      this.contractedState = contractedState;
      this.contractedSymbol = contractedSymbol;
      this.contractedToolTip = contractedToolTip;
    }

    static boolean isExpandedSymbol(String symbol) {
      symbol = Utilities.convertLabelToName(symbol);
      return symbol.equals(Utilities.convertLabelToName(MORE_EXPANDED_SYMBOL))
          || symbol.equals(Utilities
              .convertLabelToName(ADVANCED_EXPANDED_SYMBOL))
          || symbol.equals(Utilities.convertLabelToName(OPEN_EXPANDED_SYMBOL));
    }

    String getState(boolean expanded) {
      if (expanded) {
        return expandedState;
      }
      return contractedState;
    }

    String getExpandedState() {
      return expandedState;
    }

    String getContractedState() {
      return contractedState;
    }

    String getSymbol(boolean expanded) {
      if (expanded) {
        return expandedSymbol;
      }
      return contractedSymbol;
    }

    String getToolTip(boolean expanded) {
      if (expanded) {
        return expandedToolTip;
      }
      return contractedToolTip;
    }
  }
}