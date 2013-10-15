package etomo.ui.swing;

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
 * <p>Description: Small square buttons with a expanded state variable.  When
 * expanded is true,
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.24  2010/10/12 02:04:47  sueh
 * <p> bug# 1391 Reduce visibility of class.
 * <p>
 * <p> Revision 1.23  2009/10/08 20:58:58  sueh
 * <p> bug# 1277 In setName change the name to mb.name.
 * <p>
 * <p> Revision 1.22  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.21  2009/03/17 22:14:21  sueh
 * <p> bug# 1200 In createButtonStateKey() added type.getExapndedState() to key.
 * <p>
 * <p> Revision 1.20  2009/01/20 20:00:36  sueh
 * <p> bug# 1102 Changed UITestField to UITestFieldType.  Simplified the name
 * <p> by removing the expanded state portion.
 * <p>
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
final class ExpandButton extends MultiLineButton {
  public static final String rcsid = "$Id$";

  private static final Type DEFAULT_TYPE = Type.MORE;

  private final Type type;
  private final Expandable expandable1;
  private final Expandable expandable2;
  private final GlobalExpandButton globalExpandButton;

  private boolean expanded;
  private JPanel jpanelContainer = null;

  static ExpandButton getInstance(final Expandable expandable, ExpandButton.Type type) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(expandable, null, type, null);
    return instance;
  }

  static ExpandButton getGlobalInstance(final Expandable expandable1,
      ExpandButton.Type type, final GlobalExpandButton globalExpandButton) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(expandable1, null, type, globalExpandButton);
    return instance;
  }

  static ExpandButton getGlobalInstance(final Expandable expandable1,
      final Expandable expandable2, ExpandButton.Type type,
      final GlobalExpandButton globalExpandButton) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(expandable1, expandable2, type,
        globalExpandButton);
    return instance;
  }

  static ExpandButton getExpandedInstance(final Expandable expandable1,
      final Expandable expandable2, ExpandButton.Type type) {
    if (type == null) {
      type = DEFAULT_TYPE;
    }
    ExpandButton instance = new ExpandButton(expandable1, expandable2, type, true, null);
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
  private ExpandButton(final Expandable expandable1, final Expandable expandable2,
      final ExpandButton.Type type, final GlobalExpandButton globalExpandButton) {
    this(expandable1, expandable2, type, false, globalExpandButton);
  }

  private ExpandButton(final Expandable expandable1, final Expandable expandable2,
      final ExpandButton.Type type, final boolean expanded,
      final GlobalExpandButton globalExpandButton) {
    super();
    this.expandable1 = expandable1;
    this.expandable2 = expandable2;
    this.globalExpandButton = globalExpandButton;
    this.type = type;
    this.expanded = expanded;
    // Registor with the global expand button, if it exists.
    if (globalExpandButton != null) {
      globalExpandButton.register(this);
    }
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
   * unless manualName is true.  To distinguish these buttons from regular sized
   * labeled buttons, which are called "bn" in the uitest software, the name
   * includes the mini-button tag.
   * @param label
   */
  void setName(final String associatedLabel) {
    String name = Utilities.convertLabelToName(associatedLabel);
    getButton().setName(
        UITestFieldType.MINI_BUTTON.toString() + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  /**
   * 
   * @return expanded
   */
  boolean isExpanded() {
    return expanded;
  }

  static boolean isExpanded(final JButton button) {
    return Type.isExpandedSymbol(button.getText());
  }

  /**
   * Overrides MultiLineButton.createButtonStateKey.  Necessary because an
   * expander button doesn't have a done state.
   * @param dialogType
   * @return
   */
  String createButtonStateKey(final DialogType dialogType) {
    String stateKey = dialogType.getStorableName() + '.' + getName() + '.'
        + type.getExpandedState();
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

  void setButtonState(final boolean state) {
    setOriginalProcessResultDisplayState(state);
    setExpanded(state);
  }

  String getState() {
    return type.getState(expanded);
  }

  void setState(final String state) {
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

  void add(final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    double oldWeightx = constraints.weightx;
    constraints.weightx = 0.0;
    layout.setConstraints(getComponent(), constraints);
    constraints.weightx = oldWeightx;
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
  boolean equals(final ExpandButton that) {
    return equals((Object) that);
  }

  /**
   * set button setting to expanded and force a call to expand(), even if the
   * value of the button isn't changed.  To allows the screen to be initialized.
   * @param expanded
   */
  void setExpanded(final boolean expanded) {
    // prevent buttonAction from ignoring an unchanged button value
    if (this.expanded == expanded) {
      this.expanded = !expanded;
    }
    buttonAction();
  }

  /**
   * Called by the action listener.  Changes the expanded state of the button.
   * Changes the text to "<" when button is expanded, or ">" when button is not
   * expanded.  Calls the expand(ExpandButton) for the three Expandables when
   * finished setting the expand state.
   */
  private void buttonAction() {
    expanded = !expanded;
    super.setText(type.getSymbol(expanded));
    setToolTipText(type.getToolTip(expanded));
    if (expandable1 != null) {
      expandable1.expand(this);
    }
    if (expandable2 != null) {
      expandable2.expand(this);
    }
    // Tell global expand button about this action.
    if (globalExpandButton != null) {
      globalExpandButton.msgExpandButtonAction(this, expanded);
    }
  }

  /**
   * Action listener for this class
   */
  private final class ExpandButtonActionListener implements ActionListener {
    private final ExpandButton adaptee;

    private ExpandButtonActionListener(final ExpandButton adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction();
    }
  }

  static final class Type {
    private static final String MORE_EXPANDED_SYMBOL = "<html>&lt";
    private static final String ADVANCED_EXPANDED_SYMBOL = "B";
    private static final String OPEN_EXPANDED_SYMBOL = "-";

    static final Type MORE = new Type("more", MORE_EXPANDED_SYMBOL, "Show less.", "less",
        "<html>&gt", "Show more.");
    static final Type ADVANCED = new Type("advanced", ADVANCED_EXPANDED_SYMBOL,
        "Show basic options.", "basic", "A", "Show all options.");
    static final Type OPEN = new Type("open", OPEN_EXPANDED_SYMBOL, "Close panel.",
        "closed", "+", "Open panel.");

    // Backwards compatibility issue: expandedState is a key in the .edf file.
    private final String expandedState;

    private final String expandedSymbol;
    private final String expandedToolTip;
    private final String contractedState;
    private final String contractedSymbol;
    private final String contractedToolTip;

    private Type(final String expandedState, final String expandedSymbol,
        final String expandedToolTip, final String contractedState,
        final String contractedSymbol, final String contractedToolTip) {
      this.expandedState = expandedState;
      this.expandedSymbol = expandedSymbol;
      this.expandedToolTip = expandedToolTip;
      this.contractedState = contractedState;
      this.contractedSymbol = contractedSymbol;
      this.contractedToolTip = contractedToolTip;
    }

    private static boolean isExpandedSymbol(String symbol) {
      symbol = Utilities.convertLabelToName(symbol);
      return symbol.equals(Utilities.convertLabelToName(MORE_EXPANDED_SYMBOL))
          || symbol.equals(Utilities.convertLabelToName(ADVANCED_EXPANDED_SYMBOL))
          || symbol.equals(Utilities.convertLabelToName(OPEN_EXPANDED_SYMBOL));
    }

    private String getState(final boolean expanded) {
      if (expanded) {
        return expandedState;
      }
      return contractedState;
    }

    /**
     * Backwards compatibility issue:  expandedState is a key in the .edf file.
     * @return expandedState
     */
    private String getExpandedState() {
      return expandedState;
    }

    private String getContractedState() {
      return contractedState;
    }

    private String getSymbol(final boolean expanded) {
      if (expanded) {
        return expandedSymbol;
      }
      return contractedSymbol;
    }

    private String getToolTip(final boolean expanded) {
      if (expanded) {
        return expandedToolTip;
      }
      return contractedToolTip;
    }
  }
}