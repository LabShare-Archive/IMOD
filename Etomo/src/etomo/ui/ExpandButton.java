package etomo.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

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
public class ExpandButton extends MultiLineButton {
  public static final String rcsid = "$Id$";
  private String contractSymbol = "<html><big>&lt</big>";
  private String expandSymbol = "<html><big>&gt</big>";

  private boolean expanded = false;
  private Expandable container = null;

  /**
   * Create a button with ">" if expanded is false, or "<" expanded is true.
   * Add an action listener and a raised, beveled border to make it 3D.  Set the
   * size so that it is square, unless the width needs to be greater then the
   * height.  The button remembers the expanded state and keeps track of it.
   * The button can be used on any ui.
   * @param expanded Current state of the component.
   * @param component Exandable component displaying the button.
   */
  ExpandButton(boolean expanded, Expandable container) {
    super();
    this.expanded = expanded;
    this.container = container;
    initialize();
  }

  /**
   * Create a button where the current state of the component is not expanded.
   * @param component
   */
  ExpandButton(Expandable container) {
    super();
    this.container = container;
    initialize();
  }

  ExpandButton(Expandable container, String contractSymbol, String expandSymbol) {
    super();
    this.container = container;
    this.contractSymbol = contractSymbol;
    this.expandSymbol = expandSymbol;
    initialize();
  }

  /**
   * only called by constructor
   *
   */
  private void initialize() {
    if (expanded) {
      setText(contractSymbol);
    }
    else {
      setText(expandSymbol);
    }
    addActionListener(new ExpandButtonActionListener(this));
    setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
    Dimension size = getPreferredSize();
    if (size.width < size.height) {
      size.width = size.height;
    }
    setSize(size);
  }

  /**
   * 
   * @return expanded
   */
  boolean isExpanded() {
    return expanded;
  }

  void add(JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    layout.setConstraints(getComponent(), constraints);
    panel.add(getComponent());
  }

  /**
   * Equals may be used by the Expandable component to identify a button.  So
   * equals(ExpandButton) must call Object.equals(Object) so that each button
   * has a unique signature.
   * @param that
   * @return
   */
  public boolean equals(ExpandButton that) {
    return equals((Object) that);
  }
  
  /**
   * set button setting to expanded and force a call to expand(), even if the
   * value of the button isn't changed.  To allows the screen to be initialized.
   * @param expanded
   */
  public void setExpanded(boolean expanded) {
    //prevent buttonAction from ignore an unchanged button value
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
  private void buttonAction() {
    if (expanded) {
      expanded = false;
      super.setText(expandSymbol);
    }
    else {
      expanded = true;
      super.setText(contractSymbol);
    }
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
}