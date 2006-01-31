package etomo.ui;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import javax.swing.plaf.ColorUIResource;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayState;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.lang.String;

/**
 * <p>Description: Contains either a JButton or a JToggleButton.  
 * <p> Construct with the contructor to get a JButton.  Construct with a
 * getToggleButtonInstance call to get a JToggleButton.
 * <p>This class wraps the button label in html so that the text will wrap.
 * This prevents the text from changing color when the button is disabled, so
 * this class controls the text color on enable/disable.</p>
 *
 * <p>Copyright: Copyright © 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * Univeristy of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.14  2006/01/26 22:05:23  sueh
 * <p> bug# 401 Turn ProcessResultDisplay into an interface.  Place the
 * <p> functionality into ProcessResultDisplayState.  This allows a greater
 * <p> variety of classes to be ProcessResultDisplay's.
 * <p>
 * <p> Revision 3.13  2006/01/20 21:11:06  sueh
 * <p> bug# 401 Allow MultiLineButton to get and set its state and build a key to
 * <p> be used for saving to a properties file.  Extending ProcessResultDisplay.
 * <p>
 * <p> Revision 3.12  2006/01/12 22:12:22  sueh
 * <p> bug# 401 added an action listener to keep toggle buttons selected every
 * <p> time they are pressed.  Added isToggleButton().
 * <p> bug# 798 Reducing the visibility and inheritability of ui classes.
 * <p>
 * <p> Revision 3.11  2006/01/12 17:11:48  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.10  2006/01/11 22:16:13  sueh
 * <p> bug# 675 Added boolean toggleButton to save whether the button is a
 * <p> JButton or JToggleButton.  This is needed in printName.
 * <p>
 * <p> Revision 3.9  2006/01/04 20:26:30  sueh
 * <p> bug# 675 For printing the name:  putting the type first and making the type
 * <p> as constant.
 * <p>
 * <p> Revision 3.8  2006/01/03 23:42:21  sueh
 * <p> bug# 675 Made setName protected.
 * <p>
 * <p> Revision 3.7  2005/12/23 02:16:44  sueh
 * <p> bug# 675 Named the button so it can be found by JfcUnit.
 * <p>
 * <p> Revision 3.6  2005/08/22 17:56:32  sueh
 * <p> bug#  Added isDisplayable().
 * <p>
 * <p> Revision 3.5  2005/08/10 20:44:42  sueh
 * <p> bug# 711  Added getToggleButtonInstance(), isEnabled(), and
 * <p> isSelected()
 * <p>
 * <p> Revision 3.4  2005/08/09 20:25:15  sueh
 * <p> bug# 711  No longer inheriting JButton in MultiLineButton.  This allows
 * <p> MultiLineButton to treate toggling as an attribute.  Then we can get rid of
 * <p> MultiLineToggleButton.  Then we can have one Run3dmodButton which
 * <p> can be toggle or non-toggle.
 * <p>
 * <p> Revision 3.3  2005/07/06 23:37:05  sueh
 * <p> bug# 619 removed unused constructors
 * <p>
 * <p> Revision 3.2  2004/11/19 23:59:12  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.1.4.1  2004/09/17 21:37:56  sueh
 * <p> bug# 520 getDefaultUIColor() was moved to UIUtilities
 * <p>
 * <p> Revision 3.1  2004/04/26 03:16:22  rickg
 * <p> Set text margin to 2 all around
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/10/21 02:32:40  sueh
 * <p> Bug325 New class, behaves like JButton, except that it automatically makes button text multi-line.
 * <p> </p>
 */
class MultiLineButton implements ProcessResultDisplay {
  public static final String rcsid = "$$Id$$";

  public static final String ENABLED_TEXT_COLOR_PROPERTY = "Button.foreground";
  public static final String DISABLED_TEXT_COLOR_PROPERTY = "Button.disabledText";

  private final AbstractButton button;
  private final boolean toggleButton;
  private final ProcessResultDisplayState processResultDisplayState;
  private BaseScreenState screenState = null;

  private String stateKey = null;

  MultiLineButton() {
    this(null);
  }

  MultiLineButton(String label) {
    this(label, false);
  }

  protected MultiLineButton(String label, boolean toggleButton) {
    this(label, toggleButton, null);
  }

  /**
   * Pass the dialogType so that a button that is managed by
   * ProcessResultDisplay can save itself in BaseScreenState.
   * @param label
   * @param toggleButton
   * @param dialogType
   */
  private MultiLineButton(String label, boolean toggleButton,
      DialogType dialogType) {
    this.toggleButton = toggleButton;
    if (toggleButton) {
      button = new JToggleButton(format(label));
    }
    else {
      button = new JButton(format(label));
    }
    setName(label);
    init();
    processResultDisplayState = new ProcessResultDisplayState(this);
    if (dialogType != null) {
      getButtonStateKey(dialogType);
    }
  }

  static final MultiLineButton getToggleButtonInstance(String label,
      DialogType dialogType) {
    return new MultiLineButton(label, true, dialogType);
  }

  static final MultiLineButton getToggleButtonInstance(String label) {
    return new MultiLineButton(label, true);
  }

  /**
   * return's the button's state key for the property file
   * Creates the button state instance if necessary.  Sets the button state name
   * based on the dialog and the button's name.  Once the key is set, it cannot
   * be changed.
   * @param dialogType
   * @return
   */
  final String getButtonStateKey(DialogType dialogType) {
    stateKey = dialogType.getStorableName() + '.' + button.getName() + ".done";
    return stateKey;
  }

  final String getButtonStateKey() {
    return stateKey;
  }

  final void setButtonState(boolean state) {
    processResultDisplayState.setOriginalState(state);
    setSelected(state);
  }

  final boolean getButtonState() {
    return isSelected();
  }

  protected final void setName(String label) {
    String name = UIUtilities.convertLabelToName(label);
    button.setName(name);
    String buttonAttrib = toggleButton ? UITestConstants.TOGGLE_BUTTON_ATTRIB
        : UITestConstants.BUTTON_ATTRIB;
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(buttonAttrib + AutodocTokenizer.SEPARATOR_CHAR + name
          + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
  
  public final String getName() {
    return button.getName();
  }

  final void setEnabled(boolean isEnabled) {
    button.setEnabled(isEnabled);
    button.setForeground(isEnabled ? enabledTextColor : disabledTextColor);
  }

  protected final boolean isToggleButton() {
    return toggleButton;
  }

  final void setText(String label) {
    setName(label);
    button.setText(format(label));
  }

  private static final String format(String label) {
    if (label == null) {
      return null;
    }
    if (label.toLowerCase().startsWith("<html>")) {
      return label;
    }
    label = "<html><b>".concat(label).concat("</b>");
    return label;
  }

  public String toString() {
    return "[enabledTextColor=" + enabledTextColor + ",\n  disabledTextColor ="
        + disabledTextColor + ",\n  button=" + button + ",\n "
        + super.toString() + "]\n";
  }

  final void addActionListener(ActionListener actionListener) {
    button.addActionListener(actionListener);
  }

  final String getActionCommand() {
    return button.getActionCommand();
  }

  final Component getComponent() {
    return button;
  }

  final void setVisible(boolean visible) {
    button.setVisible(visible);
  }

  final void setBorder(Border border) {
    button.setBorder(border);
  }

  final void setToolTipText(String toolTip) {
    button.setToolTipText(toolTip);
  }

  final Dimension getPreferredSize() {
    return button.getPreferredSize();
  }

  final void setAlignmentX(float alignmentX) {
    button.setAlignmentX(alignmentX);
  }

  final void setAlignmentY(float alignmentY) {
    button.setAlignmentY(alignmentY);
  }

  final String getText() {
    return button.getText();
  }

  final void addMouseListener(MouseListener mouseListener) {
    button.addMouseListener(mouseListener);
  }

  final void removeActionListener(ActionListener actionListener) {
    button.removeActionListener(actionListener);
  }

  /**
   * Set the button sizes (preferred and maximum) of a button
   * @param container
   * @param size
   */
  final void setSize() {
    setSize(false);
  }

  final void setSize(boolean setMinimum) {
    Dimension size = UIParameters.getButtonDimension();
    button.setPreferredSize(size);
    button.setMaximumSize(size);
    if (setMinimum) {
      button.setMinimumSize(size);
    }
  }

  final void setSize(Dimension size) {
    button.setPreferredSize(size);
    button.setMaximumSize(size);
  }

  public final void setProcessDone(boolean done) {
    setSelected(done);
  }

  /**
   * Make sure that the displayed button matches its internal state.  A button
   * won't update its display if it is not being displayed.
   */
  public final void setScreenState(BaseScreenState screenState) {
    this.screenState = screenState;
  }

  final void setSelected(boolean selected) {
    button.setSelected(selected);
    if (screenState != null) {
      screenState.setButtonState(getButtonStateKey(), getButtonState());
    }
  }

  public final boolean getOriginalState() {
    //button has just been pushed, so that original state is the state of the
    //button before it was pushed
    return !isSelected();
  }

  final boolean isSelected() {
    return button.isSelected();
  }

  final boolean isEnabled() {
    return button.isEnabled();
  }

  final boolean isDisplayable() {
    return button.isDisplayable();
  }

  //private implementation

  private static ColorUIResource enabledTextColor = null;
  private static ColorUIResource disabledTextColor = null;

  //if changing this class to inheritable, make this method protected
  private void init() {
    button.setMargin(new Insets(2, 2, 2, 2));
    enabledTextColor = getDefaultUIColor(ENABLED_TEXT_COLOR_PROPERTY);
    disabledTextColor = getDefaultUIColor(DISABLED_TEXT_COLOR_PROPERTY);
  }

  private ColorUIResource getDefaultUIColor(String property) {
    ColorUIResource color = UIUtilities.getDefaultUIColor(property);
    if (color == null) {
      color = createDefaultColor(property);
    }
    return color;
  }

  private static ColorUIResource createDefaultColor(String property) {
    System.err.println("Warning: Cannot retrieve default UI property: "
        + property);
    if (property == ENABLED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0, 0, 0);
    }
    else if (property == DISABLED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(153, 153, 153);
    }
    else {
      throw new IllegalArgumentException(property);
    }
  }

  public void msgProcessStarting() {
    processResultDisplayState.msgProcessStarting();
  }

  public void msgProcessSucceeded() {
    processResultDisplayState.msgProcessSucceeded();
  }

  public void msgProcessFailed() {
    processResultDisplayState.msgProcessFailed();
  }

  public void msgProcessFailedToStart() {
    processResultDisplayState.msgProcessFailedToStart();
  }

  public void msgSecondaryProcess() {
    processResultDisplayState.msgSecondaryProcess();
  }

  public void addFollowingDisplay(ProcessResultDisplay followingDisplay) {
    processResultDisplayState.addFollowingDisplay(followingDisplay);
  }

  public void setOriginalState(boolean originalState) {
    processResultDisplayState.setOriginalState(originalState);
  }

  public void addFailureDisplay(ProcessResultDisplay failureDisplay) {
    processResultDisplayState.addFailureDisplay(failureDisplay);
  }

  public void addSuccessDisplay(ProcessResultDisplay successDisplay) {
    processResultDisplayState.addSuccessDisplay(successDisplay);
  }
}