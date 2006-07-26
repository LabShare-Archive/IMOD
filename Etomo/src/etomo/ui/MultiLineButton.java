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
import etomo.type.ProcessResult;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayState;
import etomo.type.UITestField;
import etomo.util.Utilities;

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
 * <p>Copyright: Copyright 2002 - 2006</p>
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
 * <p> Revision 3.23  2006/07/20 17:20:36  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.22  2006/05/01 21:18:45  sueh
 * <p> bug# 854
 * <p>
 * <p> Revision 3.21  2006/04/28 21:02:15  sueh
 * <p> bug# 787 Added getButton and getToggleButtonInstance.
 * <p>
 * <p> Revision 3.20  2006/04/25 19:17:01  sueh
 * <p> bug# 787 Added UITestField, an enum style class which contains the
 * <p> fields found in uitestaxis.adoc files.
 * <p>
 * <p> Revision 3.19  2006/04/06 20:17:14  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 3.18  2006/03/28 17:03:31  sueh
 * <p> bug# 437 Added setOriginalProcessResultDisplayState to be used by
 * <p> setButtonState.
 * <p>
 * <p> Revision 3.17  2006/03/28 00:54:29  sueh
 * <p> bug# 437 Save dialogType and do a lazy creation of stateKey.  This allows
 * <p> more flexiblility in setting the name.  Change getButtonStateKey(DialogType)
 * <p> to createButtonStateKey(DialogType).
 * <p>
 * <p> Revision 3.16  2006/02/06 21:21:04  sueh
 * <p> bug# 521 ProcessResultDisplayState:  changed following display to
 * <p> dependent display.  Added dependecy index and initialized.
 * <p>
 * <p> Revision 3.15  2006/01/31 20:59:14  sueh
 * <p> bug# 521 Added failureDisplayList, successDisplayList, and
 * <p> followingDisplayList to change the state of other displays when the
 * <p> process associated with the current instance succeeds or fails.
 * <p> Added BaseScreenState so that buttons on dialogs not current
 * <p> displayed could change their state.  Without the ability to change the
 * <p> screen state setting, they're old state would be reloaded when the
 * <p> dialog was redisplayed.
 * <p>
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
  private DialogType dialogType = null;
  private String stateKey = null;
  private boolean manualName = false;

  MultiLineButton() {
    this(null, false, null);
  }

  MultiLineButton(String label) {
    this(label, false, null);
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
  protected MultiLineButton(String label, boolean toggleButton,
      DialogType dialogType) {
    this.toggleButton = toggleButton;
    this.dialogType = dialogType;
    if (toggleButton) {
      button = new JToggleButton(format(label));
    }
    else {
      button = new JButton(format(label));
    }
    if (label != null) {
      setName(label);
    }
    init();
    processResultDisplayState = new ProcessResultDisplayState(this);
  }
  
  static final MultiLineButton getToggleButtonInstance() {
    return new MultiLineButton(null, true, null);
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
   * be changed.  Does not store the dialog type.
   * @param dialogType
   * @return
   */
  String createButtonStateKey(DialogType dialogType) {
    stateKey = dialogType.getStorableName() + '.' + button.getName() + ".done";
    return stateKey;
  }

  /**
   * Gets the button state key.  If the state key is null and the dialog type
   * is available, creates the button state key.
   * @return
   */
  final String getButtonStateKey() {
    if (stateKey == null && dialogType != null) {
      return createButtonStateKey(dialogType);
    }
    return stateKey;
  }

  void setButtonState(boolean state) {
    setOriginalProcessResultDisplayState(state);
    setSelected(state);
  }

  protected final void setOriginalProcessResultDisplayState(boolean state) {
    processResultDisplayState.setOriginalState(state);
  }

  /**
   * Returns isSelected() since the button state is selected or not selected
   * @return
   */
  boolean getButtonState() {
    return isSelected();
  }

  /**
   * Sets manualName to true.  When manualName is true, setText() cannot change
   * the name.
   * @param label
   */
  final void setManualName() {
    manualName = true;
  }

  /**
   * Calls the setName function of the button.  Called when the text is set,
   * unless manualName is true.
   * @param label
   */
  void setName(String label) {
    String name = Utilities.convertLabelToName(label);
    button.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
  
  protected final AbstractButton getButton() {
    return button;
  }

  public final String getName() {
    return button.getName();
  }

  /**
   * Sets stateKey.  For overriding createButtonStateKey().
   * @param stateKey
   * @return
   */
  protected final void setStateKey(String stateKey) {
    this.stateKey = stateKey;
  }

  final void setEnabled(boolean isEnabled) {
    button.setEnabled(isEnabled);
    button.setForeground(isEnabled ? enabledTextColor : disabledTextColor);
  }

  protected final boolean isToggleButton() {
    return toggleButton;
  }

  final void setText(String label) {
    if (!manualName) {
      setName(label);
    }
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
    Dimension size = UIParameters.INSTANCE.getButtonDimension();
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
  
  public void msg(ProcessResult processResult) {
    processResultDisplayState.msg(processResult);
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

  public void addDependentDisplay(ProcessResultDisplay dependentDisplay) {
    processResultDisplayState.addDependentDisplay(dependentDisplay);
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

  public void setDependencyIndex(int dependencyIndex) {
    processResultDisplayState.setDependencyIndex(dependencyIndex);
  }

  public int getDependencyIndex() {
    return processResultDisplayState.getDependencyIndex();
  }

  public boolean isInitialized() {
    return processResultDisplayState.isInitialized();
  }

  public void setInitialized(boolean initialize) {
    processResultDisplayState.setInitialized(initialize);
  }
}