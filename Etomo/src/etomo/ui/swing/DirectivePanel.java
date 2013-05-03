package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.EtomoDirector;
import etomo.logic.DirectiveTool;
import etomo.logic.FieldValidator;
import etomo.storage.Directive;
import etomo.storage.DirectiveType;
import etomo.storage.DirectiveValueType;
import etomo.type.AxisID;
import etomo.ui.FieldType;

/**
* <p>Description: Panel for a Setupset directive.</p>
* 
* <p>Copyright: Copyright 2013</p>
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
final class DirectivePanel implements DirectiveSetInterface {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbInclude = new CheckBox("Include: ");

  private final CheckBox cbValue;
  private final LabeledTextField ltfValue;
  private final DirectiveTool tool;
  private final Directive directive;
  private final AxisID axisID;
  private final FieldType fieldType;

  private int debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  private DirectivePanel siblingA = null;
  private DirectivePanel siblingB = null;

  private DirectivePanel(final Directive directive, final AxisID axisID,
      final DirectiveTool tool) {
    this.directive = directive;
    this.axisID = axisID;
    this.tool = tool;
    fieldType = FieldType.getInstance(directive.getValueType());
    // Initialize the value field that matches the value type. Set the other one to null.
    DirectiveType type = directive.getType();
    String axisTitle = "";
    if (axisID == AxisID.FIRST) {
      axisTitle = " - Axis A";
    }
    else if (axisID == AxisID.SECOND) {
      axisTitle = " - Axis B";
    }
    String title = directive.getTitle() + axisTitle;

    DirectiveValueType valueType = directive.getValueType();
    boolean bool = valueType == DirectiveValueType.BOOLEAN;
    FieldType fieldType = FieldType.getInstance(valueType);
    if (bool) {
      cbValue = new CheckBox(title);
      ltfValue = null;
    }
    else {
      ltfValue = new LabeledTextField(fieldType, title + ":  ");
      cbValue = null;
    }
  }

  static DirectivePanel getSoloInstance(final Directive directive,
      final DirectiveTool tool) {
    DirectivePanel instance = new DirectivePanel(directive, null, tool);
    instance.createPanel(directive);
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  static DirectivePanel getSetInstance(final Directive directive, final AxisID axisID,
      final DirectiveTool tool) {
    DirectivePanel instance = new DirectivePanel(directive, axisID, tool);
    instance.createPanel(directive);
    instance.setTooltips();
    return instance;
  }

  private void createPanel(final Directive directive) {
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.add(cbInclude);
    if (cbValue != null) {
      pnlRoot.add(cbValue);
    }
    else {
      pnlRoot.add(ltfValue.getComponent());
    }
    pnlRoot.add(Box.createHorizontalGlue());
    // init
    if (ltfValue != null) {
      ltfValue.setColumns(directive.getValueType().getColumns());
    }
    Directive.Value value = directive.getValues().getValue(axisID);
    if (value != null) {
      if (cbValue != null) {
        cbValue.setSelected(value.toBoolean());
      }
      else {
        ltfValue.setText(value.toString());
      }
    }
    // checkpoint
    if (cbValue != null) {
      cbValue.checkpoint();
    }
    else if (ltfValue != null) {
      ltfValue.checkpoint();
    }
    if (axisID == null && siblingA == null) {
      updateDisplay(false, axisID);
    }
  }

  public Component getComponent() {
    return pnlRoot;
  }

  private void addListeners() {
    cbInclude.addActionListener(new DirectiveListener(this));
  }

  void addActionListener(final ActionListener listener) {
    cbInclude.addActionListener(listener);
  }

  void setSiblings(final DirectivePanel inputA, final DirectivePanel inputB) {
    siblingA = inputA;
    siblingB = inputB;
  }

  /**
   * Updates the display.  Handles changes caused by the user, initialization, changes to
   * the editor configuration, and other directives.  If the instance has been placed
   * directly in a section panel rather then in a directive set panel, this function also
   * called pnlRoot.setVisible.  If this instance is an Any directive, this function calls
   * updateDisplay in the corresponding A and B directives.
   * 
   * The fromAxisID parameter:
   * Setupset directives: null
   * Param and Runtime directives: A or B (doesn't matter which is used) except:
   *   - during initialization when the Source Axis Type is Single: null
   *   - Called from action(): use the AxisID from where the change happened
   * @param action - true when this function was called because include checkbox or value was modified by the user.
   * @param fromAxisID - The controlling axis: null (Any), AxisID.FIRST, or AxisID.SECOND
   */
  void updateDisplay(final boolean action, final AxisID fromAxisID) {
    // Only set toggle if the action parameter is false and the axis of this instance
    // matches fromAxisID.
    boolean toggleInclude = false;
    if (!action && (axisID == fromAxisID || (axisID != null && fromAxisID != null))) {
      toggleInclude = cbInclude.isSelected() != tool.isDirectiveIncluded(directive,
          axisID);
    }
    if (axisID == null) {
      if (siblingA == null) {
        // This is a setupset instance and has been placed directly in a section panel.
        if (toggleInclude) {
          cbInclude.setSelected(!cbInclude.isSelected());
        }
        // Since this instance has been placed directly in a section panel, it is
        // responsible for deciding whether it is visible.
        if (!action) {
          boolean visible = pnlRoot.isVisible();
          if (visible != tool.isDirectiveVisible(directive, cbInclude.isSelected(),
              (cbValue != null && cbValue.isDifferentFromCheckpoint())
                  || (ltfValue != null && ltfValue.isDifferentFromCheckpoint()))) {
            pnlRoot.setVisible(!visible);
          }
        }
      }
      else if (fromAxisID == null && cbInclude.isEnabled()) {
        // If Any is enabled, then A and B must be identical. In this case changes go from
        // Any to A and B.
        if (toggleInclude) {
          cbInclude.setSelected(!cbInclude.isSelected());
        }
        siblingA.set(this);
        siblingA.updateDisplay(action, fromAxisID);
        siblingB.set(this);
        siblingB.updateDisplay(action, fromAxisID);
      }
      else {
        // Changes go from A and B to Any
        siblingA.updateDisplay(action, fromAxisID);
        siblingB.updateDisplay(action, fromAxisID);
        // Any is enabled when A and B are the same.
        if (siblingA.equals(siblingB)) {
          cbInclude.setEnabled(true);
          // Any is identical to A and B when they are the same.
          set(siblingA);
        }
        else {
          // Any is disabled when A and B are different.
          cbInclude.setEnabled(false);
          // Any include is selected if either A or B is included.
          cbInclude.setSelected(siblingA.isInclude() || siblingB.isInclude());
          // If A and B have the same value, Any also has it.
          if (siblingA.equalsValue(siblingB)) {
            setValue(siblingA);
          }
          else {
            resetValue();
          }
        }
      }
    }
    // FromAxisID is A or B.
    else if (toggleInclude) {
      cbInclude.setSelected(!cbInclude.isSelected());
    }
    // Correctly enable the value
    boolean enable = cbInclude.isSelected() && cbInclude.isEnabled();
    if (cbValue != null) {
      cbValue.setEnabled(enable);
    }
    else {
      ltfValue.setEnabled(enable);
    }
  }

  /**
   * Calls updateDisplay with the assumption that the cause isn't the user changing the
   * directive.
   */
  public void updateDisplay() {
    if (axisID == null && siblingA == null) {
      // This is a single setupset directive.
      updateDisplay(false, axisID);
    }
    else {
      // This is a directive in a directive set.
      updateDisplay(false, AxisID.FIRST);
    }
  }

  private void set(final DirectivePanel input) {
    cbInclude.setSelected(input.cbInclude.isSelected());
    setValue(input);
  }

  private void setValue(final DirectivePanel input) {
    if (cbValue != null) {
      cbValue.setSelected(input.cbValue.isSelected());
    }
    else {
      ltfValue.setText(input.ltfValue.getText());
    }
  }

  private void resetValue() {
    if (cbValue != null) {
      cbValue.setSelected(false);
    }
    else {
      ltfValue.setText("");
    }
  }

  private boolean equals(final DirectivePanel input) {
    if (input == null) {
      return false;
    }
    if (cbInclude.isSelected() != input.isInclude()) {
      return false;
    }
    return equalsValue(input);
  }

  private boolean equalsValue(final DirectivePanel input) {
    if (input == null) {
      return false;
    }
    if (cbValue != null) {
      if (cbValue.isSelected() != input.cbValue.isSelected()) {
        return false;
      }
    }
    else {
      if (!FieldValidator.equals(fieldType, ltfValue.getText(), input.ltfValue.getText())) {
        return false;
      }
    }
    return true;
  }

  public boolean isInclude() {
    return cbInclude.isSelected();
  }

  boolean isDifferentFromCheckpoint() {
    if (cbValue != null) {
      return cbValue.isDifferentFromCheckpoint();
    }
    return ltfValue.isDifferentFromCheckpoint();
  }

  public boolean isVisible() {
    return pnlRoot.isVisible();
  }

  void setVisible(final boolean input) {
    pnlRoot.setVisible(input);
  }

  private void action(final ActionEvent event) {
    updateDisplay(true, axisID);
  }

  private void setTooltips() {
    String description = directive.getDescription();
    String tooltip = directive.getKey() + ":  " + description;
    cbInclude.setToolTipText(tooltip);
    if (cbValue != null) {
      cbValue.setToolTipText(tooltip);
    }
    else {
      ltfValue.setToolTipText(tooltip);
    }
  }

  private static final class DirectiveListener implements ActionListener {
    private final DirectivePanel panel;

    private DirectiveListener(final DirectivePanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event);
    }
  }
}
