package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.logic.DirectiveTool;
import etomo.storage.Directive;
import etomo.storage.DirectiveType;
import etomo.storage.DirectiveValueType;
import etomo.storage.DirectiveValues;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.ui.FieldType;

/**
* <p>Description: Directive set panel handles the three (A, B, and both axes) directives
* that can potientially be created from a single storage.Directive instance.</p>
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
final class DirectiveSetPanel implements Expandable {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbInclude = new CheckBox("Include: ");
  private final JPanel pnlAny = new JPanel();
  private final JPanel pnlAxisA = new JPanel();
  private final JPanel pnlAxisB = new JPanel();

  private final JPanel pnlRootBody;
  private final CheckBox cbIncludeA;
  private final CheckBox cbIncludeB;
  private final CheckBox cbValue;
  private final CheckBox cbValueA;
  private final CheckBox cbValueB;
  private final LabeledTextField ltfValue;
  private final LabeledTextField ltfValueA;
  private final LabeledTextField ltfValueB;
  private final ExpandButton expandButton;
  private final DirectiveTool tool;
  private final Directive directive;

  private DirectiveSetPanel(final DirectiveTool tool, final Directive directive) {
    this.directive = directive;
    this.tool = tool;
    // Initialize the value field that matches the value type. Set the other one to null.
    DirectiveType type = directive.getType();
    String title = directive.getTitle();
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
    // Initialize the A and B axes only if they are required, otherwise set them to null.
    if (type == DirectiveType.COM_PARAM || type == DirectiveType.RUNTIME) {
      expandButton = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
      pnlRootBody = new JPanel();
      cbIncludeA = new CheckBox("Include: ");
      cbIncludeB = new CheckBox("Include: ");
      String axisATitle = " Axis A";
      String axisBTitle = " Axis B";
      if (bool) {
        cbValueA = new CheckBox(title + axisATitle);
        cbValueB = new CheckBox(title + axisBTitle);
        ltfValueA = null;
        ltfValueB = null;
      }
      else {
        ltfValueA = new LabeledTextField(fieldType, title + axisATitle + ":  ");
        ltfValueB = new LabeledTextField(fieldType, title + axisBTitle + ":  ");
        cbValueA = null;
        cbValueB = null;
      }
    }
    else {
      expandButton = null;
      pnlRootBody = null;
      cbIncludeA = null;
      cbIncludeB = null;
      cbValueA = null;
      cbValueB = null;
      ltfValueA = null;
      ltfValueB = null;
    }
  }

  static DirectiveSetPanel getInstance(final Directive directive,
      final AxisType sourceAxisType, final DirectiveTool tool) {
    DirectiveSetPanel instance = new DirectiveSetPanel(tool, directive);
    instance.createPanel(directive, sourceAxisType);
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel(final Directive directive, final AxisType sourceAxisType) {
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    if (expandButton == null) {
      pnlRoot.add(cbInclude);
      if (cbValue != null) {
        pnlRoot.add(cbValue);
      }
      else {
        pnlRoot.add(ltfValue.getComponent());
      }
    }
    else {
      pnlRoot.add(expandButton.getComponent());
      pnlRoot.add(pnlRootBody);
      // root body panel
      pnlRootBody.setLayout(new BoxLayout(pnlRootBody, BoxLayout.Y_AXIS));
      pnlRootBody.add(pnlAny);
      pnlRootBody.add(pnlAxisA);
      pnlRootBody.add(pnlAxisB);
      // any panel
      pnlAny.setLayout(new BoxLayout(pnlAny, BoxLayout.X_AXIS));
      pnlAny.add(cbInclude);
      if (cbValue != null) {
        pnlAny.add(cbValue);
      }
      else {
        pnlAny.add(ltfValue.getComponent());
      }
      // axis A panel
      pnlAxisA.setLayout(new BoxLayout(pnlAxisA, BoxLayout.X_AXIS));
      pnlAxisA.add(cbIncludeA);
      if (cbValueA != null) {
        pnlAxisA.add(cbValueA);
      }
      else {
        pnlAxisA.add(ltfValueA.getComponent());
      }
      // axis B panel
      pnlAxisB.setLayout(new BoxLayout(pnlAxisB, BoxLayout.X_AXIS));
      pnlAxisB.add(cbIncludeB);
      if (cbValueB != null) {
        pnlAxisB.add(cbValueB);
      }
      else {
        pnlAxisB.add(ltfValueB.getComponent());
      }
    }
    // init
    // set columns in text fields
    int columns = directive.getValueType().getColumns();
    if (ltfValue != null) {
      ltfValue.setColumns(columns);
    }
    if (ltfValueA != null) {
      ltfValueA.setColumns(columns);
      ltfValueB.setColumns(columns);
    }
    DirectiveValues values = directive.getValues();
    // init A axis
    if (cbIncludeA != null && sourceAxisType == AxisType.DUAL_AXIS) {
      cbIncludeA.setSelected(tool.isDirectiveIncluded(directive, AxisID.FIRST));
      init(values.getValue(AxisID.FIRST), cbValueA, ltfValueA);
      // init B axis
      if (cbIncludeB != null) {
        cbIncludeB.setSelected(tool.isDirectiveIncluded(directive, AxisID.SECOND));
        init(values.getValue(AxisID.SECOND), cbValueB, ltfValueB);
      }
      // Attempt to display less
      expand(expandButton);
      // If Any is disabled, then more must be displayed
      if (!cbInclude.isEnabled()) {
        expandButton.setExpanded(true);
        expand(expandButton);
      }
    }
    else {
      // No A and/or B axis, or this is a single axis tomogram - init Any.
      cbInclude.setSelected(tool.isDirectiveIncluded(directive, null));
      if (values.isSet(null) || sourceAxisType == AxisType.DUAL_AXIS) {
        init(values.getValue(null), cbValue, ltfValue);
      }
      else {
        // If this is single axis and there is no value, use the value from A
        init(values.getValue(AxisID.FIRST), cbValue, ltfValue);
      }
      // Handle single axis:
      if (expandButton != null) {
        // Copy from Any to A and B so the checkpoint will be correct.
        updateMore();
        // just setVisible. Don't use expand because it calls updateLess and copies from A
        // to Any.
        setVisible(expandButton.isExpanded());
      }
    }
    // checkpoint
    // Always checkpoint Any even when it is disabled. When Any is disabled, only a change
    // in A or B will cause Any to change.
    if (cbValue != null) {
      cbValue.checkpoint();
    }
    else if (ltfValue != null) {
      ltfValue.checkpoint();
    }
    if (cbValueA != null) {
      cbValueA.checkpoint();
      cbValueB.checkpoint();
    }
    else if (ltfValueA != null) {
      ltfValueA.checkpoint();
      ltfValueB.checkpoint();
    }
  }

  /**
   * React to individual changes in the Include Directive panel.  Not for initialization.
   */
  void updateIncluded() {
    boolean include = cbInclude.isSelected();
    if (tool.isToggleDirectiveIncluded(directive, null, include)) {
      cbInclude.setSelected(!include);
    }
    if (cbIncludeA != null) {
      include = cbIncludeA.isSelected();
      if (tool.isToggleDirectiveIncluded(directive, AxisID.FIRST, include)) {
        cbIncludeA.setSelected(!include);
      }
      include = cbIncludeB.isSelected();
      if (tool.isToggleDirectiveIncluded(directive, AxisID.SECOND, include)) {
        cbIncludeB.setSelected(!include);
      }
    }
    updateDisplay();
  }

  Component getComponent() {
    return pnlRoot;
  }

  private void addListeners() {
    DirectiveListener listener = new DirectiveListener(this);
    cbInclude.addActionListener(listener);
    if (cbIncludeA != null) {
      cbIncludeA.addActionListener(listener);
      cbIncludeB.addActionListener(listener);
    }
  }

  boolean isInclude() {
    return cbInclude.isSelected()
        || (cbIncludeA != null && (cbIncludeA.isSelected() || cbIncludeB.isSelected()));
  }

  private void init(final Directive.Value value, final CheckBox cbValue,
      final LabeledTextField ltfValue) {
    if (cbValue != null) {
      boolean bValue = false;
      if (value != null) {
        bValue = value.toBoolean();
      }
      cbValue.setSelected(bValue);
    }
    else {
      String sValue = null;
      if (value != null) {
        sValue = value.toString();
      }
      ltfValue.setText(sValue);
    }
  }

  /**
   * - If A & B values are different, disable Any and delete its value.
   * - If A & B values are different and A or B is included, check Any.
   * - If A & B have the same value and only one of the them is checked:
   *   - disable Any
   *   - check its include checkbox
   *   - copy the value from A/B into it's value field.
   */
  private void updateLess() {
    boolean includedA = cbIncludeA.isSelected();
    boolean includedB = cbIncludeB.isSelected();
    boolean valueEqual = equalsValueAandB();
    if (includedA != includedB || !valueEqual) {
      // There is some difference between A and B.
      setEnabledAny(false);
      cbInclude.setSelected(includedA || includedB);
      if (!valueEqual) {
        // Any should be blank/false because A and B values are different
        resetAnyValue();
      }
      else {
        copyValueToAny();
      }
    }
    else {
      // A and B are the same - copy to Any
      setEnabledAny(true);
      copyToAny();
    }
  }

  /**
   * Copy Any onto A and B if Any is enabled.
   */
  public void updateMore() {
    if (cbInclude.isEnabled()) {
      copyFromAny(AxisID.FIRST);
      copyFromAny(AxisID.SECOND);
    }
  }

  /**
   * @return true if A and B values are the same, or if there are no A and B values.
   */
  private boolean equalsValueAandB() {
    if (cbValueA != null) {
      return cbValueA.isSelected() == cbValueB.isSelected();
    }
    if (ltfValueA != null) {
      String textA = ltfValueA.getText();
      String textB = ltfValueB.getText();
      if (textA == null || textB == null) {
        if (textA == null && textB == null) {
          return true;
        }
        return false;
      }
      return textA.equals(textB);
    }
    return true;
  }

  private void setEnabledAny(final boolean enable) {
    cbInclude.setEnabled(enable);
    if (cbValue != null) {
      cbValue.setEnabled(enable);
    }
    else {
      ltfValue.setEnabled(enable);
    }
  }

  private void resetAnyValue() {
    if (cbValue != null) {
      cbValue.setSelected(false);
    }
    else {
      ltfValue.setText("");
    }
  }

  private void copyValueToAny() {
    if (cbValue != null) {
      if (cbValueA != null) {
        cbValue.setSelected(cbValueA.isSelected());
      }
      else {
        resetAnyValue();
      }
    }
    else if (ltfValueA != null) {
      ltfValue.setText(ltfValueA.getText());
    }
    else {
      resetAnyValue();
    }
  }

  private void copyToAny() {
    if (cbIncludeA != null) {
      cbInclude.setSelected(cbIncludeA.isSelected());
    }
    else {
      cbInclude.setSelected(false);
    }
    copyValueToAny();
  }

  private void copyFromAny(final AxisID axisIDTo) {
    getInclude(axisIDTo).setSelected(cbInclude.isSelected());
    if (cbValue != null) {
      getCheckBoxValue(axisIDTo).setSelected(cbValue.isSelected());
    }
    else {
      getTextFieldValue(axisIDTo).setText(ltfValue.getText());
    }
  }

  private CheckBox getInclude(final AxisID axisID) {
    if (axisID == AxisID.FIRST && cbIncludeA != null) {
      return cbIncludeA;
    }
    if (axisID == AxisID.SECOND && cbIncludeB != null) {
      return cbIncludeB;
    }
    return cbInclude;
  }

  private CheckBox getCheckBoxValue(final AxisID axisID) {
    if (axisID == AxisID.FIRST && cbValueA != null) {
      return cbValueA;
    }
    if (axisID == AxisID.SECOND && cbValueB != null) {
      return cbValueB;
    }
    return cbValue;
  }

  private LabeledTextField getTextFieldValue(final AxisID axisID) {
    if (axisID == AxisID.FIRST && ltfValueA != null) {
      return ltfValueA;
    }
    if (axisID == AxisID.SECOND && ltfValueB != null) {
      return ltfValueB;
    }
    return ltfValue;
  }

  private void updateDisplay() {
    if (cbValue != null) {
      cbValue.setEnabled(cbInclude.isSelected() && cbInclude.isEnabled());
      if (cbValueA != null) {
        cbValueA.setEnabled(cbIncludeA.isSelected());
        cbValueB.setEnabled(cbIncludeB.isSelected());
      }
    }
    else {
      ltfValue.setEnabled(cbInclude.isSelected() && cbInclude.isEnabled());
      if (ltfValueA != null) {
        ltfValueA.setEnabled(cbIncludeA.isSelected());
        ltfValueB.setEnabled(cbIncludeB.isSelected());
      }
    }
  }

  public void expand(final ExpandButton button) {
    if (button != null) {
      if (!button.isExpanded()) {
        updateLess();
      }
      else {
        updateMore();
      }
      setVisible(button.isExpanded());
    }
  }

  public void setVisible(final boolean expanded) {
    pnlAny.setVisible(!expanded);
    if (pnlAxisA != null) {
      pnlAxisA.setVisible(expanded);
      pnlAxisB.setVisible(expanded);
    }
    updateDisplay();
  }

  public void expand(final GlobalExpandButton button) {
  }

  /**
   * Calls the root panel's setVisible function with a value based on showUnchanged,
   * showHidden, and whether any of the include checkboxes are selected and whether any
   * of the values have been changed.
   * @param showUnchanged
   * @param showHidden
   * @return true if pnlRoot is visible
   */
  boolean updateVisible() {
    boolean visible = tool.isDirectiveVisible(directive, cbInclude.isSelected()
        || (cbIncludeA != null && cbIncludeA.isSelected())
        || (cbIncludeB != null && cbIncludeB.isSelected()),
        (cbValue != null && cbValue.isDifferentFromCheckpoint())
            || (cbValueA != null && cbValueA.isDifferentFromCheckpoint())
            || (cbValueB != null && cbValueB.isDifferentFromCheckpoint())
            || (ltfValue != null && ltfValue.isDifferentFromCheckpoint())
            || (ltfValueA != null && ltfValueA.isDifferentFromCheckpoint())
            || (ltfValueB != null && ltfValueB.isDifferentFromCheckpoint()));
    if (pnlRoot.isVisible() != visible) {
      pnlRoot.setVisible(visible);
    }
    return visible;
  }

  private void action(final ActionEvent event) {
    updateDisplay();
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
    if (cbIncludeA != null) {
      tooltip = directive.getName(AxisID.FIRST) + ":  " + description;
      cbIncludeA.setToolTipText(tooltip);
      if (cbValueA != null) {
        cbValueA.setToolTipText(tooltip);
      }
      else {
        ltfValueA.setToolTipText(tooltip);
      }
      tooltip = directive.getName(AxisID.SECOND) + ":  " + description;
      cbIncludeB.setToolTipText(tooltip);
      if (cbValueB != null) {
        cbValueB.setToolTipText(tooltip);
      }
      else {
        ltfValueB.setToolTipText(tooltip);
      }
    }
  }

  private static final class DirectiveListener implements ActionListener {
    private final DirectiveSetPanel panel;

    private DirectiveListener(final DirectiveSetPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event);
    }
  }
}
