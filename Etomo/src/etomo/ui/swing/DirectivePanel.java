package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.Arguments.DebugLevel;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.logic.DirectiveTool;
import etomo.logic.FieldValidator;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrFile;
import etomo.storage.DirectiveType;
import etomo.storage.DirectiveValueType;
import etomo.storage.DirectiveValues;
import etomo.type.AxisType;
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
final class DirectivePanel {
  public static final String rcsid = "$Id:$";

  private static final int NO_INDEX = 1;
  private static final int YES_INDEX = 0;

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbInclude = new CheckBox(" -");

  private final ComboBox cbValue;
  private final LabeledTextField ltfValue;
  private final SimpleButton sbFileValue;
  private final DirectiveTool tool;
  private final Directive directive;
  private final FieldType fieldType;
  private final String actionCommand;
  private final AxisType sourceAxisType;
  private final String[] valueList;
  private final boolean booleanValueType;

  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  private File lastFileChooserLocation = null;

  private DirectivePanel(final BaseManager manager, final Directive directive,
      final DirectiveTool tool, final AxisType sourceAxisType) {
    this.directive = directive;
    this.tool = tool;
    this.sourceAxisType = sourceAxisType;
    fieldType = FieldType.getInstance(directive.getValueType());
    // Initialize the value field that matches the value type. Set the other one to null.
    DirectiveType type = directive.getType();
    String title = directive.getTitle() + ":  ";
    // Need to be able to distinguish the include checkbox action commands from the three
    // directives in each directive set.
    actionCommand = "include";
    cbInclude.setActionCommand(actionCommand);
    DirectiveValueType valueType = directive.getValueType();
    FieldType fieldType = FieldType.getInstance(valueType);
    booleanValueType = valueType == DirectiveValueType.BOOLEAN;
    if (booleanValueType || directive.isChoiceList()) {
      if (booleanValueType) {
        cbValue = ComboBox.getInstance(title, false);
        cbValue.addItem("Yes");
        cbValue.addItem("No");
        valueList = null;
      }
      else {
        cbValue = ComboBox.getInstance(title, true);
        DirectiveDescrFile.ChoiceList choiceList = directive.getChoiceList();
        int size = choiceList.size();
        valueList = new String[size];
        for (int i = 0; i < size; i++) {
          cbValue.addItem(choiceList.getDescr(i));
          valueList[i] = choiceList.getValue(i);
        }
      }
      ltfValue = null;
      sbFileValue = null;
    }
    else {
      ltfValue = new LabeledTextField(fieldType, title);
      cbValue = null;
      valueList = null;
      if (valueType == DirectiveValueType.FILE) {
        sbFileValue = new SimpleButton(new ImageIcon(
            ClassLoader.getSystemResource("images/openFile.gif")));
      }
      else {
        sbFileValue = null;
      }
    }
  }

  static DirectivePanel getInstance(final BaseManager manager, final Directive directive,
      final DirectiveTool tool, final AxisType sourceAxisType) {
    DirectivePanel instance = new DirectivePanel(manager, directive, tool, sourceAxisType);
    instance.createPanel(directive);
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel(final Directive directive) {
    // init
    if (sbFileValue != null) {
      sbFileValue.setPreferredSize(FixedDim.folderButton);
      sbFileValue.setMaximumSize(FixedDim.folderButton);
    }
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.add(cbInclude);
    if (cbValue != null) {
      pnlRoot.add(cbValue.getComponent());
    }
    else {
      pnlRoot.add(ltfValue.getComponent());
      if (sbFileValue != null) {
        pnlRoot.add(sbFileValue);
      }
    }
    pnlRoot.add(Box.createHorizontalGlue());
    init();
  }

  public Component getComponent() {
    return pnlRoot;
  }

  private void addListeners() {
    cbInclude.addActionListener(new DirectiveListener(this));
    if (sbFileValue != null) {
      sbFileValue.addActionListener(new DirectiveFileValueListener(this));
    }
  }

  public Directive getState() {
    setStateInDirective();
    return directive;
  }

  void setStateInDirective() {
    directive.setInclude(isInclude());
    if (cbValue != null) {
      if (booleanValueType) {
        directive.setValue(cbValue.getSelectedIndex() == YES_INDEX);
      }
      else {
        int index = cbValue.getSelectedIndex();
        if (index >= 0) {
          directive.setValue(valueList[index]);
        }
        else {
          // Handle empty pulldown choice
          directive.setValue((String) null);
        }
      }
    }
    else {
      directive.setValue(ltfValue.getText());
    }
  }

  /**
   * This class should call this function for solo directives.
   * DirectiveSet should only call this function for the Any directive.  The Any instance
   * function calls init() for the A and B directives.
   */
  void init() {
    if (ltfValue != null) {
      ltfValue.setColumns(directive.getValueType().getColumns());
    }
    // Initialize directive - set include, set and checkpoint the value, and set root
    // panel visibility.
    setIncluded();
    initValue(directive);
    tool.setDebug(debug);
    if (!tool.isDirectiveVisible(directive, cbInclude.isSelected(), false)) {
      pnlRoot.setVisible(false);
    }
    tool.resetDebug();
    checkpoint();
  }

  /**
  * Responds to selection changes in the include checkbox.
  */
  private void action() {
    enableValue();
  }

  private void fileValueAction() {
    JFileChooser chooser = UIHarness.INSTANCE.getFileChooser();
    if (chooser == null) {
      return;
    }
    if (!ltfValue.isEmpty()) {
      chooser.setCurrentDirectory(new File(ltfValue.getText()));
    }
    else if (lastFileChooserLocation != null) {
      chooser.setCurrentDirectory(lastFileChooserLocation);
    }
    else if (directive.isCopyArg()) {
      chooser.setCurrentDirectory(EtomoDirector.INSTANCE.getIMODCalibDirectory());
    }
    else {
      String home = EtomoDirector.INSTANCE.getHomeDirectory();
      if (home != null) {
        chooser.setCurrentDirectory(new File(home));
      }
    }
    chooser.setDialogTitle("Open " + directive.getTitle());
    if (chooser.showOpenDialog(pnlRoot) == JFileChooser.APPROVE_OPTION) {
      ltfValue.setText(chooser.getSelectedFile());
    }
    lastFileChooserLocation = chooser.getCurrentDirectory();
  }

  /**
   * For solo directives function changes the include checkbox and root panel visibility.  For
   * the Any directive in a set, it changes the A and B include checkboxes, and updates
   * the include checkbox in the Any directive.  The Any checkbox is
   * derived from the A and B settings.  Function does nothing when called by A or B
   * directives directly.
   * @param includeChange - don't change the include checkbox selection status, just change visibility
   * @param expandChange - no effect
   * @return the solo instance visibility, or true
   */
  public boolean msgControlChanged(final boolean includeChange, final boolean expandChange) {
    if (includeChange) {
      setIncluded();
    }
    tool.setDebug(debug);
    boolean visible = tool.isDirectiveVisible(directive, cbInclude.isSelected(),
        isDifferentFromCheckpoint(false));
    pnlRoot.setVisible(visible);
    return visible;
  }

  /**
   * copy include and value and enable/disable value
   * Assumes the same type of value.
   * @param input
   */
  private void copy(final DirectivePanel input) {
    if (input == null) {
      cbInclude.setSelected(false);
    }
    else {
      cbInclude.setSelected(input.cbInclude.isSelected());
    }
    enableValue();
    copyValue(input);
  }

  private void copyValue(final DirectivePanel input) {
    if (input == null) {
      if (cbValue != null) {
        if (booleanValueType) {
          cbValue.setSelectedIndex(NO_INDEX);
        }
        else {
          cbValue.setSelectedIndex(-1);
        }
      }
      else {
        ltfValue.setText("");
      }
    }
    else {
      if (cbValue != null) {
        cbValue.setSelectedIndex(input.cbValue.getSelectedIndex());
      }
      else {
        ltfValue.setText(input.ltfValue.getText());
      }
    }
  }

  public void checkpoint() {
    // Checkpoint include to use when closing.
    cbInclude.checkpoint();
    // Checkpoint value to use when closing and to for deciding whether the directive
    // should be visible.
    if (cbValue != null) {
      cbValue.checkpoint();
    }
    else if (ltfValue != null) {
      ltfValue.checkpoint();
    }
  }

  private void enableValue() {
    boolean enable = cbInclude.isSelected() && cbInclude.isEnabled();
    if (cbValue != null) {
      cbValue.setEnabled(enable);
    }
    else {
      ltfValue.setEnabled(enable);
      if (sbFileValue != null) {
        sbFileValue.setEnabled(enable);
      }
    }
  }

  private boolean equals(final DirectivePanel input) {
    if (input == null) {
      return false;
    }
    if (cbInclude.isSelected() != input.cbInclude.isSelected()) {
      return false;
    }
    return equalsValue(input);
  }

  private boolean equalsValue(final DirectivePanel input) {
    if (input == null) {
      return false;
    }
    if (cbValue != null) {
      if (cbValue.getSelectedIndex() != input.cbValue.getSelectedIndex()) {
        return false;
      }
    }
    else if (!FieldValidator.equals(fieldType, ltfValue.getText(),
        input.ltfValue.getText())) {
      return false;
    }
    return true;
  }

  private void initValue(final Directive directive) {
    DirectiveValues values = directive.getValues();
    Directive.Value value = values.getValue();
    if (value != null) {
      if (cbValue != null) {
        if (booleanValueType) {
          cbValue.setSelectedIndex(value.toBoolean() ? YES_INDEX : NO_INDEX);
        }
        else {
          // Find the matching value and display for corresponding description.
          int index = -1;
          String sValue = value.toString();
          if (sValue != null && !sValue.matches("\\s*")) {
            for (int i = 0; i < valueList.length; i++) {
              if (valueList[i].equals(sValue)) {
                index = i;
                break;
              }
            }
          }
          cbValue.setSelectedIndex(index);
        }
      }
      else {
        ltfValue.setText(value.toString());
      }
    }
  }

  public boolean isDifferentFromCheckpoint(final boolean checkInclude) {
    if (checkInclude && cbInclude.isDifferentFromCheckpoint(true)) {
      return true;
    }
    if (cbValue != null) {
      return cbValue.isDifferentFromCheckpoint(true);
    }
    return ltfValue.isDifferentFromCheckpoint(true);
  }

  public boolean isEnabled() {
    return cbInclude.isEnabled();
  }

  /**
  * @return true if directive will be included in a file (checkbox selected and enabled).
  */
  public boolean isInclude() {
    return cbInclude.isSelected() && cbInclude.isEnabled();
  }

  public boolean isVisible() {
    return pnlRoot.isVisible();
  }

  private void resetValue() {
    if (cbValue != null) {
      if (booleanValueType) {
        cbValue.setSelectedIndex(NO_INDEX);
      }
      else {
        cbValue.setSelectedIndex(-1);
      }
    }
    else {
      ltfValue.setText("");
    }
  }

  private void setEnabled(final boolean enable) {
    cbInclude.setEnabled(enable);
    enableValue();
  }

  /**
   * Changes cbInclude if a checked include or exclude checkbox contradicts it (after the
   * precedense of the include/excluded checkboxes are taken into account.  Unchecked
   * include/exclude checkboxes should have no effect.
   */
  private void setIncluded() {
    boolean included = cbInclude.isSelected();
    tool.setDebug(debug);
    if (tool.isToggleDirectiveIncluded(directive, included)) {
      cbInclude.setSelected(!included);
    }
    tool.resetDebug();
    enableValue();
  }

  void setVisible(final boolean input) {
    pnlRoot.setVisible(input);
  }

  public String toString() {
    return directive.getTitle();
  }

  private void setTooltips() {
    DirectiveValues values = directive.getValues();
    String valueString = null;
    Directive.Value value = values.getValue();
    if (value != null) {
      valueString = value.toString();
    }
    String defaultValueString = null;
    value = values.getDefaultValue();
    if (value != null) {
      defaultValueString = value.toString();
    }
    String debugString = "";
    if (debug.isOn()) {
      debugString = "  Type:" + directive.getValueType() + ", Batch:"
          + directive.isBatch() + ", Tmplt:" + directive.isTemplate() + ", eTomo:"
          + directive.getEtomoColumn() + ", AxisLevelData:"
          + directive.getInDirectiveFileDebugString();
    }
    String tooltip = directive.getKeyDescription() + ":  " + directive.getDescription()
        + "." + (valueString != null ? "  Dataset value:" + valueString : "")
        + (defaultValueString != null ? "  Original value:" + defaultValueString : "")
        + debugString;
    cbInclude.setToolTipText(tooltip);
    if (cbValue != null) {
      cbValue.setToolTipText(tooltip);
    }
    else {
      ltfValue.setToolTipText(tooltip);
      if (sbFileValue != null) {
        sbFileValue.setToolTipText(TooltipFormatter.INSTANCE.format(tooltip));
      }
    }
  }

  private static final class DirectiveListener implements ActionListener {
    private final DirectivePanel panel;

    private DirectiveListener(final DirectivePanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action();
    }
  }

  private static final class DirectiveFileValueListener implements ActionListener {
    private final DirectivePanel panel;

    private DirectiveFileValueListener(final DirectivePanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.fileValueAction();
    }
  }
}
