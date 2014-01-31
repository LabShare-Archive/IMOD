package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.Arguments.DebugLevel;
import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
* <p>Description: </p>
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
* <p> $Log$
* <p> Revision 1.2  2010/12/05 05:01:29  sueh
* <p> bug# 1416 Added ComboBox(String).
* <p>
* <p> Revision 1.1  2010/11/13 16:07:35  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 3.2  2009/11/20 17:02:34  sueh
* <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
* <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
* <p> "bn." field command.
* <p>
* <p> Revision 3.1  2009/09/01 03:18:25  sueh
* <p> bug# 1222
* <p> </p>
*/

final class ComboBox {
  public static final String rcsid = "$Id$";

  private final JComboBox comboBox;
  private final JLabel label;
  private final JPanel pnlRoot;
  final boolean addEmptyChoice;// Causes the index to be off by one

  private boolean checkpointed =false;
  private int checkpointIndex = -1;
  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  private ComboBox(final String name, final boolean labeled, final boolean addEmptyChoice) {
    this.addEmptyChoice = addEmptyChoice;
    comboBox = new JComboBox();
    setName(name);
    if (labeled) {
      label = new JLabel(name);
      pnlRoot = new JPanel();
    }
    else {
      label = null;
      pnlRoot = null;
    }
  }

  static ComboBox getInstance(final String name, final boolean addEmptyChoice) {
    ComboBox instance = new ComboBox(name, true, addEmptyChoice);
    instance.createPanel();
    return instance;
  }

  static ComboBox getUnlabeledInstance(final String name) {
    ComboBox instance = new ComboBox(name, false, false);
    instance.createPanel();
    return instance;
  }

  static ComboBox getUnlabeledInstance(JLabel label) {
    ComboBox instance = new ComboBox(label.getText(), false, false);
    instance.createPanel();
    return instance;
  }

  private void createPanel() {
    if (pnlRoot != null) {
      pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
      pnlRoot.add(Box.createRigidArea(FixedDim.x2_y0));
      pnlRoot.add(label);
      pnlRoot.add(Box.createRigidArea(FixedDim.x3_y0));
      pnlRoot.add(comboBox);
      pnlRoot.add(Box.createRigidArea(FixedDim.x2_y0));
    }
  }

  void addActionListener(final ActionListener listener) {
    comboBox.addActionListener(listener);
  }

  void addFocusListener(final FocusListener listener) {
    comboBox.addFocusListener(listener);
  }

  void addItem(final Object input) {
    if (addEmptyChoice && comboBox.getItemCount() == 0) {
      comboBox.addItem(null);
    }
    comboBox.addItem(input);
  }

  String getActionCommand() {
    return comboBox.getActionCommand();
  }

  Component getComponent() {
    if (pnlRoot != null) {
      return pnlRoot;
    }
    return comboBox;
  }

  /**
   * Returns the selected index.  If addEmptyChoice is on, then the index is adjusted so
   * that it starts from zero.  If the empty choice was selected it returns -1.
   * @return
   */
  int getSelectedIndex() {
    int index = comboBox.getSelectedIndex();
    if (addEmptyChoice && index > -1) {
      return index - 1;
    }
    return index;
  }

  Object getSelectedItem() {
    return comboBox.getSelectedItem();
  }

  void removeAllItems() {
    comboBox.removeAllItems();
  }

  boolean isEnabled() {
    return comboBox.isEnabled();
  }

  void setEnabled(final boolean enabled) {
    if (label != null) {
      label.setEnabled(enabled);
    }
    comboBox.setEnabled(enabled);
  }

  void setComboBoxEnabled(final boolean enabled) {
    comboBox.setEnabled(enabled);
  }

  String getLabel() {
    return label.getText();
  }

  void setDebug(final DebugLevel input) {
    debug = input;
  }

  void setEditable(final boolean editable) {
    comboBox.setEditable(editable);
  }

  void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    comboBox.setName(UITestFieldType.COMBO_BOX.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(comboBox.getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }

  /**
   * Saves the current selected index as the checkpoint.
   */
  void checkpoint() {
    checkpointed = true;
    checkpointIndex = getSelectedIndex();
  }

  /**
   * 
   * @param alwaysCheck - check for difference even when the field is disables or invisible
   * @return
   */
  boolean isDifferentFromCheckpoint(final boolean alwaysCheck) {
    if (!alwaysCheck && (!isEnabled() || !isVisible())) {
      return false;
    }
    if (!checkpointed) {
      return true;
    }
    return checkpointIndex != getSelectedIndex();
  }

  public boolean isVisible() {
    return comboBox.isVisible();
  }

  /**
   * Selects an item.  If addEmptyChoice is on, it adjusts for it, so that a zero index
   * refers to first non-empty choice.
   * @param index
   */
  void setSelectedIndex(int index) {
    if (addEmptyChoice) {
      index++;
    }
    comboBox.setSelectedIndex(index);
  }

  void setToolTipText(final String tooltip) {
    if (label != null) {
      label.setToolTipText(tooltip);
    }
    comboBox.setToolTipText(tooltip);
  }
}
