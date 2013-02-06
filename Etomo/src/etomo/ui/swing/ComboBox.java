package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionListener;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

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

  private ComboBox(final String name, final boolean labeled) {
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

  static ComboBox getInstance(String name) {
    return new ComboBox(name, true);
  }

  static ComboBox getUnlabeledInstance(final String name) {
    return new ComboBox(name, false);
  }

  static ComboBox getUnlabeledInstance(JLabel label) {
    return new ComboBox(label.getText(), false);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    comboBox.setName(UITestFieldType.COMBO_BOX.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(comboBox.getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }

  public Component getComponent() {
    if (pnlRoot != null) {
      return pnlRoot;
    }
    return comboBox;
  }

  public void addActionListener(final ActionListener listener) {
    comboBox.addActionListener(listener);
  }

  public void addItem(final Object input) {
    comboBox.addItem(input);
  }

  public String getActionCommand() {
    return comboBox.getActionCommand();
  }

  public int getSelectedIndex() {
    return comboBox.getSelectedIndex();
  }

  public Object getSelectedItem() {
    return comboBox.getSelectedItem();
  }

  public void removeAll() {
    comboBox.removeAll();
  }

  public void removeAllItems() {
    comboBox.removeAllItems();
  }

  public void setEditable(final boolean editable) {
    comboBox.setEditable(editable);
  }

  public void setEnabled(final boolean enabled) {
    comboBox.setEnabled(enabled);
  }

  public void setSelectedIndex(final int index) {
    comboBox.setSelectedIndex(index);
  }

  public void setToolTipText(final String tooltip) {
    comboBox.setToolTipText(tooltip);
  }
}
