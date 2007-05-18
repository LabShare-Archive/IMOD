package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;

import javax.swing.BoxLayout;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.ConstEtomoNumber;
import etomo.type.ParsedElement;
import etomo.type.UITestField;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.4  2007/05/01 22:30:14  sueh
 * <p> bug# 964 Added getLabeledInstance.
 * <p>
 * <p> Revision 1.3  2007/04/13 21:52:51  sueh
 * <p> bug# 964 Added setValue(ParsedElement).
 * <p>
 * <p> Revision 1.2  2007/04/09 21:22:34  sueh
 * <p> bug# 964 Added Number getValue().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:53:15  sueh
 * <p> bug# 964 A self-naming unlabeled spinner.
 * <p> </p>
 */
final class Spinner {
  public static final String rcsid = "$Id$";

  private final SpinnerNumberModel model;
  private final JSpinner spinner;
  private JPanel panel = null;
  private JLabel label = null;

  private Spinner(final String label, final boolean labeled, final int value,
      final int minimum, final int maximum) {
    model = new SpinnerNumberModel(value, minimum, maximum, 1);
    spinner = new JSpinner(model);
    String name = Utilities.convertLabelToName(label);
    spinner.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.SPINNER.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
    if (labeled) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      this.label = new JLabel(label);
      panel.add(this.label);
      panel.add(spinner);
    }
    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    Dimension maxSize = spinner.getMaximumSize();
    if (this.label != null
        && this.label.getFont().getSize() > spinner.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2 * this.label
          .getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * spinner
          .getFont().getSize());
    }
    spinner.setMaximumSize(maxSize);
  }
  
  private final JFormattedTextField getTextField() {
    return ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
  }

  static Spinner getInstance(final String name) {
    return new Spinner(name, false, 1, 1, 1);
  }

  static Spinner getLabeledInstance(final String label) {
    return new Spinner(label, true, 1, 1, 1);
  }

  static Spinner getLabeledInstance(final String label, final int value,
      final int minimum, final int maximum) {
    return new Spinner(label, true, value, minimum, maximum);
  }

  void setToolTipText(final String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    spinner.setToolTipText(tooltip);
    if (label != null) {
      label.setToolTipText(tooltip);
    }
  }

  Container getContainer() {
    if (panel == null) {
      return spinner;
    }
    return panel;
  }

  void setEnabled(final boolean enabled) {
    spinner.setEnabled(enabled);
    if (label != null) {
      label.setEnabled(enabled);
    }
  }

  void setMax(final int max) {
    model.setMaximum(new Integer(max));
  }

  void setValue(final ParsedElement value) {
    if (value.isEmpty()) {
      spinner.setValue((Integer) model.getMinimum());
    }
    else {
      spinner.setValue(value.getRawNumber());
    }
  }

  void setValue(final ConstEtomoNumber value) {
    if (value.isNull()) {
      spinner.setValue((Integer) model.getMinimum());
    }
    else {
      spinner.setValue(value.getNumber());
    }
  }

  Number getValue() {
    return (Number) spinner.getValue();
  }
}
