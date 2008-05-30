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
 * <p> Revision 1.10  2008/05/30 21:34:15  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.9  2007/12/26 22:34:45  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.8  2007/11/06 20:32:46  sueh
 * <p> bug# 1047 Allowed the step to be set in the constructor.
 * <p>
 * <p> Revision 1.7  2007/09/07 00:29:00  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.6  2007/06/08 22:22:21  sueh
 * <p> bug# 1014 Added reset().
 * <p>
 * <p> Revision 1.5  2007/05/18 23:54:07  sueh
 * <p> bug# 987 Added getLabeledInstance() for a non-default model.
 * <p>
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
  private final Number defaultValue;
  private JPanel panel = null;
  private JLabel label = null;

  private Spinner(final String label, final boolean labeled, final int value,
      final int minimum, final int maximum, int step) {
    model = new SpinnerNumberModel(value, minimum, maximum, step);
    spinner = new JSpinner(model);
    this.defaultValue = new Integer(value);
    String name = Utilities.convertLabelToName(label);
    spinner.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
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
      maxSize.setSize(maxSize.getWidth(), 2 * this.label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * spinner.getFont().getSize());
    }
    spinner.setMaximumSize(maxSize);
  }

  private final JFormattedTextField getTextField() {
    return ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
  }

  static Spinner getInstance(final String name) {
    return new Spinner(name, false, 1, 1, 1, 1);
  }

  static Spinner getLabeledInstance(final String label) {
    return new Spinner(label, true, 1, 1, 1, 1);
  }

  static Spinner getLabeledInstance(final String label, final int value,
      final int minimum, final int maximum) {
    return new Spinner(label, true, value, minimum, maximum, 1);
  }

  static Spinner getLabeledInstance(final String label, final int value,
      final int minimum, final int maximum, final int step) {
    return new Spinner(label, true, value, minimum, maximum, step);
  }

  void setToolTipText(final String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    spinner.setToolTipText(tooltip);
    if (label != null) {
      label.setToolTipText(tooltip);
    }
  }

  void setAlignmentX(float alignmentX) {
    panel.setAlignmentX(alignmentX);
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

  void reset() {
    spinner.setValue(defaultValue);
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
