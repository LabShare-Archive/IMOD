package etomo.ui.swing;

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
import etomo.type.EtomoNumber;
import etomo.type.ParsedElement;
import etomo.type.UITestFieldType;
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
 * <p> Revision 1.2  2011/02/22 21:37:35  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.17  2010/03/03 05:07:01  sueh
 * <p> bug# 1311 Added getLabel.
 * <p>
 * <p> Revision 1.16  2009/11/20 17:36:44  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.15  2009/01/20 20:29:33  sueh
 * <p> bug# 1102 Changed UITestField to UITestFieldType.
 * <p>
 * <p> Revision 1.14  2009/01/13 19:40:12  sueh
 * <p> bug# 1170 Added a static getInstance function which takes value, min, and max.
 * <p>
 * <p> Revision 1.13  2008/09/10 21:36:49  sueh
 * <p> bug# 1135 In setValue(ParsedElement) handle possibility that the
 * <p> parameter may be null.
 * <p>
 * <p> Revision 1.12  2008/08/22 17:52:30  sueh
 * <p> bug# 1136 Added isEnabled and setValue(int).
 * <p>
 * <p> Revision 1.11  2008/05/30 22:36:18  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
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

  private final JSpinner spinner;
  private final Number defaultValue;
  private final boolean labeled;
  private final int minimum;

  private SpinnerNumberModel model = null;
  private JPanel panel = null;
  private JLabel label = null;
  private boolean debug = false;

  private int maximum;

  private Spinner(final String text, final boolean labeled, final int value,
      final int minimum, final int maximum, int step) {
    this.labeled = labeled;
    this.minimum = minimum;
    this.maximum = maximum;
    model = new SpinnerNumberModel(value, minimum, maximum, step);
    spinner = new JSpinner(model);
    this.defaultValue = new Integer(value);
    if (labeled) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      this.label = new JLabel(text);
      panel.add(this.label);
      panel.add(spinner);
    }
    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    Dimension maxSize = spinner.getMaximumSize();
    if (label != null && label.getFont().getSize() > spinner.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2 * label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * spinner.getFont().getSize());
    }
    spinner.setMaximumSize(maxSize);
    setName(text);
  }

  void setName(final String text) {
    String name = UITestFieldType.SPINNER.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + Utilities.convertLabelToName(text);
    spinner.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(spinner.getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }

  void setModel(SpinnerNumberModel input) {
    model = input;
    spinner.setModel(model);
  }

  private final JFormattedTextField getTextField() {
    return ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
  }

  static Spinner getInstance(final String text) {
    return new Spinner(text, false, 1, 1, 1, 1);
  }

  static Spinner getInstance(final String text, final int value, final int minimum,
      final int maximum) {
    return new Spinner(text, false, value, minimum, maximum, 1);
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

  String getLabel() {
    return label.getText();
  }

  boolean isEnabled() {
    return spinner.isEnabled();
  }

  void setEnabled(final boolean enabled) {
    spinner.setEnabled(enabled);
    if (label != null) {
      label.setEnabled(enabled);
    }
  }

  void setVisible(final boolean visible) {
    if (panel == null) {
      spinner.setVisible(visible);
    }
    else {
      panel.setVisible(visible);
    }
  }

  boolean isVisible() {
    return panel.isVisible();
  }

  void setMaximumSize(Dimension maximumSize) {
    spinner.setMaximumSize(maximumSize);
  }

  void reset() {
    spinner.setValue(defaultValue);
  }

  void setMax(final int max) {
    maximum = max;
    model.setMaximum(new Integer(max));
  }

  void setDebug(final boolean input) {
    debug = input;
  }

  void setValue(final ParsedElement value) {
    if (value == null || value.isEmpty()) {
      spinner.setValue((Integer) model.getMinimum());
    }
    else {
      Number rawNumber = value.getRawNumber();
      if (rawNumber != null) {
        spinner.setValue(rawNumber);
      }
      else {
        spinner.setValue((Integer) model.getMinimum());
      }
    }
  }

  void setValue(final int value) {
    if (value == EtomoNumber.INTEGER_NULL_VALUE) {
      spinner.setValue((Integer) model.getMinimum());
    }
    else {
      spinner.setValue(new Integer(value));
    }
  }

  void setValue(final String value) {
    EtomoNumber nValue = new EtomoNumber();
    nValue.set(value);
    setValue(nValue);
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
