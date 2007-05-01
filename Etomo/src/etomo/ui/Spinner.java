package etomo.ui;

import java.awt.Component;

import javax.swing.BoxLayout;
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

  private SpinnerNumberModel model = new SpinnerNumberModel(1, 1, 1, 1);
  private JSpinner spinner = new JSpinner(model);
  private JPanel panel = null;
  private JLabel label = null;

  private Spinner(String label, boolean labeled) {
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
  }

  static Spinner getInstance(String name) {
    return new Spinner(name, false);
  }

  static Spinner getLabeledInstance(String label) {
    return new Spinner(label, true);
  }

  void setToolTipText(String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    spinner.setToolTipText(tooltip);
    if (label!=null) {
      label.setToolTipText(tooltip);
    }
  }

  Component getComponent() {
    if (panel == null) {
      return spinner;
    }
    return panel;
  }

  void setEnabled(boolean enabled) {
    spinner.setEnabled(enabled);
    if (label!=null) {
      label.setEnabled(enabled);
    }
  }

  void setMax(int max) {
    model.setMaximum(new Integer(max));
  }

  void setValue(ParsedElement value) {
    if (value.isEmpty()) {
      spinner.setValue((Integer) model.getMinimum());
    }
    else {
      spinner.setValue(value.getRawNumber());
    }
  }

  void setValue(ConstEtomoNumber value) {
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
