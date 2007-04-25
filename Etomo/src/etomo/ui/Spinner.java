package etomo.ui;

import java.awt.Component;

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

  Spinner(String label) {
    String name = Utilities.convertLabelToName(label);
    spinner.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.SPINNER.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  void setToolTipText(String text) {
    spinner.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  Component getComponent() {
    return spinner;
  }

  void setEnabled(boolean enabled) {
    spinner.setEnabled(enabled);
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
