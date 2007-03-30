package etomo.ui;

import java.awt.Component;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
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
 * <p> $Log$ </p>
 */
final class Spinner {
  public static final String rcsid = "$Id$";
  
  private SpinnerNumberModel model = new SpinnerNumberModel(1,1,1,1);
  private JSpinner spinner=new JSpinner(model);

  Spinner(String label) {
    String name = Utilities.convertLabelToName(label);
    spinner.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.SPINNER.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
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
  
  public void setToolTipText(String text) {
    spinner.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }
}
