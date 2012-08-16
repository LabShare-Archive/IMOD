package etomo.ui.swing;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import etomo.type.ConstEtomoNumber;

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
 * <p> Revision 1.3  2011/05/31 21:08:10  sueh
 * <p> bug# 1460 In getContainer, used glue to prevent the spinner from expanding to its widest extent.
 * <p> Added setToolTipText.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:05:39  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2007/03/01 01:28:21  sueh
 * <p> bug# 964 Made colors constant and moved them to Colors.
 * <p>
 * <p> Revision 1.2  2007/02/09 00:47:57  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.1  2007/02/05 23:35:33  sueh
 * <p> bug# 962 A composite ui class containing a checkbox and a spinner.
 * <p> </p>
 */

final class CheckBoxSpinner {
  public static final String rcsid = "$Id$";

  private final Spinner spinner;
  private final CheckBox checkBox;

  private JPanel panel = null;
  private Color panelBackground = null;
  private Color panelHighlightBackground = null;

  CheckBoxSpinner(String text) {
    checkBox = new CheckBox(text);
    spinner = Spinner.getInstance(checkBox.getText());
    spinner.setEnabled(false);
    checkBox.addActionListener(new CheckBoxSpinnerActionListener(this));
  }

  CheckBoxSpinner(String text, final int value, final int minimum, final int maximum) {
    checkBox = new CheckBox(text);
    spinner = Spinner.getInstance(text, value, minimum, maximum);
    spinner.setEnabled(false);
    checkBox.addActionListener(new CheckBoxSpinnerActionListener(this));
  }

  Container getContainer() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add(Box.createHorizontalGlue());
      panel.add(checkBox);
      panel.add(spinner.getContainer());
      panel.add(Box.createHorizontalGlue());
    }
    return panel;
  }

  void enableSpinner() {
    spinner.setEnabled(checkBox.isSelected() && checkBox.isEnabled());
  }

  void setCheckBoxEnabled(final boolean enabled) {
    checkBox.setEnabled(enabled);
  }

  void setModel(SpinnerNumberModel model) {
    spinner.setModel(model);
  }

  void setMax(final int max) {
    spinner.setMax(max);
  }

  void setMaximumSize(Dimension maximumSize) {
    spinner.setMaximumSize(maximumSize);
  }

  Number getValue() {
    return spinner.getValue();
  }

  boolean isSelected() {
    return checkBox.isSelected();
  }

  void setSelected(boolean selected) {
    checkBox.setSelected(selected);
    enableSpinner();
  }

  void setValue(int value) {
    spinner.setValue(value);
  }

  void setValue(ConstEtomoNumber value) {
    spinner.setValue(value);
  }

  void setHighlight(boolean highlight) {
    if (panelBackground == null) {
      panelBackground = panel.getBackground();
      // greying out the highlight color to match the panel's original color
      panelHighlightBackground = Colors.subtractColor(Colors.HIGHLIGHT_BACKGROUND,
          Colors.subtractColor(Colors.BACKGROUND, panelBackground));
    }
    if (highlight) {
      checkBox.setBackground(panelHighlightBackground);
    }
    else {
      checkBox.setBackground(panelBackground);
    }
    getContainer();
    UIUtilities.highlightJTextComponents(highlight, panel);
  }

  void setToolTipText(final String text) {
    setCheckBoxToolTipText(text);
    setSpinnerToolTipText(text);
  }

  void setCheckBoxToolTipText(String text) {
    checkBox.setToolTipText(text);
  }

  void setSpinnerToolTipText(String text) {
    spinner.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  private class CheckBoxSpinnerActionListener implements ActionListener {

    CheckBoxSpinner adaptee;

    CheckBoxSpinnerActionListener(CheckBoxSpinner adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.enableSpinner();
    }
  }
}
