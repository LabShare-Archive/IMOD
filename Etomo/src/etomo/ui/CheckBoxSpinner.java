package etomo.ui;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;

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

  private final JSpinner spinner = new JSpinner();
  private final CheckBox checkBox;
  private JPanel panel = null;
  private Color panelBackground = null;
  private Color panelHighlightBackground = null;

  CheckBoxSpinner(String text) {
    checkBox = new CheckBox(text);
    spinner.setName(checkBox.getName());
    spinner.setEnabled(false);
    checkBox.addActionListener(new CheckBoxSpinnerActionListener(this));
  }

  Container getContainer() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add(checkBox);
      panel.add(spinner);
    }
    return panel;
  }

  void setText(String text) {
    checkBox.setText(text);
    spinner.setName(checkBox.getName());
  }

  void setName(String text) {
    checkBox.setName(text);
    spinner.setName(checkBox.getName());
  }

  void enableSpinner() {
    spinner.setEnabled(checkBox.isSelected());
  }

  void setModel(SpinnerModel model) {
    spinner.setModel(model);
  }

  void setMaximumSize(Dimension maximumSize) {
    spinner.setMaximumSize(maximumSize);
  }

  Object getValue() {
    return spinner.getValue();
  }

  boolean isSelected() {
    return checkBox.isSelected();
  }

  void setSelected(boolean selected) {
    checkBox.setSelected(selected);
    enableSpinner();
  }

  void setValue(Object value) {
    spinner.setValue(value);
  }

  void setHighlight(boolean highlight) {
    if (panelBackground == null) {
      panelBackground = panel.getBackground();
      //greying out the highlight color to match the panel's original color
      panelHighlightBackground = Colors.subtractColor(
          Colors.HIGHLIGHT_BACKGROUND, Colors.subtractColor(
              Colors.BACKGROUND, panelBackground));
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
