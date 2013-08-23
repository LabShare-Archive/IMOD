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

  private final JPanel panel = new JPanel();

  private final Spinner spinner;
  private final CheckBox checkBox;

  private Color panelBackground = null;
  private Color panelHighlightBackground = null;

  private CheckBoxSpinner(final String text) {
    checkBox = new CheckBox(text);
    spinner = Spinner.getInstance(checkBox.getText());

  }

  private CheckBoxSpinner(final String text, final int value, final int minimum,
      final int maximum) {
    checkBox = new CheckBox(text);
    spinner = Spinner.getInstance(text, value, minimum, maximum);
  }

  static CheckBoxSpinner getInstance(final String text) {
    CheckBoxSpinner instance = new CheckBoxSpinner(text);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  static CheckBoxSpinner getInstance(final String text, final int value,
      final int minimum, final int maximum) {
    CheckBoxSpinner instance = new CheckBoxSpinner(text, value, minimum, maximum);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    spinner.setEnabled(false);
    // panel
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    panel.add(Box.createHorizontalGlue());
    panel.add(checkBox);
    panel.add(spinner.getContainer());
    panel.add(Box.createHorizontalGlue());
  }

  private void addListeners() {
    checkBox.addActionListener(new CheckBoxSpinnerActionListener(this));
  }

  Container getContainer() {
    return panel;
  }

  private void enableSpinner() {
    spinner.setEnabled(checkBox.isSelected() && checkBox.isEnabled());
  }

  void addCheckBoxActionListener(final ActionListener actionListener) {
    checkBox.addActionListener(actionListener);
  }

  String getCheckBoxActionCommand() {
    return checkBox.getActionCommand();
  }

  void setVisible(final boolean input) {
    panel.setVisible(input);
  }

  void setCheckBoxEnabled(final boolean enabled) {
    checkBox.setEnabled(enabled);
    enableSpinner();
  }

  boolean isCheckBoxEnabled() {
    return checkBox.isEnabled();
  }

  void setModel(final SpinnerNumberModel model) {
    spinner.setModel(model);
  }

  void setMax(final int max) {
    spinner.setMax(max);
  }

  void setMaximumSize(final Dimension maximumSize) {
    spinner.setMaximumSize(maximumSize);
  }

  Number getValue() {
    return spinner.getValue();
  }

  boolean isSelected() {
    return checkBox.isSelected();
  }

  void setSelected(final boolean selected) {
    checkBox.setSelected(selected);
    enableSpinner();
  }

  void setValue(final int value) {
    spinner.setValue(value);
  }

  void setValue(final ConstEtomoNumber value) {
    spinner.setValue(value);
  }

  void setHighlight(final boolean highlight) {
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

  void setCheckBoxToolTipText(final String text) {
    checkBox.setToolTipText(text);
  }

  void setSpinnerToolTipText(final String text) {
    spinner.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  private final class CheckBoxSpinnerActionListener implements ActionListener {
    private final CheckBoxSpinner adaptee;

    private CheckBoxSpinnerActionListener(final CheckBoxSpinner adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.enableSpinner();
    }
  }
}
