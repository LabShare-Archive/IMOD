package etomo.ui;

import java.awt.*;
import java.awt.event.MouseListener;

import javax.swing.*;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.2  2002/12/10 21:34:38  rickg
 * <p> Added get and set size methods
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class LabeledTextField {
  public static final String rcsid =
    "$Id$";

  private JPanel panel = new JPanel();
  private JLabel label = new JLabel();
  private JTextField textField = new JTextField();

  public LabeledTextField(String tfLabel) {
    label.setText(tfLabel);

    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

    panel.add(label);
    panel.add(textField);

    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    Dimension maxSize = textField.getMaximumSize();
    if (label.getFont().getSize() > textField.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2*label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2*textField.getFont().getSize());
    }
    textField.setMaximumSize(maxSize);
  }

  public Container getContainer() {
    return panel;
  }

  public String getLabel() {
    return label.getText();
  }

  public String getText() {
    return textField.getText();
  }

  public void setText(String text) {
    textField.setText(text);
  }

  public void setText(int value) {
    textField.setText(String.valueOf(value));
  }

  public void setText(double value) {
    textField.setText(String.valueOf(value));
  }

  public void setEnabled(boolean isEnabled) {
    textField.setEnabled(isEnabled);
    label.setEnabled(isEnabled);
  }

  public boolean isEnabled() {
    return (textField.isEnabled());
  }

  public void setVisible(boolean isVisible) {
    panel.setVisible(isVisible);
  }

  public void setEditable(boolean editable) {
    textField.setEditable(editable);
  }

  public void setTextPreferredSize(Dimension size) {
    textField.setPreferredSize(size);
  }

  public void setTextMaxmimumSize(Dimension size) {
    textField.setMinimumSize(size);
  }

  public void setPreferredSize(Dimension size) {
    textField.setPreferredSize(size);
  }

  public void setMaximumSize(Dimension size) {
    panel.setMaximumSize(size);
  }

  public void setSize(Dimension size) {
    textField.setSize(size);
  }

  public Dimension getSize() {
    return textField.getSize();
  }

  public void setColumns(int columns) {
    textField.setColumns(columns);
  }

  public void setToolTipText(String toolTipText) {
    panel.setToolTipText(toolTipText);
    textField.setToolTipText(toolTipText);
  }

  public void addMouseListener(MouseListener listener) {
    panel.addMouseListener(listener);
    label.addMouseListener(listener);
    textField.addMouseListener(listener);
  }
}
