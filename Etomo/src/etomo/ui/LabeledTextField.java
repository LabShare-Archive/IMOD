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
 * <p> $Log$ </p>
 */
public class LabeledTextField {
  public static final String rcsid = "$Id$";

  private JPanel panel = new JPanel();
  private JLabel label = new JLabel();
  private JTextField textField = new JTextField();

  public LabeledTextField(String tfLabel) {
    label.setText(tfLabel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
//    panel.setAlignmentX(0.0F);
    panel.add(label);
    panel.add(textField);
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
    return(textField.isEnabled());
  }

  public void setVisible(boolean isVisible) {
    panel.setVisible(isVisible);
  }

  public void setEditable(boolean editable) {
    textField.setEditable(editable);
  }

  public void setTextPreferredSize(Dimension size){
    textField.setPreferredSize(size);
  }

  public void setTextMaxmimumSize(Dimension size){
    textField.setMinimumSize(size);
  }

  public void setPreferredSize(Dimension size){
    textField.setPreferredSize(size);
  }

  public void setMaximumSize(Dimension size) {
    panel.setMaximumSize(size);
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
