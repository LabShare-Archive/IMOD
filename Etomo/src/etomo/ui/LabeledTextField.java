package etomo.ui;

import java.awt.*;
import java.awt.event.KeyListener;
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
 * <p> Revision 3.3.4.1  2004/11/19 00:21:58  sueh
 * <p> bug# 520 Added equals(String) to compare a String parameter against
 * <p> getText().
 * <p>
 * <p> Revision 3.3  2004/04/16 02:10:04  sueh
 * <p> bug# 409 added addKeyListener() to allow keystrokes to reacted to
 * <p>
 * <p> Revision 3.2  2004/04/07 21:04:02  rickg
 * <p> Alignment is now set on the panel
 * <p>
 * <p> Revision 3.1  2004/03/24 03:04:41  rickg
 * <p> Added setText(float) methof
 * <p> Fixed setMaximumSize bug
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/06/03 23:27:05  rickg
 * <p> Comment updates
 * <p> Removed ambiguous methods
 * <p>
 * <p> Revision 2.1  2003/02/24 23:26:14  rickg
 * <p> Added a get label preferred size method
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.4  2002/12/31 23:13:24  rickg
 * <p> Added possible setalignmentx method
 * <p>
 * <p> Revision 1.3  2002/12/27 05:50:37  rickg
 * <p> Set the text field maximum height to twice the largest of the
 * <p> label and text font size in points.
 * <p>
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
      maxSize.setSize(maxSize.getWidth(), 2 * label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * textField.getFont().getSize());
    }
    textField.setMaximumSize(maxSize);
  }

  public boolean equals(String thatText) {
    String text = getText();
    if (text == null) {
      if (thatText == null) {
        return true;
      }
      return false;
    }
    if (thatText == null) {
      return false;
    }
    if (text.trim().equals(thatText.trim())) {
      return true;
    }     
    return false;
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

  public void setText(float value) {
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
  
  public void addKeyListener(KeyListener listener) {
    textField.addKeyListener(listener);
  }
  

  /**
   * Set the absolute preferred size of the text field
   * @param size
   */
  public void setTextPreferredSize(Dimension size) {
    textField.setPreferredSize(size);
  }

  /**
   * Set the absolute maximum size of the text field
   * @param size
   */
  public void setTextMaxmimumSize(Dimension size) {
    textField.setMaximumSize(size);
  }

  /**
   * Set the absolute preferred size of the panel
   * @param size
   */
  public void setPreferredSize(Dimension size) {
    panel.setPreferredSize(size);
  }

  /**
   * Set the absolute maximum size of the panel
   * @param size
   */
  public void setMaximumSize(Dimension size) {
    panel.setMaximumSize(size);
  }

  public Dimension getLabelPreferredSize() {
    return label.getPreferredSize();
  }
  
  public void setColumns(int columns) {
    textField.setColumns(columns);
  }

  public void setAlignmentX(float alignment) {
    panel.setAlignmentX(alignment);
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
