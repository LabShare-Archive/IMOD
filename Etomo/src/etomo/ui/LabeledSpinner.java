/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4.4.4  2004/11/16 02:29:10  sueh
 * <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
 * <p> EtomoFloat, and EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.4.4.3  2004/10/22 21:08:45  sueh
 * <p> bug# 520 Using EtomoSimpleType where possible.
 * <p>
 * <p> Revision 1.4.4.2  2004/10/22 03:27:16  sueh
 * <p> bug# 520 Added setValue(ConstEtomoInteger).
 * <p>
 * <p> Revision 1.4.4.1  2004/09/23 23:38:18  sueh
 * <p> bug# 520 Added setModel() so that the spinner model can be changed.
 * <p>
 * <p> Revision 1.4  2004/04/07 21:04:02  rickg
 * <p> Alignment is now set on the panel
 * <p>
 * <p> Revision 1.3  2004/03/24 03:04:56  rickg
 * <p> Fixed setMaximumSize bug
 * <p>
 * <p> Revision 1.2  2004/03/13 00:32:11  rickg
 * <p> New setValue(int) method
 * <p>
 * <p> Revision 1.1  2004/02/05 04:37:15  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.MouseListener;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerModel;

import etomo.type.ConstEtomoNumber;

public class LabeledSpinner {
  public static final String rcsid = "$Id$";

  private JPanel panel = new JPanel();

  private JLabel label = new JLabel();

  private JSpinner spinner = new JSpinner();


  /**
   * @param spinner
   */
  public LabeledSpinner(String spinLabel, SpinnerModel model) {
    label.setText(spinLabel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    panel.add(label);
    panel.add(spinner);
    spinner.setModel(model);

    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    Dimension maxSize = spinner.getMaximumSize();
    if (label.getFont().getSize() > spinner.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2 * label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * spinner.getFont().getSize());
    }
    spinner.setMaximumSize(maxSize);
  }

  public void setModel(SpinnerModel model) {
    spinner.setModel(model);
  }
  
  public Container getContainer() {
    return panel;
  }

  public String getLabel() {
    return label.getText();
  }
  
  public Object getValue() {
    return spinner.getValue();
  }

  public void setValue(Object value) {
    spinner.setValue(value);
  }
  
  public void setValue(ConstEtomoNumber value) {
    spinner.setValue(value.getNumber());
  }

  public void setValue(int value){
    spinner.setValue(new Integer(value));
  }

  public void setEnabled(boolean isEnabled) {
    spinner.setEnabled(isEnabled);
    label.setEnabled(isEnabled);
  }

  public boolean isEnabled() {
    return (spinner.isEnabled());
  }

  public void setVisible(boolean isVisible) {
    panel.setVisible(isVisible);
  }

  /**
   * Set the absolute preferred size of the text field
   * @param size
   */
  public void setTextPreferredSize(Dimension size) {
    spinner.setPreferredSize(size);
  }

  /**
   * Set the absolute maximum size of the text field
   * @param size
   */
  public void setTextMaxmimumSize(Dimension size) {
    spinner.setMaximumSize(size);
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
  
  public void setAlignmentX(float alignment) {
    panel.setAlignmentX(alignment);
  }

  public void setToolTipText(String toolTipText) {
    panel.setToolTipText(toolTipText);
    spinner.setToolTipText(toolTipText);
  }

  public void addMouseListener(MouseListener listener) {
    panel.addMouseListener(listener);
    label.addMouseListener(listener);
    spinner.addMouseListener(listener);
  }
  
}
