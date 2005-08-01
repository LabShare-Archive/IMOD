package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.plaf.ColorUIResource;

import etomo.type.EtomoBoolean2;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class CheckBoxCell extends InputCell {
  public static final String rcsid = "$Id$";

  private JCheckBox checkBox = new JCheckBox();
  //label: from JCheckBox.getText().  Updated in setLabel().  Always up to date
  //because it is a read only field in JCheckBox.
  private String unformattedLabel = "";

  CheckBoxCell() {
    super();
    setBackground();
    setForeground();
    setFont();
    checkBox.setBorderPainted(true);
    checkBox.setBorder(BorderFactory.createEtchedBorder());
  }

  protected final Component getComponent() {
    return checkBox;
  }

  final void setLabel(String label) {
    unformattedLabel = label;
    setForeground();
  }

  private final void setHtmlLabel(ColorUIResource color) {
    checkBox.setText(
        "<html><P style=\"font-weight:normal; color:rgb(" + color.getRed()
            + "," + color.getGreen() + "," + color.getBlue() + ")\">"
            + unformattedLabel + "</style>");
  }

  final String getLabel() {
    return unformattedLabel;
  }

  final void setValue(String value) {
    checkBox.setSelected(new EtomoBoolean2().set(value).is());
  }

  final boolean isSelected() {
    return checkBox.isSelected();
  }

  public final void addActionListener(ActionListener actionListener) {
    checkBox.addActionListener(actionListener);
  }

  protected final void setForeground() {
    checkBox.setForeground(foreground);
    if (inUse) {
      checkBox.setForeground(foreground);
      setHtmlLabel(foreground);
    }
    else {
      checkBox.setForeground(notInUseForeground);
      setHtmlLabel(notInUseForeground);
    }
  }
  
  final int getHeight() {
    return checkBox.getHeight();
  }
  
  final int getBorderHeight() {
    return checkBox.getBorder().getBorderInsets(checkBox).bottom;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2005/07/19 22:31:03  sueh
 * <p> bug# 532 changing the look of inUse == false to greyed out text.
 * <p>
 * <p> Revision 1.2  2005/07/11 22:55:40  sueh
 * <p> bug# 619 Added functions:  getBorderHeight and getHeight so that the
 * <p> height of the processor table can be calculated.
 * <p>
 * <p> Revision 1.1  2005/07/01 21:08:54  sueh
 * <p> bug# 619 CheckBoxCell is a writable table cell that inherits InputCell and
 * <p> contains a JCheckBoxCell.
 * <p> </p>
 */
