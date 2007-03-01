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
 * <p>Organization:
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
    checkBox.setBorderPainted(true);
    checkBox.setBorder(BorderFactory.createEtchedBorder());
    setBackground();
    setForeground();
    setFont();
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
  
  final void setSelected(boolean selected) {
    checkBox.setSelected(selected);
  }

  public final void addActionListener(ActionListener actionListener) {
    checkBox.addActionListener(actionListener);
  }

  protected final void setForeground() {
    checkBox.setForeground(Colors.CELL_FOREGROUND);
    if (inUse) {
      checkBox.setForeground(Colors.CELL_FOREGROUND);
      setHtmlLabel(Colors.CELL_FOREGROUND);
    }
    else {
      checkBox.setForeground(Colors.CELL_NOT_IN_USE_FOREGROUND);
      setHtmlLabel(Colors.CELL_NOT_IN_USE_FOREGROUND);
    }
  }
  
  final int getHeight() {
    return checkBox.getHeight() + checkBox.getBorder().getBorderInsets(checkBox).bottom - 1;
  }
  
  final int getWidth() {
    return checkBox.getWidth();
  }
  
  final int getLeftBorder() {
    return checkBox.getBorder().getBorderInsets(checkBox).left;
  }
  
  void setToolTipText(String text) {
    checkBox.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.9  2007/02/09 00:47:49  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.8  2007/02/05 23:34:51  sueh
 * <p> bug# 962 Improved tooltip setting.
 * <p>
 * <p> Revision 1.7  2006/10/16 22:50:31  sueh
 * <p> bug# 919  Added setToolTipText().
 * <p>
 * <p> Revision 1.6  2005/11/04 00:53:39  sueh
 * <p> fixed file comment
 * <p>
 * <p> Revision 1.5  2005/08/04 20:07:52  sueh
 * <p> bug# 532 added setSelected() and getWidth().
 * <p>
 * <p> Revision 1.4  2005/08/01 18:07:07  sueh
 * <p> bug# 532 fixed getLabel(), which was returning the checkbox text instead
 * <p> of the unformatted label.
 * <p>
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
