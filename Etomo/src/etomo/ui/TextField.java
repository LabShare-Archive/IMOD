package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
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
 */
final class TextField {
  public static final String rcsid = "$Id$";

  private final JTextField textField = new JTextField();

  TextField(String reference) {
    setName(reference);
    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    JLabel label = new JLabel(reference);
    Dimension maxSize = textField.getMaximumSize();
    if (label.getFont().getSize() > textField.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2 * label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * textField.getFont().getSize());
    }
    textField.setMaximumSize(maxSize);
  }

  void setToolTipText(String text) {
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  Component getComponent() {
    return textField;
  }

  void setAlignmentX(float alignmentX) {
    textField.setAlignmentX(alignmentX);
  }

  void setEnabled(boolean enabled) {
    textField.setEnabled(enabled);
  }

  void setEditable(boolean editable) {
    textField.setEditable(editable);
  }

  void setText(String text) {
    textField.setText(text);
  }

  String getText() {
    return textField.getText();
  }
  
  void setTextPreferredWidth(final double minWidth) {
    Dimension prefSize = textField.getPreferredSize();
    prefSize.setSize(minWidth, prefSize.getHeight());
    textField.setPreferredSize(prefSize);
  }

  void setSize(Dimension size) {
    textField.setPreferredSize(size);
    textField.setMaximumSize(size);
  }
  
  Dimension getPreferredSize(){
    return textField.getPreferredSize();
  }
  
  String getName() {
    return textField.getName();
  }
  
  boolean isEnabled() {
    return textField.isEnabled();
  }

  private void setName(String reference) {
    String name = Utilities.convertLabelToName(reference);
    textField.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.TEXT_FIELD.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2008/05/30 22:36:39  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.7  2008/05/30 21:34:57  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.6  2008/02/19 00:47:46  sueh
 * <p> bug# 1078 Added setTextPreferredWidth.
 * <p>
 * <p> Revision 1.5  2007/12/26 22:35:07  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.4  2007/09/07 00:29:17  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.3  2007/03/30 23:54:19  sueh
 * <p> bug# 964 Wrapping JTextField instead of inheriting it.  Added automatic sizing.
 * <p>
 * <p> Revision 1.2  2007/02/09 00:53:33  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.1  2006/09/13 23:58:14  sueh
 * <p> bug# 924 Added TextField:  extends JTextField and automatically names itself.
 * <p> </p>
 */
