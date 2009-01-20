package etomo.ui;

import javax.swing.Icon;
import javax.swing.JButton;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
final class SimpleButton extends JButton {
  public static final String rcsid = "$Id$";

  public SimpleButton() {
    super();
  }

  public SimpleButton(String text) {
    super(text);
    setName(text);
  }

  public SimpleButton(Icon icon) {
    super(icon);
  }

  public void setText(String text) {
    super.setText(text);
    setName(text);
  }

  public void setName(String text) {
    String 
      name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
