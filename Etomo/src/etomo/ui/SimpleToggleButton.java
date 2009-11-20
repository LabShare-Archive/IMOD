package etomo.ui;

import javax.swing.JToggleButton;

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
 * <p> $Log$
 * <p> Revision 1.1  2009/01/20 20:27:49  sueh
 * <p> bug# 1102 A self-naming JToggleButton.
 * <p> </p>
 */
final class SimpleToggleButton extends JToggleButton  {
  public static final String rcsid = "$Id$";

  public SimpleToggleButton() {
    super();
  }

  public SimpleToggleButton(String text) {
    super(text);
    setName(text);
  }

  public void setText(String text) {
    super.setText(text);
    setName(text);
  }

  public void setName(String text) {
    String name= Utilities.convertLabelToName(text);
    super.setName(UITestFieldType.BUTTON.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }
}
