package etomo.ui;

import javax.swing.JMenuItem;

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
 * <p> Revision 1.3  2009/01/20 20:15:32  sueh
 * <p> bug# 1102 A self-naming JMenuItem.
 * <p> </p>
 */
final class MenuItem extends JMenuItem {
  public static final String rcsid = "$Id$";
  
  public MenuItem() {
    super();
  }
  
  public MenuItem(String text) {
    super(text);
  }

  MenuItem(String text, int mnemonic) {
    super(text, mnemonic);
  }

  public void setText(String text) {
    super.setText(text);
    setName(text);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(UITestFieldType.MENU_ITEM.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }
}
