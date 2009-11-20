package etomo.ui;

import javax.swing.JCheckBoxMenuItem;

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
* <p> Revision 1.1  2009/01/20 19:50:21  sueh
* <p> bug# 1102 A self-naming JCheckBoxMenuItem.
* <p> </p>
*/
final class CheckBoxMenuItem extends JCheckBoxMenuItem {
  public static  final String  rcsid =  "$Id$";
  
  public CheckBoxMenuItem() {
    super();
  }
  
  public CheckBoxMenuItem(String text) {
    super(text);
  }
  
  public void setText(String text) {
    super.setText(text);
    setName(text);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(UITestFieldType.CHECK_BOX_MENU_ITEM.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }

}
