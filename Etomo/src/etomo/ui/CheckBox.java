package etomo.ui;

import javax.swing.JCheckBox;

import etomo.EtomoDirector;
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
* 
* <p> $Log$
* <p> Revision 1.1  2006/01/03 23:30:46  sueh
* <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
* <p> </p>
*/
final class CheckBox extends JCheckBox {
  public static  final String  rcsid =  "$Id$";
  
  public CheckBox(String text) {
    super(text);
    String name = UIUtilities.convertLabelToName(text);
    setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestConstants.CHECK_BOX_ATTRIB
          + AutodocTokenizer.SEPARATOR_CHAR + name
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
  /**
  public CheckBox() {
    super();
  }
  public CheckBox(String text, boolean selected) {
    super(text, selected);
  }
  public CheckBox(Action a) {
    super(a);
  }
  public CheckBox(Icon icon) {
    super(icon);
  }
  public CheckBox(Icon icon, boolean selected) {
    super(icon, selected);
  }
  public CheckBox(String text, Icon icon) {
    super(text, icon);
  }
  public CheckBox(String text, Icon icon, boolean selected) {
    super(text, icon, selected);
  }
*/
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/01/03 23:30:46  sueh
* <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
* <p> </p>
*/
