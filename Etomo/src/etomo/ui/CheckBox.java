package etomo.ui;

import javax.swing.JCheckBox;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestField;
import etomo.util.Utilities;
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
* <p> Revision 1.5  2006/04/06 20:15:51  sueh
* <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
* <p> util.Utilities.
* <p>
* <p> Revision 1.4  2006/01/12 17:08:14  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.3  2006/01/11 21:58:46  sueh
* <p> bug# 675 corrected print name functionality
* <p>
* <p> Revision 1.2  2006/01/04 20:23:29  sueh
* <p> bug# 675 For printing the name:  putting the type first and making the type
* <p> as constant.
* <p>
* <p> Revision 1.1  2006/01/03 23:30:46  sueh
* <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
* <p> </p>
*/
final class CheckBox extends JCheckBox {
  public static  final String  rcsid =  "$Id$";
  
  public CheckBox(String text) {
    super(text);
    String name = Utilities.convertLabelToName(text);
    setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.CHECK_BOX.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name
          + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
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
* <p> Revision 1.5  2006/04/06 20:15:51  sueh
* <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
* <p> util.Utilities.
* <p>
* <p> Revision 1.4  2006/01/12 17:08:14  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
* <p> Revision 1.3  2006/01/11 21:58:46  sueh
* <p> bug# 675 corrected print name functionality
* <p>
* <p> Revision 1.2  2006/01/04 20:23:29  sueh
* <p> bug# 675 For printing the name:  putting the type first and making the type
* <p> as constant.
* <p>
* <p> Revision 1.1  2006/01/03 23:30:46  sueh
* <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
* <p> </p>
*/
