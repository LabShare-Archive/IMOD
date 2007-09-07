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
* <p> Revision 1.9  2007/02/09 00:47:40  sueh
* <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
* <p> classes.
* <p>
* <p> Revision 1.8  2007/02/05 23:34:30  sueh
* <p> bug# 962 Removed commented out functions.
* <p>
* <p> Revision 1.7  2006/05/16 21:35:17  sueh
* <p> bug# 856 Changing the name whenever the label is changed so that its easy to
* <p> see what the name is.
* <p>
* <p> Revision 1.6  2006/04/25 19:12:23  sueh
* <p> bug# 787 Added UITestField, an enum style class which contains the
* <p> fields found in uitestaxis.adoc files.
* <p>
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
    setName(text);
  }
  
  public void setText(String text) {
    super.setText(text);
    setName(text);
  }
  
  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.isPrintNames()) {
      System.out.println(UITestField.CHECK_BOX.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name
          + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
  
  public void setToolTipText(String text) {
    super.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }
}
/**
* <p> $Log$
* <p> Revision 1.9  2007/02/09 00:47:40  sueh
* <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
* <p> classes.
* <p>
* <p> Revision 1.8  2007/02/05 23:34:30  sueh
* <p> bug# 962 Removed commented out functions.
* <p>
* <p> Revision 1.7  2006/05/16 21:35:17  sueh
* <p> bug# 856 Changing the name whenever the label is changed so that its easy to
* <p> see what the name is.
* <p>
* <p> Revision 1.6  2006/04/25 19:12:23  sueh
* <p> bug# 787 Added UITestField, an enum style class which contains the
* <p> fields found in uitestaxis.adoc files.
* <p>
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
