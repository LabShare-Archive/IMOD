package etomo.ui;

import javax.swing.JRadioButton;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestField;
import etomo.util.Utilities;

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
final class RadioButton extends JRadioButton {
  public static final String rcsid = "$Id$";

  public RadioButton(String text) {
    super(text);
    String name = Utilities.convertLabelToName(text);
    setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.RADIO_BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name
          + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  /*
   public RadioButton() {
   super();
   }
   
   public RadioButton(String text, boolean selected) {
   super(text, selected);
   setName(UIUtilities.convertLabelToName(text));
   }
   
   public RadioButton(Action a) {
   super(a);
   }
   
   public RadioButton(Icon icon) {
   super(icon);
   }
   
   public RadioButton(Icon icon, boolean selected) {
   super(icon, selected);
   }
   
   public RadioButton(String text, Icon icon) {
   super(text, icon);
   setName(UIUtilities.convertLabelToName(text));
   }
   
   public RadioButton(String text, Icon icon, boolean selected) {
   super(text, icon, selected);
   setName(UIUtilities.convertLabelToName(text));
   }
   
   public final void setText(String text) {
   super.setText(text);
   setName(UIUtilities.convertLabelToName(text));
   }*/
}
/**
 * <p> $Log$
 * <p> Revision 1.5  2006/04/06 20:17:51  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.4  2006/01/12 17:37:28  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.3  2006/01/11 22:32:48  sueh
 * <p> bug# 675 fixed print names functionality
 * <p>
 * <p> Revision 1.2  2006/01/04 20:28:01  sueh
 * <p> bug# 675 For printing the name:  putting the type first and making the type
 * <p> as constant.
 * <p>
 * <p> Revision 1.1  2005/12/23 02:19:17  sueh
 * <p> bug# 675 A class to allow automatic naming of radio buttons.
 * <p> </p>
 */
