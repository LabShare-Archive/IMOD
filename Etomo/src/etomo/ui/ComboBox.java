package etomo.ui;

import javax.swing.JComboBox;
import javax.swing.JLabel;

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

final class ComboBox extends JComboBox{
  public static  final String  rcsid =  "$Id$";
  
  ComboBox(JLabel label){
    setName(label.getText());
  }
  
  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.COMBO_BOX.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
