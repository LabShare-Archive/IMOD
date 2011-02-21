package etomo.ui.swing;

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
* <p> $Log$
* <p> Revision 1.2  2010/12/05 05:01:29  sueh
* <p> bug# 1416 Added ComboBox(String).
* <p>
* <p> Revision 1.1  2010/11/13 16:07:35  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 3.2  2009/11/20 17:02:34  sueh
* <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
* <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
* <p> "bn." field command.
* <p>
* <p> Revision 3.1  2009/09/01 03:18:25  sueh
* <p> bug# 1222
* <p> </p>
*/

final class ComboBox extends JComboBox {
  public static final String rcsid = "$Id$";

  ComboBox(String label) {
    setName(label);
  }

  ComboBox(JLabel label) {
    setName(label.getText());
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(UITestFieldType.COMBO_BOX.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
