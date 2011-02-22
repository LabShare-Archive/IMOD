package etomo.ui.swing;

import etomo.type.EnumeratedType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
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
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.1  2007/04/13 20:37:51  sueh
* <p> bug# 964 Changed RadioButtonParent, which was confusing, to
* <p> RadioButtonInterface.
* <p>
* <p> Revision 1.1  2007/03/03 01:05:02  sueh
* <p> bug# 973 Interface for classes that use a radio button and want to respond to the
* <p> setSelected calls that automatically turn off other buttons in the group.
* <p> </p>
*/
interface RadioButtonInterface {
  public static final String rcsid = "$Id$";

  void msgSelected();

  EnumeratedType getEnumeratedType();
}
