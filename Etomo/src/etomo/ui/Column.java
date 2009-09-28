package etomo.ui;

import java.util.ArrayList;
import java.util.List;

/**
* <p>Description: Makes it possible to enable/disable an entire column of a
* table with one fuction call.</p>
* 
* <p>Copyright: Copyright 2007, 2008, 2009</p>
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
* <p> Revision 1.1  2007/04/02 21:48:17  sueh
* <p> bug# 964 Class to manipulate columns in a table.
* <p> </p>
*/
final class Column {
  public static  final String  rcsid =  "$Id$";
  
  private final List list = new ArrayList();
  
  private boolean enabled = true;
  
  synchronized void add(Cell cell) {
    cell.setEnabled(enabled);
    list.add(cell);
  }
  
  void setEnabled(boolean enable) {
    enabled=enable;
    for (int i = 0;i<list.size();i++) {
      ((Cell)list.get(i)).setEnabled(enable);
    }
  }
}
