package etomo.type;
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
* <p> Revision 1.1  2007/08/10 17:35:49  sueh
* <p> bug# 847 Interface for information about a menu item on MenuButton's right click
* <p> menu
* <p> </p>
*/
public interface ActionElement {
  public static  final String  rcsid =  "$Id$";
  
  public String getActionCommand();
}
