package etomo.ui.swing;

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
* <p> Revision 1.2  2009/06/11 17:01:59  sueh
* <p> bug# 1221 Allows the RaptorPanel to do specific queries of its parent.
* <p> </p>
*/
interface RaptorPanelParent {
  public static  final String  rcsid =  "$Id$";
 public boolean isPickVisible();
}
