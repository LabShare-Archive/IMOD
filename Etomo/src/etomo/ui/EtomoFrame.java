package etomo.ui;

import java.awt.event.ActionEvent;

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
*/
public interface EtomoFrame {
  public static  final String  rcsid =  "$Id$";
  public void menuOptionsAction(ActionEvent event);
  public void menuFileMRUListAction(ActionEvent event);
  public void menuFileAction(ActionEvent event);
  public void menuHelpAction(ActionEvent event);
}
/**
* <p> $Log$ </p>
*/